library(here)
library(tidyverse)
library(basedosdados)
library(bigrquery)

# -------------------------------------------------------------------
# 1. Objetivo
# -------------------------------------------------------------------
#
# Inventariar as fontes INEP/Base dos Dados potencialmente úteis para
# construir o contexto estrutural pré-programa das escolas em 2024.
#
# Este módulo NÃO consolida ainda a dimensão contextual final.
# Ele verifica:
#   - códigos INEP disponíveis na dimensão local;
#   - tabelas candidatas e suas colunas;
#   - disponibilidade temporal de 2024;
#   - cobertura dos códigos locais em cada tabela;
#   - possíveis duplicidades de escola-ano.
#
# A separação entre inventário (12A) e extração (12B) evita escrever
# consultas dependentes de nomes de colunas ainda não confirmados.
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 2. Configuração
# -------------------------------------------------------------------

arquivo_config <- here("config_bigquery.R")

if (!file.exists(arquivo_config)) {
  stop(
    "Arquivo config_bigquery.R não encontrado na raiz do projeto."
  )
}

source(arquivo_config)

if (
  !exists("billing_project_id") ||
  is.na(billing_project_id) ||
  billing_project_id == "" ||
  str_detect(billing_project_id, "SUBSTITUA")
) {
  stop(
    "Informe o ID do projeto Google Cloud em config_bigquery.R."
  )
}

# Autenticação explícita antes das consultas.
# Na primeira execução, o navegador poderá ser aberto.
bigrquery::bq_auth()

basedosdados::set_billing_id(
  billing_project_id
)

pasta_saida <- here(
  "documentacao",
  "contexto_inep_2024"
)

dir.create(
  pasta_saida,
  recursive = TRUE,
  showWarnings = FALSE
)

arquivo_dim <- here(
  "dados_intermediarios",
  "dim_escola.csv"
)

if (!file.exists(arquivo_dim)) {
  stop(
    "Arquivo não encontrado: ",
    arquivo_dim,
    "\nExecute primeiro R/11B_consolidar_codigo_inep.R."
  )
}

# -------------------------------------------------------------------
# 3. Funções auxiliares
# -------------------------------------------------------------------

executar_sql_seguro <- function(sql, nome_consulta) {
  tryCatch(
    {
      resultado <- basedosdados::read_sql(sql) |>
        as_tibble() |>
        mutate(
          across(
            where(bit64::is.integer64),
            as.numeric
          )
        )

      list(
        sucesso = TRUE,
        resultado = resultado,
        erro = NA_character_,
        consulta = nome_consulta
      )
    },
    error = function(e) {
      list(
        sucesso = FALSE,
        resultado = tibble(),
        erro = conditionMessage(e),
        consulta = nome_consulta
      )
    }
  )
}

sql_string_list <- function(x) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & str_squish(x) != ""]

  if (length(x) == 0) {
    return("NULL")
  }

  paste0(
    "'",
    str_replace_all(x, "'", "''"),
    "'",
    collapse = ", "
  )
}

identificar_coluna <- function(
    nomes,
    candidatos
) {
  encontrados <- candidatos[candidatos %in% nomes]

  if (length(encontrados) == 0) {
    return(NA_character_)
  }

  encontrados[[1]]
}

# -------------------------------------------------------------------
# 4. Dimensão local e validação dos códigos INEP
# -------------------------------------------------------------------

dim_escola <- read_csv(
  arquivo_dim,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  mutate(
    codigo_inep = str_squish(codigo_inep),
    codigo_inep_valido = str_detect(
      codigo_inep,
      "^[0-9]{8}$"
    )
  )

validacao_codigos <- dim_escola |>
  summarise(
    numero_escolas = n_distinct(id_escola),
    escolas_com_codigo = n_distinct(
      id_escola[
        !is.na(codigo_inep) &
          codigo_inep != ""
      ]
    ),
    escolas_sem_codigo = n_distinct(
      id_escola[
        is.na(codigo_inep) |
          codigo_inep == ""
      ]
    ),
    codigos_formato_invalido = sum(
      !is.na(codigo_inep) &
        codigo_inep != "" &
        !codigo_inep_valido
    ),
    codigos_distintos = n_distinct(
      codigo_inep[
        codigo_inep_valido
      ]
    )
  )

codigos_duplicados <- dim_escola |>
  filter(codigo_inep_valido) |>
  count(
    codigo_inep,
    name = "numero_escolas"
  ) |>
  filter(numero_escolas > 1)

codigos_locais <- dim_escola |>
  filter(codigo_inep_valido) |>
  distinct(codigo_inep) |>
  pull(codigo_inep)

if (length(codigos_locais) == 0) {
  stop(
    "Nenhum código INEP válido foi encontrado em dim_escola.csv."
  )
}

if (nrow(codigos_duplicados) > 0) {
  write_csv(
    codigos_duplicados,
    file.path(
      pasta_saida,
      "00_codigos_inep_duplicados.csv"
    ),
    na = ""
  )

  stop(
    "Há códigos INEP associados a mais de uma escola. ",
    "Consulte 00_codigos_inep_duplicados.csv."
  )
}

# -------------------------------------------------------------------
# 5. Fontes candidatas
# -------------------------------------------------------------------
#
# Os nomes abaixo são tratados como candidatos. A existência efetiva
# das tabelas e o esquema observado serão registrados nos diagnósticos.
# -------------------------------------------------------------------

fontes_candidatas <- tribble(
  ~ordem, ~projeto, ~dataset, ~tabela, ~grupo,
  1L, "basedosdados", "br_inep_censo_escolar", "escola", "Censo Escolar",
  2L, "basedosdados", "br_inep_indicadores_educacionais", "complexidade_gestao_escola", "Gestão",
  3L, "basedosdados", "br_inep_indicadores_educacionais", "regularidade_docente", "Docentes",
  4L, "basedosdados", "br_inep_indicadores_educacionais", "adequacao_formacao_docente", "Docentes",
  5L, "basedosdados", "br_inep_indicadores_educacionais", "esforco_docente", "Docentes",
  6L, "basedosdados", "br_inep_indicadores_educacionais", "distorcao_idade_serie", "Fluxo",
  7L, "basedosdados", "br_inep_indicadores_educacionais", "media_alunos_turma", "Organização escolar",
  8L, "basedosdados", "br_inep_indicadores_educacionais", "nivel_socioeconomico", "Contexto socioeconômico"
) |>
  mutate(
    nome_completo = paste0(
      projeto,
      ".",
      dataset,
      ".",
      tabela
    )
  )

# -------------------------------------------------------------------
# 6. Inventário das tabelas existentes
# -------------------------------------------------------------------

inventario_tabelas <- fontes_candidatas |>
  group_by(projeto, dataset) |>
  group_split() |>
  map_dfr(
    function(bloco) {
      projeto_atual <- bloco$projeto[[1]]
      dataset_atual <- bloco$dataset[[1]]

      sql <- paste0(
        "SELECT ",
        "table_id AS table_name, ",
        "TIMESTAMP_MILLIS(creation_time) AS creation_time, ",
        "TIMESTAMP_MILLIS(last_modified_time) AS last_modified_time, ",
        "row_count, size_bytes ",
        "FROM `", projeto_atual, ".", dataset_atual,
        ".__TABLES__`"
      )

      consulta <- executar_sql_seguro(
        sql,
        paste0(projeto_atual, ".", dataset_atual)
      )

      if (!consulta$sucesso) {
        return(
          bloco |>
            transmute(
              projeto,
              dataset,
              tabela,
              nome_completo,
              grupo,
              tabela_encontrada = FALSE,
              creation_time = as.POSIXct(NA),
              last_modified_time = as.POSIXct(NA),
              row_count = NA_real_,
              size_bytes = NA_real_,
              erro_inventario = consulta$erro
            )
        )
      }

      bloco |>
        left_join(
          consulta$resultado,
          by = c("tabela" = "table_name")
        ) |>
        mutate(
          tabela_encontrada = !is.na(creation_time),
          erro_inventario = NA_character_
        ) |>
        select(
          projeto,
          dataset,
          tabela,
          nome_completo,
          grupo,
          tabela_encontrada,
          creation_time,
          last_modified_time,
          row_count,
          size_bytes,
          erro_inventario
        )
    }
  ) |>
  arrange(match(nome_completo, fontes_candidatas$nome_completo))

# -------------------------------------------------------------------
# 7. Inventário das colunas
# -------------------------------------------------------------------

fontes_existentes <- inventario_tabelas |>
  filter(tabela_encontrada)

inventario_colunas <- map_dfr(
  seq_len(nrow(fontes_existentes)),
  function(i) {
    fonte <- fontes_existentes[i, ]

    sql <- paste0(
      "SELECT ordinal_position, column_name, data_type, is_nullable ",
      "FROM `", fonte$projeto, ".", fonte$dataset,
      ".INFORMATION_SCHEMA.COLUMNS` ",
      "WHERE table_name = '", fonte$tabela, "' ",
      "ORDER BY ordinal_position"
    )

    consulta <- executar_sql_seguro(
      sql,
      fonte$nome_completo
    )

    if (!consulta$sucesso) {
      return(
        tibble(
          nome_completo = fonte$nome_completo,
          grupo = fonte$grupo,
          ordinal_position = NA_integer_,
          column_name = NA_character_,
          data_type = NA_character_,
          is_nullable = NA_character_,
          erro_colunas = consulta$erro
        )
      )
    }

    consulta$resultado |>
      mutate(
        nome_completo = fonte$nome_completo,
        grupo = fonte$grupo,
        erro_colunas = NA_character_
      ) |>
      relocate(nome_completo, grupo)
  }
)

# -------------------------------------------------------------------
# 8. Classificação preliminar das colunas
# -------------------------------------------------------------------

inventario_colunas_classificado <- inventario_colunas |>
  mutate(
    coluna_normalizada = str_to_lower(column_name),

    papel_sugerido = case_when(
      coluna_normalizada %in% c(
        "ano",
        "ano_censo"
      ) ~ "tempo",

      coluna_normalizada %in% c(
        "id_escola",
        "codigo_escola",
        "co_entidade",
        "id_entidade"
      ) ~ "chave_escola",

      str_detect(
        coluna_normalizada,
        "matric|turma|docent|professor|aluno"
      ) ~ "porte_recursos",

      str_detect(
        coluna_normalizada,
        paste0(
          "localizacao|dependencia|situacao_funcionamento|",
          "etapa|modalidade|tempo_integral"
        )
      ) ~ "caracterizacao",

      str_detect(
        coluna_normalizada,
        paste0(
          "agua|energia|esgoto|internet|biblioteca|laboratorio|",
          "quadra|acessibilidade|computador|equipamento|infra"
        )
      ) ~ "infraestrutura",

      str_detect(
        coluna_normalizada,
        paste0(
          "complexidade|regularidade|adequacao|esforco|",
          "distorcao|socioeconomico|inse"
        )
      ) ~ "indicador_educacional",

      TRUE ~ "outra"
    )
  )

# -------------------------------------------------------------------
# 9. Diagnóstico de estrutura por fonte
# -------------------------------------------------------------------

estrutura_fontes <- fontes_existentes |>
  rowwise() |>
  mutate(
    colunas = list(
      inventario_colunas |>
        filter(nome_completo == .data$nome_completo) |>
        pull(column_name) |>
        discard(is.na)
    ),

    coluna_ano = identificar_coluna(
      colunas,
      c("ano", "ano_censo")
    ),

    coluna_escola = identificar_coluna(
      colunas,
      c(
        "id_escola",
        "codigo_escola",
        "co_entidade",
        "id_entidade"
      )
    ),

    estrutura_apta_consulta =
      !is.na(coluna_ano) &
      !is.na(coluna_escola)
  ) |>
  ungroup() |>
  select(
    nome_completo,
    grupo,
    coluna_ano,
    coluna_escola,
    estrutura_apta_consulta
  )

# -------------------------------------------------------------------
# 10. Disponibilidade de anos e cobertura dos códigos locais
# -------------------------------------------------------------------

lista_codigos_sql <- sql_string_list(codigos_locais)

diagnostico_cobertura <- map_dfr(
  seq_len(nrow(estrutura_fontes)),
  function(i) {
    fonte <- estrutura_fontes[i, ]

    if (!isTRUE(fonte$estrutura_apta_consulta)) {
      return(
        tibble(
          nome_completo = fonte$nome_completo,
          grupo = fonte$grupo,
          consulta_executada = FALSE,
          ano = NA_integer_,
          linhas = NA_real_,
          escolas_distintas = NA_real_,
          escolas_locais_encontradas = NA_real_,
          duplicidade_potencial = NA,
          erro_consulta = paste(
            "Não foi possível identificar simultaneamente",
            "a coluna de ano e a coluna de escola."
          )
        )
      )
    }

    sql <- paste0(
      "SELECT ",
      "SAFE_CAST(`", fonte$coluna_ano, "` AS INT64) AS ano, ",
      "COUNT(*) AS linhas, ",
      "COUNT(DISTINCT CAST(`", fonte$coluna_escola,
      "` AS STRING)) AS escolas_distintas, ",
      "COUNT(DISTINCT IF(CAST(`", fonte$coluna_escola,
      "` AS STRING) IN (", lista_codigos_sql, "), ",
      "CAST(`", fonte$coluna_escola, "` AS STRING), NULL)) ",
      "AS escolas_locais_encontradas ",
      "FROM `", fonte$nome_completo, "` ",
      "WHERE SAFE_CAST(`", fonte$coluna_ano, "` AS INT64) ",
      "BETWEEN 2022 AND 2024 ",
      "GROUP BY ano ",
      "ORDER BY ano"
    )

    consulta <- executar_sql_seguro(
      sql,
      fonte$nome_completo
    )

    if (!consulta$sucesso) {
      return(
        tibble(
          nome_completo = fonte$nome_completo,
          grupo = fonte$grupo,
          consulta_executada = FALSE,
          ano = NA_integer_,
          linhas = NA_real_,
          escolas_distintas = NA_real_,
          escolas_locais_encontradas = NA_real_,
          duplicidade_potencial = NA,
          erro_consulta = consulta$erro
        )
      )
    }

    consulta$resultado |>
      mutate(
        nome_completo = fonte$nome_completo,
        grupo = fonte$grupo,
        consulta_executada = TRUE,
        duplicidade_potencial = linhas > escolas_distintas,
        erro_consulta = NA_character_
      ) |>
      relocate(
        nome_completo,
        grupo,
        consulta_executada
      )
  }
)

# -------------------------------------------------------------------
# 11. Cobertura detalhada em 2024
# -------------------------------------------------------------------

cobertura_detalhada_2024 <- map_dfr(
  seq_len(nrow(estrutura_fontes)),
  function(i) {
    fonte <- estrutura_fontes[i, ]

    if (!isTRUE(fonte$estrutura_apta_consulta)) {
      return(tibble())
    }

    sql <- paste0(
      "SELECT ",
      "CAST(`", fonte$coluna_escola, "` AS STRING) AS codigo_inep, ",
      "COUNT(*) AS numero_linhas_2024 ",
      "FROM `", fonte$nome_completo, "` ",
      "WHERE SAFE_CAST(`", fonte$coluna_ano, "` AS INT64) = 2024 ",
      "AND CAST(`", fonte$coluna_escola, "` AS STRING) ",
      "IN (", lista_codigos_sql, ") ",
      "GROUP BY codigo_inep"
    )

    consulta <- executar_sql_seguro(
      sql,
      paste0(fonte$nome_completo, " - cobertura 2024")
    )

    if (!consulta$sucesso) {
      return(tibble())
    }

    dim_escola |>
      filter(codigo_inep_valido) |>
      select(
        id_escola,
        codigo_inep,
        nome_canonico,
        assessora,
        tipo_vinculo_rede = any_of("tipo_vinculo_rede"),
        status_rede_2025 = any_of("status_rede_2025")
      ) |>
      left_join(
        consulta$resultado,
        by = "codigo_inep"
      ) |>
      mutate(
        nome_completo = fonte$nome_completo,
        grupo = fonte$grupo,
        encontrado_2024 = !is.na(numero_linhas_2024),
        numero_linhas_2024 = coalesce(
          as.numeric(numero_linhas_2024),
          0
        )
      ) |>
      relocate(
        nome_completo,
        grupo
      )
  }
)

# -------------------------------------------------------------------
# 12. Resumos para orientar o módulo 12B
# -------------------------------------------------------------------

resumo_fontes_2024 <- cobertura_detalhada_2024 |>
  group_by(
    nome_completo,
    grupo
  ) |>
  summarise(
    escolas_dimensao = n_distinct(id_escola),
    escolas_encontradas_2024 = n_distinct(
      id_escola[encontrado_2024]
    ),
    escolas_nao_encontradas_2024 = n_distinct(
      id_escola[!encontrado_2024]
    ),
    cobertura_percentual = if_else(
      escolas_dimensao > 0,
      100 * escolas_encontradas_2024 /
        escolas_dimensao,
      NA_real_
    ),
    escolas_com_multiplas_linhas = n_distinct(
      id_escola[numero_linhas_2024 > 1]
    ),
    .groups = "drop"
  ) |>
  arrange(desc(cobertura_percentual), nome_completo)

pendencias_2024 <- cobertura_detalhada_2024 |>
  filter(!encontrado_2024) |>
  select(
    nome_completo,
    grupo,
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora,
    any_of("tipo_vinculo_rede"),
    any_of("status_rede_2025")
  ) |>
  arrange(nome_completo, nome_canonico)

colunas_prioritarias <- inventario_colunas_classificado |>
  filter(
    papel_sugerido %in% c(
      "tempo",
      "chave_escola",
      "porte_recursos",
      "caracterizacao",
      "infraestrutura",
      "indicador_educacional"
    )
  ) |>
  arrange(
    nome_completo,
    factor(
      papel_sugerido,
      levels = c(
        "tempo",
        "chave_escola",
        "caracterizacao",
        "porte_recursos",
        "infraestrutura",
        "indicador_educacional"
      )
    ),
    ordinal_position
  )

# -------------------------------------------------------------------
# 13. Exportação
# -------------------------------------------------------------------

write_csv(
  validacao_codigos,
  file.path(
    pasta_saida,
    "01_validacao_codigos_inep.csv"
  ),
  na = ""
)

write_csv(
  inventario_tabelas,
  file.path(
    pasta_saida,
    "02_inventario_tabelas_candidatas.csv"
  ),
  na = ""
)

write_csv(
  inventario_colunas_classificado,
  file.path(
    pasta_saida,
    "03_inventario_colunas.csv"
  ),
  na = ""
)

write_csv(
  estrutura_fontes,
  file.path(
    pasta_saida,
    "04_estrutura_chaves_fontes.csv"
  ),
  na = ""
)

write_csv(
  diagnostico_cobertura,
  file.path(
    pasta_saida,
    "05_disponibilidade_temporal_cobertura.csv"
  ),
  na = ""
)

write_csv(
  cobertura_detalhada_2024,
  file.path(
    pasta_saida,
    "06_cobertura_detalhada_escolas_2024.csv"
  ),
  na = ""
)

write_csv(
  resumo_fontes_2024,
  file.path(
    pasta_saida,
    "07_resumo_fontes_2024.csv"
  ),
  na = ""
)

write_csv(
  pendencias_2024,
  file.path(
    pasta_saida,
    "08_escolas_nao_encontradas_2024.csv"
  ),
  na = ""
)

write_csv(
  colunas_prioritarias,
  file.path(
    pasta_saida,
    "09_colunas_prioritarias_para_extracao.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 14. Resumo no console
# -------------------------------------------------------------------

cat("\nInventário do contexto estrutural INEP concluído.\n")

cat("\nValidação dos códigos INEP locais:\n")
print(validacao_codigos)

cat("\nTabelas candidatas:\n")
print(
  inventario_tabelas |>
    select(
      nome_completo,
      grupo,
      tabela_encontrada,
      row_count,
      erro_inventario
    ),
  n = Inf
)

cat("\nEstrutura de chaves identificada:\n")
print(estrutura_fontes, n = Inf)

cat("\nCobertura das fontes em 2024:\n")
print(resumo_fontes_2024, n = Inf)

cat("\nArquivos gerados em:\n")
cat(pasta_saida, "\n")

cat(
  "\nPróximo passo: revisar principalmente os arquivos ",
  "03, 07, 08 e 09 antes da construção do módulo 12B.\n",
  sep = ""
)
