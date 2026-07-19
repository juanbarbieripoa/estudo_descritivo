library(here)
library(tidyverse)
library(data.table)

# -------------------------------------------------------------------
# 1. Diretórios e arquivos
# -------------------------------------------------------------------

dir.create(
  here("dados_intermediarios"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("documentacao", "codigo_inep"),
  recursive = TRUE,
  showWarnings = FALSE
)

arquivo_dim <- here(
  "dados_intermediarios",
  "dim_escola.csv"
)

arquivo_mapa_nomes <- here(
  "dados_intermediarios",
  "mapa_nomes_escolas.csv"
)

arquivo_mapa_exato <- here(
  "dados_intermediarios",
  "mapa_codigo_inep_exato.csv"
)

arquivo_revisao <- here(
  "documentacao",
  "codigo_inep",
  "revisao_manual_codigo_inep.csv"
)

arquivos_necessarios <- c(
  arquivo_dim,
  arquivo_mapa_nomes,
  arquivo_mapa_exato,
  arquivo_revisao
)

arquivos_ausentes <- arquivos_necessarios[
  !file.exists(arquivos_necessarios)
]

if (length(arquivos_ausentes) > 0) {
  stop(
    "Os seguintes arquivos não foram encontrados:\n",
    paste(arquivos_ausentes, collapse = "\n"),
    "\nExecute primeiro o módulo R/11A_recuperar_codigo_inep.R ",
    "e salve a revisão manual no caminho esperado."
  )
}

# -------------------------------------------------------------------
# 2. Funções auxiliares
# -------------------------------------------------------------------

normalizar_texto <- function(x) {
  x |>
    as.character() |>
    iconv(from = "", to = "ASCII//TRANSLIT") |>
    str_to_lower() |>
    str_squish()
}

primeiro_nao_vazio <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & str_squish(x) != ""]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  x[[1]]
}

coalescer_observacoes <- function(...) {
  valores <- c(...)
  valores <- valores[
    !is.na(valores) &
      str_squish(valores) != ""
  ]
  valores <- unique(str_squish(valores))
  
  if (length(valores) == 0) {
    return(NA_character_)
  }
  
  paste(valores, collapse = " | ")
}

codigo_inep_valido <- function(x) {
  !is.na(x) & str_detect(x, "^[0-9]{8}$")
}

# -------------------------------------------------------------------
# 3. Leitura
# -------------------------------------------------------------------

dim_escola_original <- read_csv(
  arquivo_dim,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

mapa_nomes_original <- read_csv(
  arquivo_mapa_nomes,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

mapa_exato <- read_csv(
  arquivo_mapa_exato,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

revisao <- data.table::fread(
  arquivo_revisao,
  encoding = "UTF-8",
  colClasses = "character",
  data.table = FALSE,
  check.names = FALSE
) |>
  as_tibble()

names(revisao) <- names(revisao) |>
  str_remove("^\\ufeff") |>
  str_trim()

colunas_revisao_necessarias <- c(
  "id_escola_local",
  "nome_canonico",
  "codigo_inep_candidato",
  "nome_inep_candidato",
  "decisao_manual",
  "observacao_validacao"
)

colunas_revisao_ausentes <- setdiff(
  colunas_revisao_necessarias,
  names(revisao)
)

if (length(colunas_revisao_ausentes) > 0) {
  stop(
    "Colunas ausentes no arquivo de revisão manual:\n",
    paste(colunas_revisao_ausentes, collapse = "\n")
  )
}

# Compatibilidade com a versão atual do mapa de nomes.
if (
  !"id_escola" %in% names(mapa_nomes_original) &&
  "id_escola_provisorio" %in% names(mapa_nomes_original)
) {
  mapa_nomes_original <- mapa_nomes_original |>
    rename(id_escola = id_escola_provisorio)
}

if (!"id_escola" %in% names(mapa_nomes_original)) {
  stop(
    "mapa_nomes_escolas.csv não contém id_escola nem ",
    "id_escola_provisorio."
  )
}

# -------------------------------------------------------------------
# 4. Padronização das decisões manuais
# -------------------------------------------------------------------

revisao <- revisao |>
  mutate(
    decisao_normalizada = normalizar_texto(decisao_manual),
    codigo_inep_candidato = str_squish(codigo_inep_candidato),
    observacao_validacao = na_if(
      str_squish(observacao_validacao),
      ""
    ),
    observacao_historico = if (
      "observacao_historico" %in% names(revisao)
    ) {
      na_if(str_squish(observacao_historico), "")
    } else {
      NA_character_
    }
  )

decisoes_validas <- c("aceitar", "rejeitar")

decisoes_invalidas <- revisao |>
  filter(
    is.na(decisao_normalizada) |
      decisao_normalizada == "" |
      !decisao_normalizada %in% decisoes_validas
  )

write_csv(
  decisoes_invalidas,
  here(
    "documentacao",
    "codigo_inep",
    "01_decisoes_manuais_invalidas.csv"
  ),
  na = ""
)

if (nrow(decisoes_invalidas) > 0) {
  stop(
    "Há decisões vazias ou não reconhecidas. Consulte ",
    "documentacao/codigo_inep/01_decisoes_manuais_invalidas.csv."
  )
}

aceites_manuais <- revisao |>
  filter(decisao_normalizada == "aceitar")

# Cada escola revisada deve ter exatamente um aceite.
validacao_numero_aceites <- revisao |>
  group_by(
    id_escola_local,
    nome_canonico
  ) |>
  summarise(
    numero_candidatos = n(),
    numero_aceites = sum(decisao_normalizada == "aceitar"),
    numero_rejeicoes = sum(decisao_normalizada == "rejeitar"),
    .groups = "drop"
  ) |>
  filter(numero_aceites != 1)

write_csv(
  validacao_numero_aceites,
  here(
    "documentacao",
    "codigo_inep",
    "02_escolas_sem_aceite_unico.csv"
  ),
  na = ""
)

if (nrow(validacao_numero_aceites) > 0) {
  stop(
    "Há escolas sem exatamente um candidato aceito. Consulte ",
    "documentacao/codigo_inep/02_escolas_sem_aceite_unico.csv."
  )
}

codigos_manuais_invalidos <- aceites_manuais |>
  filter(!codigo_inep_valido(codigo_inep_candidato))

write_csv(
  codigos_manuais_invalidos,
  here(
    "documentacao",
    "codigo_inep",
    "03_codigos_manuais_invalidos.csv"
  ),
  na = ""
)

if (nrow(codigos_manuais_invalidos) > 0) {
  stop(
    "Há códigos INEP manuais que não possuem oito dígitos. Consulte ",
    "documentacao/codigo_inep/03_codigos_manuais_invalidos.csv."
  )
}

mapa_manual <- aceites_manuais |>
  transmute(
    id_escola = id_escola_local,
    codigo_inep_manual = codigo_inep_candidato,
    nome_inep_manual = nome_inep_candidato,
    metodo_manual = "aceite_manual",
    observacao_manual = pmap_chr(
      list(
        observacao_validacao,
        observacao_historico
      ),
      coalescer_observacoes
    )
  ) |>
  distinct()

# -------------------------------------------------------------------
# 5. Padronização das correspondências exatas
# -------------------------------------------------------------------

colunas_exatas_necessarias <- c(
  "id_escola_local",
  "codigo_inep",
  "nome_inep"
)

colunas_exatas_ausentes <- setdiff(
  colunas_exatas_necessarias,
  names(mapa_exato)
)

if (length(colunas_exatas_ausentes) > 0) {
  stop(
    "Colunas ausentes em mapa_codigo_inep_exato.csv:\n",
    paste(colunas_exatas_ausentes, collapse = "\n")
  )
}

mapa_exato_limpo <- mapa_exato |>
  transmute(
    id_escola = id_escola_local,
    codigo_inep_exato = str_squish(codigo_inep),
    nome_inep_exato = nome_inep,
    metodo_exato = coalesce(
      metodo_correspondencia_inep,
      "exata_normalizada"
    )
  ) |>
  distinct()

codigos_exatos_invalidos <- mapa_exato_limpo |>
  filter(!codigo_inep_valido(codigo_inep_exato))

write_csv(
  codigos_exatos_invalidos,
  here(
    "documentacao",
    "codigo_inep",
    "04_codigos_exatos_invalidos.csv"
  ),
  na = ""
)

if (nrow(codigos_exatos_invalidos) > 0) {
  stop(
    "Há códigos INEP exatos que não possuem oito dígitos. Consulte ",
    "documentacao/codigo_inep/04_codigos_exatos_invalidos.csv."
  )
}

# -------------------------------------------------------------------
# 6. Consolidação e detecção de conflitos
# -------------------------------------------------------------------

dim_preparada <- dim_escola_original |>
  mutate(
    codigo_inep_anterior = na_if(
      str_squish(codigo_inep),
      ""
    ),
    observacao_historico_anterior = if (
      "observacao_historico" %in% names(dim_escola_original)
    ) {
      na_if(str_squish(observacao_historico), "")
    } else {
      NA_character_
    }
  ) |>
  select(-codigo_inep)

consolidacao_preliminar <- dim_preparada |>
  left_join(
    mapa_exato_limpo,
    by = "id_escola"
  ) |>
  left_join(
    mapa_manual,
    by = "id_escola"
  )

conflitos_codigo <- consolidacao_preliminar |>
  rowwise() |>
  mutate(
    codigos_distintos = list(
      unique(
        na.omit(
          c(
            codigo_inep_anterior,
            codigo_inep_exato,
            codigo_inep_manual
          )
        )
      )
    ),
    numero_codigos_distintos = length(codigos_distintos)
  ) |>
  ungroup() |>
  filter(numero_codigos_distintos > 1) |>
  select(
    id_escola,
    nome_canonico,
    codigo_inep_anterior,
    codigo_inep_exato,
    codigo_inep_manual,
    numero_codigos_distintos
  )

write_csv(
  conflitos_codigo,
  here(
    "documentacao",
    "codigo_inep",
    "05_conflitos_codigo_por_escola.csv"
  ),
  na = ""
)

if (nrow(conflitos_codigo) > 0) {
  stop(
    "Há escolas associadas a códigos INEP distintos. Consulte ",
    "documentacao/codigo_inep/05_conflitos_codigo_por_escola.csv."
  )
}

# Um código INEP não pode identificar mais de uma escola interna.
mapa_codigo_id <- consolidacao_preliminar |>
  transmute(
    id_escola,
    nome_canonico,
    codigo_inep = coalesce(
      codigo_inep_manual,
      codigo_inep_exato,
      codigo_inep_anterior
    )
  ) |>
  filter(!is.na(codigo_inep))

codigos_em_multiplas_escolas <- mapa_codigo_id |>
  group_by(codigo_inep) |>
  summarise(
    numero_ids = n_distinct(id_escola),
    ids_escola = paste(
      sort(unique(id_escola)),
      collapse = " | "
    ),
    nomes = paste(
      sort(unique(nome_canonico)),
      collapse = " | "
    ),
    .groups = "drop"
  ) |>
  filter(numero_ids > 1)

write_csv(
  codigos_em_multiplas_escolas,
  here(
    "documentacao",
    "codigo_inep",
    "06_codigo_inep_em_multiplas_escolas.csv"
  ),
  na = ""
)

if (nrow(codigos_em_multiplas_escolas) > 0) {
  stop(
    "Um mesmo código INEP foi associado a mais de uma escola interna. ",
    "Consulte documentacao/codigo_inep/",
    "06_codigo_inep_em_multiplas_escolas.csv."
  )
}

# -------------------------------------------------------------------
# 7. Dimensão final de escolas
# -------------------------------------------------------------------

dim_escola <- consolidacao_preliminar |>
  mutate(
    codigo_inep = coalesce(
      codigo_inep_manual,
      codigo_inep_exato,
      codigo_inep_anterior
    ),
    
    nome_inep = coalesce(
      nome_inep_manual,
      nome_inep_exato
    ),
    
    metodo_correspondencia_inep = case_when(
      !is.na(codigo_inep_manual) ~ "aceite_manual",
      !is.na(codigo_inep_exato) ~ "exata_normalizada",
      !is.na(codigo_inep_anterior) ~ "codigo_preexistente",
      TRUE ~ "nao_encontrado"
    ),
    
    status_codigo_inep = case_when(
      codigo_inep_valido(codigo_inep) ~ "consolidado",
      TRUE ~ "pendente"
    ),
    
    observacao_codigo_inep = observacao_manual,
    observacao_manual_normalizada = normalizar_texto(
      observacao_manual
    ),
    
    tipo_vinculo_rede = case_when(
      str_detect(
        observacao_manual_normalizada,
        "privada conveniada"
      ) ~ "privada_conveniada_sob_supervisao_municipal",
      
      str_detect(
        observacao_manual_normalizada,
        "filantropica sob supervisao municipal"
      ) ~ "filantropica_sob_supervisao_municipal",
      
      TRUE ~ "rede_municipal_direta_ou_nao_classificada"
    ),
    
    possivel_municipalizacao_recente = case_when(
      str_detect(
        observacao_manual_normalizada,
        "municipalizacao recente"
      ) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    escola_nova_recente = case_when(
      str_detect(
        observacao_manual_normalizada,
        "escola nova"
      ) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    status_rede_2025 = case_when(
      possivel_municipalizacao_recente ~
        "vinculo_estadual_em_2025_a_confirmar",
      
      escola_nova_recente ~
        "existencia_ou_funcionamento_em_2025_a_confirmar",
      
      tipo_vinculo_rede %in% c(
        "privada_conveniada_sob_supervisao_municipal",
        "filantropica_sob_supervisao_municipal"
      ) ~ "fora_da_rede_municipal_direta",
      
      TRUE ~ coalesce(
        na_if(status_rede_2025, ""),
        "rede_municipal_direta"
      )
    ),
    
    observacao_historico = pmap_chr(
      list(
        observacao_historico_anterior,
        observacao_manual
      ),
      coalescer_observacoes
    ),
    
    status_validacao = case_when(
      status_codigo_inep == "consolidado" ~
        "codigo_inep_consolidado",
      TRUE ~ coalesce(
        na_if(status_validacao, ""),
        "pendente"
      )
    )
  ) |>
  select(
    id_escola,
    codigo_inep,
    nome_inep,
    nome_canonico,
    nome_vinculo_original,
    chave_nome,
    assessora,
    metodo_correspondencia_inep,
    status_codigo_inep,
    observacao_codigo_inep,
    tipo_vinculo_rede,
    exposicao_2025_1av,
    programa_iniciado_apos_2025_1av,
    dose_assessoramento_observada,
    possivel_municipalizacao_recente,
    escola_nova_recente,
    status_rede_2025,
    observacao_historico,
    status_validacao
  ) |>
  arrange(nome_canonico)

# -------------------------------------------------------------------
# 8. Atualização do mapa de nomes
# -------------------------------------------------------------------

mapa_nomes_escolas <- mapa_nomes_original |>
  select(
    -any_of(c(
      "codigo_inep",
      "nome_inep",
      "metodo_correspondencia_inep",
      "status_codigo_inep"
    ))
  ) |>
  left_join(
    dim_escola |>
      select(
        id_escola,
        codigo_inep,
        nome_inep,
        metodo_correspondencia_inep,
        status_codigo_inep
      ),
    by = "id_escola"
  ) |>
  arrange(
    id_escola,
    fonte,
    nome_original
  )

# -------------------------------------------------------------------
# 9. Diagnósticos finais
# -------------------------------------------------------------------

pendencias_codigo_inep <- dim_escola |>
  filter(status_codigo_inep != "consolidado") |>
  select(
    id_escola,
    nome_canonico,
    assessora,
    status_codigo_inep,
    observacao_historico
  )

resumo_consolidacao <- dim_escola |>
  count(
    metodo_correspondencia_inep,
    status_codigo_inep,
    name = "numero_escolas"
  ) |>
  arrange(
    status_codigo_inep,
    metodo_correspondencia_inep
  )

casos_historicos_especiais <- dim_escola |>
  filter(
    possivel_municipalizacao_recente |
      escola_nova_recente |
      tipo_vinculo_rede !=
      "rede_municipal_direta_ou_nao_classificada"
  ) |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    nome_inep,
    tipo_vinculo_rede,
    possivel_municipalizacao_recente,
    escola_nova_recente,
    status_rede_2025,
    observacao_codigo_inep,
    observacao_historico
  )

# -------------------------------------------------------------------
# 10. Exportação
# -------------------------------------------------------------------

write_csv(
  dim_escola,
  arquivo_dim,
  na = ""
)

write_csv(
  mapa_nomes_escolas,
  arquivo_mapa_nomes,
  na = ""
)

write_csv(
  pendencias_codigo_inep,
  here(
    "documentacao",
    "codigo_inep",
    "07_pendencias_codigo_inep.csv"
  ),
  na = ""
)

write_csv(
  resumo_consolidacao,
  here(
    "documentacao",
    "codigo_inep",
    "08_resumo_consolidacao_codigo_inep.csv"
  ),
  na = ""
)

write_csv(
  casos_historicos_especiais,
  here(
    "documentacao",
    "codigo_inep",
    "09_casos_historicos_especiais.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 11. Resumo no console
# -------------------------------------------------------------------

cat("\nConsolidação definitiva dos códigos INEP concluída.\n")

cat("\nResumo por método e situação:\n")
print(resumo_consolidacao, n = Inf)

cat("\nPendências restantes:\n")
print(pendencias_codigo_inep, n = Inf)

cat("\nCasos históricos especiais:\n")
print(casos_historicos_especiais, n = Inf)

cat("\nArquivos atualizados:\n")
cat(
  "- dados_intermediarios/dim_escola.csv\n",
  "- dados_intermediarios/mapa_nomes_escolas.csv\n"
)

cat("\nDiagnósticos gerados:\n")
cat(
  "- documentacao/codigo_inep/01_decisoes_manuais_invalidas.csv\n",
  "- documentacao/codigo_inep/02_escolas_sem_aceite_unico.csv\n",
  "- documentacao/codigo_inep/03_codigos_manuais_invalidos.csv\n",
  "- documentacao/codigo_inep/04_codigos_exatos_invalidos.csv\n",
  "- documentacao/codigo_inep/05_conflitos_codigo_por_escola.csv\n",
  "- documentacao/codigo_inep/06_codigo_inep_em_multiplas_escolas.csv\n",
  "- documentacao/codigo_inep/07_pendencias_codigo_inep.csv\n",
  "- documentacao/codigo_inep/08_resumo_consolidacao_codigo_inep.csv\n",
  "- documentacao/codigo_inep/09_casos_historicos_especiais.csv\n"
)
