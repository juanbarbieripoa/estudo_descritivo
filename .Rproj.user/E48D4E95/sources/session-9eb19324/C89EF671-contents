# ===================================================================
# 16_criar_perfil_escola.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# ===================================================================
#
# OBJETIVO ANALÍTICO
#
# Construir um perfil consolidado com uma linha por escola, reunindo:
#
#   1. identificação e vínculo administrativo;
#   2. contexto estrutural pré-programa de 2024;
#   3. cobertura das avaliações de 2025 e 2026;
#   4. sínteses multissérie ponderadas pelo número de avaliados;
#   5. heterogeneidade dos resultados entre 1º e 5º ano;
#   6. indicadores transparentes de participação e composição;
#   7. uma base compacta escola × série para fichas e análises futuras.
#
# PRINCÍPIOS METODOLÓGICOS
#
# - O perfil é descritivo e observacional.
# - Nenhum resultado é interpretado como efeito da assessora.
# - A assessora gerencial representa vínculo administrativo.
# - As sínteses de proficiência entre séries são medidas agregadas de
#   apoio gerencial. A leitura substantiva deve continuar considerando
#   os resultados específicos de cada ano escolar.
# - O módulo NÃO constrói índice de complexidade, escore de qualidade,
#   ranking de escolas ou ranking de assessoras.
# - Mudanças de participação e de estudantes previstos são mantidas
#   como informação de qualidade de composição.
# - A base congelada no módulo 15 não é alterada.
#
# ENTRADAS
#
# dados_finais/dim_escola_final.rds ou .csv
# dados_finais/base_analitica_final_escola_serie.rds ou .csv
#
# PRODUTOS PRINCIPAIS
#
# dados_finais/perfil_escola_completo.csv e .rds
# dados_finais/perfil_escola_gerencial.csv e .rds
# dados_finais/perfil_escola_serie_compacto.csv e .rds
# documentacao/perfil_escola/dicionario_perfil_escola_gerencial.csv
# documentacao/perfil_escola/execucao_<data_hora>/...
# dados_finais/historico/perfil_escola/execucao_<data_hora>/...
# ===================================================================

library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Identificação da execução e diretórios
# -------------------------------------------------------------------

id_execucao <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

pasta_dados_finais <- here(
  "dados_finais"
)

pasta_historico <- here(
  "dados_finais",
  "historico",
  "perfil_escola",
  paste0("execucao_", id_execucao)
)

pasta_documentacao <- here(
  "documentacao",
  "perfil_escola"
)

pasta_execucao <- here(
  "documentacao",
  "perfil_escola",
  paste0("execucao_", id_execucao)
)

walk(
  c(
    pasta_dados_finais,
    pasta_historico,
    pasta_documentacao,
    pasta_execucao
  ),
  ~ dir.create(
    .x,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

# -------------------------------------------------------------------
# 2. Arquivos de entrada e saída
# -------------------------------------------------------------------

arquivos_entrada_rds <- c(
  dim_escola_final = here(
    "dados_finais",
    "dim_escola_final.rds"
  ),
  base_final_escola_serie = here(
    "dados_finais",
    "base_analitica_final_escola_serie.rds"
  )
)

arquivos_entrada_csv <- c(
  dim_escola_final = here(
    "dados_finais",
    "dim_escola_final.csv"
  ),
  base_final_escola_serie = here(
    "dados_finais",
    "base_analitica_final_escola_serie.csv"
  )
)

arquivos_saida <- c(
  perfil_completo_csv = here(
    "dados_finais",
    "perfil_escola_completo.csv"
  ),
  perfil_completo_rds = here(
    "dados_finais",
    "perfil_escola_completo.rds"
  ),
  perfil_gerencial_csv = here(
    "dados_finais",
    "perfil_escola_gerencial.csv"
  ),
  perfil_gerencial_rds = here(
    "dados_finais",
    "perfil_escola_gerencial.rds"
  ),
  perfil_serie_csv = here(
    "dados_finais",
    "perfil_escola_serie_compacto.csv"
  ),
  perfil_serie_rds = here(
    "dados_finais",
    "perfil_escola_serie_compacto.rds"
  ),
  dicionario_csv = here(
    "documentacao",
    "perfil_escola",
    "dicionario_perfil_escola_gerencial.csv"
  )
)

# -------------------------------------------------------------------
# 3. Funções auxiliares
# -------------------------------------------------------------------

ler_base_final <- function(nome_fonte) {
  caminho_rds <- arquivos_entrada_rds[[nome_fonte]]
  caminho_csv <- arquivos_entrada_csv[[nome_fonte]]

  if (file.exists(caminho_rds)) {
    dados <- readRDS(caminho_rds)
    origem <- caminho_rds
  } else if (file.exists(caminho_csv)) {
    dados <- read_csv(
      caminho_csv,
      show_col_types = FALSE,
      na = c("", "NA")
    )
    origem <- caminho_csv
  } else {
    stop(
      "Não foi encontrado o arquivo RDS nem o CSV da fonte `",
      nome_fonte,
      "`.\nCaminhos verificados:\n",
      caminho_rds,
      "\n",
      caminho_csv,
      "\nExecute primeiro o módulo 15."
    )
  }

  if (!is.data.frame(dados)) {
    stop(
      "A fonte `",
      nome_fonte,
      "` não foi lida como data frame."
    )
  }

  attr(dados, "caminho_origem") <- origem
  dados
}

soma_segura <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  sum(
    x,
    na.rm = TRUE
  )
}

media_segura <- function(x) {
  x <- x[
    !is.na(x)
  ]

  if (length(x) == 0) {
    return(NA_real_)
  }

  mean(x)
}

mediana_segura <- function(x) {
  x <- x[
    !is.na(x)
  ]

  if (length(x) == 0) {
    return(NA_real_)
  }

  median(x)
}

desvio_padrao_seguro <- function(x) {
  x <- x[
    !is.na(x)
  ]

  if (length(x) < 2) {
    return(NA_real_)
  }

  sd(x)
}

amplitude_segura <- function(x) {
  x <- x[
    !is.na(x)
  ]

  if (length(x) == 0) {
    return(NA_real_)
  }

  max(x) - min(x)
}

media_ponderada_segura <- function(x, peso) {
  valido <- !is.na(x) &
    !is.na(peso) &
    peso > 0

  if (!any(valido)) {
    return(NA_real_)
  }

  sum(
    x[valido] * peso[valido]
  ) / sum(peso[valido])
}

taxa_por_totais <- function(numerador, denominador) {
  numerador_total <- soma_segura(numerador)
  denominador_total <- soma_segura(denominador)

  if (
    is.na(numerador_total) ||
      is.na(denominador_total) ||
      denominador_total <= 0
  ) {
    return(NA_real_)
  }

  100 * numerador_total / denominador_total
}

variacao_relativa_segura <- function(valor_final, valor_inicial) {
  if (
    is.na(valor_final) ||
      is.na(valor_inicial) ||
      valor_inicial == 0
  ) {
    return(NA_real_)
  }

  100 * (valor_final - valor_inicial) / valor_inicial
}

classificar_participacao <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    x < 70 ~ "Abaixo de 70%",
    x < 80 ~ "70% a menos de 80%",
    x < 90 ~ "80% a menos de 90%",
    TRUE ~ "90% ou mais"
  )
}

classificar_direcao <- function(x, tolerancia = 1e-8) {
  case_when(
    is.na(x) ~ "Não comparável",
    x > tolerancia ~ "Variação positiva",
    x < -tolerancia ~ "Variação negativa",
    TRUE ~ "Sem variação mensurável"
  )
}

primeiro_nao_vazio <- function(x) {
  x <- as.character(x)
  x <- x[
    !is.na(x) &
      str_squish(x) != ""
  ]

  if (length(x) == 0) {
    return(NA_character_)
  }

  x[[1]]
}

valores_distintos_texto <- function(x) {
  x <- as.character(x)
  x <- sort(
    unique(
      x[
        !is.na(x) &
          str_squish(x) != ""
      ]
    )
  )

  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(
    x,
    collapse = " | "
  )
}

inventariar_estrutura <- function(dados, fonte) {
  map_dfr(
    seq_along(dados),
    function(i) {
      variavel <- names(dados)[[i]]
      x <- dados[[i]]

      tibble(
        fonte = fonte,
        ordem_coluna = i,
        variavel = variavel,
        classe_r = paste(
          class(x),
          collapse = " | "
        ),
        tipo_r = typeof(x),
        numero_linhas = nrow(dados),
        valores_ausentes = sum(is.na(x)),
        valores_distintos = n_distinct(
          x,
          na.rm = TRUE
        )
      )
    }
  )
}

completude_variaveis <- function(dados) {
  map_dfr(
    seq_along(dados),
    function(i) {
      variavel <- names(dados)[[i]]
      x <- dados[[i]]

      vazios_texto <- if (is.character(x)) {
        sum(
          !is.na(x) &
            str_squish(x) == ""
        )
      } else {
        0L
      }

      tibble(
        ordem_coluna = i,
        variavel = variavel,
        classe_r = paste(
          class(x),
          collapse = " | "
        ),
        valores_ausentes = sum(is.na(x)),
        valores_vazios_texto = vazios_texto,
        percentual_ausente = 100 * sum(is.na(x)) / nrow(dados),
        valores_distintos = n_distinct(
          x,
          na.rm = TRUE
        )
      )
    }
  )
}

arquivar_se_existir <- function(caminho, pasta_destino) {
  if (!file.exists(caminho)) {
    return(invisible(FALSE))
  }

  dir.create(
    pasta_destino,
    recursive = TRUE,
    showWarnings = FALSE
  )

  destino <- file.path(
    pasta_destino,
    basename(caminho)
  )

  copiado <- file.copy(
    caminho,
    destino,
    overwrite = TRUE,
    copy.date = TRUE
  )

  if (!copiado) {
    stop(
      "Não foi possível arquivar a versão anterior de: ",
      caminho
    )
  }

  invisible(TRUE)
}

manifestar_arquivo <- function(nome_produto, caminho) {
  info <- file.info(caminho)

  tibble(
    produto = nome_produto,
    caminho = normalizePath(
      caminho,
      winslash = "/",
      mustWork = TRUE
    ),
    tamanho_bytes = unname(info$size),
    data_modificacao = format(
      info$mtime,
      "%Y-%m-%d %H:%M:%S"
    ),
    md5 = unname(
      tools::md5sum(caminho)
    )
  )
}

# -------------------------------------------------------------------
# 4. Leitura das bases congeladas no módulo 15
# -------------------------------------------------------------------

dim_escola_final <- ler_base_final(
  "dim_escola_final"
)

base_final_escola_serie <- ler_base_final(
  "base_final_escola_serie"
)

caminhos_entrada_utilizados <- c(
  dim_escola_final = attr(
    dim_escola_final,
    "caminho_origem"
  ),
  base_final_escola_serie = attr(
    base_final_escola_serie,
    "caminho_origem"
  )
)

# Padronização mínima das chaves, sem alterar os demais campos.
dim_escola_final <- dim_escola_final |>
  mutate(
    id_escola = as.character(id_escola),
    codigo_inep = as.character(codigo_inep)
  )

base_final_escola_serie <- base_final_escola_serie |>
  mutate(
    id_escola = as.character(id_escola),
    codigo_inep = as.character(codigo_inep),
    ano_escolar = as.integer(ano_escolar),
    componente = as.character(componente)
  )

# -------------------------------------------------------------------
# 5. Inventário das entradas e validação de colunas
# -------------------------------------------------------------------

estrutura_entradas <- bind_rows(
  inventariar_estrutura(
    dim_escola_final,
    "dim_escola_final"
  ),
  inventariar_estrutura(
    base_final_escola_serie,
    "base_final_escola_serie"
  )
)

write_csv(
  estrutura_entradas,
  file.path(
    pasta_execucao,
    "01_estrutura_bases_entrada.csv"
  ),
  na = ""
)

colunas_obrigatorias <- list(
  dim_escola_final = c(
    "id_escola",
    "codigo_inep",
    "nome_canonico",
    "assessora_gerencial",
    "grupo_administrativo_2024_final",
    "tipo_vinculo_rede_final",
    "status_rede_2025_final",
    "matriculas_anos_iniciais",
    "turmas_anos_iniciais",
    "docentes_anos_iniciais",
    "indice_infraestrutura_basica"
  ),
  base_final_escola_serie = c(
    "id_escola",
    "codigo_inep",
    "nome_canonico",
    "assessora_gerencial",
    "ano_escolar",
    "componente",
    "painel_resultado_balanceado",
    "amostra_principal_descritiva",
    "grupo_administrativo_2024_final",
    "tipo_vinculo_rede_final",
    "status_rede_2025_final",
    "presente_2025",
    "presente_2026",
    "resultado_disponivel_2025",
    "resultado_disponivel_2026",
    "previstos_2025",
    "avaliados_2025",
    "taxa_participacao_2025",
    "proficiencia_media_2025",
    "pct_defasagem_2025",
    "pct_intermediario_2025",
    "pct_adequado_2025",
    "previstos_2026",
    "avaliados_2026",
    "taxa_participacao_2026",
    "proficiencia_media_2026",
    "pct_defasagem_2026",
    "pct_intermediario_2026",
    "pct_adequado_2026",
    "delta_participacao",
    "delta_proficiencia",
    "delta_pct_defasagem",
    "delta_pct_intermediario",
    "delta_pct_adequado",
    "variacao_relativa_previstos",
    "variacao_relativa_avaliados",
    "aumento_participacao_10pp",
    "queda_participacao_10pp",
    "mudanca_previstos_20pct",
    "observacao_composicao"
  )
)

bases_lidas <- list(
  dim_escola_final = dim_escola_final,
  base_final_escola_serie = base_final_escola_serie
)

validacao_colunas <- map_dfr(
  seq_along(colunas_obrigatorias),
  function(i) {
    fonte <- names(colunas_obrigatorias)[[i]]
    colunas <- colunas_obrigatorias[[i]]
    dados <- bases_lidas[[i]]

    tibble(
      fonte = fonte,
      coluna_obrigatoria = colunas,
      presente = colunas %in% names(dados)
    )
  }
)

write_csv(
  validacao_colunas,
  file.path(
    pasta_execucao,
    "02_validacao_colunas_obrigatorias.csv"
  ),
  na = ""
)

colunas_ausentes <- validacao_colunas |>
  filter(!presente)

if (nrow(colunas_ausentes) > 0) {
  stop(
    "Há colunas obrigatórias ausentes. Consulte: ",
    file.path(
      pasta_execucao,
      "02_validacao_colunas_obrigatorias.csv"
    )
  )
}

# -------------------------------------------------------------------
# 6. Diagnóstico de chaves e escopo
# -------------------------------------------------------------------

duplicidades_dim <- dim_escola_final |>
  count(
    id_escola,
    name = "numero_linhas"
  ) |>
  filter(numero_linhas > 1) |>
  mutate(
    fonte = "dim_escola_final",
    chave = id_escola
  ) |>
  select(
    fonte,
    chave,
    numero_linhas
  )

duplicidades_base <- base_final_escola_serie |>
  count(
    id_escola,
    ano_escolar,
    componente,
    name = "numero_linhas"
  ) |>
  filter(numero_linhas > 1) |>
  mutate(
    fonte = "base_final_escola_serie",
    chave = paste(
      id_escola,
      ano_escolar,
      componente,
      sep = " | "
    )
  ) |>
  select(
    fonte,
    chave,
    numero_linhas
  )

duplicidades_chaves <- bind_rows(
  duplicidades_dim,
  duplicidades_base
)

write_csv(
  duplicidades_chaves,
  file.path(
    pasta_execucao,
    "03_duplicidades_chaves_entrada.csv"
  ),
  na = ""
)

ids_apenas_dim <- setdiff(
  dim_escola_final$id_escola,
  base_final_escola_serie$id_escola
)

ids_apenas_base <- setdiff(
  base_final_escola_serie$id_escola,
  dim_escola_final$id_escola
)

comparacao_ids <- bind_rows(
  tibble(
    origem_exclusiva = "dim_escola_final",
    id_escola = ids_apenas_dim
  ),
  tibble(
    origem_exclusiva = "base_final_escola_serie",
    id_escola = ids_apenas_base
  )
)

write_csv(
  comparacao_ids,
  file.path(
    pasta_execucao,
    "04_ids_exclusivos_entre_bases.csv"
  ),
  na = ""
)

escopo_observado <- base_final_escola_serie |>
  count(
    ano_escolar,
    componente,
    name = "numero_linhas"
  ) |>
  arrange(
    componente,
    ano_escolar
  )

write_csv(
  escopo_observado,
  file.path(
    pasta_execucao,
    "05_escopo_observado.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 7. Base compacta escola × série
# -------------------------------------------------------------------

perfil_escola_serie_compacto <- base_final_escola_serie |>
  transmute(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_gerencial,
    ano_escolar,
    componente,
    grupo_administrativo_2024_final,
    tipo_vinculo_rede_final,
    status_rede_2025_final,
    painel_resultado_balanceado,
    amostra_principal_descritiva,
    previstos_2025,
    avaliados_2025,
    taxa_participacao_2025,
    proficiencia_media_2025,
    pct_defasagem_2025,
    pct_intermediario_2025,
    pct_adequado_2025,
    previstos_2026,
    avaliados_2026,
    taxa_participacao_2026,
    proficiencia_media_2026,
    pct_defasagem_2026,
    pct_intermediario_2026,
    pct_adequado_2026,
    delta_participacao,
    delta_proficiencia,
    delta_pct_defasagem,
    delta_pct_intermediario,
    delta_pct_adequado,
    variacao_relativa_previstos,
    variacao_relativa_avaliados,
    aumento_participacao_10pp,
    queda_participacao_10pp,
    mudanca_previstos_20pct,
    observacao_composicao,
    faixa_participacao_2025_serie = classificar_participacao(
      taxa_participacao_2025
    ),
    faixa_participacao_2026_serie = classificar_participacao(
      taxa_participacao_2026
    ),
    direcao_delta_participacao = classificar_direcao(
      delta_participacao
    ),
    direcao_delta_proficiencia = classificar_direcao(
      delta_proficiencia
    ),
    alerta_composicao_serie =
      coalesce(
        aumento_participacao_10pp,
        FALSE
      ) |
      coalesce(
        queda_participacao_10pp,
        FALSE
      ) |
      coalesce(
        mudanca_previstos_20pct,
        FALSE
      )
  ) |>
  arrange(
    nome_canonico,
    ano_escolar
  )

# -------------------------------------------------------------------
# 8. Consolidação dos resultados no nível da escola
# -------------------------------------------------------------------

resumo_avaliacoes_escola <- base_final_escola_serie |>
  group_by(
    id_escola
  ) |>
  summarise(
    numero_series_esperadas = n_distinct(ano_escolar),
    numero_componentes = n_distinct(componente),

    numero_series_presentes_2025 = sum(
      coalesce(
        presente_2025,
        FALSE
      )
    ),
    numero_series_presentes_2026 = sum(
      coalesce(
        presente_2026,
        FALSE
      )
    ),
    numero_series_resultado_2025 = sum(
      coalesce(
        resultado_disponivel_2025,
        FALSE
      )
    ),
    numero_series_resultado_2026 = sum(
      coalesce(
        resultado_disponivel_2026,
        FALSE
      )
    ),
    numero_series_comparaveis = sum(
      coalesce(
        painel_resultado_balanceado,
        FALSE
      )
    ),
    painel_completo_cinco_series =
      numero_series_comparaveis == 5,

    previstos_total_2025 = soma_segura(
      previstos_2025
    ),
    avaliados_total_2025 = soma_segura(
      avaliados_2025
    ),
    taxa_participacao_escola_2025 = taxa_por_totais(
      avaliados_2025,
      previstos_2025
    ),
    proficiencia_multisserie_ponderada_2025 =
      media_ponderada_segura(
        proficiencia_media_2025,
        avaliados_2025
      ),
    pct_defasagem_multisserie_ponderado_2025 =
      media_ponderada_segura(
        pct_defasagem_2025,
        avaliados_2025
      ),
    pct_intermediario_multisserie_ponderado_2025 =
      media_ponderada_segura(
        pct_intermediario_2025,
        avaliados_2025
      ),
    pct_adequado_multisserie_ponderado_2025 =
      media_ponderada_segura(
        pct_adequado_2025,
        avaliados_2025
      ),

    previstos_total_2026 = soma_segura(
      previstos_2026
    ),
    avaliados_total_2026 = soma_segura(
      avaliados_2026
    ),
    taxa_participacao_escola_2026 = taxa_por_totais(
      avaliados_2026,
      previstos_2026
    ),
    proficiencia_multisserie_ponderada_2026 =
      media_ponderada_segura(
        proficiencia_media_2026,
        avaliados_2026
      ),
    pct_defasagem_multisserie_ponderado_2026 =
      media_ponderada_segura(
        pct_defasagem_2026,
        avaliados_2026
      ),
    pct_intermediario_multisserie_ponderado_2026 =
      media_ponderada_segura(
        pct_intermediario_2026,
        avaliados_2026
      ),
    pct_adequado_multisserie_ponderado_2026 =
      media_ponderada_segura(
        pct_adequado_2026,
        avaliados_2026
      ),

    delta_participacao_escola =
      taxa_participacao_escola_2026 -
      taxa_participacao_escola_2025,
    delta_proficiencia_multisserie_ponderada =
      proficiencia_multisserie_ponderada_2026 -
      proficiencia_multisserie_ponderada_2025,
    delta_pct_defasagem_multisserie_ponderado =
      pct_defasagem_multisserie_ponderado_2026 -
      pct_defasagem_multisserie_ponderado_2025,
    delta_pct_intermediario_multisserie_ponderado =
      pct_intermediario_multisserie_ponderado_2026 -
      pct_intermediario_multisserie_ponderado_2025,
    delta_pct_adequado_multisserie_ponderado =
      pct_adequado_multisserie_ponderado_2026 -
      pct_adequado_multisserie_ponderado_2025,
    variacao_relativa_previstos_escola =
      variacao_relativa_segura(
        previstos_total_2026,
        previstos_total_2025
      ),
    variacao_relativa_avaliados_escola =
      variacao_relativa_segura(
        avaliados_total_2026,
        avaliados_total_2025
      ),

    mediana_participacao_series_2025 = mediana_segura(
      taxa_participacao_2025
    ),
    mediana_participacao_series_2026 = mediana_segura(
      taxa_participacao_2026
    ),
    amplitude_participacao_series_2025 = amplitude_segura(
      taxa_participacao_2025
    ),
    amplitude_participacao_series_2026 = amplitude_segura(
      taxa_participacao_2026
    ),
    desvio_participacao_series_2025 = desvio_padrao_seguro(
      taxa_participacao_2025
    ),
    desvio_participacao_series_2026 = desvio_padrao_seguro(
      taxa_participacao_2026
    ),

    mediana_proficiencia_series_2025 = mediana_segura(
      proficiencia_media_2025
    ),
    mediana_proficiencia_series_2026 = mediana_segura(
      proficiencia_media_2026
    ),
    amplitude_proficiencia_series_2025 = amplitude_segura(
      proficiencia_media_2025
    ),
    amplitude_proficiencia_series_2026 = amplitude_segura(
      proficiencia_media_2026
    ),
    desvio_proficiencia_series_2025 = desvio_padrao_seguro(
      proficiencia_media_2025
    ),
    desvio_proficiencia_series_2026 = desvio_padrao_seguro(
      proficiencia_media_2026
    ),

    numero_series_delta_proficiencia_positivo = sum(
      !is.na(delta_proficiencia) &
        delta_proficiencia > 1e-8
    ),
    numero_series_delta_proficiencia_negativo = sum(
      !is.na(delta_proficiencia) &
        delta_proficiencia < -1e-8
    ),
    numero_series_delta_proficiencia_estavel = sum(
      !is.na(delta_proficiencia) &
        abs(delta_proficiencia) <= 1e-8
    ),
    numero_series_delta_participacao_positivo = sum(
      !is.na(delta_participacao) &
        delta_participacao > 1e-8
    ),
    numero_series_delta_participacao_negativo = sum(
      !is.na(delta_participacao) &
        delta_participacao < -1e-8
    ),

    numero_series_participacao_abaixo_70_2025 = sum(
      !is.na(taxa_participacao_2025) &
        taxa_participacao_2025 < 70
    ),
    numero_series_participacao_abaixo_70_2026 = sum(
      !is.na(taxa_participacao_2026) &
        taxa_participacao_2026 < 70
    ),
    numero_series_participacao_abaixo_80_2025 = sum(
      !is.na(taxa_participacao_2025) &
        taxa_participacao_2025 < 80
    ),
    numero_series_participacao_abaixo_80_2026 = sum(
      !is.na(taxa_participacao_2026) &
        taxa_participacao_2026 < 80
    ),
    numero_series_participacao_abaixo_90_2025 = sum(
      !is.na(taxa_participacao_2025) &
        taxa_participacao_2025 < 90
    ),
    numero_series_participacao_abaixo_90_2026 = sum(
      !is.na(taxa_participacao_2026) &
        taxa_participacao_2026 < 90
    ),

    numero_series_aumento_participacao_10pp = sum(
      coalesce(
        aumento_participacao_10pp,
        FALSE
      )
    ),
    numero_series_queda_participacao_10pp = sum(
      coalesce(
        queda_participacao_10pp,
        FALSE
      )
    ),
    numero_series_mudanca_previstos_20pct = sum(
      coalesce(
        mudanca_previstos_20pct,
        FALSE
      )
    ),
    numero_series_alerta_composicao = sum(
      coalesce(
        aumento_participacao_10pp,
        FALSE
      ) |
      coalesce(
        queda_participacao_10pp,
        FALSE
      ) |
      coalesce(
        mudanca_previstos_20pct,
        FALSE
      )
    ),
    possui_alerta_composicao =
      numero_series_alerta_composicao > 0,
    observacoes_composicao = valores_distintos_texto(
      observacao_composicao
    ),
    .groups = "drop"
  ) |>
  mutate(
    faixa_participacao_escola_2025 = classificar_participacao(
      taxa_participacao_escola_2025
    ),
    faixa_participacao_escola_2026 = classificar_participacao(
      taxa_participacao_escola_2026
    ),
    direcao_delta_participacao_escola = classificar_direcao(
      delta_participacao_escola
    ),
    direcao_delta_proficiencia_multisserie = classificar_direcao(
      delta_proficiencia_multisserie_ponderada
    ),
    proporcao_series_comparaveis =
      100 * numero_series_comparaveis / numero_series_esperadas,
    proporcao_series_delta_proficiencia_positivo = case_when(
      numero_series_comparaveis > 0 ~
        100 * numero_series_delta_proficiencia_positivo /
        numero_series_comparaveis,
      TRUE ~ NA_real_
    )
  )

# -------------------------------------------------------------------
# 9. Perfil completo e perfil gerencial
# -------------------------------------------------------------------

perfil_escola_completo <- dim_escola_final |>
  left_join(
    resumo_avaliacoes_escola,
    by = "id_escola"
  ) |>
  arrange(
    nome_canonico,
    id_escola
  )

colunas_perfil_gerencial <- c(
  # Identificação e vínculo administrativo
  "id_escola",
  "codigo_inep",
  "nome_canonico",
  "nome_inep",
  "assessora_gerencial",
  "assessora",
  "grupo_administrativo_2024_final",
  "tipo_vinculo_rede_final",
  "status_rede_2025_final",
  "dependencia_administrativa_2024",
  "situacao_funcionamento_2024",
  "municipalizada_apos_2024",
  "possivel_municipalizacao_recente",
  "escola_nova_recente",
  "privada_vinculada_final",
  "caso_historico_administrativo",
  "observacao_administrativa_final",
  "requer_revisao_tecnica",
  "divergencia_censo_cadastro",
  "contexto_2024_encontrado",
  "estratos_corrigidos_disponiveis",
  "incluir_universo_municipal_direto_2024",
  "incluir_universo_municipal_ampliado_2024",
  "incluir_rede_municipal_operacional_2025",

  # Volume e organização da oferta
  "quantidade_matricula_educacao_basica",
  "matriculas_anos_iniciais",
  "turmas_anos_iniciais",
  "docentes_anos_iniciais",
  "alunos_por_turma_anos_iniciais",
  "alunos_por_docente_anos_iniciais",
  "porte_anos_iniciais",
  "quartil_porte_anos_iniciais",
  "faixa_porte_contextual_escola",
  "numero_etapas_amplas_ofertadas",
  "oferta_educacao_infantil",
  "oferta_anos_finais",
  "oferta_eja",
  "oferta_educacao_especial",

  # Características da matrícula
  "pct_matriculas_anos_iniciais_integral",
  "pct_matriculas_educacao_especial",
  "pct_matriculas_transporte_publico",
  "pct_matriculas_preta_parda_indigena",

  # Infraestrutura e recursos
  "indice_infraestrutura_basica",
  "quartil_infraestrutura_escola",
  "numero_itens_infraestrutura_presentes",
  "quantidade_sala_utilizada",
  "quantidade_sala_utilizada_climatizada",
  "quantidade_sala_utilizada_acessivel",
  "possui_espaco_leitura",
  "possui_recurso_acessibilidade",
  "laboratorio_ciencias",
  "laboratorio_informatica",
  "quadra_esportes",
  "internet_aprendizagem",
  "internet_alunos",
  "sala_atendimento_especial",
  "quantidade_computador_portatil_aluno",
  "quantidade_tablet_aluno",
  "quantidade_profissional_coordenador",
  "quantidade_profissional_psicologo",
  "quantidade_profissional_assistente_social",
  "quantidade_profissional_monitor",
  "quantidade_profissional_pedagogia",

  # Cobertura do painel
  "numero_series_esperadas",
  "numero_series_presentes_2025",
  "numero_series_presentes_2026",
  "numero_series_resultado_2025",
  "numero_series_resultado_2026",
  "numero_series_comparaveis",
  "proporcao_series_comparaveis",
  "painel_completo_cinco_series",

  # Síntese da avaliação de 2025
  "previstos_total_2025",
  "avaliados_total_2025",
  "taxa_participacao_escola_2025",
  "faixa_participacao_escola_2025",
  "proficiencia_multisserie_ponderada_2025",
  "pct_defasagem_multisserie_ponderado_2025",
  "pct_intermediario_multisserie_ponderado_2025",
  "pct_adequado_multisserie_ponderado_2025",

  # Síntese da avaliação de 2026
  "previstos_total_2026",
  "avaliados_total_2026",
  "taxa_participacao_escola_2026",
  "faixa_participacao_escola_2026",
  "proficiencia_multisserie_ponderada_2026",
  "pct_defasagem_multisserie_ponderado_2026",
  "pct_intermediario_multisserie_ponderado_2026",
  "pct_adequado_multisserie_ponderado_2026",

  # Variações agregadas
  "delta_participacao_escola",
  "direcao_delta_participacao_escola",
  "delta_proficiencia_multisserie_ponderada",
  "direcao_delta_proficiencia_multisserie",
  "delta_pct_defasagem_multisserie_ponderado",
  "delta_pct_intermediario_multisserie_ponderado",
  "delta_pct_adequado_multisserie_ponderado",
  "variacao_relativa_previstos_escola",
  "variacao_relativa_avaliados_escola",

  # Heterogeneidade entre séries
  "amplitude_participacao_series_2025",
  "amplitude_participacao_series_2026",
  "desvio_participacao_series_2025",
  "desvio_participacao_series_2026",
  "amplitude_proficiencia_series_2025",
  "amplitude_proficiencia_series_2026",
  "desvio_proficiencia_series_2025",
  "desvio_proficiencia_series_2026",
  "numero_series_delta_proficiencia_positivo",
  "numero_series_delta_proficiencia_negativo",
  "proporcao_series_delta_proficiencia_positivo",

  # Participação e composição
  "numero_series_participacao_abaixo_70_2025",
  "numero_series_participacao_abaixo_70_2026",
  "numero_series_participacao_abaixo_80_2025",
  "numero_series_participacao_abaixo_80_2026",
  "numero_series_participacao_abaixo_90_2025",
  "numero_series_participacao_abaixo_90_2026",
  "numero_series_aumento_participacao_10pp",
  "numero_series_queda_participacao_10pp",
  "numero_series_mudanca_previstos_20pct",
  "numero_series_alerta_composicao",
  "possui_alerta_composicao",
  "observacoes_composicao"
)

colunas_gerenciais_ausentes <- setdiff(
  colunas_perfil_gerencial,
  names(perfil_escola_completo)
)

if (length(colunas_gerenciais_ausentes) > 0) {
  stop(
    "As seguintes colunas previstas para o perfil gerencial estão ausentes:\n",
    paste(
      colunas_gerenciais_ausentes,
      collapse = "\n"
    )
  )
}

perfil_escola_gerencial <- perfil_escola_completo |>
  select(
    all_of(
      colunas_perfil_gerencial
    )
  ) |>
  arrange(
    assessora_gerencial,
    nome_canonico,
    id_escola
  )

# -------------------------------------------------------------------
# 10. Diagnósticos analíticos
# -------------------------------------------------------------------

cobertura_painel_escolas <- perfil_escola_gerencial |>
  count(
    numero_series_comparaveis,
    painel_completo_cinco_series,
    name = "numero_escolas"
  ) |>
  mutate(
    percentual_escolas =
      100 * numero_escolas / sum(numero_escolas)
  ) |>
  arrange(
    desc(numero_series_comparaveis)
  )

write_csv(
  cobertura_painel_escolas,
  file.path(
    pasta_execucao,
    "06_cobertura_painel_por_escola.csv"
  ),
  na = ""
)

escolas_sem_painel_completo <- perfil_escola_gerencial |>
  filter(
    !painel_completo_cinco_series
  ) |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_gerencial,
    numero_series_resultado_2025,
    numero_series_resultado_2026,
    numero_series_comparaveis,
    proporcao_series_comparaveis,
    possui_alerta_composicao
  ) |>
  arrange(
    numero_series_comparaveis,
    nome_canonico
  )

write_csv(
  escolas_sem_painel_completo,
  file.path(
    pasta_execucao,
    "07_escolas_sem_painel_completo.csv"
  ),
  na = ""
)

escolas_alertas_composicao <- perfil_escola_gerencial |>
  filter(
    possui_alerta_composicao
  ) |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_gerencial,
    numero_series_comparaveis,
    numero_series_aumento_participacao_10pp,
    numero_series_queda_participacao_10pp,
    numero_series_mudanca_previstos_20pct,
    numero_series_alerta_composicao,
    variacao_relativa_previstos_escola,
    variacao_relativa_avaliados_escola,
    delta_participacao_escola,
    observacoes_composicao
  ) |>
  arrange(
    desc(numero_series_alerta_composicao),
    nome_canonico
  )

write_csv(
  escolas_alertas_composicao,
  file.path(
    pasta_execucao,
    "08_escolas_com_alertas_composicao.csv"
  ),
  na = ""
)

# Verificação independente dos totais e taxas agregadas.
validacao_agregacoes <- base_final_escola_serie |>
  group_by(
    id_escola
  ) |>
  summarise(
    previstos_2025_recalculado = soma_segura(previstos_2025),
    avaliados_2025_recalculado = soma_segura(avaliados_2025),
    participacao_2025_recalculada = taxa_por_totais(
      avaliados_2025,
      previstos_2025
    ),
    previstos_2026_recalculado = soma_segura(previstos_2026),
    avaliados_2026_recalculado = soma_segura(avaliados_2026),
    participacao_2026_recalculada = taxa_por_totais(
      avaliados_2026,
      previstos_2026
    ),
    .groups = "drop"
  ) |>
  left_join(
    perfil_escola_gerencial |>
      select(
        id_escola,
        previstos_total_2025,
        avaliados_total_2025,
        taxa_participacao_escola_2025,
        previstos_total_2026,
        avaliados_total_2026,
        taxa_participacao_escola_2026
      ),
    by = "id_escola"
  ) |>
  mutate(
    diferenca_previstos_2025 =
      previstos_total_2025 - previstos_2025_recalculado,
    diferenca_avaliados_2025 =
      avaliados_total_2025 - avaliados_2025_recalculado,
    diferenca_participacao_2025 =
      taxa_participacao_escola_2025 - participacao_2025_recalculada,
    diferenca_previstos_2026 =
      previstos_total_2026 - previstos_2026_recalculado,
    diferenca_avaliados_2026 =
      avaliados_total_2026 - avaliados_2026_recalculado,
    diferenca_participacao_2026 =
      taxa_participacao_escola_2026 - participacao_2026_recalculada,
    divergencia = if_any(
      starts_with("diferenca_"),
      ~ !is.na(.x) & abs(.x) > 1e-8
    )
  )

write_csv(
  validacao_agregacoes,
  file.path(
    pasta_execucao,
    "09_validacao_agregacoes_escola.csv"
  ),
  na = ""
)

completude_perfil <- completude_variaveis(
  perfil_escola_gerencial
)

write_csv(
  completude_perfil,
  file.path(
    pasta_execucao,
    "10_completude_perfil_escola_gerencial.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 11. Dicionário do perfil gerencial
# -------------------------------------------------------------------

descricoes_derivadas <- c(
  numero_series_esperadas = "Número de anos escolares distintos representados para a escola no painel atual.",
  numero_series_presentes_2025 = "Número de séries com registro de presença na avaliação de 2025.",
  numero_series_presentes_2026 = "Número de séries com registro de presença na avaliação de 2026.",
  numero_series_resultado_2025 = "Número de séries com resultado disponível em 2025.",
  numero_series_resultado_2026 = "Número de séries com resultado disponível em 2026.",
  numero_series_comparaveis = "Número de séries com resultados disponíveis e comparáveis em 2025 e 2026.",
  proporcao_series_comparaveis = "Percentual das séries representadas que possuem resultados comparáveis nos dois anos.",
  painel_completo_cinco_series = "Indica se a escola possui resultados comparáveis do 1º ao 5º ano.",
  previstos_total_2025 = "Soma dos estudantes previstos nas séries com informação disponível em 2025.",
  avaliados_total_2025 = "Soma dos estudantes avaliados nas séries com informação disponível em 2025.",
  taxa_participacao_escola_2025 = "Participação agregada da escola em 2025, calculada por 100 vezes avaliados totais dividido por previstos totais.",
  proficiencia_multisserie_ponderada_2025 = "Síntese multissérie da proficiência de 2025, ponderada pelo número de estudantes avaliados em cada série.",
  pct_defasagem_multisserie_ponderado_2025 = "Percentual multissérie em defasagem em 2025, ponderado pelos estudantes avaliados.",
  pct_intermediario_multisserie_ponderado_2025 = "Percentual multissérie no padrão intermediário em 2025, ponderado pelos estudantes avaliados.",
  pct_adequado_multisserie_ponderado_2025 = "Percentual multissérie no padrão adequado em 2025, ponderado pelos estudantes avaliados.",
  previstos_total_2026 = "Soma dos estudantes previstos nas séries com informação disponível em 2026.",
  avaliados_total_2026 = "Soma dos estudantes avaliados nas séries com informação disponível em 2026.",
  taxa_participacao_escola_2026 = "Participação agregada da escola em 2026, calculada por 100 vezes avaliados totais dividido por previstos totais.",
  proficiencia_multisserie_ponderada_2026 = "Síntese multissérie da proficiência de 2026, ponderada pelo número de estudantes avaliados em cada série.",
  pct_defasagem_multisserie_ponderado_2026 = "Percentual multissérie em defasagem em 2026, ponderado pelos estudantes avaliados.",
  pct_intermediario_multisserie_ponderado_2026 = "Percentual multissérie no padrão intermediário em 2026, ponderado pelos estudantes avaliados.",
  pct_adequado_multisserie_ponderado_2026 = "Percentual multissérie no padrão adequado em 2026, ponderado pelos estudantes avaliados.",
  delta_participacao_escola = "Diferença, em pontos percentuais, entre as participações agregadas de 2026 e 2025.",
  delta_proficiencia_multisserie_ponderada = "Diferença entre as sínteses multissérie ponderadas de proficiência de 2026 e 2025.",
  delta_pct_defasagem_multisserie_ponderado = "Diferença, em pontos percentuais, do percentual multissérie em defasagem entre 2026 e 2025.",
  delta_pct_intermediario_multisserie_ponderado = "Diferença, em pontos percentuais, do percentual multissérie intermediário entre 2026 e 2025.",
  delta_pct_adequado_multisserie_ponderado = "Diferença, em pontos percentuais, do percentual multissérie adequado entre 2026 e 2025.",
  variacao_relativa_previstos_escola = "Variação percentual do total de estudantes previstos entre 2025 e 2026.",
  variacao_relativa_avaliados_escola = "Variação percentual do total de estudantes avaliados entre 2025 e 2026.",
  amplitude_participacao_series_2025 = "Diferença entre a maior e a menor participação das séries da escola em 2025.",
  amplitude_participacao_series_2026 = "Diferença entre a maior e a menor participação das séries da escola em 2026.",
  amplitude_proficiencia_series_2025 = "Diferença entre a maior e a menor proficiência média das séries da escola em 2025.",
  amplitude_proficiencia_series_2026 = "Diferença entre a maior e a menor proficiência média das séries da escola em 2026.",
  numero_series_delta_proficiencia_positivo = "Número de séries com variação positiva de proficiência entre 2025 e 2026.",
  numero_series_delta_proficiencia_negativo = "Número de séries com variação negativa de proficiência entre 2025 e 2026.",
  proporcao_series_delta_proficiencia_positivo = "Percentual das séries comparáveis com variação positiva de proficiência.",
  numero_series_alerta_composicao = "Número de séries com aumento ou queda de participação de pelo menos 10 p.p. ou variação de previstos superior a 20%.",
  possui_alerta_composicao = "Indica se ao menos uma série apresenta alerta descritivo de mudança de participação ou composição.",
  observacoes_composicao = "Conjunto das observações de composição registradas nas séries da escola."
)

unidades_derivadas <- c(
  taxa_participacao_escola_2025 = "percentual",
  taxa_participacao_escola_2026 = "percentual",
  delta_participacao_escola = "pontos percentuais",
  proficiencia_multisserie_ponderada_2025 = "escala de proficiência",
  proficiencia_multisserie_ponderada_2026 = "escala de proficiência",
  delta_proficiencia_multisserie_ponderada = "pontos da escala de proficiência",
  pct_defasagem_multisserie_ponderado_2025 = "percentual",
  pct_defasagem_multisserie_ponderado_2026 = "percentual",
  pct_intermediario_multisserie_ponderado_2025 = "percentual",
  pct_intermediario_multisserie_ponderado_2026 = "percentual",
  pct_adequado_multisserie_ponderado_2025 = "percentual",
  pct_adequado_multisserie_ponderado_2026 = "percentual",
  delta_pct_defasagem_multisserie_ponderado = "pontos percentuais",
  delta_pct_intermediario_multisserie_ponderado = "pontos percentuais",
  delta_pct_adequado_multisserie_ponderado = "pontos percentuais",
  variacao_relativa_previstos_escola = "percentual",
  variacao_relativa_avaliados_escola = "percentual",
  proporcao_series_comparaveis = "percentual",
  proporcao_series_delta_proficiencia_positivo = "percentual"
)

variaveis_dimensao <- names(
  dim_escola_final
)

variaveis_resumo <- names(
  resumo_avaliacoes_escola
)

perfil_dicionario <- map_dfr(
  seq_along(perfil_escola_gerencial),
  function(i) {
    variavel <- names(perfil_escola_gerencial)[[i]]
    x <- perfil_escola_gerencial[[i]]

    descricao_manual <- unname(
      descricoes_derivadas[variavel]
    )

    descricao <- if (
      length(descricao_manual) == 0 ||
        is.na(descricao_manual[[1]]) ||
        !nzchar(descricao_manual[[1]])
    ) {
      if (variavel %in% variaveis_dimensao) {
        paste0(
          "Variável herdada da dimensão final de escolas: ",
          str_replace_all(
            variavel,
            "_",
            " "
          ),
          ". Consulte o dicionário da base analítica final para a definição operacional completa."
        )
      } else {
        paste0(
          "Indicador derivado no módulo 16: ",
          str_replace_all(
            variavel,
            "_",
            " "
          ),
          ". Consulte o script para a regra de cálculo."
        )
      }
    } else {
      descricao_manual[[1]]
    }

    unidade_manual <- unname(
      unidades_derivadas[variavel]
    )

    unidade <- if (
      length(unidade_manual) == 0 ||
        is.na(unidade_manual[[1]]) ||
        !nzchar(unidade_manual[[1]])
    ) {
      case_when(
        str_detect(variavel, "^numero_|^quantidade_|^previstos_|^avaliados_|^matriculas_|^turmas_|^docentes_") ~ "contagem",
        str_detect(variavel, "^pct_|^taxa_|^proporcao_|variacao_relativa") ~ "percentual",
        str_detect(variavel, "^delta_pct_|delta_participacao") ~ "pontos percentuais",
        is.logical(x) ~ "indicador lógico",
        TRUE ~ "conforme variável de origem"
      )
    } else {
      unidade_manual[[1]]
    }

    origem <- case_when(
      variavel %in% setdiff(
        variaveis_resumo,
        "id_escola"
      ) ~ "derivada no módulo 16",
      variavel %in% variaveis_dimensao ~ "dim_escola_final",
      TRUE ~ "perfil consolidado"
    )

    observacao_metodologica <- case_when(
      str_detect(
        variavel,
        "proficiencia_multisserie|amplitude_proficiencia|desvio_proficiencia"
      ) ~ paste(
        "Síntese entre séries para apoio gerencial;",
        "a interpretação substantiva deve considerar cada ano escolar separadamente."
      ),
      str_detect(
        variavel,
        "delta_|variacao_relativa|alerta_composicao"
      ) ~ paste(
        "Medida descritiva; mudanças de participação e composição",
        "limitam comparações diretas entre anos."
      ),
      variavel == "assessora_gerencial" ~
        "Vínculo administrativo; não representa autoria ou efeito sobre os resultados escolares.",
      TRUE ~ NA_character_
    )

    tibble(
      ordem_coluna = i,
      variavel = variavel,
      origem = origem,
      classe_r = paste(
        class(x),
        collapse = " | "
      ),
      unidade = unidade,
      nivel_analise = "escola",
      descricao = descricao,
      observacao_metodologica = observacao_metodologica,
      valores_ausentes = sum(is.na(x)),
      valores_distintos = n_distinct(
        x,
        na.rm = TRUE
      )
    )
  }
)

# -------------------------------------------------------------------
# 12. Validações finais antes da exportação
# -------------------------------------------------------------------

percentuais_perfil <- names(perfil_escola_gerencial)[
  str_detect(
    names(perfil_escola_gerencial),
    "^pct_|^taxa_|^proporcao_"
  )
]

percentuais_perfil <- setdiff(
  percentuais_perfil,
  c(
    "delta_pct_defasagem_multisserie_ponderado",
    "delta_pct_intermediario_multisserie_ponderado",
    "delta_pct_adequado_multisserie_ponderado"
  )
)

valores_percentuais_fora_faixa <- perfil_escola_gerencial |>
  select(
    any_of(percentuais_perfil)
  ) |>
  pivot_longer(
    everything(),
    names_to = "variavel",
    values_to = "valor"
  ) |>
  filter(
    !is.na(valor),
    valor < -1e-8 |
      valor > 100 + 1e-8
  )

somas_padrao_2025 <- with(
  perfil_escola_gerencial,
  pct_defasagem_multisserie_ponderado_2025 +
    pct_intermediario_multisserie_ponderado_2025 +
    pct_adequado_multisserie_ponderado_2025
)

somas_padrao_2026 <- with(
  perfil_escola_gerencial,
  pct_defasagem_multisserie_ponderado_2026 +
    pct_intermediario_multisserie_ponderado_2026 +
    pct_adequado_multisserie_ponderado_2026
)

divergencias_soma_padroes <- sum(
  (!is.na(somas_padrao_2025) & abs(somas_padrao_2025 - 100) > 1) |
    (!is.na(somas_padrao_2026) & abs(somas_padrao_2026 - 100) > 1)
)

contagens_series_invalidas <- perfil_escola_gerencial |>
  select(
    starts_with("numero_series_")
  ) |>
  pivot_longer(
    everything(),
    names_to = "variavel",
    values_to = "valor"
  ) |>
  filter(
    !is.na(valor),
    valor < 0 |
      valor > 5
  )

validacao_final <- tribble(
  ~teste, ~categoria, ~severidade, ~valor_observado, ~criterio, ~status, ~detalhe,
  "Unicidade da dimensão de entrada", "estrutura", "erro", nrow(duplicidades_dim), "zero duplicidades de id_escola", if_else(nrow(duplicidades_dim) == 0, "aprovado", "reprovado"), "A dimensão final deve possuir uma linha por escola.",
  "Unicidade da base escola × série", "estrutura", "erro", nrow(duplicidades_base), "zero duplicidades da chave escola × série × componente", if_else(nrow(duplicidades_base) == 0, "aprovado", "reprovado"), "A base congelada deve manter chave única.",
  "Correspondência dos IDs entre entradas", "integração", "erro", nrow(comparacao_ids), "zero IDs exclusivos", if_else(nrow(comparacao_ids) == 0, "aprovado", "reprovado"), "As duas entradas devem representar o mesmo conjunto de escolas.",
  "Escopo de séries", "escopo", "erro", sum(!base_final_escola_serie$ano_escolar %in% 1:5), "zero linhas fora do 1º ao 5º ano", if_else(sum(!base_final_escola_serie$ano_escolar %in% 1:5) == 0, "aprovado", "reprovado"), "O módulo atual foi definido para os anos iniciais.",
  "Escopo de componente", "escopo", "erro", n_distinct(base_final_escola_serie$componente), "um único componente curricular", if_else(n_distinct(base_final_escola_serie$componente) == 1, "aprovado", "reprovado"), "Misturar componentes tornaria as sínteses multissérie ambíguas.",
  "Uma linha por escola no perfil completo", "estrutura", "erro", nrow(perfil_escola_completo), paste0(nrow(dim_escola_final), " linhas"), if_else(nrow(perfil_escola_completo) == nrow(dim_escola_final), "aprovado", "reprovado"), "O perfil completo deve preservar todas as escolas da dimensão.",
  "Uma linha por escola no perfil gerencial", "estrutura", "erro", n_distinct(perfil_escola_gerencial$id_escola), paste0(nrow(dim_escola_final), " IDs únicos"), if_else(n_distinct(perfil_escola_gerencial$id_escola) == nrow(dim_escola_final), "aprovado", "reprovado"), "O perfil gerencial deve ter exatamente uma linha por escola.",
  "Divergências nas agregações", "consistência", "erro", sum(validacao_agregacoes$divergencia), "zero divergências superiores a 1e-8", if_else(sum(validacao_agregacoes$divergencia) == 0, "aprovado", "reprovado"), "Totais e participações são recalculados independentemente.",
  "Percentuais fora de 0 a 100", "domínio", "erro", nrow(valores_percentuais_fora_faixa), "zero valores fora da faixa", if_else(nrow(valores_percentuais_fora_faixa) == 0, "aprovado", "reprovado"), "Não inclui deltas, que podem ser negativos.",
  "Contagens de séries fora de 0 a 5", "domínio", "erro", nrow(contagens_series_invalidas), "zero contagens inválidas", if_else(nrow(contagens_series_invalidas) == 0, "aprovado", "reprovado"), "O painel contém cinco anos escolares.",
  "Soma dos padrões multissérie", "consistência", "aviso", divergencias_soma_padroes, "preferencialmente zero divergências superiores a 1 p.p.", if_else(divergencias_soma_padroes == 0, "aprovado", "atenção"), "Pequenas diferenças podem decorrer de arredondamento nas bases de origem.",
  "Vínculo gerencial preenchido", "administrativo", "erro", sum(is.na(perfil_escola_gerencial$assessora_gerencial) | str_squish(perfil_escola_gerencial$assessora_gerencial) == ""), "zero escolas sem categoria gerencial", if_else(sum(is.na(perfil_escola_gerencial$assessora_gerencial) | str_squish(perfil_escola_gerencial$assessora_gerencial) == "") == 0, "aprovado", "reprovado"), "Ausências do vínculo original devem estar explicitadas na categoria gerencial.",
  "Escolas sem painel completo", "cobertura", "aviso", sum(!perfil_escola_gerencial$painel_completo_cinco_series), "documentar escolas sem cinco séries comparáveis", "aprovado", "Ausência de painel completo não elimina a escola do perfil estrutural.",
  "Escolas com alerta de composição", "interpretação", "aviso", sum(perfil_escola_gerencial$possui_alerta_composicao), "documentar mudanças relevantes", "aprovado", "Alertas não invalidam resultados, mas condicionam a comparação temporal."
)

write_csv(
  validacao_final,
  file.path(
    pasta_execucao,
    "11_validacao_final.csv"
  ),
  na = ""
)

erros_criticos <- validacao_final |>
  filter(
    severidade == "erro",
    status == "reprovado"
  )

if (nrow(erros_criticos) > 0) {
  stop(
    "O módulo 16 encontrou erros críticos e não exportará os produtos canônicos. Consulte: ",
    file.path(
      pasta_execucao,
      "11_validacao_final.csv"
    )
  )
}

# -------------------------------------------------------------------
# 13. Exportação com preservação de versões anteriores
# -------------------------------------------------------------------

walk(
  unname(arquivos_saida),
  ~ arquivar_se_existir(
    .x,
    pasta_historico
  )
)

write_csv(
  perfil_escola_completo,
  arquivos_saida[["perfil_completo_csv"]],
  na = ""
)

saveRDS(
  perfil_escola_completo,
  arquivos_saida[["perfil_completo_rds"]]
)

write_csv(
  perfil_escola_gerencial,
  arquivos_saida[["perfil_gerencial_csv"]],
  na = ""
)

saveRDS(
  perfil_escola_gerencial,
  arquivos_saida[["perfil_gerencial_rds"]]
)

write_csv(
  perfil_escola_serie_compacto,
  arquivos_saida[["perfil_serie_csv"]],
  na = ""
)

saveRDS(
  perfil_escola_serie_compacto,
  arquivos_saida[["perfil_serie_rds"]]
)

write_csv(
  perfil_dicionario,
  arquivos_saida[["dicionario_csv"]],
  na = ""
)

estrutura_saidas <- bind_rows(
  inventariar_estrutura(
    perfil_escola_completo,
    "perfil_escola_completo"
  ),
  inventariar_estrutura(
    perfil_escola_gerencial,
    "perfil_escola_gerencial"
  ),
  inventariar_estrutura(
    perfil_escola_serie_compacto,
    "perfil_escola_serie_compacto"
  )
)

write_csv(
  estrutura_saidas,
  file.path(
    pasta_execucao,
    "12_estrutura_bases_saida.csv"
  ),
  na = ""
)

manifesto_entradas <- map_dfr(
  seq_along(caminhos_entrada_utilizados),
  function(i) {
    nome <- names(caminhos_entrada_utilizados)[[i]]
    caminho <- caminhos_entrada_utilizados[[i]]

    manifestar_arquivo(
      nome,
      caminho
    )
  }
)

write_csv(
  manifesto_entradas,
  file.path(
    pasta_execucao,
    "13_manifesto_arquivos_entrada.csv"
  ),
  na = ""
)

manifesto_produtos <- map_dfr(
  seq_along(arquivos_saida),
  function(i) {
    nome <- names(arquivos_saida)[[i]]
    caminho <- arquivos_saida[[i]]

    manifestar_arquivo(
      nome,
      caminho
    )
  }
)

write_csv(
  manifesto_produtos,
  file.path(
    pasta_execucao,
    "14_manifesto_produtos_modulo_16.csv"
  ),
  na = ""
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    pasta_execucao,
    "15_session_info.txt"
  )
)

resumo_execucao <- c(
  paste0("Execução: ", id_execucao),
  paste0("Escolas na dimensão de entrada: ", nrow(dim_escola_final)),
  paste0("Linhas na base escola × série: ", nrow(base_final_escola_serie)),
  paste0("Escolas no perfil completo: ", nrow(perfil_escola_completo)),
  paste0("Escolas no perfil gerencial: ", nrow(perfil_escola_gerencial)),
  paste0("Colunas no perfil gerencial: ", ncol(perfil_escola_gerencial)),
  paste0("Escolas com cinco séries comparáveis: ", sum(perfil_escola_gerencial$painel_completo_cinco_series)),
  paste0("Escolas sem cinco séries comparáveis: ", sum(!perfil_escola_gerencial$painel_completo_cinco_series)),
  paste0("Escolas com alerta de composição: ", sum(perfil_escola_gerencial$possui_alerta_composicao)),
  paste0("Erros críticos: ", nrow(erros_criticos)),
  "",
  "Observações metodológicas:",
  "- O perfil é descritivo e observacional.",
  "- A assessora representa vínculo administrativo, não efeito sobre resultados.",
  "- Sínteses multissérie devem ser lidas em conjunto com resultados por série.",
  "- Mudanças de participação e composição condicionam comparações temporais.",
  "- O módulo não produz ranking nem índice de complexidade."
)

writeLines(
  resumo_execucao,
  file.path(
    pasta_execucao,
    "16_resumo_execucao.txt"
  )
)

message(
  "Módulo 16 concluído com sucesso.\n",
  "Perfil gerencial: ",
  arquivos_saida[["perfil_gerencial_csv"]],
  "\nDiagnósticos: ",
  pasta_execucao
)
