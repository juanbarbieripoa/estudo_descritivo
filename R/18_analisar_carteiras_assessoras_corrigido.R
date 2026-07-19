# ===================================================================
# 18_analisar_carteiras_assessoras.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# ===================================================================
#
# OBJETIVO ANALÍTICO
#
# Descrever a distribuição da carga de assessoramento entre as carteiras,
# combinando três perspectivas complementares:
#
#   1. carga extensiva: número de escolas, matrículas, turmas e cobertura;
#   2. carga potencial acumulada: soma dos escores escolares do módulo 17;
#   3. composição da carteira: complexidade média, dispersão, dimensões,
#      concentração de casos desafiadores e sensibilidade aos pesos.
#
# O módulo NÃO avalia a qualidade, produtividade ou efetividade das
# assessoras. O vínculo entre escola e assessora é administrativo, e os
# resultados escolares não são interpretados como efeito do assessoramento.
#
# PRINCÍPIOS METODOLÓGICOS
#
# - A quantidade de escolas é apresentada separadamente da complexidade.
# - A carga potencial acumulada é a soma do índice escolar e, portanto,
#   combina extensão e composição da carteira.
# - Médias e medianas descrevem a complexidade típica das escolas, mas não
#   substituem a carga total acumulada.
# - As quatro dimensões do índice permanecem visíveis separadamente.
# - Casos administrativos especiais, evidência educacional limitada e
#   sensibilidade aos pesos são sinalizados explicitamente.
# - As faixas do índice são quartis relativos, não categorias absolutas.
# - Referências comparativas são calculadas somente entre carteiras nominais.
# - Os agrupamentos "outras" e "Sem vinculação informada" são preservados,
#   mas não entram nas médias de referência entre carteiras nominais.
# - Qualquer redistribuição exige validação qualitativa da gestão, das
#   assessoras e das condições territoriais e operacionais.
#
# ENTRADAS
#
# dados_finais/indice_carga_potencial_escola.rds ou .csv
# dados_finais/perfil_escola_gerencial.rds ou .csv
#
# PRODUTOS PRINCIPAIS
#
# dados_finais/analise_carteiras_assessoras.csv e .rds
# dados_finais/carteira_escola_detalhe.csv e .rds
# dados_finais/analise_carteiras_cenarios.csv e .rds
# dados_finais/composicao_faixas_carteiras.csv e .rds
# documentacao/analise_carteiras/dicionario_analise_carteiras.csv
# documentacao/analise_carteiras/execucao_<data_hora>/...
# dados_finais/historico/analise_carteiras/execucao_<data_hora>/...
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
  "analise_carteiras",
  paste0("execucao_", id_execucao)
)

pasta_documentacao <- here(
  "documentacao",
  "analise_carteiras"
)

pasta_execucao <- here(
  "documentacao",
  "analise_carteiras",
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
  indice_escola = here(
    "dados_finais",
    "indice_carga_potencial_escola.rds"
  ),
  perfil_escola = here(
    "dados_finais",
    "perfil_escola_gerencial.rds"
  )
)

arquivos_entrada_csv <- c(
  indice_escola = here(
    "dados_finais",
    "indice_carga_potencial_escola.csv"
  ),
  perfil_escola = here(
    "dados_finais",
    "perfil_escola_gerencial.csv"
  )
)

arquivos_saida <- c(
  analise_carteiras_csv = here(
    "dados_finais",
    "analise_carteiras_assessoras.csv"
  ),
  analise_carteiras_rds = here(
    "dados_finais",
    "analise_carteiras_assessoras.rds"
  ),
  detalhe_escolas_csv = here(
    "dados_finais",
    "carteira_escola_detalhe.csv"
  ),
  detalhe_escolas_rds = here(
    "dados_finais",
    "carteira_escola_detalhe.rds"
  ),
  cenarios_csv = here(
    "dados_finais",
    "analise_carteiras_cenarios.csv"
  ),
  cenarios_rds = here(
    "dados_finais",
    "analise_carteiras_cenarios.rds"
  ),
  composicao_faixas_csv = here(
    "dados_finais",
    "composicao_faixas_carteiras.csv"
  ),
  composicao_faixas_rds = here(
    "dados_finais",
    "composicao_faixas_carteiras.rds"
  ),
  dicionario_csv = here(
    "documentacao",
    "analise_carteiras",
    "dicionario_analise_carteiras.csv"
  )
)

# -------------------------------------------------------------------
# 3. Funções auxiliares
# -------------------------------------------------------------------

ler_base_preferindo_rds <- function(nome_fonte) {
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
      "\nExecute primeiro os módulos 16 e 17."
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

as_logical_seguro <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  texto <- str_to_lower(
    str_trim(
      as.character(x)
    )
  )

  case_when(
    is.na(x) ~ NA,
    texto %in% c("true", "t", "1", "sim", "s", "verdadeiro") ~ TRUE,
    texto %in% c("false", "f", "0", "não", "nao", "n", "falso") ~ FALSE,
    TRUE ~ NA
  )
}

soma_segura <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )

  if (all(is.na(x))) {
    return(NA_real_)
  }

  sum(x, na.rm = TRUE)
}

media_segura <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  mean(x)
}

mediana_segura <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  median(x)
}

desvio_seguro <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  x <- x[!is.na(x)]

  if (length(x) < 2) {
    return(NA_real_)
  }

  sd(x)
}

minimo_seguro <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  min(x)
}

maximo_seguro <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  max(x)
}

amplitude_segura <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  max(x) - min(x)
}

intervalo_interquartil_seguro <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  x <- x[!is.na(x)]

  if (length(x) < 2) {
    return(NA_real_)
  }

  IQR(x)
}

media_ponderada_segura <- function(x, w) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  w <- suppressWarnings(
    as.numeric(w)
  )

  valido <- !is.na(x) & !is.na(w) & w > 0

  if (!any(valido)) {
    return(NA_real_)
  }

  weighted.mean(
    x[valido],
    w[valido]
  )
}

percentil_relativo <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )

  valido <- !is.na(x)
  resultado <- rep(
    NA_real_,
    length(x)
  )
  n_valido <- sum(valido)

  if (n_valido == 0) {
    return(resultado)
  }

  if (n_valido == 1) {
    resultado[valido] <- 50
    return(resultado)
  }

  posicao <- rank(
    x[valido],
    ties.method = "average"
  )

  resultado[valido] <- 100 * (
    posicao - 1
  ) / (
    n_valido - 1
  )

  resultado
}

percentil_condicional <- function(x, elegivel) {
  resultado <- rep(
    NA_real_,
    length(x)
  )

  elegivel <- !is.na(elegivel) & elegivel

  if (any(elegivel)) {
    resultado[elegivel] <- percentil_relativo(
      x[elegivel]
    )
  }

  resultado
}

atribuir_faixa_carteira <- function(percentil) {
  case_when(
    is.na(percentil) ~ NA_character_,
    percentil <= 25 ~ "Faixa 1 — menor carga relativa entre carteiras nominais",
    percentil <= 50 ~ "Faixa 2 — intermediária inferior entre carteiras nominais",
    percentil <= 75 ~ "Faixa 3 — intermediária superior entre carteiras nominais",
    TRUE ~ "Faixa 4 — maior carga relativa entre carteiras nominais"
  )
}

calcular_hhi <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  x <- x[!is.na(x) & x >= 0]

  if (length(x) == 0 || sum(x) <= 0) {
    return(NA_real_)
  }

  participacoes <- x / sum(x)
  sum(participacoes^2) * 10000
}

participacao_maiores <- function(x, n = 1) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  x <- x[!is.na(x) & x >= 0]

  if (length(x) == 0 || sum(x) <= 0) {
    return(NA_real_)
  }

  100 * sum(
    head(
      sort(
        x,
        decreasing = TRUE
      ),
      n
    )
  ) / sum(x)
}

arquivar_se_existir <- function(caminho) {
  if (!file.exists(caminho)) {
    return(invisible(FALSE))
  }

  destino <- file.path(
    pasta_historico,
    basename(caminho)
  )

  sucesso <- file.copy(
    caminho,
    destino,
    overwrite = FALSE
  )

  if (!sucesso) {
    stop(
      "Não foi possível arquivar a versão anterior de: ",
      caminho
    )
  }

  invisible(TRUE)
}

estrutura_base <- function(dados, nome_base) {
  tibble(
    base = nome_base,
    ordem_coluna = seq_along(dados),
    variavel = names(dados),
    classe_r = map_chr(
      dados,
      ~ paste(
        class(.x),
        collapse = " | "
      )
    ),
    valores_ausentes = map_int(
      dados,
      ~ sum(is.na(.x))
    ),
    valores_distintos = map_int(
      dados,
      ~ n_distinct(
        .x,
        na.rm = TRUE
      )
    )
  )
}

# -------------------------------------------------------------------
# 4. Leitura das bases
# -------------------------------------------------------------------

indice_escola <- ler_base_preferindo_rds(
  "indice_escola"
)

perfil_escola <- ler_base_preferindo_rds(
  "perfil_escola"
)

caminhos_entrada_usados <- c(
  indice_escola = attr(
    indice_escola,
    "caminho_origem"
  ),
  perfil_escola = attr(
    perfil_escola,
    "caminho_origem"
  )
)

# -------------------------------------------------------------------
# 5. Validação das estruturas de entrada
# -------------------------------------------------------------------

colunas_obrigatorias_indice <- c(
  "id_escola",
  "codigo_inep",
  "nome_canonico",
  "assessora_gerencial",
  "matriculas_anos_iniciais",
  "turmas_anos_iniciais",
  "numero_etapas_amplas_ofertadas",
  "numero_series_resultado_2026",
  "numero_series_comparaveis",
  "painel_completo_cinco_series",
  "qualidade_evidencia_educacional",
  "interpretacao_educacional_cautelosa",
  "score_dimensao_volume",
  "score_dimensao_estrutural",
  "score_dimensao_educacional",
  "score_dimensao_administrativa",
  "indice_carga_potencial_equilibrado",
  "indice_carga_potencial_operacional",
  "indice_carga_potencial_desafio_educacional",
  "indice_carga_potencial_transicao_administrativa",
  "faixa_indice_equilibrado",
  "faixa_indice_operacional",
  "faixa_indice_desafio_educacional",
  "faixa_indice_transicao_administrativa",
  "faixa_indice_carga_potencial",
  "indice_carga_potencial",
  "percentil_indice_carga_potencial",
  "contribuicao_volume_indice_principal",
  "contribuicao_estrutural_indice_principal",
  "contribuicao_educacional_indice_principal",
  "contribuicao_administrativa_indice_principal",
  "interpretacao_indice_cautelosa"
)

colunas_obrigatorias_perfil <- c(
  "id_escola",
  "codigo_inep",
  "nome_canonico",
  "assessora_gerencial",
  "tipo_vinculo_rede_final",
  "status_rede_2025_final",
  "municipalizada_apos_2024",
  "possivel_municipalizacao_recente",
  "escola_nova_recente",
  "privada_vinculada_final",
  "requer_revisao_tecnica",
  "divergencia_censo_cadastro",
  "previstos_total_2026",
  "avaliados_total_2026",
  "taxa_participacao_escola_2026",
  "proficiencia_multisserie_ponderada_2026",
  "pct_defasagem_multisserie_ponderado_2026",
  "pct_intermediario_multisserie_ponderado_2026",
  "pct_adequado_multisserie_ponderado_2026",
  "numero_series_alerta_composicao",
  "possui_alerta_composicao"
)

colunas_ausentes_indice <- setdiff(
  colunas_obrigatorias_indice,
  names(indice_escola)
)

colunas_ausentes_perfil <- setdiff(
  colunas_obrigatorias_perfil,
  names(perfil_escola)
)

if (length(colunas_ausentes_indice) > 0) {
  stop(
    "Colunas obrigatórias ausentes no índice escolar: ",
    paste(
      colunas_ausentes_indice,
      collapse = ", "
    )
  )
}

if (length(colunas_ausentes_perfil) > 0) {
  stop(
    "Colunas obrigatórias ausentes no perfil escolar: ",
    paste(
      colunas_ausentes_perfil,
      collapse = ", "
    )
  )
}

if (anyDuplicated(indice_escola$id_escola) > 0) {
  stop(
    "O índice escolar possui mais de uma linha por escola."
  )
}

if (anyDuplicated(perfil_escola$id_escola) > 0) {
  stop(
    "O perfil escolar possui mais de uma linha por escola."
  )
}

if (!setequal(
  indice_escola$id_escola,
  perfil_escola$id_escola
)) {
  stop(
    "O conjunto de escolas difere entre o índice e o perfil escolar."
  )
}

comparacao_atributos_chave <- indice_escola |>
  select(
    id_escola,
    codigo_inep_indice = codigo_inep,
    nome_indice = nome_canonico,
    assessora_indice = assessora_gerencial
  ) |>
  inner_join(
    perfil_escola |>
      select(
        id_escola,
        codigo_inep_perfil = codigo_inep,
        nome_perfil = nome_canonico,
        assessora_perfil = assessora_gerencial
      ),
    by = "id_escola"
  ) |>
  mutate(
    conflito_codigo_inep =
      as.character(codigo_inep_indice) != as.character(codigo_inep_perfil),
    conflito_nome = nome_indice != nome_perfil,
    conflito_assessora = assessora_indice != assessora_perfil
  )

if (any(
  comparacao_atributos_chave$conflito_codigo_inep |
    comparacao_atributos_chave$conflito_nome |
    comparacao_atributos_chave$conflito_assessora,
  na.rm = TRUE
)) {
  stop(
    "Há divergências de identificação ou vinculação entre as entradas."
  )
}

colunas_logicas_indice <- c(
  "painel_completo_cinco_series",
  "interpretacao_educacional_cautelosa",
  "interpretacao_indice_cautelosa"
)

colunas_logicas_perfil <- c(
  "municipalizada_apos_2024",
  "possivel_municipalizacao_recente",
  "escola_nova_recente",
  "privada_vinculada_final",
  "requer_revisao_tecnica",
  "divergencia_censo_cadastro",
  "possui_alerta_composicao"
)

indice_escola <- indice_escola |>
  mutate(
    across(
      all_of(colunas_logicas_indice),
      as_logical_seguro
    )
  )

perfil_escola <- perfil_escola |>
  mutate(
    across(
      all_of(colunas_logicas_perfil),
      as_logical_seguro
    )
  )

if (any(
  is.na(indice_escola$assessora_gerencial) |
    str_trim(indice_escola$assessora_gerencial) == ""
)) {
  stop(
    "Há escolas sem categoria preenchida em `assessora_gerencial`."
  )
}

# -------------------------------------------------------------------
# 6. Parâmetros explícitos da análise
# -------------------------------------------------------------------

rotulo_faixa_1 <- "Faixa 1 — menor carga potencial relativa"
rotulo_faixa_2 <- "Faixa 2 — intermediária inferior"
rotulo_faixa_3 <- "Faixa 3 — intermediária superior"
rotulo_faixa_4 <- "Faixa 4 — maior carga potencial relativa"

parametros_analise <- tribble(
  ~parametro, ~valor, ~justificativa,
  "carteira_nominal", "assessora_gerencial diferente de 'outras' e 'Sem vinculação informada'", "Somente carteiras nominais entram nas médias e percentis de referência entre carteiras comparáveis.",
  "carga_potencial_acumulada", "soma do índice escolar", "Combina quantidade de escolas e carga potencial relativa de cada unidade.",
  "complexidade_tipica", "média e mediana do índice escolar", "Descreve a composição típica da carteira sem substituir a carga total.",
  "caso_maior_carga_relativa", rotulo_faixa_4, "Faixa superior é quartil relativo, não categoria absoluta.",
  "caso_administrativo_especial", "score_dimensao_administrativa > 0", "Identifica presença de componente administrativo especial no índice.",
  "sensibilidade_elevada", "mudança de faixa em dois ou três cenários", "Sinaliza dependência relevante da especificação de pesos.",
  "concentracao_interna", "participação das maiores escolas e HHI", "Mostra se a carga acumulada está concentrada em poucos casos dentro da carteira.",
  "resultados_educacionais", "sínteses ponderadas de 2026", "São descritivos e não representam efeito ou qualidade da assessora."
)

# -------------------------------------------------------------------
# 7. Base integrada escola × carteira
# -------------------------------------------------------------------

perfil_complementar <- perfil_escola |>
  select(
    id_escola,
    tipo_vinculo_rede_final,
    status_rede_2025_final,
    municipalizada_apos_2024,
    possivel_municipalizacao_recente,
    escola_nova_recente,
    privada_vinculada_final,
    requer_revisao_tecnica,
    divergencia_censo_cadastro,
    previstos_total_2026,
    avaliados_total_2026,
    taxa_participacao_escola_2026,
    proficiencia_multisserie_ponderada_2026,
    pct_defasagem_multisserie_ponderado_2026,
    pct_intermediario_multisserie_ponderado_2026,
    pct_adequado_multisserie_ponderado_2026,
    numero_series_alerta_composicao,
    possui_alerta_composicao
  )

base_escola_carteira <- indice_escola |>
  left_join(
    perfil_complementar,
    by = "id_escola"
  ) |>
  mutate(
    tipo_carteira = case_when(
      assessora_gerencial == "Sem vinculação informada" ~
        "Sem vinculação informada",
      assessora_gerencial == "outras" ~
        "Agrupamento residual",
      TRUE ~ "Carteira nominal"
    ),
    carteira_nominal = tipo_carteira == "Carteira nominal",
    turmas_anos_iniciais_validas = if_else(
      !is.na(matriculas_anos_iniciais) &
        matriculas_anos_iniciais > 0 &
        !is.na(turmas_anos_iniciais) &
        turmas_anos_iniciais <= 0,
      NA_real_,
      as.numeric(turmas_anos_iniciais)
    ),
    numero_cenarios_com_mudanca_faixa =
      as.integer(faixa_indice_operacional != faixa_indice_equilibrado) +
      as.integer(
        faixa_indice_desafio_educacional != faixa_indice_equilibrado
      ) +
      as.integer(
        faixa_indice_transicao_administrativa != faixa_indice_equilibrado
      ),
    sensibilidade_faixa = case_when(
      numero_cenarios_com_mudanca_faixa == 0 ~
        "Estável nos cenários",
      numero_cenarios_com_mudanca_faixa == 1 ~
        "Sensibilidade moderada",
      numero_cenarios_com_mudanca_faixa >= 2 ~
        "Sensibilidade elevada",
      TRUE ~ NA_character_
    ),
    possui_complexidade_administrativa =
      !is.na(score_dimensao_administrativa) &
      score_dimensao_administrativa > 0,
    painel_incompleto = !coalesce(
      painel_completo_cinco_series,
      FALSE
    ),
    participacao_2026_abaixo_80 =
      !is.na(taxa_participacao_escola_2026) &
      taxa_participacao_escola_2026 < 80,
    caso_para_leitura_detalhada =
      faixa_indice_carga_potencial == rotulo_faixa_4 |
      coalesce(interpretacao_indice_cautelosa, FALSE) |
      possui_complexidade_administrativa |
      sensibilidade_faixa == "Sensibilidade elevada"
  )

if (any(is.na(base_escola_carteira$tipo_vinculo_rede_final))) {
  stop(
    "O cruzamento com o perfil escolar gerou valores ausentes inesperados."
  )
}

# -------------------------------------------------------------------
# 8. Detalhamento das escolas dentro de cada carteira
# -------------------------------------------------------------------

carteira_escola_detalhe <- base_escola_carteira |>
  group_by(
    assessora_gerencial
  ) |>
  arrange(
    desc(indice_carga_potencial),
    nome_canonico,
    .by_group = TRUE
  ) |>
  mutate(
    ordem_interna_carga_potencial = row_number(),
    participacao_indice_na_carteira_pct = {
      total_indice_carteira <- sum(
        indice_carga_potencial,
        na.rm = TRUE
      )

      if (
        is.finite(total_indice_carteira) &&
          total_indice_carteira > 0
      ) {
        100 * indice_carga_potencial /
          total_indice_carteira
      } else {
        rep(NA_real_, n())
      }
    },
    participacao_acumulada_indice_carteira_pct = cumsum(
      coalesce(
        participacao_indice_na_carteira_pct,
        0
      )
    ),
    entre_duas_maiores_cargas_da_carteira =
      ordem_interna_carga_potencial <= 2
  ) |>
  ungroup() |>
  select(
    assessora_gerencial,
    tipo_carteira,
    carteira_nominal,
    ordem_interna_carga_potencial,
    id_escola,
    codigo_inep,
    nome_canonico,
    matriculas_anos_iniciais,
    turmas_anos_iniciais,
    turmas_anos_iniciais_validas,
    numero_etapas_amplas_ofertadas,
    numero_series_resultado_2026,
    numero_series_comparaveis,
    painel_completo_cinco_series,
    qualidade_evidencia_educacional,
    interpretacao_educacional_cautelosa,
    score_dimensao_volume,
    score_dimensao_estrutural,
    score_dimensao_educacional,
    score_dimensao_administrativa,
    indice_carga_potencial,
    percentil_indice_carga_potencial,
    faixa_indice_carga_potencial,
    indice_carga_potencial_operacional,
    faixa_indice_operacional,
    indice_carga_potencial_desafio_educacional,
    faixa_indice_desafio_educacional,
    indice_carga_potencial_transicao_administrativa,
    faixa_indice_transicao_administrativa,
    contribuicao_volume_indice_principal,
    contribuicao_estrutural_indice_principal,
    contribuicao_educacional_indice_principal,
    contribuicao_administrativa_indice_principal,
    numero_cenarios_com_mudanca_faixa,
    sensibilidade_faixa,
    possui_complexidade_administrativa,
    interpretacao_indice_cautelosa,
    painel_incompleto,
    possui_alerta_composicao,
    participacao_2026_abaixo_80,
    caso_para_leitura_detalhada,
    entre_duas_maiores_cargas_da_carteira,
    participacao_indice_na_carteira_pct,
    participacao_acumulada_indice_carteira_pct,
    tipo_vinculo_rede_final,
    status_rede_2025_final,
    municipalizada_apos_2024,
    escola_nova_recente
  ) |>
  arrange(
    tipo_carteira,
    assessora_gerencial,
    ordem_interna_carga_potencial
  )

# -------------------------------------------------------------------
# 9. Composição das faixas do índice por carteira
# -------------------------------------------------------------------

niveis_faixa_escola <- c(
  rotulo_faixa_1,
  rotulo_faixa_2,
  rotulo_faixa_3,
  rotulo_faixa_4
)

composicao_faixas_carteiras <- base_escola_carteira |>
  mutate(
    faixa_indice_carga_potencial = factor(
      faixa_indice_carga_potencial,
      levels = niveis_faixa_escola
    )
  ) |>
  count(
    assessora_gerencial,
    tipo_carteira,
    carteira_nominal,
    faixa_indice_carga_potencial,
    .drop = FALSE,
    name = "numero_escolas"
  ) |>
  group_by(
    assessora_gerencial
  ) |>
  mutate(
    total_escolas_carteira = sum(numero_escolas),
    percentual_escolas_carteira = if_else(
      total_escolas_carteira > 0,
      100 * numero_escolas / total_escolas_carteira,
      NA_real_
    )
  ) |>
  ungroup() |>
  mutate(
    faixa_indice_carga_potencial = as.character(
      faixa_indice_carga_potencial
    )
  ) |>
  arrange(
    tipo_carteira,
    assessora_gerencial,
    match(
      faixa_indice_carga_potencial,
      niveis_faixa_escola
    )
  )

# -------------------------------------------------------------------
# 10. Síntese principal por carteira
# -------------------------------------------------------------------

analise_carteiras <- base_escola_carteira |>
  group_by(
    assessora_gerencial,
    tipo_carteira,
    carteira_nominal
  ) |>
  summarise(
    numero_escolas = n(),
    matriculas_anos_iniciais_total = soma_segura(
      matriculas_anos_iniciais
    ),
    escolas_com_matriculas_disponiveis = sum(
      !is.na(matriculas_anos_iniciais)
    ),
    turmas_anos_iniciais_total = soma_segura(
      turmas_anos_iniciais_validas
    ),
    escolas_com_turmas_disponiveis = sum(
      !is.na(turmas_anos_iniciais_validas)
    ),
    etapas_amplas_total = soma_segura(
      numero_etapas_amplas_ofertadas
    ),
    series_com_resultado_2026_total = soma_segura(
      numero_series_resultado_2026
    ),
    series_comparaveis_total = soma_segura(
      numero_series_comparaveis
    ),
    escolas_painel_completo = sum(
      coalesce(
        painel_completo_cinco_series,
        FALSE
      )
    ),
    escolas_painel_incompleto = sum(
      painel_incompleto
    ),
    previstos_total_2026_carteira = soma_segura(
      previstos_total_2026
    ),
    avaliados_total_2026_carteira = soma_segura(
      avaliados_total_2026
    ),
    taxa_participacao_agregada_2026 = if_else(
      !is.na(previstos_total_2026_carteira) &
        previstos_total_2026_carteira > 0,
      100 * avaliados_total_2026_carteira /
        previstos_total_2026_carteira,
      NA_real_
    ),
    proficiencia_multisserie_2026_ponderada =
      media_ponderada_segura(
        proficiencia_multisserie_ponderada_2026,
        avaliados_total_2026
      ),
    pct_defasagem_multisserie_2026_ponderado =
      media_ponderada_segura(
        pct_defasagem_multisserie_ponderado_2026,
        avaliados_total_2026
      ),
    pct_intermediario_multisserie_2026_ponderado =
      media_ponderada_segura(
        pct_intermediario_multisserie_ponderado_2026,
        avaliados_total_2026
      ),
    pct_adequado_multisserie_2026_ponderado =
      media_ponderada_segura(
        pct_adequado_multisserie_ponderado_2026,
        avaliados_total_2026
      ),
    carga_potencial_total = soma_segura(
      indice_carga_potencial
    ),
    indice_carga_potencial_medio = media_segura(
      indice_carga_potencial
    ),
    indice_carga_potencial_mediano = mediana_segura(
      indice_carga_potencial
    ),
    indice_carga_potencial_desvio = desvio_seguro(
      indice_carga_potencial
    ),
    indice_carga_potencial_iqr =
      intervalo_interquartil_seguro(
        indice_carga_potencial
      ),
    indice_carga_potencial_minimo = minimo_seguro(
      indice_carga_potencial
    ),
    indice_carga_potencial_maximo = maximo_seguro(
      indice_carga_potencial
    ),
    indice_carga_potencial_amplitude = amplitude_segura(
      indice_carga_potencial
    ),
    coeficiente_variacao_indice = if_else(
      !is.na(indice_carga_potencial_medio) &
        indice_carga_potencial_medio != 0 &
        !is.na(indice_carga_potencial_desvio),
      indice_carga_potencial_desvio /
        indice_carga_potencial_medio,
      NA_real_
    ),
    dimensao_volume_media = media_segura(
      score_dimensao_volume
    ),
    dimensao_estrutural_media = media_segura(
      score_dimensao_estrutural
    ),
    dimensao_educacional_media = media_segura(
      score_dimensao_educacional
    ),
    dimensao_administrativa_media = media_segura(
      score_dimensao_administrativa
    ),
    contribuicao_volume_total = soma_segura(
      contribuicao_volume_indice_principal
    ),
    contribuicao_estrutural_total = soma_segura(
      contribuicao_estrutural_indice_principal
    ),
    contribuicao_educacional_total = soma_segura(
      contribuicao_educacional_indice_principal
    ),
    contribuicao_administrativa_total = soma_segura(
      contribuicao_administrativa_indice_principal
    ),
    numero_escolas_faixa_1 = sum(
      faixa_indice_carga_potencial == rotulo_faixa_1,
      na.rm = TRUE
    ),
    numero_escolas_faixa_2 = sum(
      faixa_indice_carga_potencial == rotulo_faixa_2,
      na.rm = TRUE
    ),
    numero_escolas_faixa_3 = sum(
      faixa_indice_carga_potencial == rotulo_faixa_3,
      na.rm = TRUE
    ),
    numero_escolas_faixa_4 = sum(
      faixa_indice_carga_potencial == rotulo_faixa_4,
      na.rm = TRUE
    ),
    percentual_escolas_faixa_4 = 100 *
      numero_escolas_faixa_4 / numero_escolas,
    numero_escolas_faixas_3_4 =
      numero_escolas_faixa_3 + numero_escolas_faixa_4,
    percentual_escolas_faixas_3_4 = 100 *
      numero_escolas_faixas_3_4 / numero_escolas,
    numero_escolas_complexidade_administrativa = sum(
      possui_complexidade_administrativa,
      na.rm = TRUE
    ),
    numero_escolas_interpretacao_cautelosa = sum(
      coalesce(
        interpretacao_indice_cautelosa,
        FALSE
      )
    ),
    numero_escolas_sensibilidade_elevada = sum(
      sensibilidade_faixa == "Sensibilidade elevada",
      na.rm = TRUE
    ),
    numero_escolas_alerta_composicao = sum(
      coalesce(
        possui_alerta_composicao,
        FALSE
      )
    ),
    numero_escolas_participacao_2026_abaixo_80 = sum(
      participacao_2026_abaixo_80,
      na.rm = TRUE
    ),
    numero_escolas_leitura_detalhada = sum(
      caso_para_leitura_detalhada,
      na.rm = TRUE
    ),
    participacao_maior_escola_carga_total_pct =
      participacao_maiores(
        indice_carga_potencial,
        1
      ),
    participacao_duas_maiores_escolas_carga_total_pct =
      participacao_maiores(
        indice_carga_potencial,
        2
      ),
    hhi_concentracao_carga_interna = calcular_hhi(
      indice_carga_potencial
    ),
    nome_escola_maior_indice = nome_canonico[
      which.max(
        replace_na(
          indice_carga_potencial,
          -Inf
        )
      )
    ],
    maior_indice_escola = maximo_seguro(
      indice_carga_potencial
    ),
    .groups = "drop"
  ) |>
  mutate(
    participacao_volume_contribuicoes_pct = if_else(
      carga_potencial_total > 0,
      100 * contribuicao_volume_total /
        carga_potencial_total,
      NA_real_
    ),
    participacao_estrutural_contribuicoes_pct = if_else(
      carga_potencial_total > 0,
      100 * contribuicao_estrutural_total /
        carga_potencial_total,
      NA_real_
    ),
    participacao_educacional_contribuicoes_pct = if_else(
      carga_potencial_total > 0,
      100 * contribuicao_educacional_total /
        carga_potencial_total,
      NA_real_
    ),
    participacao_administrativa_contribuicoes_pct = if_else(
      carga_potencial_total > 0,
      100 * contribuicao_administrativa_total /
        carga_potencial_total,
      NA_real_
    )
  )

# -------------------------------------------------------------------
# 11. Participações na rede e referências entre carteiras nominais
# -------------------------------------------------------------------

totais_rede <- tibble(
  total_escolas_rede = nrow(base_escola_carteira),
  total_matriculas_rede = soma_segura(
    base_escola_carteira$matriculas_anos_iniciais
  ),
  total_turmas_rede = soma_segura(
    base_escola_carteira$turmas_anos_iniciais_validas
  ),
  total_carga_potencial_rede = soma_segura(
    base_escola_carteira$indice_carga_potencial
  )
)

referencias_nominais <- analise_carteiras |>
  filter(carteira_nominal) |>
  summarise(
    numero_carteiras_nominais = n(),
    media_escolas_carteiras_nominais = mean(numero_escolas),
    media_matriculas_carteiras_nominais = mean(
      matriculas_anos_iniciais_total,
      na.rm = TRUE
    ),
    media_turmas_carteiras_nominais = mean(
      turmas_anos_iniciais_total,
      na.rm = TRUE
    ),
    media_carga_potencial_total_carteiras_nominais = mean(
      carga_potencial_total,
      na.rm = TRUE
    ),
    mediana_carga_potencial_total_carteiras_nominais = median(
      carga_potencial_total,
      na.rm = TRUE
    ),
    media_indice_medio_carteiras_nominais = mean(
      indice_carga_potencial_medio,
      na.rm = TRUE
    )
  )

analise_carteiras <- analise_carteiras |>
  mutate(
    participacao_escolas_rede_pct = 100 *
      numero_escolas /
      totais_rede$total_escolas_rede,
    participacao_matriculas_rede_pct = if_else(
      totais_rede$total_matriculas_rede > 0,
      100 * matriculas_anos_iniciais_total /
        totais_rede$total_matriculas_rede,
      NA_real_
    ),
    participacao_turmas_rede_pct = if_else(
      totais_rede$total_turmas_rede > 0,
      100 * turmas_anos_iniciais_total /
        totais_rede$total_turmas_rede,
      NA_real_
    ),
    participacao_carga_potencial_rede_pct = if_else(
      totais_rede$total_carga_potencial_rede > 0,
      100 * carga_potencial_total /
        totais_rede$total_carga_potencial_rede,
      NA_real_
    ),
    razao_participacao_carga_sobre_escolas = if_else(
      participacao_escolas_rede_pct > 0,
      participacao_carga_potencial_rede_pct /
        participacao_escolas_rede_pct,
      NA_real_
    ),
    diferenca_escolas_referencia_nominal = if_else(
      carteira_nominal,
      numero_escolas -
        referencias_nominais$media_escolas_carteiras_nominais,
      NA_real_
    ),
    razao_matriculas_referencia_nominal = if_else(
      carteira_nominal &
        referencias_nominais$media_matriculas_carteiras_nominais > 0,
      matriculas_anos_iniciais_total /
        referencias_nominais$media_matriculas_carteiras_nominais,
      NA_real_
    ),
    razao_turmas_referencia_nominal = if_else(
      carteira_nominal &
        referencias_nominais$media_turmas_carteiras_nominais > 0,
      turmas_anos_iniciais_total /
        referencias_nominais$media_turmas_carteiras_nominais,
      NA_real_
    ),
    razao_carga_potencial_referencia_nominal = if_else(
      carteira_nominal &
        referencias_nominais$media_carga_potencial_total_carteiras_nominais > 0,
      carga_potencial_total /
        referencias_nominais$media_carga_potencial_total_carteiras_nominais,
      NA_real_
    ),
    diferenca_carga_potencial_referencia_nominal = if_else(
      carteira_nominal,
      carga_potencial_total -
        referencias_nominais$media_carga_potencial_total_carteiras_nominais,
      NA_real_
    ),
    percentil_carga_total_entre_carteiras_nominais =
      percentil_condicional(
        carga_potencial_total,
        carteira_nominal
      ),
    faixa_carga_total_entre_carteiras_nominais =
      atribuir_faixa_carteira(
        percentil_carga_total_entre_carteiras_nominais
      ),
    percentil_indice_medio_entre_carteiras_nominais =
      percentil_condicional(
        indice_carga_potencial_medio,
        carteira_nominal
      ),
    faixa_indice_medio_entre_carteiras_nominais =
      atribuir_faixa_carteira(
        percentil_indice_medio_entre_carteiras_nominais
      )
  ) |>
  arrange(
    tipo_carteira,
    desc(carga_potencial_total),
    assessora_gerencial
  )

# -------------------------------------------------------------------
# 12. Cenários de pesos agregados por carteira
# -------------------------------------------------------------------

mapa_cenarios <- tribble(
  ~cenario, ~descricao_cenario, ~variavel_indice,
  "equilibrado", "Pesos iguais entre as quatro dimensões; cenário principal provisório.", "indice_carga_potencial_equilibrado",
  "operacional", "Maior ênfase no volume e na estrutura operacional.", "indice_carga_potencial_operacional",
  "desafio_educacional", "Maior ênfase no desafio educacional observado.", "indice_carga_potencial_desafio_educacional",
  "transicao_administrativa", "Maior ênfase nos arranjos e transições administrativas.", "indice_carga_potencial_transicao_administrativa"
)

analise_carteiras_cenarios <- base_escola_carteira |>
  select(
    assessora_gerencial,
    tipo_carteira,
    carteira_nominal,
    all_of(mapa_cenarios$variavel_indice)
  ) |>
  pivot_longer(
    cols = all_of(mapa_cenarios$variavel_indice),
    names_to = "variavel_indice",
    values_to = "indice_escola_cenario"
  ) |>
  left_join(
    mapa_cenarios,
    by = "variavel_indice"
  ) |>
  group_by(
    cenario,
    descricao_cenario,
    assessora_gerencial,
    tipo_carteira,
    carteira_nominal
  ) |>
  summarise(
    numero_escolas = n(),
    carga_potencial_total_cenario = soma_segura(
      indice_escola_cenario
    ),
    indice_medio_cenario = media_segura(
      indice_escola_cenario
    ),
    indice_mediano_cenario = mediana_segura(
      indice_escola_cenario
    ),
    .groups = "drop"
  ) |>
  group_by(
    cenario
  ) |>
  mutate(
    carga_total_rede_cenario = sum(
      carga_potencial_total_cenario,
      na.rm = TRUE
    ),
    participacao_carga_rede_cenario_pct = if_else(
      carga_total_rede_cenario > 0,
      100 * carga_potencial_total_cenario /
        carga_total_rede_cenario,
      NA_real_
    ),
    ordem_carga_total_todas_categorias = min_rank(
      desc(carga_potencial_total_cenario)
    )
  ) |>
  ungroup() |>
  group_by(
    cenario
  ) |>
  mutate(
    ordem_carga_total_carteiras_nominais = if_else(
      carteira_nominal,
      min_rank(
        if_else(
          carteira_nominal,
          desc(carga_potencial_total_cenario),
          NA_real_
        )
      ),
      NA_integer_
    ),
    percentil_carga_total_cenario_carteiras_nominais =
      percentil_condicional(
        carga_potencial_total_cenario,
        carteira_nominal
      ),
    faixa_carga_total_cenario_carteiras_nominais =
      atribuir_faixa_carteira(
        percentil_carga_total_cenario_carteiras_nominais
      )
  ) |>
  ungroup()

referencia_cenario_principal <- analise_carteiras_cenarios |>
  filter(cenario == "equilibrado") |>
  select(
    assessora_gerencial,
    carga_total_equilibrado = carga_potencial_total_cenario,
    ordem_equilibrado_nominais =
      ordem_carga_total_carteiras_nominais,
    faixa_equilibrado_nominais =
      faixa_carga_total_cenario_carteiras_nominais
  )

analise_carteiras_cenarios <- analise_carteiras_cenarios |>
  left_join(
    referencia_cenario_principal,
    by = "assessora_gerencial"
  ) |>
  mutate(
    diferenca_carga_total_para_equilibrado =
      carga_potencial_total_cenario - carga_total_equilibrado,
    mudanca_ordem_nominais_para_equilibrado = if_else(
      carteira_nominal,
      ordem_carga_total_carteiras_nominais -
        ordem_equilibrado_nominais,
      NA_integer_
    ),
    mudou_faixa_carga_total_carteira = if_else(
      carteira_nominal,
      faixa_carga_total_cenario_carteiras_nominais !=
        faixa_equilibrado_nominais,
      NA
    )
  ) |>
  arrange(
    factor(
      cenario,
      levels = mapa_cenarios$cenario
    ),
    tipo_carteira,
    ordem_carga_total_todas_categorias,
    assessora_gerencial
  )

# -------------------------------------------------------------------
# 13. Diagnósticos analíticos
# -------------------------------------------------------------------

contagem_escolas_por_carteira <- analise_carteiras |>
  select(
    assessora_gerencial,
    tipo_carteira,
    carteira_nominal,
    numero_escolas,
    participacao_escolas_rede_pct,
    matriculas_anos_iniciais_total,
    turmas_anos_iniciais_total
  ) |>
  arrange(
    tipo_carteira,
    desc(numero_escolas),
    assessora_gerencial
  )

resumo_carga_extensiva <- analise_carteiras |>
  select(
    assessora_gerencial,
    tipo_carteira,
    numero_escolas,
    matriculas_anos_iniciais_total,
    turmas_anos_iniciais_total,
    etapas_amplas_total,
    series_com_resultado_2026_total,
    series_comparaveis_total,
    previstos_total_2026_carteira,
    avaliados_total_2026_carteira,
    taxa_participacao_agregada_2026,
    participacao_escolas_rede_pct,
    participacao_matriculas_rede_pct,
    participacao_turmas_rede_pct,
    diferenca_escolas_referencia_nominal,
    razao_matriculas_referencia_nominal,
    razao_turmas_referencia_nominal
  )

resumo_carga_potencial <- analise_carteiras |>
  select(
    assessora_gerencial,
    tipo_carteira,
    numero_escolas,
    carga_potencial_total,
    indice_carga_potencial_medio,
    indice_carga_potencial_mediano,
    indice_carga_potencial_desvio,
    indice_carga_potencial_iqr,
    indice_carga_potencial_minimo,
    indice_carga_potencial_maximo,
    indice_carga_potencial_amplitude,
    coeficiente_variacao_indice,
    participacao_carga_potencial_rede_pct,
    razao_participacao_carga_sobre_escolas,
    razao_carga_potencial_referencia_nominal,
    diferenca_carga_potencial_referencia_nominal,
    percentil_carga_total_entre_carteiras_nominais,
    faixa_carga_total_entre_carteiras_nominais,
    percentil_indice_medio_entre_carteiras_nominais,
    faixa_indice_medio_entre_carteiras_nominais
  )

concentracao_casos_desafiadores <- analise_carteiras |>
  select(
    assessora_gerencial,
    tipo_carteira,
    numero_escolas,
    numero_escolas_faixa_4,
    percentual_escolas_faixa_4,
    numero_escolas_faixas_3_4,
    percentual_escolas_faixas_3_4,
    numero_escolas_complexidade_administrativa,
    numero_escolas_interpretacao_cautelosa,
    numero_escolas_sensibilidade_elevada,
    numero_escolas_alerta_composicao,
    numero_escolas_participacao_2026_abaixo_80,
    numero_escolas_leitura_detalhada,
    nome_escola_maior_indice,
    maior_indice_escola,
    participacao_maior_escola_carga_total_pct,
    participacao_duas_maiores_escolas_carga_total_pct,
    hhi_concentracao_carga_interna
  )

dimensoes_por_carteira <- analise_carteiras |>
  select(
    assessora_gerencial,
    tipo_carteira,
    numero_escolas,
    dimensao_volume_media,
    dimensao_estrutural_media,
    dimensao_educacional_media,
    dimensao_administrativa_media,
    contribuicao_volume_total,
    contribuicao_estrutural_total,
    contribuicao_educacional_total,
    contribuicao_administrativa_total,
    participacao_volume_contribuicoes_pct,
    participacao_estrutural_contribuicoes_pct,
    participacao_educacional_contribuicoes_pct,
    participacao_administrativa_contribuicoes_pct
  )

sensibilidade_posicao_carteiras <- analise_carteiras_cenarios |>
  filter(cenario != "equilibrado") |>
  select(
    assessora_gerencial,
    tipo_carteira,
    carteira_nominal,
    cenario,
    carga_potencial_total_cenario,
    diferenca_carga_total_para_equilibrado,
    ordem_equilibrado_nominais,
    ordem_carga_total_carteiras_nominais,
    mudanca_ordem_nominais_para_equilibrado,
    faixa_equilibrado_nominais,
    faixa_carga_total_cenario_carteiras_nominais,
    mudou_faixa_carga_total_carteira
  )

escolas_maior_carga_por_carteira <- carteira_escola_detalhe |>
  filter(
    ordem_interna_carga_potencial <= 2 |
      caso_para_leitura_detalhada
  ) |>
  select(
    assessora_gerencial,
    tipo_carteira,
    ordem_interna_carga_potencial,
    id_escola,
    codigo_inep,
    nome_canonico,
    indice_carga_potencial,
    faixa_indice_carga_potencial,
    score_dimensao_volume,
    score_dimensao_estrutural,
    score_dimensao_educacional,
    score_dimensao_administrativa,
    sensibilidade_faixa,
    interpretacao_indice_cautelosa,
    caso_para_leitura_detalhada,
    participacao_indice_na_carteira_pct,
    participacao_acumulada_indice_carteira_pct
  )

cobertura_e_cautelas <- analise_carteiras |>
  select(
    assessora_gerencial,
    tipo_carteira,
    numero_escolas,
    escolas_com_matriculas_disponiveis,
    escolas_com_turmas_disponiveis,
    escolas_painel_completo,
    escolas_painel_incompleto,
    numero_escolas_interpretacao_cautelosa,
    numero_escolas_sensibilidade_elevada,
    numero_escolas_complexidade_administrativa,
    numero_escolas_alerta_composicao
  )

# -------------------------------------------------------------------
# 14. Validações finais
# -------------------------------------------------------------------

soma_contribuicoes_carteiras <- analise_carteiras |>
  transmute(
    assessora_gerencial,
    carga_potencial_total,
    soma_contribuicoes =
      contribuicao_volume_total +
      contribuicao_estrutural_total +
      contribuicao_educacional_total +
      contribuicao_administrativa_total,
    diferenca = abs(
      carga_potencial_total - soma_contribuicoes
    )
  )

soma_faixas_carteiras <- analise_carteiras |>
  transmute(
    assessora_gerencial,
    numero_escolas,
    soma_faixas =
      numero_escolas_faixa_1 +
      numero_escolas_faixa_2 +
      numero_escolas_faixa_3 +
      numero_escolas_faixa_4,
    diferenca = numero_escolas - soma_faixas
  )

validacoes_finais <- tribble(
  ~teste, ~resultado, ~valor_observado, ~criterio,
  "Uma linha por escola no índice", anyDuplicated(indice_escola$id_escola) == 0, anyDuplicated(indice_escola$id_escola), "Nenhuma duplicidade",
  "Uma linha por escola no perfil", anyDuplicated(perfil_escola$id_escola) == 0, anyDuplicated(perfil_escola$id_escola), "Nenhuma duplicidade",
  "Conjunto de escolas idêntico entre entradas", setequal(indice_escola$id_escola, perfil_escola$id_escola), length(setdiff(indice_escola$id_escola, perfil_escola$id_escola)) + length(setdiff(perfil_escola$id_escola, indice_escola$id_escola)), "Zero diferenças",
  "Vinculação administrativa consistente", !any(comparacao_atributos_chave$conflito_assessora, na.rm = TRUE), sum(comparacao_atributos_chave$conflito_assessora, na.rm = TRUE), "Zero conflitos",
  "Todas as escolas aparecem no detalhe", nrow(carteira_escola_detalhe) == nrow(indice_escola), nrow(carteira_escola_detalhe), paste0("Esperado: ", nrow(indice_escola)),
  "Soma das escolas das carteiras", sum(analise_carteiras$numero_escolas) == nrow(indice_escola), sum(analise_carteiras$numero_escolas), paste0("Esperado: ", nrow(indice_escola)),
  "Soma das matrículas preservada", isTRUE(all.equal(sum(analise_carteiras$matriculas_anos_iniciais_total, na.rm = TRUE), sum(base_escola_carteira$matriculas_anos_iniciais, na.rm = TRUE), tolerance = 1e-8)), sum(analise_carteiras$matriculas_anos_iniciais_total, na.rm = TRUE), paste0("Esperado: ", sum(base_escola_carteira$matriculas_anos_iniciais, na.rm = TRUE)),
  "Carga potencial acumulada preservada", isTRUE(all.equal(sum(analise_carteiras$carga_potencial_total, na.rm = TRUE), sum(base_escola_carteira$indice_carga_potencial, na.rm = TRUE), tolerance = 1e-8)), sum(analise_carteiras$carga_potencial_total, na.rm = TRUE), paste0("Esperado: ", sum(base_escola_carteira$indice_carga_potencial, na.rm = TRUE)),
  "Contribuições recompõem a carga total", all(soma_contribuicoes_carteiras$diferenca < 1e-8, na.rm = TRUE), max(soma_contribuicoes_carteiras$diferenca, na.rm = TRUE), "Diferença máxima inferior a 1e-8",
  "Faixas escolares recompõem o total das carteiras", all(soma_faixas_carteiras$diferenca == 0), max(abs(soma_faixas_carteiras$diferenca)), "Zero diferença",
  "Participações da carga da rede somam 100", abs(sum(analise_carteiras$participacao_carga_potencial_rede_pct, na.rm = TRUE) - 100) < 1e-8, sum(analise_carteiras$participacao_carga_potencial_rede_pct, na.rm = TRUE), "100",
  "Quatro cenários por carteira", all(count(analise_carteiras_cenarios, assessora_gerencial)$n == 4), min(count(analise_carteiras_cenarios, assessora_gerencial)$n), "Quatro",
  "Totais dos cenários preservados", all(analise_carteiras_cenarios |> group_by(cenario) |> summarise(total = sum(carga_potencial_total_cenario), .groups = "drop") |> left_join(mapa_cenarios, by = "cenario") |> mutate(total_entrada = map_dbl(variavel_indice, ~ sum(base_escola_carteira[[.x]], na.rm = TRUE)), diferenca = abs(total - total_entrada)) |> pull(diferenca) < 1e-8), max(analise_carteiras_cenarios |> group_by(cenario) |> summarise(total = sum(carga_potencial_total_cenario), .groups = "drop") |> left_join(mapa_cenarios, by = "cenario") |> mutate(total_entrada = map_dbl(variavel_indice, ~ sum(base_escola_carteira[[.x]], na.rm = TRUE)), diferenca = abs(total - total_entrada)) |> pull(diferenca)), "Diferença máxima inferior a 1e-8",
  "Há carteiras nominais para comparação", sum(analise_carteiras$carteira_nominal) > 0, sum(analise_carteiras$carteira_nominal), "Maior que zero",
  "Índices médios dentro de 0 a 100", all(analise_carteiras$indice_carga_potencial_medio >= 0 & analise_carteiras$indice_carga_potencial_medio <= 100, na.rm = TRUE), sum(analise_carteiras$indice_carga_potencial_medio < 0 | analise_carteiras$indice_carga_potencial_medio > 100, na.rm = TRUE), "Zero valores fora do intervalo"
) |>
  mutate(
    nivel = if_else(
      resultado,
      "OK",
      "ERRO"
    )
  )

erros_criticos <- validacoes_finais |>
  filter(!resultado)

# -------------------------------------------------------------------
# 15. Dicionário da análise principal
# -------------------------------------------------------------------

descricoes_dicionario <- c(
  assessora_gerencial = "Categoria administrativa responsável pela carteira.",
  tipo_carteira = "Classificação da categoria como carteira nominal, agrupamento residual ou sem vinculação informada.",
  carteira_nominal = "Indica se a categoria entra nas referências comparativas entre carteiras nominais.",
  numero_escolas = "Quantidade de escolas vinculadas à categoria administrativa.",
  matriculas_anos_iniciais_total = "Soma das matrículas dos anos iniciais nas escolas da carteira.",
  turmas_anos_iniciais_total = "Soma das turmas válidas dos anos iniciais nas escolas da carteira.",
  carga_potencial_total = "Soma do índice de carga potencial das escolas; representa carga acumulada relativa.",
  indice_carga_potencial_medio = "Média simples do índice escolar na carteira.",
  indice_carga_potencial_mediano = "Mediana do índice escolar na carteira.",
  indice_carga_potencial_desvio = "Desvio-padrão do índice escolar dentro da carteira.",
  dimensao_volume_media = "Média do escore de volume das escolas da carteira.",
  dimensao_estrutural_media = "Média do escore de complexidade estrutural das escolas da carteira.",
  dimensao_educacional_media = "Média do escore de desafio educacional observado das escolas da carteira.",
  dimensao_administrativa_media = "Média do escore de complexidade administrativa das escolas da carteira.",
  numero_escolas_faixa_4 = "Quantidade de escolas no quartil superior do índice relativo da rede.",
  percentual_escolas_faixa_4 = "Percentual da carteira composto por escolas no quartil superior do índice.",
  numero_escolas_interpretacao_cautelosa = "Quantidade de escolas com evidência que exige interpretação cautelosa.",
  numero_escolas_sensibilidade_elevada = "Quantidade de escolas que mudam de faixa em dois ou três cenários.",
  participacao_maior_escola_carga_total_pct = "Parcela da carga acumulada da carteira concentrada na escola com maior índice.",
  participacao_duas_maiores_escolas_carga_total_pct = "Parcela da carga acumulada concentrada nas duas escolas com maiores índices.",
  hhi_concentracao_carga_interna = "Índice Herfindahl-Hirschman das parcelas do índice escolar dentro da carteira; maior valor indica maior concentração.",
  participacao_carga_potencial_rede_pct = "Parcela da carga potencial acumulada da rede atribuída à carteira.",
  razao_participacao_carga_sobre_escolas = "Razão entre a participação da carteira na carga potencial da rede e sua participação no número de escolas.",
  razao_carga_potencial_referencia_nominal = "Carga total da carteira dividida pela média das carteiras nominais.",
  faixa_carga_total_entre_carteiras_nominais = "Faixa quartílica relativa da carga total entre carteiras nominais; não representa desempenho.",
  faixa_indice_medio_entre_carteiras_nominais = "Faixa quartílica relativa do índice médio entre carteiras nominais; não representa desempenho."
)

dicionario_analise <- tibble(
  ordem_coluna = seq_along(analise_carteiras),
  variavel = names(analise_carteiras),
  classe_r = map_chr(
    analise_carteiras,
    ~ paste(
      class(.x),
      collapse = " | "
    )
  ),
  descricao = map_chr(
    names(analise_carteiras),
    function(variavel) {
      descricao <- unname(
        descricoes_dicionario[variavel]
      )

      if (
        length(descricao) == 0 ||
          is.na(descricao[[1]]) ||
          !nzchar(descricao[[1]])
      ) {
        paste0(
          "Variável ",
          str_replace_all(
            variavel,
            "_",
            " "
          ),
          ". Consulte o script do módulo 18 para definição operacional."
        )
      } else {
        descricao[[1]]
      }
    }
  ),
  unidade_analise = "carteira administrativa",
  observacao_metodologica = case_when(
    str_detect(variavel, "proficiencia|defasagem|adequado|intermediario") ~
      "Resultado descritivo; não representa efeito ou qualidade da assessora.",
    str_detect(variavel, "faixa|percentil|ordem") ~
      "Medida relativa usada para diagnóstico de carga, não para avaliação de desempenho.",
    str_detect(variavel, "carga_potencial|indice") ~
      "Escore relativo e descritivo; não é percentual de carga absoluta.",
    TRUE ~
      "Usar em conjunto com as demais dimensões e com validação qualitativa."
  )
)

# -------------------------------------------------------------------
# 16. Estruturas, manifestos e resumo
# -------------------------------------------------------------------

estrutura_saidas <- bind_rows(
  estrutura_base(
    analise_carteiras,
    "analise_carteiras_assessoras"
  ),
  estrutura_base(
    carteira_escola_detalhe,
    "carteira_escola_detalhe"
  ),
  estrutura_base(
    analise_carteiras_cenarios,
    "analise_carteiras_cenarios"
  ),
  estrutura_base(
    composicao_faixas_carteiras,
    "composicao_faixas_carteiras"
  )
)

manifesto_entradas <- tibble(
  fonte = names(caminhos_entrada_usados),
  caminho = unname(caminhos_entrada_usados),
  existe = file.exists(caminhos_entrada_usados),
  tamanho_bytes = map_dbl(
    caminhos_entrada_usados,
    ~ if (file.exists(.x)) file.info(.x)$size else NA_real_
  ),
  md5 = map_chr(
    caminhos_entrada_usados,
    ~ if (file.exists(.x)) unname(tools::md5sum(.x)) else NA_character_
  )
)

arquivo_resumo <- file.path(
  pasta_execucao,
  "16_resumo_execucao.txt"
)

linhas_resumo <- c(
  paste0("Execução: ", id_execucao),
  paste0("Escolas analisadas: ", nrow(base_escola_carteira)),
  paste0("Categorias administrativas: ", nrow(analise_carteiras)),
  paste0("Carteiras nominais: ", sum(analise_carteiras$carteira_nominal)),
  paste0("Agrupamentos residuais: ", sum(analise_carteiras$tipo_carteira == "Agrupamento residual")),
  paste0("Categorias sem vinculação informada: ", sum(analise_carteiras$tipo_carteira == "Sem vinculação informada")),
  paste0("Menor número de escolas em carteira nominal: ", min(analise_carteiras$numero_escolas[analise_carteiras$carteira_nominal])),
  paste0("Maior número de escolas em carteira nominal: ", max(analise_carteiras$numero_escolas[analise_carteiras$carteira_nominal])),
  paste0("Carga potencial total da rede: ", round(totais_rede$total_carga_potencial_rede, 2)),
  paste0("Erros críticos: ", nrow(erros_criticos)),
  "",
  "Observações metodológicas:",
  "- A análise descreve carteiras administrativas, não desempenho das assessoras.",
  "- A carga potencial acumulada combina quantidade e composição das escolas.",
  "- Médias e medianas devem ser lidas em conjunto com a carga acumulada.",
  "- Faixas e posições são relativas e não constituem ranking de qualidade.",
  "- Resultados educacionais não devem ser atribuídos às assessoras.",
  "- Redistribuições exigem validação qualitativa, territorial e operacional."
)

write_lines(
  linhas_resumo,
  arquivo_resumo
)

write_lines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    pasta_execucao,
    "15_session_info.txt"
  )
)

# -------------------------------------------------------------------
# 17. Exportação dos diagnósticos antes dos produtos canônicos
# -------------------------------------------------------------------

write_csv(
  parametros_analise,
  file.path(
    pasta_execucao,
    "01_parametros_analise_carteiras.csv"
  ),
  na = ""
)

write_csv(
  manifesto_entradas,
  file.path(
    pasta_execucao,
    "02_manifesto_arquivos_entrada.csv"
  ),
  na = ""
)

write_csv(
  contagem_escolas_por_carteira,
  file.path(
    pasta_execucao,
    "03_contagem_escolas_por_carteira.csv"
  ),
  na = ""
)

write_csv(
  resumo_carga_extensiva,
  file.path(
    pasta_execucao,
    "04_resumo_carga_extensiva.csv"
  ),
  na = ""
)

write_csv(
  resumo_carga_potencial,
  file.path(
    pasta_execucao,
    "05_resumo_carga_potencial.csv"
  ),
  na = ""
)

write_csv(
  composicao_faixas_carteiras,
  file.path(
    pasta_execucao,
    "06_composicao_faixas_por_carteira.csv"
  ),
  na = ""
)

write_csv(
  concentracao_casos_desafiadores,
  file.path(
    pasta_execucao,
    "07_concentracao_casos_desafiadores.csv"
  ),
  na = ""
)

write_csv(
  dimensoes_por_carteira,
  file.path(
    pasta_execucao,
    "08_dimensoes_por_carteira.csv"
  ),
  na = ""
)

write_csv(
  analise_carteiras_cenarios,
  file.path(
    pasta_execucao,
    "09_cenarios_por_carteira.csv"
  ),
  na = ""
)

write_csv(
  sensibilidade_posicao_carteiras,
  file.path(
    pasta_execucao,
    "10_sensibilidade_posicao_carteiras.csv"
  ),
  na = ""
)

write_csv(
  escolas_maior_carga_por_carteira,
  file.path(
    pasta_execucao,
    "11_escolas_maior_carga_por_carteira.csv"
  ),
  na = ""
)

write_csv(
  cobertura_e_cautelas,
  file.path(
    pasta_execucao,
    "12_cobertura_e_cautelas.csv"
  ),
  na = ""
)

write_csv(
  validacoes_finais,
  file.path(
    pasta_execucao,
    "13_validacao_final.csv"
  ),
  na = ""
)

write_csv(
  estrutura_saidas,
  file.path(
    pasta_execucao,
    "14_estrutura_bases_saida.csv"
  ),
  na = ""
)

# Interrompe antes de atualizar os produtos canônicos quando houver erro.
if (nrow(erros_criticos) > 0) {
  stop(
    "O módulo 18 encontrou ",
    nrow(erros_criticos),
    " erro(s) crítico(s). Consulte `13_validacao_final.csv` em: ",
    pasta_execucao
  )
}

# -------------------------------------------------------------------
# 18. Arquivamento e exportação dos produtos finais
# -------------------------------------------------------------------

walk(
  arquivos_saida,
  arquivar_se_existir
)

write_csv(
  analise_carteiras,
  arquivos_saida[["analise_carteiras_csv"]],
  na = ""
)

saveRDS(
  analise_carteiras,
  arquivos_saida[["analise_carteiras_rds"]]
)

write_csv(
  carteira_escola_detalhe,
  arquivos_saida[["detalhe_escolas_csv"]],
  na = ""
)

saveRDS(
  carteira_escola_detalhe,
  arquivos_saida[["detalhe_escolas_rds"]]
)

write_csv(
  analise_carteiras_cenarios,
  arquivos_saida[["cenarios_csv"]],
  na = ""
)

saveRDS(
  analise_carteiras_cenarios,
  arquivos_saida[["cenarios_rds"]]
)

write_csv(
  composicao_faixas_carteiras,
  arquivos_saida[["composicao_faixas_csv"]],
  na = ""
)

saveRDS(
  composicao_faixas_carteiras,
  arquivos_saida[["composicao_faixas_rds"]]
)

write_csv(
  dicionario_analise,
  arquivos_saida[["dicionario_csv"]],
  na = ""
)

manifesto_produtos <- tibble(
  produto = names(arquivos_saida),
  caminho = unname(arquivos_saida),
  existe = file.exists(arquivos_saida),
  tamanho_bytes = map_dbl(
    arquivos_saida,
    ~ if (file.exists(.x)) file.info(.x)$size else NA_real_
  ),
  md5 = map_chr(
    arquivos_saida,
    ~ if (file.exists(.x)) unname(tools::md5sum(.x)) else NA_character_
  )
)

write_csv(
  manifesto_produtos,
  file.path(
    pasta_execucao,
    "15_manifesto_produtos_modulo_18.csv"
  ),
  na = ""
)

message(
  "Módulo 18 concluído com sucesso.\n",
  "Execução: ", id_execucao, "\n",
  "Escolas: ", nrow(base_escola_carteira), "\n",
  "Categorias administrativas: ", nrow(analise_carteiras), "\n",
  "Carteiras nominais: ", sum(analise_carteiras$carteira_nominal), "\n",
  "Diagnósticos: ", pasta_execucao
)
