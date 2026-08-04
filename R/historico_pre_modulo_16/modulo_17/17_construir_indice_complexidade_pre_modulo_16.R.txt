# ===================================================================
# 17_construir_indice_complexidade.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# ===================================================================
#
# OBJETIVO ANALÍTICO
#
# Construir um índice descritivo de carga potencial de assessoramento,
# mantendo quatro dimensões separadas e plenamente auditáveis:
#
#   1. volume de atendimento;
#   2. complexidade estrutural;
#   3. desafio educacional observado;
#   4. complexidade administrativa.
#
# O índice é uma ferramenta gerencial de diagnóstico. Não constitui:
#
# - ranking de qualidade das escolas;
# - ranking de desempenho das assessoras;
# - estimativa de efeito causal do programa;
# - regra automática de redistribuição das carteiras.
#
# PRINCÍPIOS METODOLÓGICOS
#
# - As dimensões permanecem disponíveis separadamente.
# - O índice principal usa pesos iguais entre dimensões como ponto de
#   partida provisório e transparente.
# - Três cenários alternativos de pesos são calculados para análise de
#   sensibilidade.
# - Variáveis contínuas são convertidas em posições percentílicas
#   relativas à rede analisada, com empates tratados pela posição média.
# - O desafio educacional é comparado dentro de cada ano escolar, para
#   evitar que diferenças próprias entre 1º e 5º ano contaminem o escore.
# - Resultados de 2026 são utilizados como retrato educacional mais atual.
# - Variações de proficiência 2025–2026 NÃO entram no índice.
# - Mudanças de composição entram com peso reduzido e somente quando a
#   escola-série possui observações comparáveis nos dois anos.
# - Características raciais ou étnicas não entram no índice.
# - A identidade da assessora não entra no cálculo. Apenas a ausência de
#   vinculação administrativa informada pode compor a dimensão administrativa.
# - Escores relativos dependem do universo analisado e não devem ser
#   comparados diretamente com outras redes ou períodos sem recalibração.
#
# ENTRADAS
#
# dados_finais/perfil_escola_gerencial.rds ou .csv
# dados_finais/perfil_escola_serie_compacto.rds ou .csv
#
# PRODUTOS PRINCIPAIS
#
# dados_finais/indice_carga_potencial_escola.csv e .rds
# dados_finais/componentes_indice_carga_potencial.csv e .rds
# dados_finais/componentes_educacionais_escola_serie.csv e .rds
# documentacao/indice_complexidade/dicionario_indice_carga_potencial.csv
# documentacao/indice_complexidade/execucao_<data_hora>/...
# dados_finais/historico/indice_complexidade/execucao_<data_hora>/...
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
  "indice_complexidade",
  paste0("execucao_", id_execucao)
)

pasta_documentacao <- here(
  "documentacao",
  "indice_complexidade"
)

pasta_execucao <- here(
  "documentacao",
  "indice_complexidade",
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
  perfil_escola = here(
    "dados_finais",
    "perfil_escola_gerencial.rds"
  ),
  perfil_escola_serie = here(
    "dados_finais",
    "perfil_escola_serie_compacto.rds"
  )
)

arquivos_entrada_csv <- c(
  perfil_escola = here(
    "dados_finais",
    "perfil_escola_gerencial.csv"
  ),
  perfil_escola_serie = here(
    "dados_finais",
    "perfil_escola_serie_compacto.csv"
  )
)

arquivos_saida <- c(
  indice_csv = here(
    "dados_finais",
    "indice_carga_potencial_escola.csv"
  ),
  indice_rds = here(
    "dados_finais",
    "indice_carga_potencial_escola.rds"
  ),
  componentes_csv = here(
    "dados_finais",
    "componentes_indice_carga_potencial.csv"
  ),
  componentes_rds = here(
    "dados_finais",
    "componentes_indice_carga_potencial.rds"
  ),
  componentes_educacionais_csv = here(
    "dados_finais",
    "componentes_educacionais_escola_serie.csv"
  ),
  componentes_educacionais_rds = here(
    "dados_finais",
    "componentes_educacionais_escola_serie.rds"
  ),
  dicionario_csv = here(
    "documentacao",
    "indice_complexidade",
    "dicionario_indice_carga_potencial.csv"
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
      "\nExecute primeiro o módulo 16."
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

media_segura <- function(x) {
  x <- x[
    !is.na(x)
  ]

  if (length(x) == 0) {
    return(NA_real_)
  }

  mean(x)
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

# Percentil por posição média. O menor valor recebe 0 e o maior 100.
# Variáveis constantes recebem 50 para todas as observações válidas.
percentil_relativo <- function(x, inverso = FALSE) {
  x <- suppressWarnings(
    as.numeric(x)
  )

  valido <- !is.na(x)
  n_valido <- sum(valido)
  resultado <- rep(
    NA_real_,
    length(x)
  )

  if (n_valido == 0) {
    return(resultado)
  }

  if (n_valido == 1) {
    resultado[valido] <- 50
  } else {
    posicao <- rank(
      x[valido],
      ties.method = "average"
    )

    resultado[valido] <- 100 * (
      posicao - 1
    ) / (
      n_valido - 1
    )
  }

  if (isTRUE(inverso)) {
    resultado[valido] <- 100 - resultado[valido]
  }

  resultado
}

# Calcula média ponderada apenas sobre componentes disponíveis.
# A cobertura devolvida corresponde à soma dos pesos disponíveis.
calcular_score_ponderado <- function(
    dados,
    componentes,
    pesos,
    cobertura_minima = 0) {

  if (!all(componentes %in% names(dados))) {
    stop(
      "Componentes ausentes em `calcular_score_ponderado`: ",
      paste(
        setdiff(componentes, names(dados)),
        collapse = ", "
      )
    )
  }

  if (!all(componentes %in% names(pesos))) {
    stop(
      "Pesos não definidos para todos os componentes."
    )
  }

  pesos_usados <- pesos[componentes]

  matriz <- dados |>
    select(
      all_of(componentes)
    ) |>
    as.matrix()

  armazenamento <- suppressWarnings(
    matrix(
      as.numeric(matriz),
      nrow = nrow(matriz),
      ncol = ncol(matriz),
      dimnames = dimnames(matriz)
    )
  )

  disponivel <- !is.na(armazenamento)
  armazenamento_zero <- armazenamento
  armazenamento_zero[!disponivel] <- 0

  numerador <- rowSums(
    sweep(
      armazenamento_zero,
      2,
      pesos_usados,
      `*`
    )
  )

  cobertura <- rowSums(
    sweep(
      disponivel * 1,
      2,
      pesos_usados,
      `*`
    )
  )

  score <- if_else(
    cobertura > 0 & cobertura >= cobertura_minima,
    numerador / cobertura,
    NA_real_
  )

  tibble(
    score = score,
    cobertura = cobertura
  )
}

atribuir_faixa_relativa <- function(score) {
  posicao <- percentil_relativo(score)

  case_when(
    is.na(posicao) ~ NA_character_,
    posicao <= 25 ~ "Faixa 1 — menor carga potencial relativa",
    posicao <= 50 ~ "Faixa 2 — intermediária inferior",
    posicao <= 75 ~ "Faixa 3 — intermediária superior",
    TRUE ~ "Faixa 4 — maior carga potencial relativa"
  )
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

resumo_numerico <- function(dados, variaveis, grupo = NA_character_) {
  map_dfr(
    variaveis,
    function(variavel) {
      x <- suppressWarnings(
        as.numeric(
          dados[[variavel]]
        )
      )

      tibble(
        grupo = grupo,
        variavel = variavel,
        n_valido = sum(!is.na(x)),
        media = if_else(
          any(!is.na(x)),
          mean(x, na.rm = TRUE),
          NA_real_
        ),
        desvio_padrao = if_else(
          sum(!is.na(x)) >= 2,
          sd(x, na.rm = TRUE),
          NA_real_
        ),
        minimo = if_else(
          any(!is.na(x)),
          min(x, na.rm = TRUE),
          NA_real_
        ),
        p25 = if_else(
          any(!is.na(x)),
          as.numeric(
            quantile(
              x,
              0.25,
              na.rm = TRUE,
              names = FALSE
            )
          ),
          NA_real_
        ),
        mediana = if_else(
          any(!is.na(x)),
          median(x, na.rm = TRUE),
          NA_real_
        ),
        p75 = if_else(
          any(!is.na(x)),
          as.numeric(
            quantile(
              x,
              0.75,
              na.rm = TRUE,
              names = FALSE
            )
          ),
          NA_real_
        ),
        maximo = if_else(
          any(!is.na(x)),
          max(x, na.rm = TRUE),
          NA_real_
        )
      )
    }
  )
}

# -------------------------------------------------------------------
# 4. Leitura das bases
# -------------------------------------------------------------------

perfil_escola <- ler_base_preferindo_rds(
  "perfil_escola"
)

perfil_escola_serie <- ler_base_preferindo_rds(
  "perfil_escola_serie"
)

caminhos_entrada_usados <- c(
  perfil_escola = attr(
    perfil_escola,
    "caminho_origem"
  ),
  perfil_escola_serie = attr(
    perfil_escola_serie,
    "caminho_origem"
  )
)

# -------------------------------------------------------------------
# 5. Validação de colunas e tipos essenciais
# -------------------------------------------------------------------

colunas_obrigatorias_perfil <- c(
  "id_escola",
  "codigo_inep",
  "nome_canonico",
  "assessora_gerencial",
  "grupo_administrativo_2024_final",
  "tipo_vinculo_rede_final",
  "status_rede_2025_final",
  "municipalizada_apos_2024",
  "possivel_municipalizacao_recente",
  "escola_nova_recente",
  "privada_vinculada_final",
  "requer_revisao_tecnica",
  "divergencia_censo_cadastro",
  "matriculas_anos_iniciais",
  "turmas_anos_iniciais",
  "numero_etapas_amplas_ofertadas",
  "indice_infraestrutura_basica",
  "alunos_por_turma_anos_iniciais",
  "alunos_por_docente_anos_iniciais",
  "pct_matriculas_educacao_especial",
  "pct_matriculas_anos_iniciais_integral",
  "pct_matriculas_transporte_publico",
  "numero_series_resultado_2026",
  "numero_series_comparaveis",
  "painel_completo_cinco_series"
)

colunas_obrigatorias_serie <- c(
  "id_escola",
  "codigo_inep",
  "nome_canonico",
  "assessora_gerencial",
  "ano_escolar",
  "componente",
  "painel_resultado_balanceado",
  "proficiencia_media_2026",
  "pct_defasagem_2026",
  "taxa_participacao_2026",
  "aumento_participacao_10pp",
  "queda_participacao_10pp",
  "mudanca_previstos_20pct"
)

colunas_ausentes_perfil <- setdiff(
  colunas_obrigatorias_perfil,
  names(perfil_escola)
)

colunas_ausentes_serie <- setdiff(
  colunas_obrigatorias_serie,
  names(perfil_escola_serie)
)

if (length(colunas_ausentes_perfil) > 0) {
  stop(
    "Colunas obrigatórias ausentes no perfil escolar: ",
    paste(
      colunas_ausentes_perfil,
      collapse = ", "
    )
  )
}

if (length(colunas_ausentes_serie) > 0) {
  stop(
    "Colunas obrigatórias ausentes na base escola × série: ",
    paste(
      colunas_ausentes_serie,
      collapse = ", "
    )
  )
}

colunas_logicas_perfil <- c(
  "municipalizada_apos_2024",
  "possivel_municipalizacao_recente",
  "escola_nova_recente",
  "privada_vinculada_final",
  "requer_revisao_tecnica",
  "divergencia_censo_cadastro",
  "painel_completo_cinco_series"
)

colunas_logicas_serie <- c(
  "painel_resultado_balanceado",
  "aumento_participacao_10pp",
  "queda_participacao_10pp",
  "mudanca_previstos_20pct"
)

perfil_escola <- perfil_escola |>
  mutate(
    across(
      all_of(colunas_logicas_perfil),
      as_logical_seguro
    )
  )

perfil_escola_serie <- perfil_escola_serie |>
  mutate(
    across(
      all_of(colunas_logicas_serie),
      as_logical_seguro
    ),
    ano_escolar = suppressWarnings(
      as.integer(ano_escolar)
    )
  )

chave_serie <- c(
  "id_escola",
  "ano_escolar",
  "componente"
)

if (anyDuplicated(perfil_escola$id_escola) > 0) {
  stop(
    "O perfil escolar possui mais de uma linha por escola."
  )
}

duplicidades_chave_serie <- perfil_escola_serie |>
  count(
    !!!rlang::syms(chave_serie)
  ) |>
  filter(n > 1)

if (nrow(duplicidades_chave_serie) > 0) {
  stop(
    "A base escola × série possui chaves duplicadas."
  )
}

if (!setequal(
  perfil_escola$id_escola,
  perfil_escola_serie$id_escola
)) {
  stop(
    "O conjunto de escolas difere entre as duas bases de entrada."
  )
}

if (any(
  !perfil_escola_serie$ano_escolar %in% 1:5,
  na.rm = TRUE
)) {
  stop(
    "Foram encontrados anos escolares fora do intervalo de 1 a 5."
  )
}

# -------------------------------------------------------------------
# 6. Parâmetros explícitos do índice
# -------------------------------------------------------------------

parametros_componentes <- tribble(
  ~dimensao, ~componente, ~variavel_origem, ~sentido, ~peso_dimensao, ~justificativa,
  "volume", "matriculas_anos_iniciais", "matriculas_anos_iniciais", "maior valor = maior carga", 0.60, "Representa o volume principal de estudantes potencialmente alcançados.",
  "volume", "turmas_anos_iniciais", "turmas_anos_iniciais", "maior valor = maior carga", 0.25, "Representa o número de unidades organizacionais e grupos escolares acompanhados.",
  "volume", "amplitude_etapas_ofertadas", "numero_etapas_amplas_ofertadas", "maior valor = maior carga", 0.15, "Representa maior amplitude organizacional da unidade escolar.",

  "estrutural", "infraestrutura_insuficiente", "indice_infraestrutura_basica", "menor valor = maior complexidade", 0.30, "Menor disponibilidade relativa de infraestrutura pode ampliar a necessidade de acompanhamento.",
  "estrutural", "pressao_alunos_por_turma", "alunos_por_turma_anos_iniciais", "maior valor = maior complexidade", 0.20, "Turmas relativamente maiores podem elevar a complexidade pedagógica e organizacional.",
  "estrutural", "pressao_alunos_por_docente", "alunos_por_docente_anos_iniciais", "maior valor = maior complexidade", 0.15, "Maior razão de estudantes por docente sinaliza pressão relativa sobre a oferta.",
  "estrutural", "participacao_educacao_especial", "pct_matriculas_educacao_especial", "maior valor = maior necessidade potencial de coordenação", 0.20, "Maior presença relativa de estudantes da educação especial pode exigir articulação e apoio diferenciados; não representa déficit da escola ou dos estudantes.",
  "estrutural", "amplitude_tempo_integral", "pct_matriculas_anos_iniciais_integral", "maior valor = maior complexidade operacional", 0.10, "A oferta em tempo integral amplia a jornada e a organização cotidiana da escola.",
  "estrutural", "dependencia_transporte_publico", "pct_matriculas_transporte_publico", "maior valor = maior complexidade logística", 0.05, "Maior dependência de transporte público pode ampliar desafios logísticos de acesso e frequência.",

  "educacional", "desempenho_relativo_2026", "proficiencia_media_2026 + pct_defasagem_2026, comparadas dentro da série", "menor proficiência e maior defasagem = maior desafio", 0.50, "Retrato relativo do desempenho observado em 2026, sem interpretação causal.",
  "educacional", "baixa_participacao_relativa_2026", "taxa_participacao_2026, comparada dentro da série", "menor participação = maior desafio de cobertura", 0.25, "Participação reduzida limita a cobertura e condiciona a interpretação dos resultados.",
  "educacional", "heterogeneidade_entre_series_2026", "desvio do escore relativo de desempenho entre séries", "maior dispersão = maior heterogeneidade", 0.15, "Trajetórias muito distintas entre séries podem demandar acompanhamento mais diferenciado.",
  "educacional", "instabilidade_composicao_comparavel", "mudanças relevantes de participação ou público previsto em séries comparáveis", "maior proporção = maior cautela e monitoramento", 0.10, "Mudanças de composição condicionam comparações temporais; não representam piora educacional.",

  "administrativa", "arranjo_institucional_especial", "grupo_administrativo_2024_final", "arranjo distinto da rede municipal direta = maior complexidade", 0.40, "Convênios, supervisão especial ou origem em outra dependência podem exigir articulação institucional adicional.",
  "administrativa", "transicao_institucional_recente", "municipalização, possível municipalização ou escola nova", "presença = maior complexidade", 0.30, "Transições recentes podem ampliar demandas de integração, informação e acompanhamento.",
  "administrativa", "historico_cadastral_ou_revisao", "requer_revisao_tecnica ou divergencia_censo_cadastro", "presença = maior complexidade", 0.20, "Histórico cadastral ou necessidade de revisão pode demandar atenção administrativa adicional.",
  "administrativa", "vinculo_assessoramento_nao_informado", "ausência de assessora_gerencial informada", "presença = maior complexidade", 0.10, "Ausência de vinculação administrativa explícita exige regularização gerencial."
) |>
  mutate(
    peso_no_indice_principal = 0.25 * peso_dimensao
  )

pesos_cenarios_dimensoes <- tribble(
  ~cenario, ~descricao, ~volume, ~estrutural, ~educacional, ~administrativa, ~principal,
  "equilibrado", "Pesos iguais entre as quatro dimensões; cenário principal provisório.", 0.25, 0.25, 0.25, 0.25, TRUE,
  "operacional", "Maior ênfase no volume e na estrutura operacional.", 0.40, 0.25, 0.20, 0.15, FALSE,
  "desafio_educacional", "Maior ênfase no desafio educacional observado.", 0.20, 0.25, 0.40, 0.15, FALSE,
  "transicao_administrativa", "Maior ênfase nos arranjos e transições administrativas.", 0.20, 0.20, 0.25, 0.35, FALSE
)

soma_pesos_componentes <- parametros_componentes |>
  group_by(dimensao) |>
  summarise(
    soma_pesos = sum(peso_dimensao),
    .groups = "drop"
  )

soma_pesos_cenarios <- pesos_cenarios_dimensoes |>
  transmute(
    cenario,
    soma_pesos = volume + estrutural + educacional + administrativa
  )

# -------------------------------------------------------------------
# 7. Dimensão de volume
# -------------------------------------------------------------------

# Zero turmas com matrículas positivas é tratado como indisponibilidade
# cadastral, e não como ausência real de turmas.
base_indice <- perfil_escola |>
  mutate(
    turmas_anos_iniciais_validas = if_else(
      !is.na(matriculas_anos_iniciais) &
        matriculas_anos_iniciais > 0 &
        !is.na(turmas_anos_iniciais) &
        turmas_anos_iniciais <= 0,
      NA_real_,
      as.numeric(turmas_anos_iniciais)
    ),
    score_volume_matriculas = percentil_relativo(
      matriculas_anos_iniciais
    ),
    score_volume_turmas = percentil_relativo(
      turmas_anos_iniciais_validas
    ),
    score_volume_etapas = percentil_relativo(
      numero_etapas_amplas_ofertadas
    )
  )

pesos_volume <- c(
  score_volume_matriculas = 0.60,
  score_volume_turmas = 0.25,
  score_volume_etapas = 0.15
)

resultado_volume <- calcular_score_ponderado(
  base_indice,
  names(pesos_volume),
  pesos_volume,
  cobertura_minima = 0.75
)

base_indice <- base_indice |>
  mutate(
    score_dimensao_volume = resultado_volume$score,
    cobertura_componentes_volume = resultado_volume$cobertura
  )

# -------------------------------------------------------------------
# 8. Dimensão de complexidade estrutural
# -------------------------------------------------------------------

base_indice <- base_indice |>
  mutate(
    score_estrutural_infraestrutura = percentil_relativo(
      indice_infraestrutura_basica,
      inverso = TRUE
    ),
    score_estrutural_alunos_turma = percentil_relativo(
      alunos_por_turma_anos_iniciais
    ),
    score_estrutural_alunos_docente = percentil_relativo(
      alunos_por_docente_anos_iniciais
    ),
    score_estrutural_educacao_especial = percentil_relativo(
      pct_matriculas_educacao_especial
    ),
    score_estrutural_tempo_integral = percentil_relativo(
      pct_matriculas_anos_iniciais_integral
    ),
    score_estrutural_transporte = percentil_relativo(
      pct_matriculas_transporte_publico
    )
  )

pesos_estrutural <- c(
  score_estrutural_infraestrutura = 0.30,
  score_estrutural_alunos_turma = 0.20,
  score_estrutural_alunos_docente = 0.15,
  score_estrutural_educacao_especial = 0.20,
  score_estrutural_tempo_integral = 0.10,
  score_estrutural_transporte = 0.05
)

resultado_estrutural <- calcular_score_ponderado(
  base_indice,
  names(pesos_estrutural),
  pesos_estrutural,
  cobertura_minima = 0.60
)

base_indice <- base_indice |>
  mutate(
    score_dimensao_estrutural = resultado_estrutural$score,
    cobertura_componentes_estrutural = resultado_estrutural$cobertura
  )

# -------------------------------------------------------------------
# 9. Dimensão de desafio educacional
# -------------------------------------------------------------------

componentes_educacionais_serie <- perfil_escola_serie |>
  group_by(ano_escolar) |>
  mutate(
    score_baixa_proficiencia_2026 = percentil_relativo(
      proficiencia_media_2026,
      inverso = TRUE
    ),
    score_alta_defasagem_2026 = percentil_relativo(
      pct_defasagem_2026
    ),
    score_desempenho_relativo_2026 = rowMeans(
      cbind(
        score_baixa_proficiencia_2026,
        score_alta_defasagem_2026
      ),
      na.rm = TRUE
    ),
    score_desempenho_relativo_2026 = if_else(
      is.nan(score_desempenho_relativo_2026),
      NA_real_,
      score_desempenho_relativo_2026
    ),
    score_baixa_participacao_2026 = percentil_relativo(
      taxa_participacao_2026,
      inverso = TRUE
    ),
    instabilidade_composicao_comparavel = case_when(
      painel_resultado_balanceado %in% TRUE ~
        coalesce(aumento_participacao_10pp, FALSE) |
        coalesce(queda_participacao_10pp, FALSE) |
        coalesce(mudanca_previstos_20pct, FALSE),
      TRUE ~ NA
    )
  ) |>
  ungroup()

sintese_educacional_escola <- componentes_educacionais_serie |>
  group_by(id_escola) |>
  summarise(
    numero_series_resultado_2026_calculado = sum(
      !is.na(score_desempenho_relativo_2026)
    ),
    numero_series_participacao_2026_calculado = sum(
      !is.na(score_baixa_participacao_2026)
    ),
    numero_series_comparaveis_calculado = sum(
      painel_resultado_balanceado %in% TRUE,
      na.rm = TRUE
    ),
    score_educacional_desempenho = media_segura(
      score_desempenho_relativo_2026
    ),
    score_educacional_baixa_participacao = media_segura(
      score_baixa_participacao_2026
    ),
    heterogeneidade_desempenho_bruta = desvio_padrao_seguro(
      score_desempenho_relativo_2026
    ),
    score_educacional_instabilidade_composicao = if_else(
      any(!is.na(instabilidade_composicao_comparavel)),
      100 * mean(
        as.numeric(instabilidade_composicao_comparavel),
        na.rm = TRUE
      ),
      NA_real_
    ),
    .groups = "drop"
  ) |>
  mutate(
    score_educacional_heterogeneidade = percentil_relativo(
      heterogeneidade_desempenho_bruta
    ),
    proporcao_series_resultado_2026_indice =
      100 * numero_series_resultado_2026_calculado / 5,
    proporcao_series_comparaveis_indice =
      100 * numero_series_comparaveis_calculado / 5,
    qualidade_evidencia_educacional = case_when(
      numero_series_resultado_2026_calculado == 5 ~ "Completa — cinco séries",
      numero_series_resultado_2026_calculado %in% 3:4 ~ "Parcial — três ou quatro séries",
      numero_series_resultado_2026_calculado %in% 1:2 ~ "Limitada — uma ou duas séries",
      TRUE ~ "Sem resultado educacional disponível"
    )
  )

base_indice <- base_indice |>
  left_join(
    sintese_educacional_escola,
    by = "id_escola"
  )

pesos_educacional <- c(
  score_educacional_desempenho = 0.50,
  score_educacional_baixa_participacao = 0.25,
  score_educacional_heterogeneidade = 0.15,
  score_educacional_instabilidade_composicao = 0.10
)

resultado_educacional <- calcular_score_ponderado(
  base_indice,
  names(pesos_educacional),
  pesos_educacional,
  cobertura_minima = 0.50
)

base_indice <- base_indice |>
  mutate(
    score_dimensao_educacional = resultado_educacional$score,
    cobertura_componentes_educacional = resultado_educacional$cobertura,
    interpretacao_educacional_cautelosa =
      numero_series_resultado_2026_calculado < 3 |
      cobertura_componentes_educacional < 0.75
  )

# -------------------------------------------------------------------
# 10. Dimensão de complexidade administrativa
# -------------------------------------------------------------------

base_indice <- base_indice |>
  mutate(
    score_administrativo_arranjo_especial = if_else(
      !is.na(grupo_administrativo_2024_final) &
        grupo_administrativo_2024_final !=
        "Rede municipal direta em 2024",
      100,
      0
    ),
    score_administrativo_transicao_recente = if_else(
      coalesce(municipalizada_apos_2024, FALSE) |
        coalesce(possivel_municipalizacao_recente, FALSE) |
        coalesce(escola_nova_recente, FALSE),
      100,
      0
    ),
    score_administrativo_historico_cadastral = if_else(
      coalesce(requer_revisao_tecnica, FALSE) |
        coalesce(divergencia_censo_cadastro, FALSE),
      100,
      0
    ),
    score_administrativo_sem_vinculo = if_else(
      is.na(assessora_gerencial) |
        str_trim(assessora_gerencial) == "" |
        assessora_gerencial == "Sem vinculação informada",
      100,
      0
    )
  )

pesos_administrativa <- c(
  score_administrativo_arranjo_especial = 0.40,
  score_administrativo_transicao_recente = 0.30,
  score_administrativo_historico_cadastral = 0.20,
  score_administrativo_sem_vinculo = 0.10
)

resultado_administrativa <- calcular_score_ponderado(
  base_indice,
  names(pesos_administrativa),
  pesos_administrativa,
  cobertura_minima = 1
)

base_indice <- base_indice |>
  mutate(
    score_dimensao_administrativa = resultado_administrativa$score,
    cobertura_componentes_administrativa = resultado_administrativa$cobertura
  )

# -------------------------------------------------------------------
# 11. Índice principal e cenários de sensibilidade
# -------------------------------------------------------------------

nomes_dimensoes <- c(
  volume = "score_dimensao_volume",
  estrutural = "score_dimensao_estrutural",
  educacional = "score_dimensao_educacional",
  administrativa = "score_dimensao_administrativa"
)

for (i in seq_len(nrow(pesos_cenarios_dimensoes))) {
  linha <- pesos_cenarios_dimensoes[i, ]
  nome_cenario <- linha$cenario[[1]]

  pesos_cenario <- c(
    score_dimensao_volume = linha$volume[[1]],
    score_dimensao_estrutural = linha$estrutural[[1]],
    score_dimensao_educacional = linha$educacional[[1]],
    score_dimensao_administrativa = linha$administrativa[[1]]
  )

  resultado_cenario <- calcular_score_ponderado(
    base_indice,
    names(pesos_cenario),
    pesos_cenario,
    cobertura_minima = 0.75
  )

  base_indice[[paste0(
    "indice_carga_potencial_",
    nome_cenario
  )]] <- resultado_cenario$score

  base_indice[[paste0(
    "cobertura_indice_",
    nome_cenario
  )]] <- resultado_cenario$cobertura

  base_indice[[paste0(
    "faixa_indice_",
    nome_cenario
  )]] <- atribuir_faixa_relativa(
    resultado_cenario$score
  )
}

base_indice <- base_indice |>
  mutate(
    indice_carga_potencial = indice_carga_potencial_equilibrado,
    percentil_indice_carga_potencial = percentil_relativo(
      indice_carga_potencial
    ),
    faixa_indice_carga_potencial = faixa_indice_equilibrado,
    contribuicao_volume_indice_principal =
      0.25 * score_dimensao_volume,
    contribuicao_estrutural_indice_principal =
      0.25 * score_dimensao_estrutural,
    contribuicao_educacional_indice_principal =
      0.25 * score_dimensao_educacional,
    contribuicao_administrativa_indice_principal =
      0.25 * score_dimensao_administrativa,
    interpretacao_indice_cautelosa =
      interpretacao_educacional_cautelosa |
      cobertura_componentes_volume < 1 |
      cobertura_componentes_estrutural < 1 |
      cobertura_componentes_educacional < 1 |
      cobertura_componentes_administrativa < 1
  )

# -------------------------------------------------------------------
# 12. Base final de índices
# -------------------------------------------------------------------

indice_carga_potencial_escola <- base_indice |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_gerencial,
    grupo_administrativo_2024_final,
    tipo_vinculo_rede_final,
    status_rede_2025_final,
    matriculas_anos_iniciais,
    turmas_anos_iniciais,
    numero_etapas_amplas_ofertadas,
    indice_infraestrutura_basica,
    alunos_por_turma_anos_iniciais,
    alunos_por_docente_anos_iniciais,
    pct_matriculas_educacao_especial,
    pct_matriculas_anos_iniciais_integral,
    pct_matriculas_transporte_publico,
    numero_series_resultado_2026,
    numero_series_comparaveis,
    painel_completo_cinco_series,
    starts_with("score_volume_"),
    starts_with("score_estrutural_"),
    starts_with("score_educacional_"),
    heterogeneidade_desempenho_bruta,
    numero_series_resultado_2026_calculado,
    numero_series_participacao_2026_calculado,
    numero_series_comparaveis_calculado,
    proporcao_series_resultado_2026_indice,
    proporcao_series_comparaveis_indice,
    qualidade_evidencia_educacional,
    interpretacao_educacional_cautelosa,
    starts_with("score_administrativo_"),
    starts_with("score_dimensao_"),
    starts_with("cobertura_componentes_"),
    starts_with("indice_carga_potencial_"),
    starts_with("cobertura_indice_"),
    starts_with("faixa_indice_"),
    indice_carga_potencial,
    percentil_indice_carga_potencial,
    faixa_indice_carga_potencial,
    starts_with("contribuicao_"),
    interpretacao_indice_cautelosa
  ) |>
  arrange(
    desc(indice_carga_potencial),
    nome_canonico
  )

# -------------------------------------------------------------------
# 13. Base longa de componentes
# -------------------------------------------------------------------

montar_componente_longo <- function(
    dados,
    dimensao,
    componente,
    variavel_bruta,
    variavel_score,
    peso_dimensao,
    peso_indice_principal = 0.25) {

  if (!variavel_bruta %in% names(dados)) {
    stop(
      "Variável bruta ausente ao montar componente longo: ",
      variavel_bruta
    )
  }

  if (!variavel_score %in% names(dados)) {
    stop(
      "Variável de escore ausente ao montar componente longo: ",
      variavel_score
    )
  }

  valor_bruto_vetor <- suppressWarnings(
    as.numeric(
      dados[[variavel_bruta]]
    )
  )

  score_vetor <- suppressWarnings(
    as.numeric(
      dados[[variavel_score]]
    )
  )

  dimensao_vetor <- rep(
    dimensao,
    nrow(dados)
  )

  componente_vetor <- rep(
    componente,
    nrow(dados)
  )

  variavel_bruta_vetor <- rep(
    variavel_bruta,
    nrow(dados)
  )

  peso_dimensao_vetor <- rep(
    peso_dimensao,
    nrow(dados)
  )

  peso_indice_vetor <- rep(
    peso_dimensao * peso_indice_principal,
    nrow(dados)
  )

  dados |>
    transmute(
      id_escola,
      codigo_inep,
      nome_canonico,
      assessora_gerencial,
      dimensao = dimensao_vetor,
      componente = componente_vetor,
      variavel_bruta = variavel_bruta_vetor,
      valor_bruto = valor_bruto_vetor,
      score_componente_0_100 = score_vetor,
      peso_declarado_na_dimensao = peso_dimensao_vetor,
      peso_declarado_no_indice_principal = peso_indice_vetor,
      componente_disponivel = !is.na(score_componente_0_100)
    )
}

componentes_indice_carga_potencial <- bind_rows(
  montar_componente_longo(
    base_indice,
    "volume",
    "matriculas_anos_iniciais",
    "matriculas_anos_iniciais",
    "score_volume_matriculas",
    0.60
  ),
  montar_componente_longo(
    base_indice,
    "volume",
    "turmas_anos_iniciais",
    "turmas_anos_iniciais_validas",
    "score_volume_turmas",
    0.25
  ),
  montar_componente_longo(
    base_indice,
    "volume",
    "amplitude_etapas_ofertadas",
    "numero_etapas_amplas_ofertadas",
    "score_volume_etapas",
    0.15
  ),
  montar_componente_longo(
    base_indice,
    "estrutural",
    "infraestrutura_insuficiente",
    "indice_infraestrutura_basica",
    "score_estrutural_infraestrutura",
    0.30
  ),
  montar_componente_longo(
    base_indice,
    "estrutural",
    "pressao_alunos_por_turma",
    "alunos_por_turma_anos_iniciais",
    "score_estrutural_alunos_turma",
    0.20
  ),
  montar_componente_longo(
    base_indice,
    "estrutural",
    "pressao_alunos_por_docente",
    "alunos_por_docente_anos_iniciais",
    "score_estrutural_alunos_docente",
    0.15
  ),
  montar_componente_longo(
    base_indice,
    "estrutural",
    "participacao_educacao_especial",
    "pct_matriculas_educacao_especial",
    "score_estrutural_educacao_especial",
    0.20
  ),
  montar_componente_longo(
    base_indice,
    "estrutural",
    "amplitude_tempo_integral",
    "pct_matriculas_anos_iniciais_integral",
    "score_estrutural_tempo_integral",
    0.10
  ),
  montar_componente_longo(
    base_indice,
    "estrutural",
    "dependencia_transporte_publico",
    "pct_matriculas_transporte_publico",
    "score_estrutural_transporte",
    0.05
  ),
  montar_componente_longo(
    base_indice,
    "educacional",
    "desempenho_relativo_2026",
    "score_educacional_desempenho",
    "score_educacional_desempenho",
    0.50
  ),
  montar_componente_longo(
    base_indice,
    "educacional",
    "baixa_participacao_relativa_2026",
    "score_educacional_baixa_participacao",
    "score_educacional_baixa_participacao",
    0.25
  ),
  montar_componente_longo(
    base_indice,
    "educacional",
    "heterogeneidade_entre_series_2026",
    "heterogeneidade_desempenho_bruta",
    "score_educacional_heterogeneidade",
    0.15
  ),
  montar_componente_longo(
    base_indice,
    "educacional",
    "instabilidade_composicao_comparavel",
    "score_educacional_instabilidade_composicao",
    "score_educacional_instabilidade_composicao",
    0.10
  ),
  montar_componente_longo(
    base_indice,
    "administrativa",
    "arranjo_institucional_especial",
    "score_administrativo_arranjo_especial",
    "score_administrativo_arranjo_especial",
    0.40
  ),
  montar_componente_longo(
    base_indice,
    "administrativa",
    "transicao_institucional_recente",
    "score_administrativo_transicao_recente",
    "score_administrativo_transicao_recente",
    0.30
  ),
  montar_componente_longo(
    base_indice,
    "administrativa",
    "historico_cadastral_ou_revisao",
    "score_administrativo_historico_cadastral",
    "score_administrativo_historico_cadastral",
    0.20
  ),
  montar_componente_longo(
    base_indice,
    "administrativa",
    "vinculo_assessoramento_nao_informado",
    "score_administrativo_sem_vinculo",
    "score_administrativo_sem_vinculo",
    0.10
  )
) |>
  arrange(
    id_escola,
    dimensao,
    componente
  )

# -------------------------------------------------------------------
# 14. Diagnósticos e análise de sensibilidade
# -------------------------------------------------------------------

variaveis_componentes_score <- c(
  names(pesos_volume),
  names(pesos_estrutural),
  names(pesos_educacional),
  names(pesos_administrativa)
)

variaveis_dimensoes <- unname(
  nomes_dimensoes
)

variaveis_indices_cenarios <- paste0(
  "indice_carga_potencial_",
  pesos_cenarios_dimensoes$cenario
)

distribuicao_escores <- bind_rows(
  resumo_numerico(
    base_indice,
    variaveis_componentes_score,
    "componentes"
  ),
  resumo_numerico(
    base_indice,
    variaveis_dimensoes,
    "dimensoes"
  ),
  resumo_numerico(
    base_indice,
    variaveis_indices_cenarios,
    "indices_cenarios"
  )
)

matriz_correlacao <- base_indice |>
  select(
    all_of(
      c(
        variaveis_componentes_score,
        variaveis_dimensoes,
        variaveis_indices_cenarios
      )
    )
  ) |>
  cor(
    use = "pairwise.complete.obs",
    method = "spearman"
  )

correlacoes_componentes_dimensoes_indices <- as.data.frame(
  matriz_correlacao
) |>
  rownames_to_column(
    "variavel_1"
  ) |>
  pivot_longer(
    -variavel_1,
    names_to = "variavel_2",
    values_to = "correlacao_spearman"
  )

cobertura_dimensoes <- base_indice |>
  transmute(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_gerencial,
    cobertura_componentes_volume,
    cobertura_componentes_estrutural,
    cobertura_componentes_educacional,
    cobertura_componentes_administrativa,
    numero_series_resultado_2026_calculado,
    numero_series_comparaveis_calculado,
    qualidade_evidencia_educacional,
    interpretacao_educacional_cautelosa,
    interpretacao_indice_cautelosa
  )

escolas_interpretacao_cautelosa <- cobertura_dimensoes |>
  filter(
    interpretacao_indice_cautelosa %in% TRUE
  ) |>
  arrange(
    nome_canonico
  )

analise_sensibilidade_escolas <- base_indice |>
  transmute(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_gerencial,
    indice_equilibrado = indice_carga_potencial_equilibrado,
    faixa_equilibrado = faixa_indice_equilibrado,
    indice_operacional = indice_carga_potencial_operacional,
    faixa_operacional = faixa_indice_operacional,
    diferenca_operacional_equilibrado =
      indice_operacional - indice_equilibrado,
    indice_desafio_educacional =
      indice_carga_potencial_desafio_educacional,
    faixa_desafio_educacional =
      faixa_indice_desafio_educacional,
    diferenca_desafio_equilibrado =
      indice_desafio_educacional - indice_equilibrado,
    indice_transicao_administrativa =
      indice_carga_potencial_transicao_administrativa,
    faixa_transicao_administrativa =
      faixa_indice_transicao_administrativa,
    diferenca_transicao_equilibrado =
      indice_transicao_administrativa - indice_equilibrado,
    numero_cenarios_com_mudanca_faixa =
      as.integer(faixa_operacional != faixa_equilibrado) +
      as.integer(faixa_desafio_educacional != faixa_equilibrado) +
      as.integer(faixa_transicao_administrativa != faixa_equilibrado),
    sensibilidade_faixa = case_when(
      numero_cenarios_com_mudanca_faixa == 0 ~ "Estável nos cenários",
      numero_cenarios_com_mudanca_faixa == 1 ~ "Sensibilidade moderada",
      numero_cenarios_com_mudanca_faixa >= 2 ~ "Sensibilidade elevada",
      TRUE ~ NA_character_
    )
  ) |>
  arrange(
    desc(numero_cenarios_com_mudanca_faixa),
    desc(indice_equilibrado)
  )

faixa_superior <- "Faixa 4 — maior carga potencial relativa"

resumo_sensibilidade <- pesos_cenarios_dimensoes |>
  filter(
    cenario != "equilibrado"
  ) |>
  rowwise() |>
  mutate(
    variavel_indice = paste0(
      "indice_carga_potencial_",
      cenario
    ),
    variavel_faixa = paste0(
      "faixa_indice_",
      cenario
    ),
    correlacao_spearman_com_principal = cor(
      base_indice[[variavel_indice]],
      base_indice$indice_carga_potencial_equilibrado,
      method = "spearman",
      use = "complete.obs"
    ),
    diferenca_absoluta_media = mean(
      abs(
        base_indice[[variavel_indice]] -
          base_indice$indice_carga_potencial_equilibrado
      ),
      na.rm = TRUE
    ),
    diferenca_absoluta_maxima = max(
      abs(
        base_indice[[variavel_indice]] -
          base_indice$indice_carga_potencial_equilibrado
      ),
      na.rm = TRUE
    ),
    escolas_com_mudanca_de_faixa = sum(
      base_indice[[variavel_faixa]] !=
        base_indice$faixa_indice_equilibrado,
      na.rm = TRUE
    ),
    escolas_na_faixa_superior_principal = sum(
      base_indice$faixa_indice_equilibrado == faixa_superior,
      na.rm = TRUE
    ),
    escolas_na_faixa_superior_cenario = sum(
      base_indice[[variavel_faixa]] == faixa_superior,
      na.rm = TRUE
    ),
    escolas_em_comum_na_faixa_superior = sum(
      base_indice$faixa_indice_equilibrado == faixa_superior &
        base_indice[[variavel_faixa]] == faixa_superior,
      na.rm = TRUE
    )
  ) |>
  ungroup() |>
  select(
    cenario,
    descricao,
    correlacao_spearman_com_principal,
    diferenca_absoluta_media,
    diferenca_absoluta_maxima,
    escolas_com_mudanca_de_faixa,
    escolas_na_faixa_superior_principal,
    escolas_na_faixa_superior_cenario,
    escolas_em_comum_na_faixa_superior
  )

distribuicao_faixas <- indice_carga_potencial_escola |>
  count(
    faixa_indice_carga_potencial,
    name = "numero_escolas"
  ) |>
  mutate(
    percentual_escolas =
      100 * numero_escolas / sum(numero_escolas)
  )

# -------------------------------------------------------------------
# 15. Validações finais
# -------------------------------------------------------------------

normalizacao_educacional_por_serie <- componentes_educacionais_serie |>
  group_by(ano_escolar) |>
  summarise(
    media_score_baixa_proficiencia = mean(
      score_baixa_proficiencia_2026,
      na.rm = TRUE
    ),
    media_score_alta_defasagem = mean(
      score_alta_defasagem_2026,
      na.rm = TRUE
    ),
    media_score_baixa_participacao = mean(
      score_baixa_participacao_2026,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

variaveis_proibidas_indice <- c(
  "pct_matriculas_preta_parda_indigena",
  "delta_proficiencia",
  "delta_proficiencia_multisserie_ponderada",
  "proporcao_series_delta_proficiencia_positivo"
)

texto_parametros <- paste(
  parametros_componentes$variavel_origem,
  collapse = " | "
)

numero_variaveis_proibidas <- sum(
  map_lgl(
    variaveis_proibidas_indice,
    ~ str_detect(
      texto_parametros,
      fixed(
        .x,
        ignore_case = TRUE
      )
    )
  )
)

adicionar_validacao <- function(
    teste,
    categoria,
    severidade,
    valor_observado,
    criterio,
    status,
    detalhe) {
  tibble(
    teste = teste,
    categoria = categoria,
    severidade = severidade,
    valor_observado = as.character(valor_observado),
    criterio = criterio,
    status = status,
    detalhe = detalhe
  )
}

validacoes <- bind_rows(
  adicionar_validacao(
    "Unicidade do perfil de entrada",
    "estrutura",
    "erro",
    nrow(perfil_escola) - n_distinct(perfil_escola$id_escola),
    "zero linhas excedentes",
    if_else(
      nrow(perfil_escola) == n_distinct(perfil_escola$id_escola),
      "APROVADO",
      "REPROVADO"
    ),
    "O perfil deve possuir uma linha por escola."
  ),
  adicionar_validacao(
    "Unicidade da base escola × série",
    "estrutura",
    "erro",
    nrow(duplicidades_chave_serie),
    "zero chaves duplicadas",
    if_else(
      nrow(duplicidades_chave_serie) == 0,
      "APROVADO",
      "REPROVADO"
    ),
    "Cada escola × série × componente deve ser única."
  ),
  adicionar_validacao(
    "Correspondência das escolas entre entrada e saída",
    "estrutura",
    "erro",
    length(
      setdiff(
        perfil_escola$id_escola,
        indice_carga_potencial_escola$id_escola
      )
    ) + length(
      setdiff(
        indice_carga_potencial_escola$id_escola,
        perfil_escola$id_escola
      )
    ),
    "zero diferenças",
    if_else(
      setequal(
        perfil_escola$id_escola,
        indice_carga_potencial_escola$id_escola
      ),
      "APROVADO",
      "REPROVADO"
    ),
    "Nenhuma escola pode ser incluída ou excluída silenciosamente."
  ),
  adicionar_validacao(
    "Soma dos pesos dos componentes",
    "metodologia",
    "erro",
    max(
      abs(
        soma_pesos_componentes$soma_pesos - 1
      )
    ),
    "diferença máxima inferior a 1e-10",
    if_else(
      all(
        abs(
          soma_pesos_componentes$soma_pesos - 1
        ) < 1e-10
      ),
      "APROVADO",
      "REPROVADO"
    ),
    "Os pesos internos de cada dimensão devem somar 1."
  ),
  adicionar_validacao(
    "Soma dos pesos dos cenários",
    "metodologia",
    "erro",
    max(
      abs(
        soma_pesos_cenarios$soma_pesos - 1
      )
    ),
    "diferença máxima inferior a 1e-10",
    if_else(
      all(
        abs(
          soma_pesos_cenarios$soma_pesos - 1
        ) < 1e-10
      ),
      "APROVADO",
      "REPROVADO"
    ),
    "Os pesos das quatro dimensões devem somar 1 em todos os cenários."
  ),
  adicionar_validacao(
    "Escores dimensionais dentro de 0 a 100",
    "consistencia",
    "erro",
    sum(
      map_int(
        base_indice[variaveis_dimensoes],
        ~ sum(
          !is.na(.x) &
            (.x < -1e-8 | .x > 100 + 1e-8)
        )
      )
    ),
    "zero valores fora do intervalo",
    if_else(
      sum(
        map_int(
          base_indice[variaveis_dimensoes],
          ~ sum(
            !is.na(.x) &
              (.x < -1e-8 | .x > 100 + 1e-8)
          )
        )
      ) == 0,
      "APROVADO",
      "REPROVADO"
    ),
    "Todos os escores dimensionais devem permanecer no intervalo de 0 a 100."
  ),
  adicionar_validacao(
    "Índices dos cenários dentro de 0 a 100",
    "consistencia",
    "erro",
    sum(
      map_int(
        base_indice[variaveis_indices_cenarios],
        ~ sum(
          !is.na(.x) &
            (.x < -1e-8 | .x > 100 + 1e-8)
        )
      )
    ),
    "zero valores fora do intervalo",
    if_else(
      sum(
        map_int(
          base_indice[variaveis_indices_cenarios],
          ~ sum(
            !is.na(.x) &
              (.x < -1e-8 | .x > 100 + 1e-8)
          )
        )
      ) == 0,
      "APROVADO",
      "REPROVADO"
    ),
    "Todos os índices devem permanecer no intervalo de 0 a 100."
  ),
  adicionar_validacao(
    "Disponibilidade do índice principal",
    "cobertura",
    "erro",
    sum(
      is.na(
        indice_carga_potencial_escola$indice_carga_potencial
      )
    ),
    "zero escolas sem índice principal",
    if_else(
      all(
        !is.na(
          indice_carga_potencial_escola$indice_carga_potencial
        )
      ),
      "APROVADO",
      "REPROVADO"
    ),
    "Todas as escolas devem receber índice, acompanhado de flags de cobertura quando necessário."
  ),
  adicionar_validacao(
    "Normalização educacional dentro da série",
    "metodologia",
    "erro",
    max(
      abs(
        c(
          normalizacao_educacional_por_serie$media_score_baixa_proficiencia,
          normalizacao_educacional_por_serie$media_score_alta_defasagem,
          normalizacao_educacional_por_serie$media_score_baixa_participacao
        ) - 50
      ),
      na.rm = TRUE
    ),
    "diferença máxima inferior a 1e-8",
    if_else(
      max(
        abs(
          c(
            normalizacao_educacional_por_serie$media_score_baixa_proficiencia,
            normalizacao_educacional_por_serie$media_score_alta_defasagem,
            normalizacao_educacional_por_serie$media_score_baixa_participacao
          ) - 50
        ),
        na.rm = TRUE
      ) < 1e-8,
      "APROVADO",
      "REPROVADO"
    ),
    "A posição percentílica deve ser calculada separadamente dentro de cada série."
  ),
  adicionar_validacao(
    "Ausência de variáveis proibidas no índice",
    "metodologia",
    "erro",
    numero_variaveis_proibidas,
    "zero ocorrências",
    if_else(
      numero_variaveis_proibidas == 0,
      "APROVADO",
      "REPROVADO"
    ),
    "Composição racial e variações de proficiência não devem compor o índice."
  ),
  adicionar_validacao(
    "Cobertura mínima da dimensão de volume",
    "cobertura",
    "erro",
    min(
      base_indice$cobertura_componentes_volume,
      na.rm = TRUE
    ),
    "mínimo de 0,75",
    if_else(
      all(
        base_indice$cobertura_componentes_volume >= 0.75
      ),
      "APROVADO",
      "REPROVADO"
    ),
    "A dimensão de volume deve usar ao menos 75% dos pesos declarados."
  ),
  adicionar_validacao(
    "Cobertura mínima da dimensão estrutural",
    "cobertura",
    "erro",
    min(
      base_indice$cobertura_componentes_estrutural,
      na.rm = TRUE
    ),
    "mínimo de 0,60",
    if_else(
      all(
        base_indice$cobertura_componentes_estrutural >= 0.60
      ),
      "APROVADO",
      "REPROVADO"
    ),
    "A dimensão estrutural deve usar ao menos 60% dos pesos declarados."
  ),
  adicionar_validacao(
    "Cobertura mínima da dimensão educacional",
    "cobertura",
    "erro",
    min(
      base_indice$cobertura_componentes_educacional,
      na.rm = TRUE
    ),
    "mínimo de 0,50",
    if_else(
      all(
        base_indice$cobertura_componentes_educacional >= 0.50
      ),
      "APROVADO",
      "REPROVADO"
    ),
    "A dimensão educacional deve usar ao menos metade dos pesos declarados e manter indicador de qualidade da evidência."
  ),
  adicionar_validacao(
    "Identidade da assessora não usada como componente",
    "metodologia",
    "erro",
    sum(
      parametros_componentes$variavel_origem %in%
        unique(
          na.omit(
            perfil_escola$assessora_gerencial
          )
        )
    ),
    "zero nomes de assessoras nos parâmetros",
    if_else(
      !any(
        parametros_componentes$variavel_origem %in%
          unique(
            na.omit(
              perfil_escola$assessora_gerencial
            )
          )
      ),
      "APROVADO",
      "REPROVADO"
    ),
    "O índice pode registrar ausência de vínculo, mas não pode pontuar nomes específicos de assessoras."
  )
)

numero_erros_criticos <- validacoes |>
  filter(
    severidade == "erro",
    status == "REPROVADO"
  ) |>
  nrow()

# -------------------------------------------------------------------
# 16. Dicionário das principais variáveis de saída
# -------------------------------------------------------------------

descricoes_indice <- c(
  indice_carga_potencial = "Índice principal provisório, de 0 a 100, com pesos iguais entre volume, estrutura, desafio educacional e complexidade administrativa.",
  percentil_indice_carga_potencial = "Posição percentílica relativa da escola no índice principal dentro do universo analisado.",
  faixa_indice_carga_potencial = "Faixa relativa do índice principal; não representa qualidade da escola.",
  score_dimensao_volume = "Escore de volume de atendimento, de 0 a 100.",
  score_dimensao_estrutural = "Escore de complexidade estrutural, de 0 a 100.",
  score_dimensao_educacional = "Escore de desafio educacional observado, de 0 a 100, sem interpretação causal.",
  score_dimensao_administrativa = "Escore de complexidade administrativa, de 0 a 100.",
  qualidade_evidencia_educacional = "Classificação da quantidade de séries com resultados de 2026 disponíveis.",
  interpretacao_educacional_cautelosa = "Indica cobertura educacional limitada ou cobertura insuficiente de componentes.",
  interpretacao_indice_cautelosa = "Indica que ao menos uma dimensão utilizou cobertura incompleta ou evidência educacional limitada."
)

dicionario_indice <- tibble(
  ordem_coluna = seq_along(indice_carga_potencial_escola),
  variavel = names(indice_carga_potencial_escola),
  classe_r = map_chr(
    indice_carga_potencial_escola,
    ~ paste(
      class(.x),
      collapse = " | "
    )
  )
) |>
  mutate(
    descricao = unname(
      descricoes_indice[variavel]
    ),
    descricao = if_else(
      is.na(descricao) | descricao == "",
      paste0(
        "Variável do módulo 17: ",
        str_replace_all(
          variavel,
          "_",
          " "
        ),
        ". Consulte os parâmetros de componentes e pesos para a definição operacional."
      ),
      descricao
    )
  )

# -------------------------------------------------------------------
# 17. Estrutura e manifesto preliminar
# -------------------------------------------------------------------

estrutura_bases_saida <- bind_rows(
  estrutura_base(
    indice_carga_potencial_escola,
    "indice_carga_potencial_escola"
  ),
  estrutura_base(
    componentes_indice_carga_potencial,
    "componentes_indice_carga_potencial"
  ),
  estrutura_base(
    componentes_educacionais_serie,
    "componentes_educacionais_escola_serie"
  )
)

# -------------------------------------------------------------------
# 18. Exportação dos diagnósticos da execução
# -------------------------------------------------------------------

arquivos_diagnostico <- c(
  "01_parametros_componentes.csv",
  "02_pesos_cenarios_dimensoes.csv",
  "03_distribuicao_escores.csv",
  "04_correlacoes_componentes_dimensoes_indices.csv",
  "05_cobertura_dimensoes.csv",
  "06_escolas_interpretacao_cautelosa.csv",
  "07_distribuicao_faixas_indice_principal.csv",
  "08_analise_sensibilidade_escolas.csv",
  "09_resumo_sensibilidade.csv",
  "10_normalizacao_educacional_por_serie.csv",
  "11_validacao_final.csv",
  "12_estrutura_bases_saida.csv"
)

walk2(
  list(
    parametros_componentes,
    pesos_cenarios_dimensoes,
    distribuicao_escores,
    correlacoes_componentes_dimensoes_indices,
    cobertura_dimensoes,
    escolas_interpretacao_cautelosa,
    distribuicao_faixas,
    analise_sensibilidade_escolas,
    resumo_sensibilidade,
    normalizacao_educacional_por_serie,
    validacoes,
    estrutura_bases_saida
  ),
  arquivos_diagnostico,
  ~ write_csv(
    .x,
    file.path(
      pasta_execucao,
      .y
    ),
    na = ""
  )
)

if (numero_erros_criticos > 0) {
  stop(
    "O módulo 17 encontrou ",
    numero_erros_criticos,
    " erro(s) crítico(s). Consulte `11_validacao_final.csv` em: ",
    pasta_execucao,
    ". Os produtos canônicos não foram atualizados."
  )
}

# -------------------------------------------------------------------
# 19. Arquivamento e gravação dos produtos canônicos
# -------------------------------------------------------------------

walk(
  arquivos_saida,
  arquivar_se_existir
)

write_csv(
  indice_carga_potencial_escola,
  arquivos_saida[["indice_csv"]],
  na = ""
)

saveRDS(
  indice_carga_potencial_escola,
  arquivos_saida[["indice_rds"]]
)

write_csv(
  componentes_indice_carga_potencial,
  arquivos_saida[["componentes_csv"]],
  na = ""
)

saveRDS(
  componentes_indice_carga_potencial,
  arquivos_saida[["componentes_rds"]]
)

write_csv(
  componentes_educacionais_serie,
  arquivos_saida[["componentes_educacionais_csv"]],
  na = ""
)

saveRDS(
  componentes_educacionais_serie,
  arquivos_saida[["componentes_educacionais_rds"]]
)

write_csv(
  dicionario_indice,
  arquivos_saida[["dicionario_csv"]],
  na = ""
)

# -------------------------------------------------------------------
# 20. Manifesto dos produtos e informações da sessão
# -------------------------------------------------------------------

manifesto_produtos <- tibble(
  produto = names(arquivos_saida),
  caminho = unname(arquivos_saida),
  existe = file.exists(unname(arquivos_saida)),
  tamanho_bytes = if_else(
    existe,
    as.numeric(
      file.info(caminho)$size
    ),
    NA_real_
  ),
  data_modificacao = if_else(
    existe,
    format(
      file.info(caminho)$mtime,
      "%Y-%m-%d %H:%M:%S"
    ),
    NA_character_
  ),
  md5 = if_else(
    existe,
    unname(
      tools::md5sum(caminho)
    ),
    NA_character_
  )
)

write_csv(
  manifesto_produtos,
  file.path(
    pasta_execucao,
    "13_manifesto_produtos_modulo_17.csv"
  ),
  na = ""
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    pasta_execucao,
    "14_session_info.txt"
  )
)

resumo_execucao <- c(
  paste0("Execução: ", id_execucao),
  paste0("Escolas no índice: ", nrow(indice_carga_potencial_escola)),
  paste0("Componentes declarados: ", nrow(parametros_componentes)),
  paste0("Cenários de pesos: ", nrow(pesos_cenarios_dimensoes)),
  paste0(
    "Escolas com interpretação cautelosa: ",
    sum(
      indice_carga_potencial_escola$interpretacao_indice_cautelosa,
      na.rm = TRUE
    )
  ),
  paste0(
    "Índice principal — mínimo: ",
    round(
      min(
        indice_carga_potencial_escola$indice_carga_potencial,
        na.rm = TRUE
      ),
      2
    )
  ),
  paste0(
    "Índice principal — mediana: ",
    round(
      median(
        indice_carga_potencial_escola$indice_carga_potencial,
        na.rm = TRUE
      ),
      2
    )
  ),
  paste0(
    "Índice principal — máximo: ",
    round(
      max(
        indice_carga_potencial_escola$indice_carga_potencial,
        na.rm = TRUE
      ),
      2
    )
  ),
  paste0("Erros críticos: ", numero_erros_criticos),
  "",
  "Observações metodológicas:",
  "- O índice é descritivo e relativo ao universo analisado.",
  "- Pesos iguais entre dimensões constituem cenário provisório, não regra definitiva.",
  "- Dimensões e componentes devem ser examinados separadamente.",
  "- Resultados não representam efeito causal do programa ou qualidade das assessoras.",
  "- Variações de proficiência e composição racial não entram no índice.",
  "- Escolas com evidência educacional limitada permanecem identificadas por flags de cautela.",
  "- A redistribuição de carteiras exige validação qualitativa da gestão e das assessoras."
)

writeLines(
  resumo_execucao,
  file.path(
    pasta_execucao,
    "15_resumo_execucao.txt"
  )
)

message(
  "Módulo 17 concluído com sucesso.\n",
  "Produtos principais gravados em: ",
  pasta_dados_finais,
  "\nDiagnósticos da execução gravados em: ",
  pasta_execucao
)
