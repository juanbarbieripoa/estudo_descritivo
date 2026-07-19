library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Objetivo
# -------------------------------------------------------------------
#
# Corrigir e robustecer o módulo 13:
#
#   1. atribuir estratos contextuais no nível da escola, e não no nível
#      repetido escola-série;
#   2. garantir que cada escola permaneça no mesmo quartil em todas as séries;
#   3. recalcular tabelas por estrato;
#   4. estimar os mesmos modelos descritivos com erros-padrão agrupados
#      por escola;
#   5. produzir sensibilidades por universo administrativo.
#
# As análises continuam sendo observacionais e não causais.
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 2. Diretórios e arquivo
# -------------------------------------------------------------------

dir.create(
  here("resultados", "contexto"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("documentacao", "contexto_inep_2024"),
  recursive = TRUE,
  showWarnings = FALSE
)

arquivo_base <- here(
  "dados_processados",
  "base_analitica_escola_serie_contexto_2024.csv"
)

if (!file.exists(arquivo_base)) {
  stop(
    "Arquivo não encontrado: ",
    arquivo_base,
    "\nExecute primeiro R/12E_consolidar_contexto_e_integrar_resultados.R."
  )
}

# -------------------------------------------------------------------
# 3. Funções auxiliares
# -------------------------------------------------------------------

primeiro_nao_vazio <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & str_squish(x) != ""]

  if (length(x) == 0) {
    return(NA_character_)
  }

  x[[1]]
}

primeiro_numero <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  x[[1]]
}

media_segura <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  mean(x)
}

mediana_segura <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  median(x)
}

# -------------------------------------------------------------------
# 4. Variância agrupada por escola
# -------------------------------------------------------------------
#
# Estimador cluster-robust tipo CR0 com correção de graus de liberdade.
# A finalidade é corrigir a dependência entre séries da mesma escola.
# -------------------------------------------------------------------

vcov_cluster_escola <- function(modelo, cluster) {
  X <- model.matrix(modelo)
  residuos <- residuals(modelo)
  pesos <- weights(modelo)

  if (is.null(pesos)) {
    pesos <- rep(1, length(residuos))
  }

  cluster <- as.character(cluster)

  if (length(cluster) != nrow(X)) {
    stop(
      "O vetor de agrupamento não possui o mesmo comprimento ",
      "da amostra efetivamente usada no modelo."
    )
  }

  Xw <- X * sqrt(pesos)
  uw <- residuos * sqrt(pesos)

  bread <- solve(crossprod(Xw))

  grupos <- split(
    seq_along(cluster),
    cluster
  )

  meat <- matrix(
    0,
    nrow = ncol(Xw),
    ncol = ncol(Xw)
  )

  for (indices in grupos) {
    Xg <- Xw[indices, , drop = FALSE]
    ug <- uw[indices]

    score_g <- crossprod(
      Xg,
      ug
    )

    meat <- meat +
      score_g %*% t(score_g)
  }

  n <- nrow(Xw)
  k <- ncol(Xw)
  g <- length(grupos)

  correcao <- if (
    g > 1 &&
      n > k
  ) {
    (g / (g - 1)) *
      ((n - 1) / (n - k))
  } else {
    1
  }

  correcao *
    bread %*%
    meat %*%
    bread
}

extrair_cluster <- function(
    modelo,
    cluster,
    nome_modelo
) {
  V <- vcov_cluster_escola(
    modelo,
    cluster
  )

  beta <- coef(modelo)
  erro <- sqrt(diag(V))
  estatistica <- beta / erro

  graus_liberdade <- length(
    unique(cluster)
  ) - 1

  valor_p <- 2 * pt(
    abs(estatistica),
    df = graus_liberdade,
    lower.tail = FALSE
  )

  tibble(
    modelo = nome_modelo,
    termo = names(beta),
    estimativa = unname(beta),
    erro_padrao_cluster_escola = unname(erro),
    estatistica_t_cluster = unname(estatistica),
    graus_liberdade_cluster = graus_liberdade,
    valor_p_cluster = unname(valor_p)
  )
}

# -------------------------------------------------------------------
# 5. Leitura
# -------------------------------------------------------------------

dados <- read_csv(
  arquivo_base,
  show_col_types = FALSE,
  col_types = cols(
    id_escola = col_character(),
    codigo_inep = col_character(),
    nome_canonico = col_character(),
    assessora = col_character(),
    ano_escolar = col_integer(),
    componente = col_character(),

    painel_resultado_balanceado = col_logical(),
    amostra_principal_descritiva = col_logical(),
    incluir_universo_municipal_direto_2024 = col_logical(),
    incluir_universo_municipal_ampliado_2024 = col_logical(),

    avaliados_2025 = col_double(),
    avaliados_2026 = col_double(),
    peso_medio_avaliados = col_double(),

    taxa_participacao_2025 = col_double(),
    taxa_participacao_2026 = col_double(),
    delta_participacao = col_double(),

    proficiencia_media_2025 = col_double(),
    proficiencia_media_2026 = col_double(),
    delta_proficiencia = col_double(),

    pct_adequado_2025 = col_double(),
    pct_adequado_2026 = col_double(),
    delta_pct_adequado = col_double(),

    pct_defasagem_2025 = col_double(),
    pct_defasagem_2026 = col_double(),
    delta_pct_defasagem = col_double(),

    matriculas_anos_iniciais = col_double(),
    pct_matriculas_anos_iniciais_integral = col_double(),
    indice_infraestrutura_basica = col_double(),

    requer_revisao_tecnica = col_logical(),

    .default = col_guess()
  )
)

base <- dados |>
  filter(
    amostra_principal_descritiva,
    painel_resultado_balanceado
  )

# -------------------------------------------------------------------
# 6. Uma linha contextual por escola
# -------------------------------------------------------------------

contexto_escola <- base |>
  group_by(
    id_escola
  ) |>
  summarise(
    codigo_inep = primeiro_nao_vazio(
      codigo_inep
    ),

    nome_canonico = primeiro_nao_vazio(
      nome_canonico
    ),

    matriculas_anos_iniciais =
      primeiro_numero(
        matriculas_anos_iniciais
      ),

    indice_infraestrutura_basica =
      primeiro_numero(
        indice_infraestrutura_basica
      ),

    pct_matriculas_anos_iniciais_integral =
      primeiro_numero(
        pct_matriculas_anos_iniciais_integral
      ),

    incluir_universo_municipal_direto_2024 =
      first(
        incluir_universo_municipal_direto_2024
      ),

    incluir_universo_municipal_ampliado_2024 =
      first(
        incluir_universo_municipal_ampliado_2024
      ),

    requer_revisao_tecnica =
      first(
        requer_revisao_tecnica
      ),

    .groups = "drop"
  ) |>
  arrange(
    indice_infraestrutura_basica,
    id_escola
  ) |>
  mutate(
    percentil_infraestrutura = case_when(
      !is.na(indice_infraestrutura_basica) ~
        percent_rank(
          indice_infraestrutura_basica
        ),
      TRUE ~ NA_real_
    ),

    quartil_infraestrutura_escola = case_when(
      is.na(percentil_infraestrutura) ~
        "Sem informação",

      percentil_infraestrutura <= 0.25 ~
        "Q1 — menor infraestrutura",

      percentil_infraestrutura <= 0.50 ~
        "Q2",

      percentil_infraestrutura <= 0.75 ~
        "Q3",

      TRUE ~
        "Q4 — maior infraestrutura"
    ),

    quartil_infraestrutura_escola = factor(
      quartil_infraestrutura_escola,
      levels = c(
        "Q1 — menor infraestrutura",
        "Q2",
        "Q3",
        "Q4 — maior infraestrutura",
        "Sem informação"
      )
    ),

    faixa_porte_contextual_escola = case_when(
      is.na(matriculas_anos_iniciais) ~
        "Sem informação",

      matriculas_anos_iniciais < 150 ~
        "Até 149 matrículas",

      matriculas_anos_iniciais < 300 ~
        "150 a 299 matrículas",

      matriculas_anos_iniciais < 500 ~
        "300 a 499 matrículas",

      TRUE ~
        "500 matrículas ou mais"
    ),

    faixa_porte_contextual_escola = factor(
      faixa_porte_contextual_escola,
      levels = c(
        "Até 149 matrículas",
        "150 a 299 matrículas",
        "300 a 499 matrículas",
        "500 matrículas ou mais",
        "Sem informação"
      )
    )
  )

# -------------------------------------------------------------------
# 7. Validação da estabilidade dos estratos
# -------------------------------------------------------------------

base_corrigida <- base |>
  select(
    -any_of(
      c(
        "quartil_infraestrutura",
        "faixa_porte_contextual"
      )
    )
  ) |>
  left_join(
    contexto_escola |>
      select(
        id_escola,
        quartil_infraestrutura_escola,
        faixa_porte_contextual_escola
      ),
    by = "id_escola"
  ) |>
  mutate(
    log_matriculas_anos_iniciais = case_when(
      !is.na(matriculas_anos_iniciais) &
        matriculas_anos_iniciais > 0 ~
        log(matriculas_anos_iniciais),

      TRUE ~ NA_real_
    ),

    ano_escolar_fator =
      factor(ano_escolar)
  )

validacao_estratos <- base_corrigida |>
  group_by(id_escola) |>
  summarise(
    numero_quartis_infraestrutura =
      n_distinct(
        quartil_infraestrutura_escola,
        na.rm = TRUE
      ),

    numero_faixas_porte =
      n_distinct(
        faixa_porte_contextual_escola,
        na.rm = TRUE
      ),

    .groups = "drop"
  ) |>
  summarise(
    escolas = n(),
    escolas_em_mais_de_um_quartil =
      sum(
        numero_quartis_infraestrutura > 1
      ),
    escolas_em_mais_de_uma_faixa_porte =
      sum(
        numero_faixas_porte > 1
      )
  )

distribuicao_estratos_escola <- contexto_escola |>
  count(
    quartil_infraestrutura_escola,
    faixa_porte_contextual_escola,
    name = "numero_escolas"
  ) |>
  arrange(
    quartil_infraestrutura_escola,
    faixa_porte_contextual_escola
  )

# -------------------------------------------------------------------
# 8. Resultados por estratos corrigidos
# -------------------------------------------------------------------

resumir_estrato <- function(
    base_dados,
    variavel_grupo,
    nome_dimensao
) {
  base_dados |>
    filter(
      !is.na({{ variavel_grupo }})
    ) |>
    group_by(
      ano_escolar,
      categoria = {{ variavel_grupo }}
    ) |>
    summarise(
      dimensao_contextual = nome_dimensao,

      numero_escolas =
        n_distinct(id_escola),

      participacao_2025_media =
        media_segura(
          taxa_participacao_2025
        ),

      proficiencia_2025_media =
        media_segura(
          proficiencia_media_2025
        ),

      adequado_2025_media =
        media_segura(
          pct_adequado_2025
        ),

      defasagem_2025_media =
        media_segura(
          pct_defasagem_2025
        ),

      delta_participacao_media =
        media_segura(
          delta_participacao
        ),

      delta_proficiencia_media =
        media_segura(
          delta_proficiencia
        ),

      delta_proficiencia_mediana =
        mediana_segura(
          delta_proficiencia
        ),

      delta_adequado_media =
        media_segura(
          delta_pct_adequado
        ),

      delta_defasagem_media =
        media_segura(
          delta_pct_defasagem
        ),

      .groups = "drop"
    ) |>
    relocate(
      dimensao_contextual,
      categoria,
      ano_escolar
    )
}

resultados_estratos_corrigidos <- bind_rows(
  resumir_estrato(
    base_corrigida,
    faixa_porte_contextual_escola,
    "Porte da escola"
  ),

  resumir_estrato(
    base_corrigida,
    quartil_infraestrutura_escola,
    "Infraestrutura"
  ),

  resumir_estrato(
    base_corrigida,
    factor(
      incluir_universo_municipal_direto_2024,
      levels = c(FALSE, TRUE),
      labels = c(
        "Fora da rede municipal direta em 2024",
        "Rede municipal direta em 2024"
      )
    ),
    "Situação administrativa em 2024"
  )
) |>
  arrange(
    dimensao_contextual,
    ano_escolar,
    categoria
  )

# -------------------------------------------------------------------
# 9. Modelos com erros-padrão agrupados por escola
# -------------------------------------------------------------------

base_modelos <- base_corrigida |>
  filter(
    !is.na(proficiencia_media_2025),
    !is.na(delta_proficiencia),
    !is.na(delta_participacao),
    !is.na(log_matriculas_anos_iniciais),
    !is.na(indice_infraestrutura_basica),
    !is.na(pct_matriculas_anos_iniciais_integral),
    peso_medio_avaliados > 0
  )

base_modelos <- base_modelos |>
  mutate(
    linha_modelo = row_number()
  )

modelo_inicial <- lm(
  proficiencia_media_2025 ~
    log_matriculas_anos_iniciais +
    indice_infraestrutura_basica +
    pct_matriculas_anos_iniciais_integral +
    ano_escolar_fator,
  data = base_modelos,
  weights = avaliados_2025
)

modelo_delta_todas <- lm(
  delta_proficiencia ~
    proficiencia_media_2025 +
    delta_participacao +
    log_matriculas_anos_iniciais +
    indice_infraestrutura_basica +
    pct_matriculas_anos_iniciais_integral +
    ano_escolar_fator,
  data = base_modelos,
  weights = peso_medio_avaliados
)

base_modelo_ampliado <- base_modelos |>
  filter(
    incluir_universo_municipal_ampliado_2024
  )

modelo_delta_ampliado <- lm(
  delta_proficiencia ~
    proficiencia_media_2025 +
    delta_participacao +
    log_matriculas_anos_iniciais +
    indice_infraestrutura_basica +
    pct_matriculas_anos_iniciais_integral +
    ano_escolar_fator,
  data = base_modelo_ampliado,
  weights = peso_medio_avaliados
)

coeficientes_cluster <- bind_rows(
  extrair_cluster(
    modelo_inicial,
    base_modelos$id_escola[
      as.integer(
        rownames(
          model.frame(modelo_inicial)
        )
      )
    ],
    "Modelo A — proficiência inicial e contexto"
  ),

  extrair_cluster(
    modelo_delta_todas,
    base_modelos$id_escola[
      as.integer(
        rownames(
          model.frame(modelo_delta_todas)
        )
      )
    ],
    "Modelo B — mudança de proficiência, todas as escolas"
  ),

  extrair_cluster(
    modelo_delta_ampliado,
    base_modelo_ampliado$id_escola[
      as.integer(
        rownames(
          model.frame(modelo_delta_ampliado)
        )
      )
    ],
    "Modelo C — mudança de proficiência, universo municipal ampliado"
  )
)

# -------------------------------------------------------------------
# 10. Sensibilidade sem o caso técnico
# -------------------------------------------------------------------

base_sem_revisao <- base_modelos |>
  filter(
    !coalesce(
      requer_revisao_tecnica,
      FALSE
    )
  )

modelo_delta_sem_revisao <- lm(
  delta_proficiencia ~
    proficiencia_media_2025 +
    delta_participacao +
    log_matriculas_anos_iniciais +
    indice_infraestrutura_basica +
    pct_matriculas_anos_iniciais_integral +
    ano_escolar_fator,
  data = base_sem_revisao,
  weights = peso_medio_avaliados
)

coeficientes_sensibilidade <- extrair_cluster(
  modelo_delta_sem_revisao,
  base_sem_revisao$id_escola[
    as.integer(
      rownames(
        model.frame(modelo_delta_sem_revisao)
      )
    )
  ],
  "Modelo D — mudança de proficiência, sem revisão técnica"
)

# -------------------------------------------------------------------
# 11. Exportação
# -------------------------------------------------------------------

write_csv(
  base_corrigida,
  here(
    "dados_processados",
    "base_contexto_desempenho_estratos_corrigidos.csv"
  ),
  na = ""
)

write_csv(
  validacao_estratos,
  here(
    "documentacao",
    "contexto_inep_2024",
    "26_validacao_estratos_contextuais.csv"
  ),
  na = ""
)

write_csv(
  distribuicao_estratos_escola,
  here(
    "documentacao",
    "contexto_inep_2024",
    "27_distribuicao_estratos_contextuais_escolas.csv"
  ),
  na = ""
)

write_csv(
  resultados_estratos_corrigidos,
  here(
    "resultados",
    "contexto",
    "resultados_por_estratos_contextuais_2024_corrigido.csv"
  ),
  na = ""
)

write_csv(
  coeficientes_cluster,
  here(
    "resultados",
    "contexto",
    "modelos_contextuais_coeficientes_cluster_escola.csv"
  ),
  na = ""
)

write_csv(
  coeficientes_sensibilidade,
  here(
    "resultados",
    "contexto",
    "modelo_contextual_sensibilidade_sem_revisao_tecnica.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 12. Resumo no console
# -------------------------------------------------------------------

cat(
  "\nCorreção e robustez do módulo contextual concluídas.\n"
)

cat(
  "\nValidação da estabilidade dos estratos:\n"
)

print(
  validacao_estratos,
  width = Inf
)

cat(
  "\nDistribuição dos estratos no nível da escola:\n"
)

print(
  distribuicao_estratos_escola,
  n = Inf,
  width = Inf
)

cat(
  "\nCoeficientes com erros-padrão agrupados por escola:\n"
)

print(
  coeficientes_cluster |>
    filter(
      termo %in% c(
        "proficiencia_media_2025",
        "delta_participacao",
        "log_matriculas_anos_iniciais",
        "indice_infraestrutura_basica",
        "pct_matriculas_anos_iniciais_integral"
      )
    ),
  n = Inf,
  width = Inf
)

cat(
  "\nArquivos gerados:\n",
  "- dados_processados/base_contexto_desempenho_estratos_corrigidos.csv\n",
  "- documentacao/contexto_inep_2024/26_validacao_estratos_contextuais.csv\n",
  "- documentacao/contexto_inep_2024/27_distribuicao_estratos_contextuais_escolas.csv\n",
  "- resultados/contexto/resultados_por_estratos_contextuais_2024_corrigido.csv\n",
  "- resultados/contexto/modelos_contextuais_coeficientes_cluster_escola.csv\n",
  "- resultados/contexto/modelo_contextual_sensibilidade_sem_revisao_tecnica.csv\n"
)
