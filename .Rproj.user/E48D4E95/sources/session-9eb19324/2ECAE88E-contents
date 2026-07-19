library(here)
library(tidyverse)
library(scales)

# -------------------------------------------------------------------
# 1. Objetivo
# -------------------------------------------------------------------
#
# Analisar descritivamente a relação entre características estruturais
# pré-programa (Censo Escolar 2024) e:
#   - resultados iniciais de 2025;
#   - participação em 2025 e 2026;
#   - mudanças observadas entre 2025 e 2026.
#
# As associações estimadas neste módulo NÃO têm interpretação causal.
# As variáveis contextuais são usadas para caracterização, estratificação
# e ajuste descritivo das diferenças de composição entre escolas.
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 2. Diretórios e arquivos
# -------------------------------------------------------------------

dir.create(
  here("resultados", "contexto"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("resultados", "graficos", "contexto"),
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

media_segura <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

mediana_segura <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  median(x)
}

quantil_seguro <- function(x, prob) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(
    quantile(
      x,
      probs = prob,
      names = FALSE,
      type = 7
    )
  )
}

correlacao_segura <- function(x, y) {
  validos <- complete.cases(x, y)

  if (sum(validos) < 3) {
    return(NA_real_)
  }

  if (
    sd(x[validos]) == 0 ||
      sd(y[validos]) == 0
  ) {
    return(NA_real_)
  }

  cor(
    x[validos],
    y[validos]
  )
}

extrair_coeficientes <- function(modelo, nome_modelo) {
  matriz <- summary(modelo)$coefficients

  tibble(
    modelo = nome_modelo,
    termo = rownames(matriz),
    estimativa = matriz[, "Estimate"],
    erro_padrao = matriz[, "Std. Error"],
    estatistica_t = matriz[, "t value"],
    valor_p = matriz[, "Pr(>|t|)"]
  )
}

# -------------------------------------------------------------------
# 4. Leitura e preparação
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

    painel_balanceado = col_logical(),
    painel_resultado_balanceado = col_logical(),

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

    avaliados_2025 = col_double(),
    avaliados_2026 = col_double(),
    peso_medio_avaliados = col_double(),

    matriculas_anos_iniciais = col_double(),
    turmas_anos_iniciais = col_double(),
    docentes_anos_iniciais = col_double(),
    alunos_por_turma_anos_iniciais = col_double(),
    alunos_por_docente_anos_iniciais = col_double(),
    pct_matriculas_anos_iniciais_integral = col_double(),
    pct_matriculas_educacao_especial = col_double(),
    pct_matriculas_transporte_publico = col_double(),
    pct_matriculas_preta_parda_indigena = col_double(),
    numero_etapas_amplas_ofertadas = col_double(),
    indice_infraestrutura_basica = col_double(),

    incluir_universo_municipal_direto_2024 = col_logical(),
    incluir_universo_municipal_ampliado_2024 = col_logical(),
    incluir_rede_municipal_operacional_2025 = col_logical(),

    amostra_principal_descritiva = col_logical(),
    amostra_municipal_direta_2024 = col_logical(),
    amostra_municipal_ampliada_2024 = col_logical(),
    amostra_rede_operacional_2025 = col_logical(),

    requer_revisao_tecnica = col_logical(),
    .default = col_guess()
  )
)

base <- dados |>
  filter(
    amostra_principal_descritiva,
    painel_resultado_balanceado
  ) |>
  mutate(
    assessora = case_when(
      is.na(assessora) |
        str_squish(assessora) == "" ~
        "Sem vinculação informada",
      TRUE ~ str_squish(assessora)
    ),

    serie = factor(
      paste0(ano_escolar, "º ano"),
      levels = paste0(1:5, "º ano")
    ),

    log_matriculas_anos_iniciais = case_when(
      !is.na(matriculas_anos_iniciais) &
        matriculas_anos_iniciais > 0 ~
        log(matriculas_anos_iniciais),
      TRUE ~ NA_real_
    ),

    quartil_infraestrutura = case_when(
      !is.na(indice_infraestrutura_basica) ~
        ntile(indice_infraestrutura_basica, 4),
      TRUE ~ NA_integer_
    ),

    quartil_infraestrutura = factor(
      quartil_infraestrutura,
      levels = 1:4,
      labels = c(
        "Q1 — menor infraestrutura",
        "Q2",
        "Q3",
        "Q4 — maior infraestrutura"
      )
    ),

    faixa_porte_contextual = case_when(
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

    faixa_porte_contextual = factor(
      faixa_porte_contextual,
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
# 5. Validação da base analítica
# -------------------------------------------------------------------

validacao_base <- base |>
  summarise(
    linhas_escola_serie = n(),
    escolas = n_distinct(id_escola),
    series = n_distinct(ano_escolar),

    proficiencia_inicial_disponivel = sum(
      !is.na(proficiencia_media_2025)
    ),

    delta_proficiencia_disponivel = sum(
      !is.na(delta_proficiencia)
    ),

    matriculas_contexto_disponiveis = sum(
      !is.na(matriculas_anos_iniciais)
    ),

    infraestrutura_disponivel = sum(
      !is.na(indice_infraestrutura_basica)
    ),

    alunos_turma_disponivel = sum(
      !is.na(alunos_por_turma_anos_iniciais)
    ),

    pct_integral_disponivel = sum(
      !is.na(pct_matriculas_anos_iniciais_integral)
    ),

    revisao_tecnica = sum(
      coalesce(requer_revisao_tecnica, FALSE)
    )
  )

# -------------------------------------------------------------------
# 6. Perfil inicial segundo contexto
# -------------------------------------------------------------------

resumir_faixa <- function(base_dados, variavel_grupo, nome_dimensao) {
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
      numero_escolas = n_distinct(id_escola),

      matriculas_mediana =
        mediana_segura(matriculas_anos_iniciais),

      participacao_2025_media =
        media_segura(taxa_participacao_2025),

      proficiencia_2025_media =
        media_segura(proficiencia_media_2025),

      adequado_2025_media =
        media_segura(pct_adequado_2025),

      defasagem_2025_media =
        media_segura(pct_defasagem_2025),

      delta_participacao_media =
        media_segura(delta_participacao),

      delta_proficiencia_media =
        media_segura(delta_proficiencia),

      delta_proficiencia_mediana =
        mediana_segura(delta_proficiencia),

      delta_adequado_media =
        media_segura(delta_pct_adequado),

      delta_defasagem_media =
        media_segura(delta_pct_defasagem),

      .groups = "drop"
    ) |>
    relocate(
      dimensao_contextual,
      categoria,
      ano_escolar
    )
}

perfil_por_contexto <- bind_rows(
  resumir_faixa(
    base,
    faixa_porte_contextual,
    "Porte da escola"
  ),

  resumir_faixa(
    base,
    quartil_infraestrutura,
    "Infraestrutura"
  ),

  resumir_faixa(
    base,
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
# 7. Correlações descritivas
# -------------------------------------------------------------------

variaveis_contexto <- c(
  "log_matriculas_anos_iniciais",
  "alunos_por_turma_anos_iniciais",
  "alunos_por_docente_anos_iniciais",
  "pct_matriculas_anos_iniciais_integral",
  "pct_matriculas_educacao_especial",
  "pct_matriculas_transporte_publico",
  "pct_matriculas_preta_parda_indigena",
  "numero_etapas_amplas_ofertadas",
  "indice_infraestrutura_basica"
)

variaveis_resultado <- c(
  "taxa_participacao_2025",
  "proficiencia_media_2025",
  "pct_adequado_2025",
  "delta_participacao",
  "delta_proficiencia",
  "delta_pct_adequado",
  "delta_pct_defasagem"
)

correlacoes_contexto <- crossing(
  variavel_contexto = variaveis_contexto,
  variavel_resultado = variaveis_resultado,
  ano_escolar = sort(unique(base$ano_escolar))
) |>
  mutate(
    dados = pmap(
      list(
        variavel_contexto,
        variavel_resultado,
        ano_escolar
      ),
      function(vc, vr, serie_atual) {
        bloco <- base |>
          filter(
            ano_escolar == serie_atual
          )

        tibble(
          numero_observacoes = sum(
            complete.cases(
              bloco[[vc]],
              bloco[[vr]]
            )
          ),
          correlacao = correlacao_segura(
            bloco[[vc]],
            bloco[[vr]]
          )
        )
      }
    )
  ) |>
  unnest(dados)

# -------------------------------------------------------------------
# 8. Modelos descritivos ajustados
# -------------------------------------------------------------------
#
# Modelo A: resultado inicial em 2025 segundo contexto pré-programa.
# Modelo B: mudança de proficiência segundo resultado inicial,
#           mudança de participação e contexto.
# Modelo C: mesma especificação restrita ao universo municipal ampliado.
#
# Os coeficientes são associações condicionais descritivas.
# -------------------------------------------------------------------

base_modelos <- base |>
  filter(
    !is.na(proficiencia_media_2025),
    !is.na(delta_proficiencia),
    !is.na(delta_participacao),
    !is.na(log_matriculas_anos_iniciais),
    !is.na(indice_infraestrutura_basica),
    !is.na(pct_matriculas_anos_iniciais_integral),
    peso_medio_avaliados > 0
  ) |>
  mutate(
    ano_escolar_fator = factor(ano_escolar)
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

coeficientes_modelos <- bind_rows(
  extrair_coeficientes(
    modelo_inicial,
    "Modelo A — proficiência inicial e contexto"
  ),
  extrair_coeficientes(
    modelo_delta_todas,
    "Modelo B — mudança de proficiência, todas as escolas"
  ),
  extrair_coeficientes(
    modelo_delta_ampliado,
    "Modelo C — mudança de proficiência, universo municipal ampliado"
  )
)

ajuste_modelos <- tibble(
  modelo = c(
    "Modelo A — proficiência inicial e contexto",
    "Modelo B — mudança de proficiência, todas as escolas",
    "Modelo C — mudança de proficiência, universo municipal ampliado"
  ),

  numero_observacoes = c(
    nobs(modelo_inicial),
    nobs(modelo_delta_todas),
    nobs(modelo_delta_ampliado)
  ),

  r_quadrado = c(
    summary(modelo_inicial)$r.squared,
    summary(modelo_delta_todas)$r.squared,
    summary(modelo_delta_ampliado)$r.squared
  ),

  r_quadrado_ajustado = c(
    summary(modelo_inicial)$adj.r.squared,
    summary(modelo_delta_todas)$adj.r.squared,
    summary(modelo_delta_ampliado)$adj.r.squared
  )
)

# -------------------------------------------------------------------
# 9. Perfil contextual por carteira
# -------------------------------------------------------------------

perfil_contexto_carteiras_resultados <- base |>
  filter(
    incluir_universo_municipal_ampliado_2024
  ) |>
  group_by(assessora) |>
  summarise(
    numero_escolas = n_distinct(id_escola),
    numero_escolas_series = n(),

    matriculas_mediana =
      mediana_segura(matriculas_anos_iniciais),

    infraestrutura_media =
      media_segura(indice_infraestrutura_basica),

    pct_integral_medio =
      media_segura(
        pct_matriculas_anos_iniciais_integral
      ),

    proficiencia_2025_media =
      media_segura(proficiencia_media_2025),

    delta_participacao_media =
      media_segura(delta_participacao),

    delta_proficiencia_media =
      media_segura(delta_proficiencia),

    delta_proficiencia_mediana =
      mediana_segura(delta_proficiencia),

    delta_adequado_media =
      media_segura(delta_pct_adequado),

    .groups = "drop"
  ) |>
  arrange(
    desc(delta_proficiencia_media)
  )

# -------------------------------------------------------------------
# 10. Gráficos
# -------------------------------------------------------------------

grafico_porte_delta <- base |>
  filter(
    !is.na(faixa_porte_contextual),
    faixa_porte_contextual != "Sem informação"
  ) |>
  ggplot(
    aes(
      x = faixa_porte_contextual,
      y = delta_proficiencia
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_boxplot(
    outlier.alpha = 0.4
  ) +
  facet_wrap(
    vars(serie),
    scales = "free_y"
  ) +
  labs(
    title = "Mudança de proficiência segundo o porte escolar",
    subtitle = "Contexto pré-programa medido pelo Censo Escolar de 2024",
    x = NULL,
    y = "Variação da proficiência entre 2025 e 2026",
    caption = paste(
      "Associação descritiva. As faixas de porte não representam",
      "tratamento nem intensidade de assessoramento."
    )
  ) +
  theme_minimal(
    base_size = 10
  ) +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    panel.grid.minor = element_blank()
  )

ggsave(
  here(
    "resultados",
    "graficos",
    "contexto",
    "delta_proficiencia_por_porte_2024.png"
  ),
  grafico_porte_delta,
  width = 12,
  height = 7,
  dpi = 300
)

grafico_infra_delta <- base |>
  filter(
    !is.na(indice_infraestrutura_basica),
    !is.na(delta_proficiencia)
  ) |>
  ggplot(
    aes(
      x = indice_infraestrutura_basica,
      y = delta_proficiencia,
      size = peso_medio_avaliados
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_point(
    alpha = 0.55
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(weight = peso_medio_avaliados),
    linewidth = 0.7
  ) +
  facet_wrap(
    vars(serie),
    scales = "free_y"
  ) +
  scale_size_continuous(
    name = "Média de avaliados"
  ) +
  labs(
    title = "Infraestrutura escolar e mudança da proficiência",
    subtitle = "Associação descritiva por escola-série",
    x = "Índice descritivo de infraestrutura em 2024",
    y = "Variação da proficiência entre 2025 e 2026",
    caption = paste(
      "O índice de infraestrutura não é indicador oficial do INEP.",
      "As linhas representam ajustes lineares exclusivamente descritivos."
    )
  ) +
  theme_minimal(
    base_size = 10
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here(
    "resultados",
    "graficos",
    "contexto",
    "infraestrutura_delta_proficiencia.png"
  ),
  grafico_infra_delta,
  width = 11,
  height = 7,
  dpi = 300
)

# -------------------------------------------------------------------
# 11. Exportação
# -------------------------------------------------------------------

write_csv(
  validacao_base,
  here(
    "documentacao",
    "contexto_inep_2024",
    "25_validacao_base_contexto_desempenho.csv"
  ),
  na = ""
)

write_csv(
  perfil_por_contexto,
  here(
    "resultados",
    "contexto",
    "resultados_por_estratos_contextuais_2024.csv"
  ),
  na = ""
)

write_csv(
  correlacoes_contexto,
  here(
    "resultados",
    "contexto",
    "correlacoes_contexto_resultados.csv"
  ),
  na = ""
)

write_csv(
  coeficientes_modelos,
  here(
    "resultados",
    "contexto",
    "modelos_contextuais_coeficientes.csv"
  ),
  na = ""
)

write_csv(
  ajuste_modelos,
  here(
    "resultados",
    "contexto",
    "modelos_contextuais_ajuste.csv"
  ),
  na = ""
)

write_csv(
  perfil_contexto_carteiras_resultados,
  here(
    "resultados",
    "contexto",
    "perfil_contexto_resultados_carteiras.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 12. Resumo no console
# -------------------------------------------------------------------

cat(
  "\nMódulo de contexto e desempenho concluído.\n"
)

cat(
  "\nValidação da base:\n"
)

print(
  validacao_base,
  width = Inf
)

cat(
  "\nAjuste dos modelos descritivos:\n"
)

print(
  ajuste_modelos,
  width = Inf
)

cat(
  "\nCoeficientes contextuais dos modelos:\n"
)

print(
  coeficientes_modelos |>
    filter(
      termo %in% c(
        "log_matriculas_anos_iniciais",
        "indice_infraestrutura_basica",
        "pct_matriculas_anos_iniciais_integral",
        "delta_participacao",
        "proficiencia_media_2025"
      )
    ),
  n = Inf,
  width = Inf
)

cat(
  "\nArquivos gerados:\n",
  "- documentacao/contexto_inep_2024/25_validacao_base_contexto_desempenho.csv\n",
  "- resultados/contexto/resultados_por_estratos_contextuais_2024.csv\n",
  "- resultados/contexto/correlacoes_contexto_resultados.csv\n",
  "- resultados/contexto/modelos_contextuais_coeficientes.csv\n",
  "- resultados/contexto/modelos_contextuais_ajuste.csv\n",
  "- resultados/contexto/perfil_contexto_resultados_carteiras.csv\n",
  "- resultados/graficos/contexto/delta_proficiencia_por_porte_2024.png\n",
  "- resultados/graficos/contexto/infraestrutura_delta_proficiencia.png\n"
)
