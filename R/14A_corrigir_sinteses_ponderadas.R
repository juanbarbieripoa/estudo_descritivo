library(here)
library(tidyverse)
library(scales)

# -------------------------------------------------------------------
# 14A. Correção das sínteses ponderadas e dos gráficos
# -------------------------------------------------------------------
#
# Corrige a sobrescrita de colunas dentro de summarise() no módulo 14.
# Os numeradores e denominadores são calculados explicitamente antes das
# médias ponderadas, evitando que totais escalares substituam os vetores
# originais.
# -------------------------------------------------------------------

arquivo_base <- here(
  "dados_processados",
  "base_contexto_desempenho_estratos_corrigidos.csv"
)

if (!file.exists(arquivo_base)) {
  stop(
    "Arquivo não encontrado: ",
    arquivo_base
  )
}

dir.create(
  here("resultados", "sintese"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("resultados", "graficos", "sintese"),
  recursive = TRUE,
  showWarnings = FALSE
)

base <- read_csv(
  arquivo_base,
  show_col_types = FALSE,
  col_types = cols(
    id_escola = col_character(),
    ano_escolar = col_integer(),
    previstos_2025 = col_double(),
    previstos_2026 = col_double(),
    avaliados_2025 = col_double(),
    avaliados_2026 = col_double(),
    proficiencia_media_2025 = col_double(),
    proficiencia_media_2026 = col_double(),
    pct_adequado_2025 = col_double(),
    pct_adequado_2026 = col_double(),
    pct_defasagem_2025 = col_double(),
    pct_defasagem_2026 = col_double(),
    delta_proficiencia = col_double(),
    .default = col_guess()
  )
)

# -------------------------------------------------------------------
# Função de agregação
# -------------------------------------------------------------------

agregar_resultados <- function(dados) {
  dados |>
    summarise(
      escolas = n_distinct(id_escola),

      previstos_total_2025 = sum(
        .data$previstos_2025,
        na.rm = TRUE
      ),

      previstos_total_2026 = sum(
        .data$previstos_2026,
        na.rm = TRUE
      ),

      avaliados_total_2025 = sum(
        .data$avaliados_2025,
        na.rm = TRUE
      ),

      avaliados_total_2026 = sum(
        .data$avaliados_2026,
        na.rm = TRUE
      ),

      soma_ponderada_proficiencia_2025 = sum(
        .data$proficiencia_media_2025 *
          .data$avaliados_2025,
        na.rm = TRUE
      ),

      soma_ponderada_proficiencia_2026 = sum(
        .data$proficiencia_media_2026 *
          .data$avaliados_2026,
        na.rm = TRUE
      ),

      soma_ponderada_adequado_2025 = sum(
        .data$pct_adequado_2025 *
          .data$avaliados_2025,
        na.rm = TRUE
      ),

      soma_ponderada_adequado_2026 = sum(
        .data$pct_adequado_2026 *
          .data$avaliados_2026,
        na.rm = TRUE
      ),

      soma_ponderada_defasagem_2025 = sum(
        .data$pct_defasagem_2025 *
          .data$avaliados_2025,
        na.rm = TRUE
      ),

      soma_ponderada_defasagem_2026 = sum(
        .data$pct_defasagem_2026 *
          .data$avaliados_2026,
        na.rm = TRUE
      ),

      delta_proficiencia_media_escolas = mean(
        .data$delta_proficiencia,
        na.rm = TRUE
      ),

      delta_proficiencia_mediana_escolas = median(
        .data$delta_proficiencia,
        na.rm = TRUE
      ),

      .groups = "drop"
    ) |>
    mutate(
      participacao_2025 = if_else(
        previstos_total_2025 > 0,
        avaliados_total_2025 /
          previstos_total_2025,
        NA_real_
      ),

      participacao_2026 = if_else(
        previstos_total_2026 > 0,
        avaliados_total_2026 /
          previstos_total_2026,
        NA_real_
      ),

      delta_participacao =
        participacao_2026 -
        participacao_2025,

      proficiencia_2025_ponderada = if_else(
        avaliados_total_2025 > 0,
        soma_ponderada_proficiencia_2025 /
          avaliados_total_2025,
        NA_real_
      ),

      proficiencia_2026_ponderada = if_else(
        avaliados_total_2026 > 0,
        soma_ponderada_proficiencia_2026 /
          avaliados_total_2026,
        NA_real_
      ),

      delta_proficiencia_ponderada =
        proficiencia_2026_ponderada -
        proficiencia_2025_ponderada,

      adequado_2025_ponderado = if_else(
        avaliados_total_2025 > 0,
        soma_ponderada_adequado_2025 /
          avaliados_total_2025,
        NA_real_
      ),

      adequado_2026_ponderado = if_else(
        avaliados_total_2026 > 0,
        soma_ponderada_adequado_2026 /
          avaliados_total_2026,
        NA_real_
      ),

      delta_adequado_ponderado =
        adequado_2026_ponderado -
        adequado_2025_ponderado,

      defasagem_2025_ponderada = if_else(
        avaliados_total_2025 > 0,
        soma_ponderada_defasagem_2025 /
          avaliados_total_2025,
        NA_real_
      ),

      defasagem_2026_ponderada = if_else(
        avaliados_total_2026 > 0,
        soma_ponderada_defasagem_2026 /
          avaliados_total_2026,
        NA_real_
      ),

      delta_defasagem_ponderada =
        defasagem_2026_ponderada -
        defasagem_2025_ponderada
    ) |>
    select(
      -starts_with("soma_ponderada_")
    )
}

# -------------------------------------------------------------------
# Síntese geral e por série
# -------------------------------------------------------------------

sintese_geral_corrigida <- agregar_resultados(base) |>
  mutate(
    observacoes_escola_serie = nrow(base),
    series = n_distinct(base$ano_escolar)
  ) |>
  relocate(
    escolas,
    observacoes_escola_serie,
    series
  )

sintese_por_serie_corrigida <- base |>
  group_by(ano_escolar) |>
  group_modify(
    ~ agregar_resultados(.x)
  ) |>
  ungroup() |>
  arrange(ano_escolar)

# -------------------------------------------------------------------
# Validação
# -------------------------------------------------------------------

validacao <- sintese_por_serie_corrigida |>
  transmute(
    ano_escolar,
    proficiencia_2025_ponderada,
    proficiencia_2026_ponderada,
    delta_proficiencia_ponderada,
    valor_valido =
      !is.na(delta_proficiencia_ponderada)
  )

if (any(!validacao$valor_valido)) {
  write_csv(
    validacao,
    here(
      "resultados",
      "sintese",
      "02A_validacao_sintese_ponderada.csv"
    ),
    na = ""
  )

  stop(
    "Ainda há séries sem variação ponderada válida. ",
    "Consulte 02A_validacao_sintese_ponderada.csv."
  )
}

# -------------------------------------------------------------------
# Exportação das tabelas corrigidas
# -------------------------------------------------------------------

write_csv(
  sintese_geral_corrigida,
  here(
    "resultados",
    "sintese",
    "01_sintese_geral_painel_corrigida.csv"
  ),
  na = ""
)

write_csv(
  sintese_por_serie_corrigida,
  here(
    "resultados",
    "sintese",
    "02_sintese_por_serie_corrigida.csv"
  ),
  na = ""
)

write_csv(
  validacao,
  here(
    "resultados",
    "sintese",
    "02A_validacao_sintese_ponderada.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# Gráfico corrigido da proficiência
# -------------------------------------------------------------------

grafico_series <- sintese_por_serie_corrigida |>
  ggplot(
    aes(
      x = factor(
        ano_escolar,
        levels = 1:5,
        labels = paste0(1:5, "º ano")
      ),
      y = delta_proficiencia_ponderada
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_col(
    width = 0.65
  ) +
  geom_text(
    aes(
      label = number(
        delta_proficiencia_ponderada,
        accuracy = 0.1,
        decimal.mark = ","
      )
    ),
    vjust = ifelse(
      sintese_por_serie_corrigida$
        delta_proficiencia_ponderada >= 0,
      -0.4,
      1.3
    ),
    size = 3.8
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0.12, 0.16)
    )
  ) +
  labs(
    title = "Variação ponderada da proficiência por ano escolar",
    subtitle = paste(
      "Comparação descritiva entre a 1ª avaliação",
      "formativa de 2025 e 2026"
    ),
    x = NULL,
    y = "Variação da proficiência",
    caption = paste(
      "Médias anuais ponderadas pelo número de estudantes avaliados.",
      "As diferenças não representam estimativas causais."
    )
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(
  here(
    "resultados",
    "graficos",
    "sintese",
    "variacao_proficiencia_por_serie_corrigida.png"
  ),
  grafico_series,
  width = 9,
  height = 5.5,
  dpi = 300
)

# -------------------------------------------------------------------
# Gráfico corrigido de participação
# -------------------------------------------------------------------

grafico_participacao <- sintese_por_serie_corrigida |>
  select(
    ano_escolar,
    participacao_2025,
    participacao_2026
  ) |>
  pivot_longer(
    cols = starts_with("participacao_"),
    names_to = "ano_avaliacao",
    values_to = "participacao"
  ) |>
  mutate(
    ano_avaliacao = recode(
      ano_avaliacao,
      participacao_2025 = "2025",
      participacao_2026 = "2026"
    )
  ) |>
  ggplot(
    aes(
      x = factor(
        ano_escolar,
        levels = 1:5,
        labels = paste0(1:5, "º ano")
      ),
      y = participacao,
      group = ano_avaliacao,
      linetype = ano_avaliacao
    )
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 2
  ) +
  scale_y_continuous(
    labels = percent_format(
      accuracy = 1
    )
  ) +
  labs(
    title = "Participação nas avaliações por ano escolar",
    subtitle = paste(
      "Participação agregada ponderada pelo número",
      "previsto de estudantes"
    ),
    x = NULL,
    y = "Taxa de participação",
    linetype = "Ano da avaliação"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here(
    "resultados",
    "graficos",
    "sintese",
    "participacao_por_serie_2025_2026_corrigida.png"
  ),
  grafico_participacao,
  width = 9,
  height = 5.5,
  dpi = 300
)

cat(
  "\nCorreção concluída.\n\n"
)

print(
  validacao,
  n = Inf,
  width = Inf
)

cat(
  "\nArquivos gerados:\n",
  "- resultados/sintese/01_sintese_geral_painel_corrigida.csv\n",
  "- resultados/sintese/02_sintese_por_serie_corrigida.csv\n",
  "- resultados/sintese/02A_validacao_sintese_ponderada.csv\n",
  "- resultados/graficos/sintese/variacao_proficiencia_por_serie_corrigida.png\n",
  "- resultados/graficos/sintese/participacao_por_serie_2025_2026_corrigida.png\n"
)
