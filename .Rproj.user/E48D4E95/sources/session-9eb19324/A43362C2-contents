library(here)
library(tidyverse)
library(scales)

# -------------------------------------------------------------------
# 1. Diretórios e arquivos
# -------------------------------------------------------------------

dir.create(
  here("resultados", "assessoramento"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("resultados", "graficos", "assessoramento"),
  recursive = TRUE,
  showWarnings = FALSE
)

arquivo_variacao <- here(
  "dados_processados",
  "variacao_escola_serie_com_qualidade.csv"
)

arquivo_painel <- here(
  "dados_processados",
  "painel_escola_serie_ano.csv"
)

arquivos_necessarios <- c(
  arquivo_variacao,
  arquivo_painel
)

arquivos_ausentes <- arquivos_necessarios[
  !file.exists(arquivos_necessarios)
]

if (length(arquivos_ausentes) > 0) {
  stop(
    "Arquivos não encontrados:\n",
    paste(arquivos_ausentes, collapse = "\n")
  )
}

# -------------------------------------------------------------------
# 2. Funções auxiliares
# -------------------------------------------------------------------

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

quantil_seguro <- function(x, prob) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  as.numeric(
    quantile(
      x,
      probs = prob,
      names = FALSE,
      type = 7
    )
  )
}

media_ponderada_segura <- function(x, peso) {
  
  validos <- !is.na(x) &
    !is.na(peso) &
    peso > 0
  
  if (!any(validos)) {
    return(NA_real_)
  }
  
  weighted.mean(
    x[validos],
    peso[validos]
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

# -------------------------------------------------------------------
# 3. Leitura
# -------------------------------------------------------------------

variacao <- read_csv(
  arquivo_variacao,
  show_col_types = FALSE,
  col_types = cols(
    id_escola = col_character(),
    nome_canonico = col_character(),
    assessora = col_character(),
    ano_escolar = col_integer(),
    componente = col_character(),
    
    painel_balanceado = col_logical(),
    painel_resultado_balanceado = col_logical(),
    
    previstos_2025 = col_double(),
    previstos_2026 = col_double(),
    avaliados_2025 = col_double(),
    avaliados_2026 = col_double(),
    
    taxa_participacao_2025 = col_double(),
    taxa_participacao_2026 = col_double(),
    delta_participacao = col_double(),
    
    proficiencia_media_2025 = col_double(),
    proficiencia_media_2026 = col_double(),
    delta_proficiencia = col_double(),
    
    pct_defasagem_2025 = col_double(),
    pct_defasagem_2026 = col_double(),
    delta_pct_defasagem = col_double(),
    
    pct_adequado_2025 = col_double(),
    pct_adequado_2026 = col_double(),
    delta_pct_adequado = col_double(),
    
    variacao_relativa_previstos = col_double(),
    
    .default = col_character()
  )
)

painel <- read_csv(
  arquivo_painel,
  show_col_types = FALSE,
  col_types = cols(
    ano = col_integer(),
    id_escola = col_character(),
    nome_canonico = col_character(),
    assessora = col_character(),
    ano_escolar = col_integer(),
    componente = col_character(),
    previstos = col_double(),
    avaliados = col_double(),
    taxa_participacao = col_double(),
    proficiencia_media = col_double(),
    pct_defasagem = col_double(),
    pct_adequado = col_double(),
    .default = col_character()
  )
)

# -------------------------------------------------------------------
# 4. Tratamento da assessora
# -------------------------------------------------------------------

variacao <- variacao |>
  mutate(
    assessora = case_when(
      is.na(assessora) |
        str_squish(assessora) == "" ~
        "Sem vinculação informada",
      
      TRUE ~
        str_squish(assessora)
    ),
    
    peso_medio_avaliados =
      (avaliados_2025 + avaliados_2026) / 2,
    
    participacao_80_ambos =
      taxa_participacao_2025 >= 80 &
      taxa_participacao_2026 >= 80,
    
    participacao_estavel_5pp =
      abs(delta_participacao) <= 5,
    
    previstos_estaveis_20pct =
      !is.na(variacao_relativa_previstos) &
      abs(variacao_relativa_previstos) <= 20
  )

painel <- painel |>
  mutate(
    assessora = case_when(
      is.na(assessora) |
        str_squish(assessora) == "" ~
        "Sem vinculação informada",
      
      TRUE ~
        str_squish(assessora)
    )
  )

# -------------------------------------------------------------------
# 5. Composição das carteiras
# -------------------------------------------------------------------

composicao_carteiras <- painel |>
  group_by(
    assessora,
    ano
  ) |>
  summarise(
    numero_escolas = n_distinct(id_escola),
    
    numero_escolas_series = n(),
    
    numero_series_atendidas =
      n_distinct(ano_escolar),
    
    previstos = sum(
      previstos,
      na.rm = TRUE
    ),
    
    avaliados = sum(
      avaliados,
      na.rm = TRUE
    ),
    
    taxa_participacao = if_else(
      previstos > 0,
      100 * avaliados / previstos,
      NA_real_
    ),
    
    .groups = "drop"
  ) |>
  arrange(
    assessora,
    ano
  )

# -------------------------------------------------------------------
# 6. Resultados agregados por assessora e série
# -------------------------------------------------------------------

resultado_assessora_serie <- variacao |>
  filter(
    painel_resultado_balanceado,
    assessora != "Sem vinculação informada"
  ) |>
  group_by(
    assessora,
    ano_escolar,
    componente
  ) |>
  summarise(
    numero_escolas = n_distinct(id_escola),
    
    avaliados_2025 = sum(
      avaliados_2025,
      na.rm = TRUE
    ),
    
    avaliados_2026 = sum(
      avaliados_2026,
      na.rm = TRUE
    ),
    
    participacao_2025 =
      media_ponderada_segura(
        taxa_participacao_2025,
        previstos_2025
      ),
    
    participacao_2026 =
      media_ponderada_segura(
        taxa_participacao_2026,
        previstos_2026
      ),
    
    delta_participacao =
      participacao_2026 -
      participacao_2025,
    
    proficiencia_2025 =
      media_ponderada_segura(
        proficiencia_media_2025,
        avaliados_2025
      ),
    
    proficiencia_2026 =
      media_ponderada_segura(
        proficiencia_media_2026,
        avaliados_2026
      ),
    
    delta_proficiencia_ponderada =
      media_ponderada_segura(
        delta_proficiencia,
        peso_medio_avaliados
      ),
    
    delta_proficiencia_mediana =
      mediana_segura(
        delta_proficiencia
      ),
    
    delta_proficiencia_p25 =
      quantil_seguro(
        delta_proficiencia,
        0.25
      ),
    
    delta_proficiencia_p75 =
      quantil_seguro(
        delta_proficiencia,
        0.75
      ),
    
    adequado_2025 =
      media_ponderada_segura(
        pct_adequado_2025,
        avaliados_2025
      ),
    
    adequado_2026 =
      media_ponderada_segura(
        pct_adequado_2026,
        avaliados_2026
      ),
    
    delta_adequado_ponderada =
      media_ponderada_segura(
        delta_pct_adequado,
        peso_medio_avaliados
      ),
    
    defasagem_2025 =
      media_ponderada_segura(
        pct_defasagem_2025,
        avaliados_2025
      ),
    
    defasagem_2026 =
      media_ponderada_segura(
        pct_defasagem_2026,
        avaliados_2026
      ),
    
    delta_defasagem_ponderada =
      media_ponderada_segura(
        delta_pct_defasagem,
        peso_medio_avaliados
      ),
    
    escolas_aumento_proficiencia = sum(
      delta_proficiencia > 2,
      na.rm = TRUE
    ),
    
    escolas_reducao_proficiencia = sum(
      delta_proficiencia < -2,
      na.rm = TRUE
    ),
    
    escolas_estabilidade_proficiencia = sum(
      delta_proficiencia >= -2 &
        delta_proficiencia <= 2,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    pct_escolas_aumento_proficiencia =
      100 *
      escolas_aumento_proficiencia /
      numero_escolas,
    
    pct_escolas_reducao_proficiencia =
      100 *
      escolas_reducao_proficiencia /
      numero_escolas
  ) |>
  arrange(
    ano_escolar,
    assessora
  )

# -------------------------------------------------------------------
# 7. Resultado geral da carteira
# -------------------------------------------------------------------
#
# Não se agrega diretamente a proficiência bruta de séries diferentes.
# Primeiro calculamos mudanças dentro da série e depois resumimos
# as variações da carteira.
# -------------------------------------------------------------------

resultado_geral_carteira <- variacao |>
  filter(
    painel_resultado_balanceado,
    assessora != "Sem vinculação informada"
  ) |>
  group_by(assessora) |>
  summarise(
    numero_escolas = n_distinct(id_escola),
    
    numero_escolas_series = n(),
    
    numero_series = n_distinct(
      ano_escolar
    ),
    
    avaliados_2025 = sum(
      avaliados_2025,
      na.rm = TRUE
    ),
    
    avaliados_2026 = sum(
      avaliados_2026,
      na.rm = TRUE
    ),
    
    participacao_2025 =
      media_ponderada_segura(
        taxa_participacao_2025,
        previstos_2025
      ),
    
    participacao_2026 =
      media_ponderada_segura(
        taxa_participacao_2026,
        previstos_2026
      ),
    
    delta_participacao =
      participacao_2026 -
      participacao_2025,
    
    delta_proficiencia_media =
      media_ponderada_segura(
        delta_proficiencia,
        peso_medio_avaliados
      ),
    
    delta_proficiencia_mediana =
      mediana_segura(
        delta_proficiencia
      ),
    
    delta_proficiencia_p25 =
      quantil_seguro(
        delta_proficiencia,
        0.25
      ),
    
    delta_proficiencia_p75 =
      quantil_seguro(
        delta_proficiencia,
        0.75
      ),
    
    delta_adequado_media =
      media_ponderada_segura(
        delta_pct_adequado,
        peso_medio_avaliados
      ),
    
    delta_defasagem_media =
      media_ponderada_segura(
        delta_pct_defasagem,
        peso_medio_avaliados
      ),
    
    escolas_series_aumento = sum(
      delta_proficiencia > 2,
      na.rm = TRUE
    ),
    
    escolas_series_reducao = sum(
      delta_proficiencia < -2,
      na.rm = TRUE
    ),
    
    escolas_series_estabilidade = sum(
      delta_proficiencia >= -2 &
        delta_proficiencia <= 2,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    pct_escolas_series_aumento =
      100 *
      escolas_series_aumento /
      numero_escolas_series,
    
    pct_escolas_series_reducao =
      100 *
      escolas_series_reducao /
      numero_escolas_series
  ) |>
  arrange(
    desc(delta_proficiencia_media)
  )

# -------------------------------------------------------------------
# 8. Sensibilidade por carteira
# -------------------------------------------------------------------

resumir_carteira_amostra <- function(base, nome_amostra) {
  
  base |>
    filter(
      painel_resultado_balanceado,
      assessora != "Sem vinculação informada"
    ) |>
    group_by(assessora) |>
    summarise(
      amostra = nome_amostra,
      
      numero_escolas = n_distinct(id_escola),
      
      numero_escolas_series = n(),
      
      delta_participacao_media =
        media_ponderada_segura(
          delta_participacao,
          peso_medio_avaliados
        ),
      
      delta_proficiencia_media =
        media_ponderada_segura(
          delta_proficiencia,
          peso_medio_avaliados
        ),
      
      delta_proficiencia_mediana =
        mediana_segura(
          delta_proficiencia
        ),
      
      delta_adequado_media =
        media_ponderada_segura(
          delta_pct_adequado,
          peso_medio_avaliados
        ),
      
      .groups = "drop"
    )
}

sensibilidade_carteiras <- bind_rows(
  
  resumir_carteira_amostra(
    variacao,
    "Todas as escolas-séries comparáveis"
  ),
  
  resumir_carteira_amostra(
    variacao |>
      filter(participacao_80_ambos),
    "Participação mínima de 80% nos dois anos"
  ),
  
  resumir_carteira_amostra(
    variacao |>
      filter(participacao_estavel_5pp),
    "Participação variou no máximo 5 p.p."
  ),
  
  resumir_carteira_amostra(
    variacao |>
      filter(
        participacao_estavel_5pp,
        previstos_estaveis_20pct
      ),
    "Participação e previstos relativamente estáveis"
  )
) |>
  arrange(
    assessora,
    amostra
  )

# -------------------------------------------------------------------
# 9. Perfil inicial das carteiras
# -------------------------------------------------------------------

perfil_inicial_carteiras <- variacao |>
  filter(
    painel_resultado_balanceado,
    assessora != "Sem vinculação informada"
  ) |>
  group_by(assessora) |>
  summarise(
    numero_escolas = n_distinct(id_escola),
    
    participacao_inicial =
      media_ponderada_segura(
        taxa_participacao_2025,
        previstos_2025
      ),
    
    proficiencia_inicial_media =
      media_segura(
        proficiencia_media_2025
      ),
    
    proficiencia_inicial_mediana =
      mediana_segura(
        proficiencia_media_2025
      ),
    
    proficiencia_inicial_p25 =
      quantil_seguro(
        proficiencia_media_2025,
        0.25
      ),
    
    proficiencia_inicial_p75 =
      quantil_seguro(
        proficiencia_media_2025,
        0.75
      ),
    
    adequado_inicial =
      media_ponderada_segura(
        pct_adequado_2025,
        avaliados_2025
      ),
    
    defasagem_inicial =
      media_ponderada_segura(
        pct_defasagem_2025,
        avaliados_2025
      ),
    
    .groups = "drop"
  )

resultado_geral_carteira <- resultado_geral_carteira |>
  left_join(
    perfil_inicial_carteiras,
    by = c(
      "assessora",
      "numero_escolas"
    )
  )

# -------------------------------------------------------------------
# 10. Indicadores de cautela
# -------------------------------------------------------------------

resultado_geral_carteira <- resultado_geral_carteira |>
  mutate(
    alerta_tamanho_carteira = case_when(
      numero_escolas < 3 ~
        "Carteira com menos de 3 escolas",
      
      numero_escolas < 5 ~
        "Carteira pequena: interpretar com cautela",
      
      TRUE ~
        "Tamanho suficiente para descrição"
    ),
    
    classificacao_descritiva = case_when(
      delta_proficiencia_media > 2 &
        delta_participacao >= 5 ~
        "Maior participação e aumento de proficiência",
      
      delta_proficiencia_media > 2 &
        delta_participacao > -5 ~
        "Aumento de proficiência com participação estável",
      
      delta_proficiencia_media > 2 ~
        "Aumento de proficiência com queda de participação",
      
      delta_proficiencia_media < -2 &
        delta_participacao >= 5 ~
        "Queda de proficiência com ampliação da participação",
      
      delta_proficiencia_media < -2 &
        delta_participacao <= -5 ~
        "Queda de proficiência e participação",
      
      delta_proficiencia_media < -2 ~
        "Queda de proficiência com participação estável",
      
      TRUE ~
        "Mudança média relativamente estável"
    )
  )

# -------------------------------------------------------------------
# 11. Gráfico: participação por carteira
# -------------------------------------------------------------------

grafico_participacao_carteira <- resultado_geral_carteira |>
  arrange(delta_participacao) |>
  mutate(
    assessora = factor(
      assessora,
      levels = assessora
    )
  ) |>
  ggplot(
    aes(
      x = delta_participacao,
      y = assessora
    )
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_point(
    size = 2.5
  ) +
  labs(
    title = "Mudança da participação por carteira de assessoramento",
    subtitle = "Diferença entre 2026 e a linha de base de 2025",
    x = "Variação da participação (pontos percentuais)",
    y = NULL,
    caption = paste(
      "Agrupamento administrativo das escolas.",
      "Não representa dose de assessoramento."
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
    "assessoramento",
    "delta_participacao_por_carteira.png"
  ),
  grafico_participacao_carteira,
  width = 9,
  height = 6.5,
  dpi = 300
)

# -------------------------------------------------------------------
# 12. Gráfico: proficiência por carteira
# -------------------------------------------------------------------

grafico_proficiencia_carteira <- resultado_geral_carteira |>
  arrange(delta_proficiencia_media) |>
  mutate(
    assessora = factor(
      assessora,
      levels = assessora
    )
  ) |>
  ggplot(
    aes(
      x = delta_proficiencia_media,
      y = assessora
    )
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_errorbarh(
    aes(
      xmin = delta_proficiencia_p25,
      xmax = delta_proficiencia_p75
    ),
    height = 0.2
  ) +
  geom_point(
    size = 2.5
  ) +
  labs(
    title = "Mudança da proficiência por carteira",
    subtitle = paste(
      "Ponto: média ponderada;",
      "barra: intervalo interquartil das escolas-séries"
    ),
    x = "Variação da proficiência",
    y = NULL,
    caption = paste(
      "Comparação observacional antes–depois.",
      "Não constitui avaliação individual das assessoras."
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
    "assessoramento",
    "delta_proficiencia_por_carteira.png"
  ),
  grafico_proficiencia_carteira,
  width = 9,
  height = 6.5,
  dpi = 300
)

# -------------------------------------------------------------------
# 13. Gráfico: participação e proficiência
# -------------------------------------------------------------------

grafico_quadrantes_carteiras <- resultado_geral_carteira |>
  ggplot(
    aes(
      x = delta_participacao,
      y = delta_proficiencia_media,
      size = numero_escolas
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_point(
    alpha = 0.7
  ) +
  geom_text(
    aes(label = assessora),
    check_overlap = TRUE,
    nudge_y = 0.5,
    size = 3
  ) +
  scale_size_continuous(
    name = "Número de escolas"
  ) +
  labs(
    title = "Participação e proficiência por carteira",
    subtitle = "Mudanças observadas entre 2025 e 2026",
    x = "Variação da participação (p.p.)",
    y = "Variação média da proficiência",
    caption = paste(
      "As diferenças refletem resultados e composição das carteiras;",
      "não identificam efeito da assessora."
    )
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
    "assessoramento",
    "quadrantes_participacao_proficiencia_carteiras.png"
  ),
  grafico_quadrantes_carteiras,
  width = 10,
  height = 7,
  dpi = 300
)

# -------------------------------------------------------------------
# 14. Gráfico: heterogeneidade por série
# -------------------------------------------------------------------

grafico_carteira_serie <- resultado_assessora_serie |>
  mutate(
    serie = factor(
      paste0(ano_escolar, "º ano"),
      levels = paste0(1:5, "º ano")
    )
  ) |>
  ggplot(
    aes(
      x = serie,
      y = delta_proficiencia_ponderada,
      group = assessora
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_line(
    alpha = 0.6
  ) +
  geom_point(
    size = 1.8
  ) +
  facet_wrap(
    vars(assessora)
  ) +
  labs(
    title = "Mudança de proficiência por carteira e ano escolar",
    subtitle = "Heterogeneidade interna das carteiras",
    x = NULL,
    y = "Variação média ponderada da proficiência",
    caption = paste(
      "O número de escolas varia entre carteiras e séries.",
      "Resultados exclusivamente descritivos."
    )
  ) +
  theme_minimal(
    base_size = 10
  ) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(
  here(
    "resultados",
    "graficos",
    "assessoramento",
    "delta_proficiencia_carteira_por_serie.png"
  ),
  grafico_carteira_serie,
  width = 13,
  height = 9,
  dpi = 300
)

# -------------------------------------------------------------------
# 15. Exportação
# -------------------------------------------------------------------

write_csv(
  composicao_carteiras,
  here(
    "resultados",
    "assessoramento",
    "composicao_carteiras.csv"
  ),
  na = ""
)

write_csv(
  resultado_assessora_serie,
  here(
    "resultados",
    "assessoramento",
    "resultado_assessora_por_serie.csv"
  ),
  na = ""
)

write_csv(
  resultado_geral_carteira,
  here(
    "resultados",
    "assessoramento",
    "resultado_geral_carteiras.csv"
  ),
  na = ""
)

write_csv(
  sensibilidade_carteiras,
  here(
    "resultados",
    "assessoramento",
    "sensibilidade_resultados_carteiras.csv"
  ),
  na = ""
)

write_csv(
  perfil_inicial_carteiras,
  here(
    "resultados",
    "assessoramento",
    "perfil_inicial_carteiras.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 16. Resumo no console
# -------------------------------------------------------------------

cat("\nMódulo de análise por carteira concluído.\n")

cat("\nComposição das carteiras:\n")

print(
  composicao_carteiras |>
    select(
      assessora,
      ano,
      numero_escolas,
      previstos,
      avaliados,
      taxa_participacao
    ),
  n = Inf
)

cat("\nResultados gerais das carteiras:\n")

print(
  resultado_geral_carteira |>
    select(
      assessora,
      numero_escolas,
      participacao_2025,
      participacao_2026,
      delta_participacao,
      proficiencia_inicial_media,
      delta_proficiencia_media,
      delta_proficiencia_p25,
      delta_proficiencia_p75,
      delta_adequado_media,
      classificacao_descritiva,
      alerta_tamanho_carteira
    ),
  n = Inf
)

cat("\nArquivos gerados:\n")

cat(
  "- resultados/assessoramento/composicao_carteiras.csv\n",
  "- resultados/assessoramento/resultado_assessora_por_serie.csv\n",
  "- resultados/assessoramento/resultado_geral_carteiras.csv\n",
  "- resultados/assessoramento/sensibilidade_resultados_carteiras.csv\n",
  "- resultados/assessoramento/perfil_inicial_carteiras.csv\n",
  "- resultados/graficos/assessoramento/delta_participacao_por_carteira.png\n",
  "- resultados/graficos/assessoramento/delta_proficiencia_por_carteira.png\n",
  "- resultados/graficos/assessoramento/quadrantes_participacao_proficiencia_carteiras.png\n",
  "- resultados/graficos/assessoramento/delta_proficiencia_carteira_por_serie.png\n"
)