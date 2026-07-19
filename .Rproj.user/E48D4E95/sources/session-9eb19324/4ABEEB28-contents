library(here)
library(tidyverse)
library(scales)

# -------------------------------------------------------------------
# 1. Diretórios e arquivos
# -------------------------------------------------------------------

dir.create(
  here("resultados"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("resultados", "graficos"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("documentacao"),
  recursive = TRUE,
  showWarnings = FALSE
)

arquivo_rede <- here(
  "dados_processados",
  "painel_rede_serie_ano.csv"
)

arquivo_variacao_rede <- here(
  "dados_processados",
  "variacao_rede_serie_2025_2026.csv"
)

arquivo_variacao_escolas <- here(
  "dados_processados",
  "variacao_escola_serie_com_qualidade.csv"
)

arquivos_necessarios <- c(
  arquivo_rede,
  arquivo_variacao_rede,
  arquivo_variacao_escolas
)

arquivos_ausentes <- arquivos_necessarios[
  !file.exists(arquivos_necessarios)
]

if (length(arquivos_ausentes) > 0) {
  stop(
    "Arquivos não encontrados:\n",
    paste(arquivos_ausentes, collapse = "\n"),
    "\nExecute primeiro os scripts 05 e 06."
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

desvio_seguro <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) < 2) {
    return(NA_real_)
  }
  
  sd(x)
}

formatar_ano_escolar <- function(x) {
  paste0(x, "º ano")
}

# -------------------------------------------------------------------
# 3. Leitura
# -------------------------------------------------------------------

painel_rede <- read_csv(
  arquivo_rede,
  show_col_types = FALSE,
  col_types = cols(
    ano = col_integer(),
    ano_escolar = col_integer(),
    numero_escolas = col_double(),
    numero_turmas = col_double(),
    previstos = col_double(),
    avaliados = col_double(),
    taxa_participacao = col_double(),
    proficiencia_media = col_double(),
    pct_defasagem = col_double(),
    pct_intermediario = col_double(),
    pct_adequado = col_double(),
    .default = col_character()
  )
)

variacao_rede <- read_csv(
  arquivo_variacao_rede,
  show_col_types = FALSE,
  col_types = cols(
    ano_escolar = col_integer(),
    taxa_participacao_2025 = col_double(),
    taxa_participacao_2026 = col_double(),
    proficiencia_media_2025 = col_double(),
    proficiencia_media_2026 = col_double(),
    pct_defasagem_2025 = col_double(),
    pct_defasagem_2026 = col_double(),
    pct_intermediario_2025 = col_double(),
    pct_intermediario_2026 = col_double(),
    pct_adequado_2025 = col_double(),
    pct_adequado_2026 = col_double(),
    delta_participacao = col_double(),
    delta_proficiencia = col_double(),
    delta_pct_defasagem = col_double(),
    delta_pct_intermediario = col_double(),
    delta_pct_adequado = col_double(),
    .default = col_character()
  )
)

variacao_escolas <- read_csv(
  arquivo_variacao_escolas,
  show_col_types = FALSE,
  col_types = cols(
    id_escola = col_character(),
    codigo_inep = col_character(),
    nome_canonico = col_character(),
    assessora = col_character(),
    ano_escolar = col_integer(),
    componente = col_character(),
    presente_2025 = col_logical(),
    presente_2026 = col_logical(),
    painel_balanceado = col_logical(),
    painel_resultado_balanceado = col_logical(),
    previstos_2025 = col_double(),
    previstos_2026 = col_double(),
    avaliados_2025 = col_double(),
    avaliados_2026 = col_double(),
    taxa_participacao_2025 = col_double(),
    taxa_participacao_2026 = col_double(),
    proficiencia_media_2025 = col_double(),
    proficiencia_media_2026 = col_double(),
    pct_defasagem_2025 = col_double(),
    pct_defasagem_2026 = col_double(),
    pct_intermediario_2025 = col_double(),
    pct_intermediario_2026 = col_double(),
    pct_adequado_2025 = col_double(),
    pct_adequado_2026 = col_double(),
    delta_participacao = col_double(),
    delta_proficiencia = col_double(),
    delta_pct_defasagem = col_double(),
    delta_pct_intermediario = col_double(),
    delta_pct_adequado = col_double(),
    participacao_minima_70_ambos = col_logical(),
    participacao_minima_80_ambos = col_logical(),
    mudanca_previstos_20pct = col_logical(),
    observacao_composicao = col_character(),
    .default = col_character()
  )
)

# -------------------------------------------------------------------
# 4. Quadro geral da rede
# -------------------------------------------------------------------

quadro_rede <- painel_rede |>
  mutate(
    serie = formatar_ano_escolar(ano_escolar)
  ) |>
  select(
    ano,
    ano_escolar,
    serie,
    numero_escolas,
    numero_turmas,
    previstos,
    avaliados,
    taxa_participacao,
    proficiencia_media,
    pct_defasagem,
    pct_intermediario,
    pct_adequado
  ) |>
  arrange(
    ano_escolar,
    ano
  )

# -------------------------------------------------------------------
# 5. Variação agregada da rede
# -------------------------------------------------------------------

quadro_variacao_rede <- variacao_rede |>
  mutate(
    serie = formatar_ano_escolar(ano_escolar),
    
    sinal_proficiencia = case_when(
      is.na(delta_proficiencia) ~ "sem informação",
      delta_proficiencia > 0 ~ "aumento",
      delta_proficiencia < 0 ~ "redução",
      TRUE ~ "estável"
    ),
    
    sinal_adequado = case_when(
      is.na(delta_pct_adequado) ~ "sem informação",
      delta_pct_adequado > 0 ~ "aumento",
      delta_pct_adequado < 0 ~ "redução",
      TRUE ~ "estável"
    )
  ) |>
  select(
    ano_escolar,
    serie,
    taxa_participacao_2025,
    taxa_participacao_2026,
    delta_participacao,
    proficiencia_media_2025,
    proficiencia_media_2026,
    delta_proficiencia,
    pct_defasagem_2025,
    pct_defasagem_2026,
    delta_pct_defasagem,
    pct_adequado_2025,
    pct_adequado_2026,
    delta_pct_adequado,
    sinal_proficiencia,
    sinal_adequado
  ) |>
  arrange(ano_escolar)

# -------------------------------------------------------------------
# 6. Distribuição das mudanças entre escolas
# -------------------------------------------------------------------

distribuicao_variacoes <- variacao_escolas |>
  filter(
    painel_resultado_balanceado
  ) |>
  group_by(
    ano_escolar,
    componente
  ) |>
  summarise(
    numero_escolas = n(),
    
    delta_participacao_media =
      media_segura(delta_participacao),
    
    delta_participacao_mediana =
      mediana_segura(delta_participacao),
    
    delta_proficiencia_media =
      media_segura(delta_proficiencia),
    
    delta_proficiencia_mediana =
      mediana_segura(delta_proficiencia),
    
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
    
    desvio_delta_proficiencia =
      desvio_seguro(delta_proficiencia),
    
    delta_adequado_media =
      media_segura(delta_pct_adequado),
    
    delta_adequado_mediana =
      mediana_segura(delta_pct_adequado),
    
    escolas_aumento_proficiencia = sum(
      delta_proficiencia > 0,
      na.rm = TRUE
    ),
    
    escolas_reducao_proficiencia = sum(
      delta_proficiencia < 0,
      na.rm = TRUE
    ),
    
    escolas_aumento_adequado = sum(
      delta_pct_adequado > 0,
      na.rm = TRUE
    ),
    
    escolas_reducao_adequado = sum(
      delta_pct_adequado < 0,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    pct_escolas_aumento_proficiencia =
      100 *
      escolas_aumento_proficiencia /
      numero_escolas,
    
    pct_escolas_aumento_adequado =
      100 *
      escolas_aumento_adequado /
      numero_escolas
  ) |>
  arrange(ano_escolar)

# -------------------------------------------------------------------
# 7. Classificação diagnóstica das escolas-séries
# -------------------------------------------------------------------
#
# Os limites abaixo são apenas classificações descritivas.
# Não representam significância estatística ou efeito causal.
#
# Proficiência:
#   avanço: delta > 2
#   estabilidade: entre -2 e 2
#   redução: delta < -2
#
# Participação:
#   aumento: delta >= 5 p.p.
#   estabilidade: entre -5 e 5 p.p.
#   redução: delta <= -5 p.p.
# -------------------------------------------------------------------

classificacao_escolas <- variacao_escolas |>
  mutate(
    categoria_proficiencia = case_when(
      !painel_resultado_balanceado ~
        "Sem resultado comparável",
      
      delta_proficiencia > 2 ~
        "Aumento de proficiência",
      
      delta_proficiencia < -2 ~
        "Redução de proficiência",
      
      TRUE ~
        "Proficiência relativamente estável"
    ),
    
    categoria_participacao = case_when(
      !painel_balanceado ~
        "Sem presença nos dois anos",
      
      is.na(delta_participacao) ~
        "Sem participação comparável",
      
      delta_participacao >= 5 ~
        "Aumento de participação",
      
      delta_participacao <= -5 ~
        "Redução de participação",
      
      TRUE ~
        "Participação relativamente estável"
    ),
    
    perfil_diagnostico = case_when(
      categoria_proficiencia ==
        "Aumento de proficiência" &
        categoria_participacao ==
        "Aumento de participação" ~
        "Avanço com ampliação da participação",
      
      categoria_proficiencia ==
        "Aumento de proficiência" &
        categoria_participacao ==
        "Participação relativamente estável" ~
        "Avanço com participação estável",
      
      categoria_proficiencia ==
        "Aumento de proficiência" &
        categoria_participacao ==
        "Redução de participação" ~
        "Avanço com perda de participação",
      
      categoria_proficiencia ==
        "Proficiência relativamente estável" &
        categoria_participacao ==
        "Aumento de participação" ~
        "Desempenho estável com maior participação",
      
      categoria_proficiencia ==
        "Proficiência relativamente estável" ~
        "Desempenho relativamente estável",
      
      categoria_proficiencia ==
        "Redução de proficiência" &
        categoria_participacao ==
        "Aumento de participação" ~
        "Redução com ampliação da participação",
      
      categoria_proficiencia ==
        "Redução de proficiência" &
        categoria_participacao ==
        "Participação relativamente estável" ~
        "Redução com participação estável",
      
      categoria_proficiencia ==
        "Redução de proficiência" &
        categoria_participacao ==
        "Redução de participação" ~
        "Redução de desempenho e participação",
      
      TRUE ~
        "Análise complementar necessária"
    ),
    
    prioridade_diagnostica = case_when(
      perfil_diagnostico ==
        "Redução de desempenho e participação" ~
        "Alta",
      
      perfil_diagnostico %in% c(
        "Redução com participação estável",
        "Redução com ampliação da participação",
        "Avanço com perda de participação"
      ) ~
        "Intermediária",
      
      painel_resultado_balanceado ~
        "Monitoramento",
      
      TRUE ~
        "Verificar dados"
    )
  ) |>
  arrange(
    ano_escolar,
    factor(
      prioridade_diagnostica,
      levels = c(
        "Alta",
        "Intermediária",
        "Monitoramento",
        "Verificar dados"
      )
    ),
    nome_canonico
  )

# -------------------------------------------------------------------
# 8. Resumo da classificação diagnóstica
# -------------------------------------------------------------------

resumo_classificacao <- classificacao_escolas |>
  filter(
    painel_resultado_balanceado
  ) |>
  count(
    ano_escolar,
    perfil_diagnostico,
    name = "numero_escolas"
  ) |>
  group_by(ano_escolar) |>
  mutate(
    percentual_escolas =
      100 *
      numero_escolas /
      sum(numero_escolas)
  ) |>
  ungroup() |>
  arrange(
    ano_escolar,
    desc(numero_escolas)
  )

# -------------------------------------------------------------------
# 9. Relação entre participação e desempenho
# -------------------------------------------------------------------

dados_grafico_relacao <- classificacao_escolas |>
  filter(
    painel_resultado_balanceado,
    !is.na(delta_participacao),
    !is.na(delta_proficiencia)
  )

# -------------------------------------------------------------------
# 10. Gráfico: participação da rede
# -------------------------------------------------------------------

grafico_participacao <- painel_rede |>
  mutate(
    serie = factor(
      formatar_ano_escolar(ano_escolar),
      levels = paste0(1:5, "º ano")
    ),
    ano = factor(ano)
  ) |>
  ggplot(
    aes(
      x = serie,
      y = taxa_participacao,
      group = ano,
      linetype = ano,
      shape = ano
    )
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 2.5
  ) +
  scale_y_continuous(
    labels = label_number(
      decimal.mark = ",",
      suffix = "%"
    )
  ) +
  labs(
    title = "Participação na avaliação formativa",
    subtitle = "Rede municipal, por ano escolar",
    x = NULL,
    y = "Estudantes avaliados sobre previstos",
    linetype = "Ano",
    shape = "Ano",
    caption = paste(
      "A 1ª avaliação formativa de 2025 antecede",
      "o início do programa de assessoramento."
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
  filename = here(
    "resultados",
    "graficos",
    "participacao_rede_2025_2026.png"
  ),
  plot = grafico_participacao,
  width = 9,
  height = 5.5,
  dpi = 300
)

# -------------------------------------------------------------------
# 11. Gráfico: proficiência da rede
# -------------------------------------------------------------------

grafico_proficiencia <- painel_rede |>
  mutate(
    serie = factor(
      formatar_ano_escolar(ano_escolar),
      levels = paste0(1:5, "º ano")
    ),
    ano = factor(ano)
  ) |>
  ggplot(
    aes(
      x = serie,
      y = proficiencia_media,
      group = ano,
      linetype = ano,
      shape = ano
    )
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 2.5
  ) +
  labs(
    title = "Proficiência média na avaliação formativa",
    subtitle = "Rede municipal, por ano escolar",
    x = NULL,
    y = "Proficiência média",
    linetype = "Ano",
    shape = "Ano",
    caption = paste(
      "Comparação descritiva entre a linha de base de 2025",
      "e o período posterior ao início do programa em 2026."
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
  filename = here(
    "resultados",
    "graficos",
    "proficiencia_rede_2025_2026.png"
  ),
  plot = grafico_proficiencia,
  width = 9,
  height = 5.5,
  dpi = 300
)

# -------------------------------------------------------------------
# 12. Gráfico: aprendizagem adequada
# -------------------------------------------------------------------

grafico_adequado <- painel_rede |>
  mutate(
    serie = factor(
      formatar_ano_escolar(ano_escolar),
      levels = paste0(1:5, "º ano")
    ),
    ano = factor(ano)
  ) |>
  ggplot(
    aes(
      x = serie,
      y = pct_adequado,
      group = ano,
      linetype = ano,
      shape = ano
    )
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 2.5
  ) +
  scale_y_continuous(
    labels = label_number(
      decimal.mark = ",",
      suffix = "%"
    )
  ) +
  labs(
    title = "Estudantes em aprendizagem adequada",
    subtitle = "Rede municipal, por ano escolar",
    x = NULL,
    y = "Aprendizagem adequada",
    linetype = "Ano",
    shape = "Ano",
    caption = paste(
      "Resultados ponderados pelo número de estudantes",
      "avaliados nas turmas."
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
  filename = here(
    "resultados",
    "graficos",
    "aprendizagem_adequada_rede_2025_2026.png"
  ),
  plot = grafico_adequado,
  width = 9,
  height = 5.5,
  dpi = 300
)

# -------------------------------------------------------------------
# 13. Gráfico: distribuição da variação da proficiência
# -------------------------------------------------------------------

grafico_distribuicao <- variacao_escolas |>
  filter(
    painel_resultado_balanceado
  ) |>
  mutate(
    serie = factor(
      formatar_ano_escolar(ano_escolar),
      levels = paste0(1:5, "º ano")
    )
  ) |>
  ggplot(
    aes(
      x = serie,
      y = delta_proficiencia
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_boxplot(
    outlier.alpha = 0.5
  ) +
  labs(
    title = "Distribuição da mudança de proficiência entre escolas",
    subtitle = "Diferença entre 2026 e a linha de base de 2025",
    x = NULL,
    y = "Variação da proficiência",
    caption = paste(
      "Cada observação corresponde a uma escola-série",
      "com resultado disponível nos dois anos."
    )
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = here(
    "resultados",
    "graficos",
    "distribuicao_delta_proficiencia_escolas.png"
  ),
  plot = grafico_distribuicao,
  width = 9,
  height = 5.5,
  dpi = 300
)

# -------------------------------------------------------------------
# 14. Gráfico: participação e proficiência
# -------------------------------------------------------------------

grafico_relacao <- dados_grafico_relacao |>
  mutate(
    serie = factor(
      formatar_ano_escolar(ano_escolar),
      levels = paste0(1:5, "º ano")
    )
  ) |>
  ggplot(
    aes(
      x = delta_participacao,
      y = delta_proficiencia
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
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.7
  ) +
  facet_wrap(
    vars(serie),
    scales = "free"
  ) +
  labs(
    title = "Mudança da participação e da proficiência",
    subtitle = "Associação descritiva por escola-série",
    x = "Variação da participação (pontos percentuais)",
    y = "Variação da proficiência",
    caption = paste(
      "A linha representa associação linear descritiva;",
      "não constitui estimativa causal."
    )
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = here(
    "resultados",
    "graficos",
    "relacao_delta_participacao_proficiencia.png"
  ),
  plot = grafico_relacao,
  width = 10,
  height = 7,
  dpi = 300
)

# -------------------------------------------------------------------
# 15. Escolas para atenção diagnóstica
# -------------------------------------------------------------------

escolas_atencao <- classificacao_escolas |>
  filter(
    prioridade_diagnostica %in% c(
      "Alta",
      "Intermediária"
    )
  ) |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora,
    ano_escolar,
    componente,
    previstos_2025,
    previstos_2026,
    avaliados_2025,
    avaliados_2026,
    taxa_participacao_2025,
    taxa_participacao_2026,
    delta_participacao,
    proficiencia_media_2025,
    proficiencia_media_2026,
    delta_proficiencia,
    pct_adequado_2025,
    pct_adequado_2026,
    delta_pct_adequado,
    categoria_proficiencia,
    categoria_participacao,
    perfil_diagnostico,
    prioridade_diagnostica,
    observacao_composicao
  ) |>
  arrange(
    factor(
      prioridade_diagnostica,
      levels = c(
        "Alta",
        "Intermediária"
      )
    ),
    ano_escolar,
    delta_proficiencia,
    nome_canonico
  )

# -------------------------------------------------------------------
# 16. Exportação
# -------------------------------------------------------------------

write_csv(
  quadro_rede,
  here(
    "resultados",
    "quadro_geral_rede_2025_2026.csv"
  ),
  na = ""
)

write_csv(
  quadro_variacao_rede,
  here(
    "resultados",
    "quadro_variacao_rede_2025_2026.csv"
  ),
  na = ""
)

write_csv(
  distribuicao_variacoes,
  here(
    "resultados",
    "distribuicao_variacoes_escolas.csv"
  ),
  na = ""
)

write_csv(
  classificacao_escolas,
  here(
    "dados_processados",
    "classificacao_diagnostica_escolas.csv"
  ),
  na = ""
)

write_csv(
  resumo_classificacao,
  here(
    "resultados",
    "resumo_classificacao_diagnostica.csv"
  ),
  na = ""
)

write_csv(
  escolas_atencao,
  here(
    "resultados",
    "escolas_atencao_diagnostica.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 17. Resumo no console
# -------------------------------------------------------------------

cat("\nMódulo de resultados descritivos concluído.\n")

cat("\nVariação agregada da rede:\n")

print(
  quadro_variacao_rede |>
    select(
      ano_escolar,
      taxa_participacao_2025,
      taxa_participacao_2026,
      delta_participacao,
      proficiencia_media_2025,
      proficiencia_media_2026,
      delta_proficiencia,
      pct_adequado_2025,
      pct_adequado_2026,
      delta_pct_adequado
    )
)

cat("\nDistribuição das variações entre escolas:\n")

print(
  distribuicao_variacoes |>
    select(
      ano_escolar,
      numero_escolas,
      delta_proficiencia_media,
      delta_proficiencia_mediana,
      delta_proficiencia_p25,
      delta_proficiencia_p75,
      pct_escolas_aumento_proficiencia,
      pct_escolas_aumento_adequado
    )
)

cat("\nClassificação diagnóstica:\n")

print(
  resumo_classificacao
)

cat("\nNúmero de casos para atenção diagnóstica:\n")

print(
  escolas_atencao |>
    count(
      prioridade_diagnostica,
      ano_escolar
    ) |>
    arrange(
      prioridade_diagnostica,
      ano_escolar
    )
)

cat("\nArquivos gerados:\n")

cat(
  "- resultados/quadro_geral_rede_2025_2026.csv\n",
  "- resultados/quadro_variacao_rede_2025_2026.csv\n",
  "- resultados/distribuicao_variacoes_escolas.csv\n",
  "- dados_processados/classificacao_diagnostica_escolas.csv\n",
  "- resultados/resumo_classificacao_diagnostica.csv\n",
  "- resultados/escolas_atencao_diagnostica.csv\n",
  "- resultados/graficos/participacao_rede_2025_2026.png\n",
  "- resultados/graficos/proficiencia_rede_2025_2026.png\n",
  "- resultados/graficos/aprendizagem_adequada_rede_2025_2026.png\n",
  "- resultados/graficos/distribuicao_delta_proficiencia_escolas.png\n",
  "- resultados/graficos/relacao_delta_participacao_proficiencia.png\n"
)