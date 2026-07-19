library(here)
library(tidyverse)
library(scales)

# -------------------------------------------------------------------
# 1. Objetivo
# -------------------------------------------------------------------
#
# Consolidar os resultados descritivos e de robustez em produtos
# diretamente utilizáveis no relatório técnico.
#
# O módulo:
#   - resume o painel 2025-2026;
#   - compara resultados por série;
#   - sintetiza associações contextuais robustas;
#   - identifica padrões consistentes entre especificações;
#   - gera quadros executivos e tabelas para redação;
#   - preserva linguagem estritamente não causal.
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 2. Diretórios e arquivos
# -------------------------------------------------------------------

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

dir.create(
  here("documentacao", "sintese"),
  recursive = TRUE,
  showWarnings = FALSE
)

arquivo_base <- here(
  "dados_processados",
  "base_contexto_desempenho_estratos_corrigidos.csv"
)

arquivo_coeficientes <- here(
  "resultados",
  "contexto",
  "modelos_contextuais_coeficientes_cluster_escola.csv"
)

arquivo_sensibilidade <- here(
  "resultados",
  "contexto",
  "modelo_contextual_sensibilidade_sem_revisao_tecnica.csv"
)

arquivo_estratos <- here(
  "resultados",
  "contexto",
  "resultados_por_estratos_contextuais_2024_corrigido.csv"
)

arquivos_necessarios <- c(
  arquivo_base,
  arquivo_coeficientes,
  arquivo_sensibilidade,
  arquivo_estratos
)

arquivos_ausentes <- arquivos_necessarios[
  !file.exists(arquivos_necessarios)
]

if (length(arquivos_ausentes) > 0) {
  stop(
    "Arquivos necessários não encontrados:\n",
    paste(arquivos_ausentes, collapse = "\n"),
    "\nExecute primeiro R/13A_corrigir_estratos_e_robustez.R."
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

classificar_evidencia <- function(valor_p) {
  case_when(
    is.na(valor_p) ~
      "Não estimado",

    valor_p < 0.01 ~
      "Associação estatisticamente clara",

    valor_p < 0.05 ~
      "Associação estatisticamente clara",

    valor_p < 0.10 ~
      "Associação sugestiva",

    TRUE ~
      "Sem associação estatisticamente clara"
  )
}

classificar_direcao <- function(estimativa) {
  case_when(
    is.na(estimativa) ~
      "Não estimada",

    estimativa > 0 ~
      "Positiva",

    estimativa < 0 ~
      "Negativa",

    TRUE ~
      "Nula"
  )
}

# -------------------------------------------------------------------
# 4. Leitura
# -------------------------------------------------------------------

base <- read_csv(
  arquivo_base,
  show_col_types = FALSE,
  col_types = cols(
    id_escola = col_character(),
    nome_canonico = col_character(),
    assessora = col_character(),
    ano_escolar = col_integer(),
    componente = col_character(),
    avaliados_2025 = col_double(),
    avaliados_2026 = col_double(),
    previstos_2025 = col_double(),
    previstos_2026 = col_double(),
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
    peso_medio_avaliados = col_double(),
    faixa_porte_contextual_escola = col_character(),
    quartil_infraestrutura_escola = col_character(),
    incluir_universo_municipal_ampliado_2024 = col_logical(),
    incluir_universo_municipal_direto_2024 = col_logical(),
    requer_revisao_tecnica = col_logical(),
    .default = col_guess()
  )
)

coeficientes <- read_csv(
  arquivo_coeficientes,
  show_col_types = FALSE
)

sensibilidade <- read_csv(
  arquivo_sensibilidade,
  show_col_types = FALSE
)

estratos <- read_csv(
  arquivo_estratos,
  show_col_types = FALSE
)

# -------------------------------------------------------------------
# 5. Síntese geral do painel
# -------------------------------------------------------------------

sintese_geral <- base |>
  summarise(
    escolas = n_distinct(id_escola),
    observacoes_escola_serie = n(),
    series = n_distinct(ano_escolar),

    previstos_2025 = sum(
      previstos_2025,
      na.rm = TRUE
    ),

    previstos_2026 = sum(
      previstos_2026,
      na.rm = TRUE
    ),

    avaliados_2025 = sum(
      avaliados_2025,
      na.rm = TRUE
    ),

    avaliados_2026 = sum(
      avaliados_2026,
      na.rm = TRUE
    ),

    participacao_2025 = if_else(
      previstos_2025 > 0,
      avaliados_2025 / previstos_2025,
      NA_real_
    ),

    participacao_2026 = if_else(
      previstos_2026 > 0,
      avaliados_2026 / previstos_2026,
      NA_real_
    ),

    delta_participacao =
      participacao_2026 -
      participacao_2025,

    proficiencia_2025_ponderada =
      media_ponderada_segura(
        proficiencia_media_2025,
        avaliados_2025
      ),

    proficiencia_2026_ponderada =
      media_ponderada_segura(
        proficiencia_media_2026,
        avaliados_2026
      ),

    delta_proficiencia_ponderada =
      proficiencia_2026_ponderada -
      proficiencia_2025_ponderada,

    adequado_2025_ponderado =
      media_ponderada_segura(
        pct_adequado_2025,
        avaliados_2025
      ),

    adequado_2026_ponderado =
      media_ponderada_segura(
        pct_adequado_2026,
        avaliados_2026
      ),

    delta_adequado_ponderado =
      adequado_2026_ponderado -
      adequado_2025_ponderado,

    defasagem_2025_ponderada =
      media_ponderada_segura(
        pct_defasagem_2025,
        avaliados_2025
      ),

    defasagem_2026_ponderada =
      media_ponderada_segura(
        pct_defasagem_2026,
        avaliados_2026
      ),

    delta_defasagem_ponderada =
      defasagem_2026_ponderada -
      defasagem_2025_ponderada
  )

# -------------------------------------------------------------------
# 6. Síntese por série
# -------------------------------------------------------------------

sintese_por_serie <- base |>
  group_by(
    ano_escolar
  ) |>
  summarise(
    escolas = n_distinct(id_escola),

    previstos_2025 = sum(
      previstos_2025,
      na.rm = TRUE
    ),

    previstos_2026 = sum(
      previstos_2026,
      na.rm = TRUE
    ),

    avaliados_2025 = sum(
      avaliados_2025,
      na.rm = TRUE
    ),

    avaliados_2026 = sum(
      avaliados_2026,
      na.rm = TRUE
    ),

    participacao_2025 = if_else(
      previstos_2025 > 0,
      avaliados_2025 / previstos_2025,
      NA_real_
    ),

    participacao_2026 = if_else(
      previstos_2026 > 0,
      avaliados_2026 / previstos_2026,
      NA_real_
    ),

    delta_participacao =
      participacao_2026 -
      participacao_2025,

    proficiencia_2025_ponderada =
      media_ponderada_segura(
        proficiencia_media_2025,
        avaliados_2025
      ),

    proficiencia_2026_ponderada =
      media_ponderada_segura(
        proficiencia_media_2026,
        avaliados_2026
      ),

    delta_proficiencia_ponderada =
      proficiencia_2026_ponderada -
      proficiencia_2025_ponderada,

    adequado_2025_ponderado =
      media_ponderada_segura(
        pct_adequado_2025,
        avaliados_2025
      ),

    adequado_2026_ponderado =
      media_ponderada_segura(
        pct_adequado_2026,
        avaliados_2026
      ),

    delta_adequado_ponderado =
      adequado_2026_ponderado -
      adequado_2025_ponderado,

    defasagem_2025_ponderada =
      media_ponderada_segura(
        pct_defasagem_2025,
        avaliados_2025
      ),

    defasagem_2026_ponderada =
      media_ponderada_segura(
        pct_defasagem_2026,
        avaliados_2026
      ),

    delta_defasagem_ponderada =
      defasagem_2026_ponderada -
      defasagem_2025_ponderada,

    delta_proficiencia_media_escolas =
      media_segura(delta_proficiencia),

    delta_proficiencia_mediana_escolas =
      mediana_segura(delta_proficiencia),

    .groups = "drop"
  ) |>
  arrange(
    ano_escolar
  )

# -------------------------------------------------------------------
# 7. Comparação das especificações robustas
# -------------------------------------------------------------------

termos_interesse <- c(
  "proficiencia_media_2025",
  "delta_participacao",
  "log_matriculas_anos_iniciais",
  "indice_infraestrutura_basica",
  "pct_matriculas_anos_iniciais_integral"
)

coeficientes_principais <- coeficientes |>
  filter(
    termo %in% termos_interesse
  ) |>
  mutate(
    especificacao = case_when(
      str_detect(
        modelo,
        "Modelo A"
      ) ~ "Resultado inicial em 2025",

      str_detect(
        modelo,
        "Modelo B"
      ) ~ "Variação 2025–2026 — todas as escolas",

      str_detect(
        modelo,
        "Modelo C"
      ) ~ "Variação 2025–2026 — universo municipal ampliado",

      TRUE ~ modelo
    ),

    direcao = classificar_direcao(
      estimativa
    ),

    classificacao_evidencia =
      classificar_evidencia(
        valor_p_cluster
      )
  ) |>
  select(
    especificacao,
    termo,
    estimativa,
    erro_padrao_cluster_escola,
    estatistica_t_cluster,
    valor_p_cluster,
    direcao,
    classificacao_evidencia
  )

sensibilidade_principal <- sensibilidade |>
  filter(
    termo %in% termos_interesse
  ) |>
  mutate(
    especificacao =
      "Variação 2025–2026 — sem caso de revisão técnica",

    direcao = classificar_direcao(
      estimativa
    ),

    classificacao_evidencia =
      classificar_evidencia(
        valor_p_cluster
      )
  ) |>
  select(
    especificacao,
    termo,
    estimativa,
    erro_padrao_cluster_escola,
    estatistica_t_cluster,
    valor_p_cluster,
    direcao,
    classificacao_evidencia
  )

comparacao_modelos <- bind_rows(
  coeficientes_principais,
  sensibilidade_principal
) |>
  arrange(
    termo,
    especificacao
  )

# -------------------------------------------------------------------
# 8. Consistência entre modelos de variação
# -------------------------------------------------------------------

modelos_variacao <- comparacao_modelos |>
  filter(
    str_detect(
      especificacao,
      "Variação"
    )
  )

consistencia_associacoes <- modelos_variacao |>
  group_by(
    termo
  ) |>
  summarise(
    numero_especificacoes = n(),

    estimativa_minima =
      min(
        estimativa,
        na.rm = TRUE
      ),

    estimativa_maxima =
      max(
        estimativa,
        na.rm = TRUE
      ),

    sinais_positivos =
      sum(
        estimativa > 0,
        na.rm = TRUE
      ),

    sinais_negativos =
      sum(
        estimativa < 0,
        na.rm = TRUE
      ),

    especificacoes_p_menor_005 =
      sum(
        valor_p_cluster < 0.05,
        na.rm = TRUE
      ),

    especificacoes_p_menor_010 =
      sum(
        valor_p_cluster < 0.10,
        na.rm = TRUE
      ),

    sinal_consistente = case_when(
      sinais_positivos ==
        numero_especificacoes ~
        "Positivo em todas",

      sinais_negativos ==
        numero_especificacoes ~
        "Negativo em todas",

      TRUE ~
        "Sinal não consistente"
    ),

    evidencia_consistente = case_when(
      especificacoes_p_menor_005 ==
        numero_especificacoes ~
        "Clara em todas as especificações",

      especificacoes_p_menor_010 ==
        numero_especificacoes ~
        "Ao menos sugestiva em todas",

      especificacoes_p_menor_005 > 0 ~
        "Clara apenas em parte das especificações",

      especificacoes_p_menor_010 > 0 ~
        "Sugestiva apenas em parte das especificações",

      TRUE ~
        "Sem evidência estatística consistente"
    ),

    .groups = "drop"
  )

# -------------------------------------------------------------------
# 9. Síntese dos estratos
# -------------------------------------------------------------------

sintese_estratos <- estratos |>
  group_by(
    dimensao_contextual,
    categoria
  ) |>
  summarise(
    series_observadas =
      n_distinct(ano_escolar),

    numero_escolas_min =
      min(
        numero_escolas,
        na.rm = TRUE
      ),

    numero_escolas_max =
      max(
        numero_escolas,
        na.rm = TRUE
      ),

    proficiencia_2025_media_entre_series =
      media_segura(
        proficiencia_2025_media
      ),

    delta_participacao_media_entre_series =
      media_segura(
        delta_participacao_media
      ),

    delta_proficiencia_media_entre_series =
      media_segura(
        delta_proficiencia_media
      ),

    delta_adequado_media_entre_series =
      media_segura(
        delta_adequado_media
      ),

    delta_defasagem_media_entre_series =
      media_segura(
        delta_defasagem_media
      ),

    .groups = "drop"
  ) |>
  arrange(
    dimensao_contextual,
    categoria
  )

# -------------------------------------------------------------------
# 10. Quadro interpretativo automático
# -------------------------------------------------------------------
#
# Este quadro não substitui a interpretação substantiva. Ele organiza
# o que pode ou não ser afirmado com base nos resultados.
# -------------------------------------------------------------------

quadro_interpretativo <- tribble(
  ~tema, ~conclusao_tecnica, ~grau_de_seguranca, ~limite_de_interpretacao,

  "Cobertura da análise",
  paste0(
    "O painel comparável contém ",
    n_distinct(base$id_escola),
    " escolas e ",
    nrow(base),
    " observações escola-série."
  ),
  "Alto",
  "A cobertura se refere às observações comparáveis disponíveis em 2025 e 2026.",

  "Participação",
  paste0(
    "A participação agregada passou de ",
    percent(
      sintese_geral$participacao_2025,
      accuracy = 0.1
    ),
    " para ",
    percent(
      sintese_geral$participacao_2026,
      accuracy = 0.1
    ),
    "."
  ),
  "Alto",
  paste(
    "Mudanças de participação podem alterar a composição dos estudantes",
    "avaliados e devem acompanhar qualquer leitura de desempenho."
  ),

  "Proficiência",
  paste0(
    "A proficiência média ponderada variou ",
    number(
      sintese_geral$delta_proficiencia_ponderada,
      accuracy = 0.1
    ),
    " pontos entre 2025 e 2026."
  ),
  "Alto para descrição",
  "A diferença temporal não identifica efeito causal do assessoramento.",

  "Contexto escolar",
  paste(
    "Porte, infraestrutura e tempo integral foram analisados como",
    "características pré-programa e não como tratamentos."
  ),
  "Alto",
  "As associações condicionais podem refletir composição e fatores não observados.",

  "Modelos",
  paste(
    "Os erros-padrão foram agrupados por escola para considerar",
    "a repetição das séries dentro das unidades."
  ),
  "Alto",
  paste(
    "Os modelos são exploratórios; os valores-p não transformam",
    "as associações em evidência causal."
  ),

  "Proficiência inicial",
  paste(
    "A associação entre proficiência inicial e variação posterior",
    "deve ser lida com cautela por construção matemática e regressão à média."
  ),
  "Alto",
  "Não interpretar o coeficiente como efeito de baixo desempenho inicial."
)

# -------------------------------------------------------------------
# 11. Ranking descritivo por série
# -------------------------------------------------------------------
#
# Produz apenas posições descritivas. Não classifica qualidade da escola
# nem desempenho da assessora.
# -------------------------------------------------------------------

ranking_series <- base |>
  group_by(
    ano_escolar
  ) |>
  mutate(
    posicao_delta_proficiencia =
      min_rank(
        desc(delta_proficiencia)
      ),

    percentil_delta_proficiencia =
      percent_rank(
        delta_proficiencia
      )
  ) |>
  ungroup() |>
  select(
    id_escola,
    nome_canonico,
    assessora,
    ano_escolar,
    avaliados_2025,
    avaliados_2026,
    taxa_participacao_2025,
    taxa_participacao_2026,
    delta_participacao,
    proficiencia_media_2025,
    proficiencia_media_2026,
    delta_proficiencia,
    posicao_delta_proficiencia,
    percentil_delta_proficiencia,
    faixa_porte_contextual_escola,
    quartil_infraestrutura_escola
  ) |>
  arrange(
    ano_escolar,
    posicao_delta_proficiencia,
    nome_canonico
  )

# -------------------------------------------------------------------
# 12. Gráficos de síntese
# -------------------------------------------------------------------

grafico_series <- sintese_por_serie |>
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
  geom_col() +
  labs(
    title = "Variação ponderada da proficiência por ano escolar",
    subtitle = "Comparação descritiva entre a 1ª avaliação formativa de 2025 e 2026",
    x = NULL,
    y = "Variação da proficiência",
    caption = paste(
      "Resultados ponderados pelo número de estudantes avaliados.",
      "Não representam estimativa causal."
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
    "variacao_proficiencia_por_serie.png"
  ),
  grafico_series,
  width = 9,
  height = 5.5,
  dpi = 300
)

grafico_participacao <- sintese_por_serie |>
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
    subtitle = "Participação agregada ponderada pelo número previsto de estudantes",
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
    "participacao_por_serie_2025_2026.png"
  ),
  grafico_participacao,
  width = 9,
  height = 5.5,
  dpi = 300
)

# -------------------------------------------------------------------
# 13. Exportação
# -------------------------------------------------------------------

write_csv(
  sintese_geral,
  here(
    "resultados",
    "sintese",
    "01_sintese_geral_painel.csv"
  ),
  na = ""
)

write_csv(
  sintese_por_serie,
  here(
    "resultados",
    "sintese",
    "02_sintese_por_serie.csv"
  ),
  na = ""
)

write_csv(
  comparacao_modelos,
  here(
    "resultados",
    "sintese",
    "03_comparacao_modelos_contextuais.csv"
  ),
  na = ""
)

write_csv(
  consistencia_associacoes,
  here(
    "resultados",
    "sintese",
    "04_consistencia_associacoes_contextuais.csv"
  ),
  na = ""
)

write_csv(
  sintese_estratos,
  here(
    "resultados",
    "sintese",
    "05_sintese_estratos_contextuais.csv"
  ),
  na = ""
)

write_csv(
  quadro_interpretativo,
  here(
    "resultados",
    "sintese",
    "06_quadro_interpretativo.csv"
  ),
  na = ""
)

write_csv(
  ranking_series,
  here(
    "resultados",
    "sintese",
    "07_ranking_descritivo_escola_serie.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 14. Documento metodológico em texto
# -------------------------------------------------------------------

nota_metodologica <- c(
  "# Nota metodológica da síntese",
  "",
  "## Natureza da análise",
  "",
  paste(
    "A análise compara resultados observados na primeira avaliação",
    "formativa de Língua Portuguesa de 2025 e 2026."
  ),
  "",
  paste(
    "Não há grupo de controle, medida de intensidade do assessoramento",
    "ou desenho de identificação causal. Portanto, as diferenças",
    "temporais e as associações contextuais não devem ser interpretadas",
    "como efeitos do programa."
  ),
  "",
  "## Unidade de análise",
  "",
  paste(
    "A unidade principal é escola × ano escolar. As características",
    "contextuais de 2024 se repetem entre as séries da mesma escola."
  ),
  "",
  paste(
    "Por esse motivo, os modelos exploratórios usam erros-padrão",
    "agrupados no nível da escola."
  ),
  "",
  "## Participação",
  "",
  paste(
    "As taxas de participação são apresentadas conjuntamente com os",
    "resultados de proficiência, pois mudanças na cobertura podem alterar",
    "a composição da população avaliada."
  ),
  "",
  "## Estratos contextuais",
  "",
  paste(
    "Os estratos de porte e infraestrutura foram definidos uma única vez",
    "no nível da escola e depois vinculados às observações escola-série."
  ),
  "",
  "## Proficiência inicial e variação",
  "",
  paste(
    "Modelos que relacionam o nível inicial à mudança subsequente estão",
    "sujeitos à regressão à média e à correlação mecânica decorrente da",
    "presença da medida inicial nos dois lados da especificação."
  )
)

writeLines(
  nota_metodologica,
  here(
    "documentacao",
    "sintese",
    "nota_metodologica_sintese.md"
  ),
  useBytes = TRUE
)

# -------------------------------------------------------------------
# 15. Resumo no console
# -------------------------------------------------------------------

cat(
  "\nMódulo de síntese concluído.\n"
)

cat(
  "\nSíntese geral:\n"
)

print(
  sintese_geral,
  width = Inf
)

cat(
  "\nSíntese por série:\n"
)

print(
  sintese_por_serie,
  n = Inf,
  width = Inf
)

cat(
  "\nConsistência das associações contextuais:\n"
)

print(
  consistencia_associacoes,
  n = Inf,
  width = Inf
)

cat(
  "\nArquivos gerados:\n",
  "- resultados/sintese/01_sintese_geral_painel.csv\n",
  "- resultados/sintese/02_sintese_por_serie.csv\n",
  "- resultados/sintese/03_comparacao_modelos_contextuais.csv\n",
  "- resultados/sintese/04_consistencia_associacoes_contextuais.csv\n",
  "- resultados/sintese/05_sintese_estratos_contextuais.csv\n",
  "- resultados/sintese/06_quadro_interpretativo.csv\n",
  "- resultados/sintese/07_ranking_descritivo_escola_serie.csv\n",
  "- resultados/graficos/sintese/variacao_proficiencia_por_serie.png\n",
  "- resultados/graficos/sintese/participacao_por_serie_2025_2026.png\n",
  "- documentacao/sintese/nota_metodologica_sintese.md\n"
)
