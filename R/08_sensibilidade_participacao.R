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

arquivo_variacao <- here(
  "dados_processados",
  "variacao_escola_serie_com_qualidade.csv"
)

if (!file.exists(arquivo_variacao)) {
  stop(
    "Arquivo não encontrado: ",
    arquivo_variacao,
    "\nExecute primeiro o script R/06_qualidade_composicao.R."
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
      names = FALSE
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
# 3. Leitura
# -------------------------------------------------------------------

dados <- read_csv(
  arquivo_variacao,
  show_col_types = FALSE,
  col_types = cols(
    id_escola = col_character(),
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
    
    variacao_relativa_previstos = col_double(),
    
    .default = col_character()
  )
)

# -------------------------------------------------------------------
# 4. Base analítica
# -------------------------------------------------------------------

base_analitica <- dados |>
  filter(
    painel_resultado_balanceado,
    !is.na(delta_proficiencia),
    !is.na(delta_participacao)
  ) |>
  mutate(
    peso_medio_avaliados =
      (avaliados_2025 + avaliados_2026) / 2,
    
    participacao_70_ambos =
      taxa_participacao_2025 >= 70 &
      taxa_participacao_2026 >= 70,
    
    participacao_80_ambos =
      taxa_participacao_2025 >= 80 &
      taxa_participacao_2026 >= 80,
    
    participacao_estavel_5pp =
      abs(delta_participacao) <= 5,
    
    previstos_estaveis_20pct =
      !is.na(variacao_relativa_previstos) &
      abs(variacao_relativa_previstos) <= 20,
    
    faixa_delta_participacao = case_when(
      delta_participacao <= -10 ~
        "Queda de 10 p.p. ou mais",
      
      delta_participacao < -5 ~
        "Queda entre 5 e 10 p.p.",
      
      delta_participacao <= 5 ~
        "Variação entre -5 e 5 p.p.",
      
      delta_participacao < 10 ~
        "Aumento entre 5 e 10 p.p.",
      
      TRUE ~
        "Aumento de 10 p.p. ou mais"
    ),
    
    faixa_delta_participacao = factor(
      faixa_delta_participacao,
      levels = c(
        "Queda de 10 p.p. ou mais",
        "Queda entre 5 e 10 p.p.",
        "Variação entre -5 e 5 p.p.",
        "Aumento entre 5 e 10 p.p.",
        "Aumento de 10 p.p. ou mais"
      )
    )
  )

# -------------------------------------------------------------------
# 5. Sensibilidade por diferentes amostras
# -------------------------------------------------------------------

resumir_amostra <- function(base, nome_amostra) {
  
  base |>
    group_by(
      ano_escolar,
      componente
    ) |>
    summarise(
      amostra = nome_amostra,
      
      numero_escolas = n(),
      
      avaliados_2025 = sum(
        avaliados_2025,
        na.rm = TRUE
      ),
      
      avaliados_2026 = sum(
        avaliados_2026,
        na.rm = TRUE
      ),
      
      delta_participacao_media =
        media_segura(delta_participacao),
      
      delta_proficiencia_media_simples =
        media_segura(delta_proficiencia),
      
      delta_proficiencia_media_ponderada =
        media_ponderada_segura(
          delta_proficiencia,
          peso_medio_avaliados
        ),
      
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
      
      delta_adequado_media =
        media_segura(delta_pct_adequado),
      
      delta_defasagem_media =
        media_segura(delta_pct_defasagem),
      
      .groups = "drop"
    )
}

sensibilidade_amostras <- bind_rows(
  
  resumir_amostra(
    base_analitica,
    "Todas com resultado nos dois anos"
  ),
  
  resumir_amostra(
    base_analitica |>
      filter(participacao_70_ambos),
    "Participação mínima de 70% nos dois anos"
  ),
  
  resumir_amostra(
    base_analitica |>
      filter(participacao_80_ambos),
    "Participação mínima de 80% nos dois anos"
  ),
  
  resumir_amostra(
    base_analitica |>
      filter(participacao_estavel_5pp),
    "Participação variou no máximo 5 p.p."
  ),
  
  resumir_amostra(
    base_analitica |>
      filter(
        participacao_estavel_5pp,
        previstos_estaveis_20pct
      ),
    paste(
      "Participação estável e previstos",
      "variaram no máximo 20%"
    )
  )
) |>
  arrange(
    ano_escolar,
    amostra
  )

# -------------------------------------------------------------------
# 6. Resultados por faixa de mudança da participação
# -------------------------------------------------------------------

resultado_por_faixa_participacao <- base_analitica |>
  group_by(
    ano_escolar,
    faixa_delta_participacao
  ) |>
  summarise(
    numero_escolas = n(),
    
    participacao_2025_media =
      media_segura(taxa_participacao_2025),
    
    delta_participacao_media =
      media_segura(delta_participacao),
    
    proficiencia_2025_media =
      media_segura(proficiencia_media_2025),
    
    delta_proficiencia_media =
      media_segura(delta_proficiencia),
    
    delta_proficiencia_mediana =
      mediana_segura(delta_proficiencia),
    
    delta_adequado_media =
      media_segura(delta_pct_adequado),
    
    delta_defasagem_media =
      media_segura(delta_pct_defasagem),
    
    .groups = "drop"
  )

# -------------------------------------------------------------------
# 7. Modelos descritivos ajustados
# -------------------------------------------------------------------
#
# Modelo 1:
# mudança de proficiência explicada somente pela mudança de participação.
#
# Modelo 2:
# acrescenta proficiência inicial e participação inicial.
#
# Modelo 3:
# acrescenta alteração relativa dos estudantes previstos e série.
#
# Não interpretar os coeficientes como efeitos causais.
# -------------------------------------------------------------------

base_modelo <- base_analitica |>
  filter(
    !is.na(proficiencia_media_2025),
    !is.na(taxa_participacao_2025),
    !is.na(variacao_relativa_previstos),
    peso_medio_avaliados > 0
  ) |>
  mutate(
    ano_escolar_fator = factor(ano_escolar)
  )

modelo_1 <- lm(
  delta_proficiencia ~
    delta_participacao,
  data = base_modelo,
  weights = peso_medio_avaliados
)

modelo_2 <- lm(
  delta_proficiencia ~
    delta_participacao +
    proficiencia_media_2025 +
    taxa_participacao_2025,
  data = base_modelo,
  weights = peso_medio_avaliados
)

modelo_3 <- lm(
  delta_proficiencia ~
    delta_participacao +
    proficiencia_media_2025 +
    taxa_participacao_2025 +
    variacao_relativa_previstos +
    ano_escolar_fator,
  data = base_modelo,
  weights = peso_medio_avaliados
)

coeficientes_modelos <- bind_rows(
  extrair_coeficientes(
    modelo_1,
    "Modelo 1: participação"
  ),
  extrair_coeficientes(
    modelo_2,
    "Modelo 2: resultado e participação iniciais"
  ),
  extrair_coeficientes(
    modelo_3,
    "Modelo 3: composição e efeitos de série"
  )
)

ajuste_modelos <- tibble(
  modelo = c(
    "Modelo 1: participação",
    "Modelo 2: resultado e participação iniciais",
    "Modelo 3: composição e efeitos de série"
  ),
  numero_observacoes = c(
    nobs(modelo_1),
    nobs(modelo_2),
    nobs(modelo_3)
  ),
  r_quadrado = c(
    summary(modelo_1)$r.squared,
    summary(modelo_2)$r.squared,
    summary(modelo_3)$r.squared
  ),
  r_quadrado_ajustado = c(
    summary(modelo_1)$adj.r.squared,
    summary(modelo_2)$adj.r.squared,
    summary(modelo_3)$adj.r.squared
  )
)

# -------------------------------------------------------------------
# 8. Padronização descritiva para participação estável
# -------------------------------------------------------------------
#
# Fazemos uma previsão contrafactual apenas descritiva:
# mantêm-se as características observadas das escolas, mas se define
# delta_participacao = 0.
#
# Isso não identifica o efeito do programa. É somente uma padronização
# para avaliar quanto a mudança de composição altera a leitura agregada.
# -------------------------------------------------------------------

cenario_participacao_estavel <- base_modelo |>
  mutate(
    delta_participacao = 0
  )

base_modelo <- base_modelo |>
  mutate(
    delta_proficiencia_predita_observada =
      predict(
        modelo_3,
        newdata = base_modelo
      ),
    
    delta_proficiencia_predita_participacao_estavel =
      predict(
        modelo_3,
        newdata = cenario_participacao_estavel
      )
  )

resumo_padronizacao <- base_modelo |>
  group_by(ano_escolar) |>
  summarise(
    numero_escolas = n(),
    
    delta_proficiencia_observada =
      media_ponderada_segura(
        delta_proficiencia,
        peso_medio_avaliados
      ),
    
    delta_proficiencia_predita =
      media_ponderada_segura(
        delta_proficiencia_predita_observada,
        peso_medio_avaliados
      ),
    
    delta_proficiencia_padronizada_participacao_estavel =
      media_ponderada_segura(
        delta_proficiencia_predita_participacao_estavel,
        peso_medio_avaliados
      ),
    
    diferenca_associada_composicao =
      delta_proficiencia_observada -
      delta_proficiencia_padronizada_participacao_estavel,
    
    .groups = "drop"
  )

# -------------------------------------------------------------------
# 9. Gráfico de sensibilidade por amostra
# -------------------------------------------------------------------

grafico_sensibilidade <- sensibilidade_amostras |>
  mutate(
    serie = factor(
      paste0(ano_escolar, "º ano"),
      levels = paste0(1:5, "º ano")
    )
  ) |>
  ggplot(
    aes(
      x = serie,
      y = delta_proficiencia_media_ponderada,
      group = amostra,
      linetype = amostra,
      shape = amostra
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_line(
    linewidth = 0.7
  ) +
  geom_point(
    size = 2.2
  ) +
  labs(
    title = "Sensibilidade da mudança de proficiência à composição",
    subtitle = "Resultados segundo diferentes critérios de participação",
    x = NULL,
    y = "Mudança média ponderada da proficiência",
    linetype = "Amostra",
    shape = "Amostra",
    caption = paste(
      "Análise observacional. Os filtros não transformam",
      "a comparação antes–depois em estimativa causal."
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
    "sensibilidade_proficiencia_participacao.png"
  ),
  plot = grafico_sensibilidade,
  width = 11,
  height = 7,
  dpi = 300
)

# -------------------------------------------------------------------
# 10. Gráfico: mudança de participação e proficiência
# -------------------------------------------------------------------

grafico_participacao_proficiencia <- base_analitica |>
  mutate(
    serie = factor(
      paste0(ano_escolar, "º ano"),
      levels = paste0(1:5, "º ano")
    )
  ) |>
  ggplot(
    aes(
      x = delta_participacao,
      y = delta_proficiencia,
      size = peso_medio_avaliados
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
    alpha = 0.55
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    aes(weight = peso_medio_avaliados),
    linewidth = 0.7
  ) +
  facet_wrap(
    vars(serie),
    scales = "free"
  ) +
  scale_size_continuous(
    name = "Média de avaliados"
  ) +
  labs(
    title = "Participação e proficiência no período pós-implantação",
    subtitle = paste(
      "Mudanças observadas entre a linha de base de 2025",
      "e a avaliação de 2026"
    ),
    x = "Mudança da participação (pontos percentuais)",
    y = "Mudança da proficiência",
    caption = paste(
      "O tamanho dos pontos representa o número médio",
      "de estudantes avaliados. Associação não causal."
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
    "participacao_composicao_proficiencia.png"
  ),
  plot = grafico_participacao_proficiencia,
  width = 11,
  height = 7,
  dpi = 300
)

# -------------------------------------------------------------------
# 11. Exportação
# -------------------------------------------------------------------

write_csv(
  sensibilidade_amostras,
  here(
    "resultados",
    "sensibilidade_resultados_por_amostra.csv"
  ),
  na = ""
)

write_csv(
  resultado_por_faixa_participacao,
  here(
    "resultados",
    "resultados_por_faixa_delta_participacao.csv"
  ),
  na = ""
)

write_csv(
  coeficientes_modelos,
  here(
    "resultados",
    "modelos_descritivos_coeficientes.csv"
  ),
  na = ""
)

write_csv(
  ajuste_modelos,
  here(
    "resultados",
    "modelos_descritivos_ajuste.csv"
  ),
  na = ""
)

write_csv(
  resumo_padronizacao,
  here(
    "resultados",
    "padronizacao_participacao_estavel.csv"
  ),
  na = ""
)

write_csv(
  base_modelo,
  here(
    "dados_processados",
    "base_analitica_ajuste_participacao.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 12. Resumo no console
# -------------------------------------------------------------------

cat("\nMódulo de sensibilidade à participação concluído.\n")

cat("\nResultados por amostra:\n")

print(
  sensibilidade_amostras |>
    select(
      ano_escolar,
      amostra,
      numero_escolas,
      delta_participacao_media,
      delta_proficiencia_media_ponderada,
      delta_proficiencia_mediana,
      delta_adequado_media
    )
)

cat("\nCoeficiente associado à mudança da participação:\n")

print(
  coeficientes_modelos |>
    filter(termo == "delta_participacao") |>
    select(
      modelo,
      estimativa,
      erro_padrao,
      valor_p
    )
)

cat("\nPadronização para participação estável:\n")

print(resumo_padronizacao)

cat("\nArquivos gerados:\n")

cat(
  "- resultados/sensibilidade_resultados_por_amostra.csv\n",
  "- resultados/resultados_por_faixa_delta_participacao.csv\n",
  "- resultados/modelos_descritivos_coeficientes.csv\n",
  "- resultados/modelos_descritivos_ajuste.csv\n",
  "- resultados/padronizacao_participacao_estavel.csv\n",
  "- dados_processados/base_analitica_ajuste_participacao.csv\n",
  "- resultados/graficos/sensibilidade_proficiencia_participacao.png\n",
  "- resultados/graficos/participacao_composicao_proficiencia.png\n"
)