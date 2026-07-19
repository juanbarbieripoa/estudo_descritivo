library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Diretórios e arquivos
# -------------------------------------------------------------------

dir.create(
  here("dados_processados"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("documentacao"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("resultados"),
  recursive = TRUE,
  showWarnings = FALSE
)

arquivo_painel <- here(
  "dados_processados",
  "painel_escola_serie_ano.csv"
)

arquivo_variacao <- here(
  "dados_processados",
  "variacao_escola_serie_2025_2026.csv"
)

arquivos_necessarios <- c(
  arquivo_painel,
  arquivo_variacao
)

arquivos_ausentes <- arquivos_necessarios[
  !file.exists(arquivos_necessarios)
]

if (length(arquivos_ausentes) > 0) {
  stop(
    "Arquivos não encontrados:\n",
    paste(arquivos_ausentes, collapse = "\n"),
    "\nExecute primeiro o script R/05_construir_painel.R."
  )
}

# -------------------------------------------------------------------
# 2. Funções auxiliares
# -------------------------------------------------------------------

media_ponderada_segura <- function(x, peso) {
  
  validos <- !is.na(x) &
    !is.na(peso) &
    peso > 0
  
  if (!any(validos)) {
    return(NA_real_)
  }
  
  weighted.mean(
    x = x[validos],
    w = peso[validos]
  )
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

# -------------------------------------------------------------------
# 3. Leitura
# -------------------------------------------------------------------

painel <- read_csv(
  arquivo_painel,
  show_col_types = FALSE,
  col_types = cols(
    ano = col_integer(),
    id_escola = col_character(),
    codigo_inep = col_character(),
    nome_canonico = col_character(),
    assessora = col_character(),
    ano_escolar = col_integer(),
    componente = col_character(),
    numero_turmas = col_double(),
    previstos = col_double(),
    avaliados = col_double(),
    taxa_participacao = col_double(),
    proficiencia_media = col_double(),
    pct_defasagem = col_double(),
    pct_intermediario = col_double(),
    pct_adequado = col_double(),
    painel_balanceado = col_logical(),
    .default = col_character()
  )
)

variacao <- read_csv(
  arquivo_variacao,
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
    delta_previstos = col_double(),
    delta_avaliados = col_double(),
    delta_participacao = col_double(),
    delta_proficiencia = col_double(),
    delta_pct_defasagem = col_double(),
    delta_pct_intermediario = col_double(),
    delta_pct_adequado = col_double(),
    .default = col_character()
  )
)

# -------------------------------------------------------------------
# 4. Indicadores adicionais de qualidade
# -------------------------------------------------------------------

variacao_qualidade <- variacao |>
  mutate(
    variacao_relativa_previstos = case_when(
      !is.na(previstos_2025) &
        previstos_2025 > 0 &
        !is.na(previstos_2026) ~
        100 * (
          previstos_2026 - previstos_2025
        ) / previstos_2025,
      
      TRUE ~ NA_real_
    ),
    
    variacao_relativa_avaliados = case_when(
      !is.na(avaliados_2025) &
        avaliados_2025 > 0 &
        !is.na(avaliados_2026) ~
        100 * (
          avaliados_2026 - avaliados_2025
        ) / avaliados_2025,
      
      TRUE ~ NA_real_
    ),
    
    participacao_minima_70_ambos = case_when(
      painel_balanceado &
        taxa_participacao_2025 >= 70 &
        taxa_participacao_2026 >= 70 ~ TRUE,
      
      painel_balanceado ~ FALSE,
      
      TRUE ~ NA
    ),
    
    participacao_minima_80_ambos = case_when(
      painel_balanceado &
        taxa_participacao_2025 >= 80 &
        taxa_participacao_2026 >= 80 ~ TRUE,
      
      painel_balanceado ~ FALSE,
      
      TRUE ~ NA
    ),
    
    participacao_minima_90_ambos = case_when(
      painel_balanceado &
        taxa_participacao_2025 >= 90 &
        taxa_participacao_2026 >= 90 ~ TRUE,
      
      painel_balanceado ~ FALSE,
      
      TRUE ~ NA
    ),
    
    aumento_participacao_10pp = case_when(
      is.na(delta_participacao) ~ NA,
      delta_participacao >= 10 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    queda_participacao_10pp = case_when(
      is.na(delta_participacao) ~ NA,
      delta_participacao <= -10 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    mudanca_previstos_20pct = case_when(
      is.na(variacao_relativa_previstos) ~ NA,
      abs(variacao_relativa_previstos) >= 20 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    turma_escola_pequena_2025 = case_when(
      is.na(avaliados_2025) ~ NA,
      avaliados_2025 < 20 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    turma_escola_pequena_2026 = case_when(
      is.na(avaliados_2026) ~ NA,
      avaliados_2026 < 20 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    observacao_composicao = case_when(
      !painel_balanceado ~
        "Escola-série ausente em um dos anos",
      
      mudanca_previstos_20pct &
        aumento_participacao_10pp ~
        "Mudança relevante no público previsto e aumento de participação",
      
      mudanca_previstos_20pct &
        queda_participacao_10pp ~
        "Mudança relevante no público previsto e queda de participação",
      
      mudanca_previstos_20pct ~
        "Mudança relevante no número de estudantes previstos",
      
      aumento_participacao_10pp ~
        "Aumento de participação igual ou superior a 10 p.p.",
      
      queda_participacao_10pp ~
        "Queda de participação igual ou superior a 10 p.p.",
      
      turma_escola_pequena_2025 |
        turma_escola_pequena_2026 ~
        "Baixo número de avaliados em pelo menos um ano",
      
      TRUE ~
        "Sem alerta principal de composição"
    )
  )

# -------------------------------------------------------------------
# 5. Cobertura por ano e série
# -------------------------------------------------------------------

cobertura_rede <- painel |>
  group_by(
    ano,
    ano_escolar,
    componente
  ) |>
  summarise(
    numero_escolas = n_distinct(id_escola),
    
    numero_turmas = sum(
      numero_turmas,
      na.rm = TRUE
    ),
    
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
    
    escolas_participacao_abaixo_70 = sum(
      taxa_participacao < 70,
      na.rm = TRUE
    ),
    
    escolas_participacao_70_79 = sum(
      taxa_participacao >= 70 &
        taxa_participacao < 80,
      na.rm = TRUE
    ),
    
    escolas_participacao_80_89 = sum(
      taxa_participacao >= 80 &
        taxa_participacao < 90,
      na.rm = TRUE
    ),
    
    escolas_participacao_90_mais = sum(
      taxa_participacao >= 90,
      na.rm = TRUE
    ),
    
    participacao_mediana_escolas =
      mediana_segura(taxa_participacao),
    
    participacao_p25_escolas =
      quantil_seguro(
        taxa_participacao,
        0.25
      ),
    
    participacao_p75_escolas =
      quantil_seguro(
        taxa_participacao,
        0.75
      ),
    
    .groups = "drop"
  ) |>
  arrange(
    ano_escolar,
    ano
  )

# -------------------------------------------------------------------
# 6. Resumo das mudanças de composição
# -------------------------------------------------------------------

resumo_composicao <- variacao_qualidade |>
  group_by(
    ano_escolar,
    componente
  ) |>
  summarise(
    escolas_unicas = n_distinct(id_escola),
    
    painel_presenca_balanceado = sum(
      painel_balanceado,
      na.rm = TRUE
    ),
    
    painel_resultado_balanceado = sum(
      painel_resultado_balanceado,
      na.rm = TRUE
    ),
    
    participacao_70_ambos = sum(
      participacao_minima_70_ambos,
      na.rm = TRUE
    ),
    
    participacao_80_ambos = sum(
      participacao_minima_80_ambos,
      na.rm = TRUE
    ),
    
    participacao_90_ambos = sum(
      participacao_minima_90_ambos,
      na.rm = TRUE
    ),
    
    aumento_participacao_10pp = sum(
      aumento_participacao_10pp,
      na.rm = TRUE
    ),
    
    queda_participacao_10pp = sum(
      queda_participacao_10pp,
      na.rm = TRUE
    ),
    
    mudanca_previstos_20pct = sum(
      mudanca_previstos_20pct,
      na.rm = TRUE
    ),
    
    baixo_n_avaliados = sum(
      turma_escola_pequena_2025 |
        turma_escola_pequena_2026,
      na.rm = TRUE
    ),
    
    mediana_delta_participacao =
      mediana_segura(delta_participacao),
    
    p25_delta_participacao =
      quantil_seguro(
        delta_participacao,
        0.25
      ),
    
    p75_delta_participacao =
      quantil_seguro(
        delta_participacao,
        0.75
      ),
    
    mediana_variacao_previstos =
      mediana_segura(
        variacao_relativa_previstos
      ),
    
    .groups = "drop"
  ) |>
  arrange(ano_escolar)

# -------------------------------------------------------------------
# 7. Sensibilidade do desempenho à participação
# -------------------------------------------------------------------

resumo_desempenho_sensibilidade <- bind_rows(
  
  variacao_qualidade |>
    filter(
      painel_resultado_balanceado
    ) |>
    group_by(
      ano_escolar,
      componente
    ) |>
    summarise(
      amostra = "Todas as escolas com resultado nos dois anos",
      numero_escolas = n(),
      
      delta_proficiencia_media_simples =
        mean(
          delta_proficiencia,
          na.rm = TRUE
        ),
      
      delta_proficiencia_mediana =
        mediana_segura(
          delta_proficiencia
        ),
      
      delta_adequado_media_simples =
        mean(
          delta_pct_adequado,
          na.rm = TRUE
        ),
      
      delta_defasagem_media_simples =
        mean(
          delta_pct_defasagem,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    ),
  
  variacao_qualidade |>
    filter(
      painel_resultado_balanceado,
      participacao_minima_70_ambos
    ) |>
    group_by(
      ano_escolar,
      componente
    ) |>
    summarise(
      amostra = "Participação mínima de 70% nos dois anos",
      numero_escolas = n(),
      
      delta_proficiencia_media_simples =
        mean(
          delta_proficiencia,
          na.rm = TRUE
        ),
      
      delta_proficiencia_mediana =
        mediana_segura(
          delta_proficiencia
        ),
      
      delta_adequado_media_simples =
        mean(
          delta_pct_adequado,
          na.rm = TRUE
        ),
      
      delta_defasagem_media_simples =
        mean(
          delta_pct_defasagem,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    ),
  
  variacao_qualidade |>
    filter(
      painel_resultado_balanceado,
      participacao_minima_80_ambos
    ) |>
    group_by(
      ano_escolar,
      componente
    ) |>
    summarise(
      amostra = "Participação mínima de 80% nos dois anos",
      numero_escolas = n(),
      
      delta_proficiencia_media_simples =
        mean(
          delta_proficiencia,
          na.rm = TRUE
        ),
      
      delta_proficiencia_mediana =
        mediana_segura(
          delta_proficiencia
        ),
      
      delta_adequado_media_simples =
        mean(
          delta_pct_adequado,
          na.rm = TRUE
        ),
      
      delta_defasagem_media_simples =
        mean(
          delta_pct_defasagem,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    )
) |>
  arrange(
    ano_escolar,
    amostra
  )

# -------------------------------------------------------------------
# 8. Associação descritiva entre mudança de participação e desempenho
# -------------------------------------------------------------------

associacao_participacao_desempenho <- variacao_qualidade |>
  filter(
    painel_resultado_balanceado,
    !is.na(delta_participacao),
    !is.na(delta_proficiencia)
  ) |>
  group_by(
    ano_escolar,
    componente
  ) |>
  summarise(
    numero_escolas = n(),
    
    correlacao_delta_participacao_proficiencia =
      if_else(
        n() >= 3,
        cor(
          delta_participacao,
          delta_proficiencia,
          use = "complete.obs"
        ),
        NA_real_
      ),
    
    correlacao_delta_participacao_adequado =
      if_else(
        n() >= 3,
        cor(
          delta_participacao,
          delta_pct_adequado,
          use = "complete.obs"
        ),
        NA_real_
      ),
    
    .groups = "drop"
  )

# -------------------------------------------------------------------
# 9. Lista de casos para revisão
# -------------------------------------------------------------------

casos_revisao <- variacao_qualidade |>
  filter(
    !painel_balanceado |
      mudanca_previstos_20pct |
      aumento_participacao_10pp |
      queda_participacao_10pp |
      turma_escola_pequena_2025 |
      turma_escola_pequena_2026
  ) |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora,
    ano_escolar,
    componente,
    presente_2025,
    presente_2026,
    painel_balanceado,
    painel_resultado_balanceado,
    previstos_2025,
    previstos_2026,
    variacao_relativa_previstos,
    avaliados_2025,
    avaliados_2026,
    taxa_participacao_2025,
    taxa_participacao_2026,
    delta_participacao,
    proficiencia_media_2025,
    proficiencia_media_2026,
    delta_proficiencia,
    observacao_composicao
  ) |>
  arrange(
    ano_escolar,
    desc(
      abs(
        coalesce(
          delta_participacao,
          0
        )
      )
    ),
    nome_canonico
  )

# -------------------------------------------------------------------
# 10. Diagnóstico especial da escola com possível municipalização
# -------------------------------------------------------------------

casos_municipalizacao <- variacao_qualidade |>
  filter(
    str_detect(
      str_to_upper(
        coalesce(
          nome_canonico,
          ""
        )
      ),
      "ALDEIA LUMIAR"
    )
  )

# -------------------------------------------------------------------
# 11. Exportação
# -------------------------------------------------------------------

write_csv(
  variacao_qualidade,
  here(
    "dados_processados",
    "variacao_escola_serie_com_qualidade.csv"
  ),
  na = ""
)

write_csv(
  cobertura_rede,
  here(
    "resultados",
    "cobertura_rede_por_serie_ano.csv"
  ),
  na = ""
)

write_csv(
  resumo_composicao,
  here(
    "resultados",
    "resumo_composicao_painel.csv"
  ),
  na = ""
)

write_csv(
  resumo_desempenho_sensibilidade,
  here(
    "resultados",
    "sensibilidade_desempenho_participacao.csv"
  ),
  na = ""
)

write_csv(
  associacao_participacao_desempenho,
  here(
    "resultados",
    "associacao_participacao_desempenho.csv"
  ),
  na = ""
)

write_csv(
  casos_revisao,
  here(
    "documentacao",
    "casos_revisao_composicao.csv"
  ),
  na = ""
)

write_csv(
  casos_municipalizacao,
  here(
    "documentacao",
    "diagnostico_possivel_municipalizacao.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 12. Resumo no console
# -------------------------------------------------------------------

cat("\nMódulo de qualidade e composição concluído.\n")

cat("\nCobertura da avaliação por série e ano:\n")

print(
  cobertura_rede |>
    select(
      ano,
      ano_escolar,
      numero_escolas,
      previstos,
      avaliados,
      taxa_participacao,
      escolas_participacao_abaixo_70,
      escolas_participacao_90_mais
    )
)

cat("\nResumo da composição do painel:\n")

print(
  resumo_composicao |>
    select(
      ano_escolar,
      escolas_unicas,
      painel_presenca_balanceado,
      painel_resultado_balanceado,
      participacao_70_ambos,
      participacao_80_ambos,
      aumento_participacao_10pp,
      queda_participacao_10pp,
      mudanca_previstos_20pct
    )
)

cat("\nSensibilidade da variação do desempenho:\n")

print(
  resumo_desempenho_sensibilidade |>
    select(
      ano_escolar,
      amostra,
      numero_escolas,
      delta_proficiencia_media_simples,
      delta_proficiencia_mediana,
      delta_adequado_media_simples
    )
)

cat("\nArquivos gerados:\n")

cat(
  "- dados_processados/variacao_escola_serie_com_qualidade.csv\n",
  "- resultados/cobertura_rede_por_serie_ano.csv\n",
  "- resultados/resumo_composicao_painel.csv\n",
  "- resultados/sensibilidade_desempenho_participacao.csv\n",
  "- resultados/associacao_participacao_desempenho.csv\n",
  "- documentacao/casos_revisao_composicao.csv\n",
  "- documentacao/diagnostico_possivel_municipalizacao.csv\n"
)