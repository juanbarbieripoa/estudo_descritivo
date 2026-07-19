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

arquivo_fato <- here(
  "dados_processados",
  "fato_turma_avaliacao.csv"
)

if (!file.exists(arquivo_fato)) {
  stop(
    "Arquivo não encontrado: ",
    arquivo_fato,
    "\nExecute primeiro o script R/04_importar_caed.R."
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

desvio_padrao_seguro <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) < 2) {
    return(NA_real_)
  }
  
  sd(x)
}

coeficiente_variacao_seguro <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (
    length(x) < 2 ||
    mean(x) == 0
  ) {
    return(NA_real_)
  }
  
  100 * sd(x) / mean(x)
}

# -------------------------------------------------------------------
# 3. Leitura da tabela fato
# -------------------------------------------------------------------

fato_turma <- read_csv(
  arquivo_fato,
  show_col_types = FALSE,
  col_types = cols(
    ano = col_integer(),
    exposicao_programa = col_integer(),
    id_escola = col_character(),
    codigo_inep = col_character(),
    nome_canonico = col_character(),
    nome_escola_caed = col_character(),
    assessora = col_character(),
    ano_escolar = col_integer(),
    componente = col_character(),
    avaliacao = col_character(),
    codigo_turma = col_character(),
    turma = col_character(),
    previstos = col_double(),
    avaliados = col_double(),
    taxa_participacao_caed = col_double(),
    taxa_participacao_calculada = col_double(),
    proficiencia_media = col_double(),
    pct_defasagem = col_double(),
    pct_intermediario = col_double(),
    pct_adequado = col_double(),
    .default = col_character()
  )
)

# -------------------------------------------------------------------
# 4. Validações preliminares
# -------------------------------------------------------------------

if (any(is.na(fato_turma$id_escola))) {
  warning(
    "Há linhas sem id_escola na tabela fato. ",
    "Consulte documentacao/escolas_caed_sem_id.csv."
  )
}

chave_turma <- fato_turma |>
  count(
    ano,
    ano_escolar,
    id_escola,
    codigo_turma,
    name = "n"
  ) |>
  filter(n > 1)

write_csv(
  chave_turma,
  here(
    "documentacao",
    "duplicidades_chave_turma.csv"
  ),
  na = ""
)

if (nrow(chave_turma) > 0) {
  warning(
    nrow(chave_turma),
    " chaves de turma aparecem mais de uma vez. ",
    "Consulte documentacao/duplicidades_chave_turma.csv."
  )
}

# -------------------------------------------------------------------
# 5. Painel escola × série × ano
# -------------------------------------------------------------------

# Cria previamente os numeradores ponderados.
# Isso evita que os nomes criados em summarise() sobrescrevam
# as colunas originais usadas como pesos.

fato_turma_agregacao <- fato_turma |>
  mutate(
    num_proficiencia = if_else(
      !is.na(proficiencia_media) &
        !is.na(avaliados) &
        avaliados > 0,
      proficiencia_media * avaliados,
      0
    ),
    
    peso_proficiencia = if_else(
      !is.na(proficiencia_media) &
        !is.na(avaliados) &
        avaliados > 0,
      avaliados,
      0
    ),
    
    num_defasagem = if_else(
      !is.na(pct_defasagem) &
        !is.na(avaliados) &
        avaliados > 0,
      pct_defasagem * avaliados,
      0
    ),
    
    peso_defasagem = if_else(
      !is.na(pct_defasagem) &
        !is.na(avaliados) &
        avaliados > 0,
      avaliados,
      0
    ),
    
    num_intermediario = if_else(
      !is.na(pct_intermediario) &
        !is.na(avaliados) &
        avaliados > 0,
      pct_intermediario * avaliados,
      0
    ),
    
    peso_intermediario = if_else(
      !is.na(pct_intermediario) &
        !is.na(avaliados) &
        avaliados > 0,
      avaliados,
      0
    ),
    
    num_adequado = if_else(
      !is.na(pct_adequado) &
        !is.na(avaliados) &
        avaliados > 0,
      pct_adequado * avaliados,
      0
    ),
    
    peso_adequado = if_else(
      !is.na(pct_adequado) &
        !is.na(avaliados) &
        avaliados > 0,
      avaliados,
      0
    )
  )

painel_escola_serie_ano <- fato_turma_agregacao |>
  group_by(
    ano,
    periodo_programa,
    exposicao_programa,
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora,
    ano_escolar,
    componente,
    avaliacao
  ) |>
  summarise(
    numero_turmas = n_distinct(codigo_turma),
    
    previstos = sum(
      .data$previstos,
      na.rm = TRUE
    ),
    
    avaliados = sum(
      .data$avaliados,
      na.rm = TRUE
    ),
    
    soma_num_proficiencia = sum(
      num_proficiencia,
      na.rm = TRUE
    ),
    
    soma_peso_proficiencia = sum(
      peso_proficiencia,
      na.rm = TRUE
    ),
    
    soma_num_defasagem = sum(
      num_defasagem,
      na.rm = TRUE
    ),
    
    soma_peso_defasagem = sum(
      peso_defasagem,
      na.rm = TRUE
    ),
    
    soma_num_intermediario = sum(
      num_intermediario,
      na.rm = TRUE
    ),
    
    soma_peso_intermediario = sum(
      peso_intermediario,
      na.rm = TRUE
    ),
    
    soma_num_adequado = sum(
      num_adequado,
      na.rm = TRUE
    ),
    
    soma_peso_adequado = sum(
      peso_adequado,
      na.rm = TRUE
    ),
    
    proficiencia_min_turma = if_else(
      all(is.na(.data$proficiencia_media)),
      NA_real_,
      min(
        .data$proficiencia_media,
        na.rm = TRUE
      )
    ),
    
    proficiencia_max_turma = if_else(
      all(is.na(.data$proficiencia_media)),
      NA_real_,
      max(
        .data$proficiencia_media,
        na.rm = TRUE
      )
    ),
    
    desvio_proficiencia_entre_turmas =
      desvio_padrao_seguro(
        .data$proficiencia_media
      ),
    
    cv_proficiencia_entre_turmas =
      coeficiente_variacao_seguro(
        .data$proficiencia_media
      ),
    
    menor_participacao_turma = if_else(
      all(
        is.na(
          .data$taxa_participacao_calculada
        )
      ),
      NA_real_,
      min(
        .data$taxa_participacao_calculada,
        na.rm = TRUE
      )
    ),
    
    maior_participacao_turma = if_else(
      all(
        is.na(
          .data$taxa_participacao_calculada
        )
      ),
      NA_real_,
      max(
        .data$taxa_participacao_calculada,
        na.rm = TRUE
      )
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    taxa_participacao = if_else(
      previstos > 0,
      100 * avaliados / previstos,
      NA_real_
    ),
    
    proficiencia_media = if_else(
      soma_peso_proficiencia > 0,
      soma_num_proficiencia /
        soma_peso_proficiencia,
      NA_real_
    ),
    
    pct_defasagem = if_else(
      soma_peso_defasagem > 0,
      soma_num_defasagem /
        soma_peso_defasagem,
      NA_real_
    ),
    
    pct_intermediario = if_else(
      soma_peso_intermediario > 0,
      soma_num_intermediario /
        soma_peso_intermediario,
      NA_real_
    ),
    
    pct_adequado = if_else(
      soma_peso_adequado > 0,
      soma_num_adequado /
        soma_peso_adequado,
      NA_real_
    ),
    
    soma_niveis_aprendizagem =
      pct_defasagem +
      pct_intermediario +
      pct_adequado,
    
    diferenca_soma_100 =
      abs(soma_niveis_aprendizagem - 100),
    
    participacao_70 = case_when(
      is.na(taxa_participacao) ~ NA,
      taxa_participacao >= 70 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    participacao_80 = case_when(
      is.na(taxa_participacao) ~ NA,
      taxa_participacao >= 80 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    participacao_90 = case_when(
      is.na(taxa_participacao) ~ NA,
      taxa_participacao >= 90 ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  select(
    -starts_with("soma_num_"),
    -starts_with("soma_peso_")
  ) |>
  arrange(
    id_escola,
    ano_escolar,
    ano
  )



# -------------------------------------------------------------------
# 6. Presença da escola-série nos dois anos
# -------------------------------------------------------------------

presenca_painel <- painel_escola_serie_ano |>
  distinct(
    id_escola,
    ano_escolar,
    componente,
    ano
  ) |>
  mutate(
    presente = TRUE,
    coluna_ano = paste0("presente_", ano)
  ) |>
  select(
    -ano
  ) |>
  pivot_wider(
    names_from = coluna_ano,
    values_from = presente,
    values_fill = FALSE
  ) |>
  mutate(
    painel_balanceado = presente_2025 &
      presente_2026
  )

painel_escola_serie_ano <- painel_escola_serie_ano |>
  left_join(
    presenca_painel,
    by = c(
      "id_escola",
      "ano_escolar",
      "componente"
    )
  )

primeiro_nao_vazio <- function(x) {
  
  x <- as.character(x)
  
  x <- x[
    !is.na(x) &
      stringr::str_squish(x) != ""
  ]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  x[[1]]
}

validacao_proficiencia_escola_serie <-
  painel_escola_serie_ano |>
  group_by(
    ano,
    ano_escolar
  ) |>
  summarise(
    numero_escolas = n_distinct(id_escola),
    
    escolas_com_proficiencia = sum(
      !is.na(proficiencia_media)
    ),
    
    escolas_sem_proficiencia = sum(
      is.na(proficiencia_media)
    ),
    
    .groups = "drop"
  )

cat(
  "\nValidação da proficiência por escola-série:\n"
)

print(
  validacao_proficiencia_escola_serie,
  n = Inf
)

# -------------------------------------------------------------------
# 7. Painel de variações 2025–2026
# -------------------------------------------------------------------

# Atributos cadastrais são separados da chave longitudinal.
# O pivot usa somente id_escola, ano_escolar e componente como chave.

atributos_escola <- painel_escola_serie_ano |>
  group_by(id_escola) |>
  summarise(
    codigo_inep = primeiro_nao_vazio(codigo_inep),
    nome_canonico = primeiro_nao_vazio(nome_canonico),
    assessora = primeiro_nao_vazio(assessora),
    .groups = "drop"
  )

variacao_escola_serie <- painel_escola_serie_ano |>
  filter(
    ano %in% c(2025L, 2026L)
  ) |>
  select(
    ano,
    id_escola,
    ano_escolar,
    componente,
    numero_turmas,
    previstos,
    avaliados,
    taxa_participacao,
    proficiencia_media,
    pct_defasagem,
    pct_intermediario,
    pct_adequado
  ) |>
  pivot_wider(
    id_cols = c(
      id_escola,
      ano_escolar,
      componente
    ),
    names_from = ano,
    values_from = c(
      numero_turmas,
      previstos,
      avaliados,
      taxa_participacao,
      proficiencia_media,
      pct_defasagem,
      pct_intermediario,
      pct_adequado
    ),
    names_glue = "{.value}_{ano}"
  ) |>
  left_join(
    atributos_escola,
    by = "id_escola"
  ) |>
  mutate(
    presente_2025 =
      !is.na(numero_turmas_2025),
    
    presente_2026 =
      !is.na(numero_turmas_2026),
    
    painel_balanceado =
      presente_2025 & presente_2026,
    
    resultado_disponivel_2025 =
      !is.na(proficiencia_media_2025),
    
    resultado_disponivel_2026 =
      !is.na(proficiencia_media_2026),
    
    painel_resultado_balanceado =
      resultado_disponivel_2025 &
      resultado_disponivel_2026,
    
    delta_numero_turmas = if_else(
      painel_balanceado,
      numero_turmas_2026 -
        numero_turmas_2025,
      NA_real_
    ),
    
    delta_previstos = if_else(
      painel_balanceado,
      previstos_2026 -
        previstos_2025,
      NA_real_
    ),
    
    delta_avaliados = if_else(
      painel_balanceado,
      avaliados_2026 -
        avaliados_2025,
      NA_real_
    ),
    
    delta_participacao = if_else(
      painel_balanceado,
      taxa_participacao_2026 -
        taxa_participacao_2025,
      NA_real_
    ),
    
    delta_proficiencia = if_else(
      painel_resultado_balanceado,
      proficiencia_media_2026 -
        proficiencia_media_2025,
      NA_real_
    ),
    
    delta_pct_defasagem = if_else(
      painel_resultado_balanceado,
      pct_defasagem_2026 -
        pct_defasagem_2025,
      NA_real_
    ),
    
    delta_pct_intermediario = if_else(
      painel_resultado_balanceado,
      pct_intermediario_2026 -
        pct_intermediario_2025,
      NA_real_
    ),
    
    delta_pct_adequado = if_else(
      painel_resultado_balanceado,
      pct_adequado_2026 -
        pct_adequado_2025,
      NA_real_
    )
  ) |>
  relocate(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora,
    ano_escolar,
    componente
  ) |>
  arrange(
    ano_escolar,
    nome_canonico
  )

# -------------------------------------------------------------------
# 8. Agregado da rede por série e ano
# -------------------------------------------------------------------

painel_rede_serie_ano <- fato_turma |>
  group_by(
    ano,
    periodo_programa,
    exposicao_programa,
    ano_escolar,
    componente,
    avaliacao
  ) |>
  summarise(
    numero_escolas = n_distinct(
      id_escola,
      na.rm = TRUE
    ),
    
    numero_turmas = n_distinct(
      interaction(
        id_escola,
        codigo_turma,
        drop = TRUE
      )
    ),
    
    total_previstos = sum(
      .data$previstos,
      na.rm = TRUE
    ),
    
    total_avaliados = sum(
      .data$avaliados,
      na.rm = TRUE
    ),
    
    numerador_proficiencia = sum(
      .data$proficiencia_media *
        .data$avaliados,
      na.rm = TRUE
    ),
    
    peso_proficiencia = sum(
      if_else(
        !is.na(.data$proficiencia_media),
        .data$avaliados,
        0
      ),
      na.rm = TRUE
    ),
    
    numerador_defasagem = sum(
      .data$pct_defasagem *
        .data$avaliados,
      na.rm = TRUE
    ),
    
    peso_defasagem = sum(
      if_else(
        !is.na(.data$pct_defasagem),
        .data$avaliados,
        0
      ),
      na.rm = TRUE
    ),
    
    numerador_intermediario = sum(
      .data$pct_intermediario *
        .data$avaliados,
      na.rm = TRUE
    ),
    
    peso_intermediario = sum(
      if_else(
        !is.na(.data$pct_intermediario),
        .data$avaliados,
        0
      ),
      na.rm = TRUE
    ),
    
    numerador_adequado = sum(
      .data$pct_adequado *
        .data$avaliados,
      na.rm = TRUE
    ),
    
    peso_adequado = sum(
      if_else(
        !is.na(.data$pct_adequado),
        .data$avaliados,
        0
      ),
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    previstos = total_previstos,
    avaliados = total_avaliados,
    
    taxa_participacao = if_else(
      previstos > 0,
      100 * avaliados / previstos,
      NA_real_
    ),
    
    proficiencia_media = if_else(
      peso_proficiencia > 0,
      numerador_proficiencia /
        peso_proficiencia,
      NA_real_
    ),
    
    pct_defasagem = if_else(
      peso_defasagem > 0,
      numerador_defasagem /
        peso_defasagem,
      NA_real_
    ),
    
    pct_intermediario = if_else(
      peso_intermediario > 0,
      numerador_intermediario /
        peso_intermediario,
      NA_real_
    ),
    
    pct_adequado = if_else(
      peso_adequado > 0,
      numerador_adequado /
        peso_adequado,
      NA_real_
    )
  ) |>
  select(
    ano,
    periodo_programa,
    exposicao_programa,
    ano_escolar,
    componente,
    avaliacao,
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
# 9. Variação da rede por série
# -------------------------------------------------------------------

variacao_rede_serie <- painel_rede_serie_ano |>
  select(
    ano,
    ano_escolar,
    componente,
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
  pivot_wider(
    names_from = ano,
    values_from = c(
      numero_escolas,
      numero_turmas,
      previstos,
      avaliados,
      taxa_participacao,
      proficiencia_media,
      pct_defasagem,
      pct_intermediario,
      pct_adequado
    ),
    names_glue = "{.value}_{ano}"
  ) |>
  mutate(
    delta_participacao =
      taxa_participacao_2026 -
      taxa_participacao_2025,
    
    delta_proficiencia =
      proficiencia_media_2026 -
      proficiencia_media_2025,
    
    delta_pct_defasagem =
      pct_defasagem_2026 -
      pct_defasagem_2025,
    
    delta_pct_intermediario =
      pct_intermediario_2026 -
      pct_intermediario_2025,
    
    delta_pct_adequado =
      pct_adequado_2026 -
      pct_adequado_2025
  ) |>
  arrange(ano_escolar)

# -------------------------------------------------------------------
# 10. Validações
# -------------------------------------------------------------------

validacao_painel <- painel_escola_serie_ano |>
  summarise(
    numero_linhas = n(),
    
    numero_escolas = n_distinct(
      id_escola
    ),
    
    escolas_sem_id = sum(
      is.na(id_escola)
    ),
    
    participacao_acima_100 = sum(
      taxa_participacao > 100,
      na.rm = TRUE
    ),
    
    participacao_abaixo_zero = sum(
      taxa_participacao < 0,
      na.rm = TRUE
    ),
    
    proficiencia_ausente = sum(
      is.na(proficiencia_media)
    ),
    
    niveis_fora_intervalo = sum(
      pct_defasagem < 0 |
        pct_defasagem > 100 |
        pct_intermediario < 0 |
        pct_intermediario > 100 |
        pct_adequado < 0 |
        pct_adequado > 100,
      na.rm = TRUE
    ),
    
    soma_niveis_diverge_mais_1pp = sum(
      diferenca_soma_100 > 1,
      na.rm = TRUE
    )
  )

painel_desbalanceado <- variacao_escola_serie |>
  filter(!painel_balanceado) |>
  select(
    id_escola,
    nome_canonico,
    assessora,
    ano_escolar,
    componente,
    presente_2025,
    presente_2026,
    proficiencia_media_2025,
    proficiencia_media_2026,
    previstos_2025,
    previstos_2026
  )

# -------------------------------------------------------------------
# 11. Exportação
# -------------------------------------------------------------------

write_csv(
  painel_escola_serie_ano,
  here(
    "dados_processados",
    "painel_escola_serie_ano.csv"
  ),
  na = ""
)

write_csv(
  variacao_escola_serie,
  here(
    "dados_processados",
    "variacao_escola_serie_2025_2026.csv"
  ),
  na = ""
)

write_csv(
  painel_rede_serie_ano,
  here(
    "dados_processados",
    "painel_rede_serie_ano.csv"
  ),
  na = ""
)

validacao_agregado_rede <- painel_rede_serie_ano |>
  summarise(
    linhas = n(),
    
    proficiencia_ausente = sum(
      is.na(proficiencia_media)
    ),
    
    adequado_ausente = sum(
      is.na(pct_adequado)
    ),
    
    defasagem_ausente = sum(
      is.na(pct_defasagem)
    ),
    
    participacao_ausente = sum(
      is.na(taxa_participacao)
    )
  )

cat("\nValidação do agregado da rede:\n")
print(validacao_agregado_rede)

write_csv(
  variacao_rede_serie,
  here(
    "dados_processados",
    "variacao_rede_serie_2025_2026.csv"
  ),
  na = ""
)

write_csv(
  validacao_painel,
  here(
    "documentacao",
    "validacao_painel_escola_serie_ano.csv"
  ),
  na = ""
)

write_csv(
  painel_desbalanceado,
  here(
    "documentacao",
    "painel_escola_serie_desbalanceado.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 12. Resumo no console
# -------------------------------------------------------------------

cat("\nPainel escola-série-ano criado.\n\n")

cat("Validação geral:\n")
print(validacao_painel)

cat("\nCobertura do painel por série:\n")

print(
  variacao_escola_serie |>
    group_by(ano_escolar) |>
    summarise(
      escolas_unicas = n_distinct(id_escola),
      
      presentes_2025 = sum(
        presente_2025,
        na.rm = TRUE
      ),
      
      presentes_2026 = sum(
        presente_2026,
        na.rm = TRUE
      ),
      
      painel_presenca_balanceado = sum(
        painel_balanceado,
        na.rm = TRUE
      ),
      
      painel_resultado_balanceado = sum(
        painel_resultado_balanceado,
        na.rm = TRUE
      ),
      
      escolas_desbalanceadas = sum(
        !painel_balanceado,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    )
)

cat("\nVariação agregada da rede por série:\n")

print(
  variacao_rede_serie |>
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

cat("\nArquivos gerados:\n")

cat(
  "- dados_processados/painel_escola_serie_ano.csv\n",
  "- dados_processados/variacao_escola_serie_2025_2026.csv\n",
  "- dados_processados/painel_rede_serie_ano.csv\n",
  "- dados_processados/variacao_rede_serie_2025_2026.csv\n",
  "- documentacao/validacao_painel_escola_serie_ano.csv\n",
  "- documentacao/painel_escola_serie_desbalanceado.csv\n"
)