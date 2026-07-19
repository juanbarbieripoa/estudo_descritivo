library(here)
library(tidyverse)
library(data.table)

# -------------------------------------------------------------------
# 1. Diretórios e arquivos
# -------------------------------------------------------------------

dir.create(
  here("dados_processados"),
  recursive = TRUE,
  showWarnings = FALSE
)

caminho_caed <- here("dados_brutos")

arquivo_mapa_escolas <- here(
  "dados_intermediarios",
  "mapa_nomes_escolas.csv"
)

arquivo_dim_escola <- here(
  "dados_intermediarios",
  "dim_escola.csv"
)

if (!file.exists(arquivo_mapa_escolas)) {
  stop(
    "Execute primeiro R/03_consolidar_dim_escola.R."
  )
}

arquivos_caed <- list.files(
  path = caminho_caed,
  pattern = paste0(
    "^20(25|26)_1av_form_",
    "HABILIDADES_DESEMPENHO_TURMA_",
    "[0-9]{2}anos_LP\\.csv$"
  ),
  full.names = TRUE,
  recursive = TRUE
)

if (length(arquivos_caed) != 10) {
  warning(
    "Foram encontrados ",
    length(arquivos_caed),
    " arquivos CAEd; eram esperados 10."
  )
}

# -------------------------------------------------------------------
# 2. Funções auxiliares
# -------------------------------------------------------------------

normalizar_nome <- function(x) {
  x |>
    as.character() |>
    iconv(from = "", to = "ASCII//TRANSLIT") |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9 ]", " ") |>
    str_squish()
}

remover_prefixo_numerico <- function(x) {
  x |>
    str_remove("^\\d{4,8}\\s+") |>
    str_squish()
}

extrair_ano <- function(arquivo) {
  basename(arquivo) |>
    str_extract("^20\\d{2}") |>
    as.integer()
}

extrair_ano_escolar <- function(arquivo) {
  resultado <- str_match(
    basename(arquivo),
    "_([0-9]{2})anos_"
  )
  
  as.integer(resultado[, 2])
}

converter_numero <- function(x) {
  x |>
    as.character() |>
    str_replace_all("%", "") |>
    str_replace_all("\\s+", "") |>
    na_if("-") |>
    na_if("") |>
    readr::parse_number(
      locale = locale(decimal_mark = ","),
      na = c("", "NA", "N/A", "-")
    )
}

ler_caed <- function(arquivo) {
  
  fread(
    arquivo,
    encoding = "UTF-8",
    colClasses = "character",
    na.strings = c("", "NA", "N/A")
  ) |>
    as_tibble() |>
    mutate(
      ano = extrair_ano(arquivo),
      ano_escolar = extrair_ano_escolar(arquivo),
      componente = "Língua Portuguesa",
      avaliacao = "1ª Avaliação Formativa",
      arquivo_origem = basename(arquivo)
    )
}

# -------------------------------------------------------------------
# 3. Mapa de nomes
# -------------------------------------------------------------------

mapa_escolas <- read_csv(
  arquivo_mapa_escolas,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

if (
  !"id_escola" %in% names(mapa_escolas) &&
  "id_escola_provisorio" %in% names(mapa_escolas)
) {
  mapa_escolas <- mapa_escolas |>
    rename(
      id_escola = id_escola_provisorio
    )
}

if (!"id_escola" %in% names(mapa_escolas)) {
  stop(
    "O arquivo mapa_nomes_escolas.csv não contém ",
    "'id_escola' nem 'id_escola_provisorio'.\n",
    "Colunas disponíveis:\n",
    paste(names(mapa_escolas), collapse = "\n")
  )
}

mapa_escolas <- mapa_escolas |>
  filter(fonte == "CAEd") |>
  distinct(
    nome_original,
    id_escola,
    nome_canonico,
    codigo_inep
  )

dim_escola <- read_csv(
  arquivo_dim_escola,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  select(
    id_escola,
    assessora,
    possivel_municipalizacao_recente,
    status_rede_2025
  )

# -------------------------------------------------------------------
# 4. Importação conjunta
# -------------------------------------------------------------------

caed_bruto <- map_dfr(
  arquivos_caed,
  ler_caed
)

# -------------------------------------------------------------------
# 5. Identificação das colunas
# -------------------------------------------------------------------

colunas_habilidade <- names(caed_bruto) |>
  str_subset("^H\\s*[0-9]+\\s*\\(%\\)$")

colunas_identificadoras <- c(
  "Estado",
  "Regional",
  "Município",
  "Escola",
  "Código da Turma",
  "Turma",
  "ano",
  "ano_escolar",
  "componente",
  "avaliacao",
  "arquivo_origem"
)

colunas_identificadoras <- intersect(
  colunas_identificadoras,
  names(caed_bruto)
)

colunas_resultado_esperadas <- c(
  "Previstos",
  "Avaliados",
  "Avaliados (%)",
  "Proficiência Média",
  "Defasagem",
  "Aprendizado intermediário",
  "Aprendizado adequado"
)

colunas_resultado <- intersect(
  colunas_resultado_esperadas,
  names(caed_bruto)
)

# -------------------------------------------------------------------
# 6. Resultado por turma
# -------------------------------------------------------------------

fato_turma_avaliacao <- caed_bruto |>
  select(
    all_of(colunas_identificadoras),
    all_of(colunas_resultado)
  ) |>
  rename(
    nome_escola_caed = Escola,
    codigo_turma = `Código da Turma`,
    turma = Turma,
    previstos = Previstos,
    avaliados = Avaliados,
    taxa_participacao_caed = `Avaliados (%)`,
    proficiencia_media = `Proficiência Média`,
    pct_defasagem = Defasagem,
    pct_intermediario = `Aprendizado intermediário`,
    pct_adequado = `Aprendizado adequado`
  ) |>
  mutate(
    nome_sem_codigo = remover_prefixo_numerico(
      nome_escola_caed
    ),
    chave_nome_caed = normalizar_nome(
      nome_sem_codigo
    ),
    across(
      c(
        previstos,
        avaliados,
        taxa_participacao_caed,
        proficiencia_media,
        pct_defasagem,
        pct_intermediario,
        pct_adequado
      ),
      converter_numero
    ),
    previstos = as.integer(previstos),
    avaliados = as.integer(avaliados),
    taxa_participacao_calculada = if_else(
      previstos > 0,
      100 * avaliados / previstos,
      NA_real_
    ),
    exposicao_programa = case_when(
      ano == 2025L ~ 0L,
      ano == 2026L ~ 1L,
      TRUE ~ NA_integer_
    ),
    periodo_programa = case_when(
      ano == 2025L ~ "linha_base_pre_programa",
      ano == 2026L ~ "pos_inicio_programa",
      TRUE ~ NA_character_
    )
  ) |>
  left_join(
    mapa_escolas,
    by = c(
      "nome_escola_caed" = "nome_original"
    )
  ) |>
  left_join(
    dim_escola,
    by = "id_escola"
  ) |>
  relocate(
    ano,
    periodo_programa,
    exposicao_programa,
    id_escola,
    codigo_inep,
    nome_canonico,
    nome_escola_caed,
    assessora,
    ano_escolar,
    componente,
    avaliacao,
    codigo_turma,
    turma
  )

# -------------------------------------------------------------------
# 7. Diagnóstico de escolas sem chave
# -------------------------------------------------------------------

escolas_sem_chave <- fato_turma_avaliacao |>
  filter(is.na(id_escola)) |>
  distinct(
    ano,
    ano_escolar,
    nome_escola_caed,
    arquivo_origem
  )

write_csv(
  escolas_sem_chave,
  here(
    "documentacao",
    "escolas_caed_sem_id.csv"
  ),
  na = ""
)

if (nrow(escolas_sem_chave) > 0) {
  warning(
    nrow(escolas_sem_chave),
    " combinações de escola/arquivo ficaram sem id_escola."
  )
}

# -------------------------------------------------------------------
# 8. Habilidades em formato longo
# -------------------------------------------------------------------

fato_habilidade_turma <- caed_bruto |>
  select(
    ano,
    ano_escolar,
    Escola,
    `Código da Turma`,
    Turma,
    componente,
    avaliacao,
    arquivo_origem,
    all_of(colunas_habilidade)
  ) |>
  pivot_longer(
    cols = all_of(colunas_habilidade),
    names_to = "habilidade_original",
    values_to = "percentual_acerto_original"
  ) |>
  mutate(
    codigo_habilidade = habilidade_original |>
      str_extract("\\d+") |>
      as.integer(),
    
    id_habilidade_provisorio = sprintf(
      "LP_%02d_H%02d",
      ano_escolar,
      codigo_habilidade
    ),
    
    percentual_acerto = converter_numero(
      percentual_acerto_original
    )
  ) |>
  rename(
    nome_escola_caed = Escola,
    codigo_turma = `Código da Turma`,
    turma = Turma
  ) |>
  left_join(
    mapa_escolas,
    by = c(
      "nome_escola_caed" = "nome_original"
    )
  ) |>
  select(
    ano,
    id_escola,
    codigo_inep,
    nome_canonico,
    nome_escola_caed,
    ano_escolar,
    componente,
    avaliacao,
    codigo_turma,
    turma,
    id_habilidade_provisorio,
    codigo_habilidade,
    habilidade_original,
    percentual_acerto,
    percentual_acerto_original,
    arquivo_origem
  ) |>
  arrange(
    ano,
    id_escola,
    ano_escolar,
    codigo_turma,
    codigo_habilidade
  )

# -------------------------------------------------------------------
# 9. Validações
# -------------------------------------------------------------------

validacao_turmas <- fato_turma_avaliacao |>
  summarise(
    numero_linhas = n(),
    turmas_distintas = n_distinct(
      ano,
      ano_escolar,
      codigo_turma,
      nome_escola_caed
    ),
    escolas_sem_id = sum(
      is.na(id_escola)
    ),
    previstos_negativos = sum(
      previstos < 0,
      na.rm = TRUE
    ),
    avaliados_negativos = sum(
      avaliados < 0,
      na.rm = TRUE
    ),
    participacao_acima_100 = sum(
      taxa_participacao_calculada > 100,
      na.rm = TRUE
    ),
    percentuais_fora_intervalo = sum(
      pct_defasagem < 0 |
        pct_defasagem > 100 |
        pct_intermediario < 0 |
        pct_intermediario > 100 |
        pct_adequado < 0 |
        pct_adequado > 100,
      na.rm = TRUE
    )
  )

validacao_soma_niveis <- fato_turma_avaliacao |>
  mutate(
    soma_niveis = pct_defasagem +
      pct_intermediario +
      pct_adequado,
    diferenca_100 = abs(soma_niveis - 100)
  ) |>
  filter(
    !is.na(diferenca_100),
    diferenca_100 > 1
  ) |>
  select(
    ano,
    id_escola,
    nome_canonico,
    ano_escolar,
    codigo_turma,
    pct_defasagem,
    pct_intermediario,
    pct_adequado,
    soma_niveis,
    diferenca_100
  )

# -------------------------------------------------------------------
# 10. Exportação
# -------------------------------------------------------------------

write_csv(
  fato_turma_avaliacao,
  here(
    "dados_processados",
    "fato_turma_avaliacao.csv"
  ),
  na = ""
)

write_csv(
  fato_habilidade_turma,
  here(
    "dados_processados",
    "fato_habilidade_turma.csv"
  ),
  na = ""
)

write_csv(
  validacao_turmas,
  here(
    "documentacao",
    "validacao_fato_turma.csv"
  ),
  na = ""
)

write_csv(
  validacao_soma_niveis,
  here(
    "documentacao",
    "divergencias_soma_niveis_aprendizagem.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 11. Resumo
# -------------------------------------------------------------------

cat("\nMasterfiles produzidos:\n")

cat(
  "- dados_processados/fato_turma_avaliacao.csv\n",
  "- dados_processados/fato_habilidade_turma.csv\n"
)

cat("\nValidação geral:\n")

print(validacao_turmas)

cat("\nLinhas por ano e série:\n")

print(
  fato_turma_avaliacao |>
    count(
      ano,
      ano_escolar,
      name = "numero_turmas"
    ) |>
    arrange(
      ano,
      ano_escolar
    )
)

cat("\nHabilidades por ano e série:\n")

print(
  fato_habilidade_turma |>
    distinct(
      ano,
      ano_escolar,
      codigo_habilidade
    ) |>
    count(
      ano,
      ano_escolar,
      name = "numero_habilidades"
    ) |>
    arrange(
      ano,
      ano_escolar
    )
)