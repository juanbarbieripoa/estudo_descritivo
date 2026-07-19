library(here)
library(tidyverse)
library(readxl)

# -------------------------------------------------------------------
# 1. Diretórios
# -------------------------------------------------------------------

dir.create(
  here("dados_processados"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("resultados", "contexto"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("documentacao"),
  recursive = TRUE,
  showWarnings = FALSE
)

# -------------------------------------------------------------------
# 2. Localização dos arquivos
# -------------------------------------------------------------------

localizar_arquivo <- function(nome_arquivo) {
  
  arquivos <- list.files(
    path = here("dados_brutos"),
    pattern = paste0("^", nome_arquivo, "$"),
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  if (length(arquivos) == 0) {
    stop(
      "Arquivo não encontrado: ",
      nome_arquivo
    )
  }
  
  if (length(arquivos) > 1) {
    stop(
      "Mais de um arquivo encontrado com o nome: ",
      nome_arquivo,
      "\n",
      paste(arquivos, collapse = "\n")
    )
  }
  
  arquivos
}

arquivo_contexto_escolas <- localizar_arquivo(
  "tab_dados_contextuais_escolas.xlsx"
)

arquivo_contexto_municipio <- localizar_arquivo(
  "tab_dados_contextuais_municipiopoa_md.xlsx"
)

arquivo_mapa_escolas <- here(
  "dados_intermediarios",
  "mapa_nomes_escolas.csv"
)

arquivo_dim_escola <- here(
  "dados_intermediarios",
  "dim_escola.csv"
)

if (
  !file.exists(arquivo_mapa_escolas) ||
  !file.exists(arquivo_dim_escola)
) {
  stop(
    "Execute primeiro os módulos de consolidação ",
    "da dimensão de escolas."
  )
}

# -------------------------------------------------------------------
# 3. Funções auxiliares
# -------------------------------------------------------------------

normalizar_nome <- function(x) {
  
  x |>
    as.character() |>
    iconv(
      from = "",
      to = "ASCII//TRANSLIT"
    ) |>
    str_to_upper() |>
    str_replace_all(
      "[^A-Z0-9 ]",
      " "
    ) |>
    str_squish()
}

remover_prefixo_numerico <- function(x) {
  
  x |>
    as.character() |>
    str_remove("^\\d{4,8}\\s+") |>
    str_squish()
}

converter_numero <- function(x) {
  
  x |>
    as.character() |>
    na_if("-") |>
    na_if("") |>
    readr::parse_number(
      locale = locale(
        decimal_mark = ","
      ),
      na = c(
        "",
        "-",
        "NA",
        "N/A"
      )
    )
}

extrair_percentual_perfil <- function(x) {
  
  x <- as.character(x)
  
  case_when(
    is.na(x) ~ NA_real_,
    str_detect(x, "^\\s*-%") ~ NA_real_,
    TRUE ~ converter_numero(
      str_extract(
        x,
        "\\d+[\\.,]?\\d*\\s*%"
      )
    )
  )
}

extrair_numero_estudantes <- function(x) {
  
  x <- as.character(x)
  
  case_when(
    is.na(x) ~ NA_real_,
    str_detect(x, "^\\s*-%") ~ NA_real_,
    TRUE ~ converter_numero(
      str_extract(
        x,
        "\\d+[\\.,]?\\d*\\s+estudantes?"
      )
    )
  )
}

# -------------------------------------------------------------------
# 4. Leitura dos arquivos
# -------------------------------------------------------------------

aba_escolas <- excel_sheets(
  arquivo_contexto_escolas
)[1]

aba_municipio <- excel_sheets(
  arquivo_contexto_municipio
)[1]

contexto_escolas_bruto <- read_excel(
  arquivo_contexto_escolas,
  sheet = aba_escolas,
  col_types = "text"
)

contexto_municipio_bruto <- read_excel(
  arquivo_contexto_municipio,
  sheet = aba_municipio,
  col_types = "text"
)

# -------------------------------------------------------------------
# 5. Mapa de escolas
# -------------------------------------------------------------------

mapa_escolas <- read_csv(
  arquivo_mapa_escolas,
  show_col_types = FALSE,
  col_types = cols(
    .default = col_character()
  )
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

mapa_contexto <- mapa_escolas |>
  filter(
    fonte == "Contexto CAEd"
  ) |>
  distinct(
    nome_original,
    id_escola,
    nome_canonico,
    codigo_inep
  )

# -------------------------------------------------------------------
# 6. Transformação das colunas de perfil
# -------------------------------------------------------------------

colunas_perfil <- names(
  contexto_escolas_bruto
) |>
  str_subset(" - PERFIL$")

perfil_escola_long <- contexto_escolas_bruto |>
  select(
    `AVALIAÇÃO`,
    REDE,
    `ANO ESCOLAR`,
    `COMPONENTE CURRICULAR`,
    ESCOLA,
    all_of(colunas_perfil)
  ) |>
  pivot_longer(
    cols = all_of(colunas_perfil),
    names_to = "variavel_original",
    values_to = "valor_original"
  ) |>
  mutate(
    dimensao = case_when(
      str_starts(
        variavel_original,
        "NSE "
      ) ~ "NSE",
      
      str_starts(
        variavel_original,
        "COR/RAÇA "
      ) ~ "Raça/cor",
      
      str_starts(
        variavel_original,
        "FEMININO"
      ) |
        str_starts(
          variavel_original,
          "MASCULINO"
        ) ~ "Sexo",
      
      TRUE ~ "Outra"
    ),
    
    categoria = variavel_original |>
      str_remove(" - PERFIL$") |>
      str_remove("^NSE ") |>
      str_remove("^COR/RAÇA ") |>
      str_to_sentence(),
    
    percentual = extrair_percentual_perfil(
      valor_original
    ),
    
    numero_estudantes = extrair_numero_estudantes(
      valor_original
    ),
    
    ano = 2026L,
    ano_escolar = 5L,
    componente = "Língua Portuguesa",
    
    nome_escola_contexto = ESCOLA
  ) |>
  left_join(
    mapa_contexto,
    by = c(
      "nome_escola_contexto" =
        "nome_original"
    )
  ) |>
  select(
    ano,
    ano_escolar,
    componente,
    id_escola,
    codigo_inep,
    nome_canonico,
    nome_escola_contexto,
    dimensao,
    categoria,
    percentual,
    numero_estudantes,
    valor_original,
    variavel_original
  )

# -------------------------------------------------------------------
# 7. Transformação das colunas de proficiência
# -------------------------------------------------------------------

colunas_proficiencia <- names(
  contexto_escolas_bruto
) |>
  str_subset(" - PROFICIÊNCIA$")

proficiencia_grupo_long <- contexto_escolas_bruto |>
  select(
    `AVALIAÇÃO`,
    REDE,
    `ANO ESCOLAR`,
    `COMPONENTE CURRICULAR`,
    ESCOLA,
    all_of(colunas_proficiencia)
  ) |>
  pivot_longer(
    cols = all_of(colunas_proficiencia),
    names_to = "variavel_original",
    values_to = "valor_original"
  ) |>
  mutate(
    dimensao = case_when(
      str_starts(
        variavel_original,
        "NSE "
      ) ~ "NSE",
      
      str_starts(
        variavel_original,
        "COR/RAÇA "
      ) ~ "Raça/cor",
      
      str_starts(
        variavel_original,
        "FEMININO"
      ) |
        str_starts(
          variavel_original,
          "MASCULINO"
        ) ~ "Sexo",
      
      TRUE ~ "Outra"
    ),
    
    categoria = variavel_original |>
      str_remove(
        " - PROFICIÊNCIA$"
      ) |>
      str_remove("^NSE ") |>
      str_remove("^COR/RAÇA ") |>
      str_to_sentence(),
    
    proficiencia_grupo = converter_numero(
      valor_original
    ),
    
    ano = 2026L,
    ano_escolar = 5L,
    componente = "Língua Portuguesa",
    
    nome_escola_contexto = ESCOLA
  ) |>
  left_join(
    mapa_contexto,
    by = c(
      "nome_escola_contexto" =
        "nome_original"
    )
  ) |>
  select(
    ano,
    ano_escolar,
    componente,
    id_escola,
    codigo_inep,
    nome_canonico,
    nome_escola_contexto,
    dimensao,
    categoria,
    proficiencia_grupo,
    valor_original,
    variavel_original
  )

# -------------------------------------------------------------------
# 8. Tabela contextual única
# -------------------------------------------------------------------

fato_contexto_caed <- perfil_escola_long |>
  full_join(
    proficiencia_grupo_long,
    by = c(
      "ano",
      "ano_escolar",
      "componente",
      "id_escola",
      "codigo_inep",
      "nome_canonico",
      "nome_escola_contexto",
      "dimensao",
      "categoria"
    ),
    suffix = c(
      "_perfil",
      "_proficiencia"
    )
  ) |>
  select(
    ano,
    ano_escolar,
    componente,
    id_escola,
    codigo_inep,
    nome_canonico,
    nome_escola_contexto,
    dimensao,
    categoria,
    percentual,
    numero_estudantes,
    proficiencia_grupo,
    valor_original_perfil,
    valor_original_proficiencia
  ) |>
  arrange(
    id_escola,
    dimensao,
    categoria
  )

# -------------------------------------------------------------------
# 9. Referência municipal
# -------------------------------------------------------------------

colunas_perfil_municipio <- names(
  contexto_municipio_bruto
) |>
  str_subset(" - PERFIL$")

colunas_proficiencia_municipio <- names(
  contexto_municipio_bruto
) |>
  str_subset(" - PROFICIÊNCIA$")

perfil_municipio <- contexto_municipio_bruto |>
  select(
    all_of(
      colunas_perfil_municipio
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "variavel_original",
    values_to = "valor_original"
  ) |>
  mutate(
    dimensao = case_when(
      str_starts(
        variavel_original,
        "NSE "
      ) ~ "NSE",
      
      str_starts(
        variavel_original,
        "COR/RAÇA "
      ) ~ "Raça/cor",
      
      str_starts(
        variavel_original,
        "FEMININO"
      ) |
        str_starts(
          variavel_original,
          "MASCULINO"
        ) ~ "Sexo",
      
      TRUE ~ "Outra"
    ),
    
    categoria = variavel_original |>
      str_remove(" - PERFIL$") |>
      str_remove("^NSE ") |>
      str_remove("^COR/RAÇA ") |>
      str_to_sentence(),
    
    percentual = extrair_percentual_perfil(
      valor_original
    ),
    
    numero_estudantes = extrair_numero_estudantes(
      valor_original
    )
  ) |>
  select(
    dimensao,
    categoria,
    percentual,
    numero_estudantes
  )

proficiencia_municipio <- contexto_municipio_bruto |>
  select(
    all_of(
      colunas_proficiencia_municipio
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "variavel_original",
    values_to = "valor_original"
  ) |>
  mutate(
    dimensao = case_when(
      str_starts(
        variavel_original,
        "NSE "
      ) ~ "NSE",
      
      str_starts(
        variavel_original,
        "COR/RAÇA "
      ) ~ "Raça/cor",
      
      str_starts(
        variavel_original,
        "FEMININO"
      ) |
        str_starts(
          variavel_original,
          "MASCULINO"
        ) ~ "Sexo",
      
      TRUE ~ "Outra"
    ),
    
    categoria = variavel_original |>
      str_remove(
        " - PROFICIÊNCIA$"
      ) |>
      str_remove("^NSE ") |>
      str_remove("^COR/RAÇA ") |>
      str_to_sentence(),
    
    proficiencia_grupo =
      converter_numero(
        valor_original
      )
  ) |>
  select(
    dimensao,
    categoria,
    proficiencia_grupo
  )

contexto_municipio <- perfil_municipio |>
  full_join(
    proficiencia_municipio,
    by = c(
      "dimensao",
      "categoria"
    )
  ) |>
  mutate(
    ano = 2026L,
    ano_escolar = 5L,
    componente = "Língua Portuguesa",
    unidade = "Rede municipal de Porto Alegre"
  ) |>
  relocate(
    ano,
    ano_escolar,
    componente,
    unidade
  )

# -------------------------------------------------------------------
# 10. Validação
# -------------------------------------------------------------------

escolas_contexto_sem_id <- fato_contexto_caed |>
  filter(
    is.na(id_escola) |
      id_escola == ""
  ) |>
  distinct(
    nome_escola_contexto
  )

resumo_contexto <- fato_contexto_caed |>
  group_by(
    dimensao
  ) |>
  summarise(
    numero_escolas = n_distinct(
      id_escola,
      na.rm = TRUE
    ),
    
    numero_categorias =
      n_distinct(categoria),
    
    linhas = n(),
    
    percentual_disponivel = sum(
      !is.na(percentual)
    ),
    
    numero_estudantes_disponivel = sum(
      !is.na(numero_estudantes)
    ),
    
    proficiencia_disponivel = sum(
      !is.na(proficiencia_grupo)
    ),
    
    .groups = "drop"
  )

# -------------------------------------------------------------------
# 11. Agregação contextual por carteira
# -------------------------------------------------------------------

contexto_com_assessora <- fato_contexto_caed |>
  left_join(
    dim_escola,
    by = "id_escola"
  ) |>
  mutate(
    assessora = case_when(
      is.na(assessora) |
        str_squish(assessora) == "" ~
        "Sem vinculação informada",
      
      TRUE ~ str_squish(assessora)
    ),
    
    peso_percentual = case_when(
      !is.na(percentual) &
        !is.na(numero_estudantes) &
        numero_estudantes > 0 ~
        numero_estudantes,
      
      TRUE ~ 0
    ),
    
    numerador_percentual = case_when(
      peso_percentual > 0 ~
        percentual * peso_percentual,
      
      TRUE ~ 0
    ),
    
    peso_proficiencia = case_when(
      !is.na(proficiencia_grupo) &
        !is.na(numero_estudantes) &
        numero_estudantes > 0 ~
        numero_estudantes,
      
      TRUE ~ 0
    ),
    
    numerador_proficiencia = case_when(
      peso_proficiencia > 0 ~
        proficiencia_grupo *
        peso_proficiencia,
      
      TRUE ~ 0
    )
  )

contexto_carteiras <- contexto_com_assessora |>
  group_by(
    assessora,
    dimensao,
    categoria
  ) |>
  summarise(
    numero_escolas_com_dado = n_distinct(
      id_escola[
        !is.na(numero_estudantes) &
          numero_estudantes > 0
      ]
    ),
    
    numero_estudantes = sum(
      .data$numero_estudantes,
      na.rm = TRUE
    ),
    
    soma_numerador_percentual = sum(
      numerador_percentual,
      na.rm = TRUE
    ),
    
    soma_peso_percentual = sum(
      peso_percentual,
      na.rm = TRUE
    ),
    
    soma_numerador_proficiencia = sum(
      numerador_proficiencia,
      na.rm = TRUE
    ),
    
    soma_peso_proficiencia = sum(
      peso_proficiencia,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    percentual_medio_ponderado = if_else(
      soma_peso_percentual > 0,
      soma_numerador_percentual /
        soma_peso_percentual,
      NA_real_
    ),
    
    proficiencia_media_ponderada = if_else(
      soma_peso_proficiencia > 0,
      soma_numerador_proficiencia /
        soma_peso_proficiencia,
      NA_real_
    )
  ) |>
  select(
    assessora,
    dimensao,
    categoria,
    numero_escolas_com_dado,
    numero_estudantes,
    percentual_medio_ponderado,
    proficiencia_media_ponderada
  ) |>
  arrange(
    assessora,
    dimensao,
    categoria
  )

# -------------------------------------------------------------------
# 12. Exportação
# -------------------------------------------------------------------

write_csv(
  fato_contexto_caed,
  here(
    "dados_processados",
    "fato_contexto_caed_5ano_2026.csv"
  ),
  na = ""
)

write_csv(
  contexto_municipio,
  here(
    "resultados",
    "contexto",
    "referencia_contextual_municipio_5ano_2026.csv"
  ),
  na = ""
)

write_csv(
  contexto_carteiras,
  here(
    "resultados",
    "contexto",
    "contexto_caed_por_carteira_5ano_2026.csv"
  ),
  na = ""
)

write_csv(
  escolas_contexto_sem_id,
  here(
    "documentacao",
    "escolas_contexto_caed_sem_id.csv"
  ),
  na = ""
)

write_csv(
  resumo_contexto,
  here(
    "documentacao",
    "resumo_disponibilidade_contexto_caed.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 13. Resumo no console
# -------------------------------------------------------------------

cat(
  "\nMódulo contextual CAEd concluído.\n"
)

cat(
  "\nDisponibilidade dos dados:\n"
)

print(
  resumo_contexto,
  n = Inf
)

cat(
  "\nEscolas sem identificação:\n"
)

print(
  escolas_contexto_sem_id,
  n = Inf
)

cat(
  "\nArquivos gerados:\n"
)

cat(
  "- dados_processados/fato_contexto_caed_5ano_2026.csv\n",
  "- resultados/contexto/referencia_contextual_municipio_5ano_2026.csv\n",
  "- resultados/contexto/contexto_caed_por_carteira_5ano_2026.csv\n",
  "- documentacao/escolas_contexto_caed_sem_id.csv\n",
  "- documentacao/resumo_disponibilidade_contexto_caed.csv\n"
)