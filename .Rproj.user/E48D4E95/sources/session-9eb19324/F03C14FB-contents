library(data.table)
library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Arquivos de entrada
# -------------------------------------------------------------------

arquivo_dim_provisoria <- here(
  "dados_intermediarios",
  "dim_escola_provisoria.csv"
)

arquivo_correspondencias <- here(
  "documentacao",
  "diagnostico_correspondencias_escolas.csv"
)

arquivo_revisao <- here(
  "documentacao",
  "revisao_manual_correspondencias_escolas.csv"
)

arquivos_necessarios <- c(
  arquivo_dim_provisoria,
  arquivo_correspondencias,
  arquivo_revisao
)

arquivos_ausentes <- arquivos_necessarios[
  !file.exists(arquivos_necessarios)
]

if (length(arquivos_ausentes) > 0) {
  stop(
    "Os seguintes arquivos não foram encontrados:\n",
    paste(arquivos_ausentes, collapse = "\n")
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

normalizar_decisao <- function(x) {
  x |>
    as.character() |>
    iconv(from = "", to = "ASCII//TRANSLIT") |>
    str_to_lower() |>
    str_squish()
}

# -------------------------------------------------------------------
# 3. Leitura
# -------------------------------------------------------------------

dim_provisoria <- read_csv(
  arquivo_dim_provisoria,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

correspondencias <- read_csv(
  arquivo_correspondencias,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

revisao <- data.table::fread(
  arquivo_revisao,
  encoding = "UTF-8",
  colClasses = "character",
  data.table = FALSE,
  check.names = FALSE
) |>
  as_tibble()

# Remove espaços e eventual marcador BOM dos nomes das colunas
names(revisao) <- names(revisao) |>
  stringr::str_remove("^\\ufeff") |>
  stringr::str_trim()

# Interrompe com diagnóstico claro caso a coluna tenha sido renomeada
if (!"decisao_manual" %in% names(revisao)) {
  stop(
    "A coluna 'decisao_manual' não foi encontrada.\n",
    "Colunas existentes no arquivo:\n",
    paste(names(revisao), collapse = "\n")
  )
}

revisao <- revisao |>
  mutate(
    decisao_normalizada = normalizar_decisao(decisao_manual)
  )

# -------------------------------------------------------------------
# 4. Validação das decisões manuais
# -------------------------------------------------------------------

decisoes_validas <- c(
  "aceitar",
  "rejeitar",
  "nova escola",
  "nova_escola"
)

decisoes_invalidas <- revisao |>
  filter(
    !is.na(decisao_normalizada),
    decisao_normalizada != "",
    !decisao_normalizada %in% decisoes_validas
  )

if (nrow(decisoes_invalidas) > 0) {
  write_csv(
    decisoes_invalidas,
    here(
      "documentacao",
      "erros_decisoes_manuais_escolas.csv"
    ),
    na = ""
  )
  
  stop(
    "Há valores não reconhecidos na coluna decisao_manual.\n",
    "Consulte documentacao/erros_decisoes_manuais_escolas.csv."
  )
}

# Cada nome de origem deve ter no máximo um candidato aceito.
aceites_duplicados <- revisao |>
  filter(decisao_normalizada == "aceitar") |>
  count(
    fonte,
    nome_original,
    name = "numero_aceites"
  ) |>
  filter(numero_aceites > 1)

if (nrow(aceites_duplicados) > 0) {
  write_csv(
    aceites_duplicados,
    here(
      "documentacao",
      "erros_multiplos_aceites_escolas.csv"
    ),
    na = ""
  )
  
  stop(
    "Há nomes com mais de um candidato marcado como aceitar.\n",
    "Consulte documentacao/erros_multiplos_aceites_escolas.csv."
  )
}

# -------------------------------------------------------------------
# 5. Correspondências exatas já reconhecidas
# -------------------------------------------------------------------

mapa_exato <- correspondencias |>
  filter(status_correspondencia == "exata_normalizada") |>
  transmute(
    fonte,
    nome_original,
    chave_origem = chave_nome,
    chave_dimensao = chave_nome,
    nome_vinculo = nome_vinculo_original,
    assessora,
    metodo_correspondencia = "exata_normalizada",
    observacao_correspondencia = NA_character_
  ) |>
  distinct()

# -------------------------------------------------------------------
# 6. Correspondências aceitas manualmente
# -------------------------------------------------------------------

mapa_manual <- revisao |>
  filter(decisao_normalizada == "aceitar") |>
  transmute(
    fonte,
    nome_original,
    chave_origem = chave_nome,
    chave_dimensao = normalizar_nome(
      str_remove(
        nome_vinculo_candidato,
        "^\\d{4,8}\\s+"
      )
    ),
    nome_vinculo = nome_vinculo_candidato,
    assessora = assessora_candidata,
    metodo_correspondencia = "validada_manualmente",
    observacao_correspondencia = na_if(
      observacao,
      ""
    )
  ) |>
  distinct()

# -------------------------------------------------------------------
# 7. Escolas classificadas como novas
# -------------------------------------------------------------------

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

novas_escolas <- revisao |>
  filter(
    decisao_normalizada %in% c(
      "nova escola",
      "nova_escola"
    )
  ) |>
  group_by(
    fonte,
    nome_original,
    chave_nome
  ) |>
  summarise(
    nome_canonico_manual = primeiro_nao_vazio(
      nome_canonico_manual
    ),
    observacao = primeiro_nao_vazio(
      observacao
    ),
    .groups = "drop"
  ) |>
  mutate(
    nome_canonico_novo = coalesce(
      nome_canonico_manual,
      str_remove(nome_original, "^\\d{4,8}\\s+")
    ),
    chave_dimensao = normalizar_nome(
      str_remove(
        nome_canonico_novo,
        "^\\d{4,8}\\s+"
      )
    ),
    assessora = NA_character_,
    metodo_correspondencia = "nova_escola",
    observacao_correspondencia = observacao
  )

# -------------------------------------------------------------------
# 8. Ampliação da dimensão com escolas novas
# -------------------------------------------------------------------

dim_base <- dim_provisoria |>
  transmute(
    id_escola_provisorio,
    codigo_inep,
    nome_canonico,
    nome_vinculo_original,
    chave_nome,
    assessora,
    exposicao_2025_1av =
      as.integer(exposicao_2025_1av),
    programa_iniciado_apos_2025_1av =
      as.logical(programa_iniciado_apos_2025_1av),
    dose_assessoramento_observada =
      as.logical(dose_assessoramento_observada),
    status_validacao
  )

if (nrow(novas_escolas) > 0) {
  
  proximo_numero <- dim_base$id_escola_provisorio |>
    str_extract("\\d+$") |>
    as.integer() |>
    max(na.rm = TRUE)
  
  novas_dim <- novas_escolas |>
    distinct(
      chave_dimensao,
      nome_canonico_novo,
      assessora,
      observacao_correspondencia
    ) |>
    arrange(chave_dimensao) |>
    mutate(
      id_escola_provisorio = sprintf(
        "ESC_%03d",
        proximo_numero + row_number()
      )
    ) |>
    transmute(
      id_escola_provisorio,
      codigo_inep = NA_character_,
      nome_canonico = nome_canonico_novo,
      nome_vinculo_original = NA_character_,
      chave_nome = chave_dimensao,
      assessora,
      exposicao_2025_1av = 0L,
      programa_iniciado_apos_2025_1av = TRUE,
      dose_assessoramento_observada = FALSE,
      status_validacao = "incluida_manualmente"
    )
  
  dim_base <- bind_rows(
    dim_base,
    novas_dim
  )
}

# -------------------------------------------------------------------
# 9. Mapa completo dos nomes observados
# -------------------------------------------------------------------

mapa_novas <- novas_escolas |>
  transmute(
    fonte,
    nome_original,
    chave_origem = chave_nome,
    chave_dimensao,
    nome_vinculo = NA_character_,
    assessora,
    metodo_correspondencia,
    observacao_correspondencia
  )

mapa_nomes_escolas <- bind_rows(
  mapa_exato,
  mapa_manual,
  mapa_novas
) |>
  left_join(
    dim_base |>
      select(
        id_escola_provisorio,
        chave_dimensao = chave_nome,
        nome_canonico,
        codigo_inep
      ),
    by = "chave_dimensao"
  ) |>
  arrange(
    id_escola_provisorio,
    fonte,
    nome_original
  )

# -------------------------------------------------------------------
# 10. Diagnóstico de nomes ainda sem decisão
# -------------------------------------------------------------------

nomes_nao_correspondidos <- correspondencias |>
  filter(status_correspondencia == "nao_correspondida") |>
  distinct(
    fonte,
    nome_original,
    chave_nome
  )

nomes_resolvidos <- mapa_nomes_escolas |>
  distinct(
    fonte,
    nome_original
  )

pendencias <- nomes_nao_correspondidos |>
  anti_join(
    nomes_resolvidos,
    by = c("fonte", "nome_original")
  )

write_csv(
  pendencias,
  here(
    "documentacao",
    "pendencias_correspondencia_escolas.csv"
  ),
  na = ""
)

if (nrow(pendencias) > 0) {
  warning(
    nrow(pendencias),
    " nome(s) de escola ainda estão sem correspondência. ",
    "Consulte documentacao/pendencias_correspondencia_escolas.csv."
  )
}

# -------------------------------------------------------------------
# 11. Dimensão consolidada
# -------------------------------------------------------------------

dim_escola <- dim_base |>
  mutate(
    id_escola = id_escola_provisorio,
    status_rede_2025 = case_when(
      str_detect(
        str_to_lower(
          coalesce(
            nome_canonico,
            ""
          )
        ),
        "aldeia lumiar"
      ) ~ "a_confirmar",
      TRUE ~ NA_character_
    ),
    possivel_municipalizacao_recente = case_when(
      str_detect(
        str_to_lower(
          coalesce(
            nome_canonico,
            ""
          )
        ),
        "aldeia lumiar"
      ) ~ TRUE,
      TRUE ~ FALSE
    ),
    observacao_historico = case_when(
      possivel_municipalizacao_recente ~
        paste(
          "Possível escola estadual posteriormente municipalizada;",
          "situação administrativa e data de ingresso na rede",
          "devem ser confirmadas em fonte oficial."
        ),
      TRUE ~ NA_character_
    )
  ) |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    nome_vinculo_original,
    chave_nome,
    assessora,
    exposicao_2025_1av,
    programa_iniciado_apos_2025_1av,
    dose_assessoramento_observada,
    possivel_municipalizacao_recente,
    status_rede_2025,
    observacao_historico,
    status_validacao
  ) |>
  arrange(nome_canonico)

# -------------------------------------------------------------------
# 12. Exportação
# -------------------------------------------------------------------

write_csv(
  dim_escola,
  here(
    "dados_intermediarios",
    "dim_escola.csv"
  ),
  na = ""
)

write_csv(
  mapa_nomes_escolas,
  here(
    "dados_intermediarios",
    "mapa_nomes_escolas.csv"
  ),
  na = ""
)

cat("\nDimensão de escolas consolidada.\n")

cat(
  "- dados_intermediarios/dim_escola.csv\n",
  "- dados_intermediarios/mapa_nomes_escolas.csv\n",
  "- documentacao/pendencias_correspondencia_escolas.csv\n"
)