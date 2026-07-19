library(here)
library(tidyverse)
library(readxl)

# -------------------------------------------------------------------
# 1. Diretórios
# -------------------------------------------------------------------

dir.create(
  here("dados_intermediarios"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("documentacao"),
  recursive = TRUE,
  showWarnings = FALSE
)

# -------------------------------------------------------------------
# 2. Funções auxiliares
# -------------------------------------------------------------------

normalizar_nome <- function(x) {
  x |>
    as.character() |>
    iconv(from = "", to = "ASCII//TRANSLIT") |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
    stringr::str_squish()
}

localizar_arquivo <- function(nome_arquivo) {
  
  resultado <- list.files(
    path = here("dados_brutos"),
    pattern = paste0("^", nome_arquivo, "$"),
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  if (length(resultado) == 0) {
    stop(
      "Arquivo não encontrado dentro de dados_brutos: ",
      nome_arquivo
    )
  }
  
  if (length(resultado) > 1) {
    stop(
      "Mais de um arquivo com o mesmo nome foi encontrado: ",
      nome_arquivo,
      "\nArquivos encontrados:\n",
      paste(resultado, collapse = "\n")
    )
  }
  
  resultado
}

# Remove prefixos numéricos que podem ter sido incorporados ao nome
# da escola em algumas exportações.
remover_prefixo_numerico <- function(x) {
  x |>
    stringr::str_remove("^\\d{4,8}\\s+") |>
    stringr::str_squish()
}

# -------------------------------------------------------------------
# 3. Localização dos arquivos
# -------------------------------------------------------------------

arquivo_vinculo <- localizar_arquivo(
  "tab_vinculo_escola_assessoramento.xlsx"
)

arquivo_contexto <- localizar_arquivo(
  "tab_dados_contextuais_escolas.xlsx"
)

arquivo_caed_escolas <- here(
  "dados_intermediarios",
  "cadastro_provisorio_escolas_caed.csv"
)

if (!file.exists(arquivo_caed_escolas)) {
  stop(
    "O cadastro provisório das escolas CAEd não foi encontrado.\n",
    "Execute primeiro o script R/01_inventario.R."
  )
}

# -------------------------------------------------------------------
# 4. Cadastro de referência: escola e assessora
# -------------------------------------------------------------------

vinculo_bruto <- read_excel(
  arquivo_vinculo,
  sheet = "vinculo",
  col_types = "text"
)

vinculo <- vinculo_bruto |>
  transmute(
    nome_vinculo_original = escolas,
    assessora = vinculo,
    nome_vinculo_sem_codigo = remover_prefixo_numerico(
      nome_vinculo_original
    ),
    chave_nome = normalizar_nome(nome_vinculo_sem_codigo)
  ) |>
  filter(
    !is.na(chave_nome),
    chave_nome != ""
  ) |>
  distinct()

# Verifica se duas escolas da tabela de vínculo ficaram com a mesma chave
duplicidades_vinculo <- vinculo |>
  count(chave_nome, name = "n") |>
  filter(n > 1)

if (nrow(duplicidades_vinculo) > 0) {
  
  write_csv(
    duplicidades_vinculo,
    here(
      "documentacao",
      "duplicidades_chave_vinculo.csv"
    ),
    na = ""
  )
  
  stop(
    "Foram encontradas chaves duplicadas na tabela de vínculo.\n",
    "Consulte documentacao/duplicidades_chave_vinculo.csv."
  )
}

# -------------------------------------------------------------------
# 5. Nomes encontrados nas bases CAEd
# -------------------------------------------------------------------

escolas_caed <- read_csv(
  arquivo_caed_escolas,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  transmute(
    fonte = "CAEd",
    ano = as.integer(ano),
    ano_escolar = as.integer(ano_escolar),
    nome_original = nome_escola_original,
    nome_sem_codigo = remover_prefixo_numerico(nome_original),
    chave_nome = normalizar_nome(nome_sem_codigo),
    arquivo_origem
  ) |>
  distinct()

# -------------------------------------------------------------------
# 6. Nomes encontrados na planilha contextual
# -------------------------------------------------------------------

abas_contexto <- excel_sheets(arquivo_contexto)

contexto_bruto <- map_dfr(
  abas_contexto,
  function(aba) {
    
    dados <- read_excel(
      arquivo_contexto,
      sheet = aba,
      col_types = "text"
    )
    
    if (!"ESCOLA" %in% names(dados)) {
      return(tibble())
    }
    
    dados |>
      transmute(
        fonte = "Contexto CAEd",
        ano = 2026L,
        ano_escolar = suppressWarnings(
          readr::parse_integer(`ANO ESCOLAR`)
        ),
        nome_original = ESCOLA,
        arquivo_origem = basename(arquivo_contexto),
        aba_origem = aba
      )
  }
) |>
  mutate(
    nome_sem_codigo = remover_prefixo_numerico(nome_original),
    chave_nome = normalizar_nome(nome_sem_codigo)
  ) |>
  filter(
    !is.na(chave_nome),
    chave_nome != ""
  ) |>
  distinct()

# -------------------------------------------------------------------
# 7. Universo de nomes observados
# -------------------------------------------------------------------

nomes_observados <- bind_rows(
  escolas_caed |>
    select(
      fonte,
      ano,
      ano_escolar,
      nome_original,
      nome_sem_codigo,
      chave_nome,
      arquivo_origem
    ),
  contexto_bruto |>
    select(
      fonte,
      ano,
      ano_escolar,
      nome_original,
      nome_sem_codigo,
      chave_nome,
      arquivo_origem
    )
) |>
  distinct()

# -------------------------------------------------------------------
# 8. Correspondências exatas
# -------------------------------------------------------------------

correspondencias_exatas <- nomes_observados |>
  left_join(
    vinculo |>
      select(
        chave_nome,
        nome_vinculo_original,
        assessora
      ),
    by = "chave_nome"
  ) |>
  mutate(
    status_correspondencia = case_when(
      !is.na(nome_vinculo_original) ~ "exata_normalizada",
      TRUE ~ "nao_correspondida"
    )
  )

# -------------------------------------------------------------------
# 9. Candidatos aproximados para revisão manual
# -------------------------------------------------------------------

nao_correspondidas <- correspondencias_exatas |>
  filter(status_correspondencia == "nao_correspondida") |>
  distinct(
    fonte,
    nome_original,
    nome_sem_codigo,
    chave_nome
  )

gerar_candidatos <- function(
    chave_origem,
    nomes_referencia,
    numero_candidatos = 3
) {
  
  distancias <- adist(
    chave_origem,
    nomes_referencia
  )
  
  ordem <- order(distancias)[
    seq_len(
      min(numero_candidatos, length(distancias))
    )
  ]
  
  tibble(
    posicao_candidato = seq_along(ordem),
    indice_referencia = ordem,
    distancia_edicao = as.numeric(distancias[ordem])
  )
}

candidatos_revisao <- nao_correspondidas |>
  mutate(
    candidatos = map(
      chave_nome,
      gerar_candidatos,
      nomes_referencia = vinculo$chave_nome
    )
  ) |>
  unnest(candidatos) |>
  mutate(
    nome_vinculo_candidato =
      vinculo$nome_vinculo_original[indice_referencia],
    
    assessora_candidata =
      vinculo$assessora[indice_referencia],
    
    chave_candidata =
      vinculo$chave_nome[indice_referencia],
    
    comprimento_referencia = pmax(
      nchar(chave_nome),
      nchar(chave_candidata)
    ),
    
    distancia_relativa = if_else(
      comprimento_referencia > 0,
      distancia_edicao / comprimento_referencia,
      NA_real_
    ),
    
    decisao_manual = "",
    nome_canonico_manual = "",
    observacao = ""
  ) |>
  select(
    fonte,
    nome_original,
    nome_sem_codigo,
    chave_nome,
    posicao_candidato,
    nome_vinculo_candidato,
    assessora_candidata,
    distancia_edicao,
    distancia_relativa,
    decisao_manual,
    nome_canonico_manual,
    observacao
  ) |>
  arrange(
    fonte,
    nome_original,
    posicao_candidato
  )

# -------------------------------------------------------------------
# 10. Dimensão provisória das escolas
# -------------------------------------------------------------------

dim_escola_provisoria <- vinculo |>
  arrange(chave_nome) |>
  mutate(
    id_escola_provisorio = sprintf(
      "ESC_%03d",
      row_number()
    ),
    codigo_inep = NA_character_,
    nome_canonico = nome_vinculo_sem_codigo,
    exposicao_2025_1av = 0L,
    programa_iniciado_apos_2025_1av = TRUE,
    dose_assessoramento_observada = FALSE,
    status_validacao = "provisorio"
  ) |>
  select(
    id_escola_provisorio,
    codigo_inep,
    nome_canonico,
    nome_vinculo_original,
    chave_nome,
    assessora,
    exposicao_2025_1av,
    programa_iniciado_apos_2025_1av,
    dose_assessoramento_observada,
    status_validacao
  )

# -------------------------------------------------------------------
# 11. Presença das escolas nas diferentes fontes
# -------------------------------------------------------------------

presenca_fontes <- correspondencias_exatas |>
  filter(status_correspondencia == "exata_normalizada") |>
  distinct(
    chave_nome,
    fonte,
    ano
  ) |>
  mutate(
    presente = TRUE,
    fonte_ano = case_when(
      fonte == "CAEd" ~ paste0("caed_", ano),
      fonte == "Contexto CAEd" ~ "contexto_caed_2026",
      TRUE ~ normalizar_nome(fonte)
    )
  ) |>
  select(
    chave_nome,
    fonte_ano,
    presente
  ) |>
  distinct() |>
  pivot_wider(
    names_from = fonte_ano,
    values_from = presente,
    values_fill = FALSE
  )

dim_escola_provisoria <- dim_escola_provisoria |>
  left_join(
    presenca_fontes,
    by = "chave_nome"
  ) |>
  mutate(
    across(
      starts_with("caed_") |
        starts_with("contexto_"),
      ~ replace_na(.x, FALSE)
    )
  )

# -------------------------------------------------------------------
# 12. Diagnóstico das escolas da tabela de vínculo sem correspondência
# -------------------------------------------------------------------

vinculo_sem_caed <- dim_escola_provisoria |>
  anti_join(
    correspondencias_exatas |>
      filter(
        fonte == "CAEd",
        status_correspondencia == "exata_normalizada"
      ) |>
      distinct(chave_nome),
    by = "chave_nome"
  )

# -------------------------------------------------------------------
# 13. Exportação
# -------------------------------------------------------------------

write_csv(
  dim_escola_provisoria,
  here(
    "dados_intermediarios",
    "dim_escola_provisoria.csv"
  ),
  na = ""
)

write_csv(
  correspondencias_exatas,
  here(
    "documentacao",
    "diagnostico_correspondencias_escolas.csv"
  ),
  na = ""
)

write_csv(
  candidatos_revisao,
  here(
    "documentacao",
    "revisao_manual_correspondencias_escolas.csv"
  ),
  na = ""
)

write_csv(
  vinculo_sem_caed,
  here(
    "documentacao",
    "escolas_vinculo_sem_correspondencia_caed.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 14. Resumo no console
# -------------------------------------------------------------------

resumo_correspondencias <- correspondencias_exatas |>
  distinct(
    fonte,
    nome_original,
    status_correspondencia
  ) |>
  count(
    fonte,
    status_correspondencia,
    name = "n"
  )

cat("\nResumo das correspondências:\n")

print(resumo_correspondencias)

cat("\nArquivos gerados:\n")

cat(
  "- dados_intermediarios/dim_escola_provisoria.csv\n",
  "- documentacao/diagnostico_correspondencias_escolas.csv\n",
  "- documentacao/revisao_manual_correspondencias_escolas.csv\n",
  "- documentacao/escolas_vinculo_sem_correspondencia_caed.csv\n"
)

cat(
  "\nObservação metodológica incorporada:\n",
  "- a 1ª avaliação formativa de 2025 é a linha de base pré-programa;\n",
  "- exposição nessa avaliação foi definida como zero;\n",
  "- a vinculação à assessora não representa dose observada;\n",
  "- não serão atribuídos efeitos ou resultados individuais às assessoras.\n"
)