library(here)
library(tidyverse)
library(data.table)

# -------------------------------------------------------------------
# 1. Diretórios
# -------------------------------------------------------------------

dir.create(
  here("documentacao"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("dados_intermediarios"),
  recursive = TRUE,
  showWarnings = FALSE
)

# -------------------------------------------------------------------
# 2. Localização dos arquivos CAEd
# -------------------------------------------------------------------

caminho_dados <- here("dados_brutos")

arquivos_caed <- list.files(
  path = caminho_dados,
  pattern = "^20(25|26)_1av_form_HABILIDADES_DESEMPENHO_TURMA_[0-9]{2}anos_LP\\.csv$",
  full.names = TRUE
)

if (length(arquivos_caed) == 0) {
  stop(
    "Nenhum arquivo CAEd foi encontrado em: ",
    caminho_dados
  )
}

message(
  length(arquivos_caed),
  " arquivos CAEd encontrados."
)

# -------------------------------------------------------------------
# 3. Funções auxiliares
# -------------------------------------------------------------------

normalizar_nome <- function(x) {
  x |>
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9 ]", " ") |>
    str_squish()
}

ler_caed_texto <- function(arquivo) {
  fread(
    arquivo,
    encoding = "UTF-8",
    colClasses = "character",
    na.strings = c("", "NA", "N/A")
  ) |>
    as_tibble()
}

extrair_ano <- function(arquivo) {
  basename(arquivo) |>
    str_extract("^20\\d{2}") |>
    as.integer()
}

extrair_ano_escolar <- function(arquivo) {
  resultado <- stringr::str_match(
    basename(arquivo),
    "_([0-9]{2})anos_"
  )
  
  as.integer(resultado[, 2])
}

# -------------------------------------------------------------------
# 4. Inventário dos arquivos
# -------------------------------------------------------------------

inventario_arquivos <- map_dfr(
  arquivos_caed,
  function(arquivo) {
    
    dados <- ler_caed_texto(arquivo)
    
    colunas_habilidade <- names(dados) |>
      str_subset("^H\\s*[0-9]+\\s*\\(%\\)$")
    
    tibble(
      arquivo = basename(arquivo),
      ano = extrair_ano(arquivo),
      ano_escolar = extrair_ano_escolar(arquivo),
      componente = "Língua Portuguesa",
      avaliacao = "1ª Avaliação Formativa",
      numero_linhas = nrow(dados),
      numero_colunas = ncol(dados),
      numero_habilidades = length(colunas_habilidade),
      primeira_habilidade = first(colunas_habilidade, default = NA_character_),
      ultima_habilidade = last(colunas_habilidade, default = NA_character_),
      numero_escolas = if ("Escola" %in% names(dados)) {
        n_distinct(dados[["Escola"]], na.rm = TRUE)
      } else {
        NA_integer_
      },
      numero_turmas = if ("Código da Turma" %in% names(dados)) {
        n_distinct(dados[["Código da Turma"]], na.rm = TRUE)
      } else {
        NA_integer_
      }
    )
  }
) |>
  arrange(ano, ano_escolar)

write_csv(
  inventario_arquivos,
  here("documentacao", "inventario_arquivos_caed.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 5. Mapa de presença das colunas
# -------------------------------------------------------------------

colunas_por_arquivo <- map_dfr(
  arquivos_caed,
  function(arquivo) {
    
    dados <- ler_caed_texto(arquivo)
    
    tibble(
      arquivo = basename(arquivo),
      ano = extrair_ano(arquivo),
      ano_escolar = extrair_ano_escolar(arquivo),
      coluna = names(dados),
      presente = TRUE,
      tipo_importacao = "character",
      coluna_habilidade = str_detect(
        names(dados),
        "^H\\s*[0-9]+\\s*\\(%\\)$"
      )
    )
  }
)

todas_colunas <- sort(unique(colunas_por_arquivo$coluna))

mapa_colunas <- crossing(
  arquivo = basename(arquivos_caed),
  coluna = todas_colunas
) |>
  left_join(
    colunas_por_arquivo |>
      select(
        arquivo,
        coluna,
        ano,
        ano_escolar,
        presente,
        coluna_habilidade
      ),
    by = c("arquivo", "coluna")
  ) |>
  mutate(
    presente = replace_na(presente, FALSE),
    coluna_habilidade = replace_na(coluna_habilidade, FALSE),
    ano = coalesce(
      ano,
      as.integer(str_extract(arquivo, "^20\\d{2}"))
    ),
    ano_escolar = coalesce(
      ano_escolar,
      as.integer(str_match(arquivo, "_([0-9]{2})anos_")[, 2])
    )
  ) |>
  arrange(coluna, ano, ano_escolar)

write_csv(
  mapa_colunas,
  here("documentacao", "mapa_presenca_colunas_caed.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 6. Cadastro provisório de escolas
# -------------------------------------------------------------------

cadastro_escolas_provisorio <- map_dfr(
  arquivos_caed,
  function(arquivo) {
    
    dados <- ler_caed_texto(arquivo)
    
    dados |>
      distinct(Escola) |>
      transmute(
        ano = extrair_ano(arquivo),
        ano_escolar = extrair_ano_escolar(arquivo),
        nome_escola_original = Escola,
        nome_escola_normalizado = normalizar_nome(Escola),
        arquivo_origem = basename(arquivo)
      )
  }
) |>
  distinct() |>
  arrange(nome_escola_normalizado, ano, ano_escolar)

write_csv(
  cadastro_escolas_provisorio,
  here(
    "dados_intermediarios",
    "cadastro_provisorio_escolas_caed.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 7. Resumo no console
# -------------------------------------------------------------------

cat("\nArquivos por ano e série:\n")

print(
  inventario_arquivos |>
    select(
      ano,
      ano_escolar,
      numero_linhas,
      numero_escolas,
      numero_turmas,
      numero_habilidades
    )
)

cat("\nArquivos gerados:\n")

cat(
  "- documentacao/inventario_arquivos_caed.csv\n",
  "- documentacao/mapa_presenca_colunas_caed.csv\n",
  "- dados_intermediarios/cadastro_provisorio_escolas_caed.csv\n"
)