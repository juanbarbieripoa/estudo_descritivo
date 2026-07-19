library(here)
library(tidyverse)
library(data.table)
library(stringr)

caminho_dados <- here("dados_brutos")

arquivos_csv <- list.files(
  path = caminho_dados,
  pattern = "^\\d{4}_1av_form_HABILIDADES_DESEMPENHO_TURMA_.*\\.csv$",
  full.names = TRUE
)

df_desempenho <- purrr::map_dfr(arquivos_csv, function(arquivo) {
  
  ano_arquivo <- basename(arquivo) |>
    str_extract("^\\d{4}") |>
    as.numeric()
  
  fread(
    arquivo,
    encoding = "UTF-8"
  ) |>
    mutate(ano = ano_arquivo)
})