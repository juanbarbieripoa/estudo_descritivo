library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Arquivos
# -------------------------------------------------------------------

arquivo_fato <- here(
  "dados_processados",
  "fato_turma_avaliacao.csv"
)

arquivo_painel <- here(
  "dados_processados",
  "painel_escola_serie_ano.csv"
)

arquivo_variacao <- here(
  "dados_processados",
  "variacao_escola_serie_2025_2026.csv"
)

arquivo_mapa <- here(
  "dados_intermediarios",
  "mapa_nomes_escolas.csv"
)

arquivos_necessarios <- c(
  arquivo_fato,
  arquivo_painel,
  arquivo_variacao,
  arquivo_mapa
)

ausentes <- arquivos_necessarios[
  !file.exists(arquivos_necessarios)
]

if (length(ausentes) > 0) {
  stop(
    "Arquivos ausentes:\n",
    paste(ausentes, collapse = "\n")
  )
}

dir.create(
  here("documentacao", "diagnostico_painel"),
  recursive = TRUE,
  showWarnings = FALSE
)

# -------------------------------------------------------------------
# 2. Leitura
# -------------------------------------------------------------------

fato <- read_csv(
  arquivo_fato,
  show_col_types = FALSE
)

painel <- read_csv(
  arquivo_painel,
  show_col_types = FALSE
)

variacao <- read_csv(
  arquivo_variacao,
  show_col_types = FALSE
)

mapa <- read_csv(
  arquivo_mapa,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

# -------------------------------------------------------------------
# 3. Quantidade de escolas em cada estágio
# -------------------------------------------------------------------

resumo_etapas <- bind_rows(
  
  fato |>
    group_by(ano, ano_escolar) |>
    summarise(
      etapa = "Tabela fato",
      linhas = n(),
      escolas_nome = n_distinct(
        nome_escola_caed,
        na.rm = TRUE
      ),
      escolas_id = n_distinct(
        id_escola,
        na.rm = TRUE
      ),
      linhas_sem_id = sum(
        is.na(id_escola) |
          id_escola == ""
      ),
      proficiencia_valida = sum(
        !is.na(proficiencia_media)
      ),
      .groups = "drop"
    ),
  
  painel |>
    group_by(ano, ano_escolar) |>
    summarise(
      etapa = "Painel escola-série-ano",
      linhas = n(),
      escolas_nome = n_distinct(
        nome_canonico,
        na.rm = TRUE
      ),
      escolas_id = n_distinct(
        id_escola,
        na.rm = TRUE
      ),
      linhas_sem_id = sum(
        is.na(id_escola) |
          id_escola == ""
      ),
      proficiencia_valida = sum(
        !is.na(proficiencia_media)
      ),
      .groups = "drop"
    )
) |>
  arrange(
    ano_escolar,
    ano,
    etapa
  )

# -------------------------------------------------------------------
# 4. Presença longitudinal por ID
# -------------------------------------------------------------------

presenca_por_id <- painel |>
  filter(
    ano %in% c(2025, 2026)
  ) |>
  distinct(
    id_escola,
    nome_canonico,
    ano_escolar,
    componente,
    ano,
    proficiencia_media
  ) |>
  mutate(
    presente = TRUE,
    resultado_valido = !is.na(proficiencia_media)
  ) |>
  pivot_wider(
    id_cols = c(
      id_escola,
      ano_escolar,
      componente
    ),
    names_from = ano,
    values_from = c(
      presente,
      resultado_valido
    ),
    values_fill = list(
      presente = FALSE,
      resultado_valido = FALSE
    ),
    names_glue = "{.value}_{ano}"
  ) |>
  mutate(
    presente_ambos =
      presente_2025 & presente_2026,
    
    resultado_ambos =
      resultado_valido_2025 &
      resultado_valido_2026
  )

resumo_presenca <- presenca_por_id |>
  group_by(ano_escolar) |>
  summarise(
    ids_unicos = n_distinct(id_escola),
    presentes_2025 = sum(presente_2025),
    presentes_2026 = sum(presente_2026),
    presentes_ambos = sum(presente_ambos),
    resultado_ambos = sum(resultado_ambos),
    .groups = "drop"
  )

# -------------------------------------------------------------------
# 5. Escolas presentes por nome, mas não por ID
# -------------------------------------------------------------------

presenca_por_nome <- painel |>
  filter(
    ano %in% c(2025, 2026)
  ) |>
  mutate(
    nome_chave = nome_canonico |>
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
  ) |>
  distinct(
    nome_chave,
    ano_escolar,
    ano,
    id_escola,
    nome_canonico
  )

nomes_com_ids_diferentes <- presenca_por_nome |>
  group_by(
    nome_chave,
    ano_escolar
  ) |>
  summarise(
    anos = n_distinct(ano),
    numero_ids = n_distinct(
      id_escola,
      na.rm = TRUE
    ),
    ids = paste(
      sort(unique(id_escola)),
      collapse = " | "
    ),
    nomes = paste(
      sort(unique(nome_canonico)),
      collapse = " | "
    ),
    .groups = "drop"
  ) |>
  filter(
    anos == 2,
    numero_ids > 1
  )

# -------------------------------------------------------------------
# 6. IDs exclusivos de cada ano
# -------------------------------------------------------------------

ids_por_ano <- painel |>
  filter(
    ano %in% c(2025, 2026)
  ) |>
  distinct(
    id_escola,
    nome_canonico,
    ano_escolar,
    ano
  )

ids_so_2025 <- ids_por_ano |>
  filter(ano == 2025) |>
  anti_join(
    ids_por_ano |>
      filter(ano == 2026),
    by = c(
      "id_escola",
      "ano_escolar"
    )
  )

ids_so_2026 <- ids_por_ano |>
  filter(ano == 2026) |>
  anti_join(
    ids_por_ano |>
      filter(ano == 2025),
    by = c(
      "id_escola",
      "ano_escolar"
    )
  )

# -------------------------------------------------------------------
# 7. Proficiência ausente
# -------------------------------------------------------------------

proficiencia_ausente <- painel |>
  filter(
    is.na(proficiencia_media)
  ) |>
  select(
    ano,
    id_escola,
    nome_canonico,
    ano_escolar,
    componente,
    numero_turmas,
    previstos,
    avaliados,
    taxa_participacao
  ) |>
  arrange(
    ano_escolar,
    ano,
    nome_canonico
  )

# -------------------------------------------------------------------
# 8. Duplicidades no painel
# -------------------------------------------------------------------

duplicidades_painel <- painel |>
  count(
    ano,
    id_escola,
    ano_escolar,
    componente,
    name = "n"
  ) |>
  filter(n > 1)

# -------------------------------------------------------------------
# 9. Correspondência de nomes e IDs
# -------------------------------------------------------------------

mapa_ids <- mapa |>
  select(
    fonte,
    nome_original,
    chave_origem,
    chave_dimensao,
    id_escola_provisorio = any_of(
      "id_escola_provisorio"
    ),
    id_escola = any_of("id_escola"),
    nome_canonico,
    metodo_correspondencia
  )

# -------------------------------------------------------------------
# 10. Exportação
# -------------------------------------------------------------------

write_csv(
  resumo_etapas,
  here(
    "documentacao",
    "diagnostico_painel",
    "01_resumo_etapas.csv"
  ),
  na = ""
)

write_csv(
  resumo_presenca,
  here(
    "documentacao",
    "diagnostico_painel",
    "02_resumo_presenca_longitudinal.csv"
  ),
  na = ""
)

write_csv(
  presenca_por_id,
  here(
    "documentacao",
    "diagnostico_painel",
    "03_presenca_por_id.csv"
  ),
  na = ""
)

write_csv(
  nomes_com_ids_diferentes,
  here(
    "documentacao",
    "diagnostico_painel",
    "04_nomes_com_ids_diferentes.csv"
  ),
  na = ""
)

write_csv(
  ids_so_2025,
  here(
    "documentacao",
    "diagnostico_painel",
    "05_ids_apenas_2025.csv"
  ),
  na = ""
)

write_csv(
  ids_so_2026,
  here(
    "documentacao",
    "diagnostico_painel",
    "06_ids_apenas_2026.csv"
  ),
  na = ""
)

write_csv(
  proficiencia_ausente,
  here(
    "documentacao",
    "diagnostico_painel",
    "07_proficiencia_ausente.csv"
  ),
  na = ""
)

write_csv(
  duplicidades_painel,
  here(
    "documentacao",
    "diagnostico_painel",
    "08_duplicidades_painel.csv"
  ),
  na = ""
)

write_csv(
  mapa_ids,
  here(
    "documentacao",
    "diagnostico_painel",
    "09_mapa_nomes_ids.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 11. Resumo no console
# -------------------------------------------------------------------

cat("\nResumo das etapas:\n")
print(resumo_etapas, n = Inf)

cat("\nPresença longitudinal:\n")
print(resumo_presenca, n = Inf)

cat("\nNomes presentes nos dois anos, mas com IDs diferentes:\n")
cat(nrow(nomes_com_ids_diferentes), "\n")

cat("\nLinhas com proficiência ausente:\n")
cat(nrow(proficiencia_ausente), "\n")

cat("\nDuplicidades no painel:\n")
cat(nrow(duplicidades_painel), "\n")