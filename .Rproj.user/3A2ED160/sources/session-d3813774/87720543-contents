library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Objetivo
# -------------------------------------------------------------------
#
# Consolidar a classificação administrativa observada no Censo Escolar
# de 2024 e definir universos analíticos transparentes.
#
# Este módulo:
#   - não altera dim_escola.csv;
#   - não exclui escolas das análises CAEd;
#   - usa a dependência administrativa observada em 2024 como referência;
#   - separa rede municipal direta, instituições privadas vinculadas,
#     escolas estaduais e demais situações;
#   - identifica divergências entre o Censo 2024 e o cadastro local;
#   - gera uma pauta de revisão manual para casos ambíguos.
#
# A classificação administrativa é contexto pré-programa, não resultado
# do assessoramento.
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 2. Diretórios e arquivos
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
  here("documentacao", "contexto_inep_2024"),
  recursive = TRUE,
  showWarnings = FALSE
)

arquivo_contexto <- here(
  "dados_processados",
  "dim_contexto_escola_2024_validada.csv"
)

if (!file.exists(arquivo_contexto)) {
  stop(
    "Arquivo não encontrado: ",
    arquivo_contexto,
    "\nExecute primeiro R/12C_corrigir_validar_contexto_2024.R."
  )
}

# -------------------------------------------------------------------
# 3. Funções auxiliares
# -------------------------------------------------------------------

primeiro_nao_vazio <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & str_squish(x) != ""]

  if (length(x) == 0) {
    return(NA_character_)
  }

  x[[1]]
}

# -------------------------------------------------------------------
# 4. Leitura
# -------------------------------------------------------------------

contexto <- read_csv(
  arquivo_contexto,
  show_col_types = FALSE,
  col_types = cols(
    id_escola = col_character(),
    codigo_inep = col_character(),
    nome_canonico = col_character(),
    assessora = col_character(),
    ano = col_integer(),
    dependencia_administrativa_2024 = col_character(),
    situacao_funcionamento_2024 = col_character(),
    matriculas_anos_iniciais = col_double(),
    turmas_anos_iniciais = col_double(),
    docentes_anos_iniciais = col_double(),
    tipo_vinculo_rede = col_character(),
    status_rede_2025 = col_character(),
    caso_historico_administrativo = col_character(),
    requer_revisao_tecnica = col_logical(),
    motivo_revisao_tecnica = col_character(),
    .default = col_guess()
  )
)

colunas_obrigatorias <- c(
  "id_escola",
  "codigo_inep",
  "nome_canonico",
  "dependencia_administrativa_2024",
  "situacao_funcionamento_2024",
  "tipo_vinculo_rede",
  "status_rede_2025",
  "requer_revisao_tecnica"
)

colunas_ausentes <- setdiff(
  colunas_obrigatorias,
  names(contexto)
)

if (length(colunas_ausentes) > 0) {
  stop(
    "Colunas obrigatórias ausentes em ",
    basename(arquivo_contexto),
    ":\n",
    paste(colunas_ausentes, collapse = "\n")
  )
}

# -------------------------------------------------------------------
# 5. Classificação administrativa observada em 2024
# -------------------------------------------------------------------

contexto_classificado <- contexto |>
  mutate(
    assessora = case_when(
      is.na(assessora) |
        str_squish(assessora) == "" ~
        "Sem vinculação informada",
      TRUE ~ str_squish(assessora)
    ),

    escola_ativa_2024 =
      situacao_funcionamento_2024 == "Em atividade",

    vinculo_privado_explicito = tipo_vinculo_rede %in% c(
      "privada_conveniada_sob_supervisao_municipal",
      "filantropica_sob_supervisao_municipal"
    ),

    grupo_administrativo_2024 = case_when(
      dependencia_administrativa_2024 == "Municipal" ~
        "Rede municipal direta em 2024",

      dependencia_administrativa_2024 == "Estadual" ~
        "Rede estadual em 2024",

      dependencia_administrativa_2024 == "Privada" &
        tipo_vinculo_rede ==
          "privada_conveniada_sob_supervisao_municipal" ~
        "Privada conveniada ou supervisionada",

      dependencia_administrativa_2024 == "Privada" &
        tipo_vinculo_rede ==
          "filantropica_sob_supervisao_municipal" ~
        "Filantrópica supervisionada",

      dependencia_administrativa_2024 == "Privada" ~
        "Privada sem vínculo municipal classificado",

      dependencia_administrativa_2024 == "Federal" ~
        "Rede federal em 2024",

      TRUE ~
        "Dependência administrativa não classificada"
    ),

    # Divergência entre a fonte oficial de 2024 e o cadastro local.
    divergencia_censo_cadastro = case_when(
      dependencia_administrativa_2024 == "Estadual" &
        status_rede_2025 == "rede_municipal_direta" ~ TRUE,

      dependencia_administrativa_2024 == "Privada" &
        status_rede_2025 == "rede_municipal_direta" &
        !vinculo_privado_explicito ~ TRUE,

      dependencia_administrativa_2024 == "Municipal" &
        status_rede_2025 ==
          "existencia_ou_funcionamento_em_2025_a_confirmar" ~ TRUE,

      TRUE ~ FALSE
    ),

    tipo_divergencia_administrativa = case_when(
      dependencia_administrativa_2024 == "Estadual" &
        status_rede_2025 == "rede_municipal_direta" ~
        paste(
          "Censo 2024 registra dependência estadual,",
          "mas o cadastro local classifica como rede municipal direta"
        ),

      dependencia_administrativa_2024 == "Privada" &
        status_rede_2025 == "rede_municipal_direta" &
        !vinculo_privado_explicito ~
        paste(
          "Censo 2024 registra dependência privada,",
          "mas o cadastro local classifica como rede municipal direta"
        ),

      dependencia_administrativa_2024 == "Municipal" &
        status_rede_2025 ==
          "existencia_ou_funcionamento_em_2025_a_confirmar" ~
        paste(
          "Censo 2024 registra escola municipal em atividade;",
          "a hipótese de escola nova precisa ser reinterpretada"
        ),

      TRUE ~ NA_character_
    ),

    # Universo estrito: retrato da rede municipal direta antes do programa.
    incluir_universo_municipal_direto_2024 =
      escola_ativa_2024 &
      dependencia_administrativa_2024 == "Municipal" &
      !coalesce(requer_revisao_tecnica, FALSE),

    # Universo ampliado: rede direta + instituições privadas explicitamente
    # vinculadas à política municipal, desde que sem problema técnico.
    incluir_universo_municipal_ampliado_2024 =
      escola_ativa_2024 &
      (
        dependencia_administrativa_2024 == "Municipal" |
          vinculo_privado_explicito
      ) &
      !coalesce(requer_revisao_tecnica, FALSE),

    # Contexto utilizável: qualquer escola ativa e tecnicamente consistente.
    incluir_contexto_descritivo_2024 =
      escola_ativa_2024 &
      !coalesce(requer_revisao_tecnica, FALSE),

    motivo_nao_inclusao_universo_estrito = case_when(
      incluir_universo_municipal_direto_2024 ~ NA_character_,

      !escola_ativa_2024 ~
        "Escola não estava em atividade no Censo 2024",

      coalesce(requer_revisao_tecnica, FALSE) ~
        coalesce(
          motivo_revisao_tecnica,
          "Caso com revisão técnica pendente"
        ),

      dependencia_administrativa_2024 == "Estadual" ~
        "Dependência estadual no Censo 2024",

      dependencia_administrativa_2024 == "Privada" ~
        "Dependência privada no Censo 2024",

      dependencia_administrativa_2024 == "Federal" ~
        "Dependência federal no Censo 2024",

      TRUE ~
        "Dependência administrativa não classificada"
    )
  )

# -------------------------------------------------------------------
# 6. Casos para revisão administrativa manual
# -------------------------------------------------------------------
#
# A revisão não é necessária para:
#   - Espírito Santo e Evaristo Gonçalves Netto, pois a divergência
#     estadual/municipalização já está explicitamente registrada;
#   - Pequena Casa da Criança e Aldeia Lumiar, pois o vínculo privado
#     especial já foi classificado.
#
# Permanecem como divergências cadastrais:
#   - escola municipal em 2024 marcada como "nova";
#   - escola estadual ou privada em 2024 marcada como municipal direta.
# -------------------------------------------------------------------

revisao_administrativa <- contexto_classificado |>
  filter(divergencia_censo_cadastro) |>
  transmute(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora,
    dependencia_administrativa_2024,
    situacao_funcionamento_2024,
    tipo_vinculo_rede_atual = tipo_vinculo_rede,
    status_rede_2025_atual = status_rede_2025,
    tipo_divergencia_administrativa,
    decisao_manual = "",
    tipo_vinculo_rede_corrigido = "",
    status_rede_2025_corrigido = "",
    observacao_manual = ""
  ) |>
  arrange(nome_canonico)

# -------------------------------------------------------------------
# 7. Diagnóstico dos universos
# -------------------------------------------------------------------

resumo_universos <- contexto_classificado |>
  summarise(
    escolas_dimensao = n_distinct(id_escola),

    escolas_ativas_2024 = n_distinct(
      id_escola[escola_ativa_2024]
    ),

    universo_municipal_direto_2024 = n_distinct(
      id_escola[incluir_universo_municipal_direto_2024]
    ),

    universo_municipal_ampliado_2024 = n_distinct(
      id_escola[incluir_universo_municipal_ampliado_2024]
    ),

    contexto_descritivo_utilizavel_2024 = n_distinct(
      id_escola[incluir_contexto_descritivo_2024]
    ),

    casos_revisao_tecnica = n_distinct(
      id_escola[coalesce(requer_revisao_tecnica, FALSE)]
    ),

    divergencias_censo_cadastro = n_distinct(
      id_escola[divergencia_censo_cadastro]
    )
  )

resumo_grupos <- contexto_classificado |>
  count(
    grupo_administrativo_2024,
    escola_ativa_2024,
    requer_revisao_tecnica,
    name = "numero_escolas"
  ) |>
  arrange(
    desc(numero_escolas),
    grupo_administrativo_2024
  )

resumo_por_carteira <- contexto_classificado |>
  group_by(assessora) |>
  summarise(
    numero_escolas = n_distinct(id_escola),

    municipais_diretas_2024 = n_distinct(
      id_escola[incluir_universo_municipal_direto_2024]
    ),

    universo_ampliado_2024 = n_distinct(
      id_escola[incluir_universo_municipal_ampliado_2024]
    ),

    estaduais_2024 = n_distinct(
      id_escola[
        dependencia_administrativa_2024 == "Estadual"
      ]
    ),

    privadas_2024 = n_distinct(
      id_escola[
        dependencia_administrativa_2024 == "Privada"
      ]
    ),

    revisao_tecnica = n_distinct(
      id_escola[
        coalesce(requer_revisao_tecnica, FALSE)
      ]
    ),

    divergencias_cadastrais = n_distinct(
      id_escola[divergencia_censo_cadastro]
    ),

    .groups = "drop"
  ) |>
  arrange(assessora)

# -------------------------------------------------------------------
# 8. Validações
# -------------------------------------------------------------------

duplicidades <- contexto_classificado |>
  count(
    id_escola,
    name = "numero_linhas"
  ) |>
  filter(numero_linhas > 1)

if (nrow(duplicidades) > 0) {
  write_csv(
    duplicidades,
    here(
      "documentacao",
      "contexto_inep_2024",
      "19A_duplicidades_classificacao_administrativa.csv"
    ),
    na = ""
  )

  stop(
    "Há mais de uma linha por id_escola. ",
    "Consulte 19A_duplicidades_classificacao_administrativa.csv."
  )
}

sobreposicao_invalida <- contexto_classificado |>
  filter(
    incluir_universo_municipal_direto_2024 &
      !incluir_universo_municipal_ampliado_2024
  )

if (nrow(sobreposicao_invalida) > 0) {
  stop(
    "Falha lógica: o universo municipal direto não está contido ",
    "no universo municipal ampliado."
  )
}

# -------------------------------------------------------------------
# 9. Exportação
# -------------------------------------------------------------------

write_csv(
  contexto_classificado,
  here(
    "dados_processados",
    "dim_contexto_escola_2024_classificada.csv"
  ),
  na = ""
)

write_csv(
  revisao_administrativa,
  here(
    "documentacao",
    "contexto_inep_2024",
    "19_revisao_manual_status_administrativo_2024.csv"
  ),
  na = ""
)

write_csv(
  resumo_universos,
  here(
    "documentacao",
    "contexto_inep_2024",
    "20_resumo_universos_analiticos_2024.csv"
  ),
  na = ""
)

write_csv(
  resumo_grupos,
  here(
    "documentacao",
    "contexto_inep_2024",
    "21_resumo_grupos_administrativos_2024.csv"
  ),
  na = ""
)

write_csv(
  resumo_por_carteira,
  here(
    "resultados",
    "contexto",
    "perfil_administrativo_carteiras_2024.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 10. Resumo no console
# -------------------------------------------------------------------

cat(
  "\nClassificação administrativa de 2024 concluída.\n"
)

cat(
  "\nUniversos analíticos:\n"
)

print(
  resumo_universos,
  width = Inf
)

cat(
  "\nGrupos administrativos observados em 2024:\n"
)

print(
  resumo_grupos,
  n = Inf,
  width = Inf
)

cat(
  "\nCasos para revisão administrativa manual:\n"
)

print(
  revisao_administrativa |>
    select(
      id_escola,
      nome_canonico,
      dependencia_administrativa_2024,
      status_rede_2025_atual,
      tipo_divergencia_administrativa
    ),
  n = Inf,
  width = Inf
)

cat(
  "\nArquivos gerados:\n",
  "- dados_processados/dim_contexto_escola_2024_classificada.csv\n",
  "- documentacao/contexto_inep_2024/19_revisao_manual_status_administrativo_2024.csv\n",
  "- documentacao/contexto_inep_2024/20_resumo_universos_analiticos_2024.csv\n",
  "- documentacao/contexto_inep_2024/21_resumo_grupos_administrativos_2024.csv\n",
  "- resultados/contexto/perfil_administrativo_carteiras_2024.csv\n"
)
