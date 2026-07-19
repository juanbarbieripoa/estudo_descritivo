library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Objetivo
# -------------------------------------------------------------------
#
# Aplicar as decisões manuais sobre o status administrativo das escolas,
# consolidar a dimensão contextual final de 2024 e construir uma base
# analítica escola-série com:
#
#   - contexto estrutural pré-programa de 2024;
#   - resultados CAEd de 2025 e 2026;
#   - indicadores de participação e composição;
#   - universos administrativos explícitos.
#
# O módulo preserva cópias das dimensões anteriores e não altera os
# resultados educacionais já calculados.
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 2. Diretórios e arquivos
# -------------------------------------------------------------------

dir.create(
  here("dados_intermediarios", "historico"),
  recursive = TRUE,
  showWarnings = FALSE
)

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

arquivo_dim_escola <- here(
  "dados_intermediarios",
  "dim_escola.csv"
)

arquivo_contexto_classificado <- here(
  "dados_processados",
  "dim_contexto_escola_2024_classificada.csv"
)

arquivo_revisao_preenchida <- here(
  "documentacao",
  "contexto_inep_2024",
  "19_revisao_manual_status_administrativo_2024_preenchido.csv"
)

arquivo_variacao <- here(
  "dados_processados",
  "variacao_escola_serie_com_qualidade.csv"
)

arquivos_necessarios <- c(
  arquivo_dim_escola,
  arquivo_contexto_classificado,
  arquivo_revisao_preenchida,
  arquivo_variacao
)

arquivos_ausentes <- arquivos_necessarios[
  !file.exists(arquivos_necessarios)
]

if (length(arquivos_ausentes) > 0) {
  stop(
    "Arquivos necessários não encontrados:\n",
    paste(arquivos_ausentes, collapse = "\n")
  )
}

# -------------------------------------------------------------------
# 3. Funções auxiliares
# -------------------------------------------------------------------

primeiro_nao_vazio <- function(x) {
  x <- as.character(x)
  x <- x[
    !is.na(x) &
      str_squish(x) != ""
  ]

  if (length(x) == 0) {
    return(NA_character_)
  }

  x[[1]]
}

mediana_segura <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  median(x)
}

media_ponderada_segura <- function(x, peso) {
  validos <- !is.na(x) &
    !is.na(peso) &
    peso > 0

  if (!any(validos)) {
    return(NA_real_)
  }

  weighted.mean(
    x[validos],
    peso[validos]
  )
}

# -------------------------------------------------------------------
# 4. Leitura
# -------------------------------------------------------------------

dim_escola <- read_csv(
  arquivo_dim_escola,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

contexto <- read_csv(
  arquivo_contexto_classificado,
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
    requer_revisao_tecnica = col_logical(),
    motivo_revisao_tecnica = col_character(),
    .default = col_guess()
  )
)

revisao <- read_csv(
  arquivo_revisao_preenchida,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
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
    delta_participacao = col_double(),
    delta_proficiencia = col_double(),
    delta_pct_defasagem = col_double(),
    delta_pct_intermediario = col_double(),
    delta_pct_adequado = col_double(),
    variacao_relativa_previstos = col_double(),
    observacao_composicao = col_character(),
    .default = col_guess()
  )
)

# -------------------------------------------------------------------
# 5. Validação da revisão manual
# -------------------------------------------------------------------

colunas_revisao_obrigatorias <- c(
  "id_escola",
  "codigo_inep",
  "decisao_manual",
  "tipo_vinculo_rede_corrigido",
  "status_rede_2025_corrigido",
  "observacao_manual"
)

colunas_ausentes_revisao <- setdiff(
  colunas_revisao_obrigatorias,
  names(revisao)
)

if (length(colunas_ausentes_revisao) > 0) {
  stop(
    "Colunas ausentes no arquivo de revisão preenchido:\n",
    paste(colunas_ausentes_revisao, collapse = "\n")
  )
}

decisoes_validas <- c(
  "manter",
  "corrigir",
  "confirmar_com_ressalva",
  "pendente"
)

revisao <- revisao |>
  mutate(
    decisao_manual = str_to_lower(
      str_squish(decisao_manual)
    ),
    across(
      c(
        tipo_vinculo_rede_corrigido,
        status_rede_2025_corrigido,
        observacao_manual
      ),
      ~ na_if(str_squish(.x), "")
    )
  )

decisoes_invalidas <- revisao |>
  filter(
    is.na(decisao_manual) |
      !decisao_manual %in% decisoes_validas
  )

if (nrow(decisoes_invalidas) > 0) {
  write_csv(
    decisoes_invalidas,
    here(
      "documentacao",
      "contexto_inep_2024",
      "22A_decisoes_manuais_invalidas.csv"
    ),
    na = ""
  )

  stop(
    "Há decisões manuais inválidas. ",
    "Consulte 22A_decisoes_manuais_invalidas.csv."
  )
}

revisoes_duplicadas <- revisao |>
  count(
    id_escola,
    name = "numero_linhas"
  ) |>
  filter(numero_linhas > 1)

if (nrow(revisoes_duplicadas) > 0) {
  stop(
    "Há mais de uma decisão manual para a mesma escola."
  )
}

revisoes_incompletas <- revisao |>
  filter(
    decisao_manual %in% c(
      "corrigir",
      "confirmar_com_ressalva"
    ),
    is.na(tipo_vinculo_rede_corrigido) |
      is.na(status_rede_2025_corrigido)
  )

if (nrow(revisoes_incompletas) > 0) {
  stop(
    "Há decisões de correção sem os campos corrigidos preenchidos."
  )
}

# -------------------------------------------------------------------
# 6. Aplicação das decisões à dimensão contextual
# -------------------------------------------------------------------

revisao_aplicavel <- revisao |>
  select(
    id_escola,
    codigo_inep_revisao = codigo_inep,
    decisao_manual,
    tipo_vinculo_rede_corrigido,
    status_rede_2025_corrigido,
    observacao_manual
  )

contexto_final <- contexto |>
  left_join(
    revisao_aplicavel,
    by = "id_escola"
  ) |>
  mutate(
    decisao_manual = coalesce(
      decisao_manual,
      "sem_revisao_necessaria"
    ),

    tipo_vinculo_rede_final = case_when(
      decisao_manual %in% c(
        "corrigir",
        "confirmar_com_ressalva"
      ) ~ tipo_vinculo_rede_corrigido,

      TRUE ~ tipo_vinculo_rede
    ),

    status_rede_2025_final = case_when(
      decisao_manual %in% c(
        "corrigir",
        "confirmar_com_ressalva"
      ) ~ status_rede_2025_corrigido,

      TRUE ~ status_rede_2025
    ),

    observacao_administrativa_final = case_when(
      !is.na(observacao_manual) ~ observacao_manual,
      TRUE ~ caso_historico_administrativo
    ),

    escola_ativa_2024 =
      situacao_funcionamento_2024 == "Em atividade",

    privada_vinculada_final =
      tipo_vinculo_rede_final %in% c(
        "privada_conveniada_sob_supervisao_municipal",
        "filantropica_sob_supervisao_municipal"
      ),

    municipalizada_apos_2024 =
      tipo_vinculo_rede_final ==
        "municipalizada_apos_2024",

    grupo_administrativo_2024_final = case_when(
      dependencia_administrativa_2024 == "Municipal" ~
        "Rede municipal direta em 2024",

      dependencia_administrativa_2024 == "Estadual" &
        municipalizada_apos_2024 ~
        "Estadual em 2024, municipalizada posteriormente",

      dependencia_administrativa_2024 == "Estadual" ~
        "Rede estadual em 2024",

      dependencia_administrativa_2024 == "Privada" &
        tipo_vinculo_rede_final ==
          "privada_conveniada_sob_supervisao_municipal" ~
        "Privada conveniada ou supervisionada",

      dependencia_administrativa_2024 == "Privada" &
        tipo_vinculo_rede_final ==
          "filantropica_sob_supervisao_municipal" ~
        "Filantrópica supervisionada",

      dependencia_administrativa_2024 == "Privada" ~
        "Privada sem vínculo municipal classificado",

      TRUE ~
        "Outra situação administrativa"
    ),

    # Universo administrativo: não depende da disponibilidade de uma
    # variável estrutural específica.
    incluir_universo_municipal_direto_2024 =
      escola_ativa_2024 &
      dependencia_administrativa_2024 == "Municipal",

    incluir_universo_municipal_ampliado_2024 =
      escola_ativa_2024 &
      (
        dependencia_administrativa_2024 == "Municipal" |
          privada_vinculada_final
      ),

    incluir_rede_municipal_operacional_2025 =
      status_rede_2025_final ==
        "rede_municipal_direta",

    # Disponibilidade variável-específica.
    utilizar_indicadores_matriculas =
      escola_ativa_2024 &
      !is.na(matriculas_anos_iniciais),

    utilizar_indicadores_turmas_docentes =
      escola_ativa_2024 &
      !coalesce(requer_revisao_tecnica, FALSE) &
      !is.na(turmas_anos_iniciais) &
      !is.na(docentes_anos_iniciais),

    utilizar_indicadores_infraestrutura =
      escola_ativa_2024 &
      !is.na(indice_infraestrutura_basica)
  ) |>
  select(
    -codigo_inep_revisao
  )

# -------------------------------------------------------------------
# 7. Atualização controlada da dimensão de escolas
# -------------------------------------------------------------------

atualizacao_dim <- contexto_final |>
  select(
    id_escola,
    codigo_inep_contexto = codigo_inep,
    tipo_vinculo_rede_final,
    status_rede_2025_final,
    dependencia_administrativa_2024,
    situacao_funcionamento_2024,
    grupo_administrativo_2024_final,
    municipalizada_apos_2024,
    observacao_administrativa_final
  )

dim_escola_atualizada <- dim_escola |>
  left_join(
    atualizacao_dim,
    by = "id_escola"
  ) |>
  mutate(
    codigo_inep = coalesce(
      codigo_inep,
      codigo_inep_contexto
    ),

    tipo_vinculo_rede =
      tipo_vinculo_rede_final,

    status_rede_2025 = coalesce(
      status_rede_2025_final,
      status_rede_2025
    ),

    possivel_municipalizacao_recente = case_when(
      municipalizada_apos_2024 ~ "TRUE",
      TRUE ~ coalesce(
        possivel_municipalizacao_recente,
        "FALSE"
      )
    ),

    observacao_historico = coalesce(
      observacao_administrativa_final,
      observacao_historico
    )
  ) |>
  select(
    -codigo_inep_contexto,
    -tipo_vinculo_rede_final,
    -status_rede_2025_final,
    -municipalizada_apos_2024,
    -observacao_administrativa_final
  )

# Preserva uma cópia da dimensão imediatamente anterior ao módulo.
arquivo_backup_dim <- here(
  "dados_intermediarios",
  "historico",
  "dim_escola_antes_12E.csv"
)

if (!file.exists(arquivo_backup_dim)) {
  file.copy(
    arquivo_dim_escola,
    arquivo_backup_dim,
    overwrite = FALSE
  )
}

write_csv(
  dim_escola_atualizada,
  arquivo_dim_escola,
  na = ""
)

# -------------------------------------------------------------------
# 8. Base integrada escola-série
# -------------------------------------------------------------------

contexto_selecionado <- contexto_final |>
  select(
    id_escola,
    dependencia_administrativa_2024,
    situacao_funcionamento_2024,
    tipo_vinculo_rede_final,
    status_rede_2025_final,
    grupo_administrativo_2024_final,
    municipalizada_apos_2024,
    privada_vinculada_final,
    incluir_universo_municipal_direto_2024,
    incluir_universo_municipal_ampliado_2024,
    incluir_rede_municipal_operacional_2025,
    utilizar_indicadores_matriculas,
    utilizar_indicadores_turmas_docentes,
    utilizar_indicadores_infraestrutura,
    matriculas_anos_iniciais,
    turmas_anos_iniciais,
    docentes_anos_iniciais,
    alunos_por_turma_anos_iniciais,
    alunos_por_docente_anos_iniciais,
    porte_anos_iniciais,
    quartil_porte_anos_iniciais,
    pct_matriculas_anos_iniciais_integral,
    pct_matriculas_educacao_especial,
    pct_matriculas_transporte_publico,
    pct_matriculas_preta_parda_indigena,
    numero_etapas_amplas_ofertadas,
    indice_infraestrutura_basica,
    possui_espaco_leitura,
    possui_recurso_acessibilidade,
    requer_revisao_tecnica,
    motivo_revisao_tecnica,
    observacao_administrativa_final
  )

base_analitica_integrada <- variacao |>
  left_join(
    contexto_selecionado,
    by = "id_escola"
  ) |>
  mutate(
    contexto_2024_encontrado =
      !is.na(dependencia_administrativa_2024),

    peso_medio_avaliados =
      (avaliados_2025 + avaliados_2026) / 2,

    amostra_principal_descritiva =
      painel_resultado_balanceado &
      contexto_2024_encontrado,

    amostra_municipal_direta_2024 =
      amostra_principal_descritiva &
      incluir_universo_municipal_direto_2024,

    amostra_municipal_ampliada_2024 =
      amostra_principal_descritiva &
      incluir_universo_municipal_ampliado_2024,

    amostra_rede_operacional_2025 =
      amostra_principal_descritiva &
      incluir_rede_municipal_operacional_2025
  ) |>
  arrange(
    ano_escolar,
    nome_canonico
  )

# -------------------------------------------------------------------
# 9. Diagnósticos da integração
# -------------------------------------------------------------------

validacao_integracao <- base_analitica_integrada |>
  summarise(
    linhas_escola_serie = n(),

    escolas = n_distinct(id_escola),

    linhas_com_contexto_2024 = sum(
      contexto_2024_encontrado,
      na.rm = TRUE
    ),

    linhas_sem_contexto_2024 = sum(
      !contexto_2024_encontrado,
      na.rm = TRUE
    ),

    escola_series_resultado_balanceado = sum(
      painel_resultado_balanceado,
      na.rm = TRUE
    ),

    amostra_principal_descritiva = sum(
      amostra_principal_descritiva,
      na.rm = TRUE
    ),

    amostra_municipal_direta_2024 = sum(
      amostra_municipal_direta_2024,
      na.rm = TRUE
    ),

    amostra_municipal_ampliada_2024 = sum(
      amostra_municipal_ampliada_2024,
      na.rm = TRUE
    ),

    amostra_rede_operacional_2025 = sum(
      amostra_rede_operacional_2025,
      na.rm = TRUE
    )
  )

resumo_universos_escolas <- contexto_final |>
  summarise(
    escolas_contexto = n_distinct(id_escola),

    municipais_diretas_2024 = n_distinct(
      id_escola[
        incluir_universo_municipal_direto_2024
      ]
    ),

    municipais_ampliadas_2024 = n_distinct(
      id_escola[
        incluir_universo_municipal_ampliado_2024
      ]
    ),

    rede_operacional_2025 = n_distinct(
      id_escola[
        incluir_rede_municipal_operacional_2025
      ]
    ),

    privadas_vinculadas = n_distinct(
      id_escola[privada_vinculada_final]
    ),

    municipalizadas_apos_2024 = n_distinct(
      id_escola[municipalizada_apos_2024]
    ),

    revisao_tecnica = n_distinct(
      id_escola[
        coalesce(requer_revisao_tecnica, FALSE)
      ]
    )
  )

pendencias_pos_revisao <- contexto_final |>
  filter(
    decisao_manual == "pendente" |
      is.na(tipo_vinculo_rede_final) |
      is.na(status_rede_2025_final)
  ) |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    decisao_manual,
    tipo_vinculo_rede_final,
    status_rede_2025_final,
    observacao_administrativa_final
  )

# -------------------------------------------------------------------
# 10. Perfil contextual final das carteiras
# -------------------------------------------------------------------

perfil_contextual_carteiras_final <- contexto_final |>
  filter(
    incluir_universo_municipal_ampliado_2024
  ) |>
  mutate(
    assessora = case_when(
      is.na(assessora) |
        str_squish(assessora) == "" ~
        "Sem vinculação informada",
      TRUE ~ str_squish(assessora)
    )
  ) |>
  group_by(assessora) |>
  summarise(
    numero_escolas = n_distinct(id_escola),

    escolas_rede_direta_2024 = sum(
      incluir_universo_municipal_direto_2024,
      na.rm = TRUE
    ),

    escolas_privadas_vinculadas = sum(
      privada_vinculada_final,
      na.rm = TRUE
    ),

    matriculas_total_anos_iniciais = sum(
      .data$matriculas_anos_iniciais[
        .data$utilizar_indicadores_matriculas
      ],
      na.rm = TRUE
    ),

    mediana_matriculas_anos_iniciais =
      mediana_segura(
        .data$matriculas_anos_iniciais[
          .data$utilizar_indicadores_matriculas
        ]
      ),

    turmas_total_anos_iniciais = sum(
      .data$turmas_anos_iniciais[
        .data$utilizar_indicadores_turmas_docentes
      ],
      na.rm = TRUE
    ),

    docentes_total_anos_iniciais = sum(
      .data$docentes_anos_iniciais[
        .data$utilizar_indicadores_turmas_docentes
      ],
      na.rm = TRUE
    ),

    infraestrutura_media =
      mean(
        .data$indice_infraestrutura_basica[
          .data$utilizar_indicadores_infraestrutura
        ],
        na.rm = TRUE
      ),

    pct_integral_medio_ponderado =
      media_ponderada_segura(
        .data$pct_matriculas_anos_iniciais_integral,
        .data$matriculas_anos_iniciais
      ),

    pct_educacao_especial_medio_ponderado =
      media_ponderada_segura(
        .data$pct_matriculas_educacao_especial,
        .data$matriculas_anos_iniciais
      ),

    .groups = "drop"
  ) |>
  mutate(
    alunos_por_turma_agregado = if_else(
      turmas_total_anos_iniciais > 0,
      matriculas_total_anos_iniciais /
        turmas_total_anos_iniciais,
      NA_real_
    ),

    alunos_por_docente_agregado = if_else(
      docentes_total_anos_iniciais > 0,
      matriculas_total_anos_iniciais /
        docentes_total_anos_iniciais,
      NA_real_
    )
  ) |>
  arrange(assessora)

# -------------------------------------------------------------------
# 11. Exportação
# -------------------------------------------------------------------

write_csv(
  contexto_final,
  here(
    "dados_processados",
    "dim_contexto_escola_2024_final.csv"
  ),
  na = ""
)

write_csv(
  base_analitica_integrada,
  here(
    "dados_processados",
    "base_analitica_escola_serie_contexto_2024.csv"
  ),
  na = ""
)

write_csv(
  validacao_integracao,
  here(
    "documentacao",
    "contexto_inep_2024",
    "22_validacao_integracao_contexto_resultados.csv"
  ),
  na = ""
)

write_csv(
  resumo_universos_escolas,
  here(
    "documentacao",
    "contexto_inep_2024",
    "23_resumo_universos_finais_2024_2025.csv"
  ),
  na = ""
)

write_csv(
  pendencias_pos_revisao,
  here(
    "documentacao",
    "contexto_inep_2024",
    "24_pendencias_pos_revisao_administrativa.csv"
  ),
  na = ""
)

write_csv(
  perfil_contextual_carteiras_final,
  here(
    "resultados",
    "contexto",
    "perfil_contextual_carteiras_2024_final.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 12. Resumo no console
# -------------------------------------------------------------------

cat(
  "\nDecisões administrativas consolidadas.\n"
)

cat(
  "\nResumo dos universos finais:\n"
)

print(
  resumo_universos_escolas,
  width = Inf
)

cat(
  "\nValidação da base integrada:\n"
)

print(
  validacao_integracao,
  width = Inf
)

cat(
  "\nPendências após a revisão:\n"
)

print(
  pendencias_pos_revisao,
  n = Inf,
  width = Inf
)

cat(
  "\nArquivos gerados ou atualizados:\n",
  "- dados_intermediarios/dim_escola.csv\n",
  "- dados_intermediarios/historico/dim_escola_antes_12E.csv\n",
  "- dados_processados/dim_contexto_escola_2024_final.csv\n",
  "- dados_processados/base_analitica_escola_serie_contexto_2024.csv\n",
  "- documentacao/contexto_inep_2024/22_validacao_integracao_contexto_resultados.csv\n",
  "- documentacao/contexto_inep_2024/23_resumo_universos_finais_2024_2025.csv\n",
  "- documentacao/contexto_inep_2024/24_pendencias_pos_revisao_administrativa.csv\n",
  "- resultados/contexto/perfil_contextual_carteiras_2024_final.csv\n"
)
