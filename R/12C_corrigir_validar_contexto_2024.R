library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Objetivo
# -------------------------------------------------------------------
#
# Corrigir e validar os produtos do módulo 12B sem repetir a consulta
# ao BigQuery.
#
# Correções principais:
#   1. calcular a mediana das matrículas antes de qualquer sobrescrita;
#   2. tratar tipo_situacao_funcionamento como código do Censo;
#   3. separar alertas técnicos, administrativos e de cobertura;
#   4. preservar como contexto histórico as escolas estadualizadas/
#      municipalizadas, conveniadas ou supervisionadas.
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 2. Diretórios e arquivos
# -------------------------------------------------------------------

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
  "dim_contexto_escola_2024.csv"
)

if (!file.exists(arquivo_contexto)) {
  stop(
    "Arquivo não encontrado: ",
    arquivo_contexto,
    "\nExecute primeiro R/12B_extrair_contexto_censo_2024.R."
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

mediana_segura <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  median(x)
}

media_segura <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  mean(x)
}

proporcao_segura <- function(numerador, denominador) {
  if (
    is.na(denominador) ||
      denominador <= 0
  ) {
    return(NA_real_)
  }

  100 * numerador / denominador
}

# -------------------------------------------------------------------
# 4. Leitura tipada
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
    rede = col_character(),
    tipo_situacao_funcionamento = col_character(),
    matriculas_anos_iniciais = col_double(),
    turmas_anos_iniciais = col_double(),
    docentes_anos_iniciais = col_double(),
    quantidade_matricula_fundamental_anos_iniciais_integral = col_double(),
    quantidade_matricula_especial = col_double(),
    quantidade_matricula_educacao_basica = col_double(),
    quantidade_matricula_transporte_publico = col_double(),
    quantidade_matricula_cor_raca_preta = col_double(),
    quantidade_matricula_cor_raca_parda = col_double(),
    quantidade_matricula_cor_raca_indigena = col_double(),
    oferta_anos_finais = col_logical(),
    oferta_eja = col_logical(),
    possui_espaco_leitura = col_logical(),
    laboratorio_informatica = col_double(),
    quadra_esportes = col_double(),
    internet_aprendizagem = col_double(),
    indice_infraestrutura_basica = col_double(),
    tipo_vinculo_rede = col_character(),
    status_rede_2025 = col_character(),
    .default = col_guess()
  )
)

# -------------------------------------------------------------------
# 5. Padronização dos códigos administrativos
# -------------------------------------------------------------------
#
# Censo Escolar — situação de funcionamento:
#   1 = em atividade
#   2 = paralisada
#   3 = extinta
#   4 = extinta no ano anterior
#
# Rede/dependência administrativa:
#   1 = federal
#   2 = estadual
#   3 = municipal
#   4 = privada
# -------------------------------------------------------------------

contexto_validado <- contexto |>
  mutate(
    assessora = case_when(
      is.na(assessora) |
        str_squish(assessora) == "" ~
        "Sem vinculação informada",
      TRUE ~ str_squish(assessora)
    ),

    codigo_situacao_funcionamento = str_squish(
      as.character(tipo_situacao_funcionamento)
    ),

    situacao_funcionamento_2024 = case_when(
      codigo_situacao_funcionamento == "1" ~ "Em atividade",
      codigo_situacao_funcionamento == "2" ~ "Paralisada",
      codigo_situacao_funcionamento == "3" ~ "Extinta",
      codigo_situacao_funcionamento == "4" ~ "Extinta no ano anterior",
      is.na(codigo_situacao_funcionamento) |
        codigo_situacao_funcionamento == "" ~ "Sem informação",
      TRUE ~ paste0(
        "Código não classificado: ",
        codigo_situacao_funcionamento
      )
    ),

    codigo_rede_2024 = str_squish(as.character(rede)),

    dependencia_administrativa_2024 = case_when(
      codigo_rede_2024 == "1" ~ "Federal",
      codigo_rede_2024 == "2" ~ "Estadual",
      codigo_rede_2024 == "3" ~ "Municipal",
      codigo_rede_2024 == "4" ~ "Privada",
      is.na(codigo_rede_2024) |
        codigo_rede_2024 == "" ~ "Sem informação",
      TRUE ~ paste0("Código não classificado: ", codigo_rede_2024)
    ),

    alerta_sem_linha_2024 = is.na(ano),

    alerta_sem_matriculas_anos_iniciais =
      is.na(matriculas_anos_iniciais),

    alerta_sem_turmas_anos_iniciais =
      is.na(turmas_anos_iniciais),

    alerta_sem_docentes_anos_iniciais =
      is.na(docentes_anos_iniciais),

    alerta_matriculas_sem_turmas =
      !is.na(matriculas_anos_iniciais) &
      matriculas_anos_iniciais > 0 &
      !is.na(turmas_anos_iniciais) &
      turmas_anos_iniciais == 0,

    alerta_matriculas_sem_docentes =
      !is.na(matriculas_anos_iniciais) &
      matriculas_anos_iniciais > 0 &
      !is.na(docentes_anos_iniciais) &
      docentes_anos_iniciais == 0,

    alerta_nao_ativa_2024 =
      situacao_funcionamento_2024 != "Em atividade",

    caso_historico_administrativo = case_when(
      dependencia_administrativa_2024 == "Estadual" &
        status_rede_2025 == "vinculo_estadual_em_2025_a_confirmar" ~
        "Escola estadual no Censo 2024; municipalização posterior a confirmar",

      dependencia_administrativa_2024 == "Privada" &
        tipo_vinculo_rede ==
          "privada_conveniada_sob_supervisao_municipal" ~
        "Instituição privada conveniada ou supervisionada",

      dependencia_administrativa_2024 == "Privada" &
        tipo_vinculo_rede ==
          "filantropica_sob_supervisao_municipal" ~
        "Instituição filantrópica supervisionada",

      dependencia_administrativa_2024 == "Municipal" ~
        "Rede municipal direta em 2024",

      TRUE ~
        "Situação administrativa a examinar"
    ),

    requer_revisao_tecnica =
      alerta_sem_linha_2024 |
      alerta_sem_matriculas_anos_iniciais |
      alerta_sem_turmas_anos_iniciais |
      alerta_sem_docentes_anos_iniciais |
      alerta_matriculas_sem_turmas |
      alerta_matriculas_sem_docentes |
      alerta_nao_ativa_2024,

    motivo_revisao_tecnica = pmap_chr(
      list(
        alerta_sem_linha_2024,
        alerta_sem_matriculas_anos_iniciais,
        alerta_sem_turmas_anos_iniciais,
        alerta_sem_docentes_anos_iniciais,
        alerta_matriculas_sem_turmas,
        alerta_matriculas_sem_docentes,
        alerta_nao_ativa_2024
      ),
      function(
          sem_linha,
          sem_matriculas,
          sem_turmas,
          sem_docentes,
          matriculas_sem_turmas,
          matriculas_sem_docentes,
          nao_ativa
      ) {
        motivos <- c(
          if (isTRUE(sem_linha)) "Sem linha no Censo 2024",
          if (isTRUE(sem_matriculas)) "Matrículas ausentes",
          if (isTRUE(sem_turmas)) "Turmas ausentes",
          if (isTRUE(sem_docentes)) "Docentes ausentes",
          if (isTRUE(matriculas_sem_turmas))
            "Matrículas positivas, mas nenhuma turma dos anos iniciais",
          if (isTRUE(matriculas_sem_docentes))
            "Matrículas positivas, mas nenhum docente dos anos iniciais",
          if (isTRUE(nao_ativa)) "Situação de funcionamento diferente de ativa"
        )

        if (length(motivos) == 0) {
          return(NA_character_)
        }

        paste(motivos, collapse = " | ")
      }
    )
  )

# -------------------------------------------------------------------
# 6. Perfil agregado corrigido das carteiras
# -------------------------------------------------------------------
#
# As variáveis por escola são renomeadas antes de summarise(). Isso
# impede que totais recém-criados sobrescrevam as colunas usadas para
# calcular mediana e outras estatísticas.
# -------------------------------------------------------------------

base_perfil <- contexto_validado |>
  transmute(
    assessora,
    id_escola,
    matriculas_escola = matriculas_anos_iniciais,
    turmas_escola = turmas_anos_iniciais,
    docentes_escola = docentes_anos_iniciais,
    matriculas_integral_escola =
      quantidade_matricula_fundamental_anos_iniciais_integral,
    matriculas_especial_escola = quantidade_matricula_especial,
    matriculas_educacao_basica_escola =
      quantidade_matricula_educacao_basica,
    indice_infraestrutura_escola = indice_infraestrutura_basica,
    oferta_anos_finais,
    oferta_eja,
    possui_espaco_leitura,
    laboratorio_informatica,
    quadra_esportes,
    internet_aprendizagem
  )

perfil_contextual_carteiras_corrigido <- base_perfil |>
  group_by(assessora) |>
  summarise(
    numero_escolas = n_distinct(id_escola),

    matriculas_anos_iniciais = sum(
      matriculas_escola,
      na.rm = TRUE
    ),

    media_matriculas_anos_iniciais = media_segura(
      matriculas_escola
    ),

    mediana_matriculas_anos_iniciais = mediana_segura(
      matriculas_escola
    ),

    minimo_matriculas_anos_iniciais = if_else(
      all(is.na(matriculas_escola)),
      NA_real_,
      min(matriculas_escola, na.rm = TRUE)
    ),

    maximo_matriculas_anos_iniciais = if_else(
      all(is.na(matriculas_escola)),
      NA_real_,
      max(matriculas_escola, na.rm = TRUE)
    ),

    turmas_anos_iniciais = sum(
      turmas_escola,
      na.rm = TRUE
    ),

    docentes_anos_iniciais = sum(
      docentes_escola,
      na.rm = TRUE
    ),

    media_alunos_por_turma = if_else(
      turmas_anos_iniciais > 0,
      matriculas_anos_iniciais / turmas_anos_iniciais,
      NA_real_
    ),

    media_alunos_por_docente = if_else(
      docentes_anos_iniciais > 0,
      matriculas_anos_iniciais / docentes_anos_iniciais,
      NA_real_
    ),

    pct_matriculas_integral = proporcao_segura(
      sum(matriculas_integral_escola, na.rm = TRUE),
      matriculas_anos_iniciais
    ),

    pct_matriculas_educacao_especial = proporcao_segura(
      sum(matriculas_especial_escola, na.rm = TRUE),
      sum(matriculas_educacao_basica_escola, na.rm = TRUE)
    ),

    indice_infraestrutura_medio = media_segura(
      indice_infraestrutura_escola
    ),

    escolas_com_anos_finais = sum(
      oferta_anos_finais %in% TRUE,
      na.rm = TRUE
    ),

    escolas_com_eja = sum(
      oferta_eja %in% TRUE,
      na.rm = TRUE
    ),

    escolas_com_tempo_integral = sum(
      !is.na(matriculas_integral_escola) &
        matriculas_integral_escola > 0,
      na.rm = TRUE
    ),

    escolas_com_espaco_leitura = sum(
      possui_espaco_leitura %in% TRUE,
      na.rm = TRUE
    ),

    escolas_com_laboratorio_informatica = sum(
      laboratorio_informatica == 1,
      na.rm = TRUE
    ),

    escolas_com_quadra = sum(
      quadra_esportes == 1,
      na.rm = TRUE
    ),

    escolas_com_internet_aprendizagem = sum(
      internet_aprendizagem == 1,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) |>
  arrange(assessora)

# -------------------------------------------------------------------
# 7. Diagnósticos corrigidos
# -------------------------------------------------------------------

casos_revisao_tecnica <- contexto_validado |>
  filter(requer_revisao_tecnica) |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora,
    ano,
    dependencia_administrativa_2024,
    situacao_funcionamento_2024,
    matriculas_anos_iniciais,
    turmas_anos_iniciais,
    docentes_anos_iniciais,
    tipo_vinculo_rede,
    status_rede_2025,
    caso_historico_administrativo,
    motivo_revisao_tecnica
  ) |>
  arrange(nome_canonico)

casos_historico_administrativo <- contexto_validado |>
  filter(
    dependencia_administrativa_2024 != "Municipal" |
      status_rede_2025 != "rede_municipal_direta"
  ) |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora,
    dependencia_administrativa_2024,
    situacao_funcionamento_2024,
    tipo_vinculo_rede,
    status_rede_2025,
    caso_historico_administrativo,
    requer_revisao_tecnica,
    motivo_revisao_tecnica
  ) |>
  arrange(nome_canonico)

resumo_validacao_corrigida <- tibble(
  indicador = c(
    "Escolas na base contextual",
    "Escolas em atividade em 2024",
    "Escolas com revisão técnica necessária",
    "Escolas com matrículas positivas e zero turmas",
    "Escolas com matrículas positivas e zero docentes",
    "Escolas municipais em 2024",
    "Escolas estaduais em 2024",
    "Escolas privadas em 2024",
    "Casos com histórico administrativo especial"
  ),
  valor = c(
    n_distinct(contexto_validado$id_escola),
    sum(
      contexto_validado$situacao_funcionamento_2024 == "Em atividade",
      na.rm = TRUE
    ),
    sum(contexto_validado$requer_revisao_tecnica, na.rm = TRUE),
    sum(contexto_validado$alerta_matriculas_sem_turmas, na.rm = TRUE),
    sum(contexto_validado$alerta_matriculas_sem_docentes, na.rm = TRUE),
    sum(
      contexto_validado$dependencia_administrativa_2024 == "Municipal",
      na.rm = TRUE
    ),
    sum(
      contexto_validado$dependencia_administrativa_2024 == "Estadual",
      na.rm = TRUE
    ),
    sum(
      contexto_validado$dependencia_administrativa_2024 == "Privada",
      na.rm = TRUE
    ),
    nrow(casos_historico_administrativo)
  )
)

# -------------------------------------------------------------------
# 8. Exportação
# -------------------------------------------------------------------

write_csv(
  contexto_validado,
  here(
    "dados_processados",
    "dim_contexto_escola_2024_validada.csv"
  ),
  na = ""
)

write_csv(
  perfil_contextual_carteiras_corrigido,
  here(
    "resultados",
    "contexto",
    "perfil_contextual_carteiras_2024_corrigido.csv"
  ),
  na = ""
)

write_csv(
  resumo_validacao_corrigida,
  here(
    "documentacao",
    "contexto_inep_2024",
    "16_resumo_validacao_contexto_2024_corrigido.csv"
  ),
  na = ""
)

write_csv(
  casos_revisao_tecnica,
  here(
    "documentacao",
    "contexto_inep_2024",
    "17_casos_revisao_tecnica_contexto_2024.csv"
  ),
  na = ""
)

write_csv(
  casos_historico_administrativo,
  here(
    "documentacao",
    "contexto_inep_2024",
    "18_casos_historico_administrativo_2024.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 9. Resumo no console
# -------------------------------------------------------------------

cat("\nCorreção e validação do contexto de 2024 concluídas.\n")

cat("\nResumo da validação corrigida:\n")
print(resumo_validacao_corrigida, n = Inf)

cat("\nCasos para revisão técnica:\n")
print(casos_revisao_tecnica, n = Inf)

cat("\nPerfil contextual corrigido das carteiras:\n")
print(
  perfil_contextual_carteiras_corrigido |>
    select(
      assessora,
      numero_escolas,
      matriculas_anos_iniciais,
      media_matriculas_anos_iniciais,
      mediana_matriculas_anos_iniciais,
      media_alunos_por_turma,
      media_alunos_por_docente,
      pct_matriculas_integral,
      indice_infraestrutura_medio
    ),
  n = Inf
)

cat("\nArquivos gerados:\n")
cat(
  "- dados_processados/dim_contexto_escola_2024_validada.csv\n",
  "- resultados/contexto/perfil_contextual_carteiras_2024_corrigido.csv\n",
  "- documentacao/contexto_inep_2024/16_resumo_validacao_contexto_2024_corrigido.csv\n",
  "- documentacao/contexto_inep_2024/17_casos_revisao_tecnica_contexto_2024.csv\n",
  "- documentacao/contexto_inep_2024/18_casos_historico_administrativo_2024.csv\n"
)
