library(here)
library(tidyverse)
library(basedosdados)
library(bigrquery)

# -------------------------------------------------------------------
# 1. Objetivo
# -------------------------------------------------------------------
#
# Extrair e organizar características estruturais pré-programa das
# escolas da dimensão local a partir do Censo Escolar de 2024.
#
# Unidade final:
#   id_escola x ano_contexto (2024)
#
# Este módulo produz variáveis de contexto. Elas não devem ser
# interpretadas como resultados do assessoramento.
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 2. Configuração e diretórios
# -------------------------------------------------------------------

arquivo_config <- here("config_bigquery.R")

if (!file.exists(arquivo_config)) {
  stop("Arquivo config_bigquery.R não encontrado na raiz do projeto.")
}

source(arquivo_config)

if (
  !exists("billing_project_id") ||
    is.na(billing_project_id) ||
    billing_project_id == "" ||
    str_detect(billing_project_id, "SUBSTITUA")
) {
  stop("Informe o ID do projeto Google Cloud em config_bigquery.R.")
}

# Usa credenciais já autenticadas/cacheadas. Se necessário, execute
# bigrquery::bq_auth() separadamente no console antes deste script.
basedosdados::set_billing_id(billing_project_id)

dir.create(here("dados_processados"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("resultados", "contexto"), recursive = TRUE, showWarnings = FALSE)
dir.create(
  here("documentacao", "contexto_inep_2024"),
  recursive = TRUE,
  showWarnings = FALSE
)

arquivo_dim <- here("dados_intermediarios", "dim_escola.csv")

if (!file.exists(arquivo_dim)) {
  stop(
    "Arquivo não encontrado: ", arquivo_dim,
    "\nExecute primeiro R/11B_consolidar_codigo_inep.R."
  )
}

# -------------------------------------------------------------------
# 3. Funções auxiliares
# -------------------------------------------------------------------

normalizar_integer64 <- function(df) {
  df |>
    mutate(
      across(
        where(bit64::is.integer64),
        as.numeric
      )
    )
}

primeiro_nao_vazio <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & str_squish(x) != ""]
  if (length(x) == 0) return(NA_character_)
  x[[1]]
}

proporcao_segura <- function(numerador, denominador, multiplicador = 100) {
  case_when(
    !is.na(numerador) & !is.na(denominador) & denominador > 0 ~
      multiplicador * numerador / denominador,
    TRUE ~ NA_real_
  )
}

media_ponderada_segura <- function(x, peso) {
  validos <- !is.na(x) & !is.na(peso) & peso > 0
  if (!any(validos)) return(NA_real_)
  weighted.mean(x[validos], peso[validos])
}

# -------------------------------------------------------------------
# 4. Dimensão local e validação dos códigos
# -------------------------------------------------------------------

dim_escola <- read_csv(
  arquivo_dim,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  mutate(
    codigo_inep = str_squish(codigo_inep),
    codigo_inep_valido = str_detect(codigo_inep, "^[0-9]{8}$")
  )

codigos_invalidos <- dim_escola |>
  filter(!codigo_inep_valido) |>
  select(id_escola, codigo_inep, nome_canonico, assessora)

if (nrow(codigos_invalidos) > 0) {
  write_csv(
    codigos_invalidos,
    here(
      "documentacao", "contexto_inep_2024",
      "10_codigos_invalidos_antes_extracao.csv"
    ),
    na = ""
  )
  stop(
    "Há escolas sem código INEP válido. Consulte ",
    "documentacao/contexto_inep_2024/10_codigos_invalidos_antes_extracao.csv."
  )
}

codigos_duplicados <- dim_escola |>
  count(codigo_inep, name = "numero_escolas") |>
  filter(numero_escolas > 1)

if (nrow(codigos_duplicados) > 0) {
  stop("Há códigos INEP associados a mais de uma escola na dimensão local.")
}

lista_codigos_sql <- paste0(
  "'",
  paste(sort(unique(dim_escola$codigo_inep)), collapse = "', '"),
  "'"
)

# -------------------------------------------------------------------
# 5. Consulta ao Censo Escolar de 2024
# -------------------------------------------------------------------
#
# As colunas abaixo foram confirmadas pelo inventário 12A.
# A consulta é restrita aos códigos da dimensão local para reduzir custo.
# -------------------------------------------------------------------

query_contexto <- paste0(
  "SELECT\n",
  "  ano,\n",
  "  CAST(id_escola AS STRING) AS codigo_inep,\n",
  "  sigla_uf, id_municipio, rede,\n",
  "  tipo_categoria_escola_privada,\n",
  "  tipo_localizacao, tipo_localizacao_diferenciada,\n",
  "  tipo_situacao_funcionamento,\n",
  "  vinculo_secretaria_educacao,\n",
  "  poder_publico_parceria, tipo_poder_publico_parceria,\n",
  "  conveniada_poder_publico, tipo_convenio_poder_publico,\n",
  "  etapa_ensino_infantil,\n",
  "  etapa_ensino_fundamental,\n",
  "  etapa_ensino_fundamental_anos_iniciais,\n",
  "  etapa_ensino_fundamental_anos_finais,\n",
  "  etapa_ensino_medio, etapa_ensino_eja, etapa_ensino_especial,\n",
  "  quantidade_matricula_educacao_basica,\n",
  "  quantidade_matricula_infantil,\n",
  "  quantidade_matricula_fundamental,\n",
  "  quantidade_matricula_fundamental_anos_iniciais,\n",
  "  quantidade_matricula_fundamental_1_ano,\n",
  "  quantidade_matricula_fundamental_2_ano,\n",
  "  quantidade_matricula_fundamental_3_ano,\n",
  "  quantidade_matricula_fundamental_4_ano,\n",
  "  quantidade_matricula_fundamental_5_ano,\n",
  "  quantidade_matricula_fundamental_anos_finais,\n",
  "  quantidade_matricula_eja,\n",
  "  quantidade_matricula_especial,\n",
  "  quantidade_matricula_especial_comum,\n",
  "  quantidade_matricula_especial_exclusiva,\n",
  "  quantidade_matricula_feminino,\n",
  "  quantidade_matricula_masculino,\n",
  "  quantidade_matricula_branca,\n",
  "  quantidade_matricula_preta,\n",
  "  quantidade_matricula_parda,\n",
  "  quantidade_matricula_amarela,\n",
  "  quantidade_matricula_indigena,\n",
  "  quantidade_matricula_nao_declarada,\n",
  "  quantidade_matricula_fundamental_integral,\n",
  "  quantidade_matricula_fundamental_anos_iniciais_integral,\n",
  "  quantidade_matricula_fundamental_anos_finais_integral,\n",
  "  quantidade_matricula_utiliza_transporte_publico,\n",
  "  quantidade_docente_educacao_basica,\n",
  "  quantidade_docente_fundamental,\n",
  "  quantidade_docente_fundamental_anos_iniciais,\n",
  "  quantidade_docente_fundamental_anos_finais,\n",
  "  quantidade_docente_especial,\n",
  "  quantidade_turma_educacao_basica,\n",
  "  quantidade_turma_fundamental,\n",
  "  quantidade_turma_fundamental_anos_iniciais,\n",
  "  quantidade_turma_fundamental_anos_finais,\n",
  "  quantidade_turma_fundamental_integral,\n",
  "  quantidade_sala_existente, quantidade_sala_utilizada,\n",
  "  quantidade_sala_utilizada_climatizada,\n",
  "  quantidade_sala_utilizada_acessivel,\n",
  "  agua_potavel, agua_rede_publica, energia_rede_publica,\n",
  "  esgoto_rede_publica,\n",
  "  biblioteca, biblioteca_sala_leitura, sala_leitura,\n",
  "  laboratorio_ciencias, laboratorio_informatica,\n",
  "  quadra_esportes, quadra_esportes_coberta,\n",
  "  internet, internet_aprendizagem, internet_alunos,\n",
  "  acessibilidade_rampas, acessibilidade_corrimao,\n",
  "  acessibilidade_pisos_tateis, acessibilidade_elevador,\n",
  "  acessibilidade_inexistente,\n",
  "  banheiro, banheiro_pne, cozinha, refeitorio, alimentacao,\n",
  "  sala_atendimento_especial,\n",
  "  quantidade_computador, quantidade_computador_aluno,\n",
  "  quantidade_computador_portatil_aluno, quantidade_tablet_aluno,\n",
  "  quantidade_profissional_bibliotecario,\n",
  "  quantidade_profissional_coordenador,\n",
  "  quantidade_profissional_psicologo,\n",
  "  quantidade_profissional_assistente_social,\n",
  "  quantidade_profissional_monitor,\n",
  "  quantidade_profissional_pedagogia\n",
  "FROM `basedosdados.br_inep_censo_escolar.escola`\n",
  "WHERE ano = 2024\n",
  "  AND CAST(id_escola AS STRING) IN (", lista_codigos_sql, ")"
)

contexto_bruto <- basedosdados::read_sql(query_contexto) |>
  as_tibble() |>
  normalizar_integer64() |>
  mutate(codigo_inep = as.character(codigo_inep))

# -------------------------------------------------------------------
# 6. Validação da extração
# -------------------------------------------------------------------

duplicidades_censo <- contexto_bruto |>
  count(ano, codigo_inep, name = "numero_linhas") |>
  filter(numero_linhas > 1)

if (nrow(duplicidades_censo) > 0) {
  write_csv(
    duplicidades_censo,
    here(
      "documentacao", "contexto_inep_2024",
      "11_duplicidades_censo_2024.csv"
    ),
    na = ""
  )
  stop(
    "A consulta retornou mais de uma linha para alguma escola em 2024. ",
    "Consulte 11_duplicidades_censo_2024.csv."
  )
}

escolas_nao_retornadas <- dim_escola |>
  anti_join(contexto_bruto, by = "codigo_inep") |>
  select(
    id_escola, codigo_inep, nome_canonico, assessora,
    any_of(c("tipo_vinculo_rede", "status_rede_2025"))
  )

write_csv(
  escolas_nao_retornadas,
  here(
    "documentacao", "contexto_inep_2024",
    "12_escolas_nao_retornadas_extracao.csv"
  ),
  na = ""
)

if (nrow(escolas_nao_retornadas) > 0) {
  warning(
    nrow(escolas_nao_retornadas),
    " escola(s) não foram retornadas pelo Censo Escolar de 2024."
  )
}

# -------------------------------------------------------------------
# 7. Integração com a dimensão local
# -------------------------------------------------------------------

contexto_escola_2024 <- dim_escola |>
  select(-codigo_inep_valido) |>
  left_join(contexto_bruto, by = "codigo_inep") |>
  mutate(
    ano_contexto = 2024L,
    fonte_contexto = "Censo Escolar/INEP via Base dos Dados",
    contexto_pre_programa = TRUE
  )

# -------------------------------------------------------------------
# 8. Indicadores derivados de porte, oferta e composição
# -------------------------------------------------------------------

contexto_escola_2024 <- contexto_escola_2024 |>
  mutate(
    matriculas_anos_iniciais = quantidade_matricula_fundamental_anos_iniciais,
    turmas_anos_iniciais = quantidade_turma_fundamental_anos_iniciais,
    docentes_anos_iniciais = quantidade_docente_fundamental_anos_iniciais,

    alunos_por_turma_anos_iniciais = case_when(
      !is.na(matriculas_anos_iniciais) &
        !is.na(turmas_anos_iniciais) &
        turmas_anos_iniciais > 0 ~
        matriculas_anos_iniciais / turmas_anos_iniciais,
      TRUE ~ NA_real_
    ),

    alunos_por_docente_anos_iniciais = case_when(
      !is.na(matriculas_anos_iniciais) &
        !is.na(docentes_anos_iniciais) &
        docentes_anos_iniciais > 0 ~
        matriculas_anos_iniciais / docentes_anos_iniciais,
      TRUE ~ NA_real_
    ),

    pct_matriculas_anos_iniciais_integral = proporcao_segura(
      quantidade_matricula_fundamental_anos_iniciais_integral,
      matriculas_anos_iniciais
    ),

    pct_matriculas_educacao_especial = proporcao_segura(
      quantidade_matricula_especial,
      quantidade_matricula_educacao_basica
    ),

    pct_matriculas_transporte_publico = proporcao_segura(
      quantidade_matricula_utiliza_transporte_publico,
      quantidade_matricula_educacao_basica
    ),

    pct_matriculas_preta_parda_indigena = proporcao_segura(
      rowSums(
        across(
          c(
            quantidade_matricula_preta,
            quantidade_matricula_parda,
            quantidade_matricula_indigena
          )
        ),
        na.rm = TRUE
      ),
      rowSums(
        across(
          c(
            quantidade_matricula_branca,
            quantidade_matricula_preta,
            quantidade_matricula_parda,
            quantidade_matricula_amarela,
            quantidade_matricula_indigena,
            quantidade_matricula_nao_declarada
          )
        ),
        na.rm = TRUE
      )
    ),

    oferta_educacao_infantil = etapa_ensino_infantil == 1,
    oferta_anos_iniciais = etapa_ensino_fundamental_anos_iniciais == 1,
    oferta_anos_finais = etapa_ensino_fundamental_anos_finais == 1,
    oferta_eja = etapa_ensino_eja == 1,
    oferta_educacao_especial = etapa_ensino_especial == 1,

    numero_etapas_amplas_ofertadas = rowSums(
      across(
        c(
          oferta_educacao_infantil,
          oferta_anos_iniciais,
          oferta_anos_finais,
          oferta_eja
        )
      ),
      na.rm = TRUE
    ),

    porte_anos_iniciais = case_when(
      is.na(matriculas_anos_iniciais) ~ "Sem informação",
      matriculas_anos_iniciais < 150 ~ "Até 149 matrículas",
      matriculas_anos_iniciais < 300 ~ "150 a 299 matrículas",
      matriculas_anos_iniciais < 500 ~ "300 a 499 matrículas",
      TRUE ~ "500 matrículas ou mais"
    ),

    porte_anos_iniciais = factor(
      porte_anos_iniciais,
      levels = c(
        "Até 149 matrículas",
        "150 a 299 matrículas",
        "300 a 499 matrículas",
        "500 matrículas ou mais",
        "Sem informação"
      ),
      ordered = TRUE
    )
  )

# Quartil relativo de porte dentro da rede analisada.
contexto_escola_2024 <- contexto_escola_2024 |>
  mutate(
    quartil_porte_anos_iniciais = case_when(
      is.na(matriculas_anos_iniciais) ~ NA_integer_,
      TRUE ~ ntile(matriculas_anos_iniciais, 4)
    )
  )

# -------------------------------------------------------------------
# 9. Indicadores derivados de infraestrutura
# -------------------------------------------------------------------

itens_infraestrutura <- c(
  "agua_potavel",
  "energia_rede_publica",
  "esgoto_rede_publica",
  "biblioteca_sala_leitura",
  "laboratorio_ciencias",
  "laboratorio_informatica",
  "quadra_esportes",
  "internet_aprendizagem",
  "banheiro_pne",
  "sala_atendimento_especial"
)

contexto_escola_2024 <- contexto_escola_2024 |>
  rowwise() |>
  mutate(
    numero_itens_infraestrutura_disponiveis = sum(
      !is.na(c_across(all_of(itens_infraestrutura)))
    ),
    numero_itens_infraestrutura_presentes = sum(
      c_across(all_of(itens_infraestrutura)) == 1,
      na.rm = TRUE
    ),
    indice_infraestrutura_basica = case_when(
      numero_itens_infraestrutura_disponiveis > 0 ~
        100 * numero_itens_infraestrutura_presentes /
        numero_itens_infraestrutura_disponiveis,
      TRUE ~ NA_real_
    ),
    possui_espaco_leitura = any(
      c_across(c(biblioteca, biblioteca_sala_leitura, sala_leitura)) == 1,
      na.rm = TRUE
    ),
    possui_recurso_acessibilidade = any(
      c_across(
        c(
          acessibilidade_rampas,
          acessibilidade_corrimao,
          acessibilidade_pisos_tateis,
          acessibilidade_elevador
        )
      ) == 1,
      na.rm = TRUE
    )
  ) |>
  ungroup()

# -------------------------------------------------------------------
# 10. Seleção e ordenação da dimensão contextual final
# -------------------------------------------------------------------

contexto_escola_2024 <- contexto_escola_2024 |>
  relocate(
    id_escola,
    codigo_inep,
    nome_canonico,
    nome_vinculo_original,
    assessora,
    any_of(c(
      "tipo_vinculo_rede",
      "status_rede_2025",
      "observacao_historico"
    )),
    ano_contexto,
    fonte_contexto,
    contexto_pre_programa,
    rede,
    tipo_categoria_escola_privada,
    tipo_localizacao,
    tipo_localizacao_diferenciada,
    tipo_situacao_funcionamento,
    poder_publico_parceria,
    tipo_poder_publico_parceria,
    conveniada_poder_publico,
    tipo_convenio_poder_publico,
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
    indice_infraestrutura_basica
  ) |>
  arrange(nome_canonico)

# -------------------------------------------------------------------
# 11. Perfil agregado das carteiras
# -------------------------------------------------------------------
#
# A agregação descreve a composição inicial das carteiras. Não mede
# desempenho nem efeito das assessoras.
# -------------------------------------------------------------------

perfil_contextual_carteiras <- contexto_escola_2024 |>
  mutate(
    assessora = case_when(
      is.na(assessora) | str_squish(assessora) == "" ~
        "Sem vinculação informada",
      TRUE ~ str_squish(assessora)
    )
  ) |>
  group_by(assessora) |>
  summarise(
    numero_escolas = n_distinct(id_escola),

    matriculas_anos_iniciais = sum(
      matriculas_anos_iniciais,
      na.rm = TRUE
    ),

    mediana_matriculas_anos_iniciais = median(
      matriculas_anos_iniciais,
      na.rm = TRUE
    ),

    media_alunos_por_turma = media_ponderada_segura(
      alunos_por_turma_anos_iniciais,
      turmas_anos_iniciais
    ),

    media_alunos_por_docente = media_ponderada_segura(
      alunos_por_docente_anos_iniciais,
      docentes_anos_iniciais
    ),

    pct_matriculas_integral = proporcao_segura(
      sum(quantidade_matricula_fundamental_anos_iniciais_integral, na.rm = TRUE),
      sum(matriculas_anos_iniciais, na.rm = TRUE)
    ),

    pct_matriculas_educacao_especial = proporcao_segura(
      sum(quantidade_matricula_especial, na.rm = TRUE),
      sum(quantidade_matricula_educacao_basica, na.rm = TRUE)
    ),

    indice_infraestrutura_medio = mean(
      indice_infraestrutura_basica,
      na.rm = TRUE
    ),

    escolas_com_anos_finais = sum(oferta_anos_finais, na.rm = TRUE),
    escolas_com_eja = sum(oferta_eja, na.rm = TRUE),
    escolas_com_tempo_integral = sum(
      quantidade_matricula_fundamental_anos_iniciais_integral > 0,
      na.rm = TRUE
    ),
    escolas_com_espaco_leitura = sum(possui_espaco_leitura, na.rm = TRUE),
    escolas_com_laboratorio_informatica = sum(
      laboratorio_informatica == 1,
      na.rm = TRUE
    ),
    escolas_com_quadra = sum(quadra_esportes == 1, na.rm = TRUE),
    escolas_com_internet_aprendizagem = sum(
      internet_aprendizagem == 1,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) |>
  arrange(assessora)

# -------------------------------------------------------------------
# 12. Diagnósticos de qualidade e disponibilidade
# -------------------------------------------------------------------

validacao_contexto <- tibble(
  indicador = c(
    "Escolas na dimensão local",
    "Escolas retornadas pelo Censo 2024",
    "Escolas sem linha no Censo 2024",
    "Duplicidades escola-ano",
    "Escolas sem matrículas dos anos iniciais",
    "Escolas sem turmas dos anos iniciais",
    "Escolas sem docentes dos anos iniciais"
  ),
  valor = c(
    n_distinct(dim_escola$id_escola),
    n_distinct(contexto_bruto$codigo_inep),
    nrow(escolas_nao_retornadas),
    nrow(duplicidades_censo),
    sum(is.na(contexto_escola_2024$matriculas_anos_iniciais)),
    sum(is.na(contexto_escola_2024$turmas_anos_iniciais)),
    sum(is.na(contexto_escola_2024$docentes_anos_iniciais))
  )
)

disponibilidade_variaveis <- contexto_escola_2024 |>
  summarise(
    across(
      everything(),
      ~ sum(!is.na(.x)),
      .names = "disponivel__{.col}"
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "variavel",
    values_to = "numero_escolas_com_dado"
  ) |>
  mutate(
    variavel = str_remove(variavel, "^disponivel__"),
    numero_escolas = nrow(contexto_escola_2024),
    cobertura_percentual = 100 * numero_escolas_com_dado / numero_escolas
  ) |>
  arrange(cobertura_percentual, variavel)

casos_revisao_contexto <- contexto_escola_2024 |>
  filter(
    is.na(ano) |
      is.na(matriculas_anos_iniciais) |
      is.na(turmas_anos_iniciais) |
      is.na(docentes_anos_iniciais) |
      tipo_situacao_funcionamento != "Em atividade"
  ) |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora,
    ano,
    rede,
    tipo_situacao_funcionamento,
    matriculas_anos_iniciais,
    turmas_anos_iniciais,
    docentes_anos_iniciais,
    any_of(c("tipo_vinculo_rede", "status_rede_2025"))
  )

# -------------------------------------------------------------------
# 13. Exportação
# -------------------------------------------------------------------

write_csv(
  contexto_bruto,
  here("dados_processados", "contexto_censo_escolar_2024_bruto.csv"),
  na = ""
)

write_csv(
  contexto_escola_2024,
  here("dados_processados", "dim_contexto_escola_2024.csv"),
  na = ""
)

write_csv(
  perfil_contextual_carteiras,
  here("resultados", "contexto", "perfil_contextual_carteiras_2024.csv"),
  na = ""
)

write_csv(
  validacao_contexto,
  here(
    "documentacao", "contexto_inep_2024",
    "13_validacao_extracao_contexto_2024.csv"
  ),
  na = ""
)

write_csv(
  disponibilidade_variaveis,
  here(
    "documentacao", "contexto_inep_2024",
    "14_disponibilidade_variaveis_contexto_2024.csv"
  ),
  na = ""
)

write_csv(
  casos_revisao_contexto,
  here(
    "documentacao", "contexto_inep_2024",
    "15_casos_revisao_contexto_2024.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 14. Resumo no console
# -------------------------------------------------------------------

cat("\nContexto estrutural pré-programa de 2024 concluído.\n")

cat("\nValidação da extração:\n")
print(validacao_contexto, n = Inf)

cat("\nResumo por carteira administrativa:\n")
print(
  perfil_contextual_carteiras |>
    select(
      assessora,
      numero_escolas,
      matriculas_anos_iniciais,
      mediana_matriculas_anos_iniciais,
      media_alunos_por_turma,
      pct_matriculas_integral,
      pct_matriculas_educacao_especial,
      indice_infraestrutura_medio
    ),
  n = Inf
)

cat("\nArquivos gerados:\n")
cat(
  "- dados_processados/contexto_censo_escolar_2024_bruto.csv\n",
  "- dados_processados/dim_contexto_escola_2024.csv\n",
  "- resultados/contexto/perfil_contextual_carteiras_2024.csv\n",
  "- documentacao/contexto_inep_2024/13_validacao_extracao_contexto_2024.csv\n",
  "- documentacao/contexto_inep_2024/14_disponibilidade_variaveis_contexto_2024.csv\n",
  "- documentacao/contexto_inep_2024/15_casos_revisao_contexto_2024.csv\n"
)
