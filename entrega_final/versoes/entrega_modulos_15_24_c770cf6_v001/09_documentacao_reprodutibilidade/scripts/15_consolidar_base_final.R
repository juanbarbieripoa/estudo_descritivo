# ===================================================================
# 15_consolidar_base_final.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# ===================================================================
#
# OBJETIVO
#
# Congelar uma base analítica final, estável e auditável, com uma linha
# por escola × ano escolar × componente curricular, reunindo:
#
#   1. identificação e vínculo administrativo da escola;
#   2. contexto estrutural pré-programa de 2024;
#   3. resultados observados em 2025 e 2026;
#   4. indicadores de participação e qualidade de composição;
#   5. estratos contextuais corrigidos no módulo 13A;
#   6. cadastro canônico escola × período do módulo 11C;
#   7. universos avaliativos e de carga explicitamente separados;
#   8. documentação, dicionário, hashes e diagnósticos de consistência.
#
# PRINCÍPIOS
#
# - O estudo é observacional e descritivo.
# - Resultados escolares não são atribuídos às assessoras.
# - O vínculo administrativo não comprova exposição ao assessoramento.
# - A primeira avaliação formativa de 2025 é linha de base pré-programa.
# - A exposição institucional é zero para todas as escolas em 2025.
# - A assessora gerencial de 2026 vem exclusivamente do cadastro 11C.
# - O grupo não exposto é referência descritiva, não grupo de controle.
# - Resultados escolares não podem ser atribuídos causalmente às assessoras.
# - A base corrigida do módulo 13A NÃO é usada como núcleo, pois foi
#   filtrada para a amostra principal com resultado balanceado.
# - O núcleo longitudinal completo é
#   variacao_escola_serie_com_qualidade.csv.
# - Nenhum arquivo canônico existente é substituído sem cópia histórica.
#
# PRODUTOS PRINCIPAIS
#
# dados_finais/base_analitica_final_escola_serie.csv
# dados_finais/base_analitica_final_escola_serie.rds
# dados_finais/dim_escola_final.csv
# dados_finais/dim_escola_final.rds
# documentacao/base_final/dicionario_base_analitica_final.csv
# documentacao/base_final/execucao_<data_hora>/...
# dados_finais/historico/execucao_<data_hora>/...
# ===================================================================

library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Identificação da execução e diretórios
# -------------------------------------------------------------------

instante_execucao <- Sys.time()

id_execucao <- format(
  instante_execucao,
  "%Y%m%d_%H%M%S"
)

caminho_script <- here(
  "R",
  "15_consolidar_base_final.R"
)

commit_base_integracao <- paste0(
  "4db7826e49c377541b492789a1a09d81",
  "e2beb373"
)

pasta_dados_finais <- here(
  "dados_finais"
)

pasta_historico <- here(
  "dados_finais",
  "historico",
  paste0("execucao_", id_execucao)
)

pasta_documentacao <- here(
  "documentacao",
  "base_final"
)

pasta_execucao <- here(
  "documentacao",
  "base_final",
  paste0("execucao_", id_execucao)
)

walk(
  c(
    pasta_dados_finais,
    pasta_historico,
    pasta_documentacao,
    pasta_execucao
  ),
  ~ dir.create(
    .x,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

# -------------------------------------------------------------------
# 2. Arquivos de entrada e de saída
# -------------------------------------------------------------------

arquivos_entrada <- c(
  cadastro_exposicao = here(
    "dados_intermediarios",
    "cadastro_exposicao_assessoramento.csv"
  ),
  dim_escola = here(
    "dados_intermediarios",
    "dim_escola.csv"
  ),
  contexto_2024 = here(
    "dados_processados",
    "dim_contexto_escola_2024_final.csv"
  ),
  base_integrada_anterior = here(
    "dados_processados",
    "base_analitica_escola_serie_contexto_2024.csv"
  ),
  estratos_corrigidos = here(
    "dados_processados",
    "base_contexto_desempenho_estratos_corrigidos.csv"
  ),
  variacao_qualidade = here(
    "dados_processados",
    "variacao_escola_serie_com_qualidade.csv"
  )
)

snapshot_homologado <- list(
  anos = c(2025L, 2026L),
  avaliacao_id = "1a_avaliacao_formativa",
  registros_cadastro = 112L,
  escolas_cadastradas = 56L,
  linhas_escola_serie = 280L,
  escolas_avaliadas_2025 = 54L,
  escolas_avaliadas_2026 = 56L,
  escolas_indice_carga_2026 = 53L,
  assessoras_indice_carga_2026 = 11L,
  codigos_inelegiveis_2026 = sort(
    c("43105416", "43105300", "43189768")
  ),
  versao_regra_2026 = "2026-07-24_v1"
)

if (!file.exists(caminho_script)) {
  stop(
    "O script deve ser executado pelo caminho canônico: ",
    caminho_script
  )
}

arquivos_saida <- c(
  base_final_csv = here(
    "dados_finais",
    "base_analitica_final_escola_serie.csv"
  ),
  base_final_rds = here(
    "dados_finais",
    "base_analitica_final_escola_serie.rds"
  ),
  dim_final_csv = here(
    "dados_finais",
    "dim_escola_final.csv"
  ),
  dim_final_rds = here(
    "dados_finais",
    "dim_escola_final.rds"
  ),
  dicionario_csv = here(
    "documentacao",
    "base_final",
    "dicionario_base_analitica_final.csv"
  )
)

arquivos_ausentes <- arquivos_entrada[
  !file.exists(arquivos_entrada)
]

if (length(arquivos_ausentes) > 0) {
  stop(
    "Arquivos necessários não encontrados:\n",
    paste(
      arquivos_ausentes,
      collapse = "\n"
    ),
    "\nExecute primeiro os módulos anteriores, inclusive 13A e 14A."
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

valores_distintos_texto <- function(x) {
  x <- as.character(x)
  x <- sort(
    unique(
      x[
        !is.na(x) &
          str_squish(x) != ""
      ]
    )
  )

  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(
    x,
    collapse = " | "
  )
}

converter_logico_seguro <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  x_limpo <- x |>
    as.character() |>
    str_squish() |>
    str_to_lower()

  case_when(
    is.na(x_limpo) |
      x_limpo == "" ~ NA,
    x_limpo %in% c(
      "true",
      "t",
      "1",
      "sim",
      "s"
    ) ~ TRUE,
    x_limpo %in% c(
      "false",
      "f",
      "0",
      "nao",
      "não",
      "n"
    ) ~ FALSE,
    TRUE ~ NA
  )
}

converter_numero_seguro <- function(x) {
  if (is.numeric(x)) {
    return(as.double(x))
  }

  x_limpo <- x |>
    as.character() |>
    str_squish() |>
    str_replace_all(",", ".") |>
    na_if("")

  suppressWarnings(
    as.numeric(x_limpo)
  )
}

converter_inteiro_seguro <- function(x) {
  converter_numero_seguro(x) |>
    as.integer()
}

limpar_texto <- function(x) {
  x |>
    as.character() |>
    str_squish() |>
    na_if("")
}

hash_md5 <- function(caminho) {
  if (
    length(caminho) != 1 ||
      is.na(caminho) ||
      !file.exists(caminho)
  ) {
    return(NA_character_)
  }

  unname(
    tools::md5sum(caminho)
  )
}

obter_commit_git <- function() {
  commit_sistema <- tryCatch(
    suppressWarnings(
      system2(
        "git",
        c("rev-parse", "HEAD"),
        stdout = TRUE,
        stderr = FALSE
      )
    ),
    error = function(e) character()
  )

  commit_sistema <- commit_sistema[
    str_detect(commit_sistema, "^[0-9a-fA-F]{40}$")
  ]

  if (length(commit_sistema) > 0) {
    return(
      str_to_lower(commit_sistema[[1]])
    )
  }

  pasta_git <- here(".git")
  arquivo_head <- file.path(pasta_git, "HEAD")

  if (!file.exists(arquivo_head)) {
    return(NA_character_)
  }

  head <- readLines(
    arquivo_head,
    warn = FALSE,
    n = 1
  )

  if (
    length(head) == 1 &&
      str_detect(head, "^[0-9a-fA-F]{40}$")
  ) {
    return(str_to_lower(head))
  }

  referencia <- str_remove(
    head,
    "^ref:\\s*"
  )
  arquivo_referencia <- file.path(
    pasta_git,
    referencia
  )

  if (file.exists(arquivo_referencia)) {
    commit <- readLines(
      arquivo_referencia,
      warn = FALSE,
      n = 1
    )

    if (
      length(commit) == 1 &&
        str_detect(commit, "^[0-9a-fA-F]{40}$")
    ) {
      return(str_to_lower(commit))
    }
  }

  arquivo_packed_refs <- file.path(
    pasta_git,
    "packed-refs"
  )

  if (file.exists(arquivo_packed_refs)) {
    linhas <- readLines(
      arquivo_packed_refs,
      warn = FALSE
    )
    padrao <- paste0(
      "^([0-9a-fA-F]{40})\\s+",
      referencia,
      "$"
    )
    correspondencias <- str_match(
      linhas,
      padrao
    )[, 2]
    correspondencias <- correspondencias[
      !is.na(correspondencias)
    ]

    if (length(correspondencias) > 0) {
      return(
        str_to_lower(correspondencias[[1]])
      )
    }
  }

  NA_character_
}

classificar_pipeline_existente <- function(caminho_csv) {
  if (!file.exists(caminho_csv)) {
    return("sem_produto_anterior")
  }

  cabecalho <- readLines(
    caminho_csv,
    warn = FALSE,
    n = 1
  )

  if (
    length(cabecalho) == 1 &&
      str_detect(
        cabecalho,
        "(^|,)incluir_indice_carga_2026(,|$)"
      )
  ) {
    "pipeline_pos_11C_anterior"
  } else {
    "pipeline_anterior_ao_11C"
  }
}

inventariar_estrutura <- function(dados, fonte) {
  map_dfr(
    names(dados),
    function(variavel) {
      x <- dados[[variavel]]

      tibble(
        fonte = fonte,
        ordem_coluna = match(
          variavel,
          names(dados)
        ),
        variavel = variavel,
        classe_r = paste(
          class(x),
          collapse = " | "
        ),
        tipo_r = typeof(x),
        numero_linhas = length(x),
        valores_ausentes = sum(
          is.na(x)
        ),
        valores_distintos = n_distinct(
          x,
          na.rm = TRUE
        )
      )
    }
  )
}

identificar_duplicidades <- function(
    dados,
    chaves,
    fonte
) {
  dados |>
    group_by(
      across(
        all_of(chaves)
      )
    ) |>
    summarise(
      numero_linhas = n(),
      .groups = "drop"
    ) |>
    filter(
      numero_linhas > 1
    ) |>
    mutate(
      fonte = fonte,
      .before = 1
    )
}

arquivar_se_existir <- function(
    arquivo_atual,
    rotulo,
    classificacao
) {
  if (!file.exists(arquivo_atual)) {
    return(NA_character_)
  }

  extensao <- tools::file_ext(
    arquivo_atual
  )

  destino <- file.path(
    pasta_historico,
    paste0(
      rotulo,
      "_",
      classificacao,
      "_antes_",
      id_execucao,
      ".",
      extensao
    )
  )

  sucesso <- file.copy(
    arquivo_atual,
    destino,
    overwrite = FALSE
  )

  if (!sucesso) {
    stop(
      "Não foi possível arquivar o arquivo existente: ",
      arquivo_atual
    )
  }

  normalizePath(
    destino,
    winslash = "/",
    mustWork = TRUE
  )
}

registrar_validacao <- function(
    teste,
    categoria,
    severidade,
    valor_observado,
    criterio,
    aprovado,
    detalhe
) {
  tibble(
    teste = teste,
    categoria = categoria,
    severidade = severidade,
    valor_observado = as.character(
      valor_observado
    ),
    criterio = criterio,
    status = if_else(
      aprovado,
      "aprovado",
      "revisar"
    ),
    detalhe = detalhe
  )
}

somar_fora_intervalo <- function(
    dados,
    colunas,
    minimo,
    maximo
) {
  colunas <- intersect(
    colunas,
    names(dados)
  )

  if (length(colunas) == 0) {
    return(0L)
  }

  dados |>
    select(
      all_of(colunas)
    ) |>
    pivot_longer(
      everything(),
      names_to = "variavel",
      values_to = "valor"
    ) |>
    summarise(
      total = sum(
        !is.na(valor) &
          (
            valor < minimo |
              valor > maximo
          )
      )
    ) |>
    pull(total)
}

# -------------------------------------------------------------------
# 4. Leitura das bases
# -------------------------------------------------------------------

cadastro_exposicao <- read_csv(
  arquivos_entrada[["cadastro_exposicao"]],
  show_col_types = FALSE,
  col_types = cols(
    id_escola_periodo = col_character(),
    id_escola = col_character(),
    codigo_inep = col_character(),
    nome_canonico = col_character(),
    ano = col_integer(),
    avaliacao_id = col_character(),
    avaliacao_rotulo = col_character(),
    chave_periodo = col_character(),
    periodo_programa = col_character(),
    pertence_universo_avaliativo = col_character(),
    elegivel_assessoramento = col_character(),
    recebe_assessoramento = col_character(),
    exposicao_programa_binaria = col_integer(),
    assessora_vinculo_administrativo = col_character(),
    assessora_gerencial = col_character(),
    status_carga_operacional = col_character(),
    grupo_exposicao = col_character(),
    incluir_diagnostico_avaliativo_ampliado = col_logical(),
    incluir_resultados_rede_assessorada = col_logical(),
    incluir_indice_carga = col_logical(),
    incluir_nao_exposto_descritivo = col_logical(),
    fonte_validacao = col_character(),
    data_validacao = col_date(format = "%Y-%m-%d"),
    responsavel_validacao = col_character(),
    observacao_validacao = col_character(),
    status_homologacao = col_character(),
    versao_regra = col_character()
  ),
  na = c(
    "",
    "NA",
    "N/A"
  )
)

# A dimensão é lida integralmente como texto para preservar códigos,
# inclusive códigos INEP, antes da conversão controlada dos tipos.
dim_escola <- read_csv(
  arquivos_entrada[["dim_escola"]],
  show_col_types = FALSE,
  col_types = cols(
    .default = col_character()
  ),
  na = c(
    "",
    "NA",
    "N/A"
  )
)

contexto_2024 <- read_csv(
  arquivos_entrada[["contexto_2024"]],
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
  ),
  na = c(
    "",
    "NA",
    "N/A"
  )
)

variacao_qualidade <- read_csv(
  arquivos_entrada[["variacao_qualidade"]],
  show_col_types = FALSE,
  col_types = cols(
    id_escola = col_character(),
    codigo_inep = col_character(),
    nome_canonico = col_character(),
    assessora = col_character(),
    ano_escolar = col_integer(),
    componente = col_character(),
    presente_2025 = col_logical(),
    presente_2026 = col_logical(),
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
    delta_numero_turmas = col_double(),
    delta_previstos = col_double(),
    delta_avaliados = col_double(),
    delta_participacao = col_double(),
    delta_proficiencia = col_double(),
    delta_pct_defasagem = col_double(),
    delta_pct_intermediario = col_double(),
    delta_pct_adequado = col_double(),
    variacao_relativa_previstos = col_double(),
    variacao_relativa_avaliados = col_double(),
    observacao_composicao = col_character(),
    .default = col_guess()
  ),
  na = c(
    "",
    "NA",
    "N/A"
  )
)

base_integrada_anterior <- read_csv(
  arquivos_entrada[["base_integrada_anterior"]],
  show_col_types = FALSE,
  col_types = cols(
    id_escola = col_character(),
    codigo_inep = col_character(),
    nome_canonico = col_character(),
    assessora = col_character(),
    ano_escolar = col_integer(),
    componente = col_character(),
    .default = col_guess()
  ),
  na = c(
    "",
    "NA",
    "N/A"
  )
)

estratos_corrigidos <- read_csv(
  arquivos_entrada[["estratos_corrigidos"]],
  show_col_types = FALSE,
  col_types = cols(
    id_escola = col_character(),
    codigo_inep = col_character(),
    nome_canonico = col_character(),
    assessora = col_character(),
    ano_escolar = col_integer(),
    componente = col_character(),
    quartil_infraestrutura_escola = col_character(),
    faixa_porte_contextual_escola = col_character(),
    .default = col_guess()
  ),
  na = c(
    "",
    "NA",
    "N/A"
  )
)

# Limpeza controlada das chaves.
cadastro_exposicao <- cadastro_exposicao |>
  mutate(
    across(
      any_of(
        c(
          "id_escola_periodo",
          "id_escola",
          "codigo_inep",
          "nome_canonico",
          "avaliacao_id",
          "avaliacao_rotulo",
          "chave_periodo",
          "periodo_programa",
          "pertence_universo_avaliativo",
          "elegivel_assessoramento",
          "recebe_assessoramento",
          "assessora_vinculo_administrativo",
          "assessora_gerencial",
          "status_carga_operacional",
          "grupo_exposicao",
          "fonte_validacao",
          "responsavel_validacao",
          "observacao_validacao",
          "status_homologacao",
          "versao_regra"
        )
      ),
      limpar_texto
    )
  )

dim_escola <- dim_escola |>
  mutate(
    id_escola = str_squish(
      id_escola
    ),
    codigo_inep = na_if(
      str_squish(codigo_inep),
      ""
    ),
    nome_canonico = na_if(
      str_squish(nome_canonico),
      ""
    ),
    assessora = na_if(
      str_squish(assessora),
      ""
    )
  )

contexto_2024 <- contexto_2024 |>
  mutate(
    id_escola = str_squish(
      id_escola
    ),
    codigo_inep = na_if(
      str_squish(codigo_inep),
      ""
    ),
    nome_canonico = na_if(
      str_squish(nome_canonico),
      ""
    ),
    assessora = na_if(
      str_squish(assessora),
      ""
    )
  )

variacao_qualidade <- variacao_qualidade |>
  mutate(
    id_escola = str_squish(
      id_escola
    ),
    codigo_inep = na_if(
      str_squish(codigo_inep),
      ""
    ),
    nome_canonico = na_if(
      str_squish(nome_canonico),
      ""
    ),
    assessora = na_if(
      str_squish(assessora),
      ""
    ),
    componente = na_if(
      str_squish(componente),
      ""
    )
  )

base_integrada_anterior <- base_integrada_anterior |>
  mutate(
    id_escola = str_squish(
      id_escola
    ),
    codigo_inep = na_if(
      str_squish(codigo_inep),
      ""
    ),
    nome_canonico = na_if(
      str_squish(nome_canonico),
      ""
    ),
    assessora = na_if(
      str_squish(assessora),
      ""
    ),
    componente = na_if(
      str_squish(componente),
      ""
    )
  )

estratos_corrigidos <- estratos_corrigidos |>
  mutate(
    id_escola = str_squish(
      id_escola
    ),
    componente = na_if(
      str_squish(componente),
      ""
    )
  )

# -------------------------------------------------------------------
# 5. Inventário da estrutura efetivamente observada
# -------------------------------------------------------------------

commit_git_execucao <- obter_commit_git()

manifesto_execucao <- tibble(
  id_execucao = id_execucao,
  executado_em = format(
    instante_execucao,
    "%Y-%m-%d %H:%M:%S %Z"
  ),
  script_caminho = normalizePath(
    caminho_script,
    winslash = "/",
    mustWork = TRUE
  ),
  script_nome = basename(caminho_script),
  script_md5 = hash_md5(caminho_script),
  commit_git_execucao = commit_git_execucao,
  commit_base_integracao = commit_base_integracao
)

write_csv(
  manifesto_execucao,
  file.path(
    pasta_execucao,
    "00_manifesto_execucao.csv"
  ),
  na = ""
)

bases_lidas <- list(
  cadastro_exposicao = cadastro_exposicao,
  dim_escola = dim_escola,
  contexto_2024 = contexto_2024,
  variacao_qualidade = variacao_qualidade,
  base_integrada_anterior = base_integrada_anterior,
  estratos_corrigidos = estratos_corrigidos
)

estrutura_entrada <- imap_dfr(
  bases_lidas,
  inventariar_estrutura
)

write_csv(
  estrutura_entrada,
  file.path(
    pasta_execucao,
    "01_estrutura_bases_entrada.csv"
  ),
  na = ""
)

manifesto_entrada <- imap_dfr(
  arquivos_entrada,
  function(caminho, fonte) {
    info <- file.info(
      caminho
    )

    dados <- bases_lidas[[fonte]]

    tibble(
      fonte = fonte,
      caminho = normalizePath(
        caminho,
        winslash = "/",
        mustWork = TRUE
      ),
      tamanho_bytes = info$size,
      data_modificacao = format(
        info$mtime,
        "%Y-%m-%d %H:%M:%S"
      ),
      md5 = unname(
        tools::md5sum(caminho)
      ),
      numero_linhas = nrow(dados),
      numero_colunas = ncol(dados)
    )
  }
)

write_csv(
  manifesto_entrada,
  file.path(
    pasta_execucao,
    "02_manifesto_arquivos_entrada.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 6. Validação das colunas obrigatórias
# -------------------------------------------------------------------

colunas_obrigatorias <- list(
  cadastro_exposicao = c(
    "id_escola_periodo",
    "id_escola",
    "codigo_inep",
    "nome_canonico",
    "ano",
    "avaliacao_id",
    "periodo_programa",
    "pertence_universo_avaliativo",
    "elegivel_assessoramento",
    "recebe_assessoramento",
    "exposicao_programa_binaria",
    "assessora_vinculo_administrativo",
    "assessora_gerencial",
    "status_carga_operacional",
    "grupo_exposicao",
    "incluir_diagnostico_avaliativo_ampliado",
    "incluir_resultados_rede_assessorada",
    "incluir_indice_carga",
    "incluir_nao_exposto_descritivo",
    "fonte_validacao",
    "data_validacao",
    "responsavel_validacao",
    "status_homologacao",
    "versao_regra"
  ),
  dim_escola = c(
    "id_escola",
    "codigo_inep",
    "nome_canonico",
    "assessora"
  ),
  contexto_2024 = c(
    "id_escola",
    "codigo_inep",
    "nome_canonico",
    "assessora",
    "dependencia_administrativa_2024",
    "situacao_funcionamento_2024",
    "incluir_universo_municipal_direto_2024",
    "incluir_universo_municipal_ampliado_2024",
    "incluir_rede_municipal_operacional_2025",
    "matriculas_anos_iniciais",
    "indice_infraestrutura_basica"
  ),
  variacao_qualidade = c(
    "id_escola",
    "ano_escolar",
    "componente",
    "painel_balanceado",
    "painel_resultado_balanceado",
    "previstos_2025",
    "previstos_2026",
    "avaliados_2025",
    "avaliados_2026",
    "taxa_participacao_2025",
    "taxa_participacao_2026",
    "proficiencia_media_2025",
    "proficiencia_media_2026",
    "pct_defasagem_2025",
    "pct_defasagem_2026",
    "pct_intermediario_2025",
    "pct_intermediario_2026",
    "pct_adequado_2025",
    "pct_adequado_2026",
    "delta_participacao",
    "delta_proficiencia",
    "observacao_composicao"
  ),
  base_integrada_anterior = c(
    "id_escola",
    "codigo_inep",
    "nome_canonico",
    "assessora",
    "ano_escolar",
    "componente"
  ),
  estratos_corrigidos = c(
    "id_escola",
    "ano_escolar",
    "componente",
    "quartil_infraestrutura_escola",
    "faixa_porte_contextual_escola"
  )
)

# Confere se todas as fontes previstas possuem uma base lida correspondente.
fontes_sem_base <- setdiff(
  names(colunas_obrigatorias),
  names(bases_lidas)
)

if (length(fontes_sem_base) > 0) {
  stop(
    "As seguintes fontes definidas em `colunas_obrigatorias` não existem em `bases_lidas`: ",
    paste(fontes_sem_base, collapse = ", ")
  )
}

# Iteração posicional explícita. Esta forma evita que algumas versões do
# purrr tratem o segundo argumento de imap_dfr() como índice recursivo em [[.
diagnostico_colunas <- map_dfr(
  seq_along(colunas_obrigatorias),
  function(i) {
    fonte <- names(colunas_obrigatorias)[[i]]
    colunas <- colunas_obrigatorias[[i]]
    dados_fonte <- bases_lidas[[match(fonte, names(bases_lidas))]]

    tibble(
      fonte = fonte,
      coluna_obrigatoria = colunas,
      presente = colunas %in% names(dados_fonte)
    )
  }
)

write_csv(
  diagnostico_colunas,
  file.path(
    pasta_execucao,
    "03_validacao_colunas_obrigatorias.csv"
  ),
  na = ""
)

colunas_ausentes <- diagnostico_colunas |>
  filter(
    !presente
  )

if (nrow(colunas_ausentes) > 0) {
  stop(
    "Há colunas obrigatórias ausentes. Consulte:\n",
    file.path(
      pasta_execucao,
      "03_validacao_colunas_obrigatorias.csv"
    )
  )
}

# -------------------------------------------------------------------
# 7. Validação das chaves
# -------------------------------------------------------------------

chave_escola <- "id_escola"
chave_escola_serie <- c(
  "id_escola",
  "ano_escolar",
  "componente"
)

duplicidades_chaves <- bind_rows(
  identificar_duplicidades(
    cadastro_exposicao,
    "id_escola_periodo",
    "cadastro_exposicao_id_periodo"
  ),
  identificar_duplicidades(
    cadastro_exposicao,
    c(
      "id_escola",
      "ano",
      "avaliacao_id"
    ),
    "cadastro_exposicao_escola_ano_avaliacao"
  ),
  identificar_duplicidades(
    dim_escola,
    chave_escola,
    "dim_escola"
  ),
  identificar_duplicidades(
    contexto_2024,
    chave_escola,
    "contexto_2024"
  ),
  identificar_duplicidades(
    variacao_qualidade,
    chave_escola_serie,
    "variacao_qualidade"
  ),
  identificar_duplicidades(
    base_integrada_anterior,
    chave_escola_serie,
    "base_integrada_anterior"
  ),
  identificar_duplicidades(
    estratos_corrigidos,
    chave_escola_serie,
    "estratos_corrigidos"
  )
)

write_csv(
  duplicidades_chaves,
  file.path(
    pasta_execucao,
    "04_duplicidades_chaves_entrada.csv"
  ),
  na = ""
)

if (nrow(duplicidades_chaves) > 0) {
  stop(
    "Há duplicidades nas chaves das bases de entrada. Consulte:\n",
    file.path(
      pasta_execucao,
      "04_duplicidades_chaves_entrada.csv"
    )
  )
}

# -------------------------------------------------------------------
# 7A. Contrato bloqueante do cadastro canônico do módulo 11C
# -------------------------------------------------------------------

cadastro_2025 <- cadastro_exposicao |>
  filter(ano == 2025L)

cadastro_2026 <- cadastro_exposicao |>
  filter(ano == 2026L)

ids_cadastro_sem_dim <- cadastro_exposicao |>
  distinct(id_escola) |>
  anti_join(
    dim_escola |>
      distinct(id_escola),
    by = "id_escola"
  ) |>
  mutate(tipo_divergencia = "cadastro_sem_dimensao")

ids_dim_sem_cadastro <- dim_escola |>
  distinct(id_escola) |>
  anti_join(
    cadastro_exposicao |>
      distinct(id_escola),
    by = "id_escola"
  ) |>
  mutate(tipo_divergencia = "dimensao_sem_cadastro")

ids_cadastro_dim_divergentes <- bind_rows(
  ids_cadastro_sem_dim,
  ids_dim_sem_cadastro
) |>
  select(
    tipo_divergencia,
    id_escola
  )

write_csv(
  ids_cadastro_dim_divergentes,
  file.path(
    pasta_execucao,
    "06A_ids_cadastro_dim_divergentes.csv"
  ),
  na = ""
)

colunas_sinalizadoras_cadastro <- c(
  "incluir_diagnostico_avaliativo_ampliado",
  "incluir_resultados_rede_assessorada",
  "incluir_indice_carga",
  "incluir_nao_exposto_descritivo"
)

dominios_cadastro <- list(
  pertence_universo_avaliativo = c("SIM", "NAO"),
  elegivel_assessoramento = c(
    "SIM",
    "NAO",
    "NA_APLICAVEL"
  ),
  recebe_assessoramento = c("SIM", "NAO"),
  exposicao_programa_binaria = c("0", "1"),
  status_carga_operacional = c(
    "ATIVA",
    "SEM_CARGA_DO_PROGRAMA",
    "ZERO_OU_TENDENTE_A_ZERO"
  ),
  grupo_exposicao = c(
    "ASSESSORADA",
    "LINHA_BASE_PRE_PROGRAMA",
    "NAO_EXPOSTA_INELEGIVEL"
  ),
  status_homologacao = c(
    "HOMOLOGADO",
    "REGRA_TEMPORAL_CONFIRMADA"
  )
)

violacoes_dominios_cadastro <- imap_dfr(
  dominios_cadastro,
  function(valores_permitidos, variavel) {
    valor <- cadastro_exposicao[[variavel]]
    invalido <- is.na(valor) |
      !as.character(valor) %in% valores_permitidos

    cadastro_exposicao |>
      filter(invalido) |>
      transmute(
        id_escola_periodo,
        id_escola,
        ano,
        variavel = variavel,
        valor_observado = as.character(
          .data[[variavel]]
        ),
        valores_permitidos = paste(
          valores_permitidos,
          collapse = " | "
        )
      )
  }
)

metadados_2026_incompletos <- cadastro_2026 |>
  filter(
    is.na(fonte_validacao) |
      is.na(data_validacao) |
      is.na(responsavel_validacao) |
      is.na(status_homologacao) |
      is.na(versao_regra) |
      (
        incluir_indice_carga %in% TRUE &
          is.na(assessora_gerencial)
      )
  ) |>
  select(
    id_escola_periodo,
    id_escola,
    codigo_inep,
    nome_canonico,
    fonte_validacao,
    data_validacao,
    responsavel_validacao,
    assessora_gerencial,
    status_homologacao,
    versao_regra
  )

write_csv(
  violacoes_dominios_cadastro,
  file.path(
    pasta_execucao,
    "06D1_violacoes_dominios_cadastro.csv"
  ),
  na = ""
)

write_csv(
  metadados_2026_incompletos,
  file.path(
    pasta_execucao,
    "06D2_metadados_2026_incompletos.csv"
  ),
  na = ""
)

numero_sinalizadores_ausentes <- cadastro_exposicao |>
  select(
    all_of(colunas_sinalizadoras_cadastro)
  ) |>
  map_int(
    ~ sum(is.na(.x))
  ) |>
  sum()

tipos_cadastro_validos <-
  is.character(cadastro_exposicao$id_escola_periodo) &&
  is.character(cadastro_exposicao$id_escola) &&
  is.character(cadastro_exposicao$codigo_inep) &&
  is.integer(cadastro_exposicao$ano) &&
  is.integer(
    cadastro_exposicao$exposicao_programa_binaria
  ) &&
  inherits(cadastro_exposicao$data_validacao, "Date") &&
  all(
    map_lgl(
      cadastro_exposicao[
        colunas_sinalizadoras_cadastro
      ],
      is.logical
    )
  )

coerencia_exposicao <- cadastro_exposicao |>
  summarise(
    divergencias = sum(
      exposicao_programa_binaria !=
        if_else(
          recebe_assessoramento == "SIM",
          1L,
          0L
        ),
      na.rm = TRUE
    ) +
      sum(
        is.na(exposicao_programa_binaria) |
          is.na(recebe_assessoramento)
      )
  ) |>
  pull(divergencias)

coerencia_universo_avaliativo <- cadastro_exposicao |>
  summarise(
    divergencias = sum(
      incluir_diagnostico_avaliativo_ampliado !=
        (pertence_universo_avaliativo == "SIM"),
      na.rm = TRUE
    ) +
      sum(
        is.na(
          incluir_diagnostico_avaliativo_ampliado
        ) |
          is.na(pertence_universo_avaliativo)
      )
  ) |>
  pull(divergencias)

coerencia_indice_carga <- cadastro_exposicao |>
  mutate(
    inclusao_esperada =
      elegivel_assessoramento == "SIM" &
      recebe_assessoramento == "SIM" &
      status_carga_operacional == "ATIVA" &
      status_homologacao == "HOMOLOGADO"
  ) |>
  summarise(
    divergencias = sum(
      incluir_indice_carga != inclusao_esperada,
      na.rm = TRUE
    ) +
      sum(
        is.na(incluir_indice_carga) |
          is.na(inclusao_esperada)
      )
  ) |>
  pull(divergencias)

codigos_fora_carga_2026 <- cadastro_2026 |>
  filter(
    !coalesce(
      incluir_indice_carga,
      FALSE
    )
  ) |>
  pull(codigo_inep) |>
  sort()

escolas_fora_carga_2026 <- cadastro_2026 |>
  filter(
    !coalesce(
      incluir_indice_carga,
      FALSE
    )
  ) |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    pertence_universo_avaliativo,
    elegivel_assessoramento,
    recebe_assessoramento,
    status_carga_operacional,
    grupo_exposicao,
    incluir_indice_carga,
    status_homologacao,
    versao_regra
  ) |>
  arrange(codigo_inep)

write_csv(
  escolas_fora_carga_2026,
  file.path(
    pasta_execucao,
    "06B_escolas_fora_carga_2026.csv"
  ),
  na = ""
)

resumo_universos_canonicos <- cadastro_exposicao |>
  group_by(
    ano,
    avaliacao_id,
    periodo_programa
  ) |>
  summarise(
    escolas_cadastradas = n_distinct(id_escola),
    escolas_universo_avaliativo = n_distinct(
      id_escola[
        incluir_diagnostico_avaliativo_ampliado %in% TRUE
      ]
    ),
    escolas_expostas = n_distinct(
      id_escola[
        exposicao_programa_binaria == 1L
      ]
    ),
    escolas_indice_carga = n_distinct(
      id_escola[
        incluir_indice_carga %in% TRUE
      ]
    ),
    assessoras_indice_carga = n_distinct(
      assessora_gerencial[
        incluir_indice_carga %in% TRUE
      ],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(ano)

write_csv(
  resumo_universos_canonicos,
  file.path(
    pasta_execucao,
    "06C_resumo_universos_canonicos.csv"
  ),
  na = ""
)

validacao_cadastro_canonico <- bind_rows(
  registrar_validacao(
    "Tipos do cadastro são válidos",
    "cadastro canônico",
    "erro",
    tipos_cadastro_validos,
    "identificadores textuais, ano/exposição inteiros, flags lógicas e data Date",
    tipos_cadastro_validos,
    "A leitura usa tipos controlados para impedir coerções silenciosas."
  ),
  registrar_validacao(
    "Domínios do cadastro são válidos",
    "cadastro canônico",
    "erro",
    nrow(violacoes_dominios_cadastro),
    "zero valores ausentes ou fora dos domínios",
    nrow(violacoes_dominios_cadastro) == 0,
    "Os domínios são parte do contrato formal do módulo 11C."
  ),
  registrar_validacao(
    "Metadados de homologação de 2026 completos",
    "governança",
    "erro",
    nrow(metadados_2026_incompletos),
    "zero registros incompletos",
    nrow(metadados_2026_incompletos) == 0,
    "Fonte, data, responsável, regra e assessora da carga devem estar registrados."
  ),
  registrar_validacao(
    "Cadastro possui 112 registros",
    "cadastro canônico",
    "erro",
    nrow(cadastro_exposicao),
    "exatamente 112 registros escola × período",
    nrow(cadastro_exposicao) ==
      snapshot_homologado$registros_cadastro,
    "O cadastro deve conter 56 escolas em cada um dos dois períodos."
  ),
  registrar_validacao(
    "Cadastro possui 56 escolas",
    "cadastro canônico",
    "erro",
    n_distinct(cadastro_exposicao$id_escola),
    "exatamente 56 escolas",
    n_distinct(cadastro_exposicao$id_escola) ==
      snapshot_homologado$escolas_cadastradas,
    "O cadastro deve cobrir integralmente a dimensão escolar."
  ),
  registrar_validacao(
    "Cadastro possui os dois períodos oficiais",
    "cadastro canônico",
    "erro",
    paste(
      sort(unique(cadastro_exposicao$ano)),
      collapse = " | "
    ),
    "2025 | 2026",
    setequal(
      unique(cadastro_exposicao$ano),
      snapshot_homologado$anos
    ),
    "Novas ondas exigem homologação institucional antes da integração."
  ),
  registrar_validacao(
    "Avaliação canônica esperada",
    "cadastro canônico",
    "erro",
    paste(
      sort(unique(cadastro_exposicao$avaliacao_id)),
      collapse = " | "
    ),
    snapshot_homologado$avaliacao_id,
    identical(
      sort(unique(cadastro_exposicao$avaliacao_id)),
      snapshot_homologado$avaliacao_id
    ),
    "Esta versão integra somente a primeira avaliação formativa."
  ),
  registrar_validacao(
    "Cadastro e dimensão cobrem os mesmos IDs",
    "integração",
    "erro",
    nrow(ids_cadastro_dim_divergentes),
    "zero IDs divergentes",
    nrow(ids_cadastro_dim_divergentes) == 0,
    "A integração por id_escola não pode perder nem acrescentar escolas."
  ),
  registrar_validacao(
    "Sinalizadores canônicos preenchidos",
    "cadastro canônico",
    "erro",
    numero_sinalizadores_ausentes,
    "zero valores ausentes",
    numero_sinalizadores_ausentes == 0,
    "Os quatro sinalizadores de inclusão não podem conter NA."
  ),
  registrar_validacao(
    "Exposição binária coerente",
    "cadastro canônico",
    "erro",
    coerencia_exposicao,
    "zero divergências",
    coerencia_exposicao == 0,
    "Exposição binária vale 1 somente quando recebe_assessoramento = SIM."
  ),
  registrar_validacao(
    "Universo avaliativo coerente",
    "cadastro canônico",
    "erro",
    coerencia_universo_avaliativo,
    "zero divergências",
    coerencia_universo_avaliativo == 0,
    "Presença avaliativa não implica assessoramento."
  ),
  registrar_validacao(
    "Inclusão no índice de carga coerente",
    "cadastro canônico",
    "erro",
    coerencia_indice_carga,
    "zero divergências",
    coerencia_indice_carga == 0,
    paste(
      "A carga exige elegibilidade, exposição, carga ativa",
      "e homologação."
    )
  ),
  registrar_validacao(
    "Universo avaliativo de 2025",
    "universos oficiais",
    "erro",
    sum(
      cadastro_2025[[
        "incluir_diagnostico_avaliativo_ampliado"
      ]],
      na.rm = TRUE
    ),
    "54 escolas",
    sum(
      cadastro_2025[[
        "incluir_diagnostico_avaliativo_ampliado"
      ]],
      na.rm = TRUE
    ) == snapshot_homologado$escolas_avaliadas_2025,
    "A primeira avaliação formativa de 2025 é linha de base."
  ),
  registrar_validacao(
    "Universo avaliativo de 2026",
    "universos oficiais",
    "erro",
    sum(
      cadastro_2026[[
        "incluir_diagnostico_avaliativo_ampliado"
      ]],
      na.rm = TRUE
    ),
    "56 escolas",
    sum(
      cadastro_2026[[
        "incluir_diagnostico_avaliativo_ampliado"
      ]],
      na.rm = TRUE
    ) == snapshot_homologado$escolas_avaliadas_2026,
    "Todas as 56 escolas pertencem ao universo avaliativo de 2026."
  ),
  registrar_validacao(
    "Exposição de 2025 igual a zero",
    "regra temporal",
    "erro",
    sum(
      cadastro_2025$exposicao_programa_binaria,
      na.rm = TRUE
    ),
    "zero exposições positivas",
    sum(
      cadastro_2025$exposicao_programa_binaria,
      na.rm = TRUE
    ) == 0,
    "É proibida a reclassificação retrospectiva da linha de base."
  ),
  registrar_validacao(
    "Sem assessora gerencial em 2025",
    "regra temporal",
    "erro",
    sum(!is.na(cadastro_2025$assessora_gerencial)),
    "zero vínculos gerenciais de exposição em 2025",
    sum(!is.na(cadastro_2025$assessora_gerencial)) == 0,
    "O vínculo administrativo genérico não pode inferir exposição."
  ),
  registrar_validacao(
    "Índice de carga zerado em 2025",
    "regra temporal",
    "erro",
    sum(
      cadastro_2025$incluir_indice_carga,
      na.rm = TRUE
    ),
    "zero escolas",
    sum(
      cadastro_2025$incluir_indice_carga,
      na.rm = TRUE
    ) == 0,
    "O índice de carga não se aplica à linha de base pré-programa."
  ),
  registrar_validacao(
    "Universo de carga de 2026",
    "universos oficiais",
    "erro",
    sum(
      cadastro_2026$incluir_indice_carga,
      na.rm = TRUE
    ),
    "53 escolas",
    sum(
      cadastro_2026$incluir_indice_carga,
      na.rm = TRUE
    ) == snapshot_homologado$escolas_indice_carga_2026,
    "Somente escolas elegíveis, assessoradas e homologadas entram na carga."
  ),
  registrar_validacao(
    "Assessoras no universo de carga de 2026",
    "universos oficiais",
    "erro",
    n_distinct(
      cadastro_2026$assessora_gerencial[
        cadastro_2026$incluir_indice_carga %in% TRUE
      ],
      na.rm = TRUE
    ),
    "11 assessoras",
    n_distinct(
      cadastro_2026$assessora_gerencial[
        cadastro_2026$incluir_indice_carga %in% TRUE
      ],
      na.rm = TRUE
    ) == snapshot_homologado$assessoras_indice_carga_2026,
    "Categorias residuais não integram o universo oficial de carga."
  ),
  registrar_validacao(
    "Três conveniadas fora da carga",
    "universos oficiais",
    "erro",
    paste(
      codigos_fora_carga_2026,
      collapse = " | "
    ),
    paste(
      snapshot_homologado$codigos_inelegiveis_2026,
      collapse = " | "
    ),
    identical(
      codigos_fora_carga_2026,
      snapshot_homologado$codigos_inelegiveis_2026
    ),
    paste(
      "Pequena Casa da Criança, Madre Raffo e Aldeia Lumiar",
      "permanecem no perfil geral, mas fora da carga."
    )
  ),
  registrar_validacao(
    "Versão da regra de 2026",
    "governança",
    "erro",
    paste(
      sort(unique(cadastro_2026$versao_regra)),
      collapse = " | "
    ),
    snapshot_homologado$versao_regra_2026,
    identical(
      sort(unique(cadastro_2026$versao_regra)),
      snapshot_homologado$versao_regra_2026
    ),
    "A integração usa a homologação institucional 2026-07-24_v1."
  )
)

write_csv(
  validacao_cadastro_canonico,
  file.path(
    pasta_execucao,
    "06D_validacao_cadastro_canonico.csv"
  ),
  na = ""
)

erros_cadastro <- validacao_cadastro_canonico |>
  filter(
    severidade == "erro",
    status == "revisar"
  )

if (nrow(erros_cadastro) > 0) {
  stop(
    "O cadastro canônico diverge das invariantes homologadas. ",
    "Nenhum produto final foi sobrescrito. Consulte:\n",
    file.path(
      pasta_execucao,
      "06D_validacao_cadastro_canonico.csv"
    )
  )
}

# -------------------------------------------------------------------
# 8. Conflitos de identificação entre as fontes
# -------------------------------------------------------------------

atributos_fontes <- bind_rows(
  cadastro_exposicao |>
    filter(ano == 2026L) |>
    transmute(
      fonte = "cadastro_exposicao_2026",
      id_escola,
      codigo_inep,
      nome_canonico,
      assessora = assessora_vinculo_administrativo
    ),
  dim_escola |>
    transmute(
      fonte = "dim_escola",
      id_escola,
      codigo_inep,
      nome_canonico,
      assessora
    ),
  contexto_2024 |>
    transmute(
      fonte = "contexto_2024",
      id_escola,
      codigo_inep,
      nome_canonico,
      assessora
    ),
  variacao_qualidade |>
    distinct(
      id_escola,
      codigo_inep,
      nome_canonico,
      assessora
    ) |>
    mutate(
      fonte = "variacao_qualidade",
      .before = 1
    ),
  base_integrada_anterior |>
    distinct(
      id_escola,
      codigo_inep,
      nome_canonico,
      assessora
    ) |>
    mutate(
      fonte = "base_integrada_anterior",
      .before = 1
    )
)

conflitos_atributos <- atributos_fontes |>
  group_by(
    id_escola
  ) |>
  summarise(
    numero_codigos_inep = n_distinct(
      codigo_inep[
        !is.na(codigo_inep) &
          codigo_inep != ""
      ]
    ),
    codigos_inep = valores_distintos_texto(
      codigo_inep
    ),
    numero_nomes = n_distinct(
      nome_canonico[
        !is.na(nome_canonico) &
          nome_canonico != ""
      ]
    ),
    nomes = valores_distintos_texto(
      nome_canonico
    ),
    numero_assessoras = n_distinct(
      assessora[
        !is.na(assessora) &
          assessora != ""
      ]
    ),
    assessoras = valores_distintos_texto(
      assessora
    ),
    fontes = valores_distintos_texto(
      fonte
    ),
    .groups = "drop"
  ) |>
  filter(
    numero_codigos_inep > 1 |
      numero_nomes > 1 |
      numero_assessoras > 1
  )

write_csv(
  conflitos_atributos,
  file.path(
    pasta_execucao,
    "05_conflitos_atributos_escola.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 9. Estratos corrigidos no nível da escola
# -------------------------------------------------------------------

conflitos_estratos <- estratos_corrigidos |>
  group_by(
    id_escola
  ) |>
  summarise(
    numero_quartis_infraestrutura = n_distinct(
      quartil_infraestrutura_escola,
      na.rm = TRUE
    ),
    numero_faixas_porte = n_distinct(
      faixa_porte_contextual_escola,
      na.rm = TRUE
    ),
    quartis = valores_distintos_texto(
      quartil_infraestrutura_escola
    ),
    faixas_porte = valores_distintos_texto(
      faixa_porte_contextual_escola
    ),
    .groups = "drop"
  ) |>
  filter(
    numero_quartis_infraestrutura > 1 |
      numero_faixas_porte > 1
  )

write_csv(
  conflitos_estratos,
  file.path(
    pasta_execucao,
    "06_conflitos_estratos_corrigidos.csv"
  ),
  na = ""
)

if (nrow(conflitos_estratos) > 0) {
  stop(
    "Os estratos corrigidos não são constantes dentro da escola. ",
    "Consulte 06_conflitos_estratos_corrigidos.csv."
  )
}

estratos_escola <- estratos_corrigidos |>
  group_by(
    id_escola
  ) |>
  summarise(
    quartil_infraestrutura_escola =
      primeiro_nao_vazio(
        quartil_infraestrutura_escola
      ),
    faixa_porte_contextual_escola =
      primeiro_nao_vazio(
        faixa_porte_contextual_escola
      ),
    .groups = "drop"
  )

# O cadastro possui duas linhas por escola. Ele deve ser convertido para
# uma dimensão de 56 linhas antes de qualquer join com o núcleo de 280
# linhas; a união direta duplicaria silenciosamente a base para 560 linhas.
variaveis_temporais_cadastro <- c(
  "avaliacao_id",
  "periodo_programa",
  "pertence_universo_avaliativo",
  "elegivel_assessoramento",
  "recebe_assessoramento",
  "exposicao_programa_binaria",
  "assessora_gerencial",
  "status_carga_operacional",
  "grupo_exposicao",
  "incluir_diagnostico_avaliativo_ampliado",
  "incluir_resultados_rede_assessorada",
  "incluir_indice_carga",
  "incluir_nao_exposto_descritivo",
  "fonte_validacao",
  "data_validacao",
  "responsavel_validacao",
  "status_homologacao",
  "versao_regra"
)

cadastro_temporal <- cadastro_exposicao |>
  select(
    id_escola,
    ano,
    all_of(variaveis_temporais_cadastro)
  ) |>
  pivot_wider(
    id_cols = id_escola,
    names_from = ano,
    values_from = all_of(
      variaveis_temporais_cadastro
    ),
    names_glue = "{.value}_{ano}"
  ) |>
  arrange(id_escola)

estrutura_cadastro_temporal <- inventariar_estrutura(
  cadastro_temporal,
  "cadastro_temporal_56_escolas"
)

write_csv(
  estrutura_cadastro_temporal,
  file.path(
    pasta_execucao,
    "06E_estrutura_cadastro_temporal.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 10. Dimensão final de escolas
# -------------------------------------------------------------------

# A dimensão cadastral é a fonte principal para código, nome e assessora.
# O contexto final acrescenta apenas variáveis ainda ausentes na dimensão.
colunas_contexto_novas <- setdiff(
  names(contexto_2024),
  names(dim_escola)
)

colunas_contexto_novas <- setdiff(
  colunas_contexto_novas,
  "id_escola"
)

contexto_para_dim <- contexto_2024 |>
  select(
    id_escola,
    all_of(
      colunas_contexto_novas
    )
  )

dim_escola_final <- dim_escola |>
  left_join(
    contexto_para_dim,
    by = "id_escola"
  ) |>
  left_join(
    estratos_escola,
    by = "id_escola"
  ) |>
  left_join(
    cadastro_temporal,
    by = "id_escola"
  ) |>
  mutate(
    codigo_inep = na_if(
      str_squish(
        as.character(codigo_inep)
      ),
      ""
    ),
    codigo_inep_valido = case_when(
      is.na(codigo_inep) ~ NA,
      str_detect(
        codigo_inep,
        "^[0-9]{8}$"
      ) ~ TRUE,
      TRUE ~ FALSE
    ),
    assessora_vinculo_administrativo = case_when(
      is.na(assessora) |
        str_squish(assessora) == "" ~
        "Sem vinculação informada",
      TRUE ~ str_squish(assessora)
    ),
    contexto_2024_encontrado =
      !is.na(
        dependencia_administrativa_2024
      ),
    estratos_corrigidos_disponiveis =
      !is.na(
        quartil_infraestrutura_escola
      ) |
      !is.na(
        faixa_porte_contextual_escola
      )
  ) |>
  arrange(
    nome_canonico,
    id_escola
  )

# Conversão controlada de tipos que vieram como texto na dimensão.
colunas_logicas_dim <- c(
  "programa_iniciado_apos_2025_1av",
  "dose_assessoramento_observada",
  "possivel_municipalizacao_recente",
  "municipalizada_apos_2024",
  "privada_vinculada_final",
  "incluir_universo_municipal_direto_2024",
  "incluir_universo_municipal_ampliado_2024",
  "incluir_rede_municipal_operacional_2025",
  "utilizar_indicadores_matriculas",
  "utilizar_indicadores_turmas_docentes",
  "utilizar_indicadores_infraestrutura",
  "requer_revisao_tecnica",
  "codigo_inep_valido",
  "contexto_2024_encontrado",
  "estratos_corrigidos_disponiveis",
  "incluir_diagnostico_avaliativo_ampliado_2025",
  "incluir_diagnostico_avaliativo_ampliado_2026",
  "incluir_resultados_rede_assessorada_2025",
  "incluir_resultados_rede_assessorada_2026",
  "incluir_indice_carga_2025",
  "incluir_indice_carga_2026",
  "incluir_nao_exposto_descritivo_2025",
  "incluir_nao_exposto_descritivo_2026"
)

colunas_inteiras_dim <- c(
  "exposicao_2025_1av",
  "ano",
  "exposicao_programa_binaria_2025",
  "exposicao_programa_binaria_2026"
)

dim_escola_final <- dim_escola_final |>
  mutate(
    across(
      any_of(
        colunas_logicas_dim
      ),
      converter_logico_seguro
    ),
    across(
      any_of(
        colunas_inteiras_dim
      ),
      converter_inteiro_seguro
    )
  )

# -------------------------------------------------------------------
# 11. Base analítica final escola × série
# -------------------------------------------------------------------

# Remove atributos cadastrais do núcleo para que a dimensão final seja
# a fonte única desses campos.
variacao_nucleo <- variacao_qualidade |>
  select(
    -any_of(
      c(
        "codigo_inep",
        "nome_canonico",
        "assessora",
        "quartil_infraestrutura",
        "faixa_porte_contextual",
        "quartil_infraestrutura_escola",
        "faixa_porte_contextual_escola"
      )
    )
  )

base_final <- variacao_nucleo |>
  left_join(
    dim_escola_final,
    by = "id_escola"
  ) |>
  mutate(
    peso_medio_avaliados = case_when(
      !is.na(avaliados_2025) &
        !is.na(avaliados_2026) ~
        (
          avaliados_2025 +
            avaliados_2026
        ) / 2,
      TRUE ~ NA_real_
    ),
    amostra_principal_descritiva =
      coalesce(
        painel_resultado_balanceado,
        FALSE
      ) &
      coalesce(
        contexto_2024_encontrado,
        FALSE
      ),
    amostra_municipal_direta_2024 =
      amostra_principal_descritiva &
      coalesce(
        incluir_universo_municipal_direto_2024,
        FALSE
      ),
    amostra_municipal_ampliada_2024 =
      amostra_principal_descritiva &
      coalesce(
        incluir_universo_municipal_ampliado_2024,
        FALSE
      ),
    amostra_rede_operacional_2025 =
      amostra_principal_descritiva &
      coalesce(
        incluir_rede_municipal_operacional_2025,
        FALSE
      ),
    log_matriculas_anos_iniciais = case_when(
      !is.na(matriculas_anos_iniciais) &
        matriculas_anos_iniciais > 0 ~
        log(
          matriculas_anos_iniciais
        ),
      TRUE ~ NA_real_
    )
  ) |>
  relocate(
    any_of(
      c(
        "id_escola",
        "codigo_inep",
        "codigo_inep_valido",
        "nome_canonico",
        "nome_vinculo_original",
        "chave_nome",
        "assessora",
        "assessora_vinculo_administrativo",
        "pertence_universo_avaliativo_2025",
        "pertence_universo_avaliativo_2026",
        "elegivel_assessoramento_2025",
        "elegivel_assessoramento_2026",
        "recebe_assessoramento_2025",
        "recebe_assessoramento_2026",
        "exposicao_programa_binaria_2025",
        "exposicao_programa_binaria_2026",
        "assessora_gerencial_2025",
        "assessora_gerencial_2026",
        "status_carga_operacional_2025",
        "status_carga_operacional_2026",
        "grupo_exposicao_2025",
        "grupo_exposicao_2026",
        "incluir_diagnostico_avaliativo_ampliado_2025",
        "incluir_diagnostico_avaliativo_ampliado_2026",
        "incluir_resultados_rede_assessorada_2025",
        "incluir_resultados_rede_assessorada_2026",
        "incluir_indice_carga_2025",
        "incluir_indice_carga_2026",
        "incluir_nao_exposto_descritivo_2025",
        "incluir_nao_exposto_descritivo_2026",
        "status_homologacao_2025",
        "status_homologacao_2026",
        "versao_regra_2025",
        "versao_regra_2026",
        "ano_escolar",
        "componente",
        "painel_balanceado",
        "painel_resultado_balanceado",
        "contexto_2024_encontrado",
        "amostra_principal_descritiva",
        "amostra_municipal_direta_2024",
        "amostra_municipal_ampliada_2024",
        "amostra_rede_operacional_2025",
        "incluir_universo_municipal_direto_2024",
        "incluir_universo_municipal_ampliado_2024",
        "incluir_rede_municipal_operacional_2025",
        "grupo_administrativo_2024_final",
        "tipo_vinculo_rede_final",
        "status_rede_2025_final",
        "dependencia_administrativa_2024",
        "situacao_funcionamento_2024",
        "municipalizada_apos_2024",
        "privada_vinculada_final",
        "matriculas_anos_iniciais",
        "turmas_anos_iniciais",
        "docentes_anos_iniciais",
        "alunos_por_turma_anos_iniciais",
        "alunos_por_docente_anos_iniciais",
        "pct_matriculas_anos_iniciais_integral",
        "pct_matriculas_educacao_especial",
        "pct_matriculas_transporte_publico",
        "pct_matriculas_preta_parda_indigena",
        "numero_etapas_amplas_ofertadas",
        "indice_infraestrutura_basica",
        "quartil_infraestrutura_escola",
        "faixa_porte_contextual_escola",
        "estratos_corrigidos_disponiveis",
        "previstos_2025",
        "avaliados_2025",
        "taxa_participacao_2025",
        "proficiencia_media_2025",
        "pct_defasagem_2025",
        "pct_intermediario_2025",
        "pct_adequado_2025",
        "previstos_2026",
        "avaliados_2026",
        "taxa_participacao_2026",
        "proficiencia_media_2026",
        "pct_defasagem_2026",
        "pct_intermediario_2026",
        "pct_adequado_2026",
        "peso_medio_avaliados",
        "delta_participacao",
        "delta_proficiencia",
        "delta_pct_defasagem",
        "delta_pct_intermediario",
        "delta_pct_adequado",
        "variacao_relativa_previstos",
        "variacao_relativa_avaliados",
        "observacao_composicao"
      )
    )
  ) |>
  arrange(
    ano_escolar,
    componente,
    nome_canonico,
    id_escola
  )

chaves_nucleo_antes_join <- variacao_nucleo |>
  distinct(
    across(
      all_of(chave_escola_serie)
    )
  )

chaves_base_depois_join <- base_final |>
  distinct(
    across(
      all_of(chave_escola_serie)
    )
  )

chaves_perdidas_no_join <- chaves_nucleo_antes_join |>
  anti_join(
    chaves_base_depois_join,
    by = chave_escola_serie
  )

chaves_acrescentadas_no_join <- chaves_base_depois_join |>
  anti_join(
    chaves_nucleo_antes_join,
    by = chave_escola_serie
  )

nucleo_educacional_antes <- variacao_nucleo |>
  arrange(
    id_escola,
    ano_escolar,
    componente
  )

nucleo_educacional_depois <- base_final |>
  select(
    all_of(names(variacao_nucleo))
  ) |>
  arrange(
    id_escola,
    ano_escolar,
    componente
  )

campos_educacionais_identicos <- identical(
  nucleo_educacional_antes,
  nucleo_educacional_depois
)

escolas_sem_cinco_series <- base_final |>
  distinct(
    id_escola,
    ano_escolar,
    componente
  ) |>
  count(
    id_escola,
    componente,
    name = "numero_series"
  ) |>
  filter(
    numero_series != 5L
  )

diagnostico_integridade_join <- tibble(
  indicador = c(
    "linhas_nucleo_antes",
    "linhas_base_depois",
    "escolas_nucleo_antes",
    "escolas_base_depois",
    "chaves_perdidas",
    "chaves_acrescentadas",
    "campos_educacionais_identicos",
    "escolas_sem_cinco_series"
  ),
  valor_observado = c(
    as.character(nrow(variacao_nucleo)),
    as.character(nrow(base_final)),
    as.character(n_distinct(variacao_nucleo$id_escola)),
    as.character(n_distinct(base_final$id_escola)),
    as.character(nrow(chaves_perdidas_no_join)),
    as.character(nrow(chaves_acrescentadas_no_join)),
    as.character(campos_educacionais_identicos),
    as.character(nrow(escolas_sem_cinco_series))
  ),
  criterio = c(
    "280",
    "280",
    "56",
    "56",
    "0",
    "0",
    "TRUE",
    "0"
  )
)

write_csv(
  diagnostico_integridade_join,
  file.path(
    pasta_execucao,
    "07A_integridade_join_cadastro.csv"
  ),
  na = ""
)

write_csv(
  chaves_perdidas_no_join,
  file.path(
    pasta_execucao,
    "07B_chaves_perdidas_no_join.csv"
  ),
  na = ""
)

write_csv(
  chaves_acrescentadas_no_join,
  file.path(
    pasta_execucao,
    "07C_chaves_acrescentadas_no_join.csv"
  ),
  na = ""
)

write_csv(
  escolas_sem_cinco_series,
  file.path(
    pasta_execucao,
    "07D_escolas_sem_cinco_series.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 12. Diagnósticos de cobertura e consistência
# -------------------------------------------------------------------

ids_sem_dimensao <- variacao_qualidade |>
  distinct(
    id_escola
  ) |>
  anti_join(
    dim_escola_final |>
      distinct(
        id_escola
      ),
    by = "id_escola"
  )

write_csv(
  ids_sem_dimensao,
  file.path(
    pasta_execucao,
    "07_ids_variacao_sem_dimensao.csv"
  ),
  na = ""
)

duplicidades_base_final <- identificar_duplicidades(
  base_final,
  chave_escola_serie,
  "base_final"
)

write_csv(
  duplicidades_base_final,
  file.path(
    pasta_execucao,
    "08_duplicidades_base_final.csv"
  ),
  na = ""
)

codigos_inep_duplicados <- dim_escola_final |>
  filter(
    codigo_inep_valido %in% TRUE
  ) |>
  count(
    codigo_inep,
    name = "numero_escolas"
  ) |>
  filter(
    numero_escolas > 1
  )

write_csv(
  codigos_inep_duplicados,
  file.path(
    pasta_execucao,
    "09_codigos_inep_duplicados.csv"
  ),
  na = ""
)

cobertura_base_final <- base_final |>
  group_by(
    ano_escolar,
    componente
  ) |>
  summarise(
    linhas_escola_serie = n(),
    escolas = n_distinct(
      id_escola
    ),
    painel_presenca_balanceado = sum(
      painel_balanceado,
      na.rm = TRUE
    ),
    painel_resultado_balanceado = sum(
      painel_resultado_balanceado,
      na.rm = TRUE
    ),
    com_contexto_2024 = sum(
      contexto_2024_encontrado,
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
    com_estratos_corrigidos = sum(
      estratos_corrigidos_disponiveis,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(
    ano_escolar,
    componente
  )

write_csv(
  cobertura_base_final,
  file.path(
    pasta_execucao,
    "10_cobertura_base_final.csv"
  ),
  na = ""
)

completude_base_final <- map_dfr(
  names(base_final),
  function(variavel) {
    x <- base_final[[variavel]]

    vazios_texto <- if (
      is.character(x)
    ) {
      sum(
        !is.na(x) &
          str_squish(x) == ""
      )
    } else {
      0L
    }

    tibble(
      ordem_coluna = match(
        variavel,
        names(base_final)
      ),
      variavel = variavel,
      classe_r = paste(
        class(x),
        collapse = " | "
      ),
      valores_ausentes = sum(
        is.na(x)
      ),
      valores_vazios_texto = vazios_texto,
      percentual_ausente = 100 *
        sum(
          is.na(x)
        ) /
        nrow(base_final),
      valores_distintos = n_distinct(
        x,
        na.rm = TRUE
      )
    )
  }
)

write_csv(
  completude_base_final,
  file.path(
    pasta_execucao,
    "11_completude_variaveis_base_final.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 13. Comparação com a base integrada anterior
# -------------------------------------------------------------------

chaves_apenas_base_final <- base_final |>
  distinct(
    across(
      all_of(
        chave_escola_serie
      )
    )
  ) |>
  anti_join(
    base_integrada_anterior |>
      distinct(
        across(
          all_of(
            chave_escola_serie
          )
        )
      ),
    by = chave_escola_serie
  )

chaves_apenas_base_anterior <- base_integrada_anterior |>
  distinct(
    across(
      all_of(
        chave_escola_serie
      )
    )
  ) |>
  anti_join(
    base_final |>
      distinct(
        across(
          all_of(
            chave_escola_serie
          )
        )
      ),
    by = chave_escola_serie
  )

write_csv(
  chaves_apenas_base_final,
  file.path(
    pasta_execucao,
    "12_chaves_apenas_base_final.csv"
  ),
  na = ""
)

write_csv(
  chaves_apenas_base_anterior,
  file.path(
    pasta_execucao,
    "13_chaves_apenas_base_integrada_anterior.csv"
  ),
  na = ""
)

# As colunas usadas como chave podem ser numéricas (especialmente
# `id_escola` e `ano_escolar`). Elas não devem entrar no conjunto de
# variáveis comparadas, pois seriam renomeadas em `anterior_comparacao`
# e deixariam de estar disponíveis para o `inner_join()`.
colunas_numericas_comuns <- setdiff(
  intersect(
    names(
      base_final
    )[
      map_lgl(
        base_final,
        is.numeric
      )
    ],
    names(
      base_integrada_anterior
    )[
      map_lgl(
        base_integrada_anterior,
        is.numeric
      )
    ]
  ),
  chave_escola_serie
)

comparacao_numerica <- if (
  length(colunas_numericas_comuns) > 0
) {
  anterior_comparacao <- base_integrada_anterior |>
    select(
      all_of(
        chave_escola_serie
      ),
      all_of(
        colunas_numericas_comuns
      )
    ) |>
    rename_with(
      ~ paste0(
        .x,
        "__anterior"
      ),
      all_of(
        colunas_numericas_comuns
      )
    )

  base_comparacao <- base_final |>
    select(
      all_of(
        chave_escola_serie
      ),
      all_of(
        colunas_numericas_comuns
      )
    ) |>
    inner_join(
      anterior_comparacao,
      by = chave_escola_serie
    )

  map_dfr(
    colunas_numericas_comuns,
    function(variavel) {
      atual <- base_comparacao[[variavel]]
      anterior <- base_comparacao[[
        paste0(
          variavel,
          "__anterior"
        )
      ]]

      diferenca <- abs(
        atual - anterior
      )

      tibble(
        variavel = variavel,
        observacoes_comparaveis = sum(
          !is.na(atual) &
            !is.na(anterior)
        ),
        divergencias_maiores_1e_8 = sum(
          diferenca > 1e-8,
          na.rm = TRUE
        ),
        diferenca_absoluta_maxima = if (
          all(
            is.na(diferenca)
          )
        ) {
          NA_real_
        } else {
          max(
            diferenca,
            na.rm = TRUE
          )
        }
      )
    }
  )
} else {
  tibble(
    variavel = character(),
    observacoes_comparaveis = integer(),
    divergencias_maiores_1e_8 = integer(),
    diferenca_absoluta_maxima = double()
  )
}

write_csv(
  comparacao_numerica,
  file.path(
    pasta_execucao,
    "14_comparacao_numerica_base_anterior.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 14. Quadro de validação final
# -------------------------------------------------------------------

colunas_percentuais <- c(
  "taxa_participacao_2025",
  "taxa_participacao_2026",
  "pct_defasagem_2025",
  "pct_defasagem_2026",
  "pct_intermediario_2025",
  "pct_intermediario_2026",
  "pct_adequado_2025",
  "pct_adequado_2026",
  "pct_matriculas_anos_iniciais_integral",
  "pct_matriculas_educacao_especial",
  "pct_matriculas_transporte_publico",
  "pct_matriculas_preta_parda_indigena"
)

percentuais_fora_intervalo <- somar_fora_intervalo(
  base_final,
  colunas_percentuais,
  0,
  100
)

colunas_contagens <- intersect(
  c(
    "numero_turmas_2025",
    "numero_turmas_2026",
    "previstos_2025",
    "previstos_2026",
    "avaliados_2025",
    "avaliados_2026",
    "matriculas_anos_iniciais",
    "turmas_anos_iniciais",
    "docentes_anos_iniciais"
  ),
  names(base_final)
)

contagens_negativas <- if (
  length(colunas_contagens) > 0
) {
  base_final |>
    select(
      all_of(
        colunas_contagens
      )
    ) |>
    pivot_longer(
      everything(),
      names_to = "variavel",
      values_to = "valor"
    ) |>
    summarise(
      total = sum(
        !is.na(valor) &
          valor < 0
      )
    ) |>
    pull(total)
} else {
  0L
}

soma_niveis_diverge <- base_final |>
  transmute(
    diferenca_2025 = abs(
      pct_defasagem_2025 +
        pct_intermediario_2025 +
        pct_adequado_2025 -
        100
    ),
    diferenca_2026 = abs(
      pct_defasagem_2026 +
        pct_intermediario_2026 +
        pct_adequado_2026 -
        100
    )
  ) |>
  summarise(
    total = sum(
      diferenca_2025 > 1,
      na.rm = TRUE
    ) +
      sum(
        diferenca_2026 > 1,
        na.rm = TRUE
      )
  ) |>
  pull(total)

series_fora_1a5 <- sum(
  !is.na(base_final$ano_escolar) &
    !base_final$ano_escolar %in% 1:5
)

componentes_fora_lp <- sum(
  !is.na(base_final$componente) &
    base_final$componente !=
      "Língua Portuguesa"
)

validacao_final <- bind_rows(
  validacao_cadastro_canonico,
  registrar_validacao(
    "Chave final única",
    "estrutura",
    "erro",
    nrow(duplicidades_base_final),
    "zero duplicidades em id_escola + ano_escolar + componente",
    nrow(duplicidades_base_final) == 0,
    "A base final deve ter uma linha por escola × série × componente."
  ),
  registrar_validacao(
    "IDs do núcleo presentes na dimensão",
    "integração",
    "erro",
    nrow(ids_sem_dimensao),
    "zero IDs sem dimensão",
    nrow(ids_sem_dimensao) == 0,
    "Todo id_escola do núcleo longitudinal deve existir em dim_escola_final."
  ),
  registrar_validacao(
    "Código INEP duplicado",
    "identificação",
    "erro",
    nrow(codigos_inep_duplicados),
    "zero códigos válidos associados a mais de uma escola",
    nrow(codigos_inep_duplicados) == 0,
    "Códigos INEP válidos devem identificar uma única escola."
  ),
  registrar_validacao(
    "Percentuais dentro de 0 a 100",
    "domínio",
    "erro",
    percentuais_fora_intervalo,
    "zero valores fora do intervalo",
    percentuais_fora_intervalo == 0,
    "Abrange participação, padrões de desempenho e percentuais contextuais."
  ),
  registrar_validacao(
    "Contagens não negativas",
    "domínio",
    "erro",
    contagens_negativas,
    "zero contagens negativas",
    contagens_negativas == 0,
    "Abrange turmas, previstos, avaliados, matrículas e docentes."
  ),
  registrar_validacao(
    "Soma dos padrões de desempenho",
    "consistência",
    "aviso",
    soma_niveis_diverge,
    "preferencialmente zero divergências superiores a 1 p.p.",
    soma_niveis_diverge == 0,
    "Divergências podem decorrer de arredondamento ou ausência parcial."
  ),
  registrar_validacao(
    "Anos escolares esperados",
    "escopo",
    "aviso",
    series_fora_1a5,
    "zero observações fora do 1º ao 5º ano",
    series_fora_1a5 == 0,
    "O escopo declarado do estudo compreende o 1º ao 5º ano."
  ),
  registrar_validacao(
    "Componente curricular esperado",
    "escopo",
    "aviso",
    componentes_fora_lp,
    "zero observações fora de Língua Portuguesa",
    componentes_fora_lp == 0,
    "O painel comparável atual é de Língua Portuguesa."
  ),
  registrar_validacao(
    "Dimensão final possui 56 escolas",
    "cobertura",
    "erro",
    n_distinct(
      dim_escola_final$id_escola
    ),
    "exatamente 56 escolas",
    n_distinct(
      dim_escola_final$id_escola
    ) == snapshot_homologado$escolas_cadastradas,
    "O perfil geral preserva as três escolas fora da carga."
  ),
  registrar_validacao(
    "Base final possui 56 escolas",
    "cobertura",
    "erro",
    n_distinct(
      base_final$id_escola
    ),
    "exatamente 56 escolas",
    n_distinct(
      base_final$id_escola
    ) == snapshot_homologado$escolas_cadastradas,
    "O núcleo educacional completo não pode ser filtrado pela carga."
  ),
  registrar_validacao(
    "Base final possui 280 linhas escola-série",
    "cobertura",
    "erro",
    nrow(base_final),
    "exatamente 280 linhas",
    nrow(base_final) ==
      snapshot_homologado$linhas_escola_serie,
    "Cada uma das 56 escolas deve conservar as cinco séries."
  ),
  registrar_validacao(
    "Cadastro temporal possui 56 linhas",
    "integração",
    "erro",
    nrow(cadastro_temporal),
    "exatamente 56 linhas e uma por escola",
    nrow(cadastro_temporal) ==
      snapshot_homologado$escolas_cadastradas &&
      n_distinct(cadastro_temporal$id_escola) ==
        snapshot_homologado$escolas_cadastradas,
    "O cadastro é pivotado antes do join para impedir duplicação."
  ),
  registrar_validacao(
    "Join preserva todas as chaves educacionais",
    "integração",
    "erro",
    nrow(chaves_perdidas_no_join) +
      nrow(chaves_acrescentadas_no_join),
    "zero chaves perdidas ou acrescentadas",
    nrow(chaves_perdidas_no_join) == 0 &&
      nrow(chaves_acrescentadas_no_join) == 0,
    "A integração canônica não pode modificar o universo educacional."
  ),
  registrar_validacao(
    "Join preserva os campos educacionais",
    "integração",
    "erro",
    campos_educacionais_identicos,
    "TRUE",
    campos_educacionais_identicos,
    "Todos os campos do núcleo devem permanecer idênticos após o join."
  ),
  registrar_validacao(
    "Cinco séries por escola",
    "cobertura",
    "erro",
    nrow(escolas_sem_cinco_series),
    "zero escolas com número de séries diferente de cinco",
    nrow(escolas_sem_cinco_series) == 0,
    "A base potencial deve manter 56 × 5 = 280 linhas."
  ),
  registrar_validacao(
    "Conflitos de atributos entre fontes",
    "rastreabilidade",
    "aviso",
    nrow(conflitos_atributos),
    "revisar conflitos, quando houver",
    nrow(conflitos_atributos) == 0,
    "A dimensão final é a fonte principal, mas divergências ficam documentadas."
  ),
  registrar_validacao(
    "Chaves novas em relação à base integrada anterior",
    "rastreabilidade",
    "aviso",
    nrow(chaves_apenas_base_final),
    "revisar inclusões, quando houver",
    nrow(chaves_apenas_base_final) == 0,
    "O núcleo final parte da base de variação com qualidade."
  ),
  registrar_validacao(
    "Chaves ausentes em relação à base integrada anterior",
    "rastreabilidade",
    "aviso",
    nrow(chaves_apenas_base_anterior),
    "revisar exclusões, quando houver",
    nrow(chaves_apenas_base_anterior) == 0,
    "Nenhuma exclusão silenciosa deve ocorrer."
  )
)

write_csv(
  validacao_final,
  file.path(
    pasta_execucao,
    "15_validacao_final.csv"
  ),
  na = ""
)

erros_finais <- validacao_final |>
  filter(
    severidade == "erro",
    status == "revisar"
  )

if (nrow(erros_finais) > 0) {
  stop(
    "A base final não foi exportada porque há validações críticas ",
    "não aprovadas. Consulte:\n",
    file.path(
      pasta_execucao,
      "15_validacao_final.csv"
    )
  )
}

# -------------------------------------------------------------------
# 15. Dicionário automatizado da base final
# -------------------------------------------------------------------

descricoes_principais <- c(
  id_escola = "Identificador longitudinal interno e estável da escola.",
  codigo_inep = "Código INEP associado à escola, mantido como texto.",
  codigo_inep_valido = "Indica se o código INEP possui exatamente oito dígitos.",
  nome_canonico = "Nome padronizado da escola adotado no projeto.",
  assessora = "Vínculo administrativo original entre escola e assessora.",
  assessora_vinculo_administrativo = "Vínculo administrativo cadastral; não comprova exposição ao programa.",
  pertence_universo_avaliativo_2025 = "Presença observada na primeira avaliação formativa de 2025.",
  pertence_universo_avaliativo_2026 = "Presença observada na primeira avaliação formativa de 2026.",
  elegivel_assessoramento_2025 = "Elegibilidade institucional em 2025; linha de base pré-programa.",
  elegivel_assessoramento_2026 = "Elegibilidade institucional homologada para 2026.",
  recebe_assessoramento_2025 = "Exposição institucional em 2025, obrigatoriamente igual a NAO.",
  recebe_assessoramento_2026 = "Exposição institucional homologada em 2026; não representa dose.",
  exposicao_programa_binaria_2025 = "Indicador de exposição em 2025, obrigatoriamente igual a zero.",
  exposicao_programa_binaria_2026 = "Indicador binário de exposição institucional em 2026; não representa dose.",
  assessora_gerencial_2025 = "Responsável gerencial por exposição em 2025; deve permanecer vazia.",
  assessora_gerencial_2026 = "Responsável gerencial homologada no cadastro canônico de 2026.",
  incluir_indice_carga_2025 = "Inclusão no índice de carga em 2025; obrigatoriamente FALSE.",
  incluir_indice_carga_2026 = "Inclusão no índice operacional de carga de 2026.",
  grupo_exposicao_2026 = "Grupo descritivo de exposição em 2026; não constitui grupo causal.",
  versao_regra_2026 = "Versão da homologação institucional aplicada ao período de 2026.",
  ano_escolar = "Ano escolar do Ensino Fundamental, de 1 a 5.",
  componente = "Componente curricular analisado.",
  painel_balanceado = "Indica presença da escola-série nos dois anos.",
  painel_resultado_balanceado = "Indica disponibilidade de resultado nos dois anos.",
  contexto_2024_encontrado = "Indica disponibilidade do contexto estrutural pré-programa de 2024.",
  amostra_principal_descritiva = "Resultado balanceado e contexto de 2024 disponível; não implica amostra causal.",
  amostra_municipal_direta_2024 = "Amostra principal restrita à rede municipal direta em 2024.",
  amostra_municipal_ampliada_2024 = "Amostra principal no universo municipal ampliado de 2024.",
  amostra_rede_operacional_2025 = "Amostra principal no universo operacional da rede em 2025.",
  matriculas_anos_iniciais = "Matrículas nos anos iniciais segundo o contexto de 2024.",
  turmas_anos_iniciais = "Número de turmas dos anos iniciais no contexto de 2024.",
  docentes_anos_iniciais = "Número de docentes dos anos iniciais no contexto de 2024.",
  indice_infraestrutura_basica = "Índice descritivo de disponibilidade de infraestrutura básica em 2024.",
  quartil_infraestrutura_escola = "Estrato de infraestrutura corrigido no nível da escola pelo módulo 13A.",
  faixa_porte_contextual_escola = "Faixa de porte contextual corrigida no nível da escola pelo módulo 13A.",
  previstos_2025 = "Estudantes previstos na primeira avaliação formativa de 2025.",
  avaliados_2025 = "Estudantes avaliados na primeira avaliação formativa de 2025.",
  taxa_participacao_2025 = "Percentual de participação observado em 2025.",
  proficiencia_media_2025 = "Proficiência média ponderada pelos avaliados em 2025.",
  pct_defasagem_2025 = "Percentual de estudantes no padrão defasagem em 2025.",
  pct_intermediario_2025 = "Percentual de estudantes no padrão intermediário em 2025.",
  pct_adequado_2025 = "Percentual de estudantes no padrão adequado em 2025.",
  previstos_2026 = "Estudantes previstos na primeira avaliação formativa de 2026.",
  avaliados_2026 = "Estudantes avaliados na primeira avaliação formativa de 2026.",
  taxa_participacao_2026 = "Percentual de participação observado em 2026.",
  proficiencia_media_2026 = "Proficiência média ponderada pelos avaliados em 2026.",
  pct_defasagem_2026 = "Percentual de estudantes no padrão defasagem em 2026.",
  pct_intermediario_2026 = "Percentual de estudantes no padrão intermediário em 2026.",
  pct_adequado_2026 = "Percentual de estudantes no padrão adequado em 2026.",
  peso_medio_avaliados = "Média do número de avaliados em 2025 e 2026, usada apenas como peso descritivo.",
  delta_participacao = "Diferença em pontos percentuais entre participação de 2026 e 2025.",
  delta_proficiencia = "Diferença observada entre proficiência de 2026 e 2025; sem interpretação causal.",
  delta_pct_defasagem = "Diferença em pontos percentuais no padrão defasagem entre 2026 e 2025.",
  delta_pct_intermediario = "Diferença em pontos percentuais no padrão intermediário entre 2026 e 2025.",
  delta_pct_adequado = "Diferença em pontos percentuais no padrão adequado entre 2026 e 2025.",
  variacao_relativa_previstos = "Variação percentual do número de previstos entre 2025 e 2026.",
  variacao_relativa_avaliados = "Variação percentual do número de avaliados entre 2025 e 2026.",
  observacao_composicao = "Alerta descritivo sobre mudanças de participação, público previsto ou baixo número de avaliados."
)

variaveis_derivadas_modulo_15 <- c(
  "codigo_inep_valido",
  "assessora_vinculo_administrativo",
  "contexto_2024_encontrado",
  "estratos_corrigidos_disponiveis",
  "peso_medio_avaliados",
  "amostra_principal_descritiva",
  "amostra_municipal_direta_2024",
  "amostra_municipal_ampliada_2024",
  "amostra_rede_operacional_2025",
  "log_matriculas_anos_iniciais"
)

origem_variavel <- function(variavel) {
  fontes <- character()

  if (variavel %in% names(dim_escola)) {
    fontes <- c(
      fontes,
      "dim_escola.csv"
    )
  }

  if (variavel %in% names(contexto_2024)) {
    fontes <- c(
      fontes,
      "dim_contexto_escola_2024_final.csv"
    )
  }

  if (variavel %in% names(variacao_qualidade)) {
    fontes <- c(
      fontes,
      "variacao_escola_serie_com_qualidade.csv"
    )
  }

  if (variavel %in% names(cadastro_temporal)) {
    fontes <- c(
      fontes,
      "cadastro_exposicao_assessoramento.csv"
    )
  }

  if (
    variavel %in% c(
      "quartil_infraestrutura_escola",
      "faixa_porte_contextual_escola"
    )
  ) {
    fontes <- c(
      fontes,
      "base_contexto_desempenho_estratos_corrigidos.csv"
    )
  }

  if (variavel %in% variaveis_derivadas_modulo_15) {
    fontes <- c(
      fontes,
      "derivada no módulo 15"
    )
  }

  if (length(fontes) == 0) {
    fontes <- "origem não classificada automaticamente"
  }

  paste(
    unique(fontes),
    collapse = " | "
  )
}

unidade_variavel <- function(variavel) {
  case_when(
    str_detect(
      variavel,
      "^pct_|taxa_participacao|delta_participacao|delta_pct_"
    ) ~ "percentual ou ponto percentual",
    str_detect(
      variavel,
      "matriculas|turmas|docentes|previstos|avaliados|numero_"
    ) ~ "contagem",
    str_detect(
      variavel,
      "proficiencia"
    ) ~ "escala de proficiência",
    str_detect(
      variavel,
      "indice_"
    ) ~ "índice",
    TRUE ~ "não aplicável"
  )
}

nivel_conceitual_variavel <- function(variavel) {
  if (
    variavel %in% names(dim_escola_final)
  ) {
    return(
      "escola, repetida na base escola × série"
    )
  }

  "escola × série"
}

dicionario_base_final <- map_dfr(
  names(base_final),
  function(variavel) {
    x <- base_final[[variavel]]

    # A indexação com `[[variavel]]` aborta quando a variável não possui
    # descrição manual. A indexação com `[` devolve NA e permite acionar
    # com segurança a descrição automática de fallback.
    descricao <- unname(
      descricoes_principais[variavel]
    )

    if (
      length(descricao) == 0 ||
        is.na(descricao[[1]]) ||
        !nzchar(descricao[[1]])
    ) {
      descricao <- paste0(
        "Variável ",
        str_replace_all(
          variavel,
          "_",
          " "
        ),
        ". Consulte a origem e o módulo de criação para definição operacional."
      )
    } else {
      descricao <- descricao[[1]]
    }

    tibble(
      ordem_coluna = match(
        variavel,
        names(base_final)
      ),
      variavel = variavel,
      classe_r = paste(
        class(x),
        collapse = " | "
      ),
      tipo_r = typeof(x),
      nivel_conceitual =
        nivel_conceitual_variavel(
          variavel
        ),
      unidade = unidade_variavel(
        variavel
      ),
      origem = origem_variavel(
        variavel
      ),
      descricao = descricao,
      cuidado_interpretativo = case_when(
        str_detect(
          variavel,
          "delta_|proficiencia|pct_adequado|pct_defasagem"
        ) ~
          "Resultado observacional; não atribuir causalmente ao assessoramento ou à assessora.",
        variavel %in% c(
          "assessora",
          "assessora_vinculo_administrativo"
        ) ~
          "Vínculo administrativo; não comprova exposição, intensidade ou efeito do trabalho.",
        str_detect(
          variavel,
          "^assessora_gerencial_"
        ) ~
          "Responsabilidade gerencial homologada no período; não representa qualidade, intensidade ou efeito.",
        str_detect(
          variavel,
          "exposicao|recebe_assessoramento|grupo_exposicao"
        ) ~
          "Exposição institucional binária; não representa dose e não autoriza interpretação causal.",
        TRUE ~
          NA_character_
      )
    )
  }
)

# -------------------------------------------------------------------
# 16. Exportação com histórico
# -------------------------------------------------------------------

classificacao_pipeline_anterior <- classificar_pipeline_existente(
  arquivos_saida[["base_final_csv"]]
)

produtos_anteriores <- tibble(
  produto = names(arquivos_saida),
  caminho_original = as.character(arquivos_saida),
  existia = file.exists(arquivos_saida),
  md5_original = map_chr(
    arquivos_saida,
    hash_md5
  )
)

destinos_historico <- map2_chr(
  arquivos_saida,
  names(arquivos_saida),
  ~ arquivar_se_existir(
    .x,
    .y,
    classificacao_pipeline_anterior
  )
)

manifesto_historico_anterior <- produtos_anteriores |>
  mutate(
    classificacao = classificacao_pipeline_anterior,
    caminho_historico = destinos_historico,
    md5_historico = map_chr(
      destinos_historico,
      hash_md5
    ),
    copia_integra = case_when(
      !existia ~ TRUE,
      TRUE ~
        !is.na(md5_original) &
        md5_original == md5_historico
    )
  )

write_csv(
  manifesto_historico_anterior,
  file.path(
    pasta_execucao,
    "21_manifesto_historico_anterior.csv"
  ),
  na = ""
)

if (
  any(
    manifesto_historico_anterior$existia &
      !manifesto_historico_anterior$copia_integra
  )
) {
  stop(
    "O arquivamento dos produtos anteriores falhou na conferência MD5. ",
    "Nenhum arquivo canônico foi atualizado. Consulte:\n",
    file.path(
      pasta_execucao,
      "21_manifesto_historico_anterior.csv"
    )
  )
}

write_csv(
  base_final,
  arquivos_saida[["base_final_csv"]],
  na = ""
)

saveRDS(
  base_final,
  arquivos_saida[["base_final_rds"]],
  version = 3
)

write_csv(
  dim_escola_final,
  arquivos_saida[["dim_final_csv"]],
  na = ""
)

saveRDS(
  dim_escola_final,
  arquivos_saida[["dim_final_rds"]],
  version = 3
)

write_csv(
  dicionario_base_final,
  arquivos_saida[["dicionario_csv"]],
  na = ""
)

# Cópias imutáveis da nova execução.
write_csv(
  base_final,
  file.path(
    pasta_historico,
    paste0(
      "base_analitica_final_escola_serie_",
      id_execucao,
      ".csv"
    )
  ),
  na = ""
)

saveRDS(
  base_final,
  file.path(
    pasta_historico,
    paste0(
      "base_analitica_final_escola_serie_",
      id_execucao,
      ".rds"
    )
  ),
  version = 3
)

write_csv(
  dim_escola_final,
  file.path(
    pasta_historico,
    paste0(
      "dim_escola_final_",
      id_execucao,
      ".csv"
    )
  ),
  na = ""
)

saveRDS(
  dim_escola_final,
  file.path(
    pasta_historico,
    paste0(
      "dim_escola_final_",
      id_execucao,
      ".rds"
    )
  ),
  version = 3
)

write_csv(
  dicionario_base_final,
  file.path(
    pasta_execucao,
    "16_dicionario_base_analitica_final.csv"
  ),
  na = ""
)

estrutura_saida <- bind_rows(
  inventariar_estrutura(
    dim_escola_final,
    "dim_escola_final"
  ),
  inventariar_estrutura(
    base_final,
    "base_analitica_final_escola_serie"
  )
)

write_csv(
  estrutura_saida,
  file.path(
    pasta_execucao,
    "17_estrutura_bases_saida.csv"
  ),
  na = ""
)

manifesto_produtos <- imap_dfr(
  arquivos_saida,
  function(caminho, produto) {
    info <- file.info(
      caminho
    )

    tibble(
      produto = produto,
      caminho = normalizePath(
        caminho,
        winslash = "/",
        mustWork = TRUE
      ),
      tamanho_bytes = info$size,
      data_modificacao = format(
        info$mtime,
        "%Y-%m-%d %H:%M:%S"
      ),
      md5 = unname(
        tools::md5sum(caminho)
      )
    )
  }
)

write_csv(
  manifesto_produtos,
  file.path(
    pasta_execucao,
    "18_manifesto_produtos_modulo_15.csv"
  ),
  na = ""
)

capture.output(
  sessionInfo(),
  file = file.path(
    pasta_execucao,
    "19_session_info.txt"
  )
)

resumo_execucao <- c(
  paste0(
    "Execução: ",
    id_execucao
  ),
  paste0(
    "Escolas na dimensão final: ",
    n_distinct(
      dim_escola_final$id_escola
    )
  ),
  paste0(
    "Escolas na base analítica: ",
    n_distinct(
      base_final$id_escola
    )
  ),
  paste0(
    "Linhas escola × série: ",
    nrow(base_final)
  ),
  paste0(
    "Universo avaliativo 2025: ",
    sum(
      dim_escola_final[[
        "incluir_diagnostico_avaliativo_ampliado_2025"
      ]],
      na.rm = TRUE
    )
  ),
  paste0(
    "Universo avaliativo 2026: ",
    sum(
      dim_escola_final[[
        "incluir_diagnostico_avaliativo_ampliado_2026"
      ]],
      na.rm = TRUE
    )
  ),
  paste0(
    "Universo de carga 2026: ",
    sum(
      dim_escola_final$incluir_indice_carga_2026,
      na.rm = TRUE
    )
  ),
  paste0(
    "Assessoras no universo de carga 2026: ",
    n_distinct(
      dim_escola_final$assessora_gerencial_2026[
        dim_escola_final[[
          "incluir_indice_carga_2026"
        ]] %in% TRUE
      ],
      na.rm = TRUE
    )
  ),
  paste0(
    "Exposições positivas em 2025: ",
    sum(
      dim_escola_final$exposicao_programa_binaria_2025,
      na.rm = TRUE
    )
  ),
  paste0(
    "Colunas da base final: ",
    ncol(base_final)
  ),
  paste0(
    "Linhas da amostra principal descritiva: ",
    sum(
      base_final$amostra_principal_descritiva,
      na.rm = TRUE
    )
  ),
  paste0(
    "Escolas com contexto de 2024: ",
    n_distinct(
      base_final$id_escola[
        base_final$contexto_2024_encontrado %in%
          TRUE
      ]
    )
  ),
  paste0(
    "Conflitos de atributos documentados: ",
    nrow(conflitos_atributos)
  ),
  paste0(
    "Avisos de validação: ",
    sum(
      validacao_final$severidade == "aviso" &
      validacao_final$status == "revisar"
    )
  ),
  paste0(
    "Script: ",
    basename(caminho_script)
  ),
  paste0(
    "MD5 do script: ",
    hash_md5(caminho_script)
  ),
  paste0(
    "Commit Git de referência: ",
    coalesce(
      commit_git_execucao,
      "indisponível"
    )
  ),
  "",
  "Observação metodológica:",
  paste(
    "A base é descritiva e observacional.",
    "Resultados não devem ser interpretados como efeito causal",
    "do programa ou como medida de qualidade das assessoras."
  )
)

writeLines(
  resumo_execucao,
  con = file.path(
    pasta_execucao,
    "20_resumo_execucao.txt"
  )
)

# -------------------------------------------------------------------
# 17. Resumo no console
# -------------------------------------------------------------------

cat(
  "\nMódulo 15 concluído.\n"
)

cat(
  "\nEstrutura final:\n"
)

print(
  tibble(
    produto = c(
      "Dimensão de escolas",
      "Base escola × série"
    ),
    linhas = c(
      nrow(dim_escola_final),
      nrow(base_final)
    ),
    escolas = c(
      n_distinct(
        dim_escola_final$id_escola
      ),
      n_distinct(
        base_final$id_escola
      )
    ),
    colunas = c(
      ncol(dim_escola_final),
      ncol(base_final)
    )
  )
)

cat(
  "\nCobertura por série:\n"
)

print(
  cobertura_base_final,
  n = Inf,
  width = Inf
)

cat(
  "\nValidações:\n"
)

print(
  validacao_final,
  n = Inf,
  width = Inf
)

cat(
  "\nArquivos principais gerados:\n",
  "- dados_finais/base_analitica_final_escola_serie.csv\n",
  "- dados_finais/base_analitica_final_escola_serie.rds\n",
  "- dados_finais/dim_escola_final.csv\n",
  "- dados_finais/dim_escola_final.rds\n",
  "- documentacao/base_final/dicionario_base_analitica_final.csv\n",
  "- ",
  pasta_execucao,
  "\n",
  "- ",
  pasta_historico,
  "\n",
  sep = ""
)
