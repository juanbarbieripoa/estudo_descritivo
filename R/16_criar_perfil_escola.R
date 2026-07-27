# ===================================================================
# 16_criar_perfil_escola.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# ===================================================================
#
# OBJETIVO
#
# Construir, a partir dos quatro produtos homologados do módulo 15:
#
#   1. perfil_escola_completo: uma linha por escola;
#   2. perfil_escola_gerencial: uma linha por escola, sem campos legados
#      de assessoria;
#   3. perfil_escola_serie_compacto: escola × série × componente;
#   4. dicionário conjunto dos três produtos.
#
# PRINCÍPIOS METODOLÓGICOS
#
# - Estudo observacional e descritivo, sem identificação causal.
# - A primeira avaliação de 2025 é linha de base pré-programa.
# - Exposição, carga e assessora gerencial em 2025 são nulas.
# - Vínculo administrativo não comprova exposição.
# - Resultados educacionais não são atribuídos às assessoras.
# - Sínteses multissérie são descrições operacionais, não índices,
#   escores, rankings, metas ou medidas de efeito.
# - Diferenças de proficiência de ±2 pontos e de participação de
#   ±5 p.p. são heurísticas diagnósticas, não significância estatística.
# - Alertas de ±10 p.p. de participação e 20% de composição também são
#   heurísticas diagnósticas.
# - Quartis de porte e infraestrutura são apenas variáveis contextuais
#   herdadas do módulo 15/contexto 2024 e não são recalculados aqui.
# - A tabela BigQuery
#   caed-smed.avaliacao_educacional_poa.escolas_smed_poa não é entrada.
#
# BLOQUEIOS
#
# - exige branch refatoracao_modulo_21 no commit-base homologado;
# - exige simultaneamente CSV e RDS das duas bases do módulo 15;
# - confere MD5 contra valores fixos e contra o manifesto homologado;
# - exige equivalência semântica CSV–RDS com tipos canônicos;
# - bloqueia qualquer divergência das invariantes oficiais;
# - não inspeciona nem bloqueia alterações residuais do worktree.
#
# EXPORTAÇÃO
#
# A exportação dos sete produtos é transacional:
#
#   1. construção em memória;
#   2. escrita de candidatos;
#   3. releitura e validação;
#   4. preservação histórica pré-11C;
#   5. promoção com rollback;
#   6. conferência dos hashes finais.
# ===================================================================

library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Constantes homologadas e caminhos canônicos
# -------------------------------------------------------------------

instante_execucao <- Sys.time()
id_execucao <- format(
  instante_execucao,
  "%Y%m%d_%H%M%S"
)

commit_base_integracao <- paste0(
  "e51000aa0c58cab3b7ae1a238a6c3b12e",
  "d7563d3"
)

branch_esperada <- "refatoracao_modulo_21"

caminho_script <- here(
  "R",
  "16_criar_perfil_escola.R"
)

caminho_relativo_script <- file.path(
  "R",
  "16_criar_perfil_escola.R"
)

caminho_manifesto_modulo_15 <- here(
  "documentacao",
  "base_final",
  "execucao_20260726_182530",
  "18_manifesto_produtos_modulo_15.csv"
)

arquivos_entrada <- c(
  dim_final_csv = here(
    "dados_finais",
    "dim_escola_final.csv"
  ),
  dim_final_rds = here(
    "dados_finais",
    "dim_escola_final.rds"
  ),
  base_final_csv = here(
    "dados_finais",
    "base_analitica_final_escola_serie.csv"
  ),
  base_final_rds = here(
    "dados_finais",
    "base_analitica_final_escola_serie.rds"
  )
)

hashes_entrada_homologados <- c(
  base_final_csv = "8c284e904dcf61c705d17de57ee4c615",
  base_final_rds = "ae935fe3d6e769d592d88c4c23e84a30",
  dim_final_csv = "4a3874f0287ce16372de4413dfee080c",
  dim_final_rds = "39c8f106912fdbcef99caf0434ef11e2"
)

arquivos_saida <- c(
  perfil_completo_csv = here(
    "dados_finais",
    "perfil_escola_completo.csv"
  ),
  perfil_completo_rds = here(
    "dados_finais",
    "perfil_escola_completo.rds"
  ),
  perfil_gerencial_csv = here(
    "dados_finais",
    "perfil_escola_gerencial.csv"
  ),
  perfil_gerencial_rds = here(
    "dados_finais",
    "perfil_escola_gerencial.rds"
  ),
  perfil_serie_csv = here(
    "dados_finais",
    "perfil_escola_serie_compacto.csv"
  ),
  perfil_serie_rds = here(
    "dados_finais",
    "perfil_escola_serie_compacto.rds"
  ),
  dicionario_csv = here(
    "documentacao",
    "perfil_escola",
    "dicionario_perfil_escola_gerencial.csv"
  )
)

hashes_produtos_pre_11C <- c(
  perfil_completo_csv = "b822cf3cdbaf661a6943deac44020a83",
  perfil_completo_rds = "ef90dea2317c55aa497f3e894471550c",
  perfil_gerencial_csv = "ededd2a96d64d4cc00cbab7e47a173ce",
  perfil_gerencial_rds = "55eec136dbe6f9a74f58a85d38d8a9f7",
  perfil_serie_csv = "fd034011d0cddd2967090097892b01f4",
  perfil_serie_rds = "6faa59eab9e918e6ac57afb2da91d63d",
  dicionario_csv = "697c6710233000fcb41b4b33fc73e42c"
)

ids_excluidos_carga <- c(
  "ESC_001",
  "ESC_055",
  "ESC_056"
)

codigos_excluidos_carga <- c(
  "43105416",
  "43105300",
  "43189768"
)

pares_excluidos_carga <- tribble(
  ~id_escola, ~codigo_inep,
  "ESC_001", "43105416",
  "ESC_055", "43105300",
  "ESC_056", "43189768"
)

campos_temporais_minimos <- c(
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
  "assessora_vinculo_administrativo"
)

pasta_documentacao <- here(
  "documentacao",
  "perfil_escola"
)

pasta_execucao <- file.path(
  pasta_documentacao,
  paste0("execucao_", id_execucao)
)

pasta_historico_pre_11C <- here(
  "dados_finais",
  "historico",
  "perfil_escola",
  "pre_11C_modulo_16"
)

pasta_manifesto_historico <- here(
  "documentacao",
  "perfil_escola",
  "historico_pre_11C"
)

caminho_manifesto_historico_fixo <- file.path(
  pasta_manifesto_historico,
  "manifesto_produtos_pre_11C_modulo_16.csv"
)

caminho_script_historico <- here(
  "R",
  "historico_pre_11C",
  "modulo_16",
  "16_criar_perfil_escola_pre_11C.R.txt"
)

pasta_transacao <- here(
  "dados_finais",
  "historico",
  "perfil_escola",
  "transacoes",
  paste0("execucao_", id_execucao)
)

pasta_candidatos <- file.path(
  pasta_transacao,
  "candidatos"
)

pasta_rollback <- file.path(
  pasta_transacao,
  "rollback"
)

# -------------------------------------------------------------------
# 2. Funções auxiliares gerais
# -------------------------------------------------------------------

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

normalizar_caminho <- function(
    caminho,
    deve_existir = TRUE
) {
  normalizePath(
    caminho,
    winslash = "/",
    mustWork = deve_existir
  )
}

executar_git <- function(argumentos) {
  saida <- tryCatch(
    suppressWarnings(
      system2(
        "git",
        argumentos,
        stdout = TRUE,
        stderr = FALSE
      )
    ),
    error = function(e) character()
  )

  status <- attr(
    saida,
    "status"
  )

  if (
    !is.null(status) &&
      status != 0
  ) {
    return(character())
  }

  str_squish(
    as.character(saida)
  )
}

obter_commit_git <- function() {
  saida <- executar_git(
    c(
      "-C",
      shQuote(here()),
      "rev-parse",
      "HEAD"
    )
  )

  saida <- saida[
    str_detect(
      saida,
      "^[0-9a-fA-F]{40}$"
    )
  ]

  if (length(saida) != 1) {
    return(NA_character_)
  }

  str_to_lower(
    saida[[1]]
  )
}

obter_branch_git <- function() {
  saida <- executar_git(
    c(
      "-C",
      shQuote(here()),
      "branch",
      "--show-current"
    )
  )

  if (length(saida) != 1) {
    return(NA_character_)
  }

  saida[[1]]
}

tipo_canonico <- function(x) {
  case_when(
    inherits(x, "Date") ~ "date",
    inherits(x, "POSIXct") ~ "datetime",
    is.logical(x) ~ "logical",
    is.integer(x) ~ "integer",
    is.double(x) ~ "double",
    is.character(x) ~ "character",
    TRUE ~ paste(
      class(x),
      collapse = " | "
    )
  )
}

converter_logico_seguro <- function(
    x,
    variavel
) {
  x_limpo <- x |>
    as.character() |>
    str_squish() |>
    str_to_lower()

  resultado <- case_when(
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

  invalidos <- !is.na(x_limpo) &
    x_limpo != "" &
    is.na(resultado)

  if (any(invalidos)) {
    stop(
      "Valores lógicos inválidos na coluna `",
      variavel,
      "`: ",
      paste(
        sort(
          unique(
            x[invalidos]
          )
        ),
        collapse = " | "
      )
    )
  }

  resultado
}

converter_por_modelo <- function(
    x,
    modelo,
    variavel
) {
  tipo <- tipo_canonico(
    modelo
  )

  resultado <- switch(
    tipo,
    character = as.character(x),
    logical = converter_logico_seguro(
      x,
      variavel
    ),
    integer = suppressWarnings(
      as.integer(x)
    ),
    double = suppressWarnings(
      as.double(x)
    ),
    date = as.Date(x),
    datetime = as.POSIXct(
      x,
      tz = if (
        is.null(
          attr(
            modelo,
            "tzone"
          )
        )
      ) {
        "UTC"
      } else {
        attr(
          modelo,
          "tzone"
        )
      }
    ),
    stop(
      "Tipo canônico não suportado para `",
      variavel,
      "`: ",
      tipo
    )
  )

  if (
    tipo %in% c(
      "integer",
      "double"
    )
  ) {
    entrada_nao_vazia <- !is.na(x) &
      str_squish(
        as.character(x)
      ) != ""

    if (
      any(
        entrada_nao_vazia &
          is.na(resultado)
      )
    ) {
      stop(
        "Falha de conversão numérica na coluna `",
        variavel,
        "`."
      )
    }
  }

  resultado
}

ler_csv_por_modelo <- function(
    caminho,
    modelo
) {
  bruto <- read_csv(
    caminho,
    col_types = cols(
      .default = col_character()
    ),
    na = c(
      "",
      "NA"
    ),
    trim_ws = FALSE,
    name_repair = "minimal",
    show_col_types = FALSE,
    progress = FALSE
  )

  if (!identical(
    names(bruto),
    names(modelo)
  )) {
    stop(
      "O CSV `",
      caminho,
      "` não possui nomes e ordem de colunas idênticos ao RDS homologado."
    )
  }

  saida <- bruto

  for (variavel in names(modelo)) {
    saida[[variavel]] <- converter_por_modelo(
      bruto[[variavel]],
      modelo[[variavel]],
      variavel
    )
  }

  as_tibble(
    saida
  )
}

ordenar_por_chaves <- function(
    dados,
    chaves
) {
  dados |>
    arrange(
      across(
        all_of(chaves)
      )
    ) |>
    select(
      all_of(
        names(dados)
      )
    )
}

comparar_bases_semanticamente <- function(
    rds,
    csv,
    chaves,
    fonte
) {
  rds_ordenado <- ordenar_por_chaves(
    as_tibble(rds),
    chaves
  )

  csv_ordenado <- ordenar_por_chaves(
    as_tibble(csv),
    chaves
  )

  mesmos_nomes <- identical(
    names(rds_ordenado),
    names(csv_ordenado)
  )

  mesmos_tipos <- mesmos_nomes &&
    identical(
      map_chr(
        rds_ordenado,
        tipo_canonico
      ),
      map_chr(
        csv_ordenado,
        tipo_canonico
      )
    )

  mesmas_dimensoes <- identical(
    dim(rds_ordenado),
    dim(csv_ordenado)
  )

  comparacao_valores <- if (
    mesmos_nomes &&
      mesmos_tipos &&
      mesmas_dimensoes
  ) {
    all.equal(
      rds_ordenado,
      csv_ordenado,
      check.attributes = FALSE,
      tolerance = 1e-12
    )
  } else {
    "estrutura divergente"
  }

  mesmos_valores <- isTRUE(
    comparacao_valores
  )

  diagnostico <- tibble(
    fonte = fonte,
    linhas_rds = nrow(rds_ordenado),
    linhas_csv = nrow(csv_ordenado),
    colunas_rds = ncol(rds_ordenado),
    colunas_csv = ncol(csv_ordenado),
    mesmos_nomes_e_ordem = mesmos_nomes,
    mesmos_tipos_canonicos = mesmos_tipos,
    mesmas_dimensoes = mesmas_dimensoes,
    mesmos_valores_ordenados = mesmos_valores,
    detalhe_comparacao = if (
      mesmos_valores
    ) {
      "equivalentes"
    } else {
      paste(
        comparacao_valores,
        collapse = " | "
      )
    },
    aprovado = mesmos_nomes &&
      mesmos_tipos &&
      mesmas_dimensoes &&
      mesmos_valores
  )

  list(
    dados = rds_ordenado,
    diagnostico = diagnostico
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
      "reprovado"
    ),
    detalhe = detalhe
  )
}

inventariar_estrutura <- function(
    dados,
    fonte
) {
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
        tipo_canonico = tipo_canonico(x),
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

completude_variaveis <- function(dados) {
  map_dfr(
    names(dados),
    function(variavel) {
      x <- dados[[variavel]]

      tibble(
        variavel = variavel,
        classe_r = paste(
          class(x),
          collapse = " | "
        ),
        valores_ausentes = sum(
          is.na(x)
        ),
        valores_vazios_texto = if (
          is.character(x)
        ) {
          sum(
            !is.na(x) &
              str_squish(x) == ""
          )
        } else {
          0L
        },
        percentual_ausente =
          100 * sum(is.na(x)) / length(x),
        valores_distintos = n_distinct(
          x,
          na.rm = TRUE
        )
      )
    }
  )
}

soma_segura <- function(x) {
  if (
    length(x) == 0 ||
      all(is.na(x))
  ) {
    return(NA_real_)
  }

  sum(
    x,
    na.rm = TRUE
  )
}

mediana_segura <- function(x) {
  x <- x[
    !is.na(x)
  ]

  if (length(x) == 0) {
    return(NA_real_)
  }

  median(x)
}

desvio_padrao_seguro <- function(x) {
  x <- x[
    !is.na(x)
  ]

  if (length(x) < 2) {
    return(NA_real_)
  }

  sd(x)
}

amplitude_segura <- function(x) {
  x <- x[
    !is.na(x)
  ]

  if (length(x) == 0) {
    return(NA_real_)
  }

  max(x) - min(x)
}

media_ponderada_segura <- function(
    x,
    peso
) {
  valido <- !is.na(x) &
    !is.na(peso) &
    peso > 0

  if (!any(valido)) {
    return(NA_real_)
  }

  sum(
    x[valido] * peso[valido]
  ) / sum(
    peso[valido]
  )
}

taxa_por_totais <- function(
    numerador,
    denominador
) {
  numerador_total <- soma_segura(
    numerador
  )

  denominador_total <- soma_segura(
    denominador
  )

  if (
    is.na(numerador_total) ||
      is.na(denominador_total) ||
      denominador_total <= 0
  ) {
    return(NA_real_)
  }

  100 * numerador_total / denominador_total
}

variacao_relativa_segura <- function(
    valor_final,
    valor_inicial
) {
  if (
    is.na(valor_final) ||
      is.na(valor_inicial) ||
      valor_inicial == 0
  ) {
    return(NA_real_)
  }

  100 * (
    valor_final - valor_inicial
  ) / valor_inicial
}

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

classificar_participacao <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    x < 70 ~ "Abaixo de 70%",
    x < 80 ~ "70% a menos de 80%",
    x < 90 ~ "80% a menos de 90%",
    TRUE ~ "90% ou mais"
  )
}

classificar_variacao_diagnostica <- function(
    x,
    limiar,
    unidade
) {
  case_when(
    is.na(x) ~ "Não comparável",
    x >= limiar ~ paste0(
      "Aumento diagnóstico (≥ ",
      limiar,
      " ",
      unidade,
      ")"
    ),
    x <= -limiar ~ paste0(
      "Queda diagnóstica (≤ -",
      limiar,
      " ",
      unidade,
      ")"
    ),
    TRUE ~ paste0(
      "Oscilação dentro de ±",
      limiar,
      " ",
      unidade
    )
  )
}

vazio_texto <- function(x) {
  is.na(x) |
    str_squish(
      as.character(x)
    ) == ""
}

valor_nao_exposto_2025 <- function(x) {
  x_limpo <- str_to_upper(
    str_squish(
      as.character(x)
    )
  )

  is.na(x_limpo) |
    x_limpo %in% c(
      "",
      "0",
      "FALSE",
      "NAO",
      "NÃO",
      "NA_APLICAVEL",
      "SEM_CARGA_DO_PROGRAMA",
      "ZERO_OU_TENDENTE_A_ZERO",
      "LINHA_BASE_PRE_PROGRAMA"
    )
}

manifestar_arquivo <- function(
    produto,
    caminho
) {
  info <- file.info(
    caminho
  )

  tibble(
    produto = produto,
    caminho = normalizar_caminho(
      caminho
    ),
    tamanho_bytes = unname(
      info$size
    ),
    data_modificacao = format(
      info$mtime,
      "%Y-%m-%d %H:%M:%S"
    ),
    md5 = hash_md5(
      caminho
    )
  )
}

# -------------------------------------------------------------------
# 3. Bloqueios Git e identificação da execução
# -------------------------------------------------------------------

if (!file.exists(caminho_script)) {
  stop(
    "O módulo 16 deve ser executado pelo caminho canônico: ",
    caminho_script
  )
}

commit_git_execucao <- obter_commit_git()
branch_execucao <- obter_branch_git()

if (
  is.na(commit_git_execucao) ||
    commit_git_execucao != commit_base_integracao
) {
  stop(
    "HEAD incompatível com o marco homologado.\n",
    "Esperado: ",
    commit_base_integracao,
    "\nObservado: ",
    commit_git_execucao
  )
}

if (
  is.na(branch_execucao) ||
    branch_execucao != branch_esperada
) {
  stop(
    "Branch incompatível com a execução homologada.\n",
    "Esperada: ",
    branch_esperada,
    "\nObservada: ",
    branch_execucao
  )
}

walk(
  c(
    pasta_documentacao,
    pasta_execucao,
    dirname(
      caminho_script_historico
    ),
    pasta_historico_pre_11C,
    pasta_manifesto_historico,
    pasta_candidatos,
    pasta_rollback
  ),
  ~ dir.create(
    .x,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

manifesto_execucao <- tibble(
  id_execucao = id_execucao,
  instante_execucao = format(
    instante_execucao,
    "%Y-%m-%d %H:%M:%S %z"
  ),
  caminho_script = normalizar_caminho(
    caminho_script
  ),
  script_md5 = hash_md5(
    caminho_script
  ),
  commit_base_integracao = commit_base_integracao,
  commit_git_execucao = commit_git_execucao,
  branch_execucao = branch_execucao,
  caminho_manifesto_modulo_15 = normalizar_caminho(
    caminho_manifesto_modulo_15,
    deve_existir = FALSE
  )
)

write_csv(
  manifesto_execucao,
  file.path(
    pasta_execucao,
    "00_manifesto_execucao.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 4. Presença, hashes e manifesto das entradas homologadas
# -------------------------------------------------------------------

arquivos_exigidos <- c(
  arquivos_entrada,
  manifesto_modulo_15 = caminho_manifesto_modulo_15
)

arquivos_ausentes <- arquivos_exigidos[
  !file.exists(
    arquivos_exigidos
  )
]

if (length(arquivos_ausentes) > 0) {
  stop(
    "Entradas homologadas obrigatórias ausentes:\n",
    paste(
      arquivos_ausentes,
      collapse = "\n"
    ),
    "\nNão existe fallback entre CSV e RDS."
  )
}

hashes_entrada_observados <- map_chr(
  arquivos_entrada,
  hash_md5
)

validacao_hashes_fixos <- tibble(
  produto = names(
    arquivos_entrada
  ),
  caminho = as.character(
    arquivos_entrada
  ),
  md5_esperado = unname(
    hashes_entrada_homologados[
      names(
        arquivos_entrada
      )
    ]
  ),
  md5_observado = unname(
    hashes_entrada_observados
  ),
  aprovado = md5_esperado ==
    md5_observado
)

manifesto_modulo_15 <- read_csv(
  caminho_manifesto_modulo_15,
  col_types = cols(
    produto = col_character(),
    caminho = col_character(),
    tamanho_bytes = col_double(),
    data_modificacao = col_character(),
    md5 = col_character()
  ),
  show_col_types = FALSE,
  progress = FALSE
) |>
  select(
    produto,
    md5_manifesto = md5
  )

validacao_hashes_manifesto <- validacao_hashes_fixos |>
  left_join(
    manifesto_modulo_15,
    by = "produto"
  ) |>
  mutate(
    manifesto_possui_produto =
      !is.na(md5_manifesto),
    manifesto_confere_hash_fixo =
      md5_manifesto == md5_esperado,
    manifesto_confere_arquivo =
      md5_manifesto == md5_observado,
    aprovado_manifesto =
      manifesto_possui_produto &
      manifesto_confere_hash_fixo &
      manifesto_confere_arquivo
  )

write_csv(
  validacao_hashes_manifesto,
  file.path(
    pasta_execucao,
    "01_validacao_hashes_entradas.csv"
  ),
  na = ""
)

if (
  any(
    !validacao_hashes_manifesto$aprovado |
      !validacao_hashes_manifesto$aprovado_manifesto
  )
) {
  stop(
    "Os hashes das entradas não correspondem ao marco homologado do módulo 15. Consulte: ",
    file.path(
      pasta_execucao,
      "01_validacao_hashes_entradas.csv"
    )
  )
}

# O commit registrado internamente no manifesto do módulo 15 não é
# confrontado com o commit-base atual. A legitimidade daquele registro
# histórico é independente dos hashes homologados conferidos acima.

# -------------------------------------------------------------------
# 5. Leitura controlada e equivalência CSV–RDS
# -------------------------------------------------------------------

dim_rds <- readRDS(
  arquivos_entrada[["dim_final_rds"]]
) |>
  as_tibble()

base_rds <- readRDS(
  arquivos_entrada[["base_final_rds"]]
) |>
  as_tibble()

if (
  !is.data.frame(dim_rds) ||
    !is.data.frame(base_rds)
) {
  stop(
    "Os arquivos RDS homologados devem conter data frames."
  )
}

dim_csv <- ler_csv_por_modelo(
  arquivos_entrada[["dim_final_csv"]],
  dim_rds
)

base_csv <- ler_csv_por_modelo(
  arquivos_entrada[["base_final_csv"]],
  base_rds
)

comparacao_dim <- comparar_bases_semanticamente(
  dim_rds,
  dim_csv,
  "id_escola",
  "dim_escola_final"
)

comparacao_base <- comparar_bases_semanticamente(
  base_rds,
  base_csv,
  c(
    "id_escola",
    "ano_escolar",
    "componente"
  ),
  "base_analitica_final_escola_serie"
)

diagnostico_equivalencia_entradas <- bind_rows(
  comparacao_dim$diagnostico,
  comparacao_base$diagnostico
)

write_csv(
  diagnostico_equivalencia_entradas,
  file.path(
    pasta_execucao,
    "02_equivalencia_csv_rds_entradas.csv"
  ),
  na = ""
)

if (
  any(
    !diagnostico_equivalencia_entradas$aprovado
  )
) {
  stop(
    "CSV e RDS homologados não são semanticamente equivalentes. Consulte: ",
    file.path(
      pasta_execucao,
      "02_equivalencia_csv_rds_entradas.csv"
    )
  )
}

# O RDS só é adotado como objeto computacional depois de ambos os
# formatos terem sido exigidos, lidos, tipados, ordenados e comparados.
dim_escola_final <- comparacao_dim$dados
base_final_escola_serie <- comparacao_base$dados

# -------------------------------------------------------------------
# 6. Contrato de colunas, chaves e coerência escolar das entradas
# -------------------------------------------------------------------

colunas_obrigatorias_dim <- unique(
  c(
    "id_escola",
    "codigo_inep",
    "nome_canonico",
    "assessora",
    "grupo_administrativo_2024_final",
    "tipo_vinculo_rede_final",
    "status_rede_2025_final",
    "matriculas_anos_iniciais",
    "turmas_anos_iniciais",
    "docentes_anos_iniciais",
    "indice_infraestrutura_basica",
    "quartil_porte_anos_iniciais",
    "quartil_infraestrutura_escola",
    campos_temporais_minimos
  )
)

colunas_obrigatorias_base <- unique(
  c(
    "id_escola",
    "codigo_inep",
    "nome_canonico",
    "ano_escolar",
    "componente",
    "painel_resultado_balanceado",
    "amostra_principal_descritiva",
    "presente_2025",
    "presente_2026",
    "resultado_disponivel_2025",
    "resultado_disponivel_2026",
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
    "delta_participacao",
    "delta_proficiencia",
    "delta_pct_defasagem",
    "delta_pct_intermediario",
    "delta_pct_adequado",
    "variacao_relativa_previstos",
    "variacao_relativa_avaliados",
    "aumento_participacao_10pp",
    "queda_participacao_10pp",
    "mudanca_previstos_20pct",
    "observacao_composicao",
    "grupo_administrativo_2024_final",
    "tipo_vinculo_rede_final",
    "status_rede_2025_final",
    "quartil_porte_anos_iniciais",
    "quartil_infraestrutura_escola",
    campos_temporais_minimos
  )
)

validacao_colunas <- bind_rows(
  tibble(
    fonte = "dim_escola_final",
    coluna_obrigatoria =
      colunas_obrigatorias_dim,
    presente =
      colunas_obrigatorias_dim %in%
        names(dim_escola_final)
  ),
  tibble(
    fonte = "base_analitica_final_escola_serie",
    coluna_obrigatoria =
      colunas_obrigatorias_base,
    presente =
      colunas_obrigatorias_base %in%
        names(base_final_escola_serie)
  )
)

write_csv(
  validacao_colunas,
  file.path(
    pasta_execucao,
    "03_validacao_colunas_obrigatorias.csv"
  ),
  na = ""
)

if (any(!validacao_colunas$presente)) {
  stop(
    "Há colunas obrigatórias ausentes. Consulte: ",
    file.path(
      pasta_execucao,
      "03_validacao_colunas_obrigatorias.csv"
    )
  )
}

variaveis_proibidas <- c(
  "assessora_gerencial"
)

if (
  any(
    variaveis_proibidas %in%
      names(dim_escola_final)
  ) ||
    any(
      variaveis_proibidas %in%
        names(base_final_escola_serie)
    )
) {
  stop(
    "As entradas contêm `assessora_gerencial` sem período, variável proibida no contrato pós-11C."
  )
}

duplicidades_dim <- dim_escola_final |>
  count(
    id_escola,
    name = "numero_linhas"
  ) |>
  filter(
    numero_linhas > 1
  )

duplicidades_base <- base_final_escola_serie |>
  count(
    id_escola,
    ano_escolar,
    componente,
    name = "numero_linhas"
  ) |>
  filter(
    numero_linhas > 1
  )

series_por_escola <- base_final_escola_serie |>
  group_by(
    id_escola
  ) |>
  summarise(
    numero_series = n_distinct(
      ano_escolar
    ),
    series = paste(
      sort(
        unique(
          ano_escolar
        )
      ),
      collapse = " | "
    ),
    numero_linhas = n(),
    .groups = "drop"
  )

ids_apenas_dim <- setdiff(
  dim_escola_final$id_escola,
  base_final_escola_serie$id_escola
)

ids_apenas_base <- setdiff(
  base_final_escola_serie$id_escola,
  dim_escola_final$id_escola
)

campos_escolares_compartilhados <- intersect(
  names(dim_escola_final),
  names(base_final_escola_serie)
)

base_escolar_distinta <- base_final_escola_serie |>
  select(
    all_of(
      campos_escolares_compartilhados
    )
  ) |>
  distinct()

dim_compartilhada <- dim_escola_final |>
  select(
    all_of(
      campos_escolares_compartilhados
    )
  )

comparacao_campos_escolares <- comparar_bases_semanticamente(
  dim_compartilhada,
  base_escolar_distinta,
  "id_escola",
  "campos_escolares_dimensao_vs_base"
)

write_csv(
  bind_rows(
    inventariar_estrutura(
      dim_escola_final,
      "dim_escola_final"
    ),
    inventariar_estrutura(
      base_final_escola_serie,
      "base_analitica_final_escola_serie"
    )
  ),
  file.path(
    pasta_execucao,
    "04_estrutura_bases_entrada.csv"
  ),
  na = ""
)

write_csv(
  bind_rows(
    duplicidades_dim |>
      mutate(
        fonte = "dim_escola_final",
        .before = 1
      ),
    duplicidades_base |>
      unite(
        "chave",
        id_escola,
        ano_escolar,
        componente,
        sep = " | ",
        remove = FALSE
      ) |>
      mutate(
        fonte = "base_analitica_final_escola_serie",
        .before = 1
      )
  ),
  file.path(
    pasta_execucao,
    "05_duplicidades_chaves_entrada.csv"
  ),
  na = ""
)

write_csv(
  bind_rows(
    tibble(
      origem_exclusiva = "dim_escola_final",
      id_escola = ids_apenas_dim
    ),
    tibble(
      origem_exclusiva =
        "base_analitica_final_escola_serie",
      id_escola = ids_apenas_base
    )
  ),
  file.path(
    pasta_execucao,
    "06_ids_exclusivos_entre_bases.csv"
  ),
  na = ""
)

write_csv(
  series_por_escola,
  file.path(
    pasta_execucao,
    "07_series_por_escola.csv"
  ),
  na = ""
)

write_csv(
  comparacao_campos_escolares$diagnostico,
  file.path(
    pasta_execucao,
    "08_coerencia_campos_escolares_entradas.csv"
  ),
  na = ""
)

if (
  nrow(dim_escola_final) != 56 ||
    n_distinct(dim_escola_final$id_escola) != 56 ||
    nrow(base_final_escola_serie) != 280 ||
    n_distinct(base_final_escola_serie$id_escola) != 56 ||
    nrow(duplicidades_dim) > 0 ||
    nrow(duplicidades_base) > 0 ||
    length(ids_apenas_dim) > 0 ||
    length(ids_apenas_base) > 0 ||
    any(
      series_por_escola$numero_series != 5
    ) ||
    any(
      series_por_escola$series != "1 | 2 | 3 | 4 | 5"
    ) ||
    n_distinct(
      base_final_escola_serie$componente
    ) != 1 ||
    !comparacao_campos_escolares$diagnostico$aprovado
) {
  stop(
    "As entradas violam o contrato de chaves, escopo ou coerência escolar. Consulte os diagnósticos 05 a 08."
  )
}

# -------------------------------------------------------------------
# 7. Base compacta escola × série
# -------------------------------------------------------------------

colunas_compacto <- unique(
  c(
    "id_escola",
    "codigo_inep",
    "nome_canonico",
    "assessora_vinculo_administrativo",
    "assessora_gerencial_2025",
    "assessora_gerencial_2026",
    "ano_escolar",
    "componente",
    "grupo_administrativo_2024_final",
    "tipo_vinculo_rede_final",
    "status_rede_2025_final",
    "quartil_porte_anos_iniciais",
    "quartil_infraestrutura_escola",
    campos_temporais_minimos,
    "painel_resultado_balanceado",
    "amostra_principal_descritiva",
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
    "delta_participacao",
    "delta_proficiencia",
    "delta_pct_defasagem",
    "delta_pct_intermediario",
    "delta_pct_adequado",
    "variacao_relativa_previstos",
    "variacao_relativa_avaliados",
    "aumento_participacao_10pp",
    "queda_participacao_10pp",
    "mudanca_previstos_20pct",
    "observacao_composicao"
  )
)

perfil_escola_serie_compacto <-
  base_final_escola_serie |>
  select(
    all_of(
      colunas_compacto
    )
  ) |>
  mutate(
    faixa_participacao_2025_serie =
      classificar_participacao(
        taxa_participacao_2025
      ),
    faixa_participacao_2026_serie =
      classificar_participacao(
        taxa_participacao_2026
      ),
    direcao_delta_participacao =
      classificar_variacao_diagnostica(
        delta_participacao,
        5,
        "p.p."
      ),
    direcao_delta_proficiencia =
      classificar_variacao_diagnostica(
        delta_proficiencia,
        2,
        "pontos"
      ),
    alerta_composicao_serie =
      coalesce(
        aumento_participacao_10pp,
        FALSE
      ) |
      coalesce(
        queda_participacao_10pp,
        FALSE
      ) |
      coalesce(
        mudanca_previstos_20pct,
        FALSE
      )
  ) |>
  arrange(
    id_escola,
    ano_escolar,
    componente
  )

# -------------------------------------------------------------------
# 8. Sínteses operacionais descritivas no nível da escola
# -------------------------------------------------------------------

resumo_avaliacoes_escola <-
  base_final_escola_serie |>
  group_by(
    id_escola
  ) |>
  summarise(
    numero_series_esperadas =
      n_distinct(ano_escolar),
    numero_componentes =
      n_distinct(componente),
    numero_series_presentes_2025 = sum(
      coalesce(
        presente_2025,
        FALSE
      )
    ),
    numero_series_presentes_2026 = sum(
      coalesce(
        presente_2026,
        FALSE
      )
    ),
    numero_series_resultado_2025 = sum(
      coalesce(
        resultado_disponivel_2025,
        FALSE
      )
    ),
    numero_series_resultado_2026 = sum(
      coalesce(
        resultado_disponivel_2026,
        FALSE
      )
    ),
    numero_series_comparaveis = sum(
      coalesce(
        painel_resultado_balanceado,
        FALSE
      )
    ),
    painel_completo_cinco_series =
      numero_series_comparaveis == 5,

    previstos_total_2025 =
      soma_segura(previstos_2025),
    avaliados_total_2025 =
      soma_segura(avaliados_2025),
    taxa_participacao_escola_2025 =
      taxa_por_totais(
        avaliados_2025,
        previstos_2025
      ),
    proficiencia_multisserie_ponderada_2025 =
      media_ponderada_segura(
        proficiencia_media_2025,
        avaliados_2025
      ),
    pct_defasagem_multisserie_ponderado_2025 =
      media_ponderada_segura(
        pct_defasagem_2025,
        avaliados_2025
      ),
    pct_intermediario_multisserie_ponderado_2025 =
      media_ponderada_segura(
        pct_intermediario_2025,
        avaliados_2025
      ),
    pct_adequado_multisserie_ponderado_2025 =
      media_ponderada_segura(
        pct_adequado_2025,
        avaliados_2025
      ),

    previstos_total_2026 =
      soma_segura(previstos_2026),
    avaliados_total_2026 =
      soma_segura(avaliados_2026),
    taxa_participacao_escola_2026 =
      taxa_por_totais(
        avaliados_2026,
        previstos_2026
      ),
    proficiencia_multisserie_ponderada_2026 =
      media_ponderada_segura(
        proficiencia_media_2026,
        avaliados_2026
      ),
    pct_defasagem_multisserie_ponderado_2026 =
      media_ponderada_segura(
        pct_defasagem_2026,
        avaliados_2026
      ),
    pct_intermediario_multisserie_ponderado_2026 =
      media_ponderada_segura(
        pct_intermediario_2026,
        avaliados_2026
      ),
    pct_adequado_multisserie_ponderado_2026 =
      media_ponderada_segura(
        pct_adequado_2026,
        avaliados_2026
      ),

    delta_participacao_escola =
      taxa_participacao_escola_2026 -
      taxa_participacao_escola_2025,
    delta_proficiencia_multisserie_ponderada =
      proficiencia_multisserie_ponderada_2026 -
      proficiencia_multisserie_ponderada_2025,
    delta_pct_defasagem_multisserie_ponderado =
      pct_defasagem_multisserie_ponderado_2026 -
      pct_defasagem_multisserie_ponderado_2025,
    delta_pct_intermediario_multisserie_ponderado =
      pct_intermediario_multisserie_ponderado_2026 -
      pct_intermediario_multisserie_ponderado_2025,
    delta_pct_adequado_multisserie_ponderado =
      pct_adequado_multisserie_ponderado_2026 -
      pct_adequado_multisserie_ponderado_2025,
    variacao_relativa_previstos_escola =
      variacao_relativa_segura(
        previstos_total_2026,
        previstos_total_2025
      ),
    variacao_relativa_avaliados_escola =
      variacao_relativa_segura(
        avaliados_total_2026,
        avaliados_total_2025
      ),

    mediana_participacao_series_2025 =
      mediana_segura(
        taxa_participacao_2025
      ),
    mediana_participacao_series_2026 =
      mediana_segura(
        taxa_participacao_2026
      ),
    amplitude_participacao_series_2025 =
      amplitude_segura(
        taxa_participacao_2025
      ),
    amplitude_participacao_series_2026 =
      amplitude_segura(
        taxa_participacao_2026
      ),
    desvio_participacao_series_2025 =
      desvio_padrao_seguro(
        taxa_participacao_2025
      ),
    desvio_participacao_series_2026 =
      desvio_padrao_seguro(
        taxa_participacao_2026
      ),

    mediana_proficiencia_series_2025 =
      mediana_segura(
        proficiencia_media_2025
      ),
    mediana_proficiencia_series_2026 =
      mediana_segura(
        proficiencia_media_2026
      ),
    amplitude_proficiencia_series_2025 =
      amplitude_segura(
        proficiencia_media_2025
      ),
    amplitude_proficiencia_series_2026 =
      amplitude_segura(
        proficiencia_media_2026
      ),
    desvio_proficiencia_series_2025 =
      desvio_padrao_seguro(
        proficiencia_media_2025
      ),
    desvio_proficiencia_series_2026 =
      desvio_padrao_seguro(
        proficiencia_media_2026
      ),

    numero_series_aumento_diagnostico_proficiencia =
      sum(
        !is.na(delta_proficiencia) &
          delta_proficiencia >= 2
      ),
    numero_series_queda_diagnostica_proficiencia =
      sum(
        !is.na(delta_proficiencia) &
          delta_proficiencia <= -2
      ),
    numero_series_oscilacao_proficiencia =
      sum(
        !is.na(delta_proficiencia) &
          abs(delta_proficiencia) < 2
      ),
    numero_series_aumento_diagnostico_participacao =
      sum(
        !is.na(delta_participacao) &
          delta_participacao >= 5
      ),
    numero_series_queda_diagnostica_participacao =
      sum(
        !is.na(delta_participacao) &
          delta_participacao <= -5
      ),
    numero_series_oscilacao_participacao =
      sum(
        !is.na(delta_participacao) &
          abs(delta_participacao) < 5
      ),

    numero_series_participacao_abaixo_70_2025 =
      sum(
        !is.na(taxa_participacao_2025) &
          taxa_participacao_2025 < 70
      ),
    numero_series_participacao_abaixo_70_2026 =
      sum(
        !is.na(taxa_participacao_2026) &
          taxa_participacao_2026 < 70
      ),
    numero_series_participacao_abaixo_80_2025 =
      sum(
        !is.na(taxa_participacao_2025) &
          taxa_participacao_2025 < 80
      ),
    numero_series_participacao_abaixo_80_2026 =
      sum(
        !is.na(taxa_participacao_2026) &
          taxa_participacao_2026 < 80
      ),
    numero_series_participacao_abaixo_90_2025 =
      sum(
        !is.na(taxa_participacao_2025) &
          taxa_participacao_2025 < 90
      ),
    numero_series_participacao_abaixo_90_2026 =
      sum(
        !is.na(taxa_participacao_2026) &
          taxa_participacao_2026 < 90
      ),

    numero_series_aumento_participacao_10pp =
      sum(
        coalesce(
          aumento_participacao_10pp,
          FALSE
        )
      ),
    numero_series_queda_participacao_10pp =
      sum(
        coalesce(
          queda_participacao_10pp,
          FALSE
        )
      ),
    numero_series_mudanca_previstos_20pct =
      sum(
        coalesce(
          mudanca_previstos_20pct,
          FALSE
        )
      ),
    numero_series_alerta_composicao =
      sum(
        coalesce(
          aumento_participacao_10pp,
          FALSE
        ) |
          coalesce(
            queda_participacao_10pp,
            FALSE
          ) |
          coalesce(
            mudanca_previstos_20pct,
            FALSE
          )
      ),
    possui_alerta_composicao =
      numero_series_alerta_composicao > 0,
    observacoes_composicao =
      valores_distintos_texto(
        observacao_composicao
      ),
    .groups = "drop"
  ) |>
  mutate(
    faixa_participacao_escola_2025 =
      classificar_participacao(
        taxa_participacao_escola_2025
      ),
    faixa_participacao_escola_2026 =
      classificar_participacao(
        taxa_participacao_escola_2026
      ),
    direcao_delta_participacao_escola =
      classificar_variacao_diagnostica(
        delta_participacao_escola,
        5,
        "p.p."
      ),
    direcao_delta_proficiencia_multisserie =
      classificar_variacao_diagnostica(
        delta_proficiencia_multisserie_ponderada,
        2,
        "pontos"
      ),
    proporcao_series_comparaveis =
      100 * numero_series_comparaveis /
      numero_series_esperadas,
    proporcao_series_aumento_diagnostico_proficiencia =
      case_when(
        numero_series_comparaveis > 0 ~
          100 *
          numero_series_aumento_diagnostico_proficiencia /
          numero_series_comparaveis,
        TRUE ~ NA_real_
      )
  )

# -------------------------------------------------------------------
# 9. Perfil completo e perfil gerencial
# -------------------------------------------------------------------

perfil_escola_completo <- dim_escola_final |>
  left_join(
    resumo_avaliacoes_escola,
    by = "id_escola"
  ) |>
  arrange(
    id_escola
  )

colunas_contexto_gerencial <- c(
  "id_escola",
  "codigo_inep",
  "nome_canonico",
  "nome_inep",
  "assessora_vinculo_administrativo",
  "grupo_administrativo_2024_final",
  "tipo_vinculo_rede_final",
  "status_rede_2025_final",
  "dependencia_administrativa_2024",
  "situacao_funcionamento_2024",
  "municipalizada_apos_2024",
  "possivel_municipalizacao_recente",
  "escola_nova_recente",
  "privada_vinculada_final",
  "caso_historico_administrativo",
  "observacao_administrativa_final",
  "requer_revisao_tecnica",
  "divergencia_censo_cadastro",
  "contexto_2024_encontrado",
  "estratos_corrigidos_disponiveis",
  "incluir_universo_municipal_direto_2024",
  "incluir_universo_municipal_ampliado_2024",
  "incluir_rede_municipal_operacional_2025",
  "quantidade_matricula_educacao_basica",
  "matriculas_anos_iniciais",
  "turmas_anos_iniciais",
  "docentes_anos_iniciais",
  "alunos_por_turma_anos_iniciais",
  "alunos_por_docente_anos_iniciais",
  "porte_anos_iniciais",
  "quartil_porte_anos_iniciais",
  "faixa_porte_contextual_escola",
  "numero_etapas_amplas_ofertadas",
  "oferta_educacao_infantil",
  "oferta_anos_finais",
  "oferta_eja",
  "oferta_educacao_especial",
  "pct_matriculas_anos_iniciais_integral",
  "pct_matriculas_educacao_especial",
  "pct_matriculas_transporte_publico",
  "pct_matriculas_preta_parda_indigena",
  "indice_infraestrutura_basica",
  "quartil_infraestrutura_escola",
  "numero_itens_infraestrutura_presentes",
  "quantidade_sala_utilizada",
  "quantidade_sala_utilizada_climatizada",
  "quantidade_sala_utilizada_acessivel",
  "possui_espaco_leitura",
  "possui_recurso_acessibilidade",
  "laboratorio_ciencias",
  "laboratorio_informatica",
  "quadra_esportes",
  "internet_aprendizagem",
  "internet_alunos",
  "sala_atendimento_especial",
  "quantidade_computador_portatil_aluno",
  "quantidade_tablet_aluno",
  "quantidade_profissional_coordenador",
  "quantidade_profissional_psicologo",
  "quantidade_profissional_assistente_social",
  "quantidade_profissional_monitor",
  "quantidade_profissional_pedagogia"
)

colunas_resumo_gerencial <- setdiff(
  names(resumo_avaliacoes_escola),
  "id_escola"
)

colunas_perfil_gerencial <- unique(
  c(
    colunas_contexto_gerencial,
    campos_temporais_minimos,
    colunas_resumo_gerencial
  )
)

colunas_gerenciais_ausentes <- setdiff(
  colunas_perfil_gerencial,
  names(perfil_escola_completo)
)

if (
  length(
    colunas_gerenciais_ausentes
  ) > 0
) {
  stop(
    "Colunas previstas para o perfil gerencial estão ausentes:\n",
    paste(
      colunas_gerenciais_ausentes,
      collapse = "\n"
    )
  )
}

perfil_escola_gerencial <-
  perfil_escola_completo |>
  select(
    all_of(
      colunas_perfil_gerencial
    )
  ) |>
  arrange(
    assessora_gerencial_2026,
    nome_canonico,
    id_escola
  )

if (
  "assessora" %in%
    names(perfil_escola_gerencial) ||
    "assessora" %in%
      names(perfil_escola_serie_compacto) ||
    "assessora_gerencial" %in%
      names(perfil_escola_completo) ||
    "assessora_gerencial" %in%
      names(perfil_escola_gerencial) ||
    "assessora_gerencial" %in%
      names(perfil_escola_serie_compacto)
) {
  stop(
    "Contrato de assessoria violado nos produtos construídos."
  )
}

# -------------------------------------------------------------------
# 10. Dicionário conjunto dos três produtos
# -------------------------------------------------------------------

descricoes_derivadas <- c(
  numero_series_esperadas = "Número de anos escolares representados para a escola.",
  numero_series_presentes_2025 = "Número de séries com presença na avaliação de 2025.",
  numero_series_presentes_2026 = "Número de séries com presença na avaliação de 2026.",
  numero_series_resultado_2025 = "Número de séries com resultado disponível em 2025.",
  numero_series_resultado_2026 = "Número de séries com resultado disponível em 2026.",
  numero_series_comparaveis = "Número de séries com resultados comparáveis em 2025 e 2026.",
  painel_completo_cinco_series = "Indica presença de resultados comparáveis do 1º ao 5º ano.",
  taxa_participacao_escola_2025 = "Participação agregada em 2025: 100 vezes avaliados totais dividido por previstos totais.",
  taxa_participacao_escola_2026 = "Participação agregada em 2026: 100 vezes avaliados totais dividido por previstos totais.",
  proficiencia_multisserie_ponderada_2025 = "Síntese operacional descritiva multissérie de 2025, ponderada por avaliados.",
  proficiencia_multisserie_ponderada_2026 = "Síntese operacional descritiva multissérie de 2026, ponderada por avaliados.",
  delta_participacao_escola = "Diferença descritiva da participação agregada de 2026 menos 2025, em p.p.",
  delta_proficiencia_multisserie_ponderada = "Diferença descritiva entre as sínteses multissérie de 2026 e 2025.",
  direcao_delta_participacao_escola = "Classificação heurística da variação de participação com limiar de ±5 p.p.",
  direcao_delta_proficiencia_multisserie = "Classificação heurística da variação de proficiência com limiar de ±2 pontos.",
  alerta_composicao_serie = "Alerta heurístico de participação ±10 p.p. ou mudança de previstos igual ou superior a 20%.",
  possui_alerta_composicao = "Indica pelo menos uma série com alerta heurístico de participação ou composição."
)

variaveis_dicionario <- unique(
  c(
    names(perfil_escola_completo),
    names(perfil_escola_gerencial),
    names(perfil_escola_serie_compacto)
  )
)

obter_coluna_referencia <- function(variavel) {
  if (
    variavel %in%
      names(perfil_escola_completo)
  ) {
    return(
      perfil_escola_completo[[variavel]]
    )
  }

  if (
    variavel %in%
      names(perfil_escola_gerencial)
  ) {
    return(
      perfil_escola_gerencial[[variavel]]
    )
  }

  perfil_escola_serie_compacto[[variavel]]
}

perfil_dicionario <- map_dfr(
  seq_along(
    variaveis_dicionario
  ),
  function(i) {
    variavel <- variaveis_dicionario[[i]]
    x <- obter_coluna_referencia(
      variavel
    )

    herdada_dim <-
      variavel %in%
      names(dim_escola_final)

    herdada_base <-
      variavel %in%
      names(base_final_escola_serie)

    origem <- case_when(
      variavel %in% c(
        "quartil_porte_anos_iniciais",
        "quartil_infraestrutura_escola"
      ) ~ "módulo 15 / contexto escolar 2024; variável herdada, não recalculada no módulo 16",
      herdada_dim ~ "dim_escola_final homologada no módulo 15",
      herdada_base ~ "base_analitica_final_escola_serie homologada no módulo 15",
      TRUE ~ "derivada no módulo 16"
    )

    descricao_manual <- unname(
      descricoes_derivadas[variavel]
    )

    descricao <- case_when(
      variavel == "assessora" ~ paste(
        "Campo legado preservado exclusivamente no perfil completo",
        "para rastreabilidade; proibido para exposição, carga ou",
        "atribuição gerencial."
      ),
      length(descricao_manual) == 1 &&
        !is.na(descricao_manual) ~
        descricao_manual,
      herdada_dim |
        herdada_base ~ paste0(
          "Variável herdada do módulo 15: ",
          str_replace_all(
            variavel,
            "_",
            " "
          ),
          "."
        ),
      TRUE ~ paste0(
        "Indicador derivado no módulo 16: ",
        str_replace_all(
          variavel,
          "_",
          " "
        ),
        "."
      )
    )

    observacao_metodologica <- case_when(
      variavel == "assessora" ~
        "Campo legado; não utilizar em qualquer produto analítico ou gerencial.",
      variavel ==
        "assessora_vinculo_administrativo" ~
        "Vínculo administrativo; não comprova exposição, carga, dose ou efeito.",
      variavel ==
        "assessora_gerencial_2025" ~
        "Obrigatoriamente vazia; 2025 é linha de base pré-programa.",
      variavel ==
        "assessora_gerencial_2026" ~
        "Responsabilidade gerencial homologada; não representa autoria, qualidade ou efeito.",
      str_detect(
        variavel,
        "proficiencia_multisserie|pct_defasagem_multisserie|pct_intermediario_multisserie|pct_adequado_multisserie"
      ) ~ paste(
        "Síntese operacional descritiva; não é índice, escore,",
        "ranking ou medida atribuível à assessora. Interpretar com",
        "participação, composição e resultados por série."
      ),
      str_detect(
        variavel,
        "direcao_delta.*participacao|aumento_diagnostico_participacao|queda_diagnostica_participacao|oscilacao_participacao"
      ) ~
        "Heurística diagnóstica de ±5 p.p.; não é significância estatística, meta oficial ou evidência de efeito.",
      str_detect(
        variavel,
        "direcao_delta.*proficiencia|aumento_diagnostico_proficiencia|queda_diagnostica_proficiencia|oscilacao_proficiencia"
      ) ~
        "Heurística diagnóstica de ±2 pontos; não é significância estatística, meta oficial ou evidência de efeito.",
      str_detect(
        variavel,
        "aumento_participacao_10pp|queda_participacao_10pp|mudanca_previstos_20pct|alerta_composicao"
      ) ~
        "Alerta heurístico de qualidade de comparação; não é significância estatística, meta ou efeito.",
      variavel %in% c(
        "quartil_porte_anos_iniciais",
        "quartil_infraestrutura_escola"
      ) ~
        "Variável contextual herdada do módulo 15/contexto 2024; não recalculada no módulo 16.",
      TRUE ~ NA_character_
    )

    tibble(
      ordem_dicionario = i,
      variavel = variavel,
      presente_perfil_completo =
        variavel %in%
        names(perfil_escola_completo),
      presente_perfil_gerencial =
        variavel %in%
        names(perfil_escola_gerencial),
      presente_perfil_serie_compacto =
        variavel %in%
        names(perfil_escola_serie_compacto),
      origem = origem,
      classe_r = paste(
        class(x),
        collapse = " | "
      ),
      tipo_canonico = tipo_canonico(x),
      nivel_analise = if_else(
        variavel %in%
          names(perfil_escola_serie_compacto) &&
          !variavel %in%
          names(perfil_escola_completo),
        "escola × série × componente",
        "escola"
      ),
      descricao = descricao,
      observacao_metodologica =
        observacao_metodologica
    )
  }
)

# -------------------------------------------------------------------
# 11. Validações substantivas e estruturais dos produtos
# -------------------------------------------------------------------

campos_escolares_compacto <- intersect(
  names(dim_escola_final),
  names(perfil_escola_serie_compacto)
)

compacto_escolar_distinto <-
  perfil_escola_serie_compacto |>
  select(
    all_of(
      campos_escolares_compacto
    )
  ) |>
  distinct()

dim_compacto <- dim_escola_final |>
  select(
    all_of(
      campos_escolares_compacto
    )
  )

comparacao_compacto_dim <-
  comparar_bases_semanticamente(
    dim_compacto,
    compacto_escolar_distinto,
    "id_escola",
    "campos_escolares_compacto_vs_dimensao"
  )

chaves_base <- base_final_escola_serie |>
  select(
    id_escola,
    ano_escolar,
    componente
  ) |>
  arrange(
    id_escola,
    ano_escolar,
    componente
  )

chaves_compacto <-
  perfil_escola_serie_compacto |>
  select(
    id_escola,
    ano_escolar,
    componente
  ) |>
  arrange(
    id_escola,
    ano_escolar,
    componente
  )

chaves_perdidas <- anti_join(
  chaves_base,
  chaves_compacto,
  by = c(
    "id_escola",
    "ano_escolar",
    "componente"
  )
)

chaves_acrescentadas <- anti_join(
  chaves_compacto,
  chaves_base,
  by = c(
    "id_escola",
    "ano_escolar",
    "componente"
  )
)

universo_carga_dim <- dim_escola_final |>
  filter(
    coalesce(
      incluir_indice_carga_2026,
      FALSE
    )
  )

universo_carga_compacto <-
  perfil_escola_serie_compacto |>
  filter(
    coalesce(
      incluir_indice_carga_2026,
      FALSE
    )
  )

exclusoes_observadas <- dim_escola_final |>
  filter(
    !coalesce(
      incluir_indice_carga_2026,
      FALSE
    )
  ) |>
  select(
    id_escola,
    codigo_inep
  ) |>
  arrange(
    id_escola
  )

exclusoes_esperadas <-
  pares_excluidos_carga |>
  arrange(
    id_escola
  )

exclusoes_corretas <- isTRUE(
  all.equal(
    exclusoes_observadas,
    exclusoes_esperadas,
    check.attributes = FALSE
  )
)

linhas_excluidas_compacto <-
  perfil_escola_serie_compacto |>
  semi_join(
    pares_excluidos_carga,
    by = c(
      "id_escola",
      "codigo_inep"
    )
  )

exclusoes_no_perfil_completo <-
  perfil_escola_completo |>
  semi_join(
    pares_excluidos_carga,
    by = c(
      "id_escola",
      "codigo_inep"
    )
  )

exclusoes_no_perfil_gerencial <-
  perfil_escola_gerencial |>
  semi_join(
    pares_excluidos_carga,
    by = c(
      "id_escola",
      "codigo_inep"
    )
  )

assessoria_2026_coerente_carga <-
  all(
    !vazio_texto(
      universo_carga_dim$
        assessora_gerencial_2026
    )
  ) &&
  all(
    vazio_texto(
      exclusoes_observadas |>
        left_join(
          dim_escola_final |>
            select(
              id_escola,
              codigo_inep,
              assessora_gerencial_2026
            ),
          by = c(
            "id_escola",
            "codigo_inep"
          )
        ) |>
        pull(
          assessora_gerencial_2026
        )
    )
  )

zero_2025_dim <- isTRUE(
  all(
    dim_escola_final$
      exposicao_programa_binaria_2025 == 0
  )
) &&
  all(
    !coalesce(
      dim_escola_final$incluir_indice_carga_2025,
      FALSE
    )
  ) &&
  all(
    vazio_texto(
      dim_escola_final$assessora_gerencial_2025
    )
  ) &&
  all(
    valor_nao_exposto_2025(
      dim_escola_final$recebe_assessoramento_2025
    )
  ) &&
  all(
    valor_nao_exposto_2025(
      dim_escola_final$status_carga_operacional_2025
    )
  )

zero_2025_gerencial <- isTRUE(
  all(
    perfil_escola_gerencial$
      exposicao_programa_binaria_2025 == 0
  )
) &&
  all(
    !coalesce(
      perfil_escola_gerencial$
        incluir_indice_carga_2025,
      FALSE
    )
  ) &&
  all(
    vazio_texto(
      perfil_escola_gerencial$
        assessora_gerencial_2025
    )
  )

zero_2025_compacto <- isTRUE(
  all(
    perfil_escola_serie_compacto$
      exposicao_programa_binaria_2025 == 0
  )
) &&
  all(
    !coalesce(
      perfil_escola_serie_compacto$
        incluir_indice_carga_2025,
      FALSE
    )
  ) &&
  all(
    vazio_texto(
      perfil_escola_serie_compacto$
        assessora_gerencial_2025
    )
  )

avaliativo_2025 <- sum(
  !vazio_texto(
    dim_escola_final$
      pertence_universo_avaliativo_2025
  ) &
    str_to_upper(
      dim_escola_final$
        pertence_universo_avaliativo_2025
    ) == "SIM"
)

avaliativo_2026 <- sum(
  !vazio_texto(
    dim_escola_final$
      pertence_universo_avaliativo_2026
  ) &
    str_to_upper(
      dim_escola_final$
        pertence_universo_avaliativo_2026
    ) == "SIM"
)

assessoras_carga_2026 <-
  universo_carga_dim |>
  filter(
    !vazio_texto(
      assessora_gerencial_2026
    )
  ) |>
  summarise(
    numero = n_distinct(
      assessora_gerencial_2026
    )
  ) |>
  pull(numero)

contrato_temporal_produtos <- all(
  campos_temporais_minimos %in%
    names(perfil_escola_completo)
) &&
  all(
    campos_temporais_minimos %in%
      names(perfil_escola_gerencial)
  ) &&
  all(
    campos_temporais_minimos %in%
      names(perfil_escola_serie_compacto)
  )

registro_legado_dicionario <-
  perfil_dicionario |>
  filter(
    variavel == "assessora"
  )

dicionario_legado_correto <-
  nrow(registro_legado_dicionario) == 1 &&
  registro_legado_dicionario$
    presente_perfil_completo &&
  !registro_legado_dicionario$
    presente_perfil_gerencial &&
  !registro_legado_dicionario$
    presente_perfil_serie_compacto &&
  str_detect(
    str_to_lower(
      registro_legado_dicionario$
        observacao_metodologica
    ),
    "legado"
  )

validacao_final <- bind_rows(
  registrar_validacao(
    "Perfil completo: linhas",
    "estrutura",
    "erro",
    nrow(perfil_escola_completo),
    "56",
    nrow(perfil_escola_completo) == 56,
    "A dimensão final é a única espinha do perfil."
  ),
  registrar_validacao(
    "Perfil completo: IDs únicos",
    "estrutura",
    "erro",
    n_distinct(
      perfil_escola_completo$id_escola
    ),
    "56",
    n_distinct(
      perfil_escola_completo$id_escola
    ) == 56,
    "Uma linha por escola."
  ),
  registrar_validacao(
    "Perfil gerencial: linhas",
    "estrutura",
    "erro",
    nrow(perfil_escola_gerencial),
    "56",
    nrow(perfil_escola_gerencial) == 56,
    "Uma linha por escola."
  ),
  registrar_validacao(
    "Perfil gerencial: IDs únicos",
    "estrutura",
    "erro",
    n_distinct(
      perfil_escola_gerencial$id_escola
    ),
    "56",
    n_distinct(
      perfil_escola_gerencial$id_escola
    ) == 56,
    "Uma linha por escola."
  ),
  registrar_validacao(
    "Perfil compacto: linhas",
    "estrutura",
    "erro",
    nrow(perfil_escola_serie_compacto),
    "280",
    nrow(perfil_escola_serie_compacto) == 280,
    "56 escolas × cinco séries."
  ),
  registrar_validacao(
    "Perfil compacto: IDs únicos",
    "estrutura",
    "erro",
    n_distinct(
      perfil_escola_serie_compacto$id_escola
    ),
    "56",
    n_distinct(
      perfil_escola_serie_compacto$id_escola
    ) == 56,
    "Todas as escolas devem permanecer."
  ),
  registrar_validacao(
    "Perfil compacto: chave única",
    "estrutura",
    "erro",
    nrow(
      perfil_escola_serie_compacto |>
        count(
          id_escola,
          ano_escolar,
          componente
        ) |>
        filter(
          n > 1
        )
    ),
    "zero duplicidades",
    nrow(
      perfil_escola_serie_compacto |>
        count(
          id_escola,
          ano_escolar,
          componente
        ) |>
        filter(
          n > 1
        )
    ) == 0,
    "Chave id_escola + ano_escolar + componente."
  ),
  registrar_validacao(
    "Contrato temporal mínimo nos três produtos",
    "temporal",
    "erro",
    contrato_temporal_produtos,
    "TRUE",
    contrato_temporal_produtos,
    "Todos os campos mínimos de 2025/2026 devem ser preservados."
  ),
  registrar_validacao(
    "Chaves perdidas",
    "integração",
    "erro",
    nrow(chaves_perdidas),
    "zero",
    nrow(chaves_perdidas) == 0,
    "Confronto com a base homologada do módulo 15."
  ),
  registrar_validacao(
    "Chaves acrescentadas",
    "integração",
    "erro",
    nrow(chaves_acrescentadas),
    "zero",
    nrow(chaves_acrescentadas) == 0,
    "Confronto com a base homologada do módulo 15."
  ),
  registrar_validacao(
    "Campos escolares constantes e iguais à dimensão",
    "integração",
    "erro",
    comparacao_compacto_dim$diagnostico$
      detalhe_comparacao,
    "equivalentes",
    comparacao_compacto_dim$diagnostico$
      aprovado,
    "Cada campo escolar selecionado no compacto deve repetir a dimensão."
  ),
  registrar_validacao(
    "Universo avaliativo 2025",
    "universo",
    "erro",
    avaliativo_2025,
    "54 escolas",
    avaliativo_2025 == 54,
    "Universo homologado."
  ),
  registrar_validacao(
    "Universo avaliativo 2026",
    "universo",
    "erro",
    avaliativo_2026,
    "56 escolas",
    avaliativo_2026 == 56,
    "Universo homologado."
  ),
  registrar_validacao(
    "Universo de carga 2026: IDs",
    "carga",
    "erro",
    n_distinct(
      universo_carga_dim$id_escola
    ),
    "53",
    n_distinct(
      universo_carga_dim$id_escola
    ) == 53,
    "Carga potencial relativa; não é qualidade."
  ),
  registrar_validacao(
    "Universo de carga 2026: linhas compactas",
    "carga",
    "erro",
    nrow(universo_carga_compacto),
    "265",
    nrow(universo_carga_compacto) == 265,
    "53 escolas × cinco séries."
  ),
  registrar_validacao(
    "Assessoras gerenciais no universo de carga 2026",
    "carga",
    "erro",
    assessoras_carga_2026,
    "11",
    assessoras_carga_2026 == 11,
    "Contagem apenas entre escolas incluídas na carga."
  ),
  registrar_validacao(
    "Exclusões simultâneas por ID e código INEP",
    "carga",
    "erro",
    paste(
      exclusoes_observadas$id_escola,
      exclusoes_observadas$codigo_inep,
      sep = "=",
      collapse = " | "
    ),
    paste(
      exclusoes_esperadas$id_escola,
      exclusoes_esperadas$codigo_inep,
      sep = "=",
      collapse = " | "
    ),
    exclusoes_corretas,
    "Somente as três escolas homologadas ficam fora da carga."
  ),
  registrar_validacao(
    "Exclusões preservadas no perfil compacto",
    "carga",
    "erro",
    nrow(linhas_excluidas_compacto),
    "15 linhas",
    nrow(linhas_excluidas_compacto) == 15 &&
      n_distinct(
        linhas_excluidas_compacto$id_escola
      ) == 3 &&
      all(
        !coalesce(
          linhas_excluidas_compacto$
            incluir_indice_carga_2026,
          FALSE
        )
      ),
    "As três escolas permanecem, mas nunca integram a carga."
  ),
  registrar_validacao(
    "Exclusões preservadas nos perfis escolares",
    "carga",
    "erro",
    paste0(
      "completo=",
      nrow(exclusoes_no_perfil_completo),
      "; gerencial=",
      nrow(exclusoes_no_perfil_gerencial)
    ),
    "três em cada perfil",
    nrow(exclusoes_no_perfil_completo) == 3 &&
      nrow(exclusoes_no_perfil_gerencial) == 3,
    "As três escolas permanecem nos perfis completo e gerencial."
  ),
  registrar_validacao(
    "Assessora gerencial 2026 coerente com carga",
    "assessoria",
    "erro",
    assessoria_2026_coerente_carga,
    "TRUE",
    assessoria_2026_coerente_carga,
    "Preenchida no universo de carga e vazia nas três exclusões."
  ),
  registrar_validacao(
    "Exposição, carga e assessora gerencial em 2025: dimensão",
    "temporal",
    "erro",
    zero_2025_dim,
    "TRUE",
    zero_2025_dim,
    "2025 é linha de base pré-programa."
  ),
  registrar_validacao(
    "Exposição, carga e assessora gerencial em 2025: perfil gerencial",
    "temporal",
    "erro",
    zero_2025_gerencial,
    "TRUE",
    zero_2025_gerencial,
    "2025 é linha de base pré-programa."
  ),
  registrar_validacao(
    "Exposição, carga e assessora gerencial em 2025: compacto",
    "temporal",
    "erro",
    zero_2025_compacto,
    "TRUE",
    zero_2025_compacto,
    "2025 é linha de base pré-programa."
  ),
  registrar_validacao(
    "Variável sem período ausente",
    "assessoria",
    "erro",
    any(
      c(
        names(perfil_escola_completo),
        names(perfil_escola_gerencial),
        names(perfil_escola_serie_compacto)
      ) == "assessora_gerencial"
    ),
    "FALSE",
    !any(
      c(
        names(perfil_escola_completo),
        names(perfil_escola_gerencial),
        names(perfil_escola_serie_compacto)
      ) == "assessora_gerencial"
    ),
    "Não existe assessora gerencial sem período."
  ),
  registrar_validacao(
    "Campo legado fora dos produtos gerenciais",
    "assessoria",
    "erro",
    any(
      "assessora" %in%
        names(perfil_escola_gerencial),
      "assessora" %in%
        names(perfil_escola_serie_compacto)
    ),
    "FALSE",
    !any(
      "assessora" %in%
        names(perfil_escola_gerencial),
      "assessora" %in%
        names(perfil_escola_serie_compacto)
    ),
    "O campo legado só pode existir no perfil completo."
  ),
  registrar_validacao(
    "Campo legado documentado",
    "dicionário",
    "erro",
    dicionario_legado_correto,
    "TRUE",
    dicionario_legado_correto,
    "O dicionário identifica `assessora` como legado exclusivo do perfil completo."
  )
)

write_csv(
  comparacao_compacto_dim$diagnostico,
  file.path(
    pasta_execucao,
    "09_coerencia_campos_escolares_compacto.csv"
  ),
  na = ""
)

write_csv(
  exclusoes_observadas,
  file.path(
    pasta_execucao,
    "10_escolas_fora_universo_carga.csv"
  ),
  na = ""
)

write_csv(
  validacao_final,
  file.path(
    pasta_execucao,
    "11_validacao_final_pre_exportacao.csv"
  ),
  na = ""
)

write_csv(
  bind_rows(
    inventariar_estrutura(
      perfil_escola_completo,
      "perfil_escola_completo"
    ),
    inventariar_estrutura(
      perfil_escola_gerencial,
      "perfil_escola_gerencial"
    ),
    inventariar_estrutura(
      perfil_escola_serie_compacto,
      "perfil_escola_serie_compacto"
    )
  ),
  file.path(
    pasta_execucao,
    "12_estrutura_produtos_em_memoria.csv"
  ),
  na = ""
)

write_csv(
  completude_variaveis(
    perfil_escola_gerencial
  ),
  file.path(
    pasta_execucao,
    "13_completude_perfil_escola_gerencial.csv"
  ),
  na = ""
)

erros_criticos <- validacao_final |>
  filter(
    severidade == "erro",
    status == "reprovado"
  )

if (nrow(erros_criticos) > 0) {
  stop(
    "O módulo 16 encontrou erros críticos e não escreverá candidatos nem produtos canônicos. Consulte: ",
    file.path(
      pasta_execucao,
      "11_validacao_final_pre_exportacao.csv"
    )
  )
}

# -------------------------------------------------------------------
# 12. Escrita, releitura e validação dos candidatos
# -------------------------------------------------------------------

arquivos_candidatos <- set_names(
  file.path(
    pasta_candidatos,
    basename(
      arquivos_saida
    )
  ),
  names(
    arquivos_saida
  )
)

write_csv(
  perfil_escola_completo,
  arquivos_candidatos[["perfil_completo_csv"]],
  na = ""
)

saveRDS(
  perfil_escola_completo,
  arquivos_candidatos[["perfil_completo_rds"]],
  version = 3
)

write_csv(
  perfil_escola_gerencial,
  arquivos_candidatos[["perfil_gerencial_csv"]],
  na = ""
)

saveRDS(
  perfil_escola_gerencial,
  arquivos_candidatos[["perfil_gerencial_rds"]],
  version = 3
)

write_csv(
  perfil_escola_serie_compacto,
  arquivos_candidatos[["perfil_serie_csv"]],
  na = ""
)

saveRDS(
  perfil_escola_serie_compacto,
  arquivos_candidatos[["perfil_serie_rds"]],
  version = 3
)

write_csv(
  perfil_dicionario,
  arquivos_candidatos[["dicionario_csv"]],
  na = ""
)

reler_par_candidato <- function(
    nome_csv,
    nome_rds,
    chaves,
    fonte
) {
  objeto_rds <- readRDS(
    arquivos_candidatos[[nome_rds]]
  ) |>
    as_tibble()

  objeto_csv <- ler_csv_por_modelo(
    arquivos_candidatos[[nome_csv]],
    objeto_rds
  )

  comparar_bases_semanticamente(
    objeto_rds,
    objeto_csv,
    chaves,
    fonte
  )
}

candidato_completo <- reler_par_candidato(
  "perfil_completo_csv",
  "perfil_completo_rds",
  "id_escola",
  "candidato_perfil_escola_completo"
)

candidato_gerencial <- reler_par_candidato(
  "perfil_gerencial_csv",
  "perfil_gerencial_rds",
  "id_escola",
  "candidato_perfil_escola_gerencial"
)

candidato_compacto <- reler_par_candidato(
  "perfil_serie_csv",
  "perfil_serie_rds",
  c(
    "id_escola",
    "ano_escolar",
    "componente"
  ),
  "candidato_perfil_escola_serie_compacto"
)

dicionario_candidato <- ler_csv_por_modelo(
  arquivos_candidatos[["dicionario_csv"]],
  perfil_dicionario
)

comparacao_dicionario <- comparar_bases_semanticamente(
  perfil_dicionario,
  dicionario_candidato,
  "ordem_dicionario",
  "candidato_dicionario"
)

validacao_candidatos <- bind_rows(
  candidato_completo$diagnostico,
  candidato_gerencial$diagnostico,
  candidato_compacto$diagnostico,
  comparacao_dicionario$diagnostico
)

write_csv(
  validacao_candidatos,
  file.path(
    pasta_execucao,
    "14_validacao_releitura_candidatos.csv"
  ),
  na = ""
)

manifesto_candidatos <- imap_dfr(
  arquivos_candidatos,
  ~ manifestar_arquivo(
    .y,
    .x
  )
)

write_csv(
  manifesto_candidatos,
  file.path(
    pasta_execucao,
    "15_manifesto_candidatos.csv"
  ),
  na = ""
)

if (any(!validacao_candidatos$aprovado)) {
  stop(
    "A releitura dos candidatos revelou divergências. Nenhum produto canônico foi alterado. Consulte: ",
    file.path(
      pasta_execucao,
      "14_validacao_releitura_candidatos.csv"
    )
  )
}

# -------------------------------------------------------------------
# 13. Preservação histórica pré-11C
# -------------------------------------------------------------------

preservar_script_pre_11C <- function() {
  arquivo_temporario <- file.path(
    pasta_transacao,
    "16_criar_perfil_escola_pre_11C_git.txt"
  )

  arquivo_erro <- file.path(
    pasta_transacao,
    "git_show_erro.txt"
  )

  status <- tryCatch(
    system2(
      "git",
      c(
        "-C",
        shQuote(here()),
        "show",
        paste0(
          commit_base_integracao,
          ":",
          gsub(
            "\\\\",
            "/",
            caminho_relativo_script
          )
        )
      ),
      stdout = arquivo_temporario,
      stderr = arquivo_erro
    ),
    error = function(e) 1L
  )

  if (
    !identical(
      as.integer(status),
      0L
    ) ||
      !file.exists(
        arquivo_temporario
      )
  ) {
    stop(
      "Não foi possível recuperar o script pré-11C do commit-base."
    )
  }

  md5_git <- hash_md5(
    arquivo_temporario
  )

  if (file.exists(caminho_script_historico)) {
    md5_historico <- hash_md5(
      caminho_script_historico
    )

    if (md5_historico != md5_git) {
      stop(
        "O script histórico pré-11C existente diverge do commit-base."
      )
    }
  } else {
    copiado <- file.copy(
      arquivo_temporario,
      caminho_script_historico,
      overwrite = FALSE,
      copy.date = TRUE
    )

    if (
      !copiado ||
        hash_md5(
          caminho_script_historico
        ) != md5_git
    ) {
      stop(
        "Falha ao preservar o script histórico pré-11C."
      )
    }
  }

  tibble(
    origem = paste0(
      commit_base_integracao,
      ":",
      gsub(
        "\\\\",
        "/",
        caminho_relativo_script
      )
    ),
    destino = normalizar_caminho(
      caminho_script_historico
    ),
    md5_origem_git = md5_git,
    md5_copia = hash_md5(
      caminho_script_historico
    ),
    igualdade_confirmada =
      md5_git ==
      hash_md5(
        caminho_script_historico
      )
  )
}

preservar_produtos_pre_11C <- function() {
  if (
    file.exists(
      caminho_manifesto_historico_fixo
    )
  ) {
    manifesto_existente <- read_csv(
      caminho_manifesto_historico_fixo,
      col_types = cols(
        produto = col_character(),
        origem = col_character(),
        destino = col_character(),
        md5_antes_copia = col_character(),
        md5_copia = col_character(),
        igualdade_confirmada = col_logical()
      ),
      show_col_types = FALSE,
      progress = FALSE
    )

    if (
      nrow(manifesto_existente) != 7 ||
        !setequal(
          manifesto_existente$produto,
          names(
            hashes_produtos_pre_11C
          )
        ) ||
        any(
          !manifesto_existente$
            igualdade_confirmada
        ) ||
        any(
          manifesto_existente$md5_copia !=
            unname(
              hashes_produtos_pre_11C[
                manifesto_existente$
                  produto
              ]
            )
        ) ||
        any(
          !file.exists(
            manifesto_existente$destino
          )
        ) ||
        any(
          map_chr(
            manifesto_existente$destino,
            hash_md5
          ) !=
            manifesto_existente$md5_copia
        )
    ) {
      stop(
        "O histórico pré-11C existente não passou na validação de integridade."
      )
    }

    return(
      manifesto_existente
    )
  }

  if (
    any(
      !file.exists(
        arquivos_saida
      )
    )
  ) {
    stop(
      "A primeira execução pós-11C exige os sete produtos canônicos anteriores para preservação histórica."
    )
  }

  hashes_atuais <- map_chr(
    arquivos_saida,
    hash_md5
  )

  if (
    any(
      hashes_atuais !=
        unname(
          hashes_produtos_pre_11C[
            names(
              arquivos_saida
            )
          ]
        )
    )
  ) {
    stop(
      "Os produtos canônicos existentes não correspondem aos sete produtos pré-11C homologados; arquivamento bloqueado."
    )
  }

  destinos <- set_names(
    file.path(
      pasta_historico_pre_11C,
      basename(
        arquivos_saida
      )
    ),
    names(
      arquivos_saida
    )
  )

  manifesto <- imap_dfr(
    arquivos_saida,
    function(origem, produto) {
      destino <- destinos[[produto]]

      md5_origem <- hash_md5(
        origem
      )

      copiado <- if (
        file.exists(destino)
      ) {
        hash_md5(destino) == md5_origem
      } else {
        file.copy(
          origem,
          destino,
          overwrite = FALSE,
          copy.date = TRUE
        )
      }

      md5_destino <- hash_md5(
        destino
      )

      tibble(
        produto = produto,
        origem = normalizar_caminho(
          origem
        ),
        destino = normalizar_caminho(
          destino
        ),
        md5_antes_copia = md5_origem,
        md5_copia = md5_destino,
        igualdade_confirmada =
          copiado &&
          !is.na(md5_origem) &&
          md5_origem == md5_destino
      )
    }
  )

  if (
    any(
      !manifesto$igualdade_confirmada
    )
  ) {
    stop(
      "Falha na preservação dos sete produtos pré-11C."
    )
  }

  write_csv(
    manifesto,
    caminho_manifesto_historico_fixo,
    na = ""
  )

  manifesto
}

manifesto_script_historico <-
  preservar_script_pre_11C()

manifesto_historico_pre_11C <-
  preservar_produtos_pre_11C()

write_csv(
  manifesto_script_historico,
  file.path(
    pasta_execucao,
    "16_manifesto_script_pre_11C.csv"
  ),
  na = ""
)

write_csv(
  manifesto_historico_pre_11C,
  file.path(
    pasta_execucao,
    "17_manifesto_produtos_pre_11C.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 14. Promoção transacional e rollback
# -------------------------------------------------------------------

promover_transacionalmente <- function(
    candidatos,
    destinos
) {
  produtos <- names(
    destinos
  )

  if (
    is.null(produtos) ||
      anyNA(produtos) ||
      any(produtos == "") ||
      anyDuplicated(produtos) > 0
  ) {
    stop(
      "Os destinos da promoção devem possuir nomes válidos e únicos."
    )
  }

  if (
    !identical(
      names(candidatos),
      produtos
    )
  ) {
    stop(
      "Candidatos e destinos não possuem o mesmo contrato nominal."
    )
  }

  backups <- set_names(
    file.path(
      pasta_rollback,
      basename(
        destinos
      )
    ),
    produtos
  )

  # `file.exists()` não garante a preservação dos nomes do vetor de
  # caminhos. Os nomes são repostos explicitamente porque toda a
  # transação indexa seus controles pela identidade de cada produto.
  existia <- set_names(
    file.exists(
      unname(
        destinos
      )
    ),
    produtos
  )

  hashes_antes <- set_names(
    map_chr(
      unname(
        destinos
      ),
      hash_md5
    ),
    produtos
  )

  movidos_para_rollback <- rep(
    FALSE,
    length(destinos)
  )

  promovidos <- rep(
    FALSE,
    length(destinos)
  )

  names(movidos_para_rollback) <-
    produtos
  names(promovidos) <-
    produtos

  restaurar <- function() {
    for (
      produto in rev(
        produtos
      )
    ) {
      destino <- destinos[[produto]]
      backup <- backups[[produto]]

      if (
        promovidos[[produto]] &&
          file.exists(destino)
      ) {
        file.remove(
          destino
        )
      }

      if (
        movidos_para_rollback[[produto]] &&
          file.exists(backup)
      ) {
        file.rename(
          backup,
          destino
        )
      }
    }

    restaurado <- map2_lgl(
      produtos,
      unname(
        existia
      ),
      function(produto, existia_antes) {
        destino <- destinos[[produto]]

        if (existia_antes) {
          file.exists(destino) &&
            hash_md5(destino) ==
              hashes_antes[[produto]]
        } else {
          !file.exists(destino)
        }
      }
    )

    all(restaurado)
  }

  resultado <- tryCatch(
    {
      for (produto in produtos) {
        if (existia[[produto]]) {
          movido <- file.rename(
            destinos[[produto]],
            backups[[produto]]
          )

          if (!movido) {
            stop(
              "Falha ao preparar rollback de `",
              produto,
              "`."
            )
          }

          movidos_para_rollback[[produto]] <- TRUE
        }
      }

      for (produto in produtos) {
        promovido <- file.rename(
          candidatos[[produto]],
          destinos[[produto]]
        )

        if (!promovido) {
          stop(
            "Falha ao promover `",
            produto,
            "`."
          )
        }

        promovidos[[produto]] <- TRUE
      }

      hashes_finais <- set_names(
        map_chr(
          unname(
            destinos
          ),
          hash_md5
        ),
        produtos
      )

      hashes_candidatos <- manifesto_candidatos$
        md5[
          match(
            produtos,
            manifesto_candidatos$produto
          )
        ]

      if (
        any(
          is.na(hashes_finais)
        ) ||
          any(
            hashes_finais !=
              hashes_candidatos
          )
      ) {
        stop(
          "Os hashes após promoção divergem dos candidatos."
        )
      }

      tibble(
        produto = produtos,
        destino = map_chr(
          destinos,
          normalizar_caminho
        ),
        existia_antes = unname(
          existia
        ),
        backup_rollback = map2_chr(
          unname(existia),
          backups,
          ~ if (.x) {
            normalizar_caminho(
              .y,
              deve_existir = TRUE
            )
          } else {
            NA_character_
          }
        ),
        md5_candidato = hashes_candidatos,
        md5_final = hashes_finais,
        promocao_confirmada =
          hashes_candidatos ==
          hashes_finais
      )
    },
    error = function(e) {
      rollback_confirmado <- restaurar()

      writeLines(
        c(
          paste0(
            "Falha: ",
            conditionMessage(e)
          ),
          paste0(
            "Instante: ",
            format(
              Sys.time(),
              "%Y-%m-%d %H:%M:%S %z"
            )
          ),
          paste0(
            "Rollback confirmado: ",
            rollback_confirmado
          )
        ),
        file.path(
          pasta_execucao,
          "18_falha_promocao_rollback.txt"
        )
      )

      if (rollback_confirmado) {
        stop(
          "Promoção transacional falhou; os produtos anteriores foram restaurados. Detalhe: ",
          conditionMessage(e)
        )
      } else {
        stop(
          "Promoção transacional falhou e o rollback não pôde ser integralmente confirmado. Preserve a pasta de transação e restaure pelos arquivos históricos. Detalhe: ",
          conditionMessage(e)
        )
      }
    }
  )

  resultado
}

resultado_promocao <- promover_transacionalmente(
  arquivos_candidatos,
  arquivos_saida
)

write_csv(
  resultado_promocao,
  file.path(
    pasta_execucao,
    "18_resultado_promocao_transacional.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 15. Conferência final, manifestos e resumo
# -------------------------------------------------------------------

manifesto_produtos <- imap_dfr(
  arquivos_saida,
  ~ manifestar_arquivo(
    .y,
    .x
  )
)

write_csv(
  manifesto_produtos,
  file.path(
    pasta_execucao,
    "19_manifesto_produtos_modulo_16.csv"
  ),
  na = ""
)

write_csv(
  bind_rows(
    inventariar_estrutura(
      readRDS(
        arquivos_saida[[
          "perfil_completo_rds"
        ]]
      ),
      "perfil_escola_completo_final"
    ),
    inventariar_estrutura(
      readRDS(
        arquivos_saida[[
          "perfil_gerencial_rds"
        ]]
      ),
      "perfil_escola_gerencial_final"
    ),
    inventariar_estrutura(
      readRDS(
        arquivos_saida[[
          "perfil_serie_rds"
        ]]
      ),
      "perfil_escola_serie_compacto_final"
    )
  ),
  file.path(
    pasta_execucao,
    "20_estrutura_produtos_finais.csv"
  ),
  na = ""
)

capture.output(
  sessionInfo(),
  file = file.path(
    pasta_execucao,
    "21_session_info.txt"
  )
)

resumo_execucao <- c(
  paste0(
    "Execução: ",
    id_execucao
  ),
  paste0(
    "Branch: ",
    branch_execucao
  ),
  paste0(
    "Commit-base de integração: ",
    commit_base_integracao
  ),
  paste0(
    "Commit Git da execução: ",
    commit_git_execucao
  ),
  paste0(
    "Script: ",
    normalizar_caminho(
      caminho_script
    )
  ),
  paste0(
    "MD5 do script: ",
    hash_md5(
      caminho_script
    )
  ),
  paste0(
    "Escolas no perfil completo: ",
    nrow(
      perfil_escola_completo
    )
  ),
  paste0(
    "Escolas no perfil gerencial: ",
    nrow(
      perfil_escola_gerencial
    )
  ),
  paste0(
    "Linhas no perfil compacto: ",
    nrow(
      perfil_escola_serie_compacto
    )
  ),
  paste0(
    "Escolas na carga 2026: ",
    n_distinct(
      universo_carga_dim$id_escola
    )
  ),
  paste0(
    "Assessoras na carga 2026: ",
    assessoras_carga_2026
  ),
  "",
  "Observações metodológicas:",
  "- Estudo observacional e descritivo; não identifica efeitos.",
  "- Resultados educacionais não são atribuídos às assessoras.",
  "- Sínteses multissérie são descrições operacionais, não índices ou rankings.",
  "- Proficiência ±2 pontos e participação ±5 p.p. são heurísticas diagnósticas.",
  "- Alertas de participação ±10 p.p. e composição 20% também são heurísticos.",
  "- Quartis contextuais foram herdados do módulo 15/contexto 2024.",
  "- Nenhuma consulta ou junção com BigQuery foi realizada."
)

writeLines(
  resumo_execucao,
  file.path(
    pasta_execucao,
    "22_resumo_execucao.txt"
  )
)

if (
  any(
    !resultado_promocao$
      promocao_confirmada
  )
) {
  stop(
    "A promoção foi concluída, mas a conferência final não foi integralmente aprovada."
  )
}

message(
  "Módulo 16 concluído com sucesso.\n",
  "Produtos promovidos: ",
  length(
    arquivos_saida
  ),
  "\nDiagnósticos: ",
  pasta_execucao
)
