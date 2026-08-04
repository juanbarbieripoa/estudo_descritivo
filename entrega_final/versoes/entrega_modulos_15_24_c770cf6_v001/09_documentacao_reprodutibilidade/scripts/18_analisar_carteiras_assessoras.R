# ===================================================================
# 18_analisar_carteiras_assessoras.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# ===================================================================
#
# OBJETIVO
#
# Descrever as 11 carteiras gerenciais de assessoramento de 2026 no
# universo operacional homologado de 53 escolas, mantendo rigorosamente
# separadas:
#
#   1. carga extensiva: escolas, matrículas, turmas e etapas;
#   2. índice operacional homologado do módulo 17:
#      - volume: 40%;
#      - estrutura: 35%;
#      - complexidade administrativa: 25%;
#   3. diagnóstico educacional contextual, com peso zero no índice;
#   4. alertas de participação e composição.
#
# O módulo NÃO:
#
# - avalia qualidade, produtividade ou efetividade das assessoras;
# - atribui resultados educacionais às assessoras;
# - recalcula o índice operacional;
# - cria nova normalização, percentil, faixa ou cenário de pesos;
# - produz ranking de escolas ou assessoras;
# - trata a soma dos escores como medida total ou definitiva de carga;
# - inclui ESC_001, ESC_055 ou ESC_056 nas carteiras operacionais.
#
# CONTROLE DE VERSÕES
#
# Na pasta R existiam quatro variantes concorrentes do módulo 18:
#
# - 18_analisar_carteiras_assessoras.R
# - 18_analisar_carteiras_assessoras_corrigido.R
# - 18_analisar_carteiras_assessoras_corrigido_v2.R
# - 18_analisar_carteiras_assessoras_corrigido_v3.R
#
# A variante v3 é a última versão histórica conhecida com registro de
# execução. Este arquivo passa a ser o único caminho canônico executável.
# As variantes anteriores devem ser preservadas como histórico, sem serem
# executadas ou promovidas novamente.
#
# EXECUÇÃO CONTROLADA
#
# - caminho canônico obrigatório;
# - branch e HEAD homologados;
# - seis entradas obrigatórias (três pares CSV/RDS);
# - conferência dos hashes pelos manifestos homologados dos módulos 16 e 17;
# - equivalência semântica CSV/RDS;
# - perfil institucional de 56 escolas;
# - índice operacional de 53 escolas;
# - diagnóstico educacional de 265 linhas;
# - exatamente 11 assessoras gerenciais;
# - exclusões exatas: ESC_001, ESC_055 e ESC_056;
# - escrita, releitura, validação, preservação histórica, promoção e
#   rollback transacionais.
# ===================================================================

library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Contrato da execução e caminhos canônicos
# -------------------------------------------------------------------

instante_execucao <- Sys.time()
id_execucao <- format(instante_execucao, "%Y%m%d_%H%M%S")

branch_esperada <- "refatoracao_modulo_21"
commit_base_integracao <- paste0(
  "5b75ba8ff3288afb4b2e956e6f68be26517d5c6f"
)

caminho_script <- here(
  "R",
  "18_analisar_carteiras_assessoras.R"
)

caminho_relativo_script <- file.path(
  "R",
  "18_analisar_carteiras_assessoras.R"
)

caminho_manifesto_modulo_16 <- here(
  "documentacao",
  "perfil_escola",
  "execucao_20260726_223447",
  "19_manifesto_produtos_modulo_16.csv"
)

caminho_manifesto_modulo_17 <- here(
  "documentacao",
  "indice_complexidade",
  "execucao_20260728_220908",
  "19_manifesto_produtos_modulo_17.csv"
)

arquivos_entrada <- c(
  perfil_gerencial_csv = here(
    "dados_finais",
    "perfil_escola_gerencial.csv"
  ),
  perfil_gerencial_rds = here(
    "dados_finais",
    "perfil_escola_gerencial.rds"
  ),
  indice_operacional_csv = here(
    "dados_finais",
    "indice_carga_potencial_escola.csv"
  ),
  indice_operacional_rds = here(
    "dados_finais",
    "indice_carga_potencial_escola.rds"
  ),
  diagnostico_educacional_csv = here(
    "dados_finais",
    "componentes_educacionais_escola_serie.csv"
  ),
  diagnostico_educacional_rds = here(
    "dados_finais",
    "componentes_educacionais_escola_serie.rds"
  )
)

arquivos_saida <- c(
  analise_carteiras_csv = here(
    "dados_finais",
    "analise_carteiras_assessoras.csv"
  ),
  analise_carteiras_rds = here(
    "dados_finais",
    "analise_carteiras_assessoras.rds"
  ),
  detalhe_escolas_csv = here(
    "dados_finais",
    "carteira_escola_detalhe.csv"
  ),
  detalhe_escolas_rds = here(
    "dados_finais",
    "carteira_escola_detalhe.rds"
  ),
  diagnostico_carteiras_csv = here(
    "dados_finais",
    "diagnostico_educacional_carteiras.csv"
  ),
  diagnostico_carteiras_rds = here(
    "dados_finais",
    "diagnostico_educacional_carteiras.rds"
  ),
  composicao_operacional_csv = here(
    "dados_finais",
    "composicao_operacional_carteiras.csv"
  ),
  composicao_operacional_rds = here(
    "dados_finais",
    "composicao_operacional_carteiras.rds"
  ),
  universo_institucional_csv = here(
    "dados_finais",
    "universo_institucional_carteiras.csv"
  ),
  universo_institucional_rds = here(
    "dados_finais",
    "universo_institucional_carteiras.rds"
  ),
  dicionario_csv = here(
    "documentacao",
    "analise_carteiras",
    "dicionario_analise_carteiras.csv"
  )
)

ids_excluidos_carga <- c(
  "ESC_001",
  "ESC_055",
  "ESC_056"
)

pares_excluidos_carga <- tribble(
  ~id_escola, ~codigo_inep,
  "ESC_001", "43105416",
  "ESC_055", "43105300",
  "ESC_056", "43189768"
)

pasta_documentacao <- here(
  "documentacao",
  "analise_carteiras"
)

pasta_execucao <- file.path(
  pasta_documentacao,
  paste0("execucao_", id_execucao)
)

pasta_historico <- here(
  "dados_finais",
  "historico",
  "analise_carteiras",
  paste0("pre_refatoracao_execucao_", id_execucao)
)

pasta_transacao <- here(
  "dados_finais",
  "historico",
  "analise_carteiras",
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

walk(
  c(
    pasta_documentacao,
    pasta_execucao,
    pasta_historico,
    pasta_transacao,
    pasta_candidatos,
    pasta_rollback
  ),
  ~ dir.create(
    .x,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

# -------------------------------------------------------------------
# 2. Funções auxiliares gerais
# -------------------------------------------------------------------

hash_md5 <- function(caminho) {
  if (
    length(caminho) != 1L ||
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

  if (length(saida) != 1L) {
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

  if (length(saida) != 1L) {
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
  texto <- str_to_lower(
    str_squish(
      as.character(x)
    )
  )

  resultado <- case_when(
    is.na(texto) | texto == "" ~ NA,
    texto %in% c(
      "true", "t", "1", "sim", "s"
    ) ~ TRUE,
    texto %in% c(
      "false", "f", "0", "nao", "não", "n"
    ) ~ FALSE,
    TRUE ~ NA
  )

  invalidos <- !is.na(texto) &
    texto != "" &
    is.na(resultado)

  if (any(invalidos)) {
    stop(
      "Valores lógicos inválidos em `",
      variavel,
      "`."
    )
  }

  resultado
}

converter_por_modelo <- function(
    x,
    modelo,
    variavel
) {
  tipo <- tipo_canonico(modelo)

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
      tz = "UTC"
    ),
    stop(
      "Tipo não suportado em `",
      variavel,
      "`: ",
      tipo
    )
  )

  if (tipo %in% c("integer", "double")) {
    preenchido <- !is.na(x) &
      str_squish(
        as.character(x)
      ) != ""

    if (
      any(
        preenchido &
          is.na(resultado)
      )
    ) {
      stop(
        "Falha de conversão numérica em `",
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
    na = c("", "NA"),
    trim_ws = FALSE,
    name_repair = "minimal",
    show_col_types = FALSE,
    progress = FALSE
  )

  if (
    !identical(
      names(bruto),
      names(modelo)
    )
  ) {
    stop(
      "Nomes ou ordem de colunas divergentes em: ",
      caminho
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

  as_tibble(saida)
}

comparar_bases_semanticamente <- function(
    rds,
    csv,
    chaves,
    fonte
) {
  ordenar <- function(x) {
    as_tibble(x) |>
      arrange(
        across(
          all_of(chaves)
        )
      ) |>
      select(
        all_of(
          names(x)
        )
      )
  }

  a <- ordenar(rds)
  b <- ordenar(csv)

  mesmos_nomes <- identical(
    names(a),
    names(b)
  )

  mesmos_tipos <- mesmos_nomes &&
    identical(
      map_chr(
        a,
        tipo_canonico
      ),
      map_chr(
        b,
        tipo_canonico
      )
    )

  mesmas_dimensoes <- identical(
    dim(a),
    dim(b)
  )

  comparacao <- if (
    mesmos_nomes &&
      mesmos_tipos &&
      mesmas_dimensoes
  ) {
    all.equal(
      a,
      b,
      check.attributes = FALSE,
      tolerance = 1e-12
    )
  } else {
    "estrutura divergente"
  }

  mesmos_valores <- isTRUE(
    comparacao
  )

  list(
    dados = a,
    diagnostico = tibble(
      fonte = fonte,
      linhas_rds = nrow(a),
      linhas_csv = nrow(b),
      colunas_rds = ncol(a),
      colunas_csv = ncol(b),
      mesmos_nomes_e_ordem = mesmos_nomes,
      mesmos_tipos_canonicos = mesmos_tipos,
      mesmas_dimensoes = mesmas_dimensoes,
      mesmos_valores_ordenados = mesmos_valores,
      detalhe = if (mesmos_valores) {
        "equivalentes"
      } else {
        paste(
          comparacao,
          collapse = " | "
        )
      },
      aprovado = mesmos_nomes &&
        mesmos_tipos &&
        mesmas_dimensoes &&
        mesmos_valores
    )
  )
}

normalizar_codigo_inep <- function(x) {
  x |>
    as.character() |>
    str_replace_all(
      "[^0-9]",
      ""
    ) |>
    na_if("")
}

as_logical_seguro <- function(
    x,
    variavel
) {
  if (is.logical(x)) {
    return(x)
  }

  converter_logico_seguro(
    x,
    variavel
  )
}

soma_segura <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )

  if (all(is.na(x))) {
    return(NA_real_)
  }

  sum(
    x,
    na.rm = TRUE
  )
}

media_segura <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )

  x <- x[
    !is.na(x)
  ]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  mean(x)
}

mediana_segura <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )

  x <- x[
    !is.na(x)
  ]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  median(x)
}

desvio_seguro <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )

  x <- x[
    !is.na(x)
  ]

  if (length(x) < 2L) {
    return(NA_real_)
  }

  sd(x)
}

minimo_seguro <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )

  x <- x[
    !is.na(x)
  ]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  min(x)
}

maximo_seguro <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )

  x <- x[
    !is.na(x)
  ]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x)
}

intervalo_interquartil_seguro <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )

  x <- x[
    !is.na(x)
  ]

  if (length(x) < 2L) {
    return(NA_real_)
  }

  IQR(x)
}

media_ponderada_segura <- function(
    x,
    w
) {
  x <- suppressWarnings(
    as.numeric(x)
  )

  w <- suppressWarnings(
    as.numeric(w)
  )

  valido <- !is.na(x) &
    !is.na(w) &
    w > 0

  if (!any(valido)) {
    return(NA_real_)
  }

  weighted.mean(
    x[valido],
    w[valido]
  )
}

participacao_maiores <- function(
    x,
    n = 1L
) {
  x <- suppressWarnings(
    as.numeric(x)
  )

  x <- x[
    !is.na(x) &
      x >= 0
  ]

  if (
    length(x) == 0L ||
      sum(x) <= 0
  ) {
    return(NA_real_)
  }

  100 *
    sum(
      head(
        sort(
          x,
          decreasing = TRUE
        ),
        n
      )
    ) /
    sum(x)
}

calcular_hhi <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )

  x <- x[
    !is.na(x) &
      x >= 0
  ]

  if (
    length(x) == 0L ||
      sum(x) <= 0
  ) {
    return(NA_real_)
  }

  participacoes <- x /
    sum(x)

  sum(
    participacoes^2
  ) * 10000
}

registrar_validacao <- function(
    teste,
    categoria,
    severidade,
    valor_observado,
    criterio,
    resultado,
    observacao = ""
) {
  tibble(
    teste = teste,
    categoria = categoria,
    severidade = severidade,
    valor_observado = as.character(
      valor_observado
    ),
    criterio = criterio,
    resultado = isTRUE(
      resultado
    ),
    nivel = if_else(
      isTRUE(resultado),
      "OK",
      str_to_upper(severidade)
    ),
    observacao = observacao
  )
}

estrutura_base <- function(
    dados,
    produto
) {
  tibble(
    produto = produto,
    ordem_coluna = seq_along(dados),
    variavel = names(dados),
    classe_r = map_chr(
      dados,
      ~ paste(
        class(.x),
        collapse = " | "
      )
    ),
    valores_ausentes = map_int(
      dados,
      ~ sum(
        is.na(.x)
      )
    ),
    valores_distintos = map_int(
      dados,
      ~ n_distinct(
        .x,
        na.rm = TRUE
      )
    )
  )
}

inventariar_arquivo <- function(
    nome,
    caminho
) {
  tibble(
    arquivo = nome,
    caminho = normalizar_caminho(
      caminho,
      deve_existir = FALSE
    ),
    existe = file.exists(caminho),
    tamanho_bytes = if (
      file.exists(caminho)
    ) {
      file.info(caminho)$size
    } else {
      NA_real_
    },
    md5 = hash_md5(caminho)
  )
}

copiar_com_validacao <- function(
    origem,
    destino,
    sobrescrever = FALSE
) {
  dir.create(
    dirname(destino),
    recursive = TRUE,
    showWarnings = FALSE
  )

  sucesso <- file.copy(
    origem,
    destino,
    overwrite = sobrescrever,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  if (!sucesso) {
    stop(
      "Falha ao copiar `",
      origem,
      "` para `",
      destino,
      "`."
    )
  }

  if (
    !identical(
      hash_md5(origem),
      hash_md5(destino)
    )
  ) {
    stop(
      "Hash divergente após copiar `",
      origem,
      "`."
    )
  }

  invisible(TRUE)
}

# -------------------------------------------------------------------
# 3. Bloqueios de execução, arquivos e manifestos
# -------------------------------------------------------------------

caminho_script_normalizado <- normalizar_caminho(
  caminho_script,
  deve_existir = FALSE
)

caminho_canonico_esperado <- normalizar_caminho(
  here(
    "R",
    "18_analisar_carteiras_assessoras.R"
  ),
  deve_existir = FALSE
)

if (
  !identical(
    caminho_script_normalizado,
    caminho_canonico_esperado
  )
) {
  stop(
    "O módulo 18 deve ser executado exclusivamente pelo caminho canônico: ",
    caminho_canonico_esperado
  )
}

if (!file.exists(caminho_script)) {
  stop(
    "Script canônico não encontrado: ",
    caminho_script
  )
}

branch_observada <- obter_branch_git()
commit_observado <- obter_commit_git()

if (
  is.na(branch_observada) ||
    branch_observada != branch_esperada
) {
  stop(
    "Branch divergente. Esperada: `",
    branch_esperada,
    "`; observada: `",
    branch_observada,
    "`."
  )
}

if (
  is.na(commit_observado) ||
    commit_observado != commit_base_integracao
) {
  stop(
    "HEAD divergente. Esperado: `",
    commit_base_integracao,
    "`; observado: `",
    commit_observado,
    "`."
  )
}

arquivos_obrigatorios <- c(
  arquivos_entrada,
  manifesto_modulo_16 = caminho_manifesto_modulo_16,
  manifesto_modulo_17 = caminho_manifesto_modulo_17
)

ausentes <- arquivos_obrigatorios[
  !file.exists(
    arquivos_obrigatorios
  )
]

if (length(ausentes) > 0L) {
  stop(
    "Arquivos obrigatórios ausentes:\n",
    paste(
      paste0(
        "- ",
        names(ausentes),
        ": ",
        ausentes
      ),
      collapse = "\n"
    )
  )
}

manifesto_16 <- read_csv(
  caminho_manifesto_modulo_16,
  show_col_types = FALSE,
  na = c("", "NA")
)

manifesto_17 <- read_csv(
  caminho_manifesto_modulo_17,
  show_col_types = FALSE,
  na = c("", "NA")
)

validar_manifesto <- function(
    manifesto,
    nome_manifesto
) {
  colunas_minimas <- c(
    "produto",
    "caminho",
    "tamanho_bytes",
    "md5"
  )

  ausentes <- setdiff(
    colunas_minimas,
    names(manifesto)
  )

  if (length(ausentes) > 0L) {
    stop(
      "Manifesto `",
      nome_manifesto,
      "` sem colunas: ",
      paste(
        ausentes,
        collapse = ", "
      )
    )
  }

  manifesto
}

manifesto_16 <- validar_manifesto(
  manifesto_16,
  "módulo 16"
)

manifesto_17 <- validar_manifesto(
  manifesto_17,
  "módulo 17"
)

localizar_hash_manifesto <- function(
    manifesto,
    caminho,
    nome_fonte
) {
  alvo <- basename(caminho)

  candidatos <- manifesto |>
    filter(
      basename(caminho) == alvo
    )

  if (nrow(candidatos) != 1L) {
    stop(
      "Não foi possível localizar unicamente `",
      alvo,
      "` no manifesto de ",
      nome_fonte,
      "."
    )
  }

  hash <- candidatos$md5[[1]]

  if (
    is.na(hash) ||
      !str_detect(
        hash,
        "^[0-9a-fA-F]{32}$"
      )
  ) {
    stop(
      "Hash inválido para `",
      alvo,
      "` no manifesto de ",
      nome_fonte,
      "."
    )
  }

  str_to_lower(hash)
}

hashes_esperados <- c(
  perfil_gerencial_csv = localizar_hash_manifesto(
    manifesto_16,
    arquivos_entrada[["perfil_gerencial_csv"]],
    "módulo 16"
  ),
  perfil_gerencial_rds = localizar_hash_manifesto(
    manifesto_16,
    arquivos_entrada[["perfil_gerencial_rds"]],
    "módulo 16"
  ),
  indice_operacional_csv = localizar_hash_manifesto(
    manifesto_17,
    arquivos_entrada[["indice_operacional_csv"]],
    "módulo 17"
  ),
  indice_operacional_rds = localizar_hash_manifesto(
    manifesto_17,
    arquivos_entrada[["indice_operacional_rds"]],
    "módulo 17"
  ),
  diagnostico_educacional_csv = localizar_hash_manifesto(
    manifesto_17,
    arquivos_entrada[["diagnostico_educacional_csv"]],
    "módulo 17"
  ),
  diagnostico_educacional_rds = localizar_hash_manifesto(
    manifesto_17,
    arquivos_entrada[["diagnostico_educacional_rds"]],
    "módulo 17"
  )
)

hashes_observados <- map_chr(
  arquivos_entrada,
  hash_md5
)

if (
  !identical(
    names(hashes_esperados),
    names(hashes_observados)
  )
) {
  stop(
    "Falha interna na correspondência dos hashes das entradas."
  )
}

divergencias_hash <- names(
  hashes_observados
)[
  str_to_lower(
    hashes_observados
  ) !=
    str_to_lower(
      hashes_esperados
    )
]

if (length(divergencias_hash) > 0L) {
  stop(
    "Hashes divergentes nas entradas: ",
    paste(
      divergencias_hash,
      collapse = ", "
    )
  )
}

# -------------------------------------------------------------------
# 4. Leitura simultânea e equivalência CSV–RDS
# -------------------------------------------------------------------

perfil_rds <- readRDS(
  arquivos_entrada[["perfil_gerencial_rds"]]
)

indice_rds <- readRDS(
  arquivos_entrada[["indice_operacional_rds"]]
)

diagnostico_rds <- readRDS(
  arquivos_entrada[["diagnostico_educacional_rds"]]
)

if (!is.data.frame(perfil_rds)) {
  stop(
    "O perfil gerencial RDS não é data frame."
  )
}

if (!is.data.frame(indice_rds)) {
  stop(
    "O índice operacional RDS não é data frame."
  )
}

if (!is.data.frame(diagnostico_rds)) {
  stop(
    "O diagnóstico educacional RDS não é data frame."
  )
}

perfil_csv <- ler_csv_por_modelo(
  arquivos_entrada[["perfil_gerencial_csv"]],
  perfil_rds
)

indice_csv <- ler_csv_por_modelo(
  arquivos_entrada[["indice_operacional_csv"]],
  indice_rds
)

diagnostico_csv <- ler_csv_por_modelo(
  arquivos_entrada[["diagnostico_educacional_csv"]],
  diagnostico_rds
)

comparacao_perfil <- comparar_bases_semanticamente(
  perfil_rds,
  perfil_csv,
  chaves = c("id_escola"),
  fonte = "perfil_escola_gerencial"
)

comparacao_indice <- comparar_bases_semanticamente(
  indice_rds,
  indice_csv,
  chaves = c("id_escola"),
  fonte = "indice_carga_potencial_escola"
)

comparacao_diagnostico <- comparar_bases_semanticamente(
  diagnostico_rds,
  diagnostico_csv,
  chaves = c(
    "id_escola",
    "ano_escolar",
    "componente"
  ),
  fonte = "componentes_educacionais_escola_serie"
)

diagnostico_equivalencia_entradas <- bind_rows(
  comparacao_perfil$diagnostico,
  comparacao_indice$diagnostico,
  comparacao_diagnostico$diagnostico
)

if (
  any(
    !diagnostico_equivalencia_entradas$aprovado
  )
) {
  stop(
    "Há divergência semântica entre CSV e RDS das entradas."
  )
}

perfil_escola <- comparacao_perfil$dados
indice_escola <- comparacao_indice$dados
diagnostico_educacional <- comparacao_diagnostico$dados

# -------------------------------------------------------------------
# 5. Contratos mínimos das entradas
# -------------------------------------------------------------------

colunas_obrigatorias_perfil <- c(
  "id_escola",
  "codigo_inep",
  "nome_canonico",
  "assessora_vinculo_administrativo",
  "assessora_gerencial_2026",
  "incluir_indice_carga_2026",
  "elegivel_assessoramento_2026",
  "recebe_assessoramento_2026",
  "status_carga_operacional_2026",
  "grupo_exposicao_2026"
)

colunas_obrigatorias_indice <- c(
  "id_escola",
  "codigo_inep",
  "nome_canonico",
  "assessora_vinculo_administrativo",
  "assessora_gerencial_2026",
  "incluir_indice_carga_2026",
  "matriculas_anos_iniciais",
  "turmas_anos_iniciais",
  "turmas_anos_iniciais_validas",
  "numero_etapas_amplas_ofertadas",
  "score_dimensao_volume",
  "cobertura_dimensao_volume",
  "score_dimensao_estrutural",
  "cobertura_dimensao_estrutural",
  "score_dimensao_administrativa",
  "cobertura_dimensao_administrativa",
  "contribuicao_volume",
  "contribuicao_estrutural",
  "contribuicao_administrativa",
  "indice_carga_potencial_operacional",
  "cobertura_indice_operacional",
  "interpretacao_cautelosa",
  "resultados_educacionais_no_indice",
  "nota_uso"
)

colunas_obrigatorias_diagnostico <- c(
  "id_escola",
  "codigo_inep",
  "nome_canonico",
  "assessora_gerencial_2026",
  "ano_escolar",
  "componente",
  "previstos_2026",
  "avaliados_2026",
  "taxa_participacao_2026",
  "proficiencia_media_2026",
  "pct_defasagem_2026",
  "pct_intermediario_2026",
  "pct_adequado_2026",
  "painel_resultado_balanceado",
  "delta_participacao",
  "delta_proficiencia",
  "variacao_relativa_previstos",
  "aumento_participacao_10pp",
  "queda_participacao_10pp",
  "mudanca_previstos_20pct",
  "alerta_composicao_serie",
  "uso_no_indice_carga_operacional",
  "peso_no_indice_carga_operacional",
  "natureza"
)

verificar_colunas <- function(
    dados,
    obrigatorias,
    fonte
) {
  ausentes <- setdiff(
    obrigatorias,
    names(dados)
  )

  if (length(ausentes) > 0L) {
    stop(
      "Colunas obrigatórias ausentes em `",
      fonte,
      "`: ",
      paste(
        ausentes,
        collapse = ", "
      )
    )
  }
}

verificar_colunas(
  perfil_escola,
  colunas_obrigatorias_perfil,
  "perfil_escola_gerencial"
)

verificar_colunas(
  indice_escola,
  colunas_obrigatorias_indice,
  "indice_carga_potencial_escola"
)

verificar_colunas(
  diagnostico_educacional,
  colunas_obrigatorias_diagnostico,
  "componentes_educacionais_escola_serie"
)

perfil_escola <- perfil_escola |>
  mutate(
    codigo_inep = normalizar_codigo_inep(
      codigo_inep
    ),
    incluir_indice_carga_2026 =
      as_logical_seguro(
        incluir_indice_carga_2026,
        "incluir_indice_carga_2026"
      ),
    elegivel_assessoramento_2026 =
      as_logical_seguro(
        elegivel_assessoramento_2026,
        "elegivel_assessoramento_2026"
      ),
    recebe_assessoramento_2026 =
      as_logical_seguro(
        recebe_assessoramento_2026,
        "recebe_assessoramento_2026"
      )
  )

indice_escola <- indice_escola |>
  mutate(
    codigo_inep = normalizar_codigo_inep(
      codigo_inep
    ),
    incluir_indice_carga_2026 =
      as_logical_seguro(
        incluir_indice_carga_2026,
        "incluir_indice_carga_2026"
      ),
    interpretacao_cautelosa =
      as_logical_seguro(
        interpretacao_cautelosa,
        "interpretacao_cautelosa"
      ),
    resultados_educacionais_no_indice =
      as_logical_seguro(
        resultados_educacionais_no_indice,
        "resultados_educacionais_no_indice"
      )
  )

diagnostico_educacional <- diagnostico_educacional |>
  mutate(
    codigo_inep = normalizar_codigo_inep(
      codigo_inep
    ),
    painel_resultado_balanceado =
      as_logical_seguro(
        painel_resultado_balanceado,
        "painel_resultado_balanceado"
      ),
    aumento_participacao_10pp =
      as_logical_seguro(
        aumento_participacao_10pp,
        "aumento_participacao_10pp"
      ),
    queda_participacao_10pp =
      as_logical_seguro(
        queda_participacao_10pp,
        "queda_participacao_10pp"
      ),
    mudanca_previstos_20pct =
      as_logical_seguro(
        mudanca_previstos_20pct,
        "mudanca_previstos_20pct"
      ),
    alerta_composicao_serie =
      as_logical_seguro(
        alerta_composicao_serie,
        "alerta_composicao_serie"
      ),
    uso_no_indice_carga_operacional =
      as_logical_seguro(
        uso_no_indice_carga_operacional,
        "uso_no_indice_carga_operacional"
      )
  )

# -------------------------------------------------------------------
# 6. Validações preliminares de universo e identificação
# -------------------------------------------------------------------

if (anyDuplicated(perfil_escola$id_escola) > 0L) {
  stop(
    "O perfil gerencial possui duplicidade de escola."
  )
}

if (anyDuplicated(indice_escola$id_escola) > 0L) {
  stop(
    "O índice operacional possui duplicidade de escola."
  )
}

chave_diagnostico <- diagnostico_educacional |>
  transmute(
    chave = paste(
      id_escola,
      ano_escolar,
      componente,
      sep = " | "
    )
  ) |>
  pull(chave)

if (anyDuplicated(chave_diagnostico) > 0L) {
  stop(
    "O diagnóstico educacional possui duplicidade de escola × série × componente."
  )
}

ids_perfil <- sort(
  unique(
    perfil_escola$id_escola
  )
)

ids_indice <- sort(
  unique(
    indice_escola$id_escola
  )
)

ids_diagnostico <- sort(
  unique(
    diagnostico_educacional$id_escola
  )
)

ids_diferenca <- sort(
  setdiff(
    ids_perfil,
    ids_indice
  )
)

if (
  !all(
    ids_indice %in% ids_perfil
  )
) {
  stop(
    "O índice operacional contém escola ausente do perfil institucional."
  )
}

if (
  !setequal(
    ids_diferenca,
    ids_excluidos_carga
  )
) {
  stop(
    "A diferença entre perfil e índice não corresponde às três exclusões homologadas."
  )
}

if (
  !setequal(
    ids_diagnostico,
    ids_indice
  )
) {
  stop(
    "O conjunto de escolas do diagnóstico educacional difere do índice operacional."
  )
}

pares_observados_excluidos <- perfil_escola |>
  filter(
    id_escola %in% ids_excluidos_carga
  ) |>
  transmute(
    id_escola,
    codigo_inep
  ) |>
  arrange(
    id_escola
  )

if (
  !isTRUE(
    all.equal(
      pares_observados_excluidos,
      pares_excluidos_carga |>
        arrange(id_escola),
      check.attributes = FALSE
    )
  )
) {
  stop(
    "Os pares ID × código INEP das exclusões não coincidem com o contrato."
  )
}

comparacao_atributos_indice_perfil <- indice_escola |>
  select(
    id_escola,
    codigo_inep_indice = codigo_inep,
    nome_indice = nome_canonico,
    vinculo_indice =
      assessora_vinculo_administrativo,
    assessora_indice =
      assessora_gerencial_2026
  ) |>
  inner_join(
    perfil_escola |>
      select(
        id_escola,
        codigo_inep_perfil = codigo_inep,
        nome_perfil = nome_canonico,
        vinculo_perfil =
          assessora_vinculo_administrativo,
        assessora_perfil =
          assessora_gerencial_2026
      ),
    by = "id_escola"
  ) |>
  mutate(
    conflito_codigo = codigo_inep_indice !=
      codigo_inep_perfil,
    conflito_nome = nome_indice !=
      nome_perfil,
    conflito_vinculo = coalesce(
      vinculo_indice,
      "<NA>"
    ) != coalesce(
      vinculo_perfil,
      "<NA>"
    ),
    conflito_assessora = coalesce(
      assessora_indice,
      "<NA>"
    ) != coalesce(
      assessora_perfil,
      "<NA>"
    )
  )

if (
  any(
    comparacao_atributos_indice_perfil$
      conflito_codigo |
      comparacao_atributos_indice_perfil$
        conflito_nome |
      comparacao_atributos_indice_perfil$
        conflito_vinculo |
      comparacao_atributos_indice_perfil$
        conflito_assessora
  )
) {
  stop(
    "Há divergências de identificação ou vínculo entre índice e perfil."
  )
}

if (
  any(
    is.na(
      indice_escola$
        assessora_gerencial_2026
    ) |
      str_squish(
        indice_escola$
          assessora_gerencial_2026
      ) == ""
  )
) {
  stop(
    "Há escola elegível sem `assessora_gerencial_2026`."
  )
}

categorias_residuais_proibidas <- c(
  "outras",
  "sem vinculação informada",
  "sem vinculacao informada"
)

if (
  any(
    str_to_lower(
      str_squish(
        indice_escola$
          assessora_gerencial_2026
      )
    ) %in%
      categorias_residuais_proibidas
  )
) {
  stop(
    "O universo operacional contém categoria gerencial residual proibida."
  )
}

# -------------------------------------------------------------------
# 7. Produto institucional de 56 escolas
# -------------------------------------------------------------------

universo_institucional_carteiras <- perfil_escola |>
  transmute(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_vinculo_administrativo,
    assessora_gerencial_2026,
    elegivel_assessoramento_2026,
    recebe_assessoramento_2026,
    incluir_indice_carga_2026,
    status_carga_operacional_2026,
    grupo_exposicao_2026,
    incluida_analise_operacional =
      id_escola %in% ids_indice,
    motivo_nao_inclusao = case_when(
      id_escola == "ESC_001" ~ paste(
        "Não elegível ao universo operacional homologado;",
        "fora da carga de assessoramento."
      ),
      id_escola == "ESC_055" ~ paste(
        "Não elegível ao universo operacional homologado;",
        "fora da carga de assessoramento."
      ),
      id_escola == "ESC_056" ~ paste(
        "Não elegível ao universo operacional homologado;",
        "fora da carga de assessoramento."
      ),
      TRUE ~ NA_character_
    ),
    nota_institucional = case_when(
      incluida_analise_operacional ~ paste(
        "Escola incluída no universo operacional de 2026."
      ),
      TRUE ~ paste(
        "Escola preservada no universo institucional de 56 escolas,",
        "mas excluída das carteiras operacionais."
      )
    )
  ) |>
  arrange(
    incluida_analise_operacional,
    id_escola
  )

# -------------------------------------------------------------------
# 8. Base operacional escola × carteira
# -------------------------------------------------------------------

base_escola_carteira <- indice_escola |>
  mutate(
    soma_contribuicoes_operacionais =
      contribuicao_volume +
      contribuicao_estrutural +
      contribuicao_administrativa,
    diferenca_recomposicao_indice = abs(
      indice_carga_potencial_operacional -
        soma_contribuicoes_operacionais
    ),
    possui_complexidade_administrativa =
      !is.na(
        score_dimensao_administrativa
      ) &
      score_dimensao_administrativa > 0,
    exige_leitura_operacional_cautelosa =
      coalesce(
        interpretacao_cautelosa,
        FALSE
      ) |
      cobertura_indice_operacional < 1
  ) |>
  group_by(
    assessora_gerencial_2026
  ) |>
  mutate(
    soma_indice_operacional_carteira =
      sum(
        indice_carga_potencial_operacional,
        na.rm = TRUE
      ),
    contribuicao_escola_soma_indice_carteira_pct =
      if_else(
        soma_indice_operacional_carteira > 0,
        100 *
          indice_carga_potencial_operacional /
          soma_indice_operacional_carteira,
        NA_real_
      )
  ) |>
  ungroup()

# -------------------------------------------------------------------
# 9. Detalhe das escolas nas carteiras — sem ranking
# -------------------------------------------------------------------

carteira_escola_detalhe <- base_escola_carteira |>
  transmute(
    assessora_gerencial_2026,
    assessora_vinculo_administrativo,
    divergencia_vinculo_gerencial =
      coalesce(
        assessora_vinculo_administrativo,
        "<NA>"
      ) != coalesce(
        assessora_gerencial_2026,
        "<NA>"
      ),
    id_escola,
    codigo_inep,
    nome_canonico,
    incluir_indice_carga_2026,
    matriculas_anos_iniciais,
    turmas_anos_iniciais,
    turmas_anos_iniciais_validas,
    numero_etapas_amplas_ofertadas,
    score_dimensao_volume,
    cobertura_dimensao_volume,
    score_dimensao_estrutural,
    cobertura_dimensao_estrutural,
    score_dimensao_administrativa,
    cobertura_dimensao_administrativa,
    contribuicao_volume,
    contribuicao_estrutural,
    contribuicao_administrativa,
    indice_carga_potencial_operacional,
    cobertura_indice_operacional,
    soma_indice_operacional_carteira,
    contribuicao_escola_soma_indice_carteira_pct,
    possui_complexidade_administrativa,
    exige_leitura_operacional_cautelosa,
    resultados_educacionais_no_indice,
    nota_uso,
    nota_carteira = paste(
      "Contribuição ao escore relativo da carteira;",
      "não representa horas, qualidade ou carga total real."
    )
  ) |>
  arrange(
    assessora_gerencial_2026,
    id_escola
  )

# -------------------------------------------------------------------
# 10. Síntese operacional por carteira
# -------------------------------------------------------------------

analise_carteiras <- base_escola_carteira |>
  group_by(
    assessora_gerencial_2026
  ) |>
  summarise(
    numero_escolas = n(),
    matriculas_anos_iniciais_total =
      soma_segura(
        matriculas_anos_iniciais
      ),
    escolas_com_matriculas_disponiveis =
      sum(
        !is.na(
          matriculas_anos_iniciais
        )
      ),
    turmas_anos_iniciais_total =
      soma_segura(
        turmas_anos_iniciais_validas
      ),
    escolas_com_turmas_disponiveis =
      sum(
        !is.na(
          turmas_anos_iniciais_validas
        )
      ),
    etapas_amplas_total =
      soma_segura(
        numero_etapas_amplas_ofertadas
      ),
    soma_indice_operacional_carteira =
      soma_segura(
        indice_carga_potencial_operacional
      ),
    media_indice_operacional_escolas =
      media_segura(
        indice_carga_potencial_operacional
      ),
    mediana_indice_operacional_escolas =
      mediana_segura(
        indice_carga_potencial_operacional
      ),
    desvio_indice_operacional_escolas =
      desvio_seguro(
        indice_carga_potencial_operacional
      ),
    iqr_indice_operacional_escolas =
      intervalo_interquartil_seguro(
        indice_carga_potencial_operacional
      ),
    minimo_indice_operacional_escolas =
      minimo_seguro(
        indice_carga_potencial_operacional
      ),
    maximo_indice_operacional_escolas =
      maximo_seguro(
        indice_carga_potencial_operacional
      ),
    soma_contribuicao_volume =
      soma_segura(
        contribuicao_volume
      ),
    soma_contribuicao_estrutural =
      soma_segura(
        contribuicao_estrutural
      ),
    soma_contribuicao_administrativa =
      soma_segura(
        contribuicao_administrativa
      ),
    media_score_dimensao_volume =
      media_segura(
        score_dimensao_volume
      ),
    media_score_dimensao_estrutural =
      media_segura(
        score_dimensao_estrutural
      ),
    media_score_dimensao_administrativa =
      media_segura(
        score_dimensao_administrativa
      ),
    escolas_complexidade_administrativa =
      sum(
        possui_complexidade_administrativa,
        na.rm = TRUE
      ),
    escolas_leitura_operacional_cautelosa =
      sum(
        exige_leitura_operacional_cautelosa,
        na.rm = TRUE
      ),
    escolas_divergencia_vinculo_gerencial =
      sum(
        coalesce(
          assessora_vinculo_administrativo,
          "<NA>"
        ) !=
          coalesce(
            assessora_gerencial_2026,
            "<NA>"
          )
      ),
    participacao_maior_contribuicao_escola_pct =
      participacao_maiores(
        indice_carga_potencial_operacional,
        1L
      ),
    participacao_duas_maiores_contribuicoes_pct =
      participacao_maiores(
        indice_carga_potencial_operacional,
        2L
      ),
    hhi_concentracao_escore_operacional =
      calcular_hhi(
        indice_carga_potencial_operacional
      ),
    .groups = "drop"
  ) |>
  mutate(
    participacao_volume_soma_escore_pct =
      if_else(
        soma_indice_operacional_carteira > 0,
        100 *
          soma_contribuicao_volume /
          soma_indice_operacional_carteira,
        NA_real_
      ),
    participacao_estrutural_soma_escore_pct =
      if_else(
        soma_indice_operacional_carteira > 0,
        100 *
          soma_contribuicao_estrutural /
          soma_indice_operacional_carteira,
        NA_real_
      ),
    participacao_administrativa_soma_escore_pct =
      if_else(
        soma_indice_operacional_carteira > 0,
        100 *
          soma_contribuicao_administrativa /
          soma_indice_operacional_carteira,
        NA_real_
      ),
    nota_interpretacao = paste(
      "A soma do índice operacional é um agregado relativo dos escores",
      "escolares; não mede integralmente horas, deslocamentos, eventos",
      "emergenciais, vínculo qualitativo ou toda a carga real."
    )
  ) |>
  arrange(
    assessora_gerencial_2026
  )

# -------------------------------------------------------------------
# 11. Composição operacional em formato longo
# -------------------------------------------------------------------

composicao_operacional_carteiras <- analise_carteiras |>
  select(
    assessora_gerencial_2026,
    numero_escolas,
    starts_with(
      "soma_contribuicao_"
    ),
    starts_with(
      "media_score_dimensao_"
    ),
    starts_with(
      "participacao_"
    )
  ) |>
  select(
    assessora_gerencial_2026,
    numero_escolas,
    soma_contribuicao_volume,
    soma_contribuicao_estrutural,
    soma_contribuicao_administrativa,
    media_score_dimensao_volume,
    media_score_dimensao_estrutural,
    media_score_dimensao_administrativa,
    participacao_volume_soma_escore_pct,
    participacao_estrutural_soma_escore_pct,
    participacao_administrativa_soma_escore_pct
  ) |>
  pivot_longer(
    cols = -c(
      assessora_gerencial_2026,
      numero_escolas
    ),
    names_to = "metrica",
    values_to = "valor"
  ) |>
  mutate(
    dimensao = case_when(
      str_detect(
        metrica,
        "volume"
      ) ~ "volume",
      str_detect(
        metrica,
        "estrutural"
      ) ~ "estrutura",
      str_detect(
        metrica,
        "administrativa"
      ) ~ "complexidade administrativa",
      TRUE ~ NA_character_
    ),
    tipo_metrica = case_when(
      str_starts(
        metrica,
        "soma_contribuicao"
      ) ~ "soma da contribuição no índice",
      str_starts(
        metrica,
        "media_score"
      ) ~ "média do escore dimensional",
      str_starts(
        metrica,
        "participacao"
      ) ~ "participação na soma do escore",
      TRUE ~ "outra"
    ),
    peso_dimensao_indice = case_when(
      dimensao == "volume" ~ .40,
      dimensao == "estrutura" ~ .35,
      dimensao ==
        "complexidade administrativa" ~ .25,
      TRUE ~ NA_real_
    ),
    uso_no_indice_operacional = TRUE,
    nota_metodologica = paste(
      "Decomposição do índice operacional homologado;",
      "não inclui resultados educacionais."
    )
  ) |>
  arrange(
    assessora_gerencial_2026,
    factor(
      dimensao,
      levels = c(
        "volume",
        "estrutura",
        "complexidade administrativa"
      )
    ),
    tipo_metrica
  )

# -------------------------------------------------------------------
# 12. Diagnóstico educacional contextual por carteira
# -------------------------------------------------------------------

diagnostico_educacional_carteiras <- diagnostico_educacional |>
  group_by(
    assessora_gerencial_2026
  ) |>
  summarise(
    numero_escolas_diagnostico =
      n_distinct(
        id_escola
      ),
    numero_linhas_escola_serie =
      n(),
    series_com_previstos =
      sum(
        !is.na(
          previstos_2026
        )
      ),
    series_com_avaliados =
      sum(
        !is.na(
          avaliados_2026
        )
      ),
    previstos_total_2026 =
      soma_segura(
        previstos_2026
      ),
    avaliados_total_2026 =
      soma_segura(
        avaliados_2026
      ),
    taxa_participacao_agregada_2026 =
      if_else(
        !is.na(
          previstos_total_2026
        ) &
          previstos_total_2026 > 0,
        100 *
          avaliados_total_2026 /
          previstos_total_2026,
        NA_real_
      ),
    proficiencia_2026_ponderada_avaliados =
      media_ponderada_segura(
        proficiencia_media_2026,
        avaliados_2026
      ),
    pct_defasagem_2026_ponderado_avaliados =
      media_ponderada_segura(
        pct_defasagem_2026,
        avaliados_2026
      ),
    pct_intermediario_2026_ponderado_avaliados =
      media_ponderada_segura(
        pct_intermediario_2026,
        avaliados_2026
      ),
    pct_adequado_2026_ponderado_avaliados =
      media_ponderada_segura(
        pct_adequado_2026,
        avaliados_2026
      ),
    series_painel_balanceado =
      sum(
        coalesce(
          painel_resultado_balanceado,
          FALSE
        )
      ),
    series_alerta_composicao =
      sum(
        coalesce(
          alerta_composicao_serie,
          FALSE
        )
      ),
    series_aumento_participacao_10pp =
      sum(
        coalesce(
          aumento_participacao_10pp,
          FALSE
        )
      ),
    series_queda_participacao_10pp =
      sum(
        coalesce(
          queda_participacao_10pp,
          FALSE
        )
      ),
    series_mudanca_previstos_20pct =
      sum(
        coalesce(
          mudanca_previstos_20pct,
          FALSE
        )
      ),
    menor_proficiencia_observada_2026 =
      minimo_seguro(
        proficiencia_media_2026
      ),
    maior_pct_defasagem_observado_2026 =
      maximo_seguro(
        pct_defasagem_2026
      ),
    peso_maximo_no_indice_operacional =
      maximo_seguro(
        peso_no_indice_carga_operacional
      ),
    linhas_marcadas_uso_no_indice =
      sum(
        coalesce(
          uso_no_indice_carga_operacional,
          FALSE
        )
      ),
    .groups = "drop"
  ) |>
  mutate(
    alerta_participacao_composicao =
      series_alerta_composicao > 0 |
      series_queda_participacao_10pp > 0 |
      series_mudanca_previstos_20pct > 0,
    leitura_menor_proficiencia = paste(
      "A menor proficiência observada pode sinalizar demanda pedagógica",
      "adicional, mas não representa efeito, qualidade ou responsabilidade",
      "da assessora."
    ),
    natureza = paste(
      "Diagnóstico educacional contextual, descritivo e não causal;",
      "peso zero no índice operacional."
    ),
    advertencia = paste(
      "Comparações devem considerar participação, composição, cobertura",
      "e heterogeneidade entre escolas e séries."
    )
  ) |>
  arrange(
    assessora_gerencial_2026
  )

# -------------------------------------------------------------------
# 13. Parâmetros e dicionário
# -------------------------------------------------------------------

parametros_analise <- tribble(
  ~parametro, ~valor, ~justificativa,
  "universo_institucional", "56 escolas", "Preserva todas as escolas do cadastro e do perfil homologado.",
  "universo_operacional", "53 escolas", "Somente escolas com inclusão homologada no índice de carga de 2026.",
  "carteiras_gerenciais", "11 assessoras", "Uso exclusivo de assessora_gerencial_2026.",
  "exclusoes_operacionais", "ESC_001; ESC_055; ESC_056", "Escolas preservadas institucionalmente, mas fora da carga operacional.",
  "indice_operacional", "produto homologado do módulo 17", "O módulo 18 não recalcula o índice.",
  "peso_volume", "0,40", "Peso fixado e homologado no módulo 17.",
  "peso_estrutura", "0,35", "Peso fixado e homologado no módulo 17.",
  "peso_administracao", "0,25", "Peso fixado e homologado no módulo 17.",
  "peso_resultados_educacionais", "0", "Resultados educacionais permanecem em diagnóstico paralelo.",
  "ranking", "não produzido", "O módulo não cria ordem, faixa ou percentil de carteiras ou escolas.",
  "soma_escore_carteira", "agregado relativo", "Não representa medida total ou definitiva da carga real."
)

descricoes_dicionario <- c(
  assessora_gerencial_2026 =
    "Assessora utilizada para a carteira gerencial de 2026.",
  assessora_vinculo_administrativo =
    "Vínculo administrativo preservado separadamente; não comprova exposição nem substitui a assessora gerencial.",
  numero_escolas =
    "Quantidade de escolas elegíveis incluídas na carteira operacional.",
  soma_indice_operacional_carteira =
    "Soma dos índices operacionais das escolas da carteira; agregado relativo, não carga real total.",
  media_indice_operacional_escolas =
    "Média simples do índice operacional das escolas da carteira.",
  mediana_indice_operacional_escolas =
    "Mediana do índice operacional das escolas da carteira.",
  contribuicao_escola_soma_indice_carteira_pct =
    "Participação da escola na soma dos escores operacionais da carteira.",
  hhi_concentracao_escore_operacional =
    "HHI das participações dos escores escolares; descreve concentração do escore relativo.",
  diagnostico_educacional_carteiras =
    "Síntese contextual com peso zero no índice operacional.",
  incluida_analise_operacional =
    "Indica inclusão da escola no universo operacional de 53 escolas."
)

bases_para_dicionario <- list(
  analise_carteiras_assessoras =
    analise_carteiras,
  carteira_escola_detalhe =
    carteira_escola_detalhe,
  diagnostico_educacional_carteiras =
    diagnostico_educacional_carteiras,
  composicao_operacional_carteiras =
    composicao_operacional_carteiras,
  universo_institucional_carteiras =
    universo_institucional_carteiras
)

dicionario_analise <- imap_dfr(
  bases_para_dicionario,
  function(dados, produto) {
    tibble(
      produto = produto,
      ordem_coluna = seq_along(dados),
      variavel = names(dados),
      classe_r = map_chr(
        dados,
        ~ paste(
          class(.x),
          collapse = " | "
        )
      )
    ) |>
      mutate(
        descricao = map_chr(
          variavel,
          function(nome_variavel) {
            descricao <- unname(
              descricoes_dicionario[
                nome_variavel
              ]
            )

            if (
              length(descricao) == 0L ||
                is.na(descricao[[1]]) ||
                !nzchar(
                  descricao[[1]]
                )
            ) {
              paste0(
                "Variável ",
                str_replace_all(
                  nome_variavel,
                  "_",
                  " "
                ),
                ". Consultar o script canônico do módulo 18."
              )
            } else {
              descricao[[1]]
            }
          }
        ),
        unidade_analise = case_when(
          produto ==
            "analise_carteiras_assessoras" ~
            "assessora gerencial de 2026",
          produto ==
            "carteira_escola_detalhe" ~
            "escola elegível",
          produto ==
            "diagnostico_educacional_carteiras" ~
            "assessora gerencial de 2026",
          produto ==
            "composicao_operacional_carteiras" ~
            "assessora × dimensão × métrica",
          produto ==
            "universo_institucional_carteiras" ~
            "escola institucional",
          TRUE ~ "não informado"
        ),
        observacao_metodologica = case_when(
          str_detect(
            variavel,
            "proficiencia|defasagem|adequado|intermediario"
          ) ~ paste(
            "Resultado educacional descritivo;",
            "não representa efeito ou qualidade da assessora."
          ),
          str_detect(
            variavel,
            "indice|escore|contribuicao|hhi"
          ) ~ paste(
            "Medida relativa do índice operacional;",
            "não equivale à carga real total."
          ),
          str_detect(
            variavel,
            "assessora_vinculo_administrativo"
          ) ~ paste(
            "Vínculo administrativo separado da",
            "assessora gerencial de 2026."
          ),
          TRUE ~ paste(
            "Usar em conjunto com as demais dimensões",
            "e com validação qualitativa."
          )
        )
      )
  }
)

# -------------------------------------------------------------------
# 14. Validações finais bloqueantes
# -------------------------------------------------------------------

soma_por_carteira <- analise_carteiras |>
  summarise(
    escolas = sum(
      numero_escolas
    ),
    indice = sum(
      soma_indice_operacional_carteira
    ),
    volume = sum(
      soma_contribuicao_volume
    ),
    estrutura = sum(
      soma_contribuicao_estrutural
    ),
    administrativa = sum(
      soma_contribuicao_administrativa
    )
  )

soma_indice_entrada <- sum(
  indice_escola$
    indice_carga_potencial_operacional
)

soma_componentes_entrada <- sum(
  indice_escola$
    contribuicao_volume +
    indice_escola$
      contribuicao_estrutural +
    indice_escola$
      contribuicao_administrativa
)

numero_assessoras <- n_distinct(
  indice_escola$
    assessora_gerencial_2026
)

termos_proibidos_novos <- c(
  "percentil_carga",
  "faixa_carga",
  "ordem_carga",
  "ranking",
  "cenario",
  "indice_carga_potencial_equilibrado",
  "indice_carga_potencial_desafio_educacional",
  "indice_carga_potencial_transicao_administrativa",
  "score_dimensao_educacional"
)

texto_produtos_operacionais <- paste(
  names(analise_carteiras),
  names(carteira_escola_detalhe),
  names(composicao_operacional_carteiras),
  collapse = " | "
)

ocorrencias_termos_proibidos <- sum(
  map_lgl(
    termos_proibidos_novos,
    ~ str_detect(
      texto_produtos_operacionais,
      fixed(
        .x,
        ignore_case = TRUE
      )
    )
  )
)

validacoes <- bind_rows(
  registrar_validacao(
    "Branch correta",
    "execucao",
    "erro",
    branch_observada,
    branch_esperada,
    branch_observada == branch_esperada,
    "Bloqueio do ambiente Git."
  ),
  registrar_validacao(
    "HEAD homologado",
    "execucao",
    "erro",
    commit_observado,
    commit_base_integracao,
    commit_observado == commit_base_integracao,
    "Execução antes de qualquer alteração do módulo 18."
  ),
  registrar_validacao(
    "Caminho canônico do script",
    "execucao",
    "erro",
    caminho_script_normalizado,
    caminho_canonico_esperado,
    identical(
      caminho_script_normalizado,
      caminho_canonico_esperado
    ),
    "Variantes corrigidas não devem ser executadas."
  ),
  registrar_validacao(
    "Equivalência CSV–RDS das entradas",
    "integridade",
    "erro",
    sum(
      diagnostico_equivalencia_entradas$aprovado
    ),
    "3 de 3 fontes equivalentes",
    all(
      diagnostico_equivalencia_entradas$aprovado
    ),
    "Perfil, índice e diagnóstico educacional."
  ),
  registrar_validacao(
    "Perfil institucional com 56 escolas",
    "universo",
    "erro",
    nrow(perfil_escola),
    "56",
    nrow(perfil_escola) == 56L,
    "Universo institucional oficial."
  ),
  registrar_validacao(
    "Índice operacional com 53 escolas",
    "universo",
    "erro",
    nrow(indice_escola),
    "53",
    nrow(indice_escola) == 53L,
    "Universo operacional oficial."
  ),
  registrar_validacao(
    "Diagnóstico educacional com 265 linhas",
    "universo",
    "erro",
    nrow(diagnostico_educacional),
    "265",
    nrow(diagnostico_educacional) == 265L,
    "53 escolas × cinco séries."
  ),
  registrar_validacao(
    "Exclusões operacionais exatas",
    "universo",
    "erro",
    paste(
      ids_diferenca,
      collapse = "; "
    ),
    paste(
      ids_excluidos_carga,
      collapse = "; "
    ),
    setequal(
      ids_diferenca,
      ids_excluidos_carga
    ),
    "Nenhuma outra escola pode ser excluída."
  ),
  registrar_validacao(
    "Onze assessoras gerenciais",
    "universo",
    "erro",
    numero_assessoras,
    "11",
    numero_assessoras == 11L,
    "Carteiras formadas por assessora_gerencial_2026."
  ),
  registrar_validacao(
    "Síntese com 11 linhas",
    "produto",
    "erro",
    nrow(analise_carteiras),
    "11",
    nrow(analise_carteiras) == 11L,
    "Uma linha por assessora."
  ),
  registrar_validacao(
    "Detalhe com 53 linhas",
    "produto",
    "erro",
    nrow(carteira_escola_detalhe),
    "53",
    nrow(carteira_escola_detalhe) == 53L,
    "Uma linha por escola elegível."
  ),
  registrar_validacao(
    "Universo institucional com 56 linhas",
    "produto",
    "erro",
    nrow(universo_institucional_carteiras),
    "56",
    nrow(universo_institucional_carteiras) == 56L,
    "Inclui as três escolas não elegíveis."
  ),
  registrar_validacao(
    "Composição operacional com 99 linhas",
    "produto",
    "erro",
    nrow(composicao_operacional_carteiras),
    "99",
    nrow(composicao_operacional_carteiras) == 99L,
    "11 assessoras × três dimensões × três métricas."
  ),
  registrar_validacao(
    "Diagnóstico educacional com 11 linhas",
    "produto",
    "erro",
    nrow(diagnostico_educacional_carteiras),
    "11",
    nrow(diagnostico_educacional_carteiras) == 11L,
    "Uma linha contextual por assessora."
  ),
  registrar_validacao(
    "Soma das escolas das carteiras",
    "consistencia",
    "erro",
    soma_por_carteira$escolas,
    "53",
    soma_por_carteira$escolas == 53L,
    "Recomposição do universo operacional."
  ),
  registrar_validacao(
    "Índice recomposto por contribuições",
    "metodologia",
    "erro",
    max(
      base_escola_carteira$
        diferenca_recomposicao_indice
    ),
    "diferença máxima < 1e-10",
    all(
      base_escola_carteira$
        diferenca_recomposicao_indice <
        1e-10
    ),
    "Pesos 40%, 35% e 25% já homologados."
  ),
  registrar_validacao(
    "Soma do índice preservada",
    "consistencia",
    "erro",
    soma_por_carteira$indice,
    as.character(
      soma_indice_entrada
    ),
    isTRUE(
      all.equal(
        soma_por_carteira$indice,
        soma_indice_entrada,
        tolerance = 1e-10
      )
    ),
    "Agregação não altera os escores escolares."
  ),
  registrar_validacao(
    "Soma dos componentes preservada",
    "consistencia",
    "erro",
    soma_componentes_entrada,
    as.character(
      soma_indice_entrada
    ),
    isTRUE(
      all.equal(
        soma_componentes_entrada,
        soma_indice_entrada,
        tolerance = 1e-10
      )
    ),
    "Contribuições recompõem o índice operacional."
  ),
  registrar_validacao(
    "Resultados educacionais fora do índice",
    "metodologia",
    "erro",
    sum(
      indice_escola$
        resultados_educacionais_no_indice
    ),
    "zero TRUE",
    !any(
      indice_escola$
        resultados_educacionais_no_indice
    ),
    "Trava herdada do módulo 17."
  ),
  registrar_validacao(
    "Diagnóstico educacional com peso zero",
    "metodologia",
    "erro",
    max(
      diagnostico_educacional$
        peso_no_indice_carga_operacional
    ),
    "zero",
    all(
      diagnostico_educacional$
        peso_no_indice_carga_operacional ==
        0
    ),
    "Produto paralelo."
  ),
  registrar_validacao(
    "Diagnóstico educacional não usado no índice",
    "metodologia",
    "erro",
    sum(
      diagnostico_educacional$
        uso_no_indice_carga_operacional
    ),
    "zero TRUE",
    !any(
      diagnostico_educacional$
        uso_no_indice_carga_operacional
    ),
    "Produto paralelo."
  ),
  registrar_validacao(
    "Ausência de novos rankings, faixas e cenários",
    "metodologia",
    "erro",
    ocorrencias_termos_proibidos,
    "zero ocorrências",
    ocorrencias_termos_proibidos == 0L,
    "Nenhuma ordenação gerencial nova."
  ),
  registrar_validacao(
    "Vínculos preservados separadamente",
    "metodologia",
    "erro",
    all(
      c(
        "assessora_vinculo_administrativo",
        "assessora_gerencial_2026"
      ) %in%
        names(
          carteira_escola_detalhe
        )
    ),
    "duas variáveis presentes",
    all(
      c(
        "assessora_vinculo_administrativo",
        "assessora_gerencial_2026"
      ) %in%
        names(
          carteira_escola_detalhe
        )
    ),
    "Nenhuma variável é inferida da outra."
  ),
  registrar_validacao(
    "Nenhuma categoria residual operacional",
    "universo",
    "erro",
    sum(
      str_to_lower(
        str_squish(
          indice_escola$
            assessora_gerencial_2026
        )
      ) %in%
        categorias_residuais_proibidas
    ),
    "zero",
    !any(
      str_to_lower(
        str_squish(
          indice_escola$
            assessora_gerencial_2026
        )
      ) %in%
        categorias_residuais_proibidas
    ),
    "Somente 11 carteiras nominais."
  )
)

erros_criticos <- validacoes |>
  filter(
    !resultado &
      severidade == "erro"
  )

# -------------------------------------------------------------------
# 15. Diagnósticos da execução
# -------------------------------------------------------------------

manifesto_entradas <- imap_dfr(
  arquivos_entrada,
  ~ inventariar_arquivo(
    .y,
    .x
  )
) |>
  mutate(
    md5_esperado = hashes_esperados[
      arquivo
    ],
    hash_aprovado = str_to_lower(md5) ==
      str_to_lower(md5_esperado)
  )

estrutura_saidas <- imap_dfr(
  bases_para_dicionario,
  ~ estrutura_base(
    .x,
    .y
  )
)

write_csv(
  parametros_analise,
  file.path(
    pasta_execucao,
    "01_parametros_analise_carteiras.csv"
  ),
  na = ""
)

write_csv(
  manifesto_entradas,
  file.path(
    pasta_execucao,
    "02_manifesto_arquivos_entrada.csv"
  ),
  na = ""
)

write_csv(
  diagnostico_equivalencia_entradas,
  file.path(
    pasta_execucao,
    "03_equivalencia_csv_rds_entradas.csv"
  ),
  na = ""
)

write_csv(
  universo_institucional_carteiras,
  file.path(
    pasta_execucao,
    "04_universo_institucional_56_escolas.csv"
  ),
  na = ""
)

write_csv(
  analise_carteiras,
  file.path(
    pasta_execucao,
    "05_sintese_operacional_carteiras.csv"
  ),
  na = ""
)

write_csv(
  carteira_escola_detalhe,
  file.path(
    pasta_execucao,
    "06_detalhe_escola_carteira.csv"
  ),
  na = ""
)

write_csv(
  composicao_operacional_carteiras,
  file.path(
    pasta_execucao,
    "07_composicao_operacional_carteiras.csv"
  ),
  na = ""
)

write_csv(
  diagnostico_educacional_carteiras,
  file.path(
    pasta_execucao,
    "08_diagnostico_educacional_contextual.csv"
  ),
  na = ""
)

write_csv(
  validacoes,
  file.path(
    pasta_execucao,
    "09_validacao_final.csv"
  ),
  na = ""
)

write_csv(
  estrutura_saidas,
  file.path(
    pasta_execucao,
    "10_estrutura_bases_saida.csv"
  ),
  na = ""
)

write_csv(
  dicionario_analise,
  file.path(
    pasta_execucao,
    "11_dicionario_candidato.csv"
  ),
  na = ""
)

write_lines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    pasta_execucao,
    "12_session_info.txt"
  )
)

informacoes_execucao <- tibble(
  campo = c(
    "id_execucao",
    "instante_execucao",
    "branch",
    "commit_base",
    "caminho_script",
    "md5_script",
    "perfil_institucional",
    "escolas_operacionais",
    "assessoras_gerenciais",
    "exclusoes_operacionais"
  ),
  valor = c(
    id_execucao,
    format(
      instante_execucao,
      "%Y-%m-%d %H:%M:%S %z"
    ),
    branch_observada,
    commit_observado,
    normalizar_caminho(
      caminho_script
    ),
    hash_md5(
      caminho_script
    ),
    as.character(
      nrow(perfil_escola)
    ),
    as.character(
      nrow(indice_escola)
    ),
    as.character(
      numero_assessoras
    ),
    paste(
      ids_excluidos_carga,
      collapse = "; "
    )
  )
)

write_csv(
  informacoes_execucao,
  file.path(
    pasta_execucao,
    "13_identificacao_execucao.csv"
  ),
  na = ""
)

linhas_resumo <- c(
  paste0(
    "Execução: ",
    id_execucao
  ),
  paste0(
    "Branch: ",
    branch_observada
  ),
  paste0(
    "Commit-base: ",
    commit_observado
  ),
  paste0(
    "MD5 do script: ",
    hash_md5(
      caminho_script
    )
  ),
  paste0(
    "Universo institucional: ",
    nrow(perfil_escola),
    " escolas"
  ),
  paste0(
    "Universo operacional: ",
    nrow(indice_escola),
    " escolas"
  ),
  paste0(
    "Assessoras gerenciais: ",
    numero_assessoras
  ),
  paste0(
    "Exclusões: ",
    paste(
      ids_excluidos_carga,
      collapse = ", "
    )
  ),
  paste0(
    "Erros críticos: ",
    nrow(erros_criticos)
  ),
  "",
  "Observações metodológicas:",
  "- O módulo descreve carteiras gerenciais; não avalia assessoras.",
  "- O índice operacional foi recebido do módulo 17 e não foi recalculado.",
  "- Volume, estrutura e complexidade administrativa permanecem separados.",
  "- Resultados educacionais têm peso zero e ficam em produto paralelo.",
  "- Não foram criados ranking, percentil, faixa ou cenário de carteira.",
  "- A soma do índice é um agregado relativo, não medida completa da carga real.",
  "- ESC_001, ESC_055 e ESC_056 permanecem apenas no universo institucional."
)

write_lines(
  linhas_resumo,
  file.path(
    pasta_execucao,
    "14_resumo_execucao.txt"
  )
)

if (nrow(erros_criticos) > 0L) {
  stop(
    "O módulo 18 encontrou ",
    nrow(erros_criticos),
    " erro(s) crítico(s). Consulte `09_validacao_final.csv` em: ",
    pasta_execucao
  )
}

# -------------------------------------------------------------------
# 16. Escrita dos candidatos
# -------------------------------------------------------------------

caminhos_candidatos <- file.path(
  pasta_candidatos,
  basename(
    arquivos_saida
  )
)

names(caminhos_candidatos) <- names(
  arquivos_saida
)

write_csv(
  analise_carteiras,
  caminhos_candidatos[[
    "analise_carteiras_csv"
  ]],
  na = ""
)

saveRDS(
  analise_carteiras,
  caminhos_candidatos[[
    "analise_carteiras_rds"
  ]]
)

write_csv(
  carteira_escola_detalhe,
  caminhos_candidatos[[
    "detalhe_escolas_csv"
  ]],
  na = ""
)

saveRDS(
  carteira_escola_detalhe,
  caminhos_candidatos[[
    "detalhe_escolas_rds"
  ]]
)

write_csv(
  diagnostico_educacional_carteiras,
  caminhos_candidatos[[
    "diagnostico_carteiras_csv"
  ]],
  na = ""
)

saveRDS(
  diagnostico_educacional_carteiras,
  caminhos_candidatos[[
    "diagnostico_carteiras_rds"
  ]]
)

write_csv(
  composicao_operacional_carteiras,
  caminhos_candidatos[[
    "composicao_operacional_csv"
  ]],
  na = ""
)

saveRDS(
  composicao_operacional_carteiras,
  caminhos_candidatos[[
    "composicao_operacional_rds"
  ]]
)

write_csv(
  universo_institucional_carteiras,
  caminhos_candidatos[[
    "universo_institucional_csv"
  ]],
  na = ""
)

saveRDS(
  universo_institucional_carteiras,
  caminhos_candidatos[[
    "universo_institucional_rds"
  ]]
)

write_csv(
  dicionario_analise,
  caminhos_candidatos[[
    "dicionario_csv"
  ]],
  na = ""
)

# -------------------------------------------------------------------
# 17. Releitura e equivalência dos candidatos
# -------------------------------------------------------------------

validar_par_candidato <- function(
    nome_csv,
    nome_rds,
    chaves,
    fonte
) {
  caminho_csv <- caminhos_candidatos[[
    nome_csv
  ]]

  caminho_rds <- caminhos_candidatos[[
    nome_rds
  ]]

  modelo <- readRDS(
    caminho_rds
  )

  csv <- ler_csv_por_modelo(
    caminho_csv,
    modelo
  )

  comparar_bases_semanticamente(
    modelo,
    csv,
    chaves,
    fonte
  )
}

par_analise <- validar_par_candidato(
  "analise_carteiras_csv",
  "analise_carteiras_rds",
  c(
    "assessora_gerencial_2026"
  ),
  "analise_carteiras_assessoras"
)

par_detalhe <- validar_par_candidato(
  "detalhe_escolas_csv",
  "detalhe_escolas_rds",
  c(
    "assessora_gerencial_2026",
    "id_escola"
  ),
  "carteira_escola_detalhe"
)

par_diagnostico <- validar_par_candidato(
  "diagnostico_carteiras_csv",
  "diagnostico_carteiras_rds",
  c(
    "assessora_gerencial_2026"
  ),
  "diagnostico_educacional_carteiras"
)

par_composicao <- validar_par_candidato(
  "composicao_operacional_csv",
  "composicao_operacional_rds",
  c(
    "assessora_gerencial_2026",
    "dimensao",
    "tipo_metrica"
  ),
  "composicao_operacional_carteiras"
)

par_universo <- validar_par_candidato(
  "universo_institucional_csv",
  "universo_institucional_rds",
  c(
    "id_escola"
  ),
  "universo_institucional_carteiras"
)

equivalencia_candidatos <- bind_rows(
  par_analise$diagnostico,
  par_detalhe$diagnostico,
  par_diagnostico$diagnostico,
  par_composicao$diagnostico,
  par_universo$diagnostico
)

write_csv(
  equivalencia_candidatos,
  file.path(
    pasta_execucao,
    "15_equivalencia_csv_rds_candidatos.csv"
  ),
  na = ""
)

if (
  any(
    !equivalencia_candidatos$aprovado
  )
) {
  stop(
    "Há divergência entre CSV e RDS dos produtos candidatos."
  )
}

manifesto_candidatos <- imap_dfr(
  caminhos_candidatos,
  ~ inventariar_arquivo(
    .y,
    .x
  )
)

write_csv(
  manifesto_candidatos,
  file.path(
    pasta_execucao,
    "16_manifesto_produtos_candidatos.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 18. Preservação histórica dos produtos anteriores
# -------------------------------------------------------------------

produtos_anteriores_existentes <- arquivos_saida[
  file.exists(
    arquivos_saida
  )
]

if (
  length(
    produtos_anteriores_existentes
  ) > 0L
) {
  walk2(
    produtos_anteriores_existentes,
    names(
      produtos_anteriores_existentes
    ),
    function(origem, nome) {
      destino <- file.path(
        pasta_historico,
        basename(origem)
      )

      copiar_com_validacao(
        origem,
        destino,
        sobrescrever = FALSE
      )
    }
  )
}

manifesto_historico <- if (
  length(
    produtos_anteriores_existentes
  ) > 0L
) {
  imap_dfr(
    produtos_anteriores_existentes,
    function(caminho, nome) {
      inventariar_arquivo(
        nome,
        file.path(
          pasta_historico,
          basename(caminho)
        )
      )
    }
  )
} else {
  tibble(
    arquivo = character(),
    caminho = character(),
    existe = logical(),
    tamanho_bytes = double(),
    md5 = character()
  )
}

write_csv(
  manifesto_historico,
  file.path(
    pasta_execucao,
    "17_manifesto_preservacao_historica.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 19. Promoção transacional com rollback
# -------------------------------------------------------------------

promovidos <- character()
rollback_preparado <- character()

tryCatch(
  {
    for (
      nome in names(
        arquivos_saida
      )
    ) {
      destino <- arquivos_saida[[nome]]
      candidato <- caminhos_candidatos[[nome]]

      if (file.exists(destino)) {
        rollback <- file.path(
          pasta_rollback,
          basename(destino)
        )

        copiar_com_validacao(
          destino,
          rollback,
          sobrescrever = FALSE
        )

        rollback_preparado <- c(
          rollback_preparado,
          nome
        )
      }

      dir.create(
        dirname(destino),
        recursive = TRUE,
        showWarnings = FALSE
      )

      sucesso <- file.copy(
        candidato,
        destino,
        overwrite = TRUE,
        copy.mode = TRUE,
        copy.date = TRUE
      )

      if (!sucesso) {
        stop(
          "Falha na promoção do produto `",
          nome,
          "`."
        )
      }

      if (
        !identical(
          hash_md5(candidato),
          hash_md5(destino)
        )
      ) {
        stop(
          "Hash divergente após promoção de `",
          nome,
          "`."
        )
      }

      promovidos <- c(
        promovidos,
        nome
      )
    }
  },
  error = function(e) {
    for (
      nome in rev(
        promovidos
      )
    ) {
      destino <- arquivos_saida[[nome]]
      rollback <- file.path(
        pasta_rollback,
        basename(destino)
      )

      if (file.exists(rollback)) {
        file.copy(
          rollback,
          destino,
          overwrite = TRUE,
          copy.mode = TRUE,
          copy.date = TRUE
        )
      } else if (file.exists(destino)) {
        file.remove(destino)
      }
    }

    stop(
      "Promoção transacional falhou; rollback executado. Motivo: ",
      conditionMessage(e)
    )
  }
)

# -------------------------------------------------------------------
# 20. Verificação final e manifesto dos produtos promovidos
# -------------------------------------------------------------------

manifesto_produtos <- imap_dfr(
  arquivos_saida,
  ~ inventariar_arquivo(
    .y,
    .x
  )
)

hashes_candidatos <- manifesto_candidatos |>
  select(
    arquivo,
    md5_candidato = md5
  )

verificacao_promocao <- manifesto_produtos |>
  left_join(
    hashes_candidatos,
    by = "arquivo"
  ) |>
  mutate(
    hash_promovido_igual_candidato =
      str_to_lower(md5) ==
      str_to_lower(md5_candidato)
  )

if (
  any(
    !verificacao_promocao$
      hash_promovido_igual_candidato
  )
) {
  stop(
    "A verificação final encontrou hash promovido divergente."
  )
}

write_csv(
  manifesto_produtos,
  file.path(
    pasta_execucao,
    "18_manifesto_produtos_modulo_18.csv"
  ),
  na = ""
)

write_csv(
  verificacao_promocao,
  file.path(
    pasta_execucao,
    "19_verificacao_promocao.csv"
  ),
  na = ""
)

message(
  "Módulo 18 concluído com sucesso.\n",
  "Execução: ",
  id_execucao,
  "\n",
  "Universo institucional: 56 escolas\n",
  "Universo operacional: 53 escolas\n",
  "Assessoras gerenciais: 11\n",
  "Exclusões: ESC_001, ESC_055, ESC_056\n",
  "Diagnósticos: ",
  pasta_execucao,
  "\n",
  "MD5 do script: ",
  hash_md5(
    caminho_script
  )
)
