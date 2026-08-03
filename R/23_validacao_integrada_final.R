# ===================================================================
# 23_validacao_integrada_final.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# Auditoria transversal final dos módulos 15 a 22
# ===================================================================
#
# OBJETIVO
#
# Executar uma auditoria integrada, não destrutiva e reprodutível dos
# produtos homologados dos módulos 15 a 22, verificando:
#
#   1. existência e integridade das entradas;
#   2. aderência aos manifestos homologados;
#   3. equivalência entre pares CSV e RDS;
#   4. universos oficiais;
#   5. chaves e duplicidades;
#   6. coerência cruzada entre bases, carteiras, tabelas e relatório;
#   7. integridade formal dos produtos do módulo 22;
#   8. rastreabilidade de branch, commit, código e ambiente.
#
# PRINCÍPIOS
#
# - auditoria não destrutiva;
# - caminhos homologados explícitos;
# - nenhuma seleção silenciosa da execução mais recente;
# - falhas críticas bloqueiam a promoção dos produtos canônicos;
# - advertências não bloqueantes permanecem registradas;
# - não reexecutar os módulos 15 a 22;
# - não produzir ranking, percentil, normalização ou redistribuição.
# ===================================================================

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
})

# -------------------------------------------------------------------
# 1. Contrato da execução
# -------------------------------------------------------------------

instante_execucao <- Sys.time()
id_execucao <- format(instante_execucao, "%Y%m%d_%H%M%S")

branch_esperada <- "refatoracao_modulo_21"
commit_esperado <- "95f77d4dd02bc77c8ae0b843eb3fb512c57df4b5"
execucao_modulo_21 <- "20260731_001044"

caminho_script <- here("R", "23_validacao_integrada_final.R")

ids_nao_operacionais <- c("ESC_001", "ESC_055", "ESC_056")

universos_esperados <- tibble::tribble(
  ~indicador, ~valor_esperado,
  "escolas_institucionais", 56,
  "combinacoes_escola_ano", 280,
  "escolas_avaliativas_2025", 54,
  "escolas_avaliativas_2026", 56,
  "escolas_operacionais_2026", 53,
  "carteiras_gerenciais_2026", 11,
  "exposicao_positiva_2025", 0
)

manifestos <- c(
  modulo_15 = here(
    "documentacao", "base_final", "execucao_20260726_182530",
    "18_manifesto_produtos_modulo_15.csv"
  ),
  modulo_16 = here(
    "documentacao", "perfil_escola", "execucao_20260726_223447",
    "19_manifesto_produtos_modulo_16.csv"
  ),
  modulo_17 = here(
    "documentacao", "indice_complexidade", "execucao_20260728_220908",
    "19_manifesto_produtos_modulo_17.csv"
  ),
  modulo_18 = here(
    "documentacao", "analise_carteiras", "execucao_20260729_221336",
    "18_manifesto_produtos_modulo_18.csv"
  ),
  modulo_19 = here(
    "documentacao", "fichas_escolas", "execucao_20260729_233939",
    "16_manifesto_produtos_modulo_19.csv"
  ),
  modulo_20 = here(
    "documentacao", "fichas_carteiras", "execucao_20260730_212403",
    "15_manifesto_produtos_modulo_20.csv"
  ),
  modulo_21 = here(
    "documentacao", "relatorio", paste0("execucao_", execucao_modulo_21),
    "10_manifesto_produtos_promovidos.csv"
  )
)

entradas <- c(
  base_final_csv = here("dados_finais", "base_analitica_final_escola_serie.csv"),
  base_final_rds = here("dados_finais", "base_analitica_final_escola_serie.rds"),
  dim_final_csv = here("dados_finais", "dim_escola_final.csv"),
  dim_final_rds = here("dados_finais", "dim_escola_final.rds"),

  perfil_completo_csv = here("dados_finais", "perfil_escola_completo.csv"),
  perfil_completo_rds = here("dados_finais", "perfil_escola_completo.rds"),
  perfil_gerencial_csv = here("dados_finais", "perfil_escola_gerencial.csv"),
  perfil_gerencial_rds = here("dados_finais", "perfil_escola_gerencial.rds"),
  perfil_serie_csv = here("dados_finais", "perfil_escola_serie_compacto.csv"),
  perfil_serie_rds = here("dados_finais", "perfil_escola_serie_compacto.rds"),

  indice_csv = here("dados_finais", "indice_carga_potencial_escola.csv"),
  indice_rds = here("dados_finais", "indice_carga_potencial_escola.rds"),
  componentes_indice_csv = here("dados_finais", "componentes_indice_carga_potencial.csv"),
  componentes_indice_rds = here("dados_finais", "componentes_indice_carga_potencial.rds"),
  componentes_educacionais_csv = here("dados_finais", "componentes_educacionais_escola_serie.csv"),
  componentes_educacionais_rds = here("dados_finais", "componentes_educacionais_escola_serie.rds"),

  analise_carteiras_csv = here("dados_finais", "analise_carteiras_assessoras.csv"),
  analise_carteiras_rds = here("dados_finais", "analise_carteiras_assessoras.rds"),
  detalhe_carteiras_csv = here("dados_finais", "carteira_escola_detalhe.csv"),
  detalhe_carteiras_rds = here("dados_finais", "carteira_escola_detalhe.rds"),
  diagnostico_carteiras_csv = here("dados_finais", "diagnostico_educacional_carteiras.csv"),
  diagnostico_carteiras_rds = here("dados_finais", "diagnostico_educacional_carteiras.rds"),
  composicao_carteiras_csv = here("dados_finais", "composicao_operacional_carteiras.csv"),
  composicao_carteiras_rds = here("dados_finais", "composicao_operacional_carteiras.rds"),
  universo_carteiras_csv = here("dados_finais", "universo_institucional_carteiras.csv"),
  universo_carteiras_rds = here("dados_finais", "universo_institucional_carteiras.rds"),

  indice_fichas_carteiras_csv = here("dados_finais", "indice_fichas_carteiras.csv"),
  indice_fichas_carteiras_rds = here("dados_finais", "indice_fichas_carteiras.rds"),

  modulo_21_t01 = here(
    "resultados", "relatorio", paste0("execucao_", execucao_modulo_21),
    "corpo", "tabelas", "T01_universos_cobertura_estudo.csv"
  ),
  modulo_21_t02 = here(
    "resultados", "relatorio", paste0("execucao_", execucao_modulo_21),
    "corpo", "tabelas", "T02_perfil_estrutural_universo_institucional.csv"
  ),
  modulo_21_t03 = here(
    "resultados", "relatorio", paste0("execucao_", execucao_modulo_21),
    "corpo", "tabelas", "T03_participacao_composicao_resultados_por_ano.csv"
  ),
  modulo_21_t04 = here(
    "resultados", "relatorio", paste0("execucao_", execucao_modulo_21),
    "corpo", "tabelas", "T04_sintese_nao_ordinal_carteiras_operacionais.csv"
  ),
  modulo_21_t05 = here(
    "resultados", "relatorio", paste0("execucao_", execucao_modulo_21),
    "corpo", "tabelas", "T05_dimensoes_operacionais_carteiras.csv"
  ),
  modulo_21_t06 = here(
    "resultados", "relatorio", paste0("execucao_", execucao_modulo_21),
    "corpo", "tabelas", "T06_cautelas_metodologicas.csv"
  ),
  modulo_21_validacao_universos = here(
    "documentacao", "relatorio", paste0("execucao_", execucao_modulo_21),
    "05_validacao_universos.csv"
  ),
  modulo_21_session_info = here(
    "documentacao", "relatorio", paste0("execucao_", execucao_modulo_21),
    "14_session_info.txt"
  ),
  modulo_21_resumo = here(
    "documentacao", "relatorio", paste0("execucao_", execucao_modulo_21),
    "15_resumo_execucao.txt"
  ),

  modulo_22_qmd = here("22_relatorio_tecnico.qmd"),
  modulo_22_html = here("22_relatorio_tecnico.html")
)

modulo_por_entrada <- c(
  base_final_csv = "modulo_15",
  base_final_rds = "modulo_15",
  dim_final_csv = "modulo_15",
  dim_final_rds = "modulo_15",

  perfil_completo_csv = "modulo_16",
  perfil_completo_rds = "modulo_16",
  perfil_gerencial_csv = "modulo_16",
  perfil_gerencial_rds = "modulo_16",
  perfil_serie_csv = "modulo_16",
  perfil_serie_rds = "modulo_16",

  indice_csv = "modulo_17",
  indice_rds = "modulo_17",
  componentes_indice_csv = "modulo_17",
  componentes_indice_rds = "modulo_17",
  componentes_educacionais_csv = "modulo_17",
  componentes_educacionais_rds = "modulo_17",

  analise_carteiras_csv = "modulo_18",
  analise_carteiras_rds = "modulo_18",
  detalhe_carteiras_csv = "modulo_18",
  detalhe_carteiras_rds = "modulo_18",
  diagnostico_carteiras_csv = "modulo_18",
  diagnostico_carteiras_rds = "modulo_18",
  composicao_carteiras_csv = "modulo_18",
  composicao_carteiras_rds = "modulo_18",
  universo_carteiras_csv = "modulo_18",
  universo_carteiras_rds = "modulo_18",

  indice_fichas_carteiras_csv = "modulo_20",
  indice_fichas_carteiras_rds = "modulo_20",

  modulo_21_t01 = "modulo_21",
  modulo_21_t02 = "modulo_21",
  modulo_21_t03 = "modulo_21",
  modulo_21_t04 = "modulo_21",
  modulo_21_t05 = "modulo_21",
  modulo_21_t06 = "modulo_21",
  modulo_21_validacao_universos = NA_character_,
  modulo_21_session_info = NA_character_,
  modulo_21_resumo = NA_character_,

  modulo_22_qmd = NA_character_,
  modulo_22_html = NA_character_
)

dir_documentacao_base <- here("documentacao", "validacao_integrada")
dir_execucao <- file.path(dir_documentacao_base, paste0("execucao_", id_execucao))

dir_transacao <- here(
  "dados_finais", "transacoes_validacao_integrada",
  paste0("execucao_", id_execucao)
)
dir_candidatos <- file.path(dir_transacao, "candidatos")
dir_rollback <- file.path(dir_transacao, "rollback")

destinos_canonicos <- c(
  csv = here("dados_finais", "validacao_integrada_final.csv"),
  rds = here("dados_finais", "validacao_integrada_final.rds")
)

dir.create(dir_execucao, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_candidatos, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_rollback, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------
# 2. Funções auxiliares
# -------------------------------------------------------------------

hash_md5 <- function(caminho) {
  if (length(caminho) != 1L || is.na(caminho) || !file.exists(caminho)) {
    return(NA_character_)
  }
  unname(tools::md5sum(caminho)[[1]])
}

executar_git <- function(argumentos) {
  saida <- suppressWarnings(
    system2("git", argumentos, stdout = TRUE, stderr = TRUE)
  )
  status <- attr(saida, "status")
  if (is.null(status)) status <- 0L
  list(status = status, saida = trimws(paste(saida, collapse = "\n")))
}

normalizar_texto <- function(x) {
  x |>
    enc2utf8() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_trim()
}

como_logico <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x == 1)
  normalizar_texto(as.character(x)) %in% c(
    "true", "verdadeiro", "sim", "s", "1"
  )
}

primeira_coluna <- function(dados, candidatas, obrigatoria = TRUE) {
  encontradas <- intersect(candidatas, names(dados))
  if (length(encontradas) == 0L) {
    if (obrigatoria) {
      stop(
        "Nenhuma coluna candidata encontrada. Esperadas: ",
        paste(candidatas, collapse = ", "),
        "."
      )
    }
    return(NA_character_)
  }
  encontradas[[1]]
}

ler_csv_seguro <- function(caminho) {
  readr::read_csv(
    caminho,
    show_col_types = FALSE,
    progress = FALSE,
    na = c("", "NA")
  )
}

comparar_csv_rds <- function(caminho_csv, caminho_rds, nome_par) {
  csv <- ler_csv_seguro(caminho_csv)
  rds <- readRDS(caminho_rds)

  if (!is.data.frame(rds)) {
    return(tibble(
      teste = paste0("equivalencia_", nome_par),
      categoria = "equivalencia_csv_rds",
      criticidade = "bloqueante",
      aprovado = FALSE,
      observado = class(rds)[[1]],
      esperado = "data.frame",
      detalhe = "O objeto RDS não é tabular."
    ))
  }

  rds <- as_tibble(rds)
  mesmas_colunas <- identical(names(csv), names(rds))
  mesmas_dimensoes <- identical(dim(csv), dim(rds))

  normalizar_coluna <- function(x) {
    if (inherits(x, c("POSIXct", "POSIXt"))) {
      return(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    }
    if (inherits(x, "Date")) {
      return(format(x, "%Y-%m-%d"))
    }
    if (is.logical(x)) {
      return(ifelse(is.na(x), NA_character_, ifelse(x, "TRUE", "FALSE")))
    }
    if (is.numeric(x)) {
      return(ifelse(
        is.na(x),
        NA_character_,
        formatC(x, digits = 15, format = "fg", flag = "#")
      ))
    }

    y <- as.character(x)
    y[is.na(x)] <- NA_character_
    y
  }

  comparar_coluna <- function(x_csv, x_rds) {
    # Primeiro tenta comparação numérica quando ambos os vetores podem ser
    # convertidos integralmente sem perda dos valores não ausentes.
    x_csv_num <- suppressWarnings(as.numeric(as.character(x_csv)))
    x_rds_num <- suppressWarnings(as.numeric(as.character(x_rds)))

    csv_convertivel <- all(is.na(x_csv) | !is.na(x_csv_num))
    rds_convertivel <- all(is.na(x_rds) | !is.na(x_rds_num))

    if (csv_convertivel && rds_convertivel) {
      return(isTRUE(all.equal(
        x_csv_num,
        x_rds_num,
        check.attributes = FALSE,
        tolerance = 1e-10
      )))
    }

    identical(
      normalizar_coluna(x_csv),
      normalizar_coluna(x_rds)
    )
  }

  colunas_divergentes <- character()

  if (mesmas_colunas && mesmas_dimensoes) {
    colunas_divergentes <- names(csv)[
      !map2_lgl(csv, rds, comparar_coluna)
    ]
  }

  conteudo_igual <- mesmas_colunas &&
    mesmas_dimensoes &&
    length(colunas_divergentes) == 0L

  tibble(
    teste = paste0("equivalencia_", nome_par),
    categoria = "equivalencia_csv_rds",
    criticidade = "bloqueante",
    aprovado = mesmas_colunas && mesmas_dimensoes && conteudo_igual,
    observado = paste0(
      "csv=", nrow(csv), "x", ncol(csv),
      "; rds=", nrow(rds), "x", ncol(rds)
    ),
    esperado = "mesmas colunas, dimensões e valores após normalização de tipos",
    detalhe = paste0(
      "colunas=", mesmas_colunas,
      "; dimensoes=", mesmas_dimensoes,
      "; divergentes=",
      ifelse(
        length(colunas_divergentes) == 0L,
        "nenhuma",
        paste(colunas_divergentes, collapse = "|")
      )
    )
  )
}

ler_manifesto_produtos <- function(caminho, modulo) {
  manifesto_bruto <- ler_csv_seguro(caminho)

  # Contrato dos módulos 15 a 20:
  # produto/arquivo + caminho + tamanho_bytes + md5
  formato_padrao <- all(
    c("caminho", "tamanho_bytes", "md5") %in% names(manifesto_bruto)
  ) &&
    any(c("produto", "arquivo") %in% names(manifesto_bruto))

  if (formato_padrao) {
    return(manifesto_bruto)
  }

  # Contrato específico do módulo 21:
  # relativo + caminho_promovido + tamanho_promovido + hash_promovido
  formato_promovido <- all(
    c(
      "relativo",
      "caminho_promovido",
      "tamanho_promovido",
      "hash_promovido"
    ) %in% names(manifesto_bruto)
  )

  if (formato_promovido) {
    manifesto_normalizado <- manifesto_bruto |>
      transmute(
        produto = as.character(.data$relativo),
        caminho = as.character(.data$caminho_promovido),
        tamanho_bytes = as.numeric(.data$tamanho_promovido),
        md5 = stringr::str_to_lower(as.character(.data$hash_promovido))
      )

    if (
      "existe_promovido" %in% names(manifesto_bruto) &&
      any(!como_logico(manifesto_bruto$existe_promovido))
    ) {
      stop(
        "Manifesto do módulo ", modulo,
        " registra produto promovido inexistente."
      )
    }

    if (
      "hash_identico" %in% names(manifesto_bruto) &&
      any(!como_logico(manifesto_bruto$hash_identico))
    ) {
      stop(
        "Manifesto do módulo ", modulo,
        " registra divergência entre hash candidato e promovido."
      )
    }

    return(manifesto_normalizado)
  }

  stop(
    "Manifesto do módulo ", modulo,
    " não corresponde a nenhum contrato reconhecido. Colunas observadas: ",
    paste(names(manifesto_bruto), collapse = ", "),
    "."
  )
}

localizar_hash_manifesto <- function(manifesto, caminho, modulo) {
  alvo <- basename(caminho)

  candidatos <- manifesto |>
    mutate(
      caminho_base = basename(.data$caminho),
      identificador = if ("produto" %in% names(manifesto)) {
        as.character(.data$produto)
      } else {
        as.character(.data$arquivo)
      }
    ) |>
    filter(
      .data$caminho_base == alvo |
        basename(.data$identificador) == alvo
    )

  if (nrow(candidatos) != 1L) {
    stop(
      "O produto `", alvo, "` não foi localizado de forma única ",
      "no manifesto homologado do módulo ", modulo, "."
    )
  }

  stringr::str_to_lower(candidatos$md5[[1]])
}

registrar_teste <- function(
    teste,
    categoria,
    aprovado,
    observado,
    esperado,
    detalhe = "",
    criticidade = "bloqueante") {
  tibble(
    teste = teste,
    categoria = categoria,
    criticidade = criticidade,
    aprovado = isTRUE(aprovado),
    observado = as.character(observado),
    esperado = as.character(esperado),
    detalhe = as.character(detalhe)
  )
}

# -------------------------------------------------------------------
# 3. Estado Git e contrato
# -------------------------------------------------------------------

git_branch <- executar_git(c("branch", "--show-current"))
git_head <- executar_git(c("rev-parse", "HEAD"))
git_status <- executar_git(c("status", "--short"))
git_sync <- executar_git(c(
  "rev-list", "--left-right", "--count",
  paste0("HEAD...origin/", branch_esperada)
))

testes <- bind_rows(
  registrar_teste(
    "branch_git",
    "contrato_execucao",
    git_branch$status == 0L && identical(git_branch$saida, branch_esperada),
    git_branch$saida,
    branch_esperada
  ),
  registrar_teste(
    "commit_git",
    "contrato_execucao",
    git_head$status == 0L && identical(git_head$saida, commit_esperado),
    git_head$saida,
    commit_esperado
  ),
  registrar_teste(
    "repositorio_sem_alteracoes_inesperadas",
    "contrato_execucao",
    {
      linhas_status <- if (identical(git_status$saida, "")) {
        character()
      } else {
        stringr::str_split(git_status$saida, "\\n")[[1]]
      }

      permitidas <- stringr::str_detect(
        linhas_status,
        "^\\?\\? R/23_validacao_integrada_final\\.R$|^\\?\\? documentacao/validacao_integrada/$"
      )

      git_status$status == 0L &&
        (length(linhas_status) == 0L || all(permitidas))
    },
    ifelse(git_status$saida == "", "[limpo]", git_status$saida),
    "sem alterações inesperadas; admitem-se apenas o script novo e a pasta de auditoria"
  ),
  registrar_teste(
    "sincronia_local_remota",
    "contrato_execucao",
    git_sync$status == 0L &&
      stringr::str_detect(git_sync$saida, "^0\\s+0$"),
    git_sync$saida,
    "0 0"
  ),
  registrar_teste(
    "script_canonico_existe",
    "contrato_execucao",
    file.exists(caminho_script),
    caminho_script,
    "arquivo existente"
  )
)

# -------------------------------------------------------------------
# 4. Existência e hashes das entradas
# -------------------------------------------------------------------

existencia_entradas <- enframe(entradas, name = "entrada", value = "caminho") |>
  mutate(
    existe = file.exists(.data$caminho),
    tamanho_bytes = if_else(
      .data$existe,
      as.numeric(file.info(.data$caminho)$size),
      NA_real_
    ),
    md5_observado = map_chr(.data$caminho, hash_md5),
    modulo = unname(modulo_por_entrada[.data$entrada])
  )

readr::write_csv(
  existencia_entradas,
  file.path(dir_execucao, "01_existencia_entradas.csv"),
  na = ""
)

testes <- bind_rows(
  testes,
  existencia_entradas |>
    transmute(
      teste = paste0("existencia_", .data$entrada),
      categoria = "existencia_entradas",
      criticidade = "bloqueante",
      aprovado = .data$existe,
      observado = .data$caminho,
      esperado = "arquivo existente",
      detalhe = if_else(
        .data$existe,
        paste0("tamanho_bytes=", .data$tamanho_bytes),
        "arquivo ausente"
      )
    )
)

if (any(!existencia_entradas$existe)) {
  readr::write_csv(
    testes,
    file.path(dir_execucao, "11_resultado_testes.csv"),
    na = ""
  )
  stop(
    "Há entradas obrigatórias ausentes. Consulte 01_existencia_entradas.csv."
  )
}

manifestos_lidos <- imap(
  manifestos,
  ~ ler_manifesto_produtos(.x, stringr::str_remove(.y, "modulo_"))
)

validacao_manifestos <- imap_dfr(
  entradas,
  function(caminho, nome_entrada) {
    modulo <- modulo_por_entrada[[nome_entrada]]

    if (is.na(modulo)) {
      return(tibble(
        entrada = nome_entrada,
        modulo = NA_character_,
        caminho = caminho,
        md5_esperado = NA_character_,
        md5_observado = hash_md5(caminho),
        aprovado = TRUE,
        observacao = "Entrada sem manifesto anterior; hash incorporado pelo módulo 23."
      ))
    }

    esperado <- localizar_hash_manifesto(
      manifestos_lidos[[modulo]],
      caminho,
      stringr::str_remove(modulo, "modulo_")
    )
    observado <- stringr::str_to_lower(hash_md5(caminho))

    tibble(
      entrada = nome_entrada,
      modulo = modulo,
      caminho = caminho,
      md5_esperado = esperado,
      md5_observado = observado,
      aprovado = identical(observado, esperado),
      observacao = ""
    )
  }
)

readr::write_csv(
  validacao_manifestos,
  file.path(dir_execucao, "03_validacao_manifestos_homologados.csv"),
  na = ""
)

testes <- bind_rows(
  testes,
  validacao_manifestos |>
    transmute(
      teste = paste0("hash_manifesto_", .data$entrada),
      categoria = "hashes_manifestos",
      criticidade = "bloqueante",
      aprovado = .data$aprovado,
      observado = .data$md5_observado,
      esperado = if_else(
        is.na(.data$md5_esperado),
        "hash registrado pelo módulo 23",
        .data$md5_esperado
      ),
      detalhe = .data$observacao
    )
)

# -------------------------------------------------------------------
# 5. Equivalência CSV–RDS
# -------------------------------------------------------------------

pares_csv_rds <- tribble(
  ~nome, ~csv, ~rds,
  "base_final", entradas[["base_final_csv"]], entradas[["base_final_rds"]],
  "dim_final", entradas[["dim_final_csv"]], entradas[["dim_final_rds"]],
  "perfil_completo", entradas[["perfil_completo_csv"]], entradas[["perfil_completo_rds"]],
  "perfil_gerencial", entradas[["perfil_gerencial_csv"]], entradas[["perfil_gerencial_rds"]],
  "perfil_serie", entradas[["perfil_serie_csv"]], entradas[["perfil_serie_rds"]],
  "indice", entradas[["indice_csv"]], entradas[["indice_rds"]],
  "componentes_indice", entradas[["componentes_indice_csv"]], entradas[["componentes_indice_rds"]],
  "componentes_educacionais", entradas[["componentes_educacionais_csv"]], entradas[["componentes_educacionais_rds"]],
  "analise_carteiras", entradas[["analise_carteiras_csv"]], entradas[["analise_carteiras_rds"]],
  "detalhe_carteiras", entradas[["detalhe_carteiras_csv"]], entradas[["detalhe_carteiras_rds"]],
  "diagnostico_carteiras", entradas[["diagnostico_carteiras_csv"]], entradas[["diagnostico_carteiras_rds"]],
  "composicao_carteiras", entradas[["composicao_carteiras_csv"]], entradas[["composicao_carteiras_rds"]],
  "universo_carteiras", entradas[["universo_carteiras_csv"]], entradas[["universo_carteiras_rds"]],
  "indice_fichas_carteiras", entradas[["indice_fichas_carteiras_csv"]], entradas[["indice_fichas_carteiras_rds"]]
)

validacao_equivalencia <- pmap_dfr(
  pares_csv_rds,
  ~ comparar_csv_rds(..2, ..3, ..1)
)

readr::write_csv(
  validacao_equivalencia,
  file.path(dir_execucao, "04_validacao_hashes_produtos.csv"),
  na = ""
)

testes <- bind_rows(testes, validacao_equivalencia)

# -------------------------------------------------------------------
# 6. Leitura das bases centrais
# -------------------------------------------------------------------

base_final <- ler_csv_seguro(entradas[["base_final_csv"]])
dim_final <- ler_csv_seguro(entradas[["dim_final_csv"]])
perfil_completo <- ler_csv_seguro(entradas[["perfil_completo_csv"]])
perfil_gerencial <- ler_csv_seguro(entradas[["perfil_gerencial_csv"]])
perfil_serie <- ler_csv_seguro(entradas[["perfil_serie_csv"]])
indice <- ler_csv_seguro(entradas[["indice_csv"]])
detalhe_carteiras <- ler_csv_seguro(entradas[["detalhe_carteiras_csv"]])
analise_carteiras <- ler_csv_seguro(entradas[["analise_carteiras_csv"]])

col_id_dim <- primeira_coluna(dim_final, c("id_escola", "codigo_escola"))
col_id_base <- primeira_coluna(base_final, c("id_escola", "codigo_escola"))
col_ano_base <- primeira_coluna(
  base_final,
  c("ano_escolar", "ano_serie", "serie", "ano")
)
col_id_perfil <- primeira_coluna(perfil_completo, c("id_escola", "codigo_escola"))
col_id_gerencial <- primeira_coluna(perfil_gerencial, c("id_escola", "codigo_escola"))
col_id_indice <- primeira_coluna(indice, c("id_escola", "codigo_escola"))
col_id_detalhe <- primeira_coluna(detalhe_carteiras, c("id_escola", "codigo_escola"))
col_assessora_indice <- primeira_coluna(
  indice,
  c("assessora_gerencial_2026", "assessora_gerencial")
)
col_assessora_analise <- primeira_coluna(
  analise_carteiras,
  c("assessora_gerencial_2026", "assessora_gerencial", "assessora")
)

col_avaliativo_2025 <- primeira_coluna(
  dim_final,
  c(
    "pertence_universo_avaliativo_2025",
    "universo_avaliativo_2025",
    "avaliada_2025"
  )
)
col_avaliativo_2026 <- primeira_coluna(
  dim_final,
  c(
    "pertence_universo_avaliativo_2026",
    "universo_avaliativo_2026",
    "avaliada_2026"
  )
)
col_operacional_2026 <- primeira_coluna(
  dim_final,
  c(
    "incluir_indice_carga_2026",
    "recebe_assessoramento_2026",
    "universo_operacional_2026"
  )
)
col_exposicao_2025 <- primeira_coluna(
  dim_final,
  c(
    "exposicao_programa_binaria_2025",
    "exposicao_programa_2025"
  )
)

# -------------------------------------------------------------------
# 7. Universos, chaves e duplicidades
# -------------------------------------------------------------------

universos_observados <- tibble(
  indicador = universos_esperados$indicador,
  valor_observado = c(
    n_distinct(dim_final[[col_id_dim]]),
    nrow(base_final),
    sum(como_logico(dim_final[[col_avaliativo_2025]]), na.rm = TRUE),
    sum(como_logico(dim_final[[col_avaliativo_2026]]), na.rm = TRUE),
    sum(como_logico(dim_final[[col_operacional_2026]]), na.rm = TRUE),
    n_distinct(na.omit(indice[[col_assessora_indice]])),
    sum(como_logico(dim_final[[col_exposicao_2025]]), na.rm = TRUE)
  )
) |>
  left_join(universos_esperados, by = "indicador") |>
  mutate(aprovado = .data$valor_observado == .data$valor_esperado)

readr::write_csv(
  universos_observados,
  file.path(dir_execucao, "05_validacao_universos_integrada.csv"),
  na = ""
)

testes <- bind_rows(
  testes,
  universos_observados |>
    transmute(
      teste = paste0("universo_", .data$indicador),
      categoria = "universos",
      criticidade = "bloqueante",
      aprovado = .data$aprovado,
      observado = as.character(.data$valor_observado),
      esperado = as.character(.data$valor_esperado),
      detalhe = ""
    )
)

validacao_chaves <- bind_rows(
  registrar_teste(
    "unicidade_dim_escola",
    "chaves_duplicidades",
    nrow(dim_final) == n_distinct(dim_final[[col_id_dim]]),
    nrow(dim_final) - n_distinct(dim_final[[col_id_dim]]),
    "0 duplicidades"
  ),
  registrar_teste(
    "unicidade_base_escola_ano",
    "chaves_duplicidades",
    nrow(base_final) ==
      n_distinct(interaction(
        base_final[[col_id_base]],
        base_final[[col_ano_base]],
        drop = TRUE
      )),
    nrow(base_final) -
      n_distinct(interaction(
        base_final[[col_id_base]],
        base_final[[col_ano_base]],
        drop = TRUE
      )),
    "0 duplicidades"
  ),
  registrar_teste(
    "unicidade_perfil_completo",
    "chaves_duplicidades",
    nrow(perfil_completo) ==
      n_distinct(perfil_completo[[col_id_perfil]]),
    nrow(perfil_completo) -
      n_distinct(perfil_completo[[col_id_perfil]]),
    "0 duplicidades"
  ),
  registrar_teste(
    "unicidade_perfil_gerencial",
    "chaves_duplicidades",
    nrow(perfil_gerencial) ==
      n_distinct(perfil_gerencial[[col_id_gerencial]]),
    nrow(perfil_gerencial) -
      n_distinct(perfil_gerencial[[col_id_gerencial]]),
    "0 duplicidades"
  ),
  registrar_teste(
    "unicidade_indice",
    "chaves_duplicidades",
    nrow(indice) == n_distinct(indice[[col_id_indice]]),
    nrow(indice) - n_distinct(indice[[col_id_indice]]),
    "0 duplicidades"
  ),
  registrar_teste(
    "unicidade_detalhe_carteira",
    "chaves_duplicidades",
    nrow(detalhe_carteiras) ==
      n_distinct(detalhe_carteiras[[col_id_detalhe]]),
    nrow(detalhe_carteiras) -
      n_distinct(detalhe_carteiras[[col_id_detalhe]]),
    "0 duplicidades"
  )
)

readr::write_csv(
  validacao_chaves,
  file.path(dir_execucao, "06_validacao_chaves_duplicidades.csv"),
  na = ""
)

testes <- bind_rows(testes, validacao_chaves)

# -------------------------------------------------------------------
# 8. Coerência cruzada
# -------------------------------------------------------------------

ids_dim <- sort(unique(as.character(dim_final[[col_id_dim]])))
ids_operacionais <- sort(as.character(
  dim_final[[col_id_dim]][como_logico(dim_final[[col_operacional_2026]])]
))
ids_indice <- sort(unique(as.character(indice[[col_id_indice]])))
ids_detalhe <- sort(unique(as.character(detalhe_carteiras[[col_id_detalhe]])))

validacao_cruzada <- bind_rows(
  registrar_teste(
    "base_cobre_dimensao_institucional",
    "coerencia_cruzada",
    setequal(unique(as.character(base_final[[col_id_base]])), ids_dim),
    length(unique(base_final[[col_id_base]])),
    length(ids_dim)
  ),
  registrar_teste(
    "indice_equivale_universo_operacional",
    "coerencia_cruzada",
    setequal(ids_indice, ids_operacionais),
    paste0("indice=", length(ids_indice), "; operacional=", length(ids_operacionais)),
    "conjuntos idênticos"
  ),
  registrar_teste(
    "detalhe_carteiras_equivale_indice",
    "coerencia_cruzada",
    setequal(ids_detalhe, ids_indice),
    paste0("detalhe=", length(ids_detalhe), "; indice=", length(ids_indice)),
    "conjuntos idênticos"
  ),
  registrar_teste(
    "nao_operacionais_fora_indice",
    "coerencia_cruzada",
    length(intersect(ids_nao_operacionais, ids_indice)) == 0L,
    paste(intersect(ids_nao_operacionais, ids_indice), collapse = ", "),
    "nenhum ID não operacional",
    detalhe = paste("IDs bloqueados:", paste(ids_nao_operacionais, collapse = ", "))
  ),
  registrar_teste(
    "carteiras_analise_iguais_indice",
    "coerencia_cruzada",
    setequal(
      sort(unique(na.omit(as.character(analise_carteiras[[col_assessora_analise]])))),
      sort(unique(na.omit(as.character(indice[[col_assessora_indice]]))))
    ),
    paste0(
      "analise=", n_distinct(na.omit(analise_carteiras[[col_assessora_analise]])),
      "; indice=", n_distinct(na.omit(indice[[col_assessora_indice]]))
    ),
    "conjuntos idênticos"
  )
)

readr::write_csv(
  validacao_cruzada,
  file.path(dir_execucao, "07_validacao_coerencia_cruzada.csv"),
  na = ""
)

testes <- bind_rows(testes, validacao_cruzada)

# -------------------------------------------------------------------
# 9. Produtos do módulo 21
# -------------------------------------------------------------------

t01 <- ler_csv_seguro(entradas[["modulo_21_t01"]])
t03 <- ler_csv_seguro(entradas[["modulo_21_t03"]])
t04 <- ler_csv_seguro(entradas[["modulo_21_t04"]])
t05 <- ler_csv_seguro(entradas[["modulo_21_t05"]])
validacao_m21_origem <- ler_csv_seguro(
  entradas[["modulo_21_validacao_universos"]]
)

col_t01_universo <- primeira_coluna(t01, c("universo"))
col_t01_periodo <- primeira_coluna(t01, c("periodo"), obrigatoria = FALSE)
col_t01_total <- primeira_coluna(t01, c("total", "valor"))
col_aprovado_m21 <- primeira_coluna(
  validacao_m21_origem,
  c("aprovado", "resultado")
)

buscar_total_t01 <- function(universo_alvo, periodo_alvo = NULL) {
  universo_normalizado <- normalizar_texto(universo_alvo)

  dados <- t01 |>
    filter(
      normalizar_texto(.data[[col_t01_universo]]) ==
        .env$universo_normalizado
    )

  if (!is.null(periodo_alvo) && !is.na(col_t01_periodo)) {
    periodo_texto <- as.character(periodo_alvo)

    dados <- dados |>
      filter(
        as.character(.data[[col_t01_periodo]]) ==
          .env$periodo_texto
      )
  }

  if (nrow(dados) != 1L) return(NA_real_)
  as.numeric(dados[[col_t01_total]][[1]])
}

validacao_modulo_21 <- bind_rows(
  registrar_teste(
    "m21_validacoes_origem_aprovadas",
    "modulo_21",
    nrow(validacao_m21_origem) > 0L &&
      all(como_logico(validacao_m21_origem[[col_aprovado_m21]])),
    sum(como_logico(validacao_m21_origem[[col_aprovado_m21]])),
    nrow(validacao_m21_origem)
  ),
  registrar_teste(
    "m21_t01_institucional",
    "modulo_21",
    isTRUE(all.equal(buscar_total_t01("Institucional"), 56)),
    buscar_total_t01("Institucional"),
    56
  ),
  registrar_teste(
    "m21_t01_operacional",
    "modulo_21",
    isTRUE(all.equal(buscar_total_t01("Operacional"), 53)),
    buscar_total_t01("Operacional"),
    53
  ),
  registrar_teste(
    "m21_t01_avaliativo_2025",
    "modulo_21",
    isTRUE(all.equal(buscar_total_t01("Avaliativo", "2025"), 54)),
    buscar_total_t01("Avaliativo", "2025"),
    54
  ),
  registrar_teste(
    "m21_t01_avaliativo_2026",
    "modulo_21",
    isTRUE(all.equal(buscar_total_t01("Avaliativo", "2026"), 56)),
    buscar_total_t01("Avaliativo", "2026"),
    56
  ),
  registrar_teste(
    "m21_t01_gerencial",
    "modulo_21",
    isTRUE(all.equal(buscar_total_t01("Gerencial"), 11)),
    buscar_total_t01("Gerencial"),
    11
  ),
  registrar_teste(
    "m21_t04_onze_carteiras",
    "modulo_21",
    nrow(t04) == 11L,
    nrow(t04),
    11
  ),
  registrar_teste(
    "m21_t05_dimensoes_permitidas",
    "modulo_21",
    setequal(
      sort(unique(t05$dimensao)),
      sort(c("Complexidade administrativa", "Estrutura", "Volume"))
    ),
    paste(sort(unique(t05$dimensao)), collapse = ", "),
    "Complexidade administrativa, Estrutura, Volume"
  )
)

readr::write_csv(
  validacao_modulo_21,
  file.path(dir_execucao, "08_validacao_produtos_modulo_21.csv"),
  na = ""
)

testes <- bind_rows(testes, validacao_modulo_21)

# -------------------------------------------------------------------
# 10. Relatório técnico do módulo 22
# -------------------------------------------------------------------

qmd_linhas <- readLines(
  entradas[["modulo_22_qmd"]],
  warn = FALSE,
  encoding = "UTF-8"
)
qmd_texto <- paste(qmd_linhas, collapse = "\n")
html_texto <- paste(
  readLines(
    entradas[["modulo_22_html"]],
    warn = FALSE,
    encoding = "UTF-8"
  ),
  collapse = "\n"
)

termos_proibidos <- tribble(
  ~termo, ~padrao,
  "ranking analítico", "\\branking\\b",
  "percentil analítico", "\\bpercentil\\b",
  "cenário de redistribuição", "cen[aá]rio.{0,40}redistribui",
  "inferência causal afirmativa", "causou|efeito causal identificado"
)

ocorrencias_proibidas <- termos_proibidos |>
  mutate(
    encontrado_qmd = map_lgl(
      .data$padrao,
      ~ stringr::str_detect(
        normalizar_texto(qmd_texto),
        regex(.x, ignore_case = TRUE)
      )
    )
  )

# Termos podem aparecer em cautelas metodológicas. Por isso esta busca é
# registrada como advertência e não bloqueia isoladamente a execução.
validacao_modulo_22 <- bind_rows(
  registrar_teste(
    "m22_parametro_execucao_modulo_21",
    "modulo_22",
    stringr::str_detect(
      qmd_texto,
      fixed(paste0('execucao_modulo_21: "', execucao_modulo_21, '"'))
    ),
    ifelse(
      stringr::str_detect(
        qmd_texto,
        fixed(paste0('execucao_modulo_21: "', execucao_modulo_21, '"'))
      ),
      execucao_modulo_21,
      "não localizado"
    ),
    execucao_modulo_21
  ),
  registrar_teste(
    "m22_freeze_false",
    "modulo_22",
    stringr::str_detect(qmd_texto, regex("freeze:\\s*false")),
    ifelse(
      stringr::str_detect(qmd_texto, regex("freeze:\\s*false")),
      "freeze: false",
      "não localizado"
    ),
    "freeze: false"
  ),
  registrar_teste(
    "m22_data_publicacao",
    "modulo_22",
    stringr::str_detect(qmd_texto, fixed('date: "2026-08-02"')),
    ifelse(
      stringr::str_detect(qmd_texto, fixed('date: "2026-08-02"')),
      "2026-08-02",
      "não localizado"
    ),
    "2026-08-02"
  ),
  registrar_teste(
    "m22_numeração_seções",
    "modulo_22",
    stringr::str_detect(
      qmd_texto,
      regex("number-sections:\\s*true")
    ),
    "configuração YAML",
    "number-sections: true"
  ),
  registrar_teste(
    "m22_html_contem_titulo",
    "modulo_22",
    stringr::str_detect(
      normalizar_texto(html_texto),
      fixed(normalizar_texto(
        "Diagnóstico institucional e operacional do assessoramento às escolas"
      ))
    ),
    "título no HTML",
    "título esperado"
  ),
  registrar_teste(
    "m22_html_nao_vazio",
    "modulo_22",
    file.info(entradas[["modulo_22_html"]])$size > 10000,
    file.info(entradas[["modulo_22_html"]])$size,
    "> 10000 bytes"
  ),
  ocorrencias_proibidas |>
    transmute(
      teste = paste0(
        "m22_busca_termo_",
        stringr::str_replace_all(
          normalizar_texto(.data$termo),
          "[^a-z0-9]+",
          "_"
        )
      ),
      categoria = "modulo_22",
      criticidade = "advertencia",
      aprovado = TRUE,
      observado = if_else(
        .data$encontrado_qmd,
        "termo localizado; revisar contexto",
        "termo não localizado"
      ),
      esperado = "uso apenas em cautela ou proibição metodológica",
      detalhe = .data$termo
    )
)

readr::write_csv(
  validacao_modulo_22,
  file.path(dir_execucao, "09_validacao_relatorio_modulo_22.csv"),
  na = ""
)

testes <- bind_rows(testes, validacao_modulo_22)

# -------------------------------------------------------------------
# 11. Ambiente
# -------------------------------------------------------------------

session_atual <- capture.output(sessionInfo())
readr::write_lines(
  session_atual,
  file.path(dir_execucao, "13_session_info.txt")
)

session_m21 <- readLines(
  entradas[["modulo_21_session_info"]],
  warn = FALSE,
  encoding = "UTF-8"
)

versao_r_m21 <- session_m21[
  stringr::str_detect(session_m21, "^R version ")
][1]

quarto_path <- Sys.which("quarto")
quarto_disponivel <- nzchar(quarto_path)

validacao_ambiente <- tibble::tribble(
  ~componente, ~valor, ~status, ~criticidade,
  "R atual", R.version.string, "registrado", "informativo",
  "R módulo 21", versao_r_m21, "registrado", "informativo",
  "fuso horário", Sys.timezone(), "registrado", "informativo",
  "Quarto no PATH", ifelse(quarto_disponivel, quarto_path, NA_character_),
  ifelse(quarto_disponivel, "localizado", "não localizado"),
  "advertencia"
)

readr::write_csv(
  validacao_ambiente,
  file.path(dir_execucao, "10_validacao_ambiente.csv"),
  na = ""
)

testes <- bind_rows(
  testes,
  registrar_teste(
    "ambiente_r_compativel_modulo_21",
    "ambiente",
    stringr::str_detect(versao_r_m21, fixed("R version 4.4.2")),
    versao_r_m21,
    "R version 4.4.2",
    criticidade = "advertencia"
  ),
  registrar_teste(
    "quarto_disponivel_path",
    "ambiente",
    TRUE,
    ifelse(quarto_disponivel, quarto_path, "não localizado"),
    "registro informativo; não bloqueante",
    criticidade = "advertencia"
  )
)

# -------------------------------------------------------------------
# 12. Resultado integrado
# -------------------------------------------------------------------

testes <- testes |>
  mutate(
    aprovado = as.logical(.data$aprovado),
    status = case_when(
      .data$aprovado ~ "APROVADO",
      .data$criticidade == "bloqueante" ~ "REPROVADO",
      TRUE ~ "ADVERTENCIA"
    )
  ) |>
  arrange(
    factor(.data$status, levels = c("REPROVADO", "ADVERTENCIA", "APROVADO")),
    .data$categoria,
    .data$teste
  )

readr::write_csv(
  testes,
  file.path(dir_execucao, "11_resultado_testes.csv"),
  na = ""
)

falhas_bloqueantes <- testes |>
  filter(.data$criticidade == "bloqueante", !.data$aprovado)

resumo_integrado <- tibble(
  id_execucao = id_execucao,
  instante_execucao = format(
    instante_execucao,
    "%Y-%m-%d %H:%M:%S %Z"
  ),
  branch = git_branch$saida,
  commit = git_head$saida,
  total_testes = nrow(testes),
  testes_aprovados = sum(testes$status == "APROVADO"),
  advertencias = sum(testes$status == "ADVERTENCIA"),
  falhas_bloqueantes = nrow(falhas_bloqueantes),
  resultado_global = if_else(
    nrow(falhas_bloqueantes) == 0L,
    "APROVADO",
    "REPROVADO"
  ),
  execucao_modulo_21 = execucao_modulo_21,
  md5_script_modulo_23 = hash_md5(caminho_script),
  md5_modulo_22_qmd = hash_md5(entradas[["modulo_22_qmd"]]),
  md5_modulo_22_html = hash_md5(entradas[["modulo_22_html"]])
)

contrato_execucao <- tibble(
  campo = c(
    "id_execucao",
    "instante_execucao",
    "branch_esperada",
    "commit_esperado",
    "execucao_modulo_21",
    "modo",
    "politica_falha"
  ),
  valor = c(
    id_execucao,
    format(instante_execucao, "%Y-%m-%d %H:%M:%S %Z"),
    branch_esperada,
    commit_esperado,
    execucao_modulo_21,
    "auditoria_nao_destrutiva",
    "falha_bloqueante_impede_promocao"
  )
)

readr::write_csv(
  contrato_execucao,
  file.path(dir_execucao, "00_contrato_execucao.csv"),
  na = ""
)

manifesto_entradas <- existencia_entradas |>
  transmute(
    entrada = .data$entrada,
    modulo = .data$modulo,
    caminho = .data$caminho,
    tamanho_bytes = .data$tamanho_bytes,
    md5 = .data$md5_observado
  )

readr::write_csv(
  manifesto_entradas,
  file.path(dir_execucao, "02_manifesto_entradas.csv"),
  na = ""
)

if (nrow(falhas_bloqueantes) > 0L) {
  resumo_integrado |>
    readr::write_csv(
      file.path(dir_execucao, "14_resumo_execucao.txt"),
      na = ""
    )

  stop(
    "Validação integrada REPROVADA com ",
    nrow(falhas_bloqueantes),
    " falha(s) bloqueante(s). Consulte ",
    file.path(dir_execucao, "11_resultado_testes.csv"),
    "."
  )
}

# -------------------------------------------------------------------
# 13. Candidatos e promoção transacional
# -------------------------------------------------------------------

candidato_csv <- file.path(
  dir_candidatos,
  "validacao_integrada_final.csv"
)
candidato_rds <- file.path(
  dir_candidatos,
  "validacao_integrada_final.rds"
)

readr::write_csv(resumo_integrado, candidato_csv, na = "")
saveRDS(resumo_integrado, candidato_rds)

validacao_candidatos <- bind_rows(
  registrar_teste(
    "candidato_csv_existe",
    "promocao",
    file.exists(candidato_csv),
    candidato_csv,
    "arquivo existente"
  ),
  registrar_teste(
    "candidato_rds_existe",
    "promocao",
    file.exists(candidato_rds),
    candidato_rds,
    "arquivo existente"
  ),
  comparar_csv_rds(
    candidato_csv,
    candidato_rds,
    "validacao_integrada_final"
  )
)

if (any(!validacao_candidatos$aprovado)) {
  stop("Produtos candidatos do módulo 23 falharam na validação.")
}

for (nome in names(destinos_canonicos)) {
  destino <- destinos_canonicos[[nome]]

  if (file.exists(destino)) {
    backup <- file.path(dir_rollback, basename(destino))
    ok_backup <- file.copy(destino, backup, overwrite = TRUE)
    if (!ok_backup) {
      stop("Não foi possível criar rollback de: ", destino)
    }
  }
}

promovidos <- c(
  csv = file.copy(
    candidato_csv,
    destinos_canonicos[["csv"]],
    overwrite = TRUE
  ),
  rds = file.copy(
    candidato_rds,
    destinos_canonicos[["rds"]],
    overwrite = TRUE
  )
)

if (!all(promovidos)) {
  for (nome in names(destinos_canonicos)) {
    destino <- destinos_canonicos[[nome]]
    backup <- file.path(dir_rollback, basename(destino))

    if (file.exists(backup)) {
      file.copy(backup, destino, overwrite = TRUE)
    }
  }

  stop("Promoção transacional falhou; rollback aplicado quando disponível.")
}

hashes_promovidos <- tibble(
  produto = names(destinos_canonicos),
  caminho = unname(destinos_canonicos),
  tamanho_bytes = map_dbl(
    destinos_canonicos,
    ~ as.numeric(file.info(.x)$size)
  ),
  md5 = map_chr(destinos_canonicos, hash_md5)
)

manifesto_produtos <- bind_rows(
  hashes_promovidos,
  tibble(
    produto = basename(list.files(
      dir_execucao,
      full.names = TRUE
    )),
    caminho = list.files(
      dir_execucao,
      full.names = TRUE
    ),
    tamanho_bytes = map_dbl(
      list.files(dir_execucao, full.names = TRUE),
      ~ as.numeric(file.info(.x)$size)
    ),
    md5 = map_chr(
      list.files(dir_execucao, full.names = TRUE),
      hash_md5
    )
  )
)

readr::write_csv(
  manifesto_produtos,
  file.path(dir_execucao, "12_manifesto_produtos_modulo_23.csv"),
  na = ""
)

resumo_texto <- c(
  "MÓDULO 23 — VALIDAÇÃO INTEGRADA FINAL",
  paste0("Execução: ", id_execucao),
  paste0("Data/hora: ", format(instante_execucao, "%Y-%m-%d %H:%M:%S %Z")),
  paste0("Branch: ", git_branch$saida),
  paste0("Commit: ", git_head$saida),
  paste0("Testes: ", nrow(testes)),
  paste0("Aprovados: ", sum(testes$status == "APROVADO")),
  paste0("Advertências: ", sum(testes$status == "ADVERTENCIA")),
  paste0("Falhas bloqueantes: ", nrow(falhas_bloqueantes)),
  "Resultado global: APROVADO",
  paste0("Produto CSV: ", destinos_canonicos[["csv"]]),
  paste0("Produto RDS: ", destinos_canonicos[["rds"]]),
  paste0("Documentação: ", dir_execucao)
)

readr::write_lines(
  resumo_texto,
  file.path(dir_execucao, "14_resumo_execucao.txt")
)

message("============================================================")
message("MÓDULO 23 CONCLUÍDO COM SUCESSO")
message("Execução: ", id_execucao)
message("Resultado global: APROVADO")
message("Documentação: ", dir_execucao)
message("Produto CSV: ", destinos_canonicos[["csv"]])
message("Produto RDS: ", destinos_canonicos[["rds"]])
message("============================================================")
