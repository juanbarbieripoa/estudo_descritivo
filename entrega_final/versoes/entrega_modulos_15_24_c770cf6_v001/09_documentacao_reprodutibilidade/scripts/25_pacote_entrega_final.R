# ===================================================================
# 25_pacote_entrega_final.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# Pacote institucional versionado dos módulos 15 a 24
# ===================================================================
#
# RESPONSABILIDADE
#
# Reunir exclusivamente produtos homologados, sem recalcular tabelas,
# gráficos, indicadores, bases ou relatórios. O módulo:
#
#   1. valida branch, commit, execuções, arquivos, tamanhos e hashes;
#   2. usa lista branca e caminhos explícitos;
#   3. copia os produtos para uma área transacional;
#   4. verifica integralmente as cópias;
#   5. gera inventários, manifestos e registros de rastreabilidade;
#   6. promove uma versão imutável do pacote;
#   7. não inclui produtos ainda inexistentes dos módulos 26 e 27.
#
# NÃO FAZ
#
# - não recalcula resultados;
# - não renderiza QMD;
# - não seleciona silenciosamente a execução mais recente;
# - não sobrescreve versão promovida;
# - não inclui caches, temporários ou diagnósticos locais;
# - não cria ranking, percentil, classificação ou inferência causal.
# ===================================================================

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
})

if (!requireNamespace("openssl", quietly = TRUE)) {
  stop(
    "O pacote `openssl` é obrigatório para calcular SHA-256. ",
    "Instale-o antes de executar o módulo 25."
  )
}

# -------------------------------------------------------------------
# 1. Contrato da execução
# -------------------------------------------------------------------

instante_execucao <- Sys.time()
id_execucao <- format(instante_execucao, "%Y%m%d_%H%M%S")

CONFIG <- list(
  id_pacote = "entrega_modulos_15_24_c770cf6_v001",
  versao_pacote = "v001",
  fase_entrega = "pre_sumario_apresentacao",
  branch_esperada = "refatoracao_modulo_21",
  commit_esperado = "c770cf6117a6408d787d6fd1eb3f51f48b4b1ca6",
  execucao_modulo_21 = "20260731_001044",
  execucao_modulo_23 = "20260802_211916",
  permitir_modulo_26 = FALSE,
  permitir_modulo_27 = FALSE,
  universos = c(
    escolas_institucionais = 56L,
    combinacoes_escola_ano = 280L,
    escolas_avaliativas_2025 = 54L,
    escolas_avaliativas_2026 = 56L,
    escolas_operacionais_2026 = 53L,
    carteiras_gerenciais_2026 = 11L,
    exposicao_positiva_2025 = 0L
  ),
  escolas_nao_operacionais = c("ESC_001", "ESC_055", "ESC_056")
)

caminho_script <- here("R", "25_pacote_entrega_final.R")

# -------------------------------------------------------------------
# 2. Funções auxiliares
# -------------------------------------------------------------------

falhar <- function(...) {
  stop(paste0(...), call. = FALSE)
}

normalizar_caminho <- function(x, must_work = TRUE) {
  normalizePath(x, winslash = "/", mustWork = must_work)
}

executar_git <- function(argumentos, aceitar_status = FALSE) {
  saida <- suppressWarnings(
    system2(
      "git",
      c("-C", shQuote(here()), argumentos),
      stdout = TRUE,
      stderr = TRUE
    )
  )

  status <- attr(saida, "status")
  if (is.null(status)) status <- 0L

  if (!aceitar_status && status != 0L) {
    falhar(
      "Falha ao executar Git: git ",
      paste(argumentos, collapse = " "),
      "\n",
      paste(saida, collapse = "\n")
    )
  }

  list(
    status = status,
    linhas = as.character(saida),
    texto = str_squish(paste(saida, collapse = " "))
  )
}

hash_md5 <- function(caminho) {
  if (
    length(caminho) != 1L ||
    is.na(caminho) ||
    !file.exists(caminho) ||
    dir.exists(caminho)
  ) {
    return(NA_character_)
  }

  str_to_lower(unname(tools::md5sum(caminho)[[1]]))
}

hash_sha256 <- function(caminho) {
  if (
    length(caminho) != 1L ||
    is.na(caminho) ||
    !file.exists(caminho) ||
    dir.exists(caminho)
  ) {
    return(NA_character_)
  }

  conexao <- file(caminho, open = "rb")
  on.exit(close(conexao), add = TRUE)

  str_to_lower(as.character(openssl::sha256(conexao)))
}

ler_csv_seguro <- function(caminho) {
  read_csv(
    caminho,
    show_col_types = FALSE,
    progress = FALSE,
    na = c("", "NA")
  )
}

validar_relativo <- function(x) {
  if (
    any(is.na(x)) ||
    any(str_detect(x, "(^|[/\\\\])\\.\\.([/\\\\]|$)")) ||
    any(str_detect(x, "^[A-Za-z]:")) ||
    any(str_detect(x, "^[/\\\\]"))
  ) {
    falhar("Caminho relativo inválido na lista branca.")
  }
  invisible(TRUE)
}

localizar_hash_manifesto <- function(
  manifesto,
  caminho_origem,
  coluna_hash = c("md5", "hash_md5", "hash_promovido"),
  coluna_tamanho = c(
    "tamanho_bytes", "tamanho_promovido", "tamanho_candidato"
  )
) {
  alvo <- basename(caminho_origem)

  col_hash <- intersect(coluna_hash, names(manifesto))
  col_tamanho <- intersect(coluna_tamanho, names(manifesto))

  if (length(col_hash) == 0L || length(col_tamanho) == 0L) {
    falhar(
      "Manifesto sem colunas reconhecidas de hash/tamanho: ",
      caminho_origem
    )
  }

  col_caminho <- intersect(
    c(
      "caminho", "caminho_promovido", "caminho_candidato",
      "produto", "arquivo", "relativo"
    ),
    names(manifesto)
  )

  if (length(col_caminho) == 0L) {
    falhar("Manifesto sem coluna reconhecida de identificação de arquivo.")
  }

  corresponde <- rep(FALSE, nrow(manifesto))

  for (coluna in col_caminho) {
    valores <- as.character(manifesto[[coluna]])
    corresponde <- corresponde |
      (!is.na(valores) & basename(valores) == alvo) |
      (!is.na(valores) & valores == caminho_origem)
  }

  candidatos <- manifesto[corresponde, , drop = FALSE]

  if (nrow(candidatos) != 1L) {
    falhar(
      "Produto `", alvo,
      "` não foi localizado de forma única no manifesto. Ocorrências: ",
      nrow(candidatos), "."
    )
  }

  list(
    md5 = str_to_lower(as.character(candidatos[[col_hash[[1]]]][[1]])),
    tamanho_bytes = as.numeric(candidatos[[col_tamanho[[1]]]][[1]])
  )
}

adicionar_hash_manifesto <- function(
  tabela,
  manifesto,
  modulo
) {
  resolvidos <- map(
    tabela$caminho_origem,
    ~ localizar_hash_manifesto(manifesto, .x)
  )

  tabela |>
    mutate(
      modulo_origem = modulo,
      md5_esperado = map_chr(resolvidos, "md5"),
      tamanho_esperado = map_dbl(resolvidos, "tamanho_bytes")
    )
}

listar_arquivos <- function(pasta) {
  arquivos <- list.files(
    pasta,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  arquivos <- arquivos[
    file.exists(arquivos) &
      !file.info(arquivos)$isdir
  ]

  base <- normalizar_caminho(pasta)

  tibble(
    caminho = arquivos,
    caminho_relativo = str_remove(
      normalizar_caminho(arquivos),
      paste0("^", fixed(base), "/?")
    ),
    tamanho_bytes = as.numeric(file.info(arquivos)$size),
    md5 = map_chr(arquivos, hash_md5),
    sha256 = map_chr(arquivos, hash_sha256)
  ) |>
    arrange(caminho_relativo)
}

copiar_um <- function(origem, destino) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)

  ok <- file.copy(
    from = origem,
    to = destino,
    overwrite = FALSE,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  if (!isTRUE(ok)) {
    falhar("Falha ao copiar: ", origem, " -> ", destino)
  }

  invisible(TRUE)
}

# -------------------------------------------------------------------
# 3. Validação do Git
# -------------------------------------------------------------------

git_branch <- executar_git(c("branch", "--show-current"))
git_commit <- executar_git(c("rev-parse", "HEAD"))
git_status <- executar_git(c("status", "--porcelain"), aceitar_status = TRUE)

branch_observada <- git_branch$texto
commit_observado <- git_commit$texto
linhas_status_git <- git_status$linhas

if (!identical(branch_observada, CONFIG$branch_esperada)) {
  falhar(
    "Branch divergente. Observada: ", branch_observada,
    "; esperada: ", CONFIG$branch_esperada, "."
  )
}

if (!identical(commit_observado, CONFIG$commit_esperado)) {
  falhar(
    "Commit divergente. Observado: ", commit_observado,
    "; esperado: ", CONFIG$commit_esperado, "."
  )
}

permitidos_nao_rastreados <- c(
  "R/25_pacote_entrega_final.R",
  "R/25A_auditar_entradas_pacote.R",
  "documentacao/pacote_entrega_final/",
  "entrega_final/"
)

classificar_status_git <- function(linha) {
  codigo <- str_sub(linha, 1, 2)
  caminho <- str_trim(str_sub(linha, 4))

  permitido <- codigo == "??" &&
    any(
      map_lgl(
        permitidos_nao_rastreados,
        ~ caminho == .x | str_starts(caminho, .x)
      )
    )

  tibble(
    linha = linha,
    codigo = codigo,
    caminho = caminho,
    permitido = permitido
  )
}

status_git_classificado <- if (length(linhas_status_git) == 0L) {
  tibble(
    linha = character(),
    codigo = character(),
    caminho = character(),
    permitido = logical()
  )
} else {
  map_dfr(linhas_status_git, classificar_status_git)
}

git_bloqueante <- status_git_classificado |>
  filter(!permitido)

if (nrow(git_bloqueante) > 0L) {
  falhar(
    "Há alterações Git não autorizadas antes do empacotamento:\n- ",
    paste(git_bloqueante$linha, collapse = "\n- ")
  )
}

# -------------------------------------------------------------------
# 4. Manifestos homologados
# -------------------------------------------------------------------

manifestos_caminhos <- c(
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
    "documentacao", "relatorio",
    paste0("execucao_", CONFIG$execucao_modulo_21),
    "10_manifesto_produtos_promovidos.csv"
  ),
  modulo_23 = here(
    "documentacao", "validacao_integrada",
    paste0("execucao_", CONFIG$execucao_modulo_23),
    "12_manifesto_produtos_modulo_23.csv"
  )
)

ausentes_manifestos <- manifestos_caminhos[
  !file.exists(manifestos_caminhos)
]

if (length(ausentes_manifestos) > 0L) {
  falhar(
    "Manifestos homologados ausentes:\n- ",
    paste(ausentes_manifestos, collapse = "\n- ")
  )
}

manifestos <- map(manifestos_caminhos, ler_csv_seguro)

# -------------------------------------------------------------------
# 5. Lista branca — bases analíticas homologadas
# -------------------------------------------------------------------

bases_15 <- tribble(
  ~id_produto, ~caminho_origem, ~caminho_destino, ~formato,
  "M15_BASE_CSV",
  here("dados_finais", "base_analitica_final_escola_serie.csv"),
  "07_bases_analiticas/modulo_15/base_analitica_final_escola_serie.csv",
  "csv",
  "M15_BASE_RDS",
  here("dados_finais", "base_analitica_final_escola_serie.rds"),
  "07_bases_analiticas/modulo_15/base_analitica_final_escola_serie.rds",
  "rds",
  "M15_DIM_CSV",
  here("dados_finais", "dim_escola_final.csv"),
  "07_bases_analiticas/modulo_15/dim_escola_final.csv",
  "csv",
  "M15_DIM_RDS",
  here("dados_finais", "dim_escola_final.rds"),
  "07_bases_analiticas/modulo_15/dim_escola_final.rds",
  "rds"
) |>
  adicionar_hash_manifesto(manifestos$modulo_15, "15")

bases_16 <- tribble(
  ~id_produto, ~caminho_origem, ~caminho_destino, ~formato,
  "M16_PERFIL_COMPLETO_CSV",
  here("dados_finais", "perfil_escola_completo.csv"),
  "07_bases_analiticas/modulo_16/perfil_escola_completo.csv",
  "csv",
  "M16_PERFIL_COMPLETO_RDS",
  here("dados_finais", "perfil_escola_completo.rds"),
  "07_bases_analiticas/modulo_16/perfil_escola_completo.rds",
  "rds",
  "M16_PERFIL_GERENCIAL_CSV",
  here("dados_finais", "perfil_escola_gerencial.csv"),
  "07_bases_analiticas/modulo_16/perfil_escola_gerencial.csv",
  "csv",
  "M16_PERFIL_GERENCIAL_RDS",
  here("dados_finais", "perfil_escola_gerencial.rds"),
  "07_bases_analiticas/modulo_16/perfil_escola_gerencial.rds",
  "rds",
  "M16_PERFIL_SERIE_CSV",
  here("dados_finais", "perfil_escola_serie_compacto.csv"),
  "07_bases_analiticas/modulo_16/perfil_escola_serie_compacto.csv",
  "csv",
  "M16_PERFIL_SERIE_RDS",
  here("dados_finais", "perfil_escola_serie_compacto.rds"),
  "07_bases_analiticas/modulo_16/perfil_escola_serie_compacto.rds",
  "rds"
) |>
  adicionar_hash_manifesto(manifestos$modulo_16, "16")

bases_17 <- tribble(
  ~id_produto, ~caminho_origem, ~caminho_destino, ~formato,
  "M17_INDICE_CSV",
  here("dados_finais", "indice_carga_potencial_escola.csv"),
  "07_bases_analiticas/modulo_17/indice_carga_potencial_escola.csv",
  "csv",
  "M17_INDICE_RDS",
  here("dados_finais", "indice_carga_potencial_escola.rds"),
  "07_bases_analiticas/modulo_17/indice_carga_potencial_escola.rds",
  "rds",
  "M17_COMPONENTES_CSV",
  here("dados_finais", "componentes_indice_carga_potencial.csv"),
  "07_bases_analiticas/modulo_17/componentes_indice_carga_potencial.csv",
  "csv",
  "M17_COMPONENTES_RDS",
  here("dados_finais", "componentes_indice_carga_potencial.rds"),
  "07_bases_analiticas/modulo_17/componentes_indice_carga_potencial.rds",
  "rds",
  "M17_EDUCACIONAL_CSV",
  here("dados_finais", "componentes_educacionais_escola_serie.csv"),
  "07_bases_analiticas/modulo_17/componentes_educacionais_escola_serie.csv",
  "csv",
  "M17_EDUCACIONAL_RDS",
  here("dados_finais", "componentes_educacionais_escola_serie.rds"),
  "07_bases_analiticas/modulo_17/componentes_educacionais_escola_serie.rds",
  "rds"
) |>
  adicionar_hash_manifesto(manifestos$modulo_17, "17")

bases_18 <- tribble(
  ~id_produto, ~caminho_origem, ~caminho_destino, ~formato,
  "M18_ANALISE_CSV",
  here("dados_finais", "analise_carteiras_assessoras.csv"),
  "07_bases_analiticas/modulo_18/analise_carteiras_assessoras.csv",
  "csv",
  "M18_ANALISE_RDS",
  here("dados_finais", "analise_carteiras_assessoras.rds"),
  "07_bases_analiticas/modulo_18/analise_carteiras_assessoras.rds",
  "rds",
  "M18_DETALHE_CSV",
  here("dados_finais", "carteira_escola_detalhe.csv"),
  "07_bases_analiticas/modulo_18/carteira_escola_detalhe.csv",
  "csv",
  "M18_DETALHE_RDS",
  here("dados_finais", "carteira_escola_detalhe.rds"),
  "07_bases_analiticas/modulo_18/carteira_escola_detalhe.rds",
  "rds",
  "M18_DIAGNOSTICO_CSV",
  here("dados_finais", "diagnostico_educacional_carteiras.csv"),
  "07_bases_analiticas/modulo_18/diagnostico_educacional_carteiras.csv",
  "csv",
  "M18_DIAGNOSTICO_RDS",
  here("dados_finais", "diagnostico_educacional_carteiras.rds"),
  "07_bases_analiticas/modulo_18/diagnostico_educacional_carteiras.rds",
  "rds",
  "M18_COMPOSICAO_CSV",
  here("dados_finais", "composicao_operacional_carteiras.csv"),
  "07_bases_analiticas/modulo_18/composicao_operacional_carteiras.csv",
  "csv",
  "M18_COMPOSICAO_RDS",
  here("dados_finais", "composicao_operacional_carteiras.rds"),
  "07_bases_analiticas/modulo_18/composicao_operacional_carteiras.rds",
  "rds",
  "M18_UNIVERSO_CSV",
  here("dados_finais", "universo_institucional_carteiras.csv"),
  "07_bases_analiticas/modulo_18/universo_institucional_carteiras.csv",
  "csv",
  "M18_UNIVERSO_RDS",
  here("dados_finais", "universo_institucional_carteiras.rds"),
  "07_bases_analiticas/modulo_18/universo_institucional_carteiras.rds",
  "rds"
) |>
  adicionar_hash_manifesto(manifestos$modulo_18, "18")

bases_20 <- tribble(
  ~id_produto, ~caminho_origem, ~caminho_destino, ~formato,
  "M20_INDICE_FICHAS_CSV",
  here("dados_finais", "indice_fichas_carteiras.csv"),
  "07_bases_analiticas/modulo_20/indice_fichas_carteiras.csv",
  "csv",
  "M20_INDICE_FICHAS_RDS",
  here("dados_finais", "indice_fichas_carteiras.rds"),
  "07_bases_analiticas/modulo_20/indice_fichas_carteiras.rds",
  "rds"
) |>
  adicionar_hash_manifesto(manifestos$modulo_20, "20")

# -------------------------------------------------------------------
# 6. Lista branca — produtos editoriais do módulo 21
# -------------------------------------------------------------------

dir_resultados_21 <- here(
  "resultados", "relatorio",
  paste0("execucao_", CONFIG$execucao_modulo_21)
)

produtos_21_relativos <- c(
  "corpo/tabelas/T01_universos_cobertura_estudo.csv",
  "corpo/tabelas/T02_perfil_estrutural_universo_institucional.csv",
  "corpo/tabelas/T03_participacao_composicao_resultados_por_ano.csv",
  "corpo/tabelas/T04_sintese_nao_ordinal_carteiras_operacionais.csv",
  "corpo/tabelas/T05_dimensoes_operacionais_carteiras.csv",
  "corpo/tabelas/T06_cautelas_metodologicas.csv",
  "corpo/graficos/G01_universos_estudo.png",
  "corpo/graficos/G02_composicao_institucional_rede.png",
  "corpo/graficos/G03_participacao_por_ano_escolar.png",
  "corpo/graficos/G04_participacao_proficiencia_observada.png",
  "corpo/graficos/G05_extensao_intensidade_carteiras.png",
  "corpo/graficos/G06_dimensoes_operacionais_carteiras.png",
  "anexos/tabelas/A01_universo_institucional_escolas.csv",
  "anexos/tabelas/A02_universo_operacional_2026.csv",
  "anexos/tabelas/A03_resultados_escola_ano_escolar.csv",
  "anexos/tabelas/A04_indice_operacional_escolas.csv",
  "anexos/tabelas/A05_componentes_indice_operacional.csv",
  "anexos/tabelas/A06_composicao_carteiras.csv",
  "anexos/tabelas/A07_diagnostico_educacional_carteiras.csv",
  "anexos/tabelas/A08_escolas_institucionais_nao_operacionais.csv",
  "catalogo_produtos_relatorio.csv"
)

destino_produto_21 <- function(relativo) {
  case_when(
    str_starts(relativo, "corpo/tabelas/") ~
      file.path("05_tabelas", "corpo", basename(relativo)),
    str_starts(relativo, "corpo/graficos/") ~
      file.path("06_graficos", basename(relativo)),
    str_starts(relativo, "anexos/tabelas/") ~
      file.path("05_tabelas", "anexos", basename(relativo)),
    relativo == "catalogo_produtos_relatorio.csv" ~
      file.path(
        "09_documentacao_reprodutibilidade",
        "catalogo_produtos_relatorio.csv"
      ),
    TRUE ~ NA_character_
  )
}

produtos_21 <- tibble(
  id_produto = paste0("M21_", seq_along(produtos_21_relativos)),
  caminho_origem = file.path(dir_resultados_21, produtos_21_relativos),
  caminho_destino = map_chr(produtos_21_relativos, destino_produto_21),
  formato = tools::file_ext(produtos_21_relativos)
) |>
  adicionar_hash_manifesto(manifestos$modulo_21, "21")

# -------------------------------------------------------------------
# 7. Lista branca — documentos principais 22 e 24
# -------------------------------------------------------------------

documentos_estaticos <- tribble(
  ~id_produto, ~modulo_origem, ~caminho_origem, ~caminho_destino,
  ~formato, ~tamanho_esperado, ~md5_esperado, ~sha256_esperado,

  "M22_QMD", "22",
  here("22_relatorio_tecnico.qmd"),
  "01_relatorio_tecnico/22_relatorio_tecnico.qmd",
  "qmd", 22916,
  "6b80b96ea944a64400bae5e27807d2a8",
  "2c250f7a6e1cb098ffe41f67ec0079b535815cf7d57f2d885de35940d5012933",

  "M22_HTML", "22",
  here("22_relatorio_tecnico.html"),
  "01_relatorio_tecnico/22_relatorio_tecnico.html",
  "html", 2400576,
  "260a839ee053c6ded601cd3cc4b08569",
  "eed938262127b0e2961e369674946d676adea32f03ba9a33ebc093f5ce1cebf3",

  "M24_QMD", "24",
  here("24_anexos_tecnicos.qmd"),
  "04_anexos_tecnicos/24_anexos_tecnicos.qmd",
  "qmd", 23937,
  "ab641468449bed86bc1b86578971d65d",
  "6ce94635e7b25c88a638a7c43329699c51faa58b38de375daf9d7556fa553e0e",

  "M24_HTML", "24",
  here("24_anexos_tecnicos.html"),
  "04_anexos_tecnicos/24_anexos_tecnicos.html",
  "html", 2590076,
  "f0ddf2c89fc66a6fcca8c21a40b5d14f",
  "79a26ddd41905f9eb0f07348d3714343e65d97474d9a05aa4493baf9b341b0dc",

  "M24_DOCX", "24",
  here("24_anexos_tecnicos.docx"),
  "04_anexos_tecnicos/24_anexos_tecnicos.docx",
  "docx", 613399,
  "58b9334e2a897804e0942564a28e01a0",
  "e69254f8553f51f6225d65a3bceef0d260c7ddb24ef4d5c1dda928c287c0b43e"
)

# -------------------------------------------------------------------
# 8. Lista branca — validação integrada do módulo 23
# -------------------------------------------------------------------

dir_validacao_23 <- here(
  "documentacao", "validacao_integrada",
  paste0("execucao_", CONFIG$execucao_modulo_23)
)

validacao_23_relativos <- c(
  "00_contrato_execucao.csv",
  "01_existencia_entradas.csv",
  "02_manifesto_entradas.csv",
  "03_validacao_manifestos_homologados.csv",
  "04_validacao_hashes_produtos.csv",
  "05_validacao_universos_integrada.csv",
  "06_validacao_chaves_duplicidades.csv",
  "07_validacao_coerencia_cruzada.csv",
  "08_validacao_produtos_modulo_21.csv",
  "09_validacao_relatorio_modulo_22.csv",
  "10_validacao_ambiente.csv",
  "11_resultado_testes.csv",
  "13_session_info.txt"
)

validacao_23_documentacao <- tibble(
  id_produto = paste0("M23_DOC_", seq_along(validacao_23_relativos)),
  caminho_origem = file.path(dir_validacao_23, validacao_23_relativos),
  caminho_destino = file.path(
    "08_metodologia_validacao",
    "execucao_20260802_211916",
    validacao_23_relativos
  ),
  formato = tools::file_ext(validacao_23_relativos)
) |>
  adicionar_hash_manifesto(manifestos$modulo_23, "23")

# O manifesto do módulo 23 não contém uma linha sobre si próprio.
# Como o repositório está bloqueado no commit homologado e alterações
# rastreadas são proibidas, sua integridade é ancorada no Git e os
# hashes observados são fixados para a cópia desta execução.
caminho_manifesto_23_autorreferente <- file.path(
  dir_validacao_23,
  "12_manifesto_produtos_modulo_23.csv"
)

if (!file.exists(caminho_manifesto_23_autorreferente)) {
  falhar(
    "Manifesto autorreferente do módulo 23 ausente: ",
    caminho_manifesto_23_autorreferente
  )
}

validacao_23_manifesto_autorreferente <- tribble(
  ~id_produto, ~modulo_origem, ~caminho_origem, ~caminho_destino,
  ~formato,
  "M23_DOC_MANIFESTO", "23",
  caminho_manifesto_23_autorreferente,
  paste0(
    "08_metodologia_validacao/execucao_20260802_211916/",
    "12_manifesto_produtos_modulo_23.csv"
  ),
  "csv"
) |>
  mutate(
    tamanho_esperado = as.numeric(file.info(caminho_origem)$size),
    md5_esperado = map_chr(caminho_origem, hash_md5),
    sha256_esperado = map_chr(caminho_origem, hash_sha256)
  )

validacao_23_produtos <- tribble(
  ~id_produto, ~caminho_origem, ~caminho_destino, ~formato,
  "M23_VALIDACAO_CSV",
  here("dados_finais", "validacao_integrada_final.csv"),
  "08_metodologia_validacao/produtos/validacao_integrada_final.csv",
  "csv",
  "M23_VALIDACAO_RDS",
  here("dados_finais", "validacao_integrada_final.rds"),
  "08_metodologia_validacao/produtos/validacao_integrada_final.rds",
  "rds"
) |>
  adicionar_hash_manifesto(manifestos$modulo_23, "23")

validacao_23_resumo <- tribble(
  ~id_produto, ~modulo_origem, ~caminho_origem, ~caminho_destino,
  ~formato, ~tamanho_esperado, ~md5_esperado, ~sha256_esperado,
  "M23_RESUMO", "23",
  file.path(dir_validacao_23, "14_resumo_execucao.txt"),
  paste0(
    "08_metodologia_validacao/execucao_20260802_211916/",
    "14_resumo_execucao.txt"
  ),
  "txt", 674,
  "639e4a7fae6e588945c397cd53c113e1",
  "3c7faa00c8616dd6132a767390c449da95e7e5d88e9ab397a878ea79f48a5aab"
)

# -------------------------------------------------------------------
# 9. Lista branca — scripts e fontes reprodutíveis
# -------------------------------------------------------------------

scripts_reprodutibilidade <- tribble(
  ~id_produto, ~modulo_origem, ~caminho_origem, ~caminho_destino,
  ~formato,
  "SCRIPT_15", "15", here("R", "15_consolidar_base_final.R"),
  "09_documentacao_reprodutibilidade/scripts/15_consolidar_base_final.R",
  "R",
  "SCRIPT_16", "16", here("R", "16_criar_perfil_escola.R"),
  "09_documentacao_reprodutibilidade/scripts/16_criar_perfil_escola.R",
  "R",
  "SCRIPT_17", "17", here("R", "17_construir_indice_complexidade.R"),
  "09_documentacao_reprodutibilidade/scripts/17_construir_indice_complexidade.R",
  "R",
  "SCRIPT_18", "18", here("R", "18_analisar_carteiras_assessoras.R"),
  "09_documentacao_reprodutibilidade/scripts/18_analisar_carteiras_assessoras.R",
  "R",
  "SCRIPT_19", "19", here("R", "19_gerar_fichas_escolas.R"),
  "09_documentacao_reprodutibilidade/scripts/19_gerar_fichas_escolas.R",
  "R",
  "SCRIPT_20", "20", here("R", "20_gerar_fichas_carteiras.R"),
  "09_documentacao_reprodutibilidade/scripts/20_gerar_fichas_carteiras.R",
  "R",
  "SCRIPT_21", "21", here("R", "21_tabelas_graficos_relatorio.R"),
  "09_documentacao_reprodutibilidade/scripts/21_tabelas_graficos_relatorio.R",
  "R",
  "SCRIPT_23", "23", here("R", "23_validacao_integrada_final.R"),
  "09_documentacao_reprodutibilidade/scripts/23_validacao_integrada_final.R",
  "R",
  "SCRIPT_25", "25", caminho_script,
  "09_documentacao_reprodutibilidade/scripts/25_pacote_entrega_final.R",
  "R"
) |>
  mutate(
    tamanho_esperado = map_dbl(
      caminho_origem,
      ~ if (file.exists(.x)) as.numeric(file.info(.x)$size) else NA_real_
    ),
    md5_esperado = map_chr(caminho_origem, hash_md5),
    sha256_esperado = map_chr(caminho_origem, hash_sha256)
  )

# -------------------------------------------------------------------
# 10. Consolidação da lista branca
# -------------------------------------------------------------------

preparar_manifesto_dinamico <- function(x) {
  x |>
    mutate(
      sha256_esperado = NA_character_
    ) |>
    select(
      id_produto, modulo_origem, caminho_origem, caminho_destino,
      formato, tamanho_esperado, md5_esperado, sha256_esperado
    )
}

lista_branca <- bind_rows(
  preparar_manifesto_dinamico(bases_15),
  preparar_manifesto_dinamico(bases_16),
  preparar_manifesto_dinamico(bases_17),
  preparar_manifesto_dinamico(bases_18),
  preparar_manifesto_dinamico(bases_20),
  preparar_manifesto_dinamico(produtos_21),
  documentos_estaticos,
  preparar_manifesto_dinamico(validacao_23_documentacao),
  validacao_23_manifesto_autorreferente,
  preparar_manifesto_dinamico(validacao_23_produtos),
  validacao_23_resumo,
  scripts_reprodutibilidade
) |>
  mutate(
    obrigatorio = TRUE,
    publicacao = case_when(
      str_starts(caminho_destino, "01_") ~ "institucional_principal",
      str_starts(caminho_destino, "04_") ~ "institucional_complementar",
      str_starts(caminho_destino, "05_") ~ "tabela_final",
      str_starts(caminho_destino, "06_") ~ "grafico_final",
      str_starts(caminho_destino, "07_") ~ "base_analitica",
      str_starts(caminho_destino, "08_") ~ "validacao",
      str_starts(caminho_destino, "09_") ~ "reprodutibilidade",
      TRUE ~ "outro"
    )
  )

validar_relativo(lista_branca$caminho_destino)

if (
  anyDuplicated(lista_branca$id_produto) > 0L ||
  anyDuplicated(lista_branca$caminho_destino) > 0L
) {
  falhar("Há duplicidade de ID ou destino na lista branca.")
}

if (
  CONFIG$permitir_modulo_26 ||
  CONFIG$permitir_modulo_27 ||
  any(str_detect(lista_branca$caminho_origem, "(^|[/\\\\])26_")) ||
  any(str_detect(lista_branca$caminho_origem, "(^|[/\\\\])27_"))
) {
  falhar(
    "Produtos dos módulos 26 ou 27 não podem integrar esta versão do pacote."
  )
}

# -------------------------------------------------------------------
# 11. Verificação das origens
# -------------------------------------------------------------------

origens_validadas <- lista_branca |>
  mutate(
    existe = file.exists(caminho_origem),
    eh_arquivo = existe & !dir.exists(caminho_origem),
    tamanho_observado = map_dbl(
      caminho_origem,
      ~ if (file.exists(.x) && !dir.exists(.x)) {
        as.numeric(file.info(.x)$size)
      } else {
        NA_real_
      }
    ),
    md5_observado = map_chr(caminho_origem, hash_md5),
    sha256_observado = map_chr(caminho_origem, hash_sha256),
    tamanho_aprovado = tamanho_observado == tamanho_esperado,
    md5_aprovado = md5_observado == str_to_lower(md5_esperado),
    sha256_aprovado = if_else(
      is.na(sha256_esperado) | sha256_esperado == "",
      TRUE,
      sha256_observado == str_to_lower(sha256_esperado)
    ),
    aprovado = existe & eh_arquivo &
      tamanho_observado > 0 &
      tamanho_aprovado & md5_aprovado & sha256_aprovado
  )

falhas_origem <- origens_validadas |>
  filter(!aprovado)

if (nrow(falhas_origem) > 0L) {
  print(
    falhas_origem |>
      select(
        id_produto, caminho_origem, existe, tamanho_aprovado,
        md5_aprovado, sha256_aprovado
      )
  )
  falhar("Uma ou mais origens homologadas falharam na validação.")
}

# -------------------------------------------------------------------
# 12. Validação dos universos sem recálculo analítico
# -------------------------------------------------------------------

caminho_testes_23 <- file.path(
  dir_validacao_23,
  "11_resultado_testes.csv"
)

testes_23 <- ler_csv_seguro(caminho_testes_23)

if (!all(testes_23$status == "APROVADO")) {
  falhar("A validação integrada do módulo 23 não está integralmente aprovada.")
}

if (
  nrow(testes_23) != 135L ||
  sum(testes_23$status == "APROVADO") != 135L
) {
  falhar(
    "Resultado do módulo 23 divergente do homologado: esperado 135/135."
  )
}

validacao_universos <- ler_csv_seguro(
  file.path(
    dir_validacao_23,
    "05_validacao_universos_integrada.csv"
  )
)

if ("aprovado" %in% names(validacao_universos)) {
  aprovados_universos <- validacao_universos$aprovado
  if (!is.logical(aprovados_universos)) {
    aprovados_universos <- str_to_upper(
      str_trim(as.character(aprovados_universos))
    ) %in% c("TRUE", "VERDADEIRO", "SIM", "S", "1", "APROVADO")
  }

  if (!all(aprovados_universos)) {
    falhar("A validação integrada dos universos não está aprovada.")
  }
}

# -------------------------------------------------------------------
# 13. Estrutura transacional
# -------------------------------------------------------------------

dir_entrega <- here("entrega_final")
dir_versoes <- file.path(dir_entrega, "versoes")
dir_registros <- file.path(dir_entrega, "registros_execucao")
dir_transacoes <- file.path(dir_entrega, ".transacoes")

dir_versao_final <- file.path(dir_versoes, CONFIG$id_pacote)
dir_registro_execucao <- file.path(
  dir_registros,
  paste0("execucao_", id_execucao)
)
dir_transacao <- file.path(
  dir_transacoes,
  paste0(CONFIG$id_pacote, "_", id_execucao)
)
dir_candidato <- file.path(dir_transacao, "candidato")

if (dir.exists(dir_versao_final)) {
  falhar(
    "A versão do pacote já existe e não será sobrescrita: ",
    dir_versao_final
  )
}

walk(
  c(dir_versoes, dir_registros, dir_transacoes, dir_registro_execucao),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

dir.create(dir_candidato, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------
# 14. Documentos gerados pelo próprio módulo 25
# -------------------------------------------------------------------

dir_leia_me <- file.path(dir_candidato, "00_leia_me")
dir_sumario <- file.path(dir_candidato, "02_sumario_executivo")
dir_apresentacao <- file.path(dir_candidato, "03_apresentacao")
dir_manifestos <- file.path(dir_candidato, "10_manifestos_integridade")

walk(
  c(dir_leia_me, dir_sumario, dir_apresentacao, dir_manifestos),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

writeLines(
  c(
    "# Pacote institucional — estudo_descritivo",
    "",
    paste0("Versão: `", CONFIG$id_pacote, "`"),
    paste0("Fase: `", CONFIG$fase_entrega, "`"),
    paste0("Branch: `", CONFIG$branch_esperada, "`"),
    paste0("Commit homologado: `", CONFIG$commit_esperado, "`"),
    "",
    "## Escopo",
    "",
    paste(
      "Estudo observacional e descritivo do assessoramento pedagógico",
      "às escolas da Rede Municipal de Ensino de Porto Alegre."
    ),
    "Não há pretensão causal.",
    "",
    "## Universos oficiais",
    "",
    "- 56 escolas no universo institucional;",
    "- 280 combinações potenciais escola × ano escolar;",
    "- 54 escolas no universo avaliativo de 2025;",
    "- 56 escolas no universo avaliativo de 2026;",
    "- 53 escolas no universo operacional de 2026;",
    "- 11 carteiras gerenciais;",
    "- exposição positiva ao assessoramento igual a zero em 2025;",
    "- escolas institucionais não operacionais: ESC_001, ESC_055 e ESC_056.",
    "",
    "## Formatos",
    "",
    "- HTML: formato institucional principal;",
    "- DOCX: formato complementar;",
    "- QMD e R: fontes reprodutíveis;",
    "- CSV: formato aberto principal para tabelas e bases;",
    "- RDS: formato técnico complementar para R.",
    "",
    "## Cautelas",
    "",
    "- resultados educacionais não avaliam assessoras;",
    "- resultados educacionais não integram o índice operacional;",
    "- o índice operacional não representa integralmente a carga real;",
    "- não há rankings, percentis ou classificações de carga;",
    "- os universos institucional, operacional, avaliativo e gerencial são distintos.",
    "",
    "## Produtos pendentes",
    "",
    "O sumário executivo (módulo 26) e a apresentação institucional",
    "(módulo 27) ainda não integram esta versão."
  ),
  file.path(dir_leia_me, "LEIA_ME.md"),
  useBytes = TRUE
)

writeLines(
  c(
    "NOTA DE ESCOPO",
    paste0("Pacote: ", CONFIG$id_pacote),
    "Conteúdo: produtos homologados dos módulos 15 a 24.",
    "Não contém produtos dos módulos 26 e 27.",
    "Nenhum resultado foi recalculado pelo módulo 25."
  ),
  file.path(dir_leia_me, "NOTA_DE_ESCOPO.txt"),
  useBytes = TRUE
)

writeLines(
  c(
    "PRODUTO NÃO DISPONÍVEL NESTA VERSÃO",
    "Diretório reservado ao sumário executivo do módulo 26.",
    "O módulo ainda não foi desenvolvido nem homologado."
  ),
  file.path(dir_sumario, "NAO_DISPONIVEL_NESTA_VERSAO.txt"),
  useBytes = TRUE
)

writeLines(
  c(
    "PRODUTO NÃO DISPONÍVEL NESTA VERSÃO",
    "Diretório reservado à apresentação institucional do módulo 27.",
    "O módulo ainda não foi desenvolvido nem homologado."
  ),
  file.path(dir_apresentacao, "NAO_DISPONIVEL_NESTA_VERSAO.txt"),
  useBytes = TRUE
)

# -------------------------------------------------------------------
# 15. Cópia dos produtos homologados
# -------------------------------------------------------------------

walk2(
  origens_validadas$caminho_origem,
  file.path(dir_candidato, origens_validadas$caminho_destino),
  copiar_um
)

# -------------------------------------------------------------------
# 16. Verificação pós-cópia
# -------------------------------------------------------------------

copias_validadas <- origens_validadas |>
  mutate(
    caminho_copia = file.path(dir_candidato, caminho_destino),
    copia_existe = file.exists(caminho_copia),
    tamanho_copia = map_dbl(
      caminho_copia,
      ~ if (file.exists(.x)) as.numeric(file.info(.x)$size) else NA_real_
    ),
    md5_copia = map_chr(caminho_copia, hash_md5),
    sha256_copia = map_chr(caminho_copia, hash_sha256),
    copia_igual_origem =
      copia_existe &
      tamanho_copia == tamanho_observado &
      md5_copia == md5_observado &
      sha256_copia == sha256_observado
  )

if (any(!copias_validadas$copia_igual_origem)) {
  print(
    copias_validadas |>
      filter(!copia_igual_origem) |>
      select(id_produto, caminho_origem, caminho_copia)
  )
  falhar("Uma ou mais cópias divergiram das origens.")
}

# -------------------------------------------------------------------
# 17. Bloqueio de conteúdo proibido e arquivos extras
# -------------------------------------------------------------------

padroes_proibidos <- c(
  "(^|/)\\.Rproj\\.user(/|$)",
  "(^|/)\\.Rhistory$",
  "(^|/)\\.RData$",
  "(^|/)\\.Ruserdata$",
  "(^|/)renv/cache(/|$)",
  "(^|/)_cache(/|$)",
  "(^|/)_freeze(/|$)",
  "\\.tmp$",
  "\\.temp$",
  "\\.bak$",
  "\\.lock$",
  "(^|/)~\\$",
  "(^|/)\\.DS_Store$",
  "(^|/)Thumbs\\.db$",
  "(^|/)__pycache__(/|$)",
  "(^|/)diagnostico_local(/|$)"
)

arquivos_antes_manifestos <- listar_arquivos(dir_candidato)

proibidos <- arquivos_antes_manifestos |>
  filter(
    map_lgl(
      caminho_relativo,
      ~ any(str_detect(.x, regex(padroes_proibidos, ignore_case = TRUE)))
    )
  )

if (nrow(proibidos) > 0L) {
  print(proibidos)
  falhar("Conteúdo proibido encontrado no pacote candidato.")
}

if (
  any(str_detect(
    arquivos_antes_manifestos$caminho_relativo,
    "(^|/)26_|(^|/)27_"
  ))
) {
  falhar("Produto dos módulos 26 ou 27 encontrado no pacote candidato.")
}

# -------------------------------------------------------------------
# 18. Inventário, manifestos e registros
# -------------------------------------------------------------------

registro_git <- tibble(
  campo = c(
    "branch_esperada",
    "branch_observada",
    "commit_esperado",
    "commit_observado",
    "working_tree_sem_alteracao_bloqueante"
  ),
  valor = c(
    CONFIG$branch_esperada,
    branch_observada,
    CONFIG$commit_esperado,
    commit_observado,
    "TRUE"
  )
)

write_csv(
  registro_git,
  file.path(dir_manifestos, "registro_git.csv"),
  na = ""
)

write_csv(
  status_git_classificado,
  file.path(dir_manifestos, "registro_status_git.csv"),
  na = ""
)

registro_ambiente <- c(
  paste0("id_execucao: ", id_execucao),
  paste0("data_hora: ", format(instante_execucao, "%Y-%m-%d %H:%M:%S %Z")),
  paste0("R: ", R.version.string),
  paste0("plataforma: ", R.version$platform),
  paste0("diretorio_projeto: ", normalizar_caminho(here())),
  paste0("script: ", normalizar_caminho(caminho_script)),
  paste0("md5_script: ", hash_md5(caminho_script)),
  paste0("sha256_script: ", hash_sha256(caminho_script))
)

writeLines(
  registro_ambiente,
  file.path(dir_manifestos, "registro_ambiente.txt"),
  useBytes = TRUE
)

writeLines(
  capture.output(sessionInfo()),
  file.path(dir_manifestos, "session_info.txt"),
  useBytes = TRUE
)

registro_origens <- copias_validadas |>
  transmute(
    id_produto,
    modulo_origem,
    caminho_origem = normalizar_caminho(caminho_origem),
    caminho_destino,
    formato,
    tamanho_esperado,
    tamanho_observado,
    md5_esperado,
    md5_observado,
    sha256_esperado,
    sha256_observado,
    origem_aprovada = aprovado,
    copia_igual_origem
  )

write_csv(
  registro_origens,
  file.path(dir_manifestos, "registro_origens.csv"),
  na = ""
)

registro_exclusoes <- tribble(
  ~classe, ~padrao_ou_caminho, ~motivo, ~bloqueante,
  "cache", ".Rproj.user/", "Cache local do RStudio.", TRUE,
  "temporario", "*.tmp; *.temp; *.bak; *.lock", "Arquivos transitórios.", TRUE,
  "cache", "_cache/; _freeze/; renv/cache/", "Caches de renderização/ambiente.", TRUE,
  "diagnostico", "diagnostico_local/", "Diagnóstico local não institucional.", TRUE,
  "futuro", "módulos 26 e 27", "Ainda não desenvolvidos e homologados.", TRUE,
  "historico", "scripts históricos/concorrentes", "Somente scripts canônicos.", TRUE,
  "fichas", "fichas individuais dos módulos 19 e 20",
  "Produtos condicionais não incluídos nesta versão.", FALSE
)

write_csv(
  registro_exclusoes,
  file.path(dir_manifestos, "registro_exclusoes.csv"),
  na = ""
)

validacoes_pacote <- tribble(
  ~teste, ~esperado, ~observado, ~aprovado,
  "Branch",
  CONFIG$branch_esperada, branch_observada,
  branch_observada == CONFIG$branch_esperada,
  "Commit",
  CONFIG$commit_esperado, commit_observado,
  commit_observado == CONFIG$commit_esperado,
  "Execução módulo 21",
  CONFIG$execucao_modulo_21, CONFIG$execucao_modulo_21, TRUE,
  "Execução módulo 23",
  CONFIG$execucao_modulo_23, CONFIG$execucao_modulo_23, TRUE,
  "Testes integrados",
  "135 aprovados", paste0(sum(testes_23$status == "APROVADO"), " aprovados"),
  nrow(testes_23) == 135L && all(testes_23$status == "APROVADO"),
  "Produtos obrigatórios",
  as.character(nrow(lista_branca)),
  as.character(sum(origens_validadas$aprovado)),
  all(origens_validadas$aprovado),
  "Cópias idênticas",
  as.character(nrow(lista_branca)),
  as.character(sum(copias_validadas$copia_igual_origem)),
  all(copias_validadas$copia_igual_origem),
  "Produtos módulo 26",
  "0", "0", !CONFIG$permitir_modulo_26,
  "Produtos módulo 27",
  "0", "0", !CONFIG$permitir_modulo_27
)

write_csv(
  validacoes_pacote,
  file.path(dir_manifestos, "resultado_validacoes_pacote.csv"),
  na = ""
)

if (any(!validacoes_pacote$aprovado)) {
  falhar("Uma ou mais validações finais do pacote falharam.")
}

manifesto_integridade <- registro_origens |>
  transmute(
    id_produto,
    modulo_origem,
    caminho_destino,
    tamanho_esperado,
    tamanho_origem = tamanho_observado,
    md5_esperado,
    md5_origem = md5_observado,
    sha256_esperado,
    sha256_origem = sha256_observado,
    origem_aprovada,
    copia_igual_origem
  )

write_csv(
  manifesto_integridade,
  file.path(dir_manifestos, "manifesto_integridade.csv"),
  na = ""
)

# JSON sem dependência obrigatória adicional.
if (requireNamespace("jsonlite", quietly = TRUE)) {
  jsonlite::write_json(
    manifesto_integridade,
    file.path(dir_manifestos, "manifesto_integridade.json"),
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )
} else {
  writeLines(
    c(
      "JSON não gerado: pacote `jsonlite` não disponível.",
      "O manifesto CSV permanece canônico."
    ),
    file.path(dir_manifestos, "manifesto_integridade_JSON_NAO_GERADO.txt"),
    useBytes = TRUE
  )
}

writeLines(
  c(
    "MÓDULO 25 — PACOTE DE ENTREGA FINAL",
    paste0("Execução: ", id_execucao),
    paste0("Pacote: ", CONFIG$id_pacote),
    paste0("Fase: ", CONFIG$fase_entrega),
    paste0("Branch: ", branch_observada),
    paste0("Commit: ", commit_observado),
    paste0("Produtos homologados copiados: ", nrow(lista_branca)),
    paste0("Testes integrados do módulo 23: ", nrow(testes_23)),
    "Resultado: APROVADO PARA PROMOÇÃO",
    "Módulos 26 e 27: não incluídos nesta versão."
  ),
  file.path(dir_manifestos, "RESUMO_PROMOCAO.txt"),
  useBytes = TRUE
)

# O inventário é criado por último entre os documentos internos.
# Ele registra todos os demais arquivos e exclui somente a si próprio,
# pois incorporar o próprio hash geraria autorreferência instável.
inventario_sem_proprio <- listar_arquivos(dir_candidato)

total_arquivos_inventariados <- nrow(inventario_sem_proprio)
total_arquivos_pacote <- total_arquivos_inventariados + 1L

inventario_arquivos <- inventario_sem_proprio |>
  mutate(
    id_pacote = CONFIG$id_pacote,
    versao_pacote = CONFIG$versao_pacote,
    fase_entrega = CONFIG$fase_entrega,
    escopo_inventario =
      "todos_os_arquivos_exceto_o_proprio_inventario",
    motivo_exclusao_proprio_arquivo =
      "autorreferencia_de_hash",
    total_arquivos_inventariados =
      total_arquivos_inventariados,
    total_arquivos_pacote =
      total_arquivos_pacote,
    branch = branch_observada,
    commit = commit_observado,
    data_inventario = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S %Z"
    )
  ) |>
  select(
    id_pacote,
    versao_pacote,
    fase_entrega,
    escopo_inventario,
    motivo_exclusao_proprio_arquivo,
    total_arquivos_inventariados,
    total_arquivos_pacote,
    caminho_relativo,
    tamanho_bytes,
    md5,
    sha256,
    branch,
    commit,
    data_inventario
  )

if (total_arquivos_inventariados != 94L) {
  falhar(
    "Contagem pré-inventário divergente. Observado: ",
    total_arquivos_inventariados,
    "; esperado: 94."
  )
}

if (total_arquivos_pacote != 95L) {
  falhar(
    "Contagem total prevista do pacote divergente. Observado: ",
    total_arquivos_pacote,
    "; esperado: 95."
  )
}

write_csv(
  inventario_arquivos,
  file.path(dir_manifestos, "inventario_arquivos.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 19. Fechamento do inventário integral
# -------------------------------------------------------------------

inventario_final_candidato <- listar_arquivos(dir_candidato)

if (
  any(is.na(inventario_final_candidato$md5)) ||
  any(is.na(inventario_final_candidato$sha256)) ||
  any(inventario_final_candidato$tamanho_bytes <= 0)
) {
  falhar("Inventário final contém arquivo vazio ou sem hash.")
}

# -------------------------------------------------------------------
# 20. Promoção imutável
# -------------------------------------------------------------------

ok_promocao <- file.rename(dir_candidato, dir_versao_final)

if (!isTRUE(ok_promocao)) {
  # Fallback controlado para volumes em que rename não é possível.
  dir.create(dir_versao_final, recursive = TRUE, showWarnings = FALSE)

  itens <- list.files(
    dir_candidato,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  resultado <- file.copy(
    itens,
    dir_versao_final,
    recursive = TRUE,
    overwrite = FALSE,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  if (length(resultado) != length(itens) || !all(resultado)) {
    unlink(dir_versao_final, recursive = TRUE, force = TRUE)
    falhar("Falha na promoção da versão; destino incompleto removido.")
  }
}

# -------------------------------------------------------------------
# 21. Verificação pós-promoção
# -------------------------------------------------------------------

manifesto_candidato <- inventario_final_candidato |>
  select(
    caminho_relativo,
    tamanho_candidato = tamanho_bytes,
    md5_candidato = md5,
    sha256_candidato = sha256
  )

manifesto_promovido <- listar_arquivos(dir_versao_final) |>
  select(
    caminho_relativo,
    tamanho_promovido = tamanho_bytes,
    md5_promovido = md5,
    sha256_promovido = sha256
  )

comparacao_promocao <- full_join(
  manifesto_candidato,
  manifesto_promovido,
  by = "caminho_relativo"
) |>
  mutate(
    existe_candidato = !is.na(md5_candidato),
    existe_promovido = !is.na(md5_promovido),
    tamanho_identico = tamanho_candidato == tamanho_promovido,
    md5_identico = md5_candidato == md5_promovido,
    sha256_identico = sha256_candidato == sha256_promovido,
    aprovado = existe_candidato & existe_promovido &
      tamanho_identico & md5_identico & sha256_identico
  )

write_csv(
  comparacao_promocao,
  file.path(dir_registro_execucao, "01_verificacao_pos_promocao.csv"),
  na = ""
)

if (any(!comparacao_promocao$aprovado)) {
  unlink(dir_versao_final, recursive = TRUE, force = TRUE)
  falhar(
    "Falha na verificação pós-promoção; versão promovida removida."
  )
}

# -------------------------------------------------------------------
# 22. Registro externo da execução e apontador
# -------------------------------------------------------------------

write_csv(
  origens_validadas,
  file.path(dir_registro_execucao, "02_validacao_origens.csv"),
  na = ""
)

write_csv(
  copias_validadas,
  file.path(dir_registro_execucao, "03_validacao_copias.csv"),
  na = ""
)

write_csv(
  validacoes_pacote,
  file.path(dir_registro_execucao, "04_resultado_validacoes.csv"),
  na = ""
)

writeLines(
  c(
    paste0("id_pacote=", CONFIG$id_pacote),
    paste0("versao=", CONFIG$versao_pacote),
    paste0("fase=", CONFIG$fase_entrega),
    paste0("branch=", branch_observada),
    paste0("commit=", commit_observado),
    paste0("execucao_modulo_25=", id_execucao),
    paste0(
      "caminho=",
      normalizar_caminho(dir_versao_final)
    )
  ),
  file.path(dir_entrega, "VERSAO_ATUAL.txt"),
  useBytes = TRUE
)

writeLines(
  c(
    "MÓDULO 25 CONCLUÍDO COM SUCESSO",
    paste0("Execução: ", id_execucao),
    paste0("Pacote: ", CONFIG$id_pacote),
    paste0("Produtos da lista branca: ", nrow(lista_branca)),
    paste0(
      "Arquivos integrais promovidos: ",
      nrow(manifesto_promovido)
    ),
    paste0("Destino: ", normalizar_caminho(dir_versao_final)),
    "Resultado: APROVADO",
    "Não orientar commit antes da homologação dos produtos."
  ),
  file.path(dir_registro_execucao, "05_resumo_execucao.txt"),
  useBytes = TRUE
)

# Limpeza da transação vazia ou residual.
if (dir.exists(dir_transacao)) {
  unlink(dir_transacao, recursive = TRUE, force = TRUE)
}

cat(
  paste0(
    "\n============================================================\n",
    "MÓDULO 25 CONCLUÍDO COM SUCESSO\n",
    "Execução: ", id_execucao, "\n",
    "Pacote: ", CONFIG$id_pacote, "\n",
    "Produtos homologados copiados: ", nrow(lista_branca), "\n",
    "Arquivos integrais promovidos: ", nrow(manifesto_promovido), "\n",
    "Destino: ", normalizar_caminho(dir_versao_final), "\n",
    "Resultado: APROVADO\n",
    "Não realizar commit antes da homologação.\n",
    "============================================================\n"
  )
)
