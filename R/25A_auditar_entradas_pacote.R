# ===================================================================
# 25A_auditar_entradas_pacote.R
# Diagnóstico somente leitura para preparação do módulo 25
# ===================================================================

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
})

branch_esperada <- "refatoracao_modulo_21"
commit_esperado <- "c770cf6117a6408d787d6fd1eb3f51f48b4b1ca6"

executar_git <- function(argumentos) {
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
  
  if (status != 0L) {
    stop(
      "Falha ao executar Git: git ",
      paste(argumentos, collapse = " "),
      "\n",
      paste(saida, collapse = "\n")
    )
  }
  
  str_squish(paste(saida, collapse = " "))
}

branch_observada <- executar_git(c("branch", "--show-current"))
commit_observado <- executar_git(c("rev-parse", "HEAD"))
status_git <- executar_git(c("status", "--porcelain"))

if (!identical(branch_observada, branch_esperada)) {
  stop(
    "Branch divergente. Observada: ", branch_observada,
    "; esperada: ", branch_esperada, "."
  )
}

if (!identical(commit_observado, commit_esperado)) {
  stop(
    "Commit divergente. Observado: ", commit_observado,
    "; esperado: ", commit_esperado, "."
  )
}

hash_md5 <- function(caminho) {
  if (!file.exists(caminho) || dir.exists(caminho)) {
    return(NA_character_)
  }
  unname(tools::md5sum(caminho)[[1]])
}

hash_sha256 <- function(caminho) {
  if (!file.exists(caminho) || dir.exists(caminho)) {
    return(NA_character_)
  }
  
  if (!requireNamespace("openssl", quietly = TRUE)) {
    return(NA_character_)
  }
  
  as.character(openssl::sha256(file(caminho, "rb")))
}

arquivos <- tribble(
  ~classe, ~modulo, ~caminho_relativo,
  
  "documento_principal", 22L,
  "22_relatorio_tecnico.qmd",
  
  "documento_principal", 22L,
  "22_relatorio_tecnico.html",
  
  "documento_principal", 24L,
  "24_anexos_tecnicos.qmd",
  
  "documento_principal", 24L,
  "24_anexos_tecnicos.html",
  
  "documento_principal", 24L,
  "24_anexos_tecnicos.docx",
  
  "validacao", 23L,
  "dados_finais/validacao_integrada_final.csv",
  
  "validacao", 23L,
  "dados_finais/validacao_integrada_final.rds",
  
  "validacao", 23L,
  paste0(
    "documentacao/validacao_integrada/",
    "execucao_20260802_211916/14_resumo_execucao.txt"
  )
)

inventario <- arquivos |>
  mutate(
    caminho_absoluto = map_chr(
      caminho_relativo,
      ~ normalizePath(
        here(.x),
        winslash = "/",
        mustWork = FALSE
      )
    ),
    existe = file.exists(caminho_absoluto),
    eh_diretorio = dir.exists(caminho_absoluto),
    tamanho_bytes = map_dbl(
      caminho_absoluto,
      ~ if (file.exists(.x) && !dir.exists(.x)) {
        as.numeric(file.info(.x)$size)
      } else {
        NA_real_
      }
    ),
    md5 = map_chr(caminho_absoluto, hash_md5),
    sha256 = map_chr(caminho_absoluto, hash_sha256),
    possui_conteudo = existe & !eh_diretorio &
      !is.na(tamanho_bytes) & tamanho_bytes > 0
  )

if (any(!inventario$possui_conteudo)) {
  print(inventario |> filter(!possui_conteudo))
  stop("Há arquivo obrigatório ausente, vazio ou inválido.")
}

dir_saida <- here(
  "documentacao",
  "pacote_entrega_final",
  "diagnostico_previo"
)

dir.create(
  dir_saida,
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  inventario,
  file.path(dir_saida, "01_inventario_documentos_principais.csv"),
  na = ""
)

registro_git <- tibble(
  campo = c(
    "branch_esperada",
    "branch_observada",
    "commit_esperado",
    "commit_observado",
    "working_tree_limpo"
  ),
  valor = c(
    branch_esperada,
    branch_observada,
    commit_esperado,
    commit_observado,
    as.character(identical(status_git, ""))
  )
)

write_csv(
  registro_git,
  file.path(dir_saida, "02_registro_git.csv"),
  na = ""
)

writeLines(
  c(
    "AUDITORIA PRÉVIA DO MÓDULO 25",
    paste0("Branch: ", branch_observada),
    paste0("Commit: ", commit_observado),
    paste0(
      "Working tree limpo: ",
      ifelse(identical(status_git, ""), "SIM", "NÃO")
    ),
    paste0("Arquivos auditados: ", nrow(inventario)),
    paste0(
      "Arquivos válidos: ",
      sum(inventario$possui_conteudo)
    ),
    paste0(
      "SHA-256 disponível: ",
      ifelse(all(!is.na(inventario$sha256)), "SIM", "NÃO")
    )
  ),
  file.path(dir_saida, "03_resumo_auditoria.txt"),
  useBytes = TRUE
)

cat(
  "\nAuditoria prévia concluída.\n",
  "Saída: ", dir_saida, "\n",
  "Arquivos auditados: ", nrow(inventario), "\n",
  "Working tree limpo: ",
  ifelse(identical(status_git, ""), "SIM", "NÃO"),
  "\n",
  sep = ""
)