# ===================================================================
# 21_tabelas_graficos_relatorio.R
# Revisão integral consolidada após auditoria — v2
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# ===================================================================
#
# OBJETIVO
#
# Preparar, sem recalcular os produtos analíticos homologados dos módulos
# 15 a 18, e conferindo as dependências editoriais dos módulos 19 e 20,
# as tabelas e os gráficos canônicos destinados:
#
#   1. ao relatório técnico (módulo 22);
#   2. aos anexos técnicos (módulo 24);
#   3. ao sumário executivo (módulo 26);
#   4. à apresentação institucional (módulo 27).
#
# UNIVERSOS HOMOLOGADOS
#
# - universo institucional: 56 escolas;
# - universo operacional de 2026: 53 escolas;
# - universo institucional não operacional: ESC_001, ESC_055 e ESC_056;
# - universo avaliativo de 2025: 54 escolas;
# - universo avaliativo de 2026: 56 escolas;
# - carteiras gerenciais de 2026: 11 assessoras.
#
# PRINCÍPIOS METODOLÓGICOS
#
# - estudo observacional e descritivo, sem identificação causal;
# - a primeira avaliação de 2025 é linha de base pré-programa (dose zero);
# - resultados educacionais não integram o índice operacional;
# - resultados educacionais não avaliam as assessoras;
# - participação e composição acompanham as descrições de desempenho;
# - assessora_gerencial_2026 e assessora_vinculo_administrativo são distintas;
# - o índice operacional não equivale à carga real total;
# - não produzir ranking, posição, faixa, quartil analítico do índice,
#   percentil, normalização nova, cenário alternativo ou classificação
#   de assessoras como sobrecarregadas ou subutilizadas.
#
# ENGENHARIA
#
# - caminho, branch e commit-base bloqueados;
# - presença simultânea de CSV e RDS;
# - hashes extraídos e conferidos diretamente nos manifestos homologados;
# - equivalência estrutural e semântica CSV–RDS;
# - escrita em pasta candidata;
# - validação integral antes da promoção;
# - preservação histórica, promoção transacional e rollback;
# - verificação de hashes após a promoção.
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
commit_base_integracao <- "43b782747cbb9ba6611edac73377bf00ca33c181"

caminho_script <- here("R", "21_tabelas_graficos_relatorio.R")
caminho_relativo_script <- file.path("R", "21_tabelas_graficos_relatorio.R")

ids_nao_operacionais <- c("ESC_001", "ESC_055", "ESC_056")

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
  diagnostico_educacional_escola_serie_csv = here(
    "dados_finais", "componentes_educacionais_escola_serie.csv"
  ),
  diagnostico_educacional_escola_serie_rds = here(
    "dados_finais", "componentes_educacionais_escola_serie.rds"
  ),

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
  indice_fichas_carteiras_rds = here("dados_finais", "indice_fichas_carteiras.rds")
)

ler_manifesto_produtos <- function(caminho, modulo) {
  manifesto <- read_csv(
    caminho,
    show_col_types = FALSE,
    progress = FALSE,
    na = c("", "NA")
  )

  colunas_minimas <- c("caminho", "tamanho_bytes", "md5")
  ausentes <- setdiff(colunas_minimas, names(manifesto))

  if (length(ausentes) > 0L) {
    stop(
      "Manifesto do módulo ", modulo,
      " sem colunas obrigatórias: ",
      paste(ausentes, collapse = ", "),
      "."
    )
  }

  if (!"produto" %in% names(manifesto) && !"arquivo" %in% names(manifesto)) {
    stop(
      "Manifesto do módulo ", modulo,
      " deve possuir a coluna `produto` ou `arquivo`."
    )
  }

  manifesto
}

localizar_hash_manifesto <- function(manifesto, caminho, modulo) {
  alvo <- basename(caminho)

  candidatos <- manifesto |>
    mutate(
      caminho_base = basename(caminho),
      identificador = if ("produto" %in% names(manifesto)) {
        as.character(produto)
      } else {
        as.character(arquivo)
      }
    ) |>
    filter(caminho_base == alvo | basename(identificador) == alvo)

  if (nrow(candidatos) != 1L) {
    stop(
      "O produto `", alvo, "` não foi localizado de forma única ",
      "no manifesto homologado do módulo ", modulo, "."
    )
  }

  str_to_lower(candidatos$md5[[1]])
}

manifestos_lidos <- list(
  modulo_15 = ler_manifesto_produtos(manifestos[["modulo_15"]], "15"),
  modulo_16 = ler_manifesto_produtos(manifestos[["modulo_16"]], "16"),
  modulo_17 = ler_manifesto_produtos(manifestos[["modulo_17"]], "17"),
  modulo_18 = ler_manifesto_produtos(manifestos[["modulo_18"]], "18"),
  modulo_19 = ler_manifesto_produtos(manifestos[["modulo_19"]], "19"),
  modulo_20 = ler_manifesto_produtos(manifestos[["modulo_20"]], "20")
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
  diagnostico_educacional_escola_serie_csv = "modulo_17",
  diagnostico_educacional_escola_serie_rds = "modulo_17",

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
  indice_fichas_carteiras_rds = "modulo_20"
)

if (!setequal(names(entradas), names(modulo_por_entrada))) {
  stop(
    "O contrato entre entradas e manifestos está incompleto. ",
    "Sem módulo: ",
    paste(setdiff(names(entradas), names(modulo_por_entrada)), collapse = ", "),
    "; sem entrada: ",
    paste(setdiff(names(modulo_por_entrada), names(entradas)), collapse = ", "),
    "."
  )
}

hashes_esperados <- imap_chr(
  entradas,
  function(caminho, nome_entrada) {
    modulo <- modulo_por_entrada[[nome_entrada]]
    localizar_hash_manifesto(
      manifestos_lidos[[modulo]],
      caminho,
      str_remove(modulo, "modulo_")
    )
  }
)

dir_documentacao_base <- here("documentacao", "relatorio")
dir_execucao <- file.path(dir_documentacao_base, paste0("execucao_", id_execucao))

dir_transacao <- here(
  "resultados", "relatorio", "transacoes",
  paste0("execucao_", id_execucao)
)
dir_candidatos <- file.path(dir_transacao, "candidatos")
dir_rollback <- file.path(dir_transacao, "rollback")

dir_destino_base <- here("resultados", "relatorio")
dir_destino <- file.path(dir_destino_base, paste0("execucao_", id_execucao))
dir_corpo_tabelas <- file.path(dir_destino, "corpo", "tabelas")
dir_corpo_graficos <- file.path(dir_destino, "corpo", "graficos")
dir_anexos_tabelas <- file.path(dir_destino, "anexos", "tabelas")

dir_candidato_corpo_tabelas <- file.path(dir_candidatos, "corpo", "tabelas")
dir_candidato_corpo_graficos <- file.path(dir_candidatos, "corpo", "graficos")
dir_candidato_anexos_tabelas <- file.path(dir_candidatos, "anexos", "tabelas")

dir.create(
  dir_execucao,
  recursive = TRUE,
  showWarnings = FALSE
)

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
  if (!is.null(status) && status != 0L) {
    stop("Falha ao executar Git: git ", paste(argumentos, collapse = " "))
  }
  stringr::str_squish(as.character(saida))
}

obter_branch <- function() {
  executar_git(c("-C", shQuote(here()), "branch", "--show-current"))[[1]]
}

obter_commit <- function() {
  executar_git(c("-C", shQuote(here()), "rev-parse", "HEAD"))[[1]]
}

caminho_normalizado <- function(x, must_work = TRUE) {
  normalizePath(x, winslash = "/", mustWork = must_work)
}

tipo_canonico <- function(x) {
  case_when(
    inherits(x, "Date") ~ "date",
    inherits(x, "POSIXct") ~ "datetime",
    is.logical(x) ~ "logical",
    is.integer(x) ~ "integer",
    is.double(x) ~ "double",
    is.character(x) ~ "character",
    TRUE ~ paste(class(x), collapse = " | ")
  )
}

converter_logico <- function(x, variavel) {
  texto <- str_to_lower(str_squish(as.character(x)))

  resultado <- case_when(
    is.na(texto) | texto == "" ~ NA,
    texto %in% c("true", "t", "1", "sim", "s") ~ TRUE,
    texto %in% c("false", "f", "0", "nao", "não", "n") ~ FALSE,
    TRUE ~ NA
  )

  invalidos <- !is.na(texto) & texto != "" & is.na(resultado)

  if (any(invalidos)) {
    stop(
      "Valor lógico inválido em `", variavel, "`: ",
      paste(unique(texto[invalidos]), collapse = ", "),
      "."
    )
  }

  resultado
}

converter_por_modelo <- function(x, modelo, variavel) {
  tipo <- tipo_canonico(modelo)

  switch(
    tipo,
    character = as.character(x),
    logical = converter_logico(x, variavel),
    integer = suppressWarnings(as.integer(x)),
    double = suppressWarnings(as.double(x)),
    date = as.Date(x),
    datetime = as.POSIXct(x, tz = "UTC"),
    stop("Tipo não suportado em `", variavel, "`: ", tipo, ".")
  )
}

ler_csv_por_modelo <- function(caminho, modelo) {
  bruto <- read_csv(
    caminho,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    progress = FALSE,
    na = c("", "NA", "NaN"),
    trim_ws = FALSE,
    name_repair = "minimal"
  )

  if (!identical(names(bruto), names(modelo))) {
    stop("Estrutura de colunas divergente no CSV: ", caminho, ".")
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

comparar_coluna_semantica <- function(
    x_csv,
    x_rds,
    tolerancia_absoluta = 1e-10,
    tolerancia_relativa = 1e-10
) {
  if (!identical(is.na(x_csv), is.na(x_rds))) {
    return(FALSE)
  }

  if (is.numeric(x_csv) && is.numeric(x_rds)) {
    validos <- !is.na(x_csv) & !is.na(x_rds)

    if (!any(validos)) {
      return(TRUE)
    }

    a <- as.numeric(x_csv[validos])
    b <- as.numeric(x_rds[validos])
    diferenca <- abs(a - b)
    escala <- pmax(abs(a), abs(b), 1)

    return(
      all(
        diferenca <=
          tolerancia_absoluta +
          tolerancia_relativa * escala
      )
    )
  }

  if (
    inherits(x_csv, "Date") &&
      inherits(x_rds, "Date")
  ) {
    return(identical(as.numeric(x_csv), as.numeric(x_rds)))
  }

  if (
    inherits(x_csv, "POSIXct") &&
      inherits(x_rds, "POSIXct")
  ) {
    return(identical(as.numeric(x_csv), as.numeric(x_rds)))
  }

  if (is.character(x_csv) && is.character(x_rds)) {
    return(identical(enc2utf8(x_csv), enc2utf8(x_rds)))
  }

  identical(x_csv, x_rds)
}

comparar_semantica <- function(
    csv,
    rds,
    nome_par,
    chaves,
    tolerancia_absoluta = 1e-10,
    tolerancia_relativa = 1e-10
) {
  csv <- as_tibble(csv)
  rds <- as_tibble(rds)

  if (!identical(names(csv), names(rds))) {
    stop("Nomes ou ordem de colunas divergentes no par ", nome_par, ".")
  }

  if (!identical(dim(csv), dim(rds))) {
    stop(
      "Dimensões divergentes no par ", nome_par,
      ". CSV: ", nrow(csv), " × ", ncol(csv),
      "; RDS: ", nrow(rds), " × ", ncol(rds), "."
    )
  }

  chaves_ausentes <- setdiff(chaves, names(csv))

  if (length(chaves_ausentes) > 0L) {
    stop(
      "Chaves ausentes no par ", nome_par, ": ",
      paste(chaves_ausentes, collapse = ", "), "."
    )
  }

  if (anyDuplicated(csv[chaves]) > 0L || anyDuplicated(rds[chaves]) > 0L) {
    stop("As chaves não identificam unicamente as linhas do par ", nome_par, ".")
  }

  csv <- csv |>
    arrange(across(all_of(chaves)))

  rds <- rds |>
    arrange(across(all_of(chaves)))

  divergencias <- map_lgl(
    names(csv),
    function(coluna) {
      !comparar_coluna_semantica(
        csv[[coluna]],
        rds[[coluna]],
        tolerancia_absoluta,
        tolerancia_relativa
      )
    }
  )

  if (any(divergencias)) {
    stop(
      "Divergência semântica CSV–RDS no par ", nome_par,
      ". Colunas: ",
      paste(names(csv)[divergencias], collapse = ", "),
      "."
    )
  }

  tibble(
    par = nome_par,
    linhas = nrow(csv),
    colunas = ncol(csv),
    chaves = paste(chaves, collapse = " + "),
    tolerancia_absoluta = tolerancia_absoluta,
    tolerancia_relativa = tolerancia_relativa,
    equivalente = TRUE
  )
}

ler_par <- function(nome, caminho_csv, caminho_rds, chaves) {
  rds <- readRDS(caminho_rds)

  if (!is.data.frame(rds)) {
    stop("O RDS do par ", nome, " não contém um data frame.")
  }

  rds <- as_tibble(rds)
  csv <- ler_csv_por_modelo(caminho_csv, rds)

  comparacao <- comparar_semantica(
    csv = csv,
    rds = rds,
    nome_par = nome,
    chaves = chaves
  )

  list(
    dados = rds,
    comparacao = comparacao
  )
}

media_ponderada_segura <- function(x, pesos) {
  x <- suppressWarnings(as.numeric(x))
  pesos <- suppressWarnings(as.numeric(pesos))

  validos <- !is.na(x) & !is.na(pesos) & pesos > 0

  if (!any(validos)) {
    return(NA_real_)
  }

  weighted.mean(x[validos], pesos[validos])
}

divisao_segura <- function(numerador, denominador) {
  if (
    length(denominador) != 1L ||
      is.na(denominador) ||
      !is.finite(denominador) ||
      denominador <= 0
  ) {
    return(NA_real_)
  }

  as.numeric(numerador) / as.numeric(denominador)
}


interpretar_flag <- function(x, variavel) {
  if (is.logical(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    invalidos <- !is.na(x) & !x %in% c(0, 1)

    if (any(invalidos)) {
      stop(
        "Valores numéricos inválidos na flag `",
        variavel,
        "`: ",
        paste(sort(unique(x[invalidos])), collapse = ", "),
        "."
      )
    }

    return(
      case_when(
        is.na(x) ~ NA,
        x == 1 ~ TRUE,
        x == 0 ~ FALSE
      )
    )
  }

  texto <- str_to_lower(
    str_squish(
      as.character(x)
    )
  )

  resultado <- case_when(
    is.na(texto) | texto == "" ~ NA,
    texto %in% c(
      "true", "t", "1",
      "sim", "s", "yes", "y"
    ) ~ TRUE,
    texto %in% c(
      "false", "f", "0",
      "nao", "não", "n", "no"
    ) ~ FALSE,
    TRUE ~ NA
  )

  invalidos <- !is.na(texto) &
    texto != "" &
    is.na(resultado)

  if (any(invalidos)) {
    stop(
      "Valores textuais inválidos na flag `",
      variavel,
      "`: ",
      paste(sort(unique(texto[invalidos])), collapse = ", "),
      "."
    )
  }

  resultado
}

validar_colunas <- function(dados, colunas, nome_base) {
  ausentes <- setdiff(colunas, names(dados))
  if (length(ausentes) > 0L) {
    stop(
      "Colunas obrigatórias ausentes em ", nome_base, ": ",
      paste(ausentes, collapse = ", ")
    )
  }
  invisible(TRUE)
}

validar_ausencia_colunas_proibidas <- function(dados, nome_base) {
  padrao <- paste(
    c(
      "percentil", "ranking",
      "faixa_indice", "faixa_carga", "cenario", "cenário",
      "ordem_interna_carga", "score_dimensao_educacional",
      "dimensao_educacional"
    ),
    collapse = "|"
  )
  proibidas <- names(dados)[str_detect(str_to_lower(names(dados)), padrao)]
  if (length(proibidas) > 0L) {
    stop(
      "Colunas metodologicamente proibidas encontradas em ", nome_base,
      ": ", paste(proibidas, collapse = ", ")
    )
  }
  invisible(TRUE)
}

salvar_tabela <- function(dados, caminho) {
  dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)
  write_csv(dados, caminho, na = "")
  if (!file.exists(caminho) || file.info(caminho)$size <= 0) {
    stop("Falha ao gravar tabela candidata: ", caminho)
  }
}

salvar_grafico <- function(grafico, caminho, largura = 10, altura = 6) {
  dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = caminho,
    plot = grafico,
    width = largura,
    height = altura,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  if (!file.exists(caminho) || file.info(caminho)$size <= 0) {
    stop("Falha ao gravar gráfico candidato: ", caminho)
  }
}

tema_relatorio <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.25)),
      plot.subtitle = element_text(size = rel(0.98)),
      plot.caption = element_text(hjust = 0, size = rel(0.75)),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "bottom",
      plot.margin = margin(12, 18, 12, 12)
    )
}

fmt_1 <- scales::label_number(
  accuracy = 0.1,
  decimal.mark = ",",
  big.mark = "."
)

fmt_0 <- scales::label_number(
  accuracy = 1,
  decimal.mark = ",",
  big.mark = "."
)

# -------------------------------------------------------------------
# 3. Bloqueios de ambiente, Git, arquivos e hashes
# -------------------------------------------------------------------

if (!file.exists(caminho_script)) {
  stop("Script canônico não localizado: ", caminho_script)
}

if (
  caminho_normalizado(caminho_script) !=
    caminho_normalizado(file.path(here(), caminho_relativo_script))
) {
  stop("O módulo 21 deve ser executado exclusivamente pelo caminho canônico.")
}

branch_observada <- obter_branch()
commit_observado <- str_to_lower(obter_commit())

if (!identical(branch_observada, branch_esperada)) {
  stop(
    "Branch incorreta. Observada: ", branch_observada,
    "; esperada: ", branch_esperada, "."
  )
}

if (!identical(commit_observado, commit_base_integracao)) {
  stop(
    "Commit-base incorreto. Observado: ", commit_observado,
    "; esperado: ", commit_base_integracao, "."
  )
}

arquivos_controlados <- c(manifestos, entradas)
existencia <- tibble(
  item = names(arquivos_controlados),
  caminho = unname(arquivos_controlados),
  existe = file.exists(arquivos_controlados),
  tamanho_bytes = if_else(
    file.exists(arquivos_controlados),
    as.numeric(file.info(arquivos_controlados)$size),
    NA_real_
  )
)

write_csv(
  existencia,
  file.path(dir_execucao, "01_existencia_arquivos.csv"),
  na = ""
)

if (any(!existencia$existe) || any(existencia$tamanho_bytes <= 0, na.rm = TRUE)) {
  stop("Há entradas ou manifestos ausentes/vazios. Consulte 01_existencia_arquivos.csv.")
}

hashes_observados <- map_chr(entradas, hash_md5)
validacao_hashes <- tibble(
  entrada = names(entradas),
  caminho = unname(entradas),
  hash_esperado = unname(hashes_esperados[names(entradas)]),
  hash_observado = unname(hashes_observados),
  aprovado = hash_esperado == hash_observado
)

write_csv(
  validacao_hashes,
  file.path(dir_execucao, "02_validacao_hashes_entradas.csv"),
  na = ""
)

if (
  any(is.na(validacao_hashes$aprovado)) ||
    any(!validacao_hashes$aprovado)
) {
  divergentes <- validacao_hashes |>
    filter(is.na(aprovado) | !aprovado) |>
    pull(entrada)

  stop(
    "Hashes de entrada divergentes ou ausentes: ",
    paste(divergentes, collapse = ", "),
    ". Consulte 02_validacao_hashes_entradas.csv."
  )
}

# -------------------------------------------------------------------
# 4. Leitura dos pares CSV–RDS
# -------------------------------------------------------------------

chaves_pares <- list(
  base_final = c("id_escola", "ano_escolar"),
  dim_final = "id_escola",
  perfil_completo = "id_escola",
  perfil_gerencial = "id_escola",
  perfil_serie = c("id_escola", "ano_escolar", "componente"),
  indice = "id_escola",
  componentes_indice = c("id_escola", "dimensao", "componente"),
  diagnostico_educacional_escola_serie = c(
    "id_escola",
    "ano_escolar",
    "componente"
  ),
  analise_carteiras = "assessora_gerencial_2026",
  detalhe_carteiras = c("assessora_gerencial_2026", "id_escola"),
  diagnostico_carteiras = "assessora_gerencial_2026",
  composicao_carteiras = c(
    "assessora_gerencial_2026",
    "dimensao",
    "metrica"
  ),
  universo_carteiras = "id_escola",
  indice_fichas_carteiras = "assessora_gerencial_2026"
)

pares <- list(
  base_final = ler_par(
    "base_final",
    entradas[["base_final_csv"]],
    entradas[["base_final_rds"]],
    chaves_pares$base_final
  ),
  dim_final = ler_par(
    "dim_final",
    entradas[["dim_final_csv"]],
    entradas[["dim_final_rds"]],
    chaves_pares$dim_final
  ),
  perfil_completo = ler_par(
    "perfil_completo",
    entradas[["perfil_completo_csv"]],
    entradas[["perfil_completo_rds"]],
    chaves_pares$perfil_completo
  ),
  perfil_gerencial = ler_par(
    "perfil_gerencial",
    entradas[["perfil_gerencial_csv"]],
    entradas[["perfil_gerencial_rds"]],
    chaves_pares$perfil_gerencial
  ),
  perfil_serie = ler_par(
    "perfil_serie",
    entradas[["perfil_serie_csv"]],
    entradas[["perfil_serie_rds"]],
    chaves_pares$perfil_serie
  ),
  indice = ler_par(
    "indice",
    entradas[["indice_csv"]],
    entradas[["indice_rds"]],
    chaves_pares$indice
  ),
  componentes_indice = ler_par(
    "componentes_indice",
    entradas[["componentes_indice_csv"]],
    entradas[["componentes_indice_rds"]],
    chaves_pares$componentes_indice
  ),
  diagnostico_educacional_escola_serie = ler_par(
    "diagnostico_educacional_escola_serie",
    entradas[["diagnostico_educacional_escola_serie_csv"]],
    entradas[["diagnostico_educacional_escola_serie_rds"]],
    chaves_pares$diagnostico_educacional_escola_serie
  ),
  analise_carteiras = ler_par(
    "analise_carteiras",
    entradas[["analise_carteiras_csv"]],
    entradas[["analise_carteiras_rds"]],
    chaves_pares$analise_carteiras
  ),
  detalhe_carteiras = ler_par(
    "detalhe_carteiras",
    entradas[["detalhe_carteiras_csv"]],
    entradas[["detalhe_carteiras_rds"]],
    chaves_pares$detalhe_carteiras
  ),
  diagnostico_carteiras = ler_par(
    "diagnostico_carteiras",
    entradas[["diagnostico_carteiras_csv"]],
    entradas[["diagnostico_carteiras_rds"]],
    chaves_pares$diagnostico_carteiras
  ),
  composicao_carteiras = ler_par(
    "composicao_carteiras",
    entradas[["composicao_carteiras_csv"]],
    entradas[["composicao_carteiras_rds"]],
    chaves_pares$composicao_carteiras
  ),
  universo_carteiras = ler_par(
    "universo_carteiras",
    entradas[["universo_carteiras_csv"]],
    entradas[["universo_carteiras_rds"]],
    chaves_pares$universo_carteiras
  ),
  indice_fichas_carteiras = ler_par(
    "indice_fichas_carteiras",
    entradas[["indice_fichas_carteiras_csv"]],
    entradas[["indice_fichas_carteiras_rds"]],
    chaves_pares$indice_fichas_carteiras
  )
)

equivalencia_pares <- map_dfr(pares, "comparacao")
write_csv(
  equivalencia_pares,
  file.path(dir_execucao, "03_equivalencia_csv_rds.csv"),
  na = ""
)

bases <- map(pares, "dados")

base_final <- bases$base_final
dim_final <- bases$dim_final
perfil_completo <- bases$perfil_completo
perfil <- bases$perfil_gerencial
perfil_serie <- bases$perfil_serie
indice <- bases$indice
componentes_indice <- bases$componentes_indice
analise_carteiras <- bases$analise_carteiras
detalhe_carteiras <- bases$detalhe_carteiras
diagnostico_carteiras <- bases$diagnostico_carteiras
composicao_carteiras <- bases$composicao_carteiras
universo_carteiras <- bases$universo_carteiras
indice_fichas_carteiras <- bases$indice_fichas_carteiras


perfil <- perfil |>
  mutate(
    pertence_universo_avaliativo_2025 =
      interpretar_flag(
        pertence_universo_avaliativo_2025,
        "perfil$pertence_universo_avaliativo_2025"
      ),
    pertence_universo_avaliativo_2026 =
      interpretar_flag(
        pertence_universo_avaliativo_2026,
        "perfil$pertence_universo_avaliativo_2026"
      ),
    recebe_assessoramento_2025 =
      interpretar_flag(
        recebe_assessoramento_2025,
        "perfil$recebe_assessoramento_2025"
      ),
    recebe_assessoramento_2026 =
      interpretar_flag(
        recebe_assessoramento_2026,
        "perfil$recebe_assessoramento_2026"
      ),
    incluir_indice_carga_2025 =
      interpretar_flag(
        incluir_indice_carga_2025,
        "perfil$incluir_indice_carga_2025"
      ),
    incluir_indice_carga_2026 =
      interpretar_flag(
        incluir_indice_carga_2026,
        "perfil$incluir_indice_carga_2026"
      ),
    possui_alerta_composicao =
      interpretar_flag(
        possui_alerta_composicao,
        "perfil$possui_alerta_composicao"
      )
  )

perfil_serie <- perfil_serie |>
  mutate(
    pertence_universo_avaliativo_2025 =
      interpretar_flag(
        pertence_universo_avaliativo_2025,
        "perfil_serie$pertence_universo_avaliativo_2025"
      ),
    pertence_universo_avaliativo_2026 =
      interpretar_flag(
        pertence_universo_avaliativo_2026,
        "perfil_serie$pertence_universo_avaliativo_2026"
      ),
    alerta_composicao_serie =
      interpretar_flag(
        alerta_composicao_serie,
        "perfil_serie$alerta_composicao_serie"
      )
  )

indice <- indice |>
  mutate(
    incluir_indice_carga_2026 =
      interpretar_flag(
        incluir_indice_carga_2026,
        "indice$incluir_indice_carga_2026"
      ),
    resultados_educacionais_no_indice =
      interpretar_flag(
        resultados_educacionais_no_indice,
        "indice$resultados_educacionais_no_indice"
      )
  )

uso_entradas <- tribble(
  ~base, ~papel,
  "base_final", "Dependência de integridade da cadeia; não usada em agregação local.",
  "dim_final", "Dependência de integridade da cadeia; não usada em agregação local.",
  "perfil_completo", "Fonte explícita do anexo institucional A01.",
  "perfil_gerencial", "Fonte do perfil institucional, universos e anexo operacional A02.",
  "perfil_serie", "Fonte das tabelas e gráficos educacionais e do anexo A03.",
  "indice", "Fonte do anexo A04 e validação do universo operacional.",
  "componentes_indice", "Fonte do anexo A05.",
  "diagnostico_educacional_escola_serie", "Dependência de integridade do diagnóstico educacional do módulo 17.",
  "analise_carteiras", "Fonte das tabelas e gráficos de carteiras.",
  "detalhe_carteiras", "Dependência de integridade e validação metodológica das carteiras.",
  "diagnostico_carteiras", "Fonte do anexo A07.",
  "composicao_carteiras", "Fonte do anexo A06.",
  "universo_carteiras", "Fonte do anexo A08.",
  "indice_fichas_carteiras", "Dependência editorial do módulo 20; valida existência das 11 fichas gerenciais."
)

write_csv(
  uso_entradas,
  file.path(dir_execucao, "04_uso_entradas.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 5. Contratos de colunas e proibições
# -------------------------------------------------------------------

validar_colunas(
  perfil,
  c(
    "id_escola", "codigo_inep", "nome_canonico",
    "assessora_vinculo_administrativo", "assessora_gerencial_2026",
    "pertence_universo_avaliativo_2025",
    "pertence_universo_avaliativo_2026",
    "recebe_assessoramento_2025", "recebe_assessoramento_2026",
    "exposicao_programa_binaria_2025",
    "exposicao_programa_binaria_2026",
    "incluir_indice_carga_2025", "incluir_indice_carga_2026",
    "grupo_administrativo_2024_final",
    "matriculas_anos_iniciais", "turmas_anos_iniciais",
    "docentes_anos_iniciais", "indice_infraestrutura_basica",
    "pct_matriculas_anos_iniciais_integral",
    "pct_matriculas_educacao_especial",
    "possui_alerta_composicao"
  ),
  "perfil_escola_gerencial"
)

validar_colunas(
  perfil_serie,
  c(
    "id_escola", "ano_escolar", "componente",
    "pertence_universo_avaliativo_2025",
    "pertence_universo_avaliativo_2026",
    "previstos_2025", "avaliados_2025", "taxa_participacao_2025",
    "proficiencia_media_2025", "pct_defasagem_2025", "pct_adequado_2025",
    "previstos_2026", "avaliados_2026", "taxa_participacao_2026",
    "proficiencia_media_2026", "pct_defasagem_2026", "pct_adequado_2026",
    "delta_participacao", "delta_proficiencia",
    "variacao_relativa_previstos", "alerta_composicao_serie",
    "observacao_composicao"
  ),
  "perfil_escola_serie_compacto"
)

validar_colunas(
  indice,
  c(
    "id_escola", "codigo_inep", "nome_canonico",
    "assessora_vinculo_administrativo", "assessora_gerencial_2026",
    "incluir_indice_carga_2026",
    "score_dimensao_volume", "score_dimensao_estrutural",
    "score_dimensao_administrativa",
    "contribuicao_volume", "contribuicao_estrutural",
    "contribuicao_administrativa",
    "indice_carga_potencial_operacional",
    "resultados_educacionais_no_indice", "nota_uso"
  ),
  "indice_carga_potencial_escola"
)

validar_colunas(
  analise_carteiras,
  c(
    "assessora_gerencial_2026", "numero_escolas",
    "matriculas_anos_iniciais_total", "turmas_anos_iniciais_total",
    "soma_indice_operacional_carteira",
    "media_indice_operacional_escolas",
    "mediana_indice_operacional_escolas",
    "desvio_indice_operacional_escolas",
    "soma_contribuicao_volume",
    "soma_contribuicao_estrutural",
    "soma_contribuicao_administrativa",
    "media_score_dimensao_volume",
    "media_score_dimensao_estrutural",
    "media_score_dimensao_administrativa",
    "nota_interpretacao"
  ),
  "analise_carteiras_assessoras"
)

walk2(
  list(indice, analise_carteiras, detalhe_carteiras, componentes_indice),
  c(
    "indice_carga_potencial_escola",
    "analise_carteiras_assessoras",
    "carteira_escola_detalhe",
    "componentes_indice_carga_potencial"
  ),
  validar_ausencia_colunas_proibidas
)

if (any(indice$resultados_educacionais_no_indice %in% TRUE, na.rm = TRUE)) {
  stop("Resultados educacionais foram encontrados dentro do índice operacional.")
}

# -------------------------------------------------------------------
# 6. Validação dos universos homologados
# -------------------------------------------------------------------

universo_institucional <- perfil
universo_operacional <- perfil |> filter(incluir_indice_carga_2026 %in% TRUE)
universo_nao_operacional <- perfil |> filter(!incluir_indice_carga_2026 %in% TRUE)

validacoes_universos <- tribble(
  ~teste, ~observado, ~esperado, ~aprovado,
  "Escolas no universo institucional",
  n_distinct(universo_institucional$id_escola), 56L,
  n_distinct(universo_institucional$id_escola) == 56L,

  "Escolas no universo operacional de 2026",
  n_distinct(universo_operacional$id_escola), 53L,
  n_distinct(universo_operacional$id_escola) == 53L,

  "Escolas institucionais não operacionais",
  n_distinct(universo_nao_operacional$id_escola), 3L,
  n_distinct(universo_nao_operacional$id_escola) == 3L,

  "Escolas no universo avaliativo de 2025",
  n_distinct(perfil$id_escola[perfil$pertence_universo_avaliativo_2025 %in% TRUE]),
  54L,
  n_distinct(perfil$id_escola[perfil$pertence_universo_avaliativo_2025 %in% TRUE]) == 54L,

  "Escolas no universo avaliativo de 2026",
  n_distinct(perfil$id_escola[perfil$pertence_universo_avaliativo_2026 %in% TRUE]),
  56L,
  n_distinct(perfil$id_escola[perfil$pertence_universo_avaliativo_2026 %in% TRUE]) == 56L,

  "Escolas no índice operacional",
  n_distinct(indice$id_escola), 53L,
  n_distinct(indice$id_escola) == 53L,

  "Carteiras gerenciais de 2026",
  n_distinct(analise_carteiras$assessora_gerencial_2026), 11L,
  n_distinct(analise_carteiras$assessora_gerencial_2026) == 11L,

  "Exposição positiva em 2025",
  sum(perfil$exposicao_programa_binaria_2025 == 1, na.rm = TRUE), 0L,
  sum(perfil$exposicao_programa_binaria_2025 == 1, na.rm = TRUE) == 0L,

  "Escolas operacionais sem assessora",
  sum(
    universo_operacional$assessora_gerencial_2026 == "" |
      is.na(universo_operacional$assessora_gerencial_2026)
  ),
  0L,
  sum(
    universo_operacional$assessora_gerencial_2026 == "" |
      is.na(universo_operacional$assessora_gerencial_2026)
  ) == 0L
)

write_csv(
  validacoes_universos,
  file.path(dir_execucao, "05_validacao_universos.csv"),
  na = ""
)

if (
  any(is.na(validacoes_universos$aprovado)) ||
    any(!validacoes_universos$aprovado)
) {
  falhas <- validacoes_universos |>
    filter(is.na(aprovado) | !aprovado) |>
    transmute(
      mensagem = paste0(
        teste,
        ": observado = ",
        observado,
        "; esperado = ",
        esperado
      )
    ) |>
    pull(mensagem)

  stop(
    "Invariantes de universo falharam:\n- ",
    paste(falhas, collapse = "\n- ")
  )
}

if (!setequal(universo_nao_operacional$id_escola, ids_nao_operacionais)) {
  stop("O conjunto de escolas institucionais não operacionais diverge do homologado.")
}

categorias_residuais <- c("outras", "sem vinculação informada", "sem vinculacao informada")
assessoras_normalizadas <- str_to_lower(str_squish(analise_carteiras$assessora_gerencial_2026))
if (any(assessoras_normalizadas %in% categorias_residuais)) {
  stop("Categoria residual encontrada entre as carteiras operacionais.")
}

if (nrow(indice_fichas_carteiras) != 11L) {
  stop("O índice editorial das fichas de carteiras deve possuir 11 linhas.")
}

if (
  any(indice_fichas_carteiras$status_geracao != "SUCESSO", na.rm = TRUE)
) {
  stop("Há ficha gerencial do módulo 20 sem status de sucesso.")
}

walk(
  c(
    dir_candidato_corpo_tabelas,
    dir_candidato_corpo_graficos,
    dir_candidato_anexos_tabelas,
    dir_rollback
  ),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# -------------------------------------------------------------------
# 7. Matriz editorial canônica
# -------------------------------------------------------------------

matriz_editorial <- tribble(
  ~id, ~destino, ~tipo, ~titulo, ~universo, ~decisao,
  "T01", "Corpo", "Tabela", "Universos e cobertura do estudo", "Múltiplos universos", "Produzir",
  "T02", "Corpo", "Tabela", "Perfil estrutural do universo institucional", "56 escolas", "Produzir",
  "T03", "Corpo", "Tabela", "Participação, composição e resultados por ano escolar", "54/56 escolas avaliativas", "Produzir",
  "T04", "Corpo", "Tabela", "Síntese não ordinal das carteiras operacionais", "53 escolas / 11 assessoras", "Produzir",
  "T05", "Corpo", "Tabela", "Dimensões operacionais das carteiras", "53 escolas / 11 assessoras", "Produzir",
  "T06", "Corpo", "Tabela", "Cautelas metodológicas e institucionais", "Aplicação transversal", "Produzir",
  "G01", "Corpo", "Gráfico", "Universos do estudo", "Múltiplos universos", "Produzir",
  "G02", "Corpo", "Gráfico", "Composição administrativa institucional", "56 escolas", "Produzir",
  "G03", "Corpo", "Gráfico", "Participação por ano escolar", "54/56 escolas avaliativas", "Produzir",
  "G04", "Corpo", "Gráfico", "Participação e proficiência observada", "54/56 escolas avaliativas", "Produzir",
  "G05", "Corpo", "Gráfico", "Extensão e intensidade das carteiras", "53 escolas / 11 assessoras", "Produzir",
  "G06", "Corpo", "Gráfico", "Dimensões operacionais das carteiras", "53 escolas / 11 assessoras", "Produzir",
  "A01", "Anexo", "Tabela", "Universo institucional das escolas", "56 escolas", "Selecionar",
  "A02", "Anexo", "Tabela", "Universo operacional de 2026", "53 escolas", "Selecionar",
  "A03", "Anexo", "Tabela", "Resultados escola × ano escolar", "Universos avaliativos", "Selecionar",
  "A04", "Anexo", "Tabela", "Índice operacional por escola", "53 escolas", "Selecionar",
  "A05", "Anexo", "Tabela", "Componentes do índice operacional", "53 escolas", "Selecionar",
  "A06", "Anexo", "Tabela", "Composição das carteiras", "53 escolas / 11 assessoras", "Selecionar",
  "A07", "Anexo", "Tabela", "Diagnóstico educacional das carteiras", "11 assessoras", "Selecionar",
  "A08", "Anexo", "Tabela", "Escolas institucionais não operacionais", "3 escolas", "Selecionar",
  "A09", "Anexo", "Tabela", "Catálogo editorial", "Produtos do módulo 21", "Produzir"
)

write_csv(
  matriz_editorial,
  file.path(dir_execucao, "06_matriz_editorial.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 8. Tabelas candidatas para o corpo
# -------------------------------------------------------------------

tabela_universos <- tribble(
  ~ordem, ~universo, ~periodo, ~unidade, ~total, ~uso_analitico,
  1L, "Institucional", "Cadastro consolidado", "escolas", 56L,
  "Caracterização geral da rede e fichas institucionais.",
  2L, "Operacional", "2026", "escolas", 53L,
  "Índice operacional e análise das carteiras.",
  3L, "Institucional não operacional", "2026", "escolas", 3L,
  "Caracterização institucional; fora da carga operacional.",
  4L, "Avaliativo", "2025", "escolas", 54L,
  "Descrição da primeira avaliação de 2025, anterior ao assessoramento.",
  5L, "Avaliativo", "2026", "escolas", 56L,
  "Descrição da primeira avaliação de 2026.",
  6L, "Gerencial", "2026", "assessoras", 11L,
  "Descrição não ordinal das carteiras operacionais."
)

tabela_perfil <- tibble(
  indicador = c(
    "Escolas",
    "Matrículas nos anos iniciais",
    "Turmas dos anos iniciais",
    "Docentes dos anos iniciais",
    "Mediana de matrículas por escola",
    "Média de estudantes por turma",
    "Matrículas em tempo integral",
    "Matrículas da educação especial",
    "Índice médio de infraestrutura",
    "Escolas com alerta de composição"
  ),
  valor = c(
    n_distinct(perfil$id_escola),
    sum(perfil$matriculas_anos_iniciais, na.rm = TRUE),
    sum(perfil$turmas_anos_iniciais, na.rm = TRUE),
    sum(perfil$docentes_anos_iniciais, na.rm = TRUE),
    median(perfil$matriculas_anos_iniciais, na.rm = TRUE),
    weighted.mean(
      perfil$alunos_por_turma_anos_iniciais,
      perfil$turmas_anos_iniciais,
      na.rm = TRUE
    ),
    weighted.mean(
      perfil$pct_matriculas_anos_iniciais_integral,
      perfil$matriculas_anos_iniciais,
      na.rm = TRUE
    ),
    weighted.mean(
      perfil$pct_matriculas_educacao_especial,
      perfil$matriculas_anos_iniciais,
      na.rm = TRUE
    ),
    mean(perfil$indice_infraestrutura_basica, na.rm = TRUE),
    sum(perfil$possui_alerta_composicao %in% TRUE, na.rm = TRUE)
  ),
  natureza = c(
    "Contagem", "Contagem", "Contagem", "Contagem", "Mediana",
    "Média ponderada", "Percentual ponderado", "Percentual ponderado",
    "Média", "Contagem"
  ),
  universo = "Institucional — 56 escolas",
  referencia_temporal = c(
    rep("Censo Escolar 2024", 9),
    "CAEd 2025–2026"
  )
)

tabela_resultados <- perfil_serie |>
  group_by(ano_escolar) |>
  summarise(
    escolas_avaliativas_2025 = n_distinct(
      id_escola[pertence_universo_avaliativo_2025 %in% TRUE]
    ),
    escolas_avaliativas_2026 = n_distinct(
      id_escola[pertence_universo_avaliativo_2026 %in% TRUE]
    ),
    previstos_2025 = sum(
      previstos_2025[pertence_universo_avaliativo_2025 %in% TRUE],
      na.rm = TRUE
    ),
    avaliados_2025 = sum(
      avaliados_2025[pertence_universo_avaliativo_2025 %in% TRUE],
      na.rm = TRUE
    ),
    previstos_2026 = sum(
      previstos_2026[pertence_universo_avaliativo_2026 %in% TRUE],
      na.rm = TRUE
    ),
    avaliados_2026 = sum(
      avaliados_2026[pertence_universo_avaliativo_2026 %in% TRUE],
      na.rm = TRUE
    ),
    participacao_2025 = 100 * divisao_segura(avaliados_2025, previstos_2025),
    participacao_2026 = 100 * divisao_segura(avaliados_2026, previstos_2026),
    proficiencia_2025 = media_ponderada_segura(
      proficiencia_media_2025[
        pertence_universo_avaliativo_2025 %in% TRUE
      ],
      avaliados_2025[
        pertence_universo_avaliativo_2025 %in% TRUE
      ]
    ),
    proficiencia_2026 = media_ponderada_segura(
      proficiencia_media_2026[
        pertence_universo_avaliativo_2026 %in% TRUE
      ],
      avaliados_2026[
        pertence_universo_avaliativo_2026 %in% TRUE
      ]
    ),
    defasagem_2025 = media_ponderada_segura(
      pct_defasagem_2025[
        pertence_universo_avaliativo_2025 %in% TRUE
      ],
      avaliados_2025[
        pertence_universo_avaliativo_2025 %in% TRUE
      ]
    ),
    defasagem_2026 = media_ponderada_segura(
      pct_defasagem_2026[
        pertence_universo_avaliativo_2026 %in% TRUE
      ],
      avaliados_2026[
        pertence_universo_avaliativo_2026 %in% TRUE
      ]
    ),
    adequado_2025 = media_ponderada_segura(
      pct_adequado_2025[
        pertence_universo_avaliativo_2025 %in% TRUE
      ],
      avaliados_2025[
        pertence_universo_avaliativo_2025 %in% TRUE
      ]
    ),
    adequado_2026 = media_ponderada_segura(
      pct_adequado_2026[
        pertence_universo_avaliativo_2026 %in% TRUE
      ],
      avaliados_2026[
        pertence_universo_avaliativo_2026 %in% TRUE
      ]
    ),
    series_escolas_alerta_composicao = sum(
      alerta_composicao_serie %in% TRUE,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    delta_participacao_pp = participacao_2026 - participacao_2025,
    delta_proficiencia = proficiencia_2026 - proficiencia_2025,
    across(
      c(
        participacao_2025, participacao_2026, delta_participacao_pp,
        proficiencia_2025, proficiencia_2026, delta_proficiencia,
        defasagem_2025, defasagem_2026, adequado_2025, adequado_2026
      ),
      ~ round(.x, 1)
    ),
    nota = paste(
      "2025 corresponde à linha de base anterior ao assessoramento.",
      "Variações são descritivas e condicionadas por participação e composição."
    )
  ) |>
  arrange(ano_escolar)

tabela_carteiras <- analise_carteiras |>
  transmute(
    assessora_gerencial_2026 = str_to_title(assessora_gerencial_2026),
    numero_escolas,
    matriculas_anos_iniciais_total,
    turmas_anos_iniciais_total,
    soma_indice_operacional_carteira = round(
      soma_indice_operacional_carteira, 1
    ),
    media_indice_operacional_escolas = round(
      media_indice_operacional_escolas, 1
    ),
    mediana_indice_operacional_escolas = round(
      mediana_indice_operacional_escolas, 1
    ),
    desvio_indice_operacional_escolas = round(
      desvio_indice_operacional_escolas, 1
    ),
    nota_interpretacao
  ) |>
  arrange(assessora_gerencial_2026)

tabela_dimensoes <- analise_carteiras |>
  transmute(
    assessora_gerencial_2026 = str_to_title(assessora_gerencial_2026),
    volume = round(media_score_dimensao_volume, 1),
    estrutura = round(media_score_dimensao_estrutural, 1),
    complexidade_administrativa = round(
      media_score_dimensao_administrativa, 1
    )
  ) |>
  pivot_longer(
    cols = c(volume, estrutura, complexidade_administrativa),
    names_to = "dimensao",
    values_to = "escore_medio"
  ) |>
  mutate(
    dimensao = recode(
      dimensao,
      volume = "Volume",
      estrutura = "Estrutura",
      complexidade_administrativa = "Complexidade administrativa"
    )
  ) |>
  arrange(assessora_gerencial_2026, dimensao)

tabela_cautelas <- tribble(
  ~tema, ~orientacao,
  "Desenho analítico",
  "O estudo é observacional e descritivo; não identifica efeitos causais.",
  "Linha de base",
  "A primeira avaliação de 2025 ocorreu antes do assessoramento e representa dose zero.",
  "Resultados educacionais",
  "Proficiência, adequação, defasagem e participação não integram o índice operacional.",
  "Assessora",
  "Resultados das escolas não avaliam qualidade, produtividade ou efetividade das assessoras.",
  "Participação e composição",
  "Qualquer descrição de desempenho deve ser acompanhada de cobertura e alertas de composição.",
  "Índice operacional",
  "O índice é relativo e parcial; não representa integralmente horas, deslocamentos, emergências ou carga real.",
  "Carteiras",
  "Comparações usam referências agregadas não ordinais e não classificam assessoras como sobrecarregadas ou subutilizadas.",
  "Decisão gerencial",
  "Os produtos quantitativos subsidiam, mas não determinam automaticamente redistribuições."
)

tabelas_corpo <- list(
  T01 = tabela_universos,
  T02 = tabela_perfil,
  T03 = tabela_resultados,
  T04 = tabela_carteiras,
  T05 = tabela_dimensoes,
  T06 = tabela_cautelas
)

nomes_tabelas_corpo <- c(
  T01 = "T01_universos_cobertura_estudo.csv",
  T02 = "T02_perfil_estrutural_universo_institucional.csv",
  T03 = "T03_participacao_composicao_resultados_por_ano.csv",
  T04 = "T04_sintese_nao_ordinal_carteiras_operacionais.csv",
  T05 = "T05_dimensoes_operacionais_carteiras.csv",
  T06 = "T06_cautelas_metodologicas.csv"
)

walk2(
  tabelas_corpo,
  file.path(dir_candidato_corpo_tabelas, nomes_tabelas_corpo),
  salvar_tabela
)

# -------------------------------------------------------------------
# 9. Tabelas candidatas para os anexos
# -------------------------------------------------------------------

anexo_institucional <- perfil_completo |>
  select(
    id_escola, codigo_inep, nome_canonico,
    assessora_vinculo_administrativo,
    assessora_gerencial_2025, assessora_gerencial_2026,
    pertence_universo_avaliativo_2025,
    pertence_universo_avaliativo_2026,
    recebe_assessoramento_2025, recebe_assessoramento_2026,
    incluir_indice_carga_2025, incluir_indice_carga_2026,
    grupo_administrativo_2024_final,
    matriculas_anos_iniciais, turmas_anos_iniciais,
    docentes_anos_iniciais, alunos_por_turma_anos_iniciais,
    pct_matriculas_anos_iniciais_integral,
    pct_matriculas_educacao_especial,
    indice_infraestrutura_basica,
    possui_alerta_composicao
  ) |>
  arrange(nome_canonico)

anexo_operacional <- perfil |>
  filter(incluir_indice_carga_2026 %in% TRUE) |>
  select(
    id_escola, codigo_inep, nome_canonico,
    assessora_vinculo_administrativo,
    assessora_gerencial_2026,
    incluir_indice_carga_2026,
    grupo_administrativo_2024_final,
    matriculas_anos_iniciais, turmas_anos_iniciais,
    docentes_anos_iniciais, alunos_por_turma_anos_iniciais,
    pct_matriculas_anos_iniciais_integral,
    pct_matriculas_educacao_especial,
    indice_infraestrutura_basica,
    possui_alerta_composicao
  ) |>
  arrange(nome_canonico)

anexo_resultados <- perfil_serie |>
  select(
    id_escola, codigo_inep, nome_canonico,
    assessora_vinculo_administrativo,
    assessora_gerencial_2025, assessora_gerencial_2026,
    ano_escolar, componente,
    pertence_universo_avaliativo_2025,
    pertence_universo_avaliativo_2026,
    previstos_2025, avaliados_2025, taxa_participacao_2025,
    proficiencia_media_2025, pct_defasagem_2025, pct_adequado_2025,
    previstos_2026, avaliados_2026, taxa_participacao_2026,
    proficiencia_media_2026, pct_defasagem_2026, pct_adequado_2026,
    delta_participacao, delta_proficiencia,
    variacao_relativa_previstos,
    alerta_composicao_serie, observacao_composicao
  ) |>
  arrange(nome_canonico, ano_escolar)

anexo_indice <- indice |>
  select(
    id_escola, codigo_inep, nome_canonico,
    assessora_vinculo_administrativo, assessora_gerencial_2026,
    incluir_indice_carga_2026,
    score_dimensao_volume, score_dimensao_estrutural,
    score_dimensao_administrativa,
    contribuicao_volume, contribuicao_estrutural,
    contribuicao_administrativa,
    indice_carga_potencial_operacional,
    cobertura_indice_operacional,
    interpretacao_cautelosa,
    resultados_educacionais_no_indice,
    nota_uso
  ) |>
  arrange(nome_canonico)

anexo_componentes <- componentes_indice |>
  select(
    id_escola, codigo_inep, nome_canonico,
    assessora_gerencial_2026,
    dimensao, componente,
    variavel_origem, valor_origem,
    score_relativo,
    peso_na_dimensao,
    peso_no_indice,
    uso_no_indice
  ) |>
  arrange(nome_canonico, dimensao, componente)

anexo_composicao <- composicao_carteiras |>
  select(
    assessora_gerencial_2026,
    numero_escolas,
    metrica,
    valor,
    dimensao,
    tipo_metrica,
    peso_dimensao_indice,
    uso_no_indice_operacional,
    nota_metodologica
  ) |>
  arrange(assessora_gerencial_2026, dimensao, metrica)

anexo_diagnostico <- diagnostico_carteiras |>
  select(
    assessora_gerencial_2026,
    numero_escolas_diagnostico,
    numero_linhas_escola_serie,
    series_com_previstos,
    series_com_avaliados,
    previstos_total_2026,
    avaliados_total_2026,
    taxa_participacao_agregada_2026,
    proficiencia_2026_ponderada_avaliados,
    pct_defasagem_2026_ponderado_avaliados,
    pct_intermediario_2026_ponderado_avaliados,
    pct_adequado_2026_ponderado_avaliados,
    series_painel_balanceado,
    series_alerta_composicao,
    series_aumento_participacao_10pp,
    series_queda_participacao_10pp,
    series_mudanca_previstos_20pct,
    menor_proficiencia_observada_2026,
    maior_pct_defasagem_observado_2026,
    peso_maximo_no_indice_operacional,
    linhas_marcadas_uso_no_indice,
    alerta_participacao_composicao,
    leitura_menor_proficiencia,
    natureza,
    advertencia
  ) |>
  arrange(assessora_gerencial_2026)

anexo_nao_operacional <- universo_carteiras |>
  filter(id_escola %in% ids_nao_operacionais) |>
  arrange(id_escola)

anexos <- list(
  A01 = anexo_institucional,
  A02 = anexo_operacional,
  A03 = anexo_resultados,
  A04 = anexo_indice,
  A05 = anexo_componentes,
  A06 = anexo_composicao,
  A07 = anexo_diagnostico,
  A08 = anexo_nao_operacional
)

nomes_anexos <- c(
  A01 = "A01_universo_institucional_escolas.csv",
  A02 = "A02_universo_operacional_2026.csv",
  A03 = "A03_resultados_escola_ano_escolar.csv",
  A04 = "A04_indice_operacional_escolas.csv",
  A05 = "A05_componentes_indice_operacional.csv",
  A06 = "A06_composicao_carteiras.csv",
  A07 = "A07_diagnostico_educacional_carteiras.csv",
  A08 = "A08_escolas_institucionais_nao_operacionais.csv"
)

walk2(
  anexos,
  file.path(dir_candidato_anexos_tabelas, nomes_anexos),
  salvar_tabela
)

# -------------------------------------------------------------------
# 10. Gráficos candidatos
# -------------------------------------------------------------------

grafico_universos <- tabela_universos |>
  filter(unidade == "escolas") |>
  mutate(
    rotulo = paste0(universo, " — ", periodo),
    rotulo = factor(rotulo, levels = rev(rotulo))
  ) |>
  ggplot(aes(x = total, y = rotulo)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = total), hjust = -0.15, fontface = "bold") +
  scale_x_continuous(
    labels = fmt_0,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Universos de escolas utilizados no estudo",
    subtitle = "Os recortes institucional, operacional e avaliativo têm finalidades distintas",
    x = "Número de escolas",
    y = NULL,
    caption = paste(
      "Fonte: módulos 15 a 18; módulo 20 conferido como dependência editorial.",
      "O universo institucional não deve ser confundido com a carga operacional."
    )
  ) +
  tema_relatorio()

composicao_institucional <- perfil |>
  count(grupo_administrativo_2024_final, name = "numero_escolas") |>
  mutate(
    grupo = str_wrap(grupo_administrativo_2024_final, 34),
    grupo = fct_reorder(grupo, numero_escolas)
  )

grafico_composicao <- ggplot(
  composicao_institucional,
  aes(x = numero_escolas, y = grupo)
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = numero_escolas),
    hjust = -0.15,
    fontface = "bold"
  ) +
  scale_x_continuous(
    labels = fmt_0,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Composição administrativa do universo institucional",
    subtitle = "Caracterização das 56 escolas com referência ao contexto de 2024",
    x = "Número de escolas",
    y = NULL,
    caption = paste(
      "Fonte: módulo 16 e Censo Escolar 2024.",
      "As categorias descrevem contexto administrativo, não qualidade escolar."
    )
  ) +
  tema_relatorio()

participacao_longa <- tabela_resultados |>
  select(ano_escolar, participacao_2025, participacao_2026) |>
  pivot_longer(
    cols = starts_with("participacao_"),
    names_to = "periodo",
    values_to = "participacao"
  ) |>
  mutate(
    periodo = recode(
      periodo,
      participacao_2025 = "2025 — linha de base",
      participacao_2026 = "2026"
    )
  )

grafico_participacao <- ggplot(
  participacao_longa,
  aes(
    x = factor(ano_escolar),
    y = participacao,
    shape = periodo
  )
) +
  geom_point(
    size = 3.2,
    position = position_dodge(width = 0.35)
  ) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1)) +
  labs(
    title = "Participação nas avaliações por ano escolar",
    subtitle = "Cobertura observada em 2025 e 2026",
    x = "Ano escolar",
    y = "Participação",
    caption = paste(
      "Fonte: módulo 16; CAEd 2025–2026.",
      "A avaliação de 2025 antecede o assessoramento."
    )
  ) +
  tema_relatorio()

painel_resultados <- tabela_resultados |>
  select(
    ano_escolar,
    participacao_2025, participacao_2026,
    proficiencia_2025, proficiencia_2026,
    series_escolas_alerta_composicao
  ) |>
  pivot_longer(
    cols = c(
      participacao_2025, participacao_2026,
      proficiencia_2025, proficiencia_2026
    ),
    names_to = c("indicador", "periodo"),
    names_pattern = "(participacao|proficiencia)_(2025|2026)",
    values_to = "valor"
  ) |>
  mutate(
    indicador = recode(
      indicador,
      participacao = "Participação (%)",
      proficiencia = "Proficiência média"
    ),
    periodo = if_else(periodo == "2025", "2025 — linha de base", "2026")
  )

grafico_painel <- ggplot(
  painel_resultados,
  aes(
    x = factor(ano_escolar),
    y = valor,
    shape = periodo
  )
) +
  geom_point(
    size = 3,
    position = position_dodge(width = 0.35)
  ) +
  facet_wrap(~ indicador, scales = "free_y", ncol = 1) +
  labs(
    title = "Participação e proficiência observada por ano escolar",
    subtitle = "Indicadores apresentados em conjunto, sem sugerir trajetória longitudinal",
    x = "Ano escolar",
    y = NULL,
    caption = paste(
      "Fonte: módulo 16; CAEd 2025–2026.",
      "As variações são descritivas, não causais, e devem considerar os alertas de composição da tabela T03."
    )
  ) +
  tema_relatorio()

codigos_carteiras <- analise_carteiras |>
  arrange(assessora_gerencial_2026) |>
  transmute(
    assessora_gerencial_2026,
    codigo_carteira = sprintf(
      "Carteira %02d",
      row_number()
    )
  )

dados_extensao <- analise_carteiras |>
  left_join(
    codigos_carteiras,
    by = "assessora_gerencial_2026"
  )

grafico_extensao_intensidade <- ggplot(
  dados_extensao,
  aes(
    x = numero_escolas,
    y = media_indice_operacional_escolas,
    size = soma_indice_operacional_carteira,
    label = codigo_carteira
  )
) +
  geom_point(shape = 21, fill = "white", stroke = 1) +
  geom_text(nudge_y = 1.5, check_overlap = TRUE, size = 3.1) +
  geom_hline(
    yintercept = mean(
      analise_carteiras$media_indice_operacional_escolas,
      na.rm = TRUE
    ),
    linetype = "dashed"
  ) +
  scale_size_continuous(range = c(4, 9), labels = fmt_1) +
  scale_x_continuous(
    breaks = sort(unique(analise_carteiras$numero_escolas)),
    labels = fmt_0
  ) +
  labs(
    title = "Extensão e componentes operacionais observados nas carteiras",
    subtitle = paste(
      "Número de escolas, índice médio e soma do índice são referências complementares;",
      "a linha tracejada é a média das 11 carteiras"
    ),
    x = "Número de escolas",
    y = "Índice operacional médio",
    size = "Soma do índice operacional",
    caption = paste(
      "Fonte: módulo 18.",
      "Os códigos evitam leitura nominal valorativa no corpo do relatório.",
      "O gráfico não mede integralmente a carga real e não estabelece ranking."
    )
  ) +
  tema_relatorio()

dimensoes_grafico <- tabela_dimensoes |>
  left_join(
    codigos_carteiras |>
      mutate(
        assessora_gerencial_2026 =
          str_to_title(assessora_gerencial_2026)
      ),
    by = "assessora_gerencial_2026"
  ) |>
  group_by(dimensao) |>
  mutate(
    referencia_dimensao = mean(
      escore_medio,
      na.rm = TRUE
    )
  ) |>
  ungroup()

grafico_dimensoes <- ggplot(
  dimensoes_grafico,
  aes(
    x = escore_medio,
    y = fct_rev(factor(codigo_carteira))
  )
) +
  geom_vline(
    aes(xintercept = referencia_dimensao),
    linetype = "dashed"
  ) +
  geom_point(size = 2.8) +
  facet_wrap(
    ~ dimensao,
    scales = "free_x",
    ncol = 1
  ) +
  scale_x_continuous(labels = fmt_1) +
  labs(
    title = "Dimensões operacionais das carteiras",
    subtitle = paste(
      "Volume, estrutura e complexidade administrativa;",
      "linhas tracejadas indicam referências médias não ordinais"
    ),
    x = "Escore médio",
    y = NULL,
    caption = paste(
      "Fonte: módulos 17 e 18.",
      "Resultados educacionais não integram estas dimensões.",
      "Os códigos evitam leitura nominal valorativa no corpo do relatório."
    )
  ) +
  tema_relatorio()

graficos <- list(
  G01 = grafico_universos,
  G02 = grafico_composicao,
  G03 = grafico_participacao,
  G04 = grafico_painel,
  G05 = grafico_extensao_intensidade,
  G06 = grafico_dimensoes
)

nomes_graficos <- c(
  G01 = "G01_universos_estudo.png",
  G02 = "G02_composicao_institucional_rede.png",
  G03 = "G03_participacao_por_ano_escolar.png",
  G04 = "G04_participacao_proficiencia_observada.png",
  G05 = "G05_extensao_intensidade_carteiras.png",
  G06 = "G06_dimensoes_operacionais_carteiras.png"
)

walk2(
  graficos,
  file.path(dir_candidato_corpo_graficos, nomes_graficos),
  salvar_grafico
)

# -------------------------------------------------------------------
# 11. Catálogo editorial candidato
# -------------------------------------------------------------------

arquivos_candidatos <- c(
  setNames(
    file.path(dir_candidato_corpo_tabelas, nomes_tabelas_corpo),
    names(nomes_tabelas_corpo)
  ),
  setNames(
    file.path(dir_candidato_corpo_graficos, nomes_graficos),
    names(nomes_graficos)
  ),
  setNames(
    file.path(dir_candidato_anexos_tabelas, nomes_anexos),
    names(nomes_anexos)
  )
)

catalogo_editorial <- matriz_editorial |>
  filter(id != "A09") |>
  mutate(
    caminho_candidato = unname(arquivos_candidatos[id]),
    arquivo = basename(caminho_candidato),
    existe = file.exists(caminho_candidato),
    tamanho_bytes = file.info(caminho_candidato)$size,
    hash_md5 = map_chr(caminho_candidato, hash_md5),
    nota_metodologica = case_when(
      id %in% c("T03", "G03", "G04", "A03", "A07") ~
        "Desempenho acompanhado de participação/composição; 2025 é linha de base anterior ao assessoramento.",
      id %in% c("T04", "T05", "G05", "G06", "A04", "A05", "A06") ~
        "Índice operacional parcial, relativo e sem componente educacional; não mede integralmente a carga real.",
      TRUE ~
        "Produto descritivo, não causal e sem classificação ordinal."
    )
  ) |>
  select(
    id, destino, tipo, titulo, universo, decisao,
    arquivo, existe, tamanho_bytes, hash_md5, nota_metodologica
  )

caminho_catalogo_candidato <- file.path(
  dir_candidatos, "catalogo_produtos_relatorio.csv"
)
salvar_tabela(catalogo_editorial, caminho_catalogo_candidato)

arquivos_candidatos <- c(
  arquivos_candidatos,
  CATALOGO = caminho_catalogo_candidato
)

# -------------------------------------------------------------------
# 12. Validações dos candidatos
# -------------------------------------------------------------------

validar_sem_termos_proibidos <- function(caminho) {
  if (tools::file_ext(caminho) != "csv") return(TRUE)
  texto <- paste(readLines(caminho, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  padrao <- paste(
    c(
      "percentil", "ranking",
      "posição ordinal", "posicao ordinal",
      "faixa do índice", "faixa_indice",
      "cenário alternativo", "cenario alternativo"
    ),
    collapse = "|"
  )
  !str_detect(str_to_lower(texto), padrao)
}

validar_colunas_editoriais <- function(caminho) {
  if (tools::file_ext(caminho) != "csv") {
    return(TRUE)
  }

  dados <- read_csv(
    caminho,
    show_col_types = FALSE,
    progress = FALSE,
    n_max = 1
  )

  padrao_coluna <- paste(
    c(
      "ranking",
      "^posicao$",
      "^posição$",
      "percentil",
      "faixa_indice",
      "faixa_carga",
      "cenario",
      "cenário",
      "classificacao_carga",
      "classificação_carga"
    ),
    collapse = "|"
  )

  !any(
    str_detect(
      str_to_lower(names(dados)),
      padrao_coluna
    )
  )
}

validacoes_candidatos <- tribble(
  ~teste, ~aprovado, ~observado, ~esperado,
  "Todos os candidatos existem",
  all(file.exists(arquivos_candidatos)),
  sum(file.exists(arquivos_candidatos)),
  length(arquivos_candidatos),

  "Todos os candidatos possuem conteúdo",
  all(file.info(arquivos_candidatos)$size > 0),
  sum(file.info(arquivos_candidatos)$size > 0),
  length(arquivos_candidatos),

  "Seis tabelas para o corpo",
  length(nomes_tabelas_corpo) == 6L,
  length(nomes_tabelas_corpo),
  6L,

  "Seis gráficos para o corpo",
  length(nomes_graficos) == 6L,
  length(nomes_graficos),
  6L,

  "Oito tabelas para os anexos",
  length(nomes_anexos) == 8L,
  length(nomes_anexos),
  8L,

  "T01 registra os seis universos/referências",
  nrow(tabela_universos) == 6L,
  nrow(tabela_universos),
  6L,

  "T04 contém exatamente 11 carteiras",
  nrow(tabela_carteiras) == 11L,
  nrow(tabela_carteiras),
  11L,

  "T05 contém três dimensões por carteira",
  nrow(tabela_dimensoes) == 33L,
  nrow(tabela_dimensoes),
  33L,

  "Anexo operacional contém 53 escolas",
  n_distinct(anexo_operacional$id_escola) == 53L,
  n_distinct(anexo_operacional$id_escola),
  53L,

  "Anexo não operacional contém três escolas",
  n_distinct(anexo_nao_operacional$id_escola) == 3L,
  n_distinct(anexo_nao_operacional$id_escola),
  3L,

  "Ausência de termos editoriais proibidos",
  all(map_lgl(arquivos_candidatos, validar_sem_termos_proibidos)),
  sum(map_lgl(arquivos_candidatos, validar_sem_termos_proibidos)),
  length(arquivos_candidatos),

  "Ausência de colunas classificatórias proibidas",
  all(map_lgl(arquivos_candidatos, validar_colunas_editoriais)),
  sum(map_lgl(arquivos_candidatos, validar_colunas_editoriais)),
  length(arquivos_candidatos)
)

write_csv(
  validacoes_candidatos,
  file.path(dir_execucao, "07_validacao_produtos_candidatos.csv"),
  na = ""
)

if (any(!validacoes_candidatos$aprovado)) {
  stop("Uma ou mais validações dos produtos candidatos falharam.")
}

manifesto_candidatos <- tibble(
  id_produto = names(arquivos_candidatos),
  caminho = unname(arquivos_candidatos),
  tamanho_bytes = as.numeric(file.info(arquivos_candidatos)$size),
  hash_md5 = map_chr(arquivos_candidatos, hash_md5)
)

write_csv(
  manifesto_candidatos,
  file.path(dir_execucao, "08_manifesto_candidatos.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 13. Verificação da preservação histórica do script
# -------------------------------------------------------------------

candidatos_historico_script <- c(
  here(
    "R",
    "historico_pre_refatoracao_modulo_21",
    "21_tabelas_graficos_relatorio.R"
  ),
  here(
    "R",
    "historico_pre_refatoracao_modulo_21",
    "modulo_21",
    "21_tabelas_graficos_relatorio_pre_refatoracao.R.txt"
  )
)

historico_script_existente <- candidatos_historico_script[
  file.exists(candidatos_historico_script)
]

if (length(historico_script_existente) != 1L) {
  stop(
    "A preservação histórica do módulo 21 deve existir em exatamente ",
    "um dos caminhos homologados. Caminhos verificados: ",
    paste(candidatos_historico_script, collapse = " | "),
    "."
  )
}

manifesto_historico_script <- tibble(
  caminho = caminho_normalizado(historico_script_existente[[1]]),
  tamanho_bytes = as.numeric(
    file.info(historico_script_existente[[1]])$size
  ),
  hash_md5 = hash_md5(historico_script_existente[[1]])
)

write_csv(
  manifesto_historico_script,
  file.path(dir_execucao, "09_manifesto_script_historico.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 14. Promoção transacional da pasta de resultados
# -------------------------------------------------------------------

if (dir.exists(dir_destino)) {
  stop(
    "A pasta final da execução já existe e não será sobrescrita: ",
    dir_destino,
    "."
  )
}

dir.create(
  dir_destino,
  recursive = TRUE,
  showWarnings = FALSE
)

itens_candidatos <- list.files(
  dir_candidatos,
  all.files = TRUE,
  full.names = TRUE,
  no.. = TRUE
)

resultado_copia <- file.copy(
  from = itens_candidatos,
  to = dir_destino,
  recursive = TRUE,
  overwrite = FALSE,
  copy.mode = TRUE,
  copy.date = TRUE
)

if (
  length(resultado_copia) != length(itens_candidatos) ||
    !all(resultado_copia)
) {
  unlink(
    dir_destino,
    recursive = TRUE,
    force = TRUE
  )

  stop(
    "Falha na promoção integral da pasta de resultados; ",
    "o destino incompleto foi removido."
  )
}

# -------------------------------------------------------------------
# 15. Verificação pós-promoção
# -------------------------------------------------------------------

listar_arquivos_relativos <- function(pasta) {
  arquivos <- list.files(
    pasta,
    recursive = TRUE,
    full.names = TRUE
  )

  arquivos <- arquivos[
    !file.info(arquivos)$isdir
  ]

  tibble(
    relativo = str_remove(
      caminho_normalizado(arquivos),
      paste0(
        "^",
        fixed(caminho_normalizado(pasta)),
        "/?"
      )
    ),
    caminho = arquivos,
    tamanho_bytes = as.numeric(file.info(arquivos)$size),
    hash_md5 = map_chr(arquivos, hash_md5)
  )
}

manifesto_candidato_integral <- listar_arquivos_relativos(dir_candidatos) |>
  rename(
    caminho_candidato = caminho,
    tamanho_candidato = tamanho_bytes,
    hash_candidato = hash_md5
  )

manifesto_promovido_integral <- listar_arquivos_relativos(dir_destino) |>
  rename(
    caminho_promovido = caminho,
    tamanho_promovido = tamanho_bytes,
    hash_promovido = hash_md5
  )

manifesto_promovidos <- full_join(
  manifesto_candidato_integral,
  manifesto_promovido_integral,
  by = "relativo"
) |>
  mutate(
    existe_candidato = !is.na(caminho_candidato),
    existe_promovido = !is.na(caminho_promovido),
    tamanho_identico =
      tamanho_candidato == tamanho_promovido,
    hash_identico =
      hash_candidato == hash_promovido
  )

write_csv(
  manifesto_promovidos,
  file.path(dir_execucao, "10_manifesto_produtos_promovidos.csv"),
  na = ""
)

if (
  any(!manifesto_promovidos$existe_candidato) ||
    any(!manifesto_promovidos$existe_promovido) ||
    any(!manifesto_promovidos$tamanho_identico) ||
    any(!manifesto_promovidos$hash_identico)
) {
  unlink(
    dir_destino,
    recursive = TRUE,
    force = TRUE
  )

  stop(
    "Falha na validação pós-promoção; ",
    "a pasta final foi removida."
  )
}

# -------------------------------------------------------------------
# 16. Documentação final
# -------------------------------------------------------------------

manifesto_entradas <- tibble(
  entrada = names(entradas),
  caminho = unname(entradas),
  tamanho_bytes = as.numeric(file.info(entradas)$size),
  hash_md5 = map_chr(entradas, hash_md5)
)

write_csv(
  manifesto_entradas,
  file.path(dir_execucao, "11_manifesto_entradas.csv"),
  na = ""
)

contrato_execucao <- tibble(
  campo = c(
    "id_execucao", "data_hora", "branch_esperada", "branch_observada",
    "commit_base", "commit_observado", "script_canonico",
    "md5_script_executado", "universo_institucional",
    "universo_operacional_2026", "universo_avaliativo_2025",
    "universo_avaliativo_2026", "assessoras_gerenciais_2026"
  ),
  valor = c(
    id_execucao,
    format(instante_execucao, "%Y-%m-%d %H:%M:%S"),
    branch_esperada,
    branch_observada,
    commit_base_integracao,
    commit_observado,
    caminho_normalizado(caminho_script),
    hash_md5(caminho_script),
    "56", "53", "54", "56", "11"
  )
)

write_csv(
  contrato_execucao,
  file.path(dir_execucao, "12_contrato_execucao.csv"),
  na = ""
)

produtos_homologacao <- manifesto_promovidos |>
  mutate(
    id_produto = case_when(
      str_detect(relativo, "^corpo/tabelas/") ~ str_extract(basename(relativo), "^T\\d+"),
      str_detect(relativo, "^corpo/graficos/") ~ str_extract(basename(relativo), "^G\\d+"),
      str_detect(relativo, "^anexos/tabelas/") ~ str_extract(basename(relativo), "^A\\d+"),
      basename(relativo) == "catalogo_produtos_relatorio.csv" ~ "CATALOGO",
      TRUE ~ NA_character_
    )
  ) |>
  transmute(
    id_produto,
    caminho = caminho_promovido,
    tamanho_bytes = tamanho_promovido,
    hash_md5 = hash_promovido,
    criterio_homologacao = case_when(
      str_detect(id_produto, "^T") ~
        "Conferir indicadores, universo, rótulos, arredondamentos e cautelas.",
      str_detect(id_produto, "^G") ~
        "Conferir legibilidade, escalas, títulos, notas, ausência de ordenação valorativa e adequação institucional.",
      str_detect(id_produto, "^A") ~
        "Conferir seleção explícita de campos, universo e ausência de colunas proibidas.",
      TRUE ~
        "Conferir arquitetura editorial e metadados."
    )
  )

write_csv(
  produtos_homologacao,
  file.path(dir_execucao, "13_produtos_para_homologacao.csv"),
  na = ""
)

writeLines(
  capture.output(sessionInfo()),
  file.path(dir_execucao, "14_session_info.txt"),
  useBytes = TRUE
)

resumo <- c(
  "MÓDULO 21 — TABELAS E GRÁFICOS PARA O RELATÓRIO",
  paste0("Execução: ", id_execucao),
  paste0("Branch: ", branch_observada),
  paste0("Commit-base: ", commit_observado),
  "",
  "Universos:",
  "- institucional: 56 escolas;",
  "- operacional de 2026: 53 escolas;",
  "- institucional não operacional: 3 escolas;",
  "- avaliativo de 2025: 54 escolas;",
  "- avaliativo de 2026: 56 escolas;",
  "- gerencial: 11 assessoras.",
  "",
  "Produtos promovidos:",
  paste0("- tabelas do corpo: ", length(nomes_tabelas_corpo), ";"),
  paste0("- gráficos do corpo: ", length(nomes_graficos), ";"),
  paste0("- tabelas dos anexos: ", length(nomes_anexos), ";"),
  "- catálogo editorial: 1.",
  "",
  "Cautelas:",
  "- estudo observacional e descritivo;",
  "- 2025 é linha de base anterior ao assessoramento;",
  "- educação não integra o índice operacional;",
  "- resultados escolares não avaliam assessoras;",
  "- índice operacional não representa integralmente a carga real;",
  "- não foram produzidos rankings, posições, faixas, percentis ou cenários.",
  "",
  "Próximo passo:",
  "- devolver os produtos listados em 13_produtos_para_homologacao.csv;",
  "- não iniciar o módulo 22 antes da homologação."
)

writeLines(
  resumo,
  file.path(dir_execucao, "15_resumo_execucao.txt"),
  useBytes = TRUE
)

cat(
  paste0(
    "\nMódulo 21 concluído com sucesso.\n",
    "Execução: ", id_execucao, "\n",
    "Tabelas do corpo: ", length(nomes_tabelas_corpo), "\n",
    "Gráficos do corpo: ", length(nomes_graficos), "\n",
    "Tabelas dos anexos: ", length(nomes_anexos), "\n",
    "Documentação: ", dir_execucao, "\n",
    "Aguarde a homologação antes de iniciar o módulo 22.\n"
  )
)
