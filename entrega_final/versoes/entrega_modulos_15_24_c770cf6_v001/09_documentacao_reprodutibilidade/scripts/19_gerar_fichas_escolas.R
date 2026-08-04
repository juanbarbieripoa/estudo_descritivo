# ===================================================================
# 19_gerar_fichas_escolas.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# ===================================================================
#
# Produz 56 fichas institucionais:
# - 53 operacionais, com índice homologado do módulo 17;
# - 3 não operacionais: ESC_001, ESC_055 e ESC_056.
#
# Resultados educacionais permanecem fora do índice. O módulo não cria
# ranking, faixa, percentil, cenário ou nova normalização.
#
# Versões históricas conhecidas:
# - 19_gerar_fichas_escolas.R
# - 19_gerar_fichas_escolas_corrigido.R
# - 19_gerar_fichas_escolas_corrigido_v2.R
#
# Este arquivo é o único caminho canônico executável.
# ===================================================================

library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Contrato e caminhos
# -------------------------------------------------------------------

instante_execucao <- Sys.time()
id_execucao <- format(instante_execucao, "%Y%m%d_%H%M%S")

branch_esperada <- "refatoracao_modulo_21"
commit_base_integracao <- "d098ad77be7233829bc235112aaa1fe86f9b86a5"

TENTAR_GERAR_PDF_COMPILADO <- TRUE

caminho_script <- here("R", "19_gerar_fichas_escolas.R")

manifestos <- c(
  modulo_16 = here(
    "documentacao", "perfil_escola",
    "execucao_20260726_223447",
    "19_manifesto_produtos_modulo_16.csv"
  ),
  modulo_17 = here(
    "documentacao", "indice_complexidade",
    "execucao_20260728_220908",
    "19_manifesto_produtos_modulo_17.csv"
  ),
  modulo_18 = here(
    "documentacao", "analise_carteiras",
    "execucao_20260729_221336",
    "18_manifesto_produtos_modulo_18.csv"
  )
)

entradas <- c(
  perfil_gerencial_csv = here("dados_finais", "perfil_escola_gerencial.csv"),
  perfil_gerencial_rds = here("dados_finais", "perfil_escola_gerencial.rds"),
  perfil_serie_csv = here("dados_finais", "perfil_escola_serie_compacto.csv"),
  perfil_serie_rds = here("dados_finais", "perfil_escola_serie_compacto.rds"),
  indice_csv = here("dados_finais", "indice_carga_potencial_escola.csv"),
  indice_rds = here("dados_finais", "indice_carga_potencial_escola.rds"),
  diagnostico_csv = here(
    "dados_finais", "componentes_educacionais_escola_serie.csv"
  ),
  diagnostico_rds = here(
    "dados_finais", "componentes_educacionais_escola_serie.rds"
  ),
  detalhe_csv = here("dados_finais", "carteira_escola_detalhe.csv"),
  detalhe_rds = here("dados_finais", "carteira_escola_detalhe.rds"),
  universo_csv = here("dados_finais", "universo_institucional_carteiras.csv"),
  universo_rds = here("dados_finais", "universo_institucional_carteiras.rds")
)

saidas <- c(
  base_csv = here("dados_finais", "base_fichas_escolas.csv"),
  base_rds = here("dados_finais", "base_fichas_escolas.rds"),
  indice_csv = here("dados_finais", "indice_fichas_escolas.csv"),
  indice_rds = here("dados_finais", "indice_fichas_escolas.rds"),
  dicionario_csv = here(
    "documentacao", "fichas_escolas",
    "dicionario_base_fichas_escolas.csv"
  )
)

ids_excluidos <- c("ESC_001", "ESC_055", "ESC_056")

pasta_documentacao <- here("documentacao", "fichas_escolas")
pasta_execucao <- file.path(
  pasta_documentacao, paste0("execucao_", id_execucao)
)

pasta_resultados <- here("resultados", "fichas_escolas")
pasta_resultados_final <- file.path(
  pasta_resultados, paste0("execucao_", id_execucao)
)

pasta_transacao <- file.path(
  pasta_resultados, "transacoes", paste0("execucao_", id_execucao)
)
pasta_candidatos <- file.path(pasta_transacao, "candidatos")
pasta_html <- file.path(pasta_candidatos, "html")
pasta_graficos <- file.path(pasta_candidatos, "graficos")
pasta_rollback <- file.path(pasta_transacao, "rollback")

pasta_historico_dados <- here(
  "dados_finais", "historico", "fichas_escolas",
  paste0("pre_refatoracao_execucao_", id_execucao)
)

walk(
  c(
    pasta_documentacao, pasta_execucao, pasta_resultados,
    pasta_transacao, pasta_candidatos, pasta_html,
    pasta_graficos, pasta_rollback, pasta_historico_dados
  ),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# -------------------------------------------------------------------
# 2. Funções auxiliares
# -------------------------------------------------------------------

md5 <- function(x) {
  if (!file.exists(x)) return(NA_character_)
  unname(tools::md5sum(x))
}

norm <- function(x, must = TRUE) {
  normalizePath(x, winslash = "/", mustWork = must)
}

git_cmd <- function(args) {
  out <- tryCatch(
    suppressWarnings(system2("git", args, stdout = TRUE, stderr = FALSE)),
    error = function(e) character()
  )
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) return(character())
  str_squish(as.character(out))
}

git_head <- function() {
  x <- git_cmd(c("-C", shQuote(here()), "rev-parse", "HEAD"))
  x <- x[str_detect(x, "^[0-9a-fA-F]{40}$")]
  if (length(x) != 1L) return(NA_character_)
  str_to_lower(x[[1]])
}

git_branch <- function() {
  x <- git_cmd(c("-C", shQuote(here()), "branch", "--show-current"))
  if (length(x) != 1L) return(NA_character_)
  x[[1]]
}

tipo <- function(x) {
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

logico_seguro <- function(x, nome) {
  if (is.logical(x)) return(x)
  z <- str_to_lower(str_squish(as.character(x)))
  out <- case_when(
    is.na(z) | z == "" ~ NA,
    z %in% c("true", "t", "1", "sim", "s") ~ TRUE,
    z %in% c("false", "f", "0", "nao", "não", "n") ~ FALSE,
    TRUE ~ NA
  )
  if (any(!is.na(z) & z != "" & is.na(out))) {
    stop("Valor lógico inválido em `", nome, "`.")
  }
  out
}

converter <- function(x, modelo, nome) {
  switch(
    tipo(modelo),
    character = as.character(x),
    logical = logico_seguro(x, nome),
    integer = suppressWarnings(as.integer(x)),
    double = suppressWarnings(as.double(x)),
    date = as.Date(x),
    datetime = as.POSIXct(x, tz = "UTC"),
    stop("Tipo não suportado em `", nome, "`.")
  )
}

ler_csv_modelo <- function(caminho, modelo) {
  bruto <- read_csv(
    caminho,
    col_types = cols(.default = col_character()),
    na = c("", "NA"),
    trim_ws = FALSE,
    name_repair = "minimal",
    show_col_types = FALSE,
    progress = FALSE
  )
  if (!identical(names(bruto), names(modelo))) {
    stop("Estrutura CSV divergente em: ", caminho)
  }
  out <- bruto
  for (nm in names(modelo)) {
    out[[nm]] <- converter(bruto[[nm]], modelo[[nm]], nm)
  }
  as_tibble(out)
}

comparar_par <- function(rds, csv, chaves, fonte) {
  ord <- function(x) {
    as_tibble(x) |>
      arrange(across(all_of(chaves))) |>
      select(all_of(names(x)))
  }
  a <- ord(rds)
  b <- ord(csv)
  nomes <- identical(names(a), names(b))
  tipos <- nomes && identical(map_chr(a, tipo), map_chr(b, tipo))
  dimensoes <- identical(dim(a), dim(b))
  cmp <- if (nomes && tipos && dimensoes) {
    all.equal(a, b, check.attributes = FALSE, tolerance = 1e-12)
  } else {
    "estrutura divergente"
  }
  iguais <- isTRUE(cmp)
  list(
    dados = a,
    diagnostico = tibble(
      fonte = fonte,
      linhas_rds = nrow(a),
      linhas_csv = nrow(b),
      colunas_rds = ncol(a),
      colunas_csv = ncol(b),
      mesmos_nomes_e_ordem = nomes,
      mesmos_tipos = tipos,
      mesmas_dimensoes = dimensoes,
      mesmos_valores = iguais,
      detalhe = if (iguais) "equivalentes" else paste(cmp, collapse = " | "),
      aprovado = nomes && tipos && dimensoes && iguais
    )
  )
}

ler_par <- function(nome_rds, nome_csv, chaves, fonte) {
  rds <- readRDS(entradas[[nome_rds]])
  if (!is.data.frame(rds)) stop("RDS inválido: ", fonte)
  csv <- ler_csv_modelo(entradas[[nome_csv]], rds)
  comparar_par(rds, csv, chaves, fonte)
}

fmt_num <- function(x, d = 0) {
  if (length(x) == 0L || is.na(x[[1]])) return("Não disponível")
  formatC(
    as.numeric(x[[1]]), format = "f", digits = d,
    big.mark = ".", decimal.mark = ","
  )
}

fmt_pct <- function(x, d = 1) {
  if (length(x) == 0L || is.na(x[[1]])) return("Não disponível")
  paste0(fmt_num(x, d), "%")
}

fmt_texto <- function(x, padrao = "Não informado") {
  if (
    length(x) == 0L || is.na(x[[1]]) ||
      !nzchar(str_trim(as.character(x[[1]])))
  ) return(padrao)
  as.character(x[[1]])
}

sim_nao <- function(x) {
  if (length(x) == 0L || is.na(x[[1]])) return("Não informado")
  ifelse(isTRUE(x[[1]]), "Sim", "Não")
}

escape_html <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x |>
    str_replace_all("&", "&amp;") |>
    str_replace_all("<", "&lt;") |>
    str_replace_all(">", "&gt;") |>
    str_replace_all('"', "&quot;") |>
    str_replace_all("'", "&#39;")
}

slug <- function(x) {
  z <- suppressWarnings(iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"))
  z[is.na(z)] <- x[is.na(z)]
  z <- z |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_replace_all("(^-+|-+$)", "") |>
    str_sub(1, 80)
  if_else(is.na(z) | z == "", "escola", z)
}

soma_segura <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

media_ponderada <- function(x, w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

validacao <- function(
    teste, categoria, severidade, observado,
    criterio, resultado, observacao = ""
) {
  tibble(
    teste = teste,
    categoria = categoria,
    severidade = severidade,
    valor_observado = as.character(observado),
    criterio = criterio,
    resultado = isTRUE(resultado),
    nivel = if_else(isTRUE(resultado), "OK", str_to_upper(severidade)),
    observacao = observacao
  )
}

inventariar <- function(nome, caminho) {
  tibble(
    arquivo = nome,
    caminho = norm(caminho, FALSE),
    existe = file.exists(caminho),
    tamanho_bytes = if (file.exists(caminho)) file.info(caminho)$size else NA_real_,
    md5 = md5(caminho)
  )
}

nao_vazio <- function(caminho, minimo = 100L) {
  file.exists(caminho) &&
    is.finite(file.info(caminho)$size) &&
    file.info(caminho)$size >= minimo
}

copiar_validado <- function(origem, destino, overwrite = FALSE) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(
    origem, destino, overwrite = overwrite,
    copy.mode = TRUE, copy.date = TRUE
  )
  if (!ok || !identical(md5(origem), md5(destino))) {
    stop("Falha ao copiar com integridade: ", origem)
  }
}

# -------------------------------------------------------------------
# 3. Bloqueios Git e integridade dos insumos
# -------------------------------------------------------------------

if (!file.exists(caminho_script)) {
  stop("Script canônico ausente: ", caminho_script)
}

if (!identical(
  norm(caminho_script, FALSE),
  norm(here("R", "19_gerar_fichas_escolas.R"), FALSE)
)) {
  stop("Execução fora do caminho canônico.")
}

branch <- git_branch()
head <- git_head()

if (is.na(branch) || branch != branch_esperada) {
  stop("Branch divergente: ", branch)
}
if (is.na(head) || head != commit_base_integracao) {
  stop("HEAD divergente: ", head)
}

obrigatorios <- c(entradas, manifestos)
faltantes <- obrigatorios[!file.exists(obrigatorios)]
if (length(faltantes) > 0L) {
  stop(
    "Arquivos obrigatórios ausentes:\n",
    paste(names(faltantes), faltantes, sep = ": ", collapse = "\n")
  )
}

ler_manifesto <- function(caminho, nome) {
  x <- read_csv(
    caminho,
    show_col_types = FALSE,
    na = c("", "NA")
  )
  
  colunas_minimas <- c(
    "caminho",
    "tamanho_bytes",
    "md5"
  )
  
  ausentes <- setdiff(
    colunas_minimas,
    names(x)
  )
  
  if (length(ausentes) > 0L) {
    stop(
      "Manifesto ",
      nome,
      " sem colunas: ",
      paste(
        ausentes,
        collapse = ", "
      )
    )
  }
  
  if (
    !"produto" %in% names(x) &&
    !"arquivo" %in% names(x)
  ) {
    stop(
      "Manifesto ",
      nome,
      " não possui coluna identificadora `produto` ou `arquivo`."
    )
  }
  
  x
}

m16 <- ler_manifesto(manifestos[["modulo_16"]], "módulo 16")
m17 <- ler_manifesto(manifestos[["modulo_17"]], "módulo 17")
m18 <- ler_manifesto(manifestos[["modulo_18"]], "módulo 18")

hash_manifesto <- function(manifesto, caminho, origem) {
  alvo <- basename(caminho)
  cand <- manifesto |> filter(basename(caminho) == alvo)
  if (nrow(cand) != 1L) {
    stop("Arquivo não localizado unicamente no manifesto ", origem, ": ", alvo)
  }
  str_to_lower(cand$md5[[1]])
}

hashes_esperados <- c(
  perfil_gerencial_csv = hash_manifesto(m16, entradas[["perfil_gerencial_csv"]], "16"),
  perfil_gerencial_rds = hash_manifesto(m16, entradas[["perfil_gerencial_rds"]], "16"),
  perfil_serie_csv = hash_manifesto(m16, entradas[["perfil_serie_csv"]], "16"),
  perfil_serie_rds = hash_manifesto(m16, entradas[["perfil_serie_rds"]], "16"),
  indice_csv = hash_manifesto(m17, entradas[["indice_csv"]], "17"),
  indice_rds = hash_manifesto(m17, entradas[["indice_rds"]], "17"),
  diagnostico_csv = hash_manifesto(m17, entradas[["diagnostico_csv"]], "17"),
  diagnostico_rds = hash_manifesto(m17, entradas[["diagnostico_rds"]], "17"),
  detalhe_csv = hash_manifesto(m18, entradas[["detalhe_csv"]], "18"),
  detalhe_rds = hash_manifesto(m18, entradas[["detalhe_rds"]], "18"),
  universo_csv = hash_manifesto(m18, entradas[["universo_csv"]], "18"),
  universo_rds = hash_manifesto(m18, entradas[["universo_rds"]], "18")
)

hashes_observados <- map_chr(entradas, md5)
div <- names(hashes_observados)[
  str_to_lower(hashes_observados) != str_to_lower(hashes_esperados)
]
if (length(div) > 0L) {
  stop("Hashes divergentes: ", paste(div, collapse = ", "))
}

# -------------------------------------------------------------------
# 4. Leitura dos seis pares
# -------------------------------------------------------------------

p_perfil <- ler_par(
  "perfil_gerencial_rds", "perfil_gerencial_csv",
  "id_escola", "perfil_escola_gerencial"
)

p_serie <- ler_par(
  "perfil_serie_rds", "perfil_serie_csv",
  c("id_escola", "ano_escolar", "componente"),
  "perfil_escola_serie_compacto"
)

p_indice <- ler_par(
  "indice_rds", "indice_csv",
  "id_escola", "indice_carga_potencial_escola"
)

p_diag <- ler_par(
  "diagnostico_rds", "diagnostico_csv",
  c("id_escola", "ano_escolar", "componente"),
  "componentes_educacionais_escola_serie"
)

p_detalhe <- ler_par(
  "detalhe_rds", "detalhe_csv",
  c("assessora_gerencial_2026", "id_escola"),
  "carteira_escola_detalhe"
)

p_universo <- ler_par(
  "universo_rds", "universo_csv",
  "id_escola", "universo_institucional_carteiras"
)

equiv_entradas <- bind_rows(
  p_perfil$diagnostico, p_serie$diagnostico,
  p_indice$diagnostico, p_diag$diagnostico,
  p_detalhe$diagnostico, p_universo$diagnostico
)

if (!all(equiv_entradas$aprovado)) {
  stop("Há divergência CSV–RDS nas entradas.")
}

perfil <- p_perfil$dados
serie <- p_serie$dados
indice <- p_indice$dados
diagnostico <- p_diag$dados
detalhe <- p_detalhe$dados
universo <- p_universo$dados

# -------------------------------------------------------------------
# 5. Contratos de colunas e universos
# -------------------------------------------------------------------

exigir <- function(dados, cols, fonte) {
  aus <- setdiff(cols, names(dados))
  if (length(aus) > 0L) {
    stop("Colunas ausentes em ", fonte, ": ", paste(aus, collapse = ", "))
  }
}

exigir(
  perfil,
  c(
    "id_escola", "codigo_inep", "nome_canonico",
    "assessora_vinculo_administrativo", "assessora_gerencial_2026",
    "elegivel_assessoramento_2026", "recebe_assessoramento_2026",
    "incluir_indice_carga_2026", "status_carga_operacional_2026",
    "grupo_exposicao_2026"
  ),
  "perfil"
)

exigir(
  serie,
  c(
    "id_escola", "codigo_inep", "nome_canonico",
    "assessora_gerencial_2026", "ano_escolar", "componente",
    "previstos_2025", "avaliados_2025", "taxa_participacao_2025",
    "proficiencia_media_2025", "pct_defasagem_2025",
    "pct_intermediario_2025", "pct_adequado_2025",
    "previstos_2026", "avaliados_2026", "taxa_participacao_2026",
    "proficiencia_media_2026", "pct_defasagem_2026",
    "pct_intermediario_2026", "pct_adequado_2026",
    "painel_resultado_balanceado", "delta_participacao",
    "delta_proficiencia", "alerta_composicao_serie"
  ),
  "perfil_serie"
)

exigir(
  indice,
  c(
    "id_escola", "codigo_inep", "nome_canonico",
    "assessora_vinculo_administrativo", "assessora_gerencial_2026",
    "score_dimensao_volume", "score_dimensao_estrutural",
    "score_dimensao_administrativa", "contribuicao_volume",
    "contribuicao_estrutural", "contribuicao_administrativa",
    "indice_carga_potencial_operacional", "cobertura_indice_operacional",
    "interpretacao_cautelosa", "resultados_educacionais_no_indice"
  ),
  "indice"
)

exigir(
  diagnostico,
  c(
    "id_escola", "ano_escolar", "componente",
    "peso_no_indice_carga_operacional",
    "uso_no_indice_carga_operacional"
  ),
  "diagnostico"
)

exigir(
  detalhe,
  c(
    "id_escola", "assessora_gerencial_2026",
    "assessora_vinculo_administrativo",
    "indice_carga_potencial_operacional"
  ),
  "detalhe"
)

exigir(
  universo,
  c(
    "id_escola", "codigo_inep", "nome_canonico",
    "assessora_vinculo_administrativo", "assessora_gerencial_2026",
    "elegivel_assessoramento_2026", "recebe_assessoramento_2026",
    "incluir_indice_carga_2026", "status_carga_operacional_2026",
    "grupo_exposicao_2026", "incluida_analise_operacional",
    "motivo_nao_inclusao", "nota_institucional"
  ),
  "universo"
)

universo <- universo |>
  mutate(
    incluida_analise_operacional = logico_seguro(
      incluida_analise_operacional, "incluida_analise_operacional"
    )
  )

indice <- indice |>
  mutate(
    interpretacao_cautelosa = logico_seguro(
      interpretacao_cautelosa, "interpretacao_cautelosa"
    ),
    resultados_educacionais_no_indice = logico_seguro(
      resultados_educacionais_no_indice,
      "resultados_educacionais_no_indice"
    )
  )

diagnostico <- diagnostico |>
  mutate(
    uso_no_indice_carga_operacional = logico_seguro(
      uso_no_indice_carga_operacional,
      "uso_no_indice_carga_operacional"
    )
  )

ids_universo <- sort(unique(universo$id_escola))
ids_indice <- sort(unique(indice$id_escola))
ids_excluidos_observados <- sort(setdiff(ids_universo, ids_indice))

if (nrow(perfil) != 56L || nrow(serie) != 280L) {
  stop("Universo institucional divergente.")
}
if (nrow(indice) != 53L || nrow(detalhe) != 53L) {
  stop("Universo operacional divergente.")
}
if (nrow(diagnostico) != 265L) {
  stop("Diagnóstico operacional divergente.")
}
if (!setequal(ids_excluidos_observados, ids_excluidos)) {
  stop("Exclusões operacionais divergentes.")
}
if (!setequal(unique(perfil$id_escola), ids_universo)) {
  stop("Perfil e universo institucional divergem.")
}
if (!setequal(unique(serie$id_escola), ids_universo)) {
  stop("Perfil por série não cobre as 56 escolas.")
}
if (!setequal(unique(detalhe$id_escola), ids_indice)) {
  stop("Detalhe e índice divergem.")
}
if (!setequal(unique(diagnostico$id_escola), ids_indice)) {
  stop("Diagnóstico e índice divergem.")
}

numero_assessoras <- n_distinct(detalhe$assessora_gerencial_2026)
if (numero_assessoras != 11L) stop("Número de assessoras divergente.")

# -------------------------------------------------------------------
# 6. Base institucional de 56 fichas
# -------------------------------------------------------------------

contextuais <- intersect(
  c(
    "grupo_administrativo_2024_final", "tipo_vinculo_rede_final",
    "status_rede_2025_final", "observacao_administrativa_final",
    "municipalizada_apos_2024", "escola_nova_recente",
    "matriculas_anos_iniciais", "turmas_anos_iniciais",
    "docentes_anos_iniciais", "alunos_por_turma_anos_iniciais",
    "alunos_por_docente_anos_iniciais",
    "pct_matriculas_educacao_especial",
    "pct_matriculas_anos_iniciais_integral",
    "pct_matriculas_transporte_publico",
    "indice_infraestrutura_basica",
    "numero_etapas_amplas_ofertadas"
  ),
  names(perfil)
)

resumo_edu <- serie |>
  group_by(id_escola) |>
  summarise(
    previstos_total_2025 = soma_segura(previstos_2025),
    avaliados_total_2025 = soma_segura(avaliados_2025),
    participacao_2025 = if_else(
      previstos_total_2025 > 0,
      100 * avaliados_total_2025 / previstos_total_2025,
      NA_real_
    ),
    proficiencia_2025 = media_ponderada(
      proficiencia_media_2025, avaliados_2025
    ),
    previstos_total_2026 = soma_segura(previstos_2026),
    avaliados_total_2026 = soma_segura(avaliados_2026),
    participacao_2026 = if_else(
      previstos_total_2026 > 0,
      100 * avaliados_total_2026 / previstos_total_2026,
      NA_real_
    ),
    proficiencia_2026 = media_ponderada(
      proficiencia_media_2026, avaliados_2026
    ),
    series_alerta_composicao = sum(
      coalesce(
        logico_seguro(alerta_composicao_serie, "alerta_composicao_serie"),
        FALSE
      )
    ),
    .groups = "drop"
  )

base <- universo |>
  left_join(
    perfil |> select(id_escola, all_of(contextuais)),
    by = "id_escola"
  ) |>
  left_join(
    indice |>
      select(
        id_escola, score_dimensao_volume,
        score_dimensao_estrutural,
        score_dimensao_administrativa,
        contribuicao_volume, contribuicao_estrutural,
        contribuicao_administrativa,
        indice_carga_potencial_operacional,
        cobertura_indice_operacional,
        interpretacao_cautelosa,
        resultados_educacionais_no_indice
      ),
    by = "id_escola"
  ) |>
  left_join(resumo_edu, by = "id_escola") |>
  mutate(
    tipo_ficha = if_else(
      incluida_analise_operacional,
      "OPERACIONAL",
      "INSTITUCIONAL_NAO_OPERACIONAL"
    ),
    divergencia_vinculo_gerencial = coalesce(
      assessora_vinculo_administrativo, "<NA>"
    ) != coalesce(assessora_gerencial_2026, "<NA>"),
    slug_escola = paste0(id_escola, "_", slug(nome_canonico)),
    html_nome = paste0(slug_escola, ".html"),
    grafico_operacional_nome = if_else(
      incluida_analise_operacional,
      paste0(slug_escola, "_dimensoes_operacionais.png"),
      NA_character_
    ),
    grafico_participacao_nome = paste0(slug_escola, "_participacao.png"),
    grafico_proficiencia_nome = paste0(slug_escola, "_proficiencia.png")
  ) |>
  arrange(id_escola)

# -------------------------------------------------------------------
# 7. Gráficos
# -------------------------------------------------------------------

tema <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

grafico_operacional <- function(escola, caminho) {
  d <- tibble(
    dimensao = factor(
      c("Volume", "Estrutura", "Complexidade administrativa"),
      levels = c(
        "Complexidade administrativa", "Estrutura", "Volume"
      )
    ),
    escore = c(
      escola$score_dimensao_volume,
      escola$score_dimensao_estrutural,
      escola$score_dimensao_administrativa
    )
  )
  p <- ggplot(d, aes(escore, dimensao)) +
    geom_col() +
    scale_x_continuous(limits = c(0, 105)) +
    labs(
      title = "Dimensões do índice operacional",
      subtitle = "Volume 40%; estrutura 35%; administração 25%.",
      x = "Escore relativo", y = NULL
    ) +
    tema()
  ggsave(caminho, p, width = 8, height = 4.5, dpi = 150)
}

grafico_serie <- function(dados, variavel_2025, variavel_2026, titulo, y, caminho) {
  d <- dados |>
    select(
      ano_escolar,
      valor_2025 = all_of(variavel_2025),
      valor_2026 = all_of(variavel_2026)
    ) |>
    pivot_longer(
      starts_with("valor_"),
      names_to = "ano",
      values_to = "valor"
    ) |>
    mutate(
      ano = if_else(ano == "valor_2025", "2025", "2026"),
      serie = paste0(ano_escolar, "º ano")
    )
  p <- ggplot(d, aes(serie, valor, group = ano, linetype = ano)) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    labs(
      title = titulo,
      subtitle = "Resultado descritivo, observacional e não causal.",
      x = NULL, y = y, linetype = "Ano"
    ) +
    tema()
  ggsave(caminho, p, width = 8, height = 4.5, dpi = 150)
}

# -------------------------------------------------------------------
# 8. HTML
# -------------------------------------------------------------------

css <- paste0(
  "<style>",
  "body{font-family:Arial,sans-serif;background:#f4f6f8;color:#263238;margin:0}",
  ".pagina{max-width:1080px;margin:auto;background:white;padding:28px 36px}",
  "h1{font-size:25px;margin:0 0 8px}h2{font-size:18px;border-bottom:2px solid #cfd8dc;padding-bottom:6px;margin-top:26px}",
  ".grid{display:grid;grid-template-columns:repeat(3,1fr);gap:10px}",
  ".card{border:1px solid #cfd8dc;border-radius:6px;padding:10px;background:#fafafa}",
  ".rot{font-size:11px;text-transform:uppercase;color:#607d8b;font-weight:bold}",
  ".val{font-size:16px;margin-top:4px}.selo{display:inline-block;padding:6px 10px;border-radius:14px;background:#eceff1;margin:4px;font-size:12px}",
  ".nota{border-left:4px solid #78909c;background:#f5f7f8;padding:12px;margin:14px 0;font-size:12px}",
  ".alerta{border-left:4px solid #ef6c00;background:#fff8e1;padding:12px;margin:14px 0;font-size:12px}",
  "table{width:100%;border-collapse:collapse;font-size:12px}th,td{border:1px solid #cfd8dc;padding:6px}th{background:#eceff1}",
  "img{width:100%;max-width:900px;border:1px solid #cfd8dc;margin:12px 0}",
  ".rodape{margin-top:28px;border-top:1px solid #cfd8dc;padding-top:10px;font-size:10px;color:#607d8b}",
  "@media print{body{background:white}.pagina{max-width:none}.quebra{page-break-before:always}}",
  "</style>"
)

card <- function(rotulo, valor) {
  paste0(
    "<div class='card'><div class='rot'>", escape_html(rotulo),
    "</div><div class='val'>", escape_html(valor), "</div></div>"
  )
}

tabela_series <- function(d) {
  linhas <- map_chr(seq_len(nrow(d)), function(i) {
    x <- d[i, ]
    paste0(
      "<tr><td>", x$ano_escolar, "º</td>",
      "<td>", fmt_num(x$previstos_2025), "</td>",
      "<td>", fmt_num(x$avaliados_2025), "</td>",
      "<td>", fmt_pct(x$taxa_participacao_2025), "</td>",
      "<td>", fmt_num(x$proficiencia_media_2025, 1), "</td>",
      "<td>", fmt_num(x$previstos_2026), "</td>",
      "<td>", fmt_num(x$avaliados_2026), "</td>",
      "<td>", fmt_pct(x$taxa_participacao_2026), "</td>",
      "<td>", fmt_num(x$proficiencia_media_2026, 1), "</td></tr>"
    )
  })
  paste0(
    "<table><thead><tr><th>Série</th><th>Prev. 2025</th>",
    "<th>Aval. 2025</th><th>Part. 2025</th><th>Prof. 2025</th>",
    "<th>Prev. 2026</th><th>Aval. 2026</th><th>Part. 2026</th>",
    "<th>Prof. 2026</th></tr></thead><tbody>",
    paste(linhas, collapse = ""), "</tbody></table>"
  )
}

render_html <- function(escola, dserie) {
  operacional <- isTRUE(escola$incluida_analise_operacional[[1]])
  bloco_indice <- if (operacional) {
    paste0(
      "<h2>Índice operacional</h2><div class='grid'>",
      card("Índice operacional", fmt_num(escola$indice_carga_potencial_operacional, 1)),
      card("Volume", fmt_num(escola$score_dimensao_volume, 1)),
      card("Estrutura", fmt_num(escola$score_dimensao_estrutural, 1)),
      card("Complexidade administrativa", fmt_num(escola$score_dimensao_administrativa, 1)),
      card("Cobertura", fmt_pct(100 * escola$cobertura_indice_operacional, 1)),
      card("Leitura cautelosa", sim_nao(escola$interpretacao_cautelosa)),
      "</div><img src='../graficos/", escape_html(escola$grafico_operacional_nome),
      "'><div class='nota'>Índice relativo e descritivo; não mede integralmente ",
      "horas, deslocamentos, eventos emergenciais ou toda a carga real.</div>"
    )
  } else {
    paste0(
      "<h2>Situação operacional</h2><div class='alerta'>",
      escape_html(fmt_texto(escola$motivo_nao_inclusao)),
      " Esta escola permanece no universo institucional, sem índice e sem ",
      "carteira operacional.</div>"
    )
  }

  paste0(
    "<!DOCTYPE html><html lang='pt-BR'><head><meta charset='UTF-8'>",
    "<meta name='viewport' content='width=device-width,initial-scale=1'>",
    "<title>", escape_html(escola$nome_canonico), "</title>", css,
    "</head><body><div class='pagina'>",
    "<h1>", escape_html(escola$nome_canonico), "</h1>",
    "<span class='selo'>", escape_html(escola$tipo_ficha), "</span>",
    "<span class='selo'>ID: ", escape_html(escola$id_escola), "</span>",
    "<span class='selo'>INEP: ", escape_html(escola$codigo_inep), "</span>",
    "<h2>Identificação e vínculos</h2><div class='grid'>",
    card("Assessora gerencial 2026", fmt_texto(escola$assessora_gerencial_2026)),
    card("Vínculo administrativo", fmt_texto(escola$assessora_vinculo_administrativo)),
    card("Elegível 2026", sim_nao(escola$elegivel_assessoramento_2026)),
    card("Recebe assessoramento 2026", sim_nao(escola$recebe_assessoramento_2026)),
    card("Status operacional", fmt_texto(escola$status_carga_operacional_2026)),
    card("Grupo de exposição", fmt_texto(escola$grupo_exposicao_2026)),
    "</div>",
    bloco_indice,
    "<h2>Diagnóstico educacional contextual</h2><div class='grid'>",
    card("Previstos 2025", fmt_num(escola$previstos_total_2025)),
    card("Avaliados 2025", fmt_num(escola$avaliados_total_2025)),
    card("Participação 2025", fmt_pct(escola$participacao_2025)),
    card("Previstos 2026", fmt_num(escola$previstos_total_2026)),
    card("Avaliados 2026", fmt_num(escola$avaliados_total_2026)),
    card("Participação 2026", fmt_pct(escola$participacao_2026)),
    card("Proficiência 2025", fmt_num(escola$proficiencia_2025, 1)),
    card("Proficiência 2026", fmt_num(escola$proficiencia_2026, 1)),
    card("Séries com alerta", fmt_num(escola$series_alerta_composicao)),
    "</div>",
    "<img src='../graficos/", escape_html(escola$grafico_participacao_nome), "'>",
    "<img src='../graficos/", escape_html(escola$grafico_proficiencia_nome), "'>",
    "<div class='nota'>Resultados educacionais são observacionais, descritivos ",
    "e sujeitos a diferenças de participação, composição e cobertura. Não ",
    "representam efeito do assessoramento nem desempenho da assessora.</div>",
    "<h2>Resultados por ano escolar</h2>",
    tabela_series(dserie),
    "<div class='rodape'>Execução ", id_execucao,
    " — módulo 19 — estudo observacional e descritivo.</div>",
    "</div></body></html>"
  )
}

# -------------------------------------------------------------------
# 9. Geração dos candidatos
# -------------------------------------------------------------------

resultado <- vector("list", nrow(base))

for (i in seq_len(nrow(base))) {
  escola <- base[i, , drop = FALSE]
  dserie <- serie |>
    filter(id_escola == escola$id_escola[[1]]) |>
    arrange(ano_escolar)

  html_path <- file.path(pasta_html, escola$html_nome[[1]])
  part_path <- file.path(pasta_graficos, escola$grafico_participacao_nome[[1]])
  prof_path <- file.path(pasta_graficos, escola$grafico_proficiencia_nome[[1]])
  op_path <- if (isTRUE(escola$incluida_analise_operacional[[1]])) {
    file.path(pasta_graficos, escola$grafico_operacional_nome[[1]])
  } else {
    NA_character_
  }

  status <- "SUCESSO"
  erro <- NA_character_

  tryCatch(
    {
      if (!is.na(op_path)) grafico_operacional(escola, op_path)
      grafico_serie(
        dserie, "taxa_participacao_2025", "taxa_participacao_2026",
        "Participação nas avaliações", "Participação (%)", part_path
      )
      grafico_serie(
        dserie, "proficiencia_media_2025", "proficiencia_media_2026",
        "Proficiência observada", "Proficiência média", prof_path
      )
      writeLines(render_html(escola, dserie), html_path, useBytes = TRUE)
    },
    error = function(e) {
      status <<- "ERRO"
      erro <<- conditionMessage(e)
    }
  )

  resultado[[i]] <- tibble(
    id_escola = escola$id_escola[[1]],
    codigo_inep = escola$codigo_inep[[1]],
    nome_canonico = escola$nome_canonico[[1]],
    tipo_ficha = escola$tipo_ficha[[1]],
    assessora_gerencial_2026 = escola$assessora_gerencial_2026[[1]],
    status_geracao = status,
    mensagem_erro = erro,
    arquivo_html = html_path,
    html_existe = nao_vazio(html_path, 1000),
    arquivo_grafico_operacional = op_path,
    grafico_operacional_existe = if (is.na(op_path)) NA else nao_vazio(op_path, 1000),
    arquivo_grafico_participacao = part_path,
    grafico_participacao_existe = nao_vazio(part_path, 1000),
    arquivo_grafico_proficiencia = prof_path,
    grafico_proficiencia_existe = nao_vazio(prof_path, 1000)
  )
}

resultado <- bind_rows(resultado)

if (any(resultado$status_geracao != "SUCESSO")) {
  write_csv(
    resultado,
    file.path(pasta_execucao, "07_resultado_geracao_fichas.csv"),
    na = ""
  )
  stop("Falha na geração individual das fichas.")
}

# HTML compilado
secoes <- map_chr(seq_len(nrow(base)), function(i) {
  escola <- base[i, , drop = FALSE]
  dserie <- serie |> filter(id_escola == escola$id_escola[[1]]) |> arrange(ano_escolar)
  x <- render_html(escola, dserie)
  corpo <- str_match(x, "(?s)<body><div class='pagina'>(.*)</div></body>")[, 2]
  paste0("<section class='pagina quebra'>", corpo, "</section>")
})

html_compilado <- file.path(pasta_candidatos, "fichas_escolas_compiladas.html")
writeLines(
  paste0(
    "<!DOCTYPE html><html lang='pt-BR'><head><meta charset='UTF-8'>",
    css, "</head><body>", paste(secoes, collapse = "\n"), "</body></html>"
  ),
  html_compilado,
  useBytes = TRUE
)

# PDF opcional
pdf_compilado <- file.path(pasta_candidatos, "fichas_escolas_compiladas.pdf")
status_pdf <- "NAO_GERADO"
mensagem_pdf <- NA_character_

if (TENTAR_GERAR_PDF_COMPILADO && requireNamespace("pagedown", quietly = TRUE)) {
  tryCatch(
    {
      pagedown::chrome_print(
        input = html_compilado,
        output = pdf_compilado,
        wait = 2
      )
      if (nao_vazio(pdf_compilado, 10000)) status_pdf <- "GERADO"
    },
    error = function(e) {
      mensagem_pdf <<- conditionMessage(e)
    }
  )
} else {
  mensagem_pdf <- "pagedown indisponível ou geração não solicitada."
}

# -------------------------------------------------------------------
# 10. Bases canônicas candidatas
# -------------------------------------------------------------------

base_saida <- base |>
  select(
    id_escola, codigo_inep, nome_canonico, tipo_ficha,
    incluida_analise_operacional, motivo_nao_inclusao,
    nota_institucional, assessora_gerencial_2026,
    assessora_vinculo_administrativo, divergencia_vinculo_gerencial,
    elegivel_assessoramento_2026, recebe_assessoramento_2026,
    incluir_indice_carga_2026, status_carga_operacional_2026,
    grupo_exposicao_2026, all_of(contextuais),
    score_dimensao_volume, score_dimensao_estrutural,
    score_dimensao_administrativa, contribuicao_volume,
    contribuicao_estrutural, contribuicao_administrativa,
    indice_carga_potencial_operacional, cobertura_indice_operacional,
    interpretacao_cautelosa, resultados_educacionais_no_indice,
    previstos_total_2025, avaliados_total_2025, participacao_2025,
    proficiencia_2025, previstos_total_2026, avaliados_total_2026,
    participacao_2026, proficiencia_2026, series_alerta_composicao,
    html_nome, grafico_operacional_nome,
    grafico_participacao_nome, grafico_proficiencia_nome
  )

indice_saida <- resultado |>
  transmute(
    id_escola, codigo_inep, nome_canonico, tipo_ficha,
    assessora_gerencial_2026, status_geracao, mensagem_erro,
    arquivo_html = file.path(
      "resultados", "fichas_escolas",
      paste0("execucao_", id_execucao), "html", basename(arquivo_html)
    ),
    arquivo_grafico_operacional = if_else(
      tipo_ficha == "OPERACIONAL",
      file.path(
        "resultados", "fichas_escolas",
        paste0("execucao_", id_execucao), "graficos",
        basename(arquivo_grafico_operacional)
      ),
      NA_character_
    ),
    arquivo_grafico_participacao = file.path(
      "resultados", "fichas_escolas",
      paste0("execucao_", id_execucao), "graficos",
      basename(arquivo_grafico_participacao)
    ),
    arquivo_grafico_proficiencia = file.path(
      "resultados", "fichas_escolas",
      paste0("execucao_", id_execucao), "graficos",
      basename(arquivo_grafico_proficiencia)
    ),
    html_existe, grafico_operacional_existe,
    grafico_participacao_existe, grafico_proficiencia_existe
  )

dicionario <- imap_dfr(
  list(base_fichas_escolas = base_saida, indice_fichas_escolas = indice_saida),
  function(dados, produto) {
    tibble(
      produto = produto,
      ordem_coluna = seq_along(dados),
      variavel = names(dados),
      classe_r = map_chr(dados, ~ paste(class(.x), collapse = " | ")),
      observacao_metodologica = case_when(
        str_detect(
          names(dados),
          "proficiencia|participacao|defasagem|adequado|intermediario"
        ) ~ "Resultado educacional descritivo; não representa efeito ou desempenho da assessora.",
        str_detect(
          names(dados),
          "indice|score|contribuicao"
        ) ~ "Medida relativa operacional; não equivale à carga real total.",
        TRUE ~ "Usar com leitura contextual e qualitativa."
      )
    )
  }
)

candidatos_saida <- file.path(pasta_candidatos, basename(saidas))
names(candidatos_saida) <- names(saidas)

write_csv(base_saida, candidatos_saida[["base_csv"]], na = "")
saveRDS(base_saida, candidatos_saida[["base_rds"]])
write_csv(indice_saida, candidatos_saida[["indice_csv"]], na = "")
saveRDS(indice_saida, candidatos_saida[["indice_rds"]])
write_csv(dicionario, candidatos_saida[["dicionario_csv"]], na = "")

eq_base <- comparar_par(
  readRDS(candidatos_saida[["base_rds"]]),
  ler_csv_modelo(
    candidatos_saida[["base_csv"]],
    readRDS(candidatos_saida[["base_rds"]])
  ),
  "id_escola", "base_fichas_escolas"
)

eq_indice <- comparar_par(
  readRDS(candidatos_saida[["indice_rds"]]),
  ler_csv_modelo(
    candidatos_saida[["indice_csv"]],
    readRDS(candidatos_saida[["indice_rds"]])
  ),
  "id_escola", "indice_fichas_escolas"
)

equiv_candidatos <- bind_rows(
  eq_base$diagnostico,
  eq_indice$diagnostico
)

# -------------------------------------------------------------------
# 11. Validações finais
# -------------------------------------------------------------------

arquivos_candidatos <- list.files(
  pasta_candidatos, recursive = TRUE, full.names = TRUE
)
arquivos_candidatos <- arquivos_candidatos[
  !file.info(arquivos_candidatos)$isdir
]

manifesto_resultados <- map_dfr(
  arquivos_candidatos,
  ~ inventariar(
    str_remove(norm(.x, FALSE), paste0("^", fixed(norm(pasta_candidatos, FALSE)), "/?")),
    .x
  )
)

n_html <- sum(str_detect(manifesto_resultados$arquivo, "^html/.+\\.html$"))
n_op <- sum(str_detect(
  manifesto_resultados$arquivo,
  "^graficos/.+_dimensoes_operacionais\\.png$"
))
n_part <- sum(str_detect(
  manifesto_resultados$arquivo,
  "^graficos/.+_participacao\\.png$"
))
n_prof <- sum(str_detect(
  manifesto_resultados$arquivo,
  "^graficos/.+_proficiencia\\.png$"
))

validacoes <- bind_rows(
  validacao("Branch", "execucao", "erro", branch, branch_esperada, branch == branch_esperada),
  validacao("HEAD", "execucao", "erro", head, commit_base_integracao, head == commit_base_integracao),
  validacao("Entradas equivalentes", "integridade", "erro", sum(equiv_entradas$aprovado), "6", all(equiv_entradas$aprovado)),
  validacao("Perfil 56", "universo", "erro", nrow(perfil), "56", nrow(perfil) == 56L),
  validacao("Perfil série 280", "universo", "erro", nrow(serie), "280", nrow(serie) == 280L),
  validacao("Índice 53", "universo", "erro", nrow(indice), "53", nrow(indice) == 53L),
  validacao("Diagnóstico 265", "universo", "erro", nrow(diagnostico), "265", nrow(diagnostico) == 265L),
  validacao("Detalhe 53", "universo", "erro", nrow(detalhe), "53", nrow(detalhe) == 53L),
  validacao("Universo 56", "universo", "erro", nrow(universo), "56", nrow(universo) == 56L),
  validacao("Exclusões exatas", "universo", "erro", paste(ids_excluidos_observados, collapse = "; "), paste(ids_excluidos, collapse = "; "), setequal(ids_excluidos_observados, ids_excluidos)),
  validacao("Assessoras", "universo", "erro", numero_assessoras, "11", numero_assessoras == 11L),
  validacao("Base 56", "produto", "erro", nrow(base_saida), "56", nrow(base_saida) == 56L),
  validacao("Operacionais 53", "produto", "erro", sum(base_saida$tipo_ficha == "OPERACIONAL"), "53", sum(base_saida$tipo_ficha == "OPERACIONAL") == 53L),
  validacao("Não operacionais 3", "produto", "erro", sum(base_saida$tipo_ficha == "INSTITUCIONAL_NAO_OPERACIONAL"), "3", sum(base_saida$tipo_ficha == "INSTITUCIONAL_NAO_OPERACIONAL") == 3L),
  validacao("Não operacionais sem índice", "metodologia", "erro", sum(!base_saida$incluida_analise_operacional & !is.na(base_saida$indice_carga_potencial_operacional)), "0", !any(!base_saida$incluida_analise_operacional & !is.na(base_saida$indice_carga_potencial_operacional))),
  validacao("Educacional fora do índice", "metodologia", "erro", sum(coalesce(indice$resultados_educacionais_no_indice, FALSE)), "0", !any(coalesce(indice$resultados_educacionais_no_indice, FALSE))),
  validacao("Peso educacional zero", "metodologia", "erro", max(diagnostico$peso_no_indice_carga_operacional), "0", all(diagnostico$peso_no_indice_carga_operacional == 0)),
  validacao("HTMLs", "produto", "erro", n_html, "56", n_html == 56L),
  validacao("Gráficos operacionais", "produto", "erro", n_op, "53", n_op == 53L),
  validacao("Gráficos participação", "produto", "erro", n_part, "56", n_part == 56L),
  validacao("Gráficos proficiência", "produto", "erro", n_prof, "56", n_prof == 56L),
  validacao("HTML compilado", "produto", "erro", nao_vazio(html_compilado, 10000), "TRUE", nao_vazio(html_compilado, 10000)),
  validacao("Candidatos equivalentes", "integridade", "erro", sum(equiv_candidatos$aprovado), "2", all(equiv_candidatos$aprovado))
)

erros <- validacoes |> filter(!resultado & severidade == "erro")

# -------------------------------------------------------------------
# 12. Documentação
# -------------------------------------------------------------------

manifesto_entradas <- imap_dfr(
  entradas,
  ~ inventariar(.y, .x)
) |>
  mutate(
    md5_esperado = hashes_esperados[arquivo],
    hash_aprovado = str_to_lower(md5) == str_to_lower(md5_esperado)
  )

write_csv(
  tribble(
    ~parametro, ~valor,
    "universo_institucional", "56",
    "universo_operacional", "53",
    "fichas_nao_operacionais", "3",
    "assessoras", "11",
    "peso_volume", "0,40",
    "peso_estrutura", "0,35",
    "peso_administracao", "0,25",
    "peso_educacional", "0",
    "pdf_compilado", status_pdf
  ),
  file.path(pasta_execucao, "01_parametros_execucao.csv"),
  na = ""
)

write_csv(manifesto_entradas, file.path(pasta_execucao, "02_manifesto_arquivos_entrada.csv"), na = "")
write_csv(equiv_entradas, file.path(pasta_execucao, "03_equivalencia_csv_rds_entradas.csv"), na = "")
write_csv(universo, file.path(pasta_execucao, "04_universo_institucional_56_escolas.csv"), na = "")
write_csv(base_saida, file.path(pasta_execucao, "05_base_fichas_candidata.csv"), na = "")
write_csv(indice_saida, file.path(pasta_execucao, "06_indice_fichas_candidato.csv"), na = "")
write_csv(resultado, file.path(pasta_execucao, "07_resultado_geracao_fichas.csv"), na = "")
write_csv(manifesto_resultados, file.path(pasta_execucao, "08_manifesto_resultados_candidatos.csv"), na = "")
write_csv(validacoes, file.path(pasta_execucao, "09_validacao_final.csv"), na = "")
write_csv(equiv_candidatos, file.path(pasta_execucao, "10_equivalencia_csv_rds_candidatos.csv"), na = "")
write_csv(dicionario, file.path(pasta_execucao, "11_dicionario_candidato.csv"), na = "")
write_lines(capture.output(sessionInfo()), file.path(pasta_execucao, "12_session_info.txt"))

write_csv(
  tibble(
    campo = c(
      "id_execucao", "instante", "branch", "commit_base",
      "caminho_script", "md5_script", "status_pdf", "mensagem_pdf"
    ),
    valor = c(
      id_execucao,
      format(instante_execucao, "%Y-%m-%d %H:%M:%S %z"),
      branch, head, norm(caminho_script), md5(caminho_script),
      status_pdf, coalesce(mensagem_pdf, "")
    )
  ),
  file.path(pasta_execucao, "13_identificacao_execucao.csv"),
  na = ""
)

write_lines(
  c(
    paste0("Execução: ", id_execucao),
    paste0("Branch: ", branch),
    paste0("Commit-base: ", head),
    paste0("MD5 do script: ", md5(caminho_script)),
    "Fichas institucionais: 56",
    "Fichas operacionais: 53",
    "Fichas não operacionais: 3",
    paste0("PDF compilado: ", status_pdf),
    paste0("Erros críticos: ", nrow(erros)),
    "",
    "Resultados educacionais permanecem fora do índice.",
    "Não foram criados ranking, faixa, percentil ou cenário.",
    "Vínculo administrativo e assessora gerencial permanecem separados."
  ),
  file.path(pasta_execucao, "14_resumo_execucao.txt")
)

if (nrow(erros) > 0L) {
  stop(
    "O módulo 19 encontrou ", nrow(erros),
    " erro(s) crítico(s). Consulte 09_validacao_final.csv."
  )
}

# -------------------------------------------------------------------
# 13. Preservação e promoção
# -------------------------------------------------------------------

anteriores <- saidas[file.exists(saidas)]
if (length(anteriores) > 0L) {
  walk2(
    anteriores, names(anteriores),
    ~ copiar_validado(
      .x,
      file.path(pasta_historico_dados, basename(.x)),
      FALSE
    )
  )
}

manifesto_historico <- if (length(anteriores) > 0L) {
  imap_dfr(
    anteriores,
    ~ inventariar(.y, file.path(pasta_historico_dados, basename(.x)))
  )
} else {
  tibble(
    arquivo = character(), caminho = character(),
    existe = logical(), tamanho_bytes = double(), md5 = character()
  )
}

write_csv(
  manifesto_historico,
  file.path(pasta_execucao, "15_manifesto_preservacao_historica.csv"),
  na = ""
)

promovidos <- character()

tryCatch(
  {
    for (nm in names(saidas)) {
      destino <- saidas[[nm]]
      candidato <- candidatos_saida[[nm]]
      if (file.exists(destino)) {
        copiar_validado(
          destino,
          file.path(pasta_rollback, basename(destino)),
          FALSE
        )
      }
      dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
      ok <- file.copy(candidato, destino, overwrite = TRUE)
      if (!ok || !identical(md5(candidato), md5(destino))) {
        stop("Falha na promoção de ", nm)
      }
      promovidos <- c(promovidos, nm)
    }
  },
  error = function(e) {
    for (nm in rev(promovidos)) {
      destino <- saidas[[nm]]
      rollback <- file.path(pasta_rollback, basename(destino))
      if (file.exists(rollback)) {
        file.copy(rollback, destino, overwrite = TRUE)
      } else if (file.exists(destino)) {
        file.remove(destino)
      }
    }
    stop("Promoção dos dados falhou; rollback executado: ", conditionMessage(e))
  }
)

if (dir.exists(pasta_resultados_final)) {
  stop(
    "Pasta final já existe: ",
    pasta_resultados_final
  )
}

dir.create(
  pasta_resultados_final,
  recursive = TRUE,
  showWarnings = FALSE
)

itens_resultados_candidatos <- list.files(
  pasta_candidatos,
  all.files = TRUE,
  full.names = TRUE,
  no.. = TRUE
)

if (length(itens_resultados_candidatos) == 0L) {
  unlink(
    pasta_resultados_final,
    recursive = TRUE,
    force = TRUE
  )
  
  stop(
    "A pasta candidata de resultados está vazia."
  )
}

resultado_copia <- file.copy(
  from = itens_resultados_candidatos,
  to = pasta_resultados_final,
  recursive = TRUE,
  overwrite = FALSE,
  copy.mode = TRUE,
  copy.date = TRUE
)

if (
  length(resultado_copia) !=
  length(itens_resultados_candidatos) ||
  !all(resultado_copia)
) {
  unlink(
    pasta_resultados_final,
    recursive = TRUE,
    force = TRUE
  )
  
  stop(
    "Falha na promoção da pasta de resultados; ",
    "o destino incompleto foi removido."
  )
}

arquivos_promovidos <- list.files(
  pasta_resultados_final, recursive = TRUE, full.names = TRUE
)
arquivos_promovidos <- arquivos_promovidos[
  !file.info(arquivos_promovidos)$isdir
]

manifesto_promovidos <- map_dfr(
  arquivos_promovidos,
  ~ inventariar(
    str_remove(
      norm(.x, FALSE),
      paste0("^", fixed(norm(pasta_resultados_final, FALSE)), "/?")
    ),
    .x
  )
)

manifesto_produtos <- imap_dfr(
  saidas,
  ~ inventariar(.y, .x)
)

write_csv(
  manifesto_produtos,
  file.path(pasta_execucao, "16_manifesto_produtos_modulo_19.csv"),
  na = ""
)

write_csv(
  manifesto_promovidos,
  file.path(pasta_execucao, "17_manifesto_resultados_promovidos.csv"),
  na = ""
)

write_csv(
  manifesto_resultados |>
    select(arquivo, md5_candidato = md5) |>
    left_join(
      manifesto_promovidos |>
        select(arquivo, md5_promovido = md5),
      by = "arquivo"
    ) |>
    mutate(
      hash_igual = str_to_lower(md5_candidato) ==
        str_to_lower(md5_promovido)
    ),
  file.path(pasta_execucao, "18_verificacao_promocao_resultados.csv"),
  na = ""
)

write_csv(
  imap_dfr(
    candidatos_saida,
    ~ inventariar(.y, .x)
  ) |>
    select(arquivo, md5_candidato = md5) |>
    left_join(
      manifesto_produtos |>
        select(arquivo, md5_promovido = md5),
      by = "arquivo"
    ) |>
    mutate(
      hash_igual = str_to_lower(md5_candidato) ==
        str_to_lower(md5_promovido)
    ),
  file.path(pasta_execucao, "19_verificacao_promocao_produtos.csv"),
  na = ""
)

message(
  "Módulo 19 concluído com sucesso.\n",
  "Execução: ", id_execucao, "\n",
  "Fichas institucionais: 56\n",
  "Fichas operacionais: 53\n",
  "Fichas não operacionais: 3\n",
  "PDF compilado: ", status_pdf, "\n",
  "Resultados: ", pasta_resultados_final, "\n",
  "Diagnósticos: ", pasta_execucao, "\n",
  "MD5 do script: ", md5(caminho_script)
)
