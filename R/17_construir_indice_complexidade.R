# ===================================================================
# 17_construir_indice_complexidade.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# ===================================================================
#
# OBJETIVO
#
# Construir um índice descritivo de carga potencial operacional de
# assessoramento para o universo homologado de 2026, mantendo separadas:
#
#   1. dimensão de volume (40%);
#   2. dimensão estrutural (35%);
#   3. dimensão administrativa (25%).
#
# Resultados educacionais NÃO compõem o índice. Proficiência, defasagem,
# participação, composição e cobertura são preservadas em produto
# diagnóstico paralelo, destinado a subsidiar uma camada gerencial
# posterior sobre demanda pedagógica adicional.
#
# O índice não é ranking de qualidade, avaliação das assessoras, estimativa
# causal, meta oficial ou regra automática de redistribuição de carteiras.
#
# EXECUÇÃO CONTROLADA
#
# - caminho canônico obrigatório;
# - branch e commit-base fixos;
# - quatro entradas obrigatórias (dois pares CSV/RDS);
# - hashes fixos e manifesto homologado do módulo 16;
# - equivalência semântica CSV/RDS;
# - universo inicial de 56 escolas/280 linhas;
# - universo de cálculo de 53 escolas/265 linhas/11 assessoras;
# - preservação histórica;
# - escrita, releitura, validação, promoção e rollback transacionais.
# ===================================================================

library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Contrato da execução
# -------------------------------------------------------------------

instante_execucao <- Sys.time()
id_execucao <- format(instante_execucao, "%Y%m%d_%H%M%S")

commit_base_integracao <- paste0(
  "91b2c51fb296f1c00c494cb46288dd210",
  "ced62d0"
)
branch_esperada <- "refatoracao_modulo_21"

caminho_script <- here("R", "17_construir_indice_complexidade.R")
caminho_relativo_script <- file.path(
  "R", "17_construir_indice_complexidade.R"
)

caminho_manifesto_modulo_16 <- here(
  "documentacao", "perfil_escola", "execucao_20260726_223447",
  "19_manifesto_produtos_modulo_16.csv"
)

arquivos_entrada <- c(
  perfil_gerencial_csv = here(
    "dados_finais", "perfil_escola_gerencial.csv"
  ),
  perfil_gerencial_rds = here(
    "dados_finais", "perfil_escola_gerencial.rds"
  ),
  perfil_serie_csv = here(
    "dados_finais", "perfil_escola_serie_compacto.csv"
  ),
  perfil_serie_rds = here(
    "dados_finais", "perfil_escola_serie_compacto.rds"
  )
)

hashes_entrada_homologados <- c(
  perfil_gerencial_csv = "35e75d94b2f8b6e84a5a51708afb76e6",
  perfil_gerencial_rds = "ba49b4fe72596f4b0c16b7dc7f830646",
  perfil_serie_csv = "9ddb99063daea52ac4efa712fd9fad8b",
  perfil_serie_rds = "116a2d0c97085a90aac716d911f75c89"
)

arquivos_saida <- c(
  indice_csv = here(
    "dados_finais", "indice_carga_potencial_escola.csv"
  ),
  indice_rds = here(
    "dados_finais", "indice_carga_potencial_escola.rds"
  ),
  componentes_csv = here(
    "dados_finais", "componentes_indice_carga_potencial.csv"
  ),
  componentes_rds = here(
    "dados_finais", "componentes_indice_carga_potencial.rds"
  ),
  diagnostico_educacional_csv = here(
    "dados_finais", "componentes_educacionais_escola_serie.csv"
  ),
  diagnostico_educacional_rds = here(
    "dados_finais", "componentes_educacionais_escola_serie.rds"
  ),
  dicionario_csv = here(
    "documentacao", "indice_complexidade",
    "dicionario_indice_carga_potencial.csv"
  )
)

ids_excluidos_carga <- c("ESC_001", "ESC_055", "ESC_056")
pares_excluidos_carga <- tribble(
  ~id_escola, ~codigo_inep,
  "ESC_001", "43105416",
  "ESC_055", "43105300",
  "ESC_056", "43189768"
)

pasta_documentacao <- here("documentacao", "indice_complexidade")
pasta_execucao <- file.path(
  pasta_documentacao, paste0("execucao_", id_execucao)
)
pasta_historico <- here(
  "dados_finais", "historico", "indice_complexidade",
  "pre_modulo_16_modulo_17"
)
pasta_manifesto_historico <- file.path(
  pasta_documentacao, "historico_pre_modulo_16"
)
caminho_manifesto_historico <- file.path(
  pasta_manifesto_historico,
  "manifesto_produtos_pre_modulo_16_modulo_17.csv"
)
caminho_script_historico <- here(
  "R", "historico_pre_modulo_16", "modulo_17",
  "17_construir_indice_complexidade_pre_modulo_16.R.txt"
)
pasta_transacao <- here(
  "dados_finais", "historico", "indice_complexidade", "transacoes",
  paste0("execucao_", id_execucao)
)
pasta_candidatos <- file.path(pasta_transacao, "candidatos")
pasta_rollback <- file.path(pasta_transacao, "rollback")

# -------------------------------------------------------------------
# 2. Funções auxiliares
# -------------------------------------------------------------------

hash_md5 <- function(caminho) {
  if (
    length(caminho) != 1L ||
      is.na(caminho) ||
      !file.exists(caminho)
  ) {
    return(NA_character_)
  }
  unname(tools::md5sum(caminho))
}

normalizar_caminho <- function(caminho, deve_existir = TRUE) {
  normalizePath(
    caminho, winslash = "/", mustWork = deve_existir
  )
}

executar_git <- function(argumentos) {
  saida <- tryCatch(
    suppressWarnings(system2(
      "git", argumentos, stdout = TRUE, stderr = FALSE
    )),
    error = function(e) character()
  )
  status <- attr(saida, "status")
  if (!is.null(status) && status != 0) {
    return(character())
  }
  str_squish(as.character(saida))
}

extrair_arquivo_git <- function(revisao, caminho_relativo, destino) {
  status <- tryCatch(
    suppressWarnings(system2(
      "git",
      c(
        "-C", shQuote(here()), "show",
        paste0(revisao, ":", caminho_relativo)
      ),
      stdout = destino,
      stderr = FALSE
    )),
    error = function(e) 1L
  )
  is.numeric(status) &&
    length(status) == 1L &&
    status == 0L &&
    file.exists(destino) &&
    file.info(destino)$size > 0
}

obter_commit_git <- function() {
  saida <- executar_git(c(
    "-C", shQuote(here()), "rev-parse", "HEAD"
  ))
  saida <- saida[str_detect(saida, "^[0-9a-fA-F]{40}$")]
  if (length(saida) != 1L) return(NA_character_)
  str_to_lower(saida[[1]])
}

obter_branch_git <- function() {
  saida <- executar_git(c(
    "-C", shQuote(here()), "branch", "--show-current"
  ))
  if (length(saida) != 1L) return(NA_character_)
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
    TRUE ~ paste(class(x), collapse = " | ")
  )
}

converter_logico_seguro <- function(x, variavel) {
  texto <- str_to_lower(str_squish(as.character(x)))
  resultado <- case_when(
    is.na(texto) | texto == "" ~ NA,
    texto %in% c("true", "t", "1", "sim", "s") ~ TRUE,
    texto %in% c("false", "f", "0", "nao", "não", "n") ~ FALSE,
    TRUE ~ NA
  )
  invalidos <- !is.na(texto) & texto != "" & is.na(resultado)
  if (any(invalidos)) {
    stop("Valores lógicos inválidos em `", variavel, "`.")
  }
  resultado
}

converter_por_modelo <- function(x, modelo, variavel) {
  tipo <- tipo_canonico(modelo)
  resultado <- switch(
    tipo,
    character = as.character(x),
    logical = converter_logico_seguro(x, variavel),
    integer = suppressWarnings(as.integer(x)),
    double = suppressWarnings(as.double(x)),
    date = as.Date(x),
    datetime = as.POSIXct(x, tz = "UTC"),
    stop("Tipo não suportado em `", variavel, "`: ", tipo)
  )
  if (tipo %in% c("integer", "double")) {
    preenchido <- !is.na(x) & str_squish(as.character(x)) != ""
    if (any(preenchido & is.na(resultado))) {
      stop("Falha de conversão numérica em `", variavel, "`.")
    }
  }
  resultado
}

ler_csv_por_modelo <- function(caminho, modelo) {
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
    stop("Nomes ou ordem de colunas divergentes em: ", caminho)
  }
  saida <- bruto
  for (variavel in names(modelo)) {
    saida[[variavel]] <- converter_por_modelo(
      bruto[[variavel]], modelo[[variavel]], variavel
    )
  }
  as_tibble(saida)
}

comparar_bases_semanticamente <- function(
    rds, csv, chaves, fonte
) {
  ordenar <- function(x) {
    as_tibble(x) |>
      arrange(across(all_of(chaves))) |>
      select(all_of(names(x)))
  }
  a <- ordenar(rds)
  b <- ordenar(csv)
  mesmos_nomes <- identical(names(a), names(b))
  mesmos_tipos <- mesmos_nomes && identical(
    map_chr(a, tipo_canonico),
    map_chr(b, tipo_canonico)
  )
  mesmas_dimensoes <- identical(dim(a), dim(b))
  comparacao <- if (
    mesmos_nomes && mesmos_tipos && mesmas_dimensoes
  ) {
    all.equal(
      a, b, check.attributes = FALSE, tolerance = 1e-12
    )
  } else {
    "estrutura divergente"
  }
  mesmos_valores <- isTRUE(comparacao)
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
        paste(comparacao, collapse = " | ")
      },
      aprovado = mesmos_nomes && mesmos_tipos &&
        mesmas_dimensoes && mesmos_valores
    )
  )
}

as_logical_seguro <- function(x, variavel) {
  if (is.logical(x)) return(x)
  converter_logico_seguro(x, variavel)
}

percentil_relativo <- function(x, inverso = FALSE) {
  x <- suppressWarnings(as.numeric(x))
  valido <- !is.na(x)
  n <- sum(valido)
  resultado <- rep(NA_real_, length(x))
  if (n == 0L) return(resultado)
  if (n == 1L || length(unique(x[valido])) == 1L) {
    resultado[valido] <- 50
    return(resultado)
  }
  resultado[valido] <- 100 * (
    rank(x[valido], ties.method = "average") - 1
  ) / (n - 1)
  if (inverso) resultado[valido] <- 100 - resultado[valido]
  resultado
}

calcular_score_ponderado <- function(
    dados, variaveis, pesos, cobertura_minima
) {
  matriz <- as.matrix(dados[variaveis])
  armazenamento <- matrix(
    rep(pesos, each = nrow(matriz)),
    nrow = nrow(matriz)
  )
  disponibilidade <- !is.na(matriz)
  cobertura <- rowSums(disponibilidade * armazenamento)
  numerador <- rowSums(
    replace(matriz, is.na(matriz), 0) * armazenamento
  )
  score <- ifelse(
    cobertura >= cobertura_minima,
    numerador / cobertura,
    NA_real_
  )
  list(score = score, cobertura = cobertura)
}

registrar_validacao <- function(
    teste, categoria, severidade, valor_observado,
    criterio, aprovado, detalhe
) {
  tibble(
    teste = teste,
    categoria = categoria,
    severidade = severidade,
    valor_observado = as.character(valor_observado),
    criterio = criterio,
    status = if_else(aprovado, "aprovado", "reprovado"),
    detalhe = detalhe
  )
}

estrutura_base <- function(dados, fonte) {
  map_dfr(names(dados), function(variavel) {
    tibble(
      fonte = fonte,
      ordem_coluna = match(variavel, names(dados)),
      variavel = variavel,
      classe_r = paste(class(dados[[variavel]]), collapse = " | "),
      numero_na = sum(is.na(dados[[variavel]])),
      numero_distintos = n_distinct(
        dados[[variavel]], na.rm = TRUE
      )
    )
  })
}

inventariar_arquivo <- function(produto, caminho) {
  existe <- file.exists(caminho)
  info <- if (existe) file.info(caminho) else NULL
  tibble(
    produto = produto,
    caminho = normalizar_caminho(caminho, FALSE),
    existe = existe,
    tamanho_bytes = if (existe) as.numeric(info$size) else NA_real_,
    data_modificacao = if (existe) {
      format(info$mtime, "%Y-%m-%d %H:%M:%S")
    } else {
      NA_character_
    },
    md5 = hash_md5(caminho)
  )
}

gravar_produto <- function(produto, objeto, caminho) {
  if (str_ends(caminho, fixed(".csv"))) {
    write_csv(objeto, caminho, na = "")
  } else if (str_ends(caminho, fixed(".rds"))) {
    saveRDS(objeto, caminho)
  } else {
    stop("Extensão de produto não suportada: ", caminho)
  }
}

reler_produto <- function(caminho, modelo = NULL) {
  if (str_ends(caminho, fixed(".rds"))) {
    return(readRDS(caminho) |> as_tibble())
  }
  if (is.null(modelo)) {
    return(read_csv(
      caminho, show_col_types = FALSE, progress = FALSE,
      na = c("", "NA")
    ))
  }
  ler_csv_por_modelo(caminho, modelo)
}

# -------------------------------------------------------------------
# 3. Bloqueios Git e criação tardia dos diretórios
# -------------------------------------------------------------------

if (!file.exists(caminho_script)) {
  stop("Execute o módulo pelo caminho canônico: ", caminho_script)
}

commit_git_execucao <- obter_commit_git()
branch_execucao <- obter_branch_git()

if (
  is.na(commit_git_execucao) ||
    commit_git_execucao != commit_base_integracao
) {
  stop(
    "HEAD incompatível.\nEsperado: ", commit_base_integracao,
    "\nObservado: ", commit_git_execucao
  )
}
if (
  is.na(branch_execucao) ||
    branch_execucao != branch_esperada
) {
  stop(
    "Branch incompatível.\nEsperada: ", branch_esperada,
    "\nObservada: ", branch_execucao
  )
}

arquivos_exigidos <- c(
  arquivos_entrada,
  manifesto_modulo_16 = caminho_manifesto_modulo_16
)
ausentes <- arquivos_exigidos[!file.exists(arquivos_exigidos)]
if (length(ausentes) > 0L) {
  stop(
    "Entradas obrigatórias ausentes:\n",
    paste(ausentes, collapse = "\n"),
    "\nNão existe fallback entre CSV e RDS."
  )
}

walk(c(
  pasta_documentacao,
  pasta_execucao,
  dirname(caminho_script_historico),
  pasta_historico,
  pasta_manifesto_historico,
  pasta_candidatos,
  pasta_rollback
), ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))

manifesto_execucao <- tibble(
  id_execucao = id_execucao,
  instante_execucao = format(
    instante_execucao, "%Y-%m-%d %H:%M:%S %z"
  ),
  caminho_script = normalizar_caminho(caminho_script),
  script_md5 = hash_md5(caminho_script),
  commit_base_integracao = commit_base_integracao,
  commit_git_execucao = commit_git_execucao,
  branch_execucao = branch_execucao,
  manifesto_modulo_16 = normalizar_caminho(
    caminho_manifesto_modulo_16
  ),
  decisao_metodologica = paste(
    "Índice operacional sem resultados educacionais;",
    "diagnóstico educacional paralelo com peso zero."
  )
)
write_csv(
  manifesto_execucao,
  file.path(pasta_execucao, "00_manifesto_execucao.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 4. Integridade e equivalência das entradas
# -------------------------------------------------------------------

hashes_observados <- map_chr(arquivos_entrada, hash_md5)
manifesto_16 <- read_csv(
  caminho_manifesto_modulo_16,
  show_col_types = FALSE,
  progress = FALSE
) |>
  select(produto, md5_manifesto = md5)

nomes_manifesto <- c(
  perfil_gerencial_csv = "perfil_gerencial_csv",
  perfil_gerencial_rds = "perfil_gerencial_rds",
  perfil_serie_csv = "perfil_serie_csv",
  perfil_serie_rds = "perfil_serie_rds"
)

validacao_hashes <- tibble(
  entrada = names(arquivos_entrada),
  produto_manifesto = unname(nomes_manifesto[names(arquivos_entrada)]),
  caminho = unname(arquivos_entrada),
  md5_esperado = unname(
    hashes_entrada_homologados[names(arquivos_entrada)]
  ),
  md5_observado = unname(hashes_observados)
) |>
  left_join(
    manifesto_16,
    by = c("produto_manifesto" = "produto")
  ) |>
  mutate(
    aprovado = !is.na(md5_manifesto) &
      md5_esperado == md5_observado &
      md5_manifesto == md5_observado
  )

write_csv(
  validacao_hashes,
  file.path(pasta_execucao, "01_validacao_hashes_entradas.csv"),
  na = ""
)
if (any(!validacao_hashes$aprovado)) {
  stop("Hashes das entradas divergem do módulo 16 homologado.")
}

perfil_gerencial_rds <- readRDS(
  arquivos_entrada[["perfil_gerencial_rds"]]
) |> as_tibble()
perfil_serie_rds <- readRDS(
  arquivos_entrada[["perfil_serie_rds"]]
) |> as_tibble()

perfil_gerencial_csv <- ler_csv_por_modelo(
  arquivos_entrada[["perfil_gerencial_csv"]],
  perfil_gerencial_rds
)
perfil_serie_csv <- ler_csv_por_modelo(
  arquivos_entrada[["perfil_serie_csv"]],
  perfil_serie_rds
)

comparacao_gerencial <- comparar_bases_semanticamente(
  perfil_gerencial_rds, perfil_gerencial_csv,
  "id_escola", "perfil_escola_gerencial"
)
comparacao_serie <- comparar_bases_semanticamente(
  perfil_serie_rds, perfil_serie_csv,
  c("id_escola", "ano_escolar", "componente"),
  "perfil_escola_serie_compacto"
)
diagnostico_equivalencia <- bind_rows(
  comparacao_gerencial$diagnostico,
  comparacao_serie$diagnostico
)
write_csv(
  diagnostico_equivalencia,
  file.path(pasta_execucao, "02_equivalencia_csv_rds.csv"),
  na = ""
)
if (any(!diagnostico_equivalencia$aprovado)) {
  stop("CSV e RDS do módulo 16 não são equivalentes.")
}

perfil_gerencial <- comparacao_gerencial$dados
perfil_serie <- comparacao_serie$dados

# -------------------------------------------------------------------
# 5. Contratos estruturais e universo oficial
# -------------------------------------------------------------------

colunas_gerencial <- c(
  "id_escola", "codigo_inep", "nome_canonico",
  "assessora_vinculo_administrativo",
  "assessora_gerencial_2025", "assessora_gerencial_2026",
  "incluir_indice_carga_2025", "incluir_indice_carga_2026",
  "matriculas_anos_iniciais", "turmas_anos_iniciais",
  "numero_etapas_amplas_ofertadas",
  "indice_infraestrutura_basica",
  "alunos_por_turma_anos_iniciais",
  "alunos_por_docente_anos_iniciais",
  "pct_matriculas_educacao_especial",
  "pct_matriculas_anos_iniciais_integral",
  "pct_matriculas_transporte_publico",
  "grupo_administrativo_2024_final",
  "municipalizada_apos_2024",
  "possivel_municipalizacao_recente",
  "escola_nova_recente",
  "requer_revisao_tecnica",
  "divergencia_censo_cadastro"
)
colunas_serie <- c(
  "id_escola", "codigo_inep", "nome_canonico",
  "assessora_gerencial_2025", "assessora_gerencial_2026",
  "incluir_indice_carga_2025", "incluir_indice_carga_2026",
  "ano_escolar", "componente",
  "painel_resultado_balanceado",
  "previstos_2026", "avaliados_2026",
  "taxa_participacao_2026", "proficiencia_media_2026",
  "pct_defasagem_2026", "pct_intermediario_2026",
  "pct_adequado_2026",
  "delta_participacao", "delta_proficiencia",
  "variacao_relativa_previstos",
  "aumento_participacao_10pp",
  "queda_participacao_10pp",
  "mudanca_previstos_20pct",
  "alerta_composicao_serie"
)
faltantes <- c(
  setdiff(colunas_gerencial, names(perfil_gerencial)),
  setdiff(colunas_serie, names(perfil_serie))
)
if (length(faltantes) > 0L) {
  stop(
    "Colunas obrigatórias ausentes: ",
    paste(unique(faltantes), collapse = " | ")
  )
}

variaveis_proibidas <- c("assessora", "assessora_gerencial")
presentes_proibidas <- intersect(
  variaveis_proibidas,
  union(names(perfil_gerencial), names(perfil_serie))
)
if (length(presentes_proibidas) > 0L) {
  stop(
    "Variáveis sem período proibidas nas entradas: ",
    paste(presentes_proibidas, collapse = " | ")
  )
}

perfil_gerencial <- perfil_gerencial |>
  mutate(
    codigo_inep = as.character(codigo_inep),
    incluir_indice_carga_2025 = as_logical_seguro(
      incluir_indice_carga_2025, "incluir_indice_carga_2025"
    ),
    incluir_indice_carga_2026 = as_logical_seguro(
      incluir_indice_carga_2026, "incluir_indice_carga_2026"
    )
  )
perfil_serie <- perfil_serie |>
  mutate(
    codigo_inep = as.character(codigo_inep),
    incluir_indice_carga_2025 = as_logical_seguro(
      incluir_indice_carga_2025, "incluir_indice_carga_2025"
    ),
    incluir_indice_carga_2026 = as_logical_seguro(
      incluir_indice_carga_2026, "incluir_indice_carga_2026"
    ),
    ano_escolar = as.integer(ano_escolar)
  )

duplicadas_gerencial <- perfil_gerencial |>
  count(id_escola) |>
  filter(n != 1L)
duplicadas_serie <- perfil_serie |>
  count(id_escola, ano_escolar, componente) |>
  filter(n != 1L)

exclusoes_observadas <- perfil_gerencial |>
  filter(!coalesce(incluir_indice_carga_2026, FALSE)) |>
  select(id_escola, codigo_inep) |>
  arrange(id_escola)

universo_gerencial <- perfil_gerencial |>
  filter(coalesce(incluir_indice_carga_2026, FALSE))
universo_serie <- perfil_serie |>
  filter(coalesce(incluir_indice_carga_2026, FALSE))

constancia_serie <- universo_serie |>
  group_by(id_escola) |>
  summarise(
    n_codigo = n_distinct(codigo_inep),
    n_nome = n_distinct(nome_canonico),
    n_assessora = n_distinct(assessora_gerencial_2026),
    n_incluir = n_distinct(incluir_indice_carga_2026),
    numero_series = n_distinct(ano_escolar),
    numero_linhas = n(),
    .groups = "drop"
  ) |>
  mutate(
    aprovado = n_codigo == 1L &
      n_nome == 1L &
      n_assessora == 1L &
      n_incluir == 1L &
      numero_series == 5L &
      numero_linhas == 5L
  )

assessoras_invalidas <- universo_gerencial |>
  filter(
    is.na(assessora_gerencial_2026) |
      str_squish(assessora_gerencial_2026) == ""
  )

validacoes_universo <- bind_rows(
  registrar_validacao(
    "Perfil gerencial inicial", "universo", "erro",
    nrow(perfil_gerencial), "56 escolas",
    nrow(perfil_gerencial) == 56L,
    "Uma linha por escola antes do filtro."
  ),
  registrar_validacao(
    "Perfil série inicial", "universo", "erro",
    nrow(perfil_serie), "280 linhas",
    nrow(perfil_serie) == 280L,
    "56 escolas × cinco séries."
  ),
  registrar_validacao(
    "Unicidade do perfil gerencial", "chaves", "erro",
    nrow(duplicadas_gerencial), "zero",
    nrow(duplicadas_gerencial) == 0L,
    "id_escola deve ser único."
  ),
  registrar_validacao(
    "Unicidade escola × série × componente", "chaves", "erro",
    nrow(duplicadas_serie), "zero",
    nrow(duplicadas_serie) == 0L,
    "A chave compacta deve ser única."
  ),
  registrar_validacao(
    "Exclusões por ID e código INEP", "universo", "erro",
    nrow(exclusoes_observadas), "três pares exatos",
    identical(exclusoes_observadas, pares_excluidos_carga),
    "ESC_001, ESC_055 e ESC_056."
  ),
  registrar_validacao(
    "Universo gerencial de carga", "universo", "erro",
    nrow(universo_gerencial), "53 escolas",
    nrow(universo_gerencial) == 53L,
    "Somente incluir_indice_carga_2026 == TRUE."
  ),
  registrar_validacao(
    "Universo escola × série de carga", "universo", "erro",
    nrow(universo_serie), "265 linhas",
    nrow(universo_serie) == 265L,
    "53 escolas × cinco séries."
  ),
  registrar_validacao(
    "Número de assessoras", "universo", "erro",
    n_distinct(universo_gerencial$assessora_gerencial_2026),
    "11 assessoras",
    n_distinct(
      universo_gerencial$assessora_gerencial_2026
    ) == 11L,
    "Contagem no universo oficial."
  ),
  registrar_validacao(
    "Assessora de 2026 preenchida", "cadastro", "erro",
    nrow(assessoras_invalidas), "zero",
    nrow(assessoras_invalidas) == 0L,
    "Ausência de assessora é erro, não componente."
  ),
  registrar_validacao(
    "Constância e cinco séries", "estrutura", "erro",
    sum(!constancia_serie$aprovado), "zero escolas",
    all(constancia_serie$aprovado),
    "Campos escolares constantes e cinco séries por escola."
  ),
  registrar_validacao(
    "Carga de 2025 vazia", "temporal", "erro",
    sum(coalesce(perfil_gerencial$incluir_indice_carga_2025, FALSE)),
    "zero escolas",
    !any(coalesce(
      perfil_gerencial$incluir_indice_carga_2025, FALSE
    )),
    "2025 é linha de base sem carga do programa."
  ),
  registrar_validacao(
    "Assessora gerencial de 2025 vazia", "temporal", "erro",
    sum(
      !is.na(perfil_gerencial$assessora_gerencial_2025) &
        str_squish(perfil_gerencial$assessora_gerencial_2025) != ""
    ),
    "zero escolas",
    !any(
      !is.na(perfil_gerencial$assessora_gerencial_2025) &
        str_squish(perfil_gerencial$assessora_gerencial_2025) != ""
    ),
    "Nenhum vínculo gerencial deve ser imputado a 2025."
  )
)
write_csv(
  validacoes_universo,
  file.path(pasta_execucao, "03_validacao_universo.csv"),
  na = ""
)
if (any(
  validacoes_universo$severidade == "erro" &
    validacoes_universo$status == "reprovado"
)) {
  stop("Contrato do universo oficial reprovado.")
}

# -------------------------------------------------------------------
# 6. Parâmetros operacionais
# -------------------------------------------------------------------

parametros_componentes <- tribble(
  ~dimensao, ~componente, ~variavel_origem, ~sentido,
  ~peso_na_dimensao, ~peso_no_indice, ~uso_no_indice, ~justificativa,
  "volume", "matriculas_anos_iniciais", "matriculas_anos_iniciais",
  "maior = maior carga", .60, .40 * .60, TRUE,
  "Volume principal de estudantes potencialmente alcançados.",
  "volume", "turmas_anos_iniciais", "turmas_anos_iniciais",
  "maior = maior carga", .25, .40 * .25, TRUE,
  "Número de grupos escolares acompanhados.",
  "volume", "amplitude_etapas", "numero_etapas_amplas_ofertadas",
  "maior = maior carga", .15, .40 * .15, TRUE,
  "Amplitude organizacional da unidade.",
  "estrutural", "infraestrutura_insuficiente",
  "indice_infraestrutura_basica", "menor = maior complexidade",
  .30, .35 * .30, TRUE,
  "Infraestrutura relativa potencialmente insuficiente.",
  "estrutural", "pressao_alunos_turma",
  "alunos_por_turma_anos_iniciais", "maior = maior complexidade",
  .20, .35 * .20, TRUE,
  "Pressão organizacional por turma.",
  "estrutural", "pressao_alunos_docente",
  "alunos_por_docente_anos_iniciais", "maior = maior complexidade",
  .15, .35 * .15, TRUE,
  "Pressão relativa por docente.",
  "estrutural", "educacao_especial",
  "pct_matriculas_educacao_especial", "maior = maior coordenação",
  .20, .35 * .20, TRUE,
  "Pode requerer articulação e apoio diferenciados; não é déficit.",
  "estrutural", "tempo_integral",
  "pct_matriculas_anos_iniciais_integral", "maior = maior organização",
  .10, .35 * .10, TRUE,
  "Jornada ampliada aumenta a complexidade operacional.",
  "estrutural", "transporte_publico",
  "pct_matriculas_transporte_publico", "maior = maior logística",
  .05, .35 * .05, TRUE,
  "Sinaliza desafios logísticos potenciais.",
  "administrativa", "arranjo_institucional_especial",
  "grupo_administrativo_2024_final", "presença = maior complexidade",
  .45, .25 * .45, TRUE,
  "Arranjos especiais exigem articulação institucional.",
  "administrativa", "transicao_recente",
  "municipalização, possível municipalização ou escola nova",
  "presença = maior complexidade", .35, .25 * .35, TRUE,
  "Transições recentes ampliam demandas de integração.",
  "administrativa", "historico_cadastral",
  "requer_revisao_tecnica ou divergencia_censo_cadastro",
  "presença = maior complexidade", .20, .25 * .20, TRUE,
  "Demandas cadastrais ou históricas adicionais.",
  "diagnostico_educacional_paralelo", "menor_proficiencia",
  "proficiencia_media_2026", "menor = maior demanda pedagógica",
  0, 0, FALSE,
  "Preservado fora do índice para futura camada diagnóstica.",
  "diagnostico_educacional_paralelo", "defasagem",
  "pct_defasagem_2026", "maior = maior demanda pedagógica",
  0, 0, FALSE,
  "Preservado fora do índice para futura camada diagnóstica.",
  "diagnostico_educacional_paralelo", "participacao",
  "taxa_participacao_2026", "menor = maior cautela de cobertura",
  0, 0, FALSE,
  "Condiciona a leitura; não é componente de carga operacional."
)

pesos_dimensoes <- tribble(
  ~dimensao, ~peso, ~papel,
  "volume", .40, "componente do índice principal",
  "estrutural", .35, "componente do índice principal",
  "administrativa", .25, "componente do índice principal",
  "educacional", 0, "diagnóstico paralelo; proibido no índice"
)

# -------------------------------------------------------------------
# 7. Índice operacional — somente 53 escolas
# -------------------------------------------------------------------

base_indice <- universo_gerencial |>
  mutate(
    municipalizada_apos_2024 = as_logical_seguro(
      municipalizada_apos_2024,
      "municipalizada_apos_2024"
    ),
    possivel_municipalizacao_recente = as_logical_seguro(
      possivel_municipalizacao_recente,
      "possivel_municipalizacao_recente"
    ),
    escola_nova_recente = as_logical_seguro(
      escola_nova_recente,
      "escola_nova_recente"
    ),
    requer_revisao_tecnica = as_logical_seguro(
      requer_revisao_tecnica,
      "requer_revisao_tecnica"
    ),
    divergencia_censo_cadastro = as_logical_seguro(
      divergencia_censo_cadastro,
      "divergencia_censo_cadastro"
    ),
    turmas_anos_iniciais_validas = if_else(
      !is.na(matriculas_anos_iniciais) &
        matriculas_anos_iniciais > 0 &
        !is.na(turmas_anos_iniciais) &
        turmas_anos_iniciais <= 0,
      NA_real_,
      as.numeric(turmas_anos_iniciais)
    ),
    score_volume_matriculas = percentil_relativo(
      matriculas_anos_iniciais
    ),
    score_volume_turmas = percentil_relativo(
      turmas_anos_iniciais_validas
    ),
    score_volume_etapas = percentil_relativo(
      numero_etapas_amplas_ofertadas
    ),
    score_estrutural_infraestrutura = percentil_relativo(
      indice_infraestrutura_basica, inverso = TRUE
    ),
    score_estrutural_alunos_turma = percentil_relativo(
      alunos_por_turma_anos_iniciais
    ),
    score_estrutural_alunos_docente = percentil_relativo(
      alunos_por_docente_anos_iniciais
    ),
    score_estrutural_educacao_especial = percentil_relativo(
      pct_matriculas_educacao_especial
    ),
    score_estrutural_tempo_integral = percentil_relativo(
      pct_matriculas_anos_iniciais_integral
    ),
    score_estrutural_transporte = percentil_relativo(
      pct_matriculas_transporte_publico
    ),
    score_administrativo_arranjo_especial = if_else(
      !is.na(grupo_administrativo_2024_final) &
        grupo_administrativo_2024_final !=
          "Rede municipal direta em 2024",
      100, 0
    ),
    score_administrativo_transicao_recente = if_else(
      coalesce(municipalizada_apos_2024, FALSE) |
        coalesce(possivel_municipalizacao_recente, FALSE) |
        coalesce(escola_nova_recente, FALSE),
      100, 0
    ),
    score_administrativo_historico_cadastral = if_else(
      coalesce(requer_revisao_tecnica, FALSE) |
        coalesce(divergencia_censo_cadastro, FALSE),
      100, 0
    )
  )

pesos_volume <- c(
  score_volume_matriculas = .60,
  score_volume_turmas = .25,
  score_volume_etapas = .15
)
pesos_estrutural <- c(
  score_estrutural_infraestrutura = .30,
  score_estrutural_alunos_turma = .20,
  score_estrutural_alunos_docente = .15,
  score_estrutural_educacao_especial = .20,
  score_estrutural_tempo_integral = .10,
  score_estrutural_transporte = .05
)
pesos_administrativa <- c(
  score_administrativo_arranjo_especial = .45,
  score_administrativo_transicao_recente = .35,
  score_administrativo_historico_cadastral = .20
)

resultado_volume <- calcular_score_ponderado(
  base_indice, names(pesos_volume), pesos_volume, .75
)
resultado_estrutural <- calcular_score_ponderado(
  base_indice, names(pesos_estrutural), pesos_estrutural, .60
)
resultado_administrativa <- calcular_score_ponderado(
  base_indice, names(pesos_administrativa),
  pesos_administrativa, 1
)

base_indice <- base_indice |>
  mutate(
    score_dimensao_volume = resultado_volume$score,
    cobertura_dimensao_volume = resultado_volume$cobertura,
    score_dimensao_estrutural = resultado_estrutural$score,
    cobertura_dimensao_estrutural = resultado_estrutural$cobertura,
    score_dimensao_administrativa = resultado_administrativa$score,
    cobertura_dimensao_administrativa =
      resultado_administrativa$cobertura
  )

resultado_indice <- calcular_score_ponderado(
  base_indice,
  c(
    "score_dimensao_volume",
    "score_dimensao_estrutural",
    "score_dimensao_administrativa"
  ),
  c(.40, .35, .25),
  cobertura_minima = 1
)

base_indice <- base_indice |>
  mutate(
    indice_carga_potencial_operacional = resultado_indice$score,
    cobertura_indice_operacional = resultado_indice$cobertura,
    contribuicao_volume = .40 * score_dimensao_volume,
    contribuicao_estrutural = .35 * score_dimensao_estrutural,
    contribuicao_administrativa = .25 *
      score_dimensao_administrativa,
    interpretacao_cautelosa = cobertura_dimensao_volume < 1 |
      cobertura_dimensao_estrutural < 1 |
      cobertura_dimensao_administrativa < 1
  )

indice_carga_potencial_escola <- base_indice |>
  transmute(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_vinculo_administrativo,
    assessora_gerencial_2026,
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
    interpretacao_cautelosa,
    resultados_educacionais_no_indice = FALSE,
    nota_uso = paste(
      "Índice relativo e descritivo; examinar dimensões separadamente;",
      "não usar isoladamente para redistribuição."
    )
  ) |>
  arrange(id_escola)

# -------------------------------------------------------------------
# 8. Componentes longos e diagnóstico educacional paralelo
# -------------------------------------------------------------------

montar_componente <- function(
    dados, dimensao, componente, origem, score, peso
) {
  tibble(
    id_escola = dados$id_escola,
    codigo_inep = dados$codigo_inep,
    nome_canonico = dados$nome_canonico,
    assessora_gerencial_2026 =
      dados$assessora_gerencial_2026,
    dimensao = dimensao,
    componente = componente,
    variavel_origem = origem,
    valor_origem = as.numeric(dados[[origem]]),
    score_relativo = as.numeric(dados[[score]]),
    peso_na_dimensao = peso,
    peso_no_indice = peso * case_when(
      dimensao == "volume" ~ .40,
      dimensao == "estrutural" ~ .35,
      dimensao == "administrativa" ~ .25,
      TRUE ~ 0
    ),
    uso_no_indice = TRUE
  )
}

componentes_indice_carga_potencial <- bind_rows(
  montar_componente(
    base_indice, "volume", "matriculas_anos_iniciais",
    "matriculas_anos_iniciais", "score_volume_matriculas", .60
  ),
  montar_componente(
    base_indice, "volume", "turmas_anos_iniciais",
    "turmas_anos_iniciais_validas", "score_volume_turmas", .25
  ),
  montar_componente(
    base_indice, "volume", "amplitude_etapas",
    "numero_etapas_amplas_ofertadas", "score_volume_etapas", .15
  ),
  montar_componente(
    base_indice, "estrutural", "infraestrutura_insuficiente",
    "indice_infraestrutura_basica",
    "score_estrutural_infraestrutura", .30
  ),
  montar_componente(
    base_indice, "estrutural", "pressao_alunos_turma",
    "alunos_por_turma_anos_iniciais",
    "score_estrutural_alunos_turma", .20
  ),
  montar_componente(
    base_indice, "estrutural", "pressao_alunos_docente",
    "alunos_por_docente_anos_iniciais",
    "score_estrutural_alunos_docente", .15
  ),
  montar_componente(
    base_indice, "estrutural", "educacao_especial",
    "pct_matriculas_educacao_especial",
    "score_estrutural_educacao_especial", .20
  ),
  montar_componente(
    base_indice, "estrutural", "tempo_integral",
    "pct_matriculas_anos_iniciais_integral",
    "score_estrutural_tempo_integral", .10
  ),
  montar_componente(
    base_indice, "estrutural", "transporte_publico",
    "pct_matriculas_transporte_publico",
    "score_estrutural_transporte", .05
  ),
  montar_componente(
    base_indice, "administrativa", "arranjo_institucional_especial",
    "score_administrativo_arranjo_especial",
    "score_administrativo_arranjo_especial", .45
  ),
  montar_componente(
    base_indice, "administrativa", "transicao_recente",
    "score_administrativo_transicao_recente",
    "score_administrativo_transicao_recente", .35
  ),
  montar_componente(
    base_indice, "administrativa", "historico_cadastral",
    "score_administrativo_historico_cadastral",
    "score_administrativo_historico_cadastral", .20
  )
) |>
  arrange(id_escola, dimensao, componente)

componentes_educacionais_escola_serie <- universo_serie |>
  group_by(ano_escolar) |>
  mutate(
    posicao_relativa_menor_proficiencia_2026 =
      percentil_relativo(
        proficiencia_media_2026, inverso = TRUE
      ),
    posicao_relativa_maior_defasagem_2026 =
      percentil_relativo(pct_defasagem_2026),
    posicao_relativa_menor_participacao_2026 =
      percentil_relativo(
        taxa_participacao_2026, inverso = TRUE
      )
  ) |>
  ungroup() |>
  transmute(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_gerencial_2026,
    ano_escolar,
    componente,
    previstos_2026,
    avaliados_2026,
    taxa_participacao_2026,
    proficiencia_media_2026,
    pct_defasagem_2026,
    pct_intermediario_2026,
    pct_adequado_2026,
    posicao_relativa_menor_proficiencia_2026,
    posicao_relativa_maior_defasagem_2026,
    posicao_relativa_menor_participacao_2026,
    painel_resultado_balanceado,
    delta_participacao,
    delta_proficiencia,
    variacao_relativa_previstos,
    aumento_participacao_10pp,
    queda_participacao_10pp,
    mudanca_previstos_20pct,
    alerta_composicao_serie,
    uso_no_indice_carga_operacional = FALSE,
    peso_no_indice_carga_operacional = 0,
    natureza = paste(
      "diagnóstico educacional paralelo, não causal;",
      "base para camada gerencial posterior"
    )
  ) |>
  arrange(id_escola, ano_escolar, componente)

# -------------------------------------------------------------------
# 9. Dicionário e validações finais
# -------------------------------------------------------------------

descricoes <- c(
  indice_carga_potencial_operacional = paste(
    "Índice relativo de 0 a 100: volume 40%, estrutura 35%",
    "e administração 25%; sem resultados educacionais."
  ),
  score_dimensao_volume = "Escore relativo do volume de atendimento.",
  score_dimensao_estrutural =
    "Escore relativo de complexidade estrutural.",
  score_dimensao_administrativa =
    "Escore de complexidade administrativa.",
  resultados_educacionais_no_indice =
    "Trava documental: deve ser sempre FALSE.",
  interpretacao_cautelosa =
    "TRUE quando algum componente operacional tem cobertura incompleta."
)
dicionario_indice <- tibble(
  ordem_coluna = seq_along(indice_carga_potencial_escola),
  variavel = names(indice_carga_potencial_escola),
  classe_r = map_chr(
    indice_carga_potencial_escola,
    ~ paste(class(.x), collapse = " | ")
  )
) |>
  mutate(
    descricao = unname(descricoes[variavel]),
    descricao = if_else(
      is.na(descricao),
      paste0(
        "Variável operacional do módulo 17: ",
        str_replace_all(variavel, "_", " "), "."
      ),
      descricao
    ),
    universo = "53 escolas elegíveis à carga de assessoramento em 2026",
    advertencia = paste(
      "Não é ranking de qualidade, avaliação de assessora,",
      "efeito causal ou regra automática."
    )
  )

texto_indice <- paste(
  names(indice_carga_potencial_escola),
  parametros_componentes$variavel_origem[
    parametros_componentes$uso_no_indice
  ],
  collapse = " | "
)
termos_educacionais_proibidos <- c(
  "proficiencia", "defasagem", "participacao",
  "avaliados", "previstos", "delta_proficiencia",
  "score_dimensao_educacional"
)
ocorrencias_proibidas <- sum(map_lgl(
  termos_educacionais_proibidos,
  ~ str_detect(
    texto_indice, fixed(.x, ignore_case = TRUE)
  )
))

validacoes_finais <- bind_rows(
  registrar_validacao(
    "Índice entre 0 e 100", "consistencia", "erro",
    sum(
      !is.na(
        indice_carga_potencial_escola$
          indice_carga_potencial_operacional
      ) &
        (
          indice_carga_potencial_escola$
            indice_carga_potencial_operacional < -1e-8 |
          indice_carga_potencial_escola$
            indice_carga_potencial_operacional > 100 + 1e-8
        )
    ),
    "zero valores fora do intervalo",
    all(
      !is.na(
        indice_carga_potencial_escola$
          indice_carga_potencial_operacional
      ) &
        indice_carga_potencial_escola$
          indice_carga_potencial_operacional >= -1e-8 &
        indice_carga_potencial_escola$
          indice_carga_potencial_operacional <= 100 + 1e-8
    ),
    "Todos os 53 índices devem estar no intervalo."
  ),
  registrar_validacao(
    "Índice disponível", "cobertura", "erro",
    sum(is.na(
      indice_carga_potencial_escola$
        indice_carga_potencial_operacional
    )),
    "zero escolas sem índice",
    !any(is.na(
      indice_carga_potencial_escola$
        indice_carga_potencial_operacional
    )),
    "Cobertura operacional obrigatória."
  ),
  registrar_validacao(
    "Resultados educacionais ausentes do índice",
    "metodologia", "erro",
    ocorrencias_proibidas, "zero termos",
    ocorrencias_proibidas == 0L,
    "Proficiência, defasagem e participação ficam no diagnóstico."
  ),
  registrar_validacao(
    "Flag educacional sempre falsa", "metodologia", "erro",
    sum(
      indice_carga_potencial_escola$
        resultados_educacionais_no_indice
    ),
    "zero TRUE",
    !any(
      indice_carga_potencial_escola$
        resultados_educacionais_no_indice
    ),
    "Trava explícita no produto principal."
  ),
  registrar_validacao(
    "Diagnóstico educacional com peso zero",
    "metodologia", "erro",
    sum(
      componentes_educacionais_escola_serie$
        peso_no_indice_carga_operacional != 0
    ),
    "zero linhas com peso diferente de zero",
    all(
      componentes_educacionais_escola_serie$
        peso_no_indice_carga_operacional == 0
    ),
    "Produto paralelo nunca alimenta o índice."
  ),
  registrar_validacao(
    "Produto principal com 53 escolas", "universo", "erro",
    nrow(indice_carga_potencial_escola), "53",
    nrow(indice_carga_potencial_escola) == 53L,
    "Universo oficial."
  ),
  registrar_validacao(
    "Diagnóstico paralelo com 265 linhas", "universo", "erro",
    nrow(componentes_educacionais_escola_serie), "265",
    nrow(componentes_educacionais_escola_serie) == 265L,
    "53 escolas × cinco séries."
  ),
  registrar_validacao(
    "Pesos dimensionais do índice", "metodologia", "erro",
    sum(pesos_dimensoes$peso), "1",
    abs(sum(pesos_dimensoes$peso) - 1) < 1e-12,
    "Inclui peso educacional explicitamente igual a zero."
  )
)

validacoes <- bind_rows(validacoes_universo, validacoes_finais)
write_csv(
  validacoes,
  file.path(pasta_execucao, "11_validacao_final.csv"),
  na = ""
)
if (any(
  validacoes$severidade == "erro" &
    validacoes$status == "reprovado"
)) {
  stop("Validações finais reprovaram a execução.")
}

# -------------------------------------------------------------------
# 10. Candidatos, releitura e equivalência
# -------------------------------------------------------------------

objetos_saida <- list(
  indice_csv = indice_carga_potencial_escola,
  indice_rds = indice_carga_potencial_escola,
  componentes_csv = componentes_indice_carga_potencial,
  componentes_rds = componentes_indice_carga_potencial,
  diagnostico_educacional_csv =
    componentes_educacionais_escola_serie,
  diagnostico_educacional_rds =
    componentes_educacionais_escola_serie,
  dicionario_csv = dicionario_indice
)

caminhos_candidatos <- set_names(
  file.path(pasta_candidatos, basename(arquivos_saida)),
  names(arquivos_saida)
)
walk(names(arquivos_saida), function(produto) {
  gravar_produto(
    produto,
    objetos_saida[[produto]],
    caminhos_candidatos[[produto]]
  )
})

modelos_csv <- list(
  indice_csv = indice_carga_potencial_escola,
  componentes_csv = componentes_indice_carga_potencial,
  diagnostico_educacional_csv =
    componentes_educacionais_escola_serie,
  dicionario_csv = dicionario_indice
)

candidatos_relidos <- map(
  names(caminhos_candidatos),
  function(produto) {
    reler_produto(
      caminhos_candidatos[[produto]],
      modelos_csv[[produto]]
    )
  }
) |> set_names(names(caminhos_candidatos))

chaves_produtos <- list(
  indice_csv = "id_escola",
  indice_rds = "id_escola",
  componentes_csv = c("id_escola", "dimensao", "componente"),
  componentes_rds = c("id_escola", "dimensao", "componente"),
  diagnostico_educacional_csv =
    c("id_escola", "ano_escolar", "componente"),
  diagnostico_educacional_rds =
    c("id_escola", "ano_escolar", "componente"),
  dicionario_csv = "ordem_coluna"
)

validacao_candidatos <- map_dfr(
  names(caminhos_candidatos),
  function(produto) {
    esperado <- objetos_saida[[produto]] |>
      arrange(across(all_of(chaves_produtos[[produto]])))
    observado <- candidatos_relidos[[produto]] |>
      arrange(across(all_of(chaves_produtos[[produto]])))
    comparacao <- all.equal(
      esperado, observado,
      check.attributes = FALSE,
      tolerance = 1e-12
    )
    tibble(
      produto = produto,
      caminho_candidato = caminhos_candidatos[[produto]],
      linhas = nrow(observado),
      colunas = ncol(observado),
      md5 = hash_md5(caminhos_candidatos[[produto]]),
      equivalente_ao_objeto = isTRUE(comparacao),
      detalhe = if (isTRUE(comparacao)) {
        "equivalente"
      } else {
        paste(comparacao, collapse = " | ")
      }
    )
  }
)
write_csv(
  validacao_candidatos,
  file.path(pasta_execucao, "12_validacao_candidatos.csv"),
  na = ""
)
if (any(!validacao_candidatos$equivalente_ao_objeto)) {
  stop("Candidatos não equivalem aos objetos em memória.")
}

validacao_pares_candidatos <- bind_rows(
  comparar_bases_semanticamente(
    candidatos_relidos$indice_rds,
    candidatos_relidos$indice_csv,
    "id_escola", "indice"
  )$diagnostico,
  comparar_bases_semanticamente(
    candidatos_relidos$componentes_rds,
    candidatos_relidos$componentes_csv,
    c("id_escola", "dimensao", "componente"),
    "componentes"
  )$diagnostico,
  comparar_bases_semanticamente(
    candidatos_relidos$diagnostico_educacional_rds,
    candidatos_relidos$diagnostico_educacional_csv,
    c("id_escola", "ano_escolar", "componente"),
    "diagnostico_educacional"
  )$diagnostico
)
write_csv(
  validacao_pares_candidatos,
  file.path(pasta_execucao, "13_equivalencia_pares_candidatos.csv"),
  na = ""
)
if (any(!validacao_pares_candidatos$aprovado)) {
  stop("Pares CSV/RDS candidatos divergentes.")
}

# -------------------------------------------------------------------
# 11. Preservação histórica
# -------------------------------------------------------------------

arquivo_temporario_script <- file.path(
  pasta_transacao, "script_historico_git.txt"
)
extracao_aprovada <- extrair_arquivo_git(
  commit_base_integracao,
  caminho_relativo_script,
  arquivo_temporario_script
)
if (!extracao_aprovada) {
  stop("Não foi possível recuperar o script histórico do commit-base.")
}
md5_script_git <- hash_md5(arquivo_temporario_script)

if (file.exists(caminho_script_historico)) {
  if (hash_md5(caminho_script_historico) != md5_script_git) {
    stop("Script histórico existente diverge do commit-base.")
  }
} else {
  copiado <- file.copy(
    arquivo_temporario_script,
    caminho_script_historico,
    overwrite = FALSE
  )
  if (!copiado ||
      hash_md5(caminho_script_historico) != md5_script_git) {
    stop("Falha ao preservar o script histórico.")
  }
}

preservar_produtos_historicos <- function() {
  if (file.exists(caminho_manifesto_historico)) {
    manifesto <- read_csv(
      caminho_manifesto_historico,
      show_col_types = FALSE,
      progress = FALSE
    )
    divergentes <- manifesto |>
      filter(tipo == "produto") |>
      mutate(
        existe = file.exists(destino),
        md5_atual = map_chr(destino, hash_md5)
      ) |>
      filter(!existe | md5_atual != md5_copia)
    if (nrow(divergentes) > 0L) {
      stop("Histórico fixo do módulo 17 está divergente.")
    }
    return(manifesto)
  }

  produtos_existentes <- arquivos_saida[
    file.exists(arquivos_saida)
  ]
  manifesto_produtos <- map_dfr(
    names(produtos_existentes),
    function(produto) {
      origem <- produtos_existentes[[produto]]
      destino <- file.path(
        pasta_historico,
        paste0("pre_modulo_16_", basename(origem))
      )
      md5_origem <- hash_md5(origem)
      copiado <- file.copy(origem, destino, overwrite = FALSE)
      if (!copiado && !file.exists(destino)) {
        stop("Falha ao arquivar produto anterior: ", produto)
      }
      md5_destino <- hash_md5(destino)
      if (md5_origem != md5_destino) {
        stop("Hash divergente na cópia histórica: ", produto)
      }
      tibble(
        tipo = "produto",
        item = produto,
        origem = normalizar_caminho(origem),
        destino = normalizar_caminho(destino),
        md5_antes_copia = md5_origem,
        md5_copia = md5_destino,
        igualdade_confirmada = TRUE
      )
    }
  )
  manifesto_script <- tibble(
    tipo = "script",
    item = "script_pre_modulo_16",
    origem = paste0(
      "git:", commit_base_integracao, ":",
      caminho_relativo_script
    ),
    destino = normalizar_caminho(caminho_script_historico),
    md5_antes_copia = md5_script_git,
    md5_copia = hash_md5(caminho_script_historico),
    igualdade_confirmada =
      md5_script_git == hash_md5(caminho_script_historico)
  )
  manifesto <- bind_rows(manifesto_script, manifesto_produtos)
  write_csv(manifesto, caminho_manifesto_historico, na = "")
  manifesto
}

manifesto_historico <- preservar_produtos_historicos()
write_csv(
  manifesto_historico,
  file.path(pasta_execucao, "14_manifesto_historico.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 12. Promoção transacional e rollback
# -------------------------------------------------------------------

promover_transacionalmente <- function() {
  produtos <- names(arquivos_saida)
  destinos <- set_names(
    as.character(arquivos_saida[produtos]), produtos
  )
  candidatos <- set_names(
    as.character(caminhos_candidatos[produtos]), produtos
  )
  existia <- set_names(
    file.exists(unname(destinos)), produtos
  )
  hashes_antes <- set_names(
    map_chr(unname(destinos), hash_md5), produtos
  )
  movidos_rollback <- set_names(
    rep(FALSE, length(produtos)), produtos
  )
  promovidos <- set_names(
    rep(FALSE, length(produtos)), produtos
  )

  restaurar <- function() {
    ok <- TRUE
    for (produto in rev(produtos)) {
      destino <- destinos[[produto]]
      backup <- file.path(
        pasta_rollback, basename(destino)
      )
      if (promovidos[[produto]] && file.exists(destino)) {
        ok <- file.remove(destino) && ok
      }
      if (
        movidos_rollback[[produto]] &&
          file.exists(backup)
      ) {
        ok <- file.rename(backup, destino) && ok
      }
    }
    confirmado <- map_lgl(produtos, function(produto) {
      destino <- destinos[[produto]]
      if (existia[[produto]]) {
        file.exists(destino) &&
          hash_md5(destino) == hashes_antes[[produto]]
      } else {
        !file.exists(destino)
      }
    })
    ok && all(confirmado)
  }

  tryCatch({
    for (produto in produtos) {
      destino <- destinos[[produto]]
      candidato <- candidatos[[produto]]
      dir.create(
        dirname(destino), recursive = TRUE, showWarnings = FALSE
      )
      if (existia[[produto]]) {
        backup <- file.path(
          pasta_rollback, basename(destino)
        )
        if (!file.rename(destino, backup)) {
          stop("Falha ao preparar rollback de `", produto, "`.")
        }
        movidos_rollback[[produto]] <- TRUE
      }
      if (!file.rename(candidato, destino)) {
        stop("Falha ao promover `", produto, "`.")
      }
      promovidos[[produto]] <- TRUE
    }

    hashes_finais <- set_names(
      map_chr(unname(destinos), hash_md5), produtos
    )
    hashes_candidatos <- set_names(
      validacao_candidatos$md5[
        match(produtos, validacao_candidatos$produto)
      ],
      produtos
    )
    if (
      any(is.na(hashes_finais)) ||
        any(hashes_finais != hashes_candidatos)
    ) {
      stop("Hashes finais divergem dos candidatos.")
    }
    tibble(
      produto = produtos,
      candidato = candidatos,
      destino = destinos,
      existia_antes = unname(existia),
      md5_antes = unname(hashes_antes),
      md5_candidato = unname(hashes_candidatos),
      md5_final = unname(hashes_finais),
      promocao_confirmada =
        unname(hashes_candidatos == hashes_finais)
    )
  }, error = function(e) {
    rollback_confirmado <- restaurar()
    writeLines(
      c(
        paste0("Erro: ", conditionMessage(e)),
        paste0(
          "Rollback confirmado: ", rollback_confirmado
        )
      ),
      file.path(
        pasta_execucao, "18_falha_promocao_rollback.txt"
      )
    )
    if (rollback_confirmado) {
      stop(
        "Promoção transacional falhou; produtos anteriores ",
        "restaurados. Detalhe: ", conditionMessage(e)
      )
    }
    stop(
      "Promoção falhou e rollback não foi integralmente ",
      "confirmado. Preserve a pasta de transação. Detalhe: ",
      conditionMessage(e)
    )
  })
}

resultado_promocao <- promover_transacionalmente()
write_csv(
  resultado_promocao,
  file.path(
    pasta_execucao, "18_resultado_promocao_transacional.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 13. Documentação final da execução
# -------------------------------------------------------------------

write_csv(
  parametros_componentes,
  file.path(pasta_execucao, "04_parametros_componentes.csv"),
  na = ""
)
write_csv(
  pesos_dimensoes,
  file.path(pasta_execucao, "05_pesos_dimensoes.csv"),
  na = ""
)
write_csv(
  exclusoes_observadas,
  file.path(pasta_execucao, "06_escolas_excluidas_carga.csv"),
  na = ""
)
write_csv(
  constancia_serie,
  file.path(pasta_execucao, "07_constancia_escola_serie.csv"),
  na = ""
)
write_csv(
  estrutura_base(
    indice_carga_potencial_escola,
    "indice_carga_potencial_escola"
  ) |>
    bind_rows(estrutura_base(
      componentes_indice_carga_potencial,
      "componentes_indice_carga_potencial"
    )) |>
    bind_rows(estrutura_base(
      componentes_educacionais_escola_serie,
      "componentes_educacionais_escola_serie"
    )),
  file.path(pasta_execucao, "08_estrutura_bases_saida.csv"),
  na = ""
)

manifesto_produtos <- map_dfr(
  names(arquivos_saida),
  ~ inventariar_arquivo(.x, arquivos_saida[[.x]])
)
write_csv(
  manifesto_produtos,
  file.path(
    pasta_execucao, "19_manifesto_produtos_modulo_17.csv"
  ),
  na = ""
)

writeLines(
  capture.output(sessionInfo()),
  file.path(pasta_execucao, "20_session_info.txt")
)

resumo_execucao <- c(
  paste0("Execução: ", id_execucao),
  paste0("Commit-base: ", commit_base_integracao),
  paste0("HEAD: ", commit_git_execucao),
  paste0("Branch: ", branch_execucao),
  paste0("MD5 do script: ", hash_md5(caminho_script)),
  "Universo inicial: 56 escolas e 280 linhas escola × série.",
  "Universo do índice: 53 escolas, 265 linhas e 11 assessoras.",
  "Exclusões: ESC_001, ESC_055 e ESC_056.",
  "Índice: volume 40%, estrutura 35%, administração 25%.",
  "Resultados educacionais no índice: NÃO.",
  paste(
    "Diagnóstico educacional paralelo:",
    "preservado com peso zero para camada gerencial posterior."
  ),
  "",
  "Advertências:",
  "- ferramenta relativa, descritiva e não causal;",
  "- não é ranking de qualidade ou desempenho de assessoras;",
  "- não determina redistribuição automática;",
  "- decisões exigem leitura conjunta das dimensões operacionais;",
  "- a futura camada pedagógica deverá explicitar que menor",
  "  proficiência pode demandar esforço adicional de assessoramento;",
  "- participação, composição e cobertura devem acompanhar essa leitura."
)
writeLines(
  resumo_execucao,
  file.path(pasta_execucao, "21_resumo_execucao.txt")
)

message(
  "Módulo 17 concluído com sucesso.\n",
  "Índice operacional: 53 escolas.\n",
  "Resultados educacionais no índice: NÃO.\n",
  "Diagnóstico educacional paralelo preservado: 265 linhas.\n",
  "Documentação: ", pasta_execucao
)
