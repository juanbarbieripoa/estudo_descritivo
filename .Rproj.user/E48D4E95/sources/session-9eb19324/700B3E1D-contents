# ===================================================================
# 20_gerar_fichas_carteiras.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# ===================================================================
#
# OBJETIVO ANALÍTICO
#
# Produzir fichas gerenciais padronizadas das carteiras de assessoramento,
# reunindo:
#
#   1. carga extensiva: escolas, matrículas, turmas e cobertura avaliativa;
#   2. carga potencial acumulada e complexidade média das escolas;
#   3. quatro dimensões do índice descritivo de carga potencial;
#   4. composição interna da carteira e concentração de casos;
#   5. sensibilidade da carga a cenários alternativos de pesos;
#   6. alertas de cobertura, transição administrativa e interpretação;
#   7. relação detalhada das escolas vinculadas à carteira.
#
# As fichas descrevem carteiras administrativas e NÃO avaliam a qualidade
# das assessoras. Resultados educacionais não representam efeito causal do
# assessoramento. Posições, faixas e comparações são relativas e servem
# apenas como instrumentos de diagnóstico gerencial.
#
# ENTRADAS
#
# dados_finais/analise_carteiras_assessoras.rds ou .csv
# dados_finais/carteira_escola_detalhe.rds ou .csv
# dados_finais/analise_carteiras_cenarios.rds ou .csv
# dados_finais/base_fichas_escolas.rds ou .csv
#
# PRODUTOS PRINCIPAIS
#
# dados_finais/base_fichas_carteiras.csv e .rds
# resultados/fichas_carteiras/execucao_<data_hora>/html/*.html
# resultados/fichas_carteiras/execucao_<data_hora>/graficos/*.png
# resultados/fichas_carteiras/execucao_<data_hora>/fichas_carteiras_compiladas.html
# resultados/fichas_carteiras/execucao_<data_hora>/fichas_carteiras_compiladas.pdf
#   (opcional, quando pagedown e navegador compatível estiverem disponíveis)
# documentacao/fichas_carteiras/dicionario_base_fichas_carteiras.csv
# documentacao/fichas_carteiras/execucao_<data_hora>/...
# ===================================================================

library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Parâmetros gerais e identificação da execução
# -------------------------------------------------------------------

id_execucao <- format(Sys.time(), "%Y%m%d_%H%M%S")

# A conversão para PDF é opcional. A ausência de pagedown ou de navegador
# compatível não interrompe a geração dos HTMLs.
TENTAR_GERAR_PDF_COMPILADO <- TRUE

pasta_dados_finais <- here("dados_finais")
pasta_resultados <- here("resultados", "fichas_carteiras")
pasta_resultados_execucao <- here(
  "resultados", "fichas_carteiras", paste0("execucao_", id_execucao)
)
pasta_html <- file.path(pasta_resultados_execucao, "html")
pasta_graficos <- file.path(pasta_resultados_execucao, "graficos")

pasta_documentacao <- here("documentacao", "fichas_carteiras")
pasta_execucao <- here(
  "documentacao", "fichas_carteiras", paste0("execucao_", id_execucao)
)
pasta_historico <- here(
  "dados_finais", "historico", "fichas_carteiras",
  paste0("execucao_", id_execucao)
)

for (pasta in c(
  pasta_dados_finais,
  pasta_resultados,
  pasta_resultados_execucao,
  pasta_html,
  pasta_graficos,
  pasta_documentacao,
  pasta_execucao,
  pasta_historico
)) {
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
}

# -------------------------------------------------------------------
# 2. Arquivos de entrada e saída
# -------------------------------------------------------------------

arquivos_entrada_rds <- c(
  analise_carteiras = here("dados_finais", "analise_carteiras_assessoras.rds"),
  detalhe_escolas = here("dados_finais", "carteira_escola_detalhe.rds"),
  cenarios_carteiras = here("dados_finais", "analise_carteiras_cenarios.rds"),
  fichas_escolas = here("dados_finais", "base_fichas_escolas.rds")
)

arquivos_entrada_csv <- c(
  analise_carteiras = here("dados_finais", "analise_carteiras_assessoras.csv"),
  detalhe_escolas = here("dados_finais", "carteira_escola_detalhe.csv"),
  cenarios_carteiras = here("dados_finais", "analise_carteiras_cenarios.csv"),
  fichas_escolas = here("dados_finais", "base_fichas_escolas.csv")
)

arquivos_saida <- c(
  base_fichas_csv = here("dados_finais", "base_fichas_carteiras.csv"),
  base_fichas_rds = here("dados_finais", "base_fichas_carteiras.rds"),
  dicionario_csv = here(
    "documentacao", "fichas_carteiras",
    "dicionario_base_fichas_carteiras.csv"
  )
)

arquivo_html_compilado <- file.path(
  pasta_resultados_execucao,
  "fichas_carteiras_compiladas.html"
)

arquivo_pdf_compilado <- file.path(
  pasta_resultados_execucao,
  "fichas_carteiras_compiladas.pdf"
)

# -------------------------------------------------------------------
# 3. Funções auxiliares gerais
# -------------------------------------------------------------------

ler_base_preferindo_rds <- function(nome_fonte) {
  caminho_rds <- arquivos_entrada_rds[[nome_fonte]]
  caminho_csv <- arquivos_entrada_csv[[nome_fonte]]

  if (file.exists(caminho_rds)) {
    dados <- readRDS(caminho_rds)
    origem <- caminho_rds
    formato <- "RDS"
  } else if (file.exists(caminho_csv)) {
    dados <- read_csv(
      caminho_csv,
      show_col_types = FALSE,
      progress = FALSE,
      na = c("", "NA", "NaN")
    )
    origem <- caminho_csv
    formato <- "CSV"
  } else {
    stop(
      "Nenhum arquivo de entrada foi encontrado para a fonte `",
      nome_fonte,
      "`. Caminhos verificados: ",
      caminho_rds,
      " e ",
      caminho_csv
    )
  }

  list(
    dados = as_tibble(dados),
    origem = origem,
    formato = formato
  )
}

arquivar_arquivo_existente <- function(caminho) {
  if (file.exists(caminho)) {
    destino <- file.path(pasta_historico, basename(caminho))
    ok <- file.copy(caminho, destino, overwrite = TRUE)

    if (!isTRUE(ok)) {
      stop("Não foi possível arquivar o arquivo existente: ", caminho)
    }
  }
}

calcular_md5 <- function(caminho) {
  if (!file.exists(caminho)) {
    return(NA_character_)
  }

  unname(tools::md5sum(caminho)[[1]])
}

html_escape <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- str_replace_all(x, "&", "&amp;")
  x <- str_replace_all(x, "<", "&lt;")
  x <- str_replace_all(x, ">", "&gt;")
  x <- str_replace_all(x, '"', "&quot;")
  x <- str_replace_all(x, "'", "&#39;")
  x
}

slugificar <- function(x) {
  x_original <- as.character(x)

  x_ascii <- suppressWarnings(
    iconv(x_original, from = "UTF-8", to = "ASCII//TRANSLIT")
  )

  x_ascii[is.na(x_ascii)] <- x_original[is.na(x_ascii)]

  x_ascii |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_replace_all("(^-+|-+$)", "") |>
    str_sub(1, 80)
}

nome_exibicao_carteira <- function(x) {
  x <- as.character(x)

  case_when(
    x == "outras" ~ "Outras",
    x == "Sem vinculação informada" ~ "Sem vinculação informada",
    TRUE ~ str_to_title(x)
  )
}

fmt_num <- function(x, digits = 0) {
  if (length(x) == 0 || is.na(x[[1]])) {
    return("Não disponível")
  }

  formatC(
    as.numeric(x[[1]]),
    format = "f",
    digits = digits,
    big.mark = ".",
    decimal.mark = ","
  )
}

fmt_pct <- function(x, digits = 1) {
  if (length(x) == 0 || is.na(x[[1]])) {
    return("Não disponível")
  }

  paste0(fmt_num(x, digits), "%")
}

fmt_ratio <- function(x, digits = 2) {
  if (length(x) == 0 || is.na(x[[1]])) {
    return("Não comparável")
  }

  paste0(fmt_num(x, digits), "×")
}

sim_nao <- function(x) {
  if (length(x) == 0 || is.na(x[[1]])) {
    return("Não informado")
  }

  ifelse(isTRUE(x[[1]]), "Sim", "Não")
}

valor_seguro <- function(numerador, denominador) {
  if (
    length(denominador) == 0 ||
      is.na(denominador[[1]]) ||
      !is.finite(as.numeric(denominador[[1]])) ||
      as.numeric(denominador[[1]]) == 0
  ) {
    return(rep(NA_real_, length(numerador)))
  }

  as.numeric(numerador) / as.numeric(denominador[[1]])
}

flag_texto_escola <- function(linha) {
  flags <- character(0)

  if (isTRUE(linha$possui_complexidade_administrativa[[1]])) {
    flags <- c(flags, "Complexidade administrativa")
  }

  if (isTRUE(linha$interpretacao_indice_cautelosa[[1]])) {
    flags <- c(flags, "Índice com cautela")
  }

  if (isTRUE(linha$painel_incompleto[[1]])) {
    flags <- c(flags, "Painel incompleto")
  }

  if (!is.na(linha$sensibilidade_faixa[[1]]) &&
      linha$sensibilidade_faixa[[1]] == "Sensibilidade elevada") {
    flags <- c(flags, "Sensível aos pesos")
  }

  if (isTRUE(linha$participacao_2026_abaixo_80[[1]])) {
    flags <- c(flags, "Participação 2026 abaixo de 80%")
  }

  if (length(flags) == 0) {
    "Sem alerta prioritário"
  } else {
    paste(flags, collapse = "; ")
  }
}

# -------------------------------------------------------------------
# 4. Leitura das bases e manifesto dos insumos
# -------------------------------------------------------------------

fontes <- names(arquivos_entrada_rds)
leituras <- vector("list", length(fontes))
names(leituras) <- fontes

for (i in seq_along(fontes)) {
  leituras[[i]] <- ler_base_preferindo_rds(fontes[[i]])
}

analise_carteiras <- leituras$analise_carteiras$dados
detalhe_escolas <- leituras$detalhe_escolas$dados
cenarios_carteiras <- leituras$cenarios_carteiras$dados
fichas_escolas <- leituras$fichas_escolas$dados

manifesto_entrada <- bind_rows(lapply(fontes, function(fonte) {
  leitura <- leituras[[fonte]]
  caminho <- leitura$origem

  tibble(
    fonte = fonte,
    arquivo = caminho,
    formato = leitura$formato,
    existe = file.exists(caminho),
    tamanho_bytes = if (file.exists(caminho)) file.info(caminho)$size else NA_real_,
    modificado_em = if (file.exists(caminho)) {
      as.character(file.info(caminho)$mtime)
    } else {
      NA_character_
    },
    md5 = calcular_md5(caminho),
    linhas = nrow(leitura$dados),
    colunas = ncol(leitura$dados)
  )
}))

write_csv(
  manifesto_entrada,
  file.path(pasta_execucao, "01_manifesto_arquivos_entrada.csv"),
  na = ""
)

estrutura_entrada <- bind_rows(lapply(fontes, function(fonte) {
  dados <- leituras[[fonte]]$dados

  tibble(
    fonte = fonte,
    coluna = names(dados),
    classe = vapply(dados, function(x) paste(class(x), collapse = " | "), character(1)),
    valores_nao_ausentes = vapply(dados, function(x) sum(!is.na(x)), numeric(1)),
    valores_ausentes = vapply(dados, function(x) sum(is.na(x)), numeric(1)),
    valores_distintos = vapply(dados, function(x) n_distinct(x, na.rm = TRUE), numeric(1))
  )
}))

write_csv(
  estrutura_entrada,
  file.path(pasta_execucao, "02_estrutura_bases_entrada.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 5. Validação das colunas obrigatórias
# -------------------------------------------------------------------

colunas_obrigatorias <- list(
  analise_carteiras = c(
    "assessora_gerencial", "tipo_carteira", "carteira_nominal",
    "numero_escolas", "matriculas_anos_iniciais_total",
    "turmas_anos_iniciais_total", "series_comparaveis_total",
    "escolas_painel_incompleto", "taxa_participacao_agregada_2026",
    "carga_potencial_total", "indice_carga_potencial_medio",
    "indice_carga_potencial_mediano", "indice_carga_potencial_minimo",
    "indice_carga_potencial_maximo", "dimensao_volume_media",
    "dimensao_estrutural_media", "dimensao_educacional_media",
    "dimensao_administrativa_media", "numero_escolas_faixa_1",
    "numero_escolas_faixa_2", "numero_escolas_faixa_3",
    "numero_escolas_faixa_4", "numero_escolas_faixas_3_4",
    "numero_escolas_complexidade_administrativa",
    "numero_escolas_interpretacao_cautelosa",
    "numero_escolas_sensibilidade_elevada",
    "numero_escolas_alerta_composicao",
    "participacao_maior_escola_carga_total_pct",
    "participacao_duas_maiores_escolas_carga_total_pct",
    "nome_escola_maior_indice", "maior_indice_escola",
    "participacao_escolas_rede_pct", "participacao_matriculas_rede_pct",
    "participacao_turmas_rede_pct", "participacao_carga_potencial_rede_pct",
    "razao_carga_potencial_referencia_nominal",
    "faixa_carga_total_entre_carteiras_nominais",
    "faixa_indice_medio_entre_carteiras_nominais"
  ),
  detalhe_escolas = c(
    "assessora_gerencial", "tipo_carteira", "carteira_nominal",
    "ordem_interna_carga_potencial", "id_escola", "codigo_inep",
    "nome_canonico", "matriculas_anos_iniciais", "turmas_anos_iniciais",
    "numero_series_resultado_2026", "numero_series_comparaveis",
    "painel_completo_cinco_series", "qualidade_evidencia_educacional",
    "score_dimensao_volume", "score_dimensao_estrutural",
    "score_dimensao_educacional", "score_dimensao_administrativa",
    "indice_carga_potencial", "faixa_indice_carga_potencial",
    "sensibilidade_faixa", "possui_complexidade_administrativa",
    "interpretacao_indice_cautelosa", "painel_incompleto",
    "possui_alerta_composicao", "participacao_2026_abaixo_80",
    "participacao_indice_na_carteira_pct", "tipo_vinculo_rede_final",
    "status_rede_2025_final"
  ),
  cenarios_carteiras = c(
    "cenario", "descricao_cenario", "assessora_gerencial",
    "tipo_carteira", "carteira_nominal", "numero_escolas",
    "carga_potencial_total_cenario", "indice_medio_cenario",
    "participacao_carga_rede_cenario_pct",
    "ordem_carga_total_carteiras_nominais",
    "faixa_carga_total_cenario_carteiras_nominais",
    "diferenca_carga_total_para_equilibrado",
    "mudanca_ordem_nominais_para_equilibrado",
    "mudou_faixa_carga_total_carteira"
  ),
  fichas_escolas = c(
    "id_escola", "assessora_gerencial", "arquivo_html",
    "observacoes_composicao_limpa", "status_geracao", "html_existe"
  )
)

diagnostico_colunas <- bind_rows(lapply(seq_along(colunas_obrigatorias), function(i) {
  fonte <- names(colunas_obrigatorias)[[i]]
  colunas <- colunas_obrigatorias[[i]]
  dados <- leituras[[fonte]]$dados

  tibble(
    fonte = fonte,
    coluna_obrigatoria = colunas,
    presente = colunas %in% names(dados)
  )
}))

write_csv(
  diagnostico_colunas,
  file.path(pasta_execucao, "03_validacao_colunas_obrigatorias.csv"),
  na = ""
)

colunas_ausentes <- diagnostico_colunas |>
  filter(!presente)

if (nrow(colunas_ausentes) > 0) {
  stop(
    "Há colunas obrigatórias ausentes. Consulte: ",
    file.path(pasta_execucao, "03_validacao_colunas_obrigatorias.csv")
  )
}

# -------------------------------------------------------------------
# 6. Validações de chaves e cobertura entre os insumos
# -------------------------------------------------------------------

duplicidades_carteiras <- analise_carteiras |>
  count(assessora_gerencial, name = "n") |>
  filter(n > 1)

duplicidades_escolas <- detalhe_escolas |>
  count(id_escola, name = "n") |>
  filter(n > 1)

duplicidades_cenarios <- cenarios_carteiras |>
  count(assessora_gerencial, cenario, name = "n") |>
  filter(n > 1)

duplicidades_fichas_escolas <- fichas_escolas |>
  count(id_escola, name = "n") |>
  filter(n > 1)

write_csv(
  bind_rows(
    duplicidades_carteiras |> mutate(fonte = "analise_carteiras", chave = assessora_gerencial) |> select(fonte, chave, n),
    duplicidades_escolas |> mutate(fonte = "detalhe_escolas", chave = id_escola) |> select(fonte, chave, n),
    duplicidades_cenarios |> transmute(fonte = "cenarios_carteiras", chave = paste(assessora_gerencial, cenario, sep = " | "), n),
    duplicidades_fichas_escolas |> mutate(fonte = "fichas_escolas", chave = id_escola) |> select(fonte, chave, n)
  ),
  file.path(pasta_execucao, "04_duplicidades_chaves.csv"),
  na = ""
)

if (
  nrow(duplicidades_carteiras) > 0 ||
    nrow(duplicidades_escolas) > 0 ||
    nrow(duplicidades_cenarios) > 0 ||
    nrow(duplicidades_fichas_escolas) > 0
) {
  stop("Foram encontradas duplicidades nas chaves dos insumos.")
}

categorias_analise <- sort(unique(analise_carteiras$assessora_gerencial))
categorias_detalhe <- sort(unique(detalhe_escolas$assessora_gerencial))
categorias_cenarios <- sort(unique(cenarios_carteiras$assessora_gerencial))

if (!identical(categorias_analise, categorias_detalhe) ||
    !identical(categorias_analise, categorias_cenarios)) {
  stop("O conjunto de categorias administrativas diverge entre os insumos.")
}

escolas_detalhe <- sort(unique(detalhe_escolas$id_escola))
escolas_fichas <- sort(unique(fichas_escolas$id_escola))

if (!identical(escolas_detalhe, escolas_fichas)) {
  stop("O conjunto de escolas diverge entre o detalhe das carteiras e as fichas escolares.")
}

cobertura_insumos <- analise_carteiras |>
  select(assessora_gerencial, numero_escolas_analise = numero_escolas) |>
  left_join(
    detalhe_escolas |>
      count(assessora_gerencial, name = "numero_escolas_detalhe"),
    by = "assessora_gerencial"
  ) |>
  left_join(
    cenarios_carteiras |>
      count(assessora_gerencial, name = "numero_cenarios"),
    by = "assessora_gerencial"
  ) |>
  left_join(
    fichas_escolas |>
      count(assessora_gerencial, name = "numero_fichas_escolas"),
    by = "assessora_gerencial"
  ) |>
  mutate(
    escolas_consistentes = numero_escolas_analise == numero_escolas_detalhe &
      numero_escolas_analise == numero_fichas_escolas,
    quatro_cenarios = numero_cenarios == 4
  )

write_csv(
  cobertura_insumos,
  file.path(pasta_execucao, "05_cobertura_insumos_por_carteira.csv"),
  na = ""
)

if (any(!cobertura_insumos$escolas_consistentes) ||
    any(!cobertura_insumos$quatro_cenarios)) {
  stop("Há inconsistências de cobertura entre os insumos por carteira.")
}

numero_carteiras_nominais <- sum(
  analise_carteiras$carteira_nominal,
  na.rm = TRUE
)

if (numero_carteiras_nominais == 0) {
  stop("Nenhuma carteira nominal foi identificada para construir as referências.")
}

# -------------------------------------------------------------------
# 7. Referências das carteiras nominais e base das fichas
# -------------------------------------------------------------------

referencias_nominais <- analise_carteiras |>
  filter(carteira_nominal) |>
  summarise(
    media_numero_escolas = mean(numero_escolas, na.rm = TRUE),
    media_matriculas = mean(matriculas_anos_iniciais_total, na.rm = TRUE),
    media_turmas = mean(turmas_anos_iniciais_total, na.rm = TRUE),
    media_carga_total = mean(carga_potencial_total, na.rm = TRUE),
    media_indice_escola = weighted.mean(
      indice_carga_potencial_medio,
      numero_escolas,
      na.rm = TRUE
    ),
    media_dimensao_volume = weighted.mean(
      dimensao_volume_media,
      numero_escolas,
      na.rm = TRUE
    ),
    media_dimensao_estrutural = weighted.mean(
      dimensao_estrutural_media,
      numero_escolas,
      na.rm = TRUE
    ),
    media_dimensao_educacional = weighted.mean(
      dimensao_educacional_media,
      numero_escolas,
      na.rm = TRUE
    ),
    media_dimensao_administrativa = weighted.mean(
      dimensao_administrativa_media,
      numero_escolas,
      na.rm = TRUE
    )
  )

base_fichas <- analise_carteiras |>
  mutate(
    nome_exibicao = nome_exibicao_carteira(assessora_gerencial),
    ordem_tipo = case_when(
      carteira_nominal ~ 1L,
      tipo_carteira == "Agrupamento residual" ~ 2L,
      TRUE ~ 3L
    ),
    slug_carteira = slugificar(paste(tipo_carteira, assessora_gerencial, sep = "-")),
    razao_escolas_referencia_nominal = valor_seguro(
      numero_escolas,
      referencias_nominais$media_numero_escolas
    ),
    razao_matriculas_referencia_nominal_ficha = valor_seguro(
      matriculas_anos_iniciais_total,
      referencias_nominais$media_matriculas
    ),
    razao_turmas_referencia_nominal_ficha = valor_seguro(
      turmas_anos_iniciais_total,
      referencias_nominais$media_turmas
    ),
    razao_carga_referencia_nominal_ficha = valor_seguro(
      carga_potencial_total,
      referencias_nominais$media_carga_total
    ),
    razao_indice_medio_referencia_nominal = valor_seguro(
      indice_carga_potencial_medio,
      referencias_nominais$media_indice_escola
    ),
    perfil_carga_descritivo = case_when(
      !carteira_nominal ~ "Categoria administrativa não comparável diretamente às carteiras nominais",
      razao_carga_referencia_nominal_ficha >= 1.15 &
        razao_indice_medio_referencia_nominal >= 1.10 ~
        "Carga acumulada elevada, combinando volume e intensidade média",
      razao_carga_referencia_nominal_ficha >= 1.15 &
        razao_indice_medio_referencia_nominal < 1.10 ~
        "Carga acumulada elevada, predominantemente associada ao volume",
      razao_carga_referencia_nominal_ficha < 0.85 &
        razao_indice_medio_referencia_nominal >= 1.10 ~
        "Carga acumulada menor, com intensidade média relativamente elevada",
      razao_carga_referencia_nominal_ficha < 0.85 ~
        "Carga acumulada abaixo da referência das carteiras nominais",
      razao_indice_medio_referencia_nominal >= 1.10 ~
        "Carga total próxima da referência, com maior intensidade média",
      razao_indice_medio_referencia_nominal < 0.90 ~
        "Carga total próxima da referência, com menor intensidade média",
      TRUE ~ "Carga e intensidade próximas das referências nominais"
    ),
    exige_validacao_qualitativa_prioritaria =
      numero_escolas_interpretacao_cautelosa > 0 |
      numero_escolas_complexidade_administrativa > 0 |
      numero_escolas_sensibilidade_elevada > 0 |
      !carteira_nominal,
    arquivo_html = file.path(
      pasta_html,
      paste0(slug_carteira, ".html")
    ),
    arquivo_grafico_carga = file.path(
      pasta_graficos,
      paste0(slug_carteira, "_carga_relativa.png")
    ),
    arquivo_grafico_dimensoes = file.path(
      pasta_graficos,
      paste0(slug_carteira, "_dimensoes.png")
    ),
    arquivo_grafico_escolas = file.path(
      pasta_graficos,
      paste0(slug_carteira, "_escolas.png")
    ),
    arquivo_grafico_cenarios = file.path(
      pasta_graficos,
      paste0(slug_carteira, "_cenarios.png")
    )
  ) |>
  arrange(ordem_tipo, nome_exibicao)

# Acrescenta às escolas somente os campos complementares do módulo 19.
fichas_escolas_complementares <- fichas_escolas |>
  select(
    id_escola,
    arquivo_html_escola = arquivo_html,
    observacoes_composicao_limpa,
    status_ficha_escola = status_geracao,
    html_ficha_escola_existe = html_existe
  )

detalhe_fichas <- detalhe_escolas |>
  left_join(fichas_escolas_complementares, by = "id_escola") |>
  arrange(assessora_gerencial, ordem_interna_carga_potencial)

# Aplicação linha a linha explícita, sem depender de `rowwise()` ou de
# funções de contexto do dplyr, para maior compatibilidade entre versões.
detalhe_fichas$alerta_sintetico <- vapply(
  seq_len(nrow(detalhe_fichas)),
  function(i) flag_texto_escola(detalhe_fichas[i, , drop = FALSE]),
  FUN.VALUE = character(1)
)

# -------------------------------------------------------------------
# 8. Diagnósticos gerenciais consolidados
# -------------------------------------------------------------------

resumo_fichas <- base_fichas |>
  select(
    assessora_gerencial,
    nome_exibicao,
    tipo_carteira,
    carteira_nominal,
    numero_escolas,
    matriculas_anos_iniciais_total,
    turmas_anos_iniciais_total,
    carga_potencial_total,
    indice_carga_potencial_medio,
    perfil_carga_descritivo,
    exige_validacao_qualitativa_prioritaria
  )

write_csv(
  resumo_fichas,
  file.path(pasta_execucao, "06_resumo_fichas_carteiras.csv"),
  na = ""
)

alertas_carteiras <- base_fichas |>
  filter(
    !carteira_nominal |
      escolas_painel_incompleto > 0 |
      numero_escolas_interpretacao_cautelosa > 0 |
      numero_escolas_complexidade_administrativa > 0 |
      numero_escolas_sensibilidade_elevada > 0
  ) |>
  transmute(
    assessora_gerencial,
    nome_exibicao,
    tipo_carteira,
    numero_escolas,
    escolas_painel_incompleto = escolas_painel_incompleto,
    escolas_indice_cauteloso = numero_escolas_interpretacao_cautelosa,
    escolas_complexidade_administrativa = numero_escolas_complexidade_administrativa,
    escolas_sensibilidade_elevada = numero_escolas_sensibilidade_elevada,
    perfil_carga_descritivo,
    observacao = case_when(
      !carteira_nominal ~
        "Categoria preservada para rastreabilidade, mas não comparável diretamente às carteiras nominais.",
      TRUE ~
        "A interpretação deve combinar os indicadores quantitativos com informações territoriais e operacionais."
    )
  )

write_csv(
  alertas_carteiras,
  file.path(pasta_execucao, "07_alertas_e_cautelas_carteiras.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 9. Funções de geração dos gráficos
# -------------------------------------------------------------------

tema_grafico <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9),
    legend.position = "bottom",
    axis.title.y = element_blank()
  )

gerar_grafico_carga_relativa <- function(linha_carteira, caminho) {
  dados <- tibble(
    indicador = factor(
      c("Escolas", "Matrículas", "Turmas", "Carga potencial"),
      levels = rev(c("Escolas", "Matrículas", "Turmas", "Carga potencial"))
    ),
    razao_pct = 100 * c(
      linha_carteira$razao_escolas_referencia_nominal[[1]],
      linha_carteira$razao_matriculas_referencia_nominal_ficha[[1]],
      linha_carteira$razao_turmas_referencia_nominal_ficha[[1]],
      linha_carteira$razao_carga_referencia_nominal_ficha[[1]]
    )
  )

  subtitulo <- if (isTRUE(linha_carteira$carteira_nominal[[1]])) {
    paste0("100 = média das ", numero_carteiras_nominais, " carteiras nominais")
  } else {
    "Referência nominal exibida apenas para contextualização; categoria não equivalente"
  }

  grafico <- ggplot(dados, aes(x = razao_pct, y = indicador)) +
    geom_vline(xintercept = 100, linetype = "dashed", linewidth = 0.6) +
    geom_col(width = 0.62, fill = "#315B7D") +
    geom_text(
      aes(label = paste0(formatC(razao_pct, format = "f", digits = 0), "%")),
      hjust = -0.08,
      size = 3.3
    ) +
    scale_x_continuous(
      limits = c(0, max(130, dados$razao_pct, na.rm = TRUE) * 1.18),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = "Carga relativa à referência das carteiras nominais",
      subtitle = subtitulo,
      x = "Proporção da média nominal"
    ) +
    tema_grafico

  ggsave(caminho, grafico, width = 8.2, height = 4.2, dpi = 180)
}

gerar_grafico_dimensoes <- function(linha_carteira, caminho) {
  dados <- tibble(
    dimensao = factor(
      c("Volume", "Estrutural", "Educacional", "Administrativa"),
      levels = rev(c("Volume", "Estrutural", "Educacional", "Administrativa"))
    ),
    carteira = c(
      linha_carteira$dimensao_volume_media[[1]],
      linha_carteira$dimensao_estrutural_media[[1]],
      linha_carteira$dimensao_educacional_media[[1]],
      linha_carteira$dimensao_administrativa_media[[1]]
    ),
    referencia_nominal = c(
      referencias_nominais$media_dimensao_volume[[1]],
      referencias_nominais$media_dimensao_estrutural[[1]],
      referencias_nominais$media_dimensao_educacional[[1]],
      referencias_nominais$media_dimensao_administrativa[[1]]
    )
  ) |>
    pivot_longer(
      cols = c(carteira, referencia_nominal),
      names_to = "grupo",
      values_to = "escore"
    ) |>
    mutate(
      grupo = recode(
        grupo,
        carteira = "Carteira",
        referencia_nominal = "Média nominal"
      )
    )

  grafico <- ggplot(dados, aes(x = escore, y = dimensao, fill = grupo)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.62) +
    geom_text(
      aes(label = formatC(escore, format = "f", digits = 1)),
      position = position_dodge(width = 0.72),
      hjust = -0.08,
      size = 3.0
    ) +
    scale_x_continuous(
      limits = c(0, max(100, dados$escore, na.rm = TRUE) * 1.12),
      expand = expansion(mult = c(0, 0.01))
    ) +
    scale_fill_manual(values = c("Carteira" = "#315B7D", "Média nominal" = "#B8C5CF")) +
    labs(
      title = "Dimensões médias da carteira",
      subtitle = "Escores relativos de 0 a 100; não representam qualidade profissional",
      x = "Escore médio",
      fill = NULL
    ) +
    tema_grafico

  ggsave(caminho, grafico, width = 8.2, height = 4.5, dpi = 180)
}

gerar_grafico_escolas <- function(dados_escolas, caminho) {
  dados <- dados_escolas |>
    arrange(indice_carga_potencial) |>
    mutate(
      nome_grafico = str_wrap(nome_canonico, width = 32),
      nome_grafico = factor(nome_grafico, levels = nome_grafico),
      faixa_curta = case_when(
        str_detect(faixa_indice_carga_potencial, "Faixa 1") ~ "Faixa 1",
        str_detect(faixa_indice_carga_potencial, "Faixa 2") ~ "Faixa 2",
        str_detect(faixa_indice_carga_potencial, "Faixa 3") ~ "Faixa 3",
        str_detect(faixa_indice_carga_potencial, "Faixa 4") ~ "Faixa 4",
        TRUE ~ "Sem faixa"
      )
    )

  grafico <- ggplot(
    dados,
    aes(x = indice_carga_potencial, y = nome_grafico, fill = faixa_curta)
  ) +
    geom_col(width = 0.65) +
    geom_text(
      aes(label = formatC(indice_carga_potencial, format = "f", digits = 1)),
      hjust = -0.08,
      size = 3.0
    ) +
    scale_x_continuous(
      limits = c(0, max(65, dados$indice_carga_potencial, na.rm = TRUE) * 1.18),
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_fill_manual(
      values = c(
        "Faixa 1" = "#DCE7EF",
        "Faixa 2" = "#9CB8CC",
        "Faixa 3" = "#5D88A7",
        "Faixa 4" = "#315B7D",
        "Sem faixa" = "#BDBDBD"
      )
    ) +
    labs(
      title = "Carga potencial das escolas da carteira",
      subtitle = "Ordenação interna por carga potencial; não é ranking de qualidade",
      x = "Índice descritivo de carga potencial",
      fill = NULL
    ) +
    tema_grafico +
    theme(axis.text.y = element_text(size = 8.5))

  altura <- max(4.3, 0.55 * nrow(dados) + 1.8)
  ggsave(caminho, grafico, width = 8.4, height = altura, dpi = 180)
}

gerar_grafico_cenarios <- function(dados_cenarios, caminho) {
  ordem_cenarios <- c(
    "equilibrado",
    "operacional",
    "desafio_educacional",
    "transicao_administrativa"
  )

  dados <- dados_cenarios |>
    mutate(
      cenario = factor(cenario, levels = ordem_cenarios),
      rotulo = recode(
        as.character(cenario),
        equilibrado = "Equilibrado",
        operacional = "Operacional",
        desafio_educacional = "Desafio educacional",
        transicao_administrativa = "Transição administrativa"
      ),
      rotulo = factor(rotulo, levels = rev(c(
        "Equilibrado", "Operacional", "Desafio educacional",
        "Transição administrativa"
      )))
    )

  valor_equilibrado <- dados |>
    filter(cenario == "equilibrado") |>
    pull(carga_potencial_total_cenario)

  linha_referencia <- if (length(valor_equilibrado) == 0) NA_real_ else valor_equilibrado[[1]]

  grafico <- ggplot(dados, aes(x = carga_potencial_total_cenario, y = rotulo)) +
    geom_vline(
      xintercept = linha_referencia,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    geom_col(width = 0.62, fill = "#315B7D") +
    geom_text(
      aes(label = formatC(carga_potencial_total_cenario, format = "f", digits = 1)),
      hjust = -0.08,
      size = 3.2
    ) +
    scale_x_continuous(
      limits = c(
        0,
        max(dados$carga_potencial_total_cenario, na.rm = TRUE) * 1.18
      ),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = "Sensibilidade da carga acumulada aos cenários",
      subtitle = "A linha tracejada representa o cenário principal equilibrado",
      x = "Carga potencial acumulada"
    ) +
    tema_grafico

  ggsave(caminho, grafico, width = 8.2, height = 4.2, dpi = 180)
}

# -------------------------------------------------------------------
# 10. Funções para construção das fichas HTML
# -------------------------------------------------------------------

css_fichas <- "
@page { size: A4; margin: 12mm 11mm 13mm 11mm; }
* { box-sizing: border-box; }
body {
  font-family: Arial, Helvetica, sans-serif;
  color: #24313a;
  margin: 0;
  background: #ffffff;
  font-size: 10pt;
  line-height: 1.35;
}
.ficha { page-break-after: always; }
.ficha:last-child { page-break-after: auto; }
.cabecalho {
  border-bottom: 4px solid #315B7D;
  padding-bottom: 7px;
  margin-bottom: 10px;
}
.instituicao { font-size: 8.5pt; color: #586975; text-transform: uppercase; }
h1 { font-size: 20pt; margin: 3px 0 1px 0; color: #1f435d; }
.subtitulo { font-size: 10pt; color: #536674; }
.aviso {
  background: #f4f7f9;
  border-left: 4px solid #7596ad;
  padding: 7px 9px;
  margin: 8px 0 10px 0;
}
.aviso-atencao {
  background: #fff8e8;
  border-left-color: #cc9a2e;
}
.grade-resumo {
  display: grid;
  grid-template-columns: repeat(6, 1fr);
  gap: 6px;
  margin: 8px 0 10px 0;
}
.cartao {
  border: 1px solid #d9e1e6;
  border-radius: 4px;
  padding: 7px;
  min-height: 58px;
  background: #fbfcfd;
}
.cartao .rotulo { color: #667986; font-size: 7.8pt; text-transform: uppercase; }
.cartao .valor { color: #1f435d; font-size: 15pt; font-weight: bold; margin-top: 3px; }
h2 {
  font-size: 12.5pt;
  color: #1f435d;
  border-bottom: 1px solid #cbd7df;
  padding-bottom: 3px;
  margin: 13px 0 7px 0;
}
.grade-graficos {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 8px;
}
.bloco-grafico {
  border: 1px solid #d9e1e6;
  border-radius: 4px;
  padding: 5px;
  break-inside: avoid;
}
.grafico { width: 100%; max-height: 84mm; object-fit: contain; }
table { width: 100%; border-collapse: collapse; margin: 5px 0 10px 0; }
th {
  background: #315B7D;
  color: white;
  font-weight: bold;
  padding: 5px 4px;
  border: 1px solid #315B7D;
  font-size: 8pt;
}
td {
  padding: 4px;
  border: 1px solid #d6dfe5;
  vertical-align: top;
  font-size: 7.8pt;
}
tr:nth-child(even) td { background: #f7f9fa; }
.texto-pequeno { font-size: 8pt; color: #5f707b; }
.nota-metodologica {
  margin-top: 10px;
  padding-top: 6px;
  border-top: 1px solid #cbd7df;
  font-size: 7.8pt;
  color: #586975;
}
.capa {
  min-height: 245mm;
  display: flex;
  flex-direction: column;
  justify-content: center;
  page-break-after: always;
}
.capa h1 { font-size: 28pt; }
.capa .linha { width: 70px; height: 5px; background: #315B7D; margin: 15px 0; }
@media screen and (max-width: 900px) {
  .grade-resumo { grid-template-columns: repeat(2, 1fr); }
  .grade-graficos { grid-template-columns: 1fr; }
}
"

montar_cartao <- function(rotulo, valor) {
  paste0(
    '<div class="cartao"><div class="rotulo">',
    html_escape(rotulo),
    '</div><div class="valor">',
    html_escape(valor),
    '</div></div>'
  )
}

montar_tabela_escolas <- function(dados_escolas) {
  linhas <- vapply(seq_len(nrow(dados_escolas)), function(i) {
    linha <- dados_escolas[i, , drop = FALSE]

    paste0(
      "<tr>",
      "<td>", html_escape(linha$ordem_interna_carga_potencial[[1]]), "</td>",
      "<td><strong>", html_escape(linha$nome_canonico[[1]]), "</strong><br>",
      "<span class='texto-pequeno'>INEP ", html_escape(linha$codigo_inep[[1]]),
      "</span></td>",
      "<td>", html_escape(fmt_num(linha$matriculas_anos_iniciais, 0)), "</td>",
      "<td>", html_escape(fmt_num(linha$indice_carga_potencial, 1)), "</td>",
      "<td>", html_escape(linha$faixa_indice_carga_potencial[[1]]), "</td>",
      "<td>", html_escape(fmt_num(linha$score_dimensao_volume, 1)), "</td>",
      "<td>", html_escape(fmt_num(linha$score_dimensao_estrutural, 1)), "</td>",
      "<td>", html_escape(fmt_num(linha$score_dimensao_educacional, 1)), "</td>",
      "<td>", html_escape(fmt_num(linha$score_dimensao_administrativa, 1)), "</td>",
      "<td>", html_escape(linha$alerta_sintetico[[1]]), "</td>",
      "</tr>"
    )
  }, FUN.VALUE = character(1))

  paste0(
    "<table><thead><tr>",
    "<th>Ordem interna</th>",
    "<th>Escola</th>",
    "<th>Matrículas</th>",
    "<th>Índice</th>",
    "<th>Faixa relativa</th>",
    "<th>Volume</th>",
    "<th>Estrutural</th>",
    "<th>Educacional</th>",
    "<th>Administrativa</th>",
    "<th>Alertas</th>",
    "</tr></thead><tbody>",
    paste(linhas, collapse = ""),
    "</tbody></table>"
  )
}

montar_tabela_cenarios <- function(dados_cenarios) {
  ordem_cenarios <- c(
    "equilibrado", "operacional", "desafio_educacional",
    "transicao_administrativa"
  )

  dados <- dados_cenarios |>
    mutate(cenario = factor(cenario, levels = ordem_cenarios)) |>
    arrange(cenario)

  linhas <- vapply(seq_len(nrow(dados)), function(i) {
    linha <- dados[i, , drop = FALSE]

    nome_cenario <- recode(
      as.character(linha$cenario[[1]]),
      equilibrado = "Equilibrado",
      operacional = "Operacional",
      desafio_educacional = "Desafio educacional",
      transicao_administrativa = "Transição administrativa"
    )

    posicao <- if (is.na(linha$ordem_carga_total_carteiras_nominais[[1]])) {
      "Não aplicável"
    } else {
      paste0(linha$ordem_carga_total_carteiras_nominais[[1]], "ª")
    }

    mudanca <- if (is.na(linha$mudanca_ordem_nominais_para_equilibrado[[1]])) {
      "Não aplicável"
    } else if (linha$mudanca_ordem_nominais_para_equilibrado[[1]] == 0) {
      "Sem mudança"
    } else {
      paste0(
        ifelse(linha$mudanca_ordem_nominais_para_equilibrado[[1]] > 0, "+", ""),
        linha$mudanca_ordem_nominais_para_equilibrado[[1]],
        " posição(ões)"
      )
    }

    paste0(
      "<tr>",
      "<td><strong>", html_escape(nome_cenario), "</strong></td>",
      "<td>", html_escape(fmt_num(linha$carga_potencial_total_cenario, 1)), "</td>",
      "<td>", html_escape(fmt_num(linha$indice_medio_cenario, 1)), "</td>",
      "<td>", html_escape(posicao), "</td>",
      "<td>", html_escape(mudanca), "</td>",
      "<td>", html_escape(linha$faixa_carga_total_cenario_carteiras_nominais[[1]]), "</td>",
      "</tr>"
    )
  }, FUN.VALUE = character(1))

  paste0(
    "<table><thead><tr>",
    "<th>Cenário</th>",
    "<th>Carga acumulada</th>",
    "<th>Índice médio</th>",
    "<th>Posição de carga*</th>",
    "<th>Mudança frente ao equilibrado</th>",
    "<th>Faixa relativa*</th>",
    "</tr></thead><tbody>",
    paste(linhas, collapse = ""),
    "</tbody></table>",
    "<div class='texto-pequeno'>* Posição e faixa são calculadas somente entre as 11 carteiras nominais e descrevem carga potencial, não desempenho profissional.</div>"
  )
}

texto_interpretativo <- function(linha) {
  texto_base <- linha$perfil_carga_descritivo[[1]]

  complementos <- character(0)

  if (linha$numero_escolas_faixa_4[[1]] > 0) {
    complementos <- c(
      complementos,
      paste0(
        linha$numero_escolas_faixa_4[[1]],
        " escola(s) na faixa superior de carga potencial"
      )
    )
  }

  if (linha$numero_escolas_interpretacao_cautelosa[[1]] > 0) {
    complementos <- c(
      complementos,
      paste0(
        linha$numero_escolas_interpretacao_cautelosa[[1]],
        " escola(s) com interpretação cautelosa do índice"
      )
    )
  }

  if (linha$numero_escolas_complexidade_administrativa[[1]] > 0) {
    complementos <- c(
      complementos,
      paste0(
        linha$numero_escolas_complexidade_administrativa[[1]],
        " escola(s) com complexidade administrativa"
      )
    )
  }

  if (linha$numero_escolas_sensibilidade_elevada[[1]] > 0) {
    complementos <- c(
      complementos,
      paste0(
        linha$numero_escolas_sensibilidade_elevada[[1]],
        " escola(s) altamente sensíveis aos pesos"
      )
    )
  }

  if (length(complementos) == 0) {
    paste0(texto_base, ".")
  } else {
    paste0(texto_base, ". Pontos de atenção: ", paste(complementos, collapse = "; "), ".")
  }
}

construir_corpo_ficha <- function(
  linha,
  dados_escolas,
  dados_cenarios,
  caminho_grafico_carga,
  caminho_grafico_dimensoes,
  caminho_grafico_escolas,
  caminho_grafico_cenarios
) {
  aviso_tipo <- if (isTRUE(linha$carteira_nominal[[1]])) {
    paste0(
      "Carteira nominal. As comparações usam como referência a média das ",
      numero_carteiras_nominais, " " ,
      "carteiras nominais e devem ser complementadas por informações qualitativas, ",
      "territoriais e operacionais."
    )
  } else {
    paste0(
      "Categoria administrativa preservada para rastreabilidade. Não deve ser ",
      "comparada diretamente às carteiras nominais nem interpretada como carteira ",
      "regular de uma assessora."
    )
  }

  classe_aviso <- if (isTRUE(linha$carteira_nominal[[1]])) {
    "aviso"
  } else {
    "aviso aviso-atencao"
  }

  cartoes <- paste0(
    montar_cartao("Escolas", fmt_num(linha$numero_escolas, 0)),
    montar_cartao("Matrículas", fmt_num(linha$matriculas_anos_iniciais_total, 0)),
    montar_cartao("Turmas", fmt_num(linha$turmas_anos_iniciais_total, 0)),
    montar_cartao("Carga total", fmt_num(linha$carga_potencial_total, 1)),
    montar_cartao("Índice médio", fmt_num(linha$indice_carga_potencial_medio, 1)),
    montar_cartao("Carga da rede", fmt_pct(linha$participacao_carga_potencial_rede_pct, 1))
  )

  tabela_contexto <- paste0(
    "<table><tbody>",
    "<tr><th>Indicador</th><th>Valor observado</th><th>Leitura</th></tr>",
    "<tr><td>Participação nas escolas da rede</td><td>",
    html_escape(fmt_pct(linha$participacao_escolas_rede_pct, 1)),
    "</td><td>Parcela das 56 escolas vinculada à categoria</td></tr>",
    "<tr><td>Participação nas matrículas</td><td>",
    html_escape(fmt_pct(linha$participacao_matriculas_rede_pct, 1)),
    "</td><td>Parcela das matrículas dos anos iniciais da rede analisada</td></tr>",
    "<tr><td>Participação nas turmas</td><td>",
    html_escape(fmt_pct(linha$participacao_turmas_rede_pct, 1)),
    "</td><td>Parcela das turmas dos anos iniciais da rede analisada</td></tr>",
    "<tr><td>Carga em relação à média nominal</td><td>",
    html_escape(fmt_ratio(linha$razao_carga_referencia_nominal_ficha, 2)),
    "</td><td>", html_escape(linha$faixa_carga_total_entre_carteiras_nominais[[1]]), "</td></tr>",
    "<tr><td>Intensidade média em relação à referência</td><td>",
    html_escape(fmt_ratio(linha$razao_indice_medio_referencia_nominal, 2)),
    "</td><td>", html_escape(linha$faixa_indice_medio_entre_carteiras_nominais[[1]]), "</td></tr>",
    "<tr><td>Maior escola na carga da carteira</td><td>",
    html_escape(linha$nome_escola_maior_indice[[1]]),
    "</td><td>Índice ", html_escape(fmt_num(linha$maior_indice_escola, 1)), "</td></tr>",
    "<tr><td>Concentração na maior escola</td><td>",
    html_escape(fmt_pct(linha$participacao_maior_escola_carga_total_pct, 1)),
    "</td><td>Parcela da carga total da carteira concentrada na unidade de maior índice</td></tr>",
    "<tr><td>Concentração nas duas maiores</td><td>",
    html_escape(fmt_pct(linha$participacao_duas_maiores_escolas_carga_total_pct, 1)),
    "</td><td>Deve ser interpretada junto ao número de escolas da carteira</td></tr>",
    "</tbody></table>"
  )

  tabela_alertas <- paste0(
    "<table><tbody>",
    "<tr><th>Condição</th><th>Escolas</th></tr>",
    "<tr><td>Faixa superior de carga potencial</td><td>",
    html_escape(fmt_num(linha$numero_escolas_faixa_4, 0)), "</td></tr>",
    "<tr><td>Faixas intermediária superior ou superior</td><td>",
    html_escape(fmt_num(linha$numero_escolas_faixas_3_4, 0)), "</td></tr>",
    "<tr><td>Complexidade administrativa</td><td>",
    html_escape(fmt_num(linha$numero_escolas_complexidade_administrativa, 0)), "</td></tr>",
    "<tr><td>Interpretação cautelosa do índice</td><td>",
    html_escape(fmt_num(linha$numero_escolas_interpretacao_cautelosa, 0)), "</td></tr>",
    "<tr><td>Sensibilidade elevada aos pesos</td><td>",
    html_escape(fmt_num(linha$numero_escolas_sensibilidade_elevada, 0)), "</td></tr>",
    "<tr><td>Painel 2025–2026 incompleto</td><td>",
    html_escape(fmt_num(linha$escolas_painel_incompleto, 0)), "</td></tr>",
    "<tr><td>Participação de 2026 abaixo de 80%</td><td>",
    html_escape(fmt_num(linha$numero_escolas_participacao_2026_abaixo_80, 0)), "</td></tr>",
    "</tbody></table>"
  )

  paste0(
    '<section class="ficha">',
    '<div class="cabecalho">',
    '<div class="instituicao">UEF-SMED-PMPA · Estudo descritivo do assessoramento escolar</div>',
    '<h1>', html_escape(linha$nome_exibicao[[1]]), '</h1>',
    '<div class="subtitulo">', html_escape(linha$tipo_carteira[[1]]),
    ' · Ficha gerencial da carteira</div>',
    '</div>',
    '<div class="', classe_aviso, '">', html_escape(aviso_tipo), '</div>',
    '<div class="grade-resumo">', cartoes, '</div>',
    '<div class="aviso"><strong>Síntese descritiva:</strong> ',
    html_escape(texto_interpretativo(linha)), '</div>',
    '<h2>Volume, intensidade e concentração</h2>',
    tabela_contexto,
    '<div class="grade-graficos">',
    '<div class="bloco-grafico"><img class="grafico" src="',
    html_escape(caminho_grafico_carga), '" alt="Carga relativa"></div>',
    '<div class="bloco-grafico"><img class="grafico" src="',
    html_escape(caminho_grafico_dimensoes), '" alt="Dimensões da carteira"></div>',
    '</div>',
    '<h2>Composição das escolas</h2>',
    '<div class="grade-graficos">',
    '<div class="bloco-grafico"><img class="grafico" src="',
    html_escape(caminho_grafico_escolas), '" alt="Carga das escolas"></div>',
    '<div>', tabela_alertas, '</div>',
    '</div>',
    montar_tabela_escolas(dados_escolas),
    '<h2>Sensibilidade a cenários alternativos</h2>',
    '<div class="grade-graficos">',
    '<div class="bloco-grafico"><img class="grafico" src="',
    html_escape(caminho_grafico_cenarios), '" alt="Cenários de pesos"></div>',
    '<div>', montar_tabela_cenarios(dados_cenarios), '</div>',
    '</div>',
    '<div class="nota-metodologica"><strong>Nota metodológica.</strong> ',
    'A carga potencial acumulada combina quantidade e composição das escolas. ',
    'O índice é relativo ao universo analisado, não representa horas efetivas de trabalho, ',
    'não mede qualidade profissional e não autoriza redistribuição automática. ',
    'Resultados educacionais são observacionais e não devem ser atribuídos às assessoras. ',
    'Qualquer decisão gerencial requer validação qualitativa, territorial e operacional.</div>',
    '</section>'
  )
}

montar_documento_html <- function(titulo, corpo) {
  paste0(
    '<!DOCTYPE html><html lang="pt-BR"><head>',
    '<meta charset="UTF-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1.0">',
    '<title>', html_escape(titulo), '</title>',
    '<style>', css_fichas, '</style>',
    '</head><body>', corpo, '</body></html>'
  )
}

# -------------------------------------------------------------------
# 11. Geração das fichas individuais e dos gráficos
# -------------------------------------------------------------------

resultados_geracao <- vector("list", nrow(base_fichas))
corpos_compilado <- character(0)

for (i in seq_len(nrow(base_fichas))) {
  linha <- base_fichas[i, , drop = FALSE]
  categoria <- linha$assessora_gerencial[[1]]

  dados_escolas <- detalhe_fichas |>
    filter(assessora_gerencial == categoria) |>
    arrange(ordem_interna_carga_potencial)

  dados_cenarios <- cenarios_carteiras |>
    filter(assessora_gerencial == categoria)

  status <- "gerada"
  mensagem_erro <- NA_character_

  tryCatch(
    {
      gerar_grafico_carga_relativa(
        linha,
        linha$arquivo_grafico_carga[[1]]
      )
      gerar_grafico_dimensoes(
        linha,
        linha$arquivo_grafico_dimensoes[[1]]
      )
      gerar_grafico_escolas(
        dados_escolas,
        linha$arquivo_grafico_escolas[[1]]
      )
      gerar_grafico_cenarios(
        dados_cenarios,
        linha$arquivo_grafico_cenarios[[1]]
      )

      corpo_individual <- construir_corpo_ficha(
        linha,
        dados_escolas,
        dados_cenarios,
        file.path("..", "graficos", basename(linha$arquivo_grafico_carga[[1]])),
        file.path("..", "graficos", basename(linha$arquivo_grafico_dimensoes[[1]])),
        file.path("..", "graficos", basename(linha$arquivo_grafico_escolas[[1]])),
        file.path("..", "graficos", basename(linha$arquivo_grafico_cenarios[[1]]))
      )

      html_individual <- montar_documento_html(
        paste0("Ficha da carteira — ", linha$nome_exibicao[[1]]),
        corpo_individual
      )

      writeLines(
        enc2utf8(html_individual),
        linha$arquivo_html[[1]],
        useBytes = TRUE
      )

      corpo_compilado <- construir_corpo_ficha(
        linha,
        dados_escolas,
        dados_cenarios,
        file.path("graficos", basename(linha$arquivo_grafico_carga[[1]])),
        file.path("graficos", basename(linha$arquivo_grafico_dimensoes[[1]])),
        file.path("graficos", basename(linha$arquivo_grafico_escolas[[1]])),
        file.path("graficos", basename(linha$arquivo_grafico_cenarios[[1]]))
      )

      corpos_compilado <- c(corpos_compilado, corpo_compilado)
    },
    error = function(e) {
      status <<- "erro"
      mensagem_erro <<- conditionMessage(e)
    }
  )

  resultados_geracao[[i]] <- tibble(
    assessora_gerencial = categoria,
    nome_exibicao = linha$nome_exibicao[[1]],
    tipo_carteira = linha$tipo_carteira[[1]],
    arquivo_html = linha$arquivo_html[[1]],
    arquivo_grafico_carga = linha$arquivo_grafico_carga[[1]],
    arquivo_grafico_dimensoes = linha$arquivo_grafico_dimensoes[[1]],
    arquivo_grafico_escolas = linha$arquivo_grafico_escolas[[1]],
    arquivo_grafico_cenarios = linha$arquivo_grafico_cenarios[[1]],
    status_geracao = status,
    mensagem_erro = mensagem_erro,
    html_existe = file.exists(linha$arquivo_html[[1]]),
    grafico_carga_existe = file.exists(linha$arquivo_grafico_carga[[1]]),
    grafico_dimensoes_existe = file.exists(linha$arquivo_grafico_dimensoes[[1]]),
    grafico_escolas_existe = file.exists(linha$arquivo_grafico_escolas[[1]]),
    grafico_cenarios_existe = file.exists(linha$arquivo_grafico_cenarios[[1]])
  )
}

manifesto_fichas <- bind_rows(resultados_geracao)

write_csv(
  manifesto_fichas,
  file.path(pasta_execucao, "08_manifesto_fichas_individuais.csv"),
  na = ""
)

fichas_nao_geradas <- manifesto_fichas |>
  filter(
    status_geracao != "gerada" |
      !html_existe |
      !grafico_carga_existe |
      !grafico_dimensoes_existe |
      !grafico_escolas_existe |
      !grafico_cenarios_existe
  )

write_csv(
  fichas_nao_geradas,
  file.path(pasta_execucao, "09_fichas_nao_geradas.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 12. Documento compilado
# -------------------------------------------------------------------

capa <- paste0(
  '<section class="capa">',
  '<div class="instituicao">Unidade de Ensino Fundamental · Secretaria Municipal de Educação de Porto Alegre</div>',
  '<h1>Fichas gerenciais das carteiras de assessoramento</h1>',
  '<div class="linha"></div>',
  '<div class="subtitulo">Diagnóstico descritivo da rede e da distribuição da carga potencial</div>',
  '<p class="texto-pequeno" style="margin-top:25px; max-width:150mm;">',
  'Documento de apoio à gestão. As fichas descrevem carteiras administrativas e ',
  'não constituem avaliação das assessoras. O índice de carga potencial é relativo, ',
  'observacional e deve ser validado com informações qualitativas, territoriais e operacionais.',
  '</p>',
  '<p class="texto-pequeno">Execução: ', html_escape(id_execucao), '</p>',
  '</section>'
)

html_compilado <- montar_documento_html(
  "Fichas gerenciais das carteiras de assessoramento",
  paste0(capa, paste(corpos_compilado, collapse = ""))
)

writeLines(
  enc2utf8(html_compilado),
  arquivo_html_compilado,
  useBytes = TRUE
)

# -------------------------------------------------------------------
# 13. Conversão opcional do compilado para PDF
# -------------------------------------------------------------------

status_pdf <- tibble(
  tentativa_realizada = FALSE,
  pdf_gerado = FALSE,
  arquivo_pdf = arquivo_pdf_compilado,
  navegador = NA_character_,
  mensagem = "Conversão para PDF não solicitada."
)

localizar_navegador <- function() {
  candidatos_sys <- c(
    Sys.which("google-chrome"),
    Sys.which("chrome"),
    Sys.which("chromium"),
    Sys.which("chromium-browser"),
    Sys.which("msedge")
  )

  candidatos_windows <- c(
    file.path(Sys.getenv("PROGRAMFILES"), "Google", "Chrome", "Application", "chrome.exe"),
    file.path(Sys.getenv("PROGRAMFILES(X86)"), "Google", "Chrome", "Application", "chrome.exe"),
    file.path(Sys.getenv("LOCALAPPDATA"), "Google", "Chrome", "Application", "chrome.exe"),
    file.path(Sys.getenv("PROGRAMFILES"), "Microsoft", "Edge", "Application", "msedge.exe"),
    file.path(Sys.getenv("PROGRAMFILES(X86)"), "Microsoft", "Edge", "Application", "msedge.exe")
  )

  candidatos <- unique(c(candidatos_sys, candidatos_windows))
  candidatos <- candidatos[nzchar(candidatos) & file.exists(candidatos)]

  if (length(candidatos) == 0) NA_character_ else candidatos[[1]]
}

if (isTRUE(TENTAR_GERAR_PDF_COMPILADO)) {
  status_pdf$tentativa_realizada <- TRUE

  if (!requireNamespace("pagedown", quietly = TRUE)) {
    status_pdf$mensagem <- paste0(
      "O pacote `pagedown` não está instalado. O HTML compilado foi gerado e pode ",
      "ser aberto no navegador e impresso manualmente em PDF."
    )
  } else {
    navegador <- localizar_navegador()
    status_pdf$navegador <- navegador

    if (is.na(navegador)) {
      status_pdf$mensagem <- paste0(
        "Nenhum Chrome, Chromium ou Edge compatível foi localizado. O HTML ",
        "compilado pode ser aberto no navegador e impresso manualmente em PDF."
      )
    } else {
      tryCatch(
        {
          pagedown::chrome_print(
            input = arquivo_html_compilado,
            output = arquivo_pdf_compilado,
            browser = navegador,
            wait = 2
          )

          status_pdf$pdf_gerado <- file.exists(arquivo_pdf_compilado) &&
            file.info(arquivo_pdf_compilado)$size > 0

          status_pdf$mensagem <- ifelse(
            status_pdf$pdf_gerado,
            "PDF compilado gerado com sucesso.",
            "A conversão terminou sem criar um PDF válido."
          )
        },
        error = function(e) {
          status_pdf$mensagem <<- paste0(
            "Falha não crítica na conversão para PDF: ",
            conditionMessage(e),
            ". O HTML compilado permanece disponível."
          )
        }
      )
    }
  }
}

write_csv(
  status_pdf,
  file.path(pasta_execucao, "10_status_conversao_pdf.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 14. Base final das fichas de carteira
# -------------------------------------------------------------------

base_fichas_saida <- base_fichas |>
  select(
    assessora_gerencial,
    nome_exibicao,
    tipo_carteira,
    carteira_nominal,
    numero_escolas,
    matriculas_anos_iniciais_total,
    turmas_anos_iniciais_total,
    series_comparaveis_total,
    escolas_painel_completo,
    escolas_painel_incompleto,
    taxa_participacao_agregada_2026,
    carga_potencial_total,
    indice_carga_potencial_medio,
    indice_carga_potencial_mediano,
    indice_carga_potencial_minimo,
    indice_carga_potencial_maximo,
    dimensao_volume_media,
    dimensao_estrutural_media,
    dimensao_educacional_media,
    dimensao_administrativa_media,
    numero_escolas_faixa_1,
    numero_escolas_faixa_2,
    numero_escolas_faixa_3,
    numero_escolas_faixa_4,
    numero_escolas_complexidade_administrativa,
    numero_escolas_interpretacao_cautelosa,
    numero_escolas_sensibilidade_elevada,
    numero_escolas_alerta_composicao,
    numero_escolas_participacao_2026_abaixo_80,
    participacao_maior_escola_carga_total_pct,
    participacao_duas_maiores_escolas_carga_total_pct,
    nome_escola_maior_indice,
    maior_indice_escola,
    participacao_escolas_rede_pct,
    participacao_matriculas_rede_pct,
    participacao_turmas_rede_pct,
    participacao_carga_potencial_rede_pct,
    razao_escolas_referencia_nominal,
    razao_matriculas_referencia_nominal_ficha,
    razao_turmas_referencia_nominal_ficha,
    razao_carga_referencia_nominal_ficha,
    razao_indice_medio_referencia_nominal,
    faixa_carga_total_entre_carteiras_nominais,
    faixa_indice_medio_entre_carteiras_nominais,
    perfil_carga_descritivo,
    exige_validacao_qualitativa_prioritaria,
    arquivo_html,
    arquivo_grafico_carga,
    arquivo_grafico_dimensoes,
    arquivo_grafico_escolas,
    arquivo_grafico_cenarios
  ) |>
  left_join(
    manifesto_fichas |>
      select(
        assessora_gerencial,
        status_geracao,
        mensagem_erro,
        html_existe,
        grafico_carga_existe,
        grafico_dimensoes_existe,
        grafico_escolas_existe,
        grafico_cenarios_existe
      ),
    by = "assessora_gerencial"
  )

# -------------------------------------------------------------------
# 15. Dicionário da base das fichas
# -------------------------------------------------------------------

descricoes <- c(
  assessora_gerencial = "Identificador administrativo da carteira ou categoria de vínculo.",
  nome_exibicao = "Nome formatado da carteira para apresentação.",
  tipo_carteira = "Natureza da categoria: carteira nominal, agrupamento residual ou sem vinculação.",
  carteira_nominal = "Indica se a categoria integra o conjunto comparável das 11 carteiras nominais.",
  numero_escolas = "Número de escolas vinculadas à categoria.",
  matriculas_anos_iniciais_total = "Soma das matrículas dos anos iniciais nas escolas da carteira.",
  turmas_anos_iniciais_total = "Soma das turmas dos anos iniciais nas escolas da carteira.",
  carga_potencial_total = "Soma dos índices descritivos de carga potencial das escolas da carteira.",
  indice_carga_potencial_medio = "Média do índice de carga potencial das escolas da carteira.",
  perfil_carga_descritivo = "Síntese automática do perfil de volume e intensidade da carteira.",
  exige_validacao_qualitativa_prioritaria = "Flag para carteiras com situações que exigem leitura qualitativa prioritária.",
  arquivo_html = "Caminho da ficha HTML individual da carteira.",
  status_geracao = "Resultado da tentativa de geração da ficha individual."
)

dicionario <- tibble(
  variavel = names(base_fichas_saida),
  classe = vapply(
    base_fichas_saida,
    function(x) paste(class(x), collapse = " | "),
    character(1)
  )
) |>
  mutate(
    descricao = vapply(variavel, function(v) {
      descricao <- unname(descricoes[v])

      if (length(descricao) == 0 || is.na(descricao[[1]]) || !nzchar(descricao[[1]])) {
        paste0(
          "Variável ",
          str_replace_all(v, "_", " "),
          ". Consulte o módulo 18 ou 20 para a definição operacional."
        )
      } else {
        descricao[[1]]
      }
    }, FUN.VALUE = character(1))
  )

# -------------------------------------------------------------------
# 16. Validações finais
# -------------------------------------------------------------------

soma_escolas_base <- sum(base_fichas_saida$numero_escolas, na.rm = TRUE)
soma_escolas_detalhe <- nrow(detalhe_fichas)
soma_carga_base <- sum(base_fichas_saida$carga_potencial_total, na.rm = TRUE)
soma_carga_detalhe <- sum(detalhe_fichas$indice_carga_potencial, na.rm = TRUE)

validacao_final <- tibble(
  teste = c(
    "Categorias únicas na base das fichas",
    "Número de categorias preservado",
    "Número de escolas preservado",
    "Carga potencial total recomposta",
    "Quatro cenários por categoria",
    "Uma ficha HTML por categoria",
    "Quatro gráficos por categoria",
    "HTML compilado criado",
    "Índices das escolas entre 0 e 100",
    "Participações de carga das escolas entre 0 e 100",
    "Categorias não nominais identificadas explicitamente"
  ),
  status = c(
    n_distinct(base_fichas_saida$assessora_gerencial) == nrow(base_fichas_saida),
    nrow(base_fichas_saida) == nrow(analise_carteiras),
    soma_escolas_base == soma_escolas_detalhe,
    isTRUE(all.equal(soma_carga_base, soma_carga_detalhe, tolerance = 1e-8)),
    all(cobertura_insumos$numero_cenarios == 4),
    sum(manifesto_fichas$html_existe) == nrow(analise_carteiras),
    sum(
      manifesto_fichas$grafico_carga_existe &
        manifesto_fichas$grafico_dimensoes_existe &
        manifesto_fichas$grafico_escolas_existe &
        manifesto_fichas$grafico_cenarios_existe
    ) == nrow(analise_carteiras),
    file.exists(arquivo_html_compilado) && file.info(arquivo_html_compilado)$size > 0,
    all(
      detalhe_fichas$indice_carga_potencial >= 0 &
        detalhe_fichas$indice_carga_potencial <= 100,
      na.rm = TRUE
    ),
    all(
      detalhe_fichas$participacao_indice_na_carteira_pct >= 0 &
        detalhe_fichas$participacao_indice_na_carteira_pct <= 100,
      na.rm = TRUE
    ),
    all(
      base_fichas_saida$carteira_nominal |
        base_fichas_saida$tipo_carteira %in%
          c("Agrupamento residual", "Sem vinculação informada")
    )
  ),
  valor_observado = as.character(c(
    n_distinct(base_fichas_saida$assessora_gerencial),
    nrow(base_fichas_saida),
    soma_escolas_base,
    formatC(soma_carga_base, format = "f", digits = 8),
    paste(sort(unique(cobertura_insumos$numero_cenarios)), collapse = " | "),
    sum(manifesto_fichas$html_existe),
    sum(
      manifesto_fichas$grafico_carga_existe &
        manifesto_fichas$grafico_dimensoes_existe &
        manifesto_fichas$grafico_escolas_existe &
        manifesto_fichas$grafico_cenarios_existe
    ),
    ifelse(file.exists(arquivo_html_compilado), file.info(arquivo_html_compilado)$size, 0),
    paste0(
      formatC(min(detalhe_fichas$indice_carga_potencial, na.rm = TRUE), format = "f", digits = 2),
      " a ",
      formatC(max(detalhe_fichas$indice_carga_potencial, na.rm = TRUE), format = "f", digits = 2)
    ),
    paste0(
      formatC(min(detalhe_fichas$participacao_indice_na_carteira_pct, na.rm = TRUE), format = "f", digits = 2),
      " a ",
      formatC(max(detalhe_fichas$participacao_indice_na_carteira_pct, na.rm = TRUE), format = "f", digits = 2)
    ),
    paste(sort(unique(base_fichas_saida$tipo_carteira)), collapse = " | ")
  )),
  valor_esperado = c(
    as.character(nrow(base_fichas_saida)),
    as.character(nrow(analise_carteiras)),
    as.character(nrow(detalhe_fichas)),
    formatC(soma_carga_detalhe, format = "f", digits = 8),
    "4",
    as.character(nrow(analise_carteiras)),
    as.character(nrow(analise_carteiras)),
    "> 0 bytes",
    "0 a 100",
    "0 a 100",
    "Carteira nominal | Agrupamento residual | Sem vinculação informada"
  ),
  criticidade = "Erro crítico"
)

write_csv(
  validacao_final,
  file.path(pasta_execucao, "11_validacao_final.csv"),
  na = ""
)

erros_criticos <- validacao_final |>
  filter(!status)

if (nrow(erros_criticos) > 0) {
  stop(
    "A exportação foi interrompida por erro crítico. Consulte: ",
    file.path(pasta_execucao, "11_validacao_final.csv")
  )
}

# -------------------------------------------------------------------
# 17. Exportação dos produtos canônicos
# -------------------------------------------------------------------

for (caminho in arquivos_saida) {
  arquivar_arquivo_existente(caminho)
}

write_csv(base_fichas_saida, arquivos_saida[["base_fichas_csv"]], na = "")
saveRDS(base_fichas_saida, arquivos_saida[["base_fichas_rds"]])
write_csv(dicionario, arquivos_saida[["dicionario_csv"]], na = "")

estrutura_saida <- bind_rows(
  tibble(
    base = "base_fichas_carteiras",
    coluna = names(base_fichas_saida),
    classe = vapply(base_fichas_saida, function(x) paste(class(x), collapse = " | "), character(1)),
    valores_nao_ausentes = vapply(base_fichas_saida, function(x) sum(!is.na(x)), numeric(1)),
    valores_ausentes = vapply(base_fichas_saida, function(x) sum(is.na(x)), numeric(1)),
    valores_distintos = vapply(base_fichas_saida, function(x) n_distinct(x, na.rm = TRUE), numeric(1))
  )
)

write_csv(
  estrutura_saida,
  file.path(pasta_execucao, "12_estrutura_base_fichas_carteiras.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 18. Manifesto dos produtos e informações da sessão
# -------------------------------------------------------------------

produtos <- c(
  arquivos_saida,
  html_compilado = arquivo_html_compilado,
  pdf_compilado = arquivo_pdf_compilado
)

manifesto_produtos <- tibble(
  produto = names(produtos),
  arquivo = unname(produtos),
  existe = file.exists(unname(produtos)),
  tamanho_bytes = vapply(unname(produtos), function(x) {
    if (file.exists(x)) file.info(x)$size else NA_real_
  }, numeric(1)),
  modificado_em = vapply(unname(produtos), function(x) {
    if (file.exists(x)) as.character(file.info(x)$mtime) else NA_character_
  }, character(1)),
  md5 = vapply(unname(produtos), calcular_md5, character(1))
)

write_csv(
  manifesto_produtos,
  file.path(pasta_execucao, "13_manifesto_produtos_modulo_20.csv"),
  na = ""
)

capture.output(
  sessionInfo(),
  file = file.path(pasta_execucao, "14_session_info.txt")
)

# -------------------------------------------------------------------
# 19. Resumo da execução
# -------------------------------------------------------------------

resumo_execucao <- c(
  paste0("Execução: ", id_execucao),
  paste0("Categorias administrativas: ", nrow(base_fichas_saida)),
  paste0("Carteiras nominais: ", sum(base_fichas_saida$carteira_nominal)),
  paste0(
    "Categorias não nominais: ",
    sum(!base_fichas_saida$carteira_nominal)
  ),
  paste0("Escolas representadas: ", sum(base_fichas_saida$numero_escolas)),
  paste0("Fichas HTML individuais geradas: ", sum(manifesto_fichas$html_existe)),
  paste0(
    "Gráficos gerados: ",
    sum(manifesto_fichas$grafico_carga_existe) +
      sum(manifesto_fichas$grafico_dimensoes_existe) +
      sum(manifesto_fichas$grafico_escolas_existe) +
      sum(manifesto_fichas$grafico_cenarios_existe)
  ),
  paste0("HTML compilado: ", arquivo_html_compilado),
  paste0("PDF compilado gerado: ", ifelse(status_pdf$pdf_gerado, "Sim", "Não")),
  paste0(
    "Carteiras/categorias com validação qualitativa prioritária: ",
    sum(base_fichas_saida$exige_validacao_qualitativa_prioritaria)
  ),
  paste0("Fichas com erro: ", nrow(fichas_nao_geradas)),
  paste0("Erros críticos: ", nrow(erros_criticos)),
  "",
  "Observações metodológicas:",
  "- As fichas descrevem carteiras administrativas, não desempenho das assessoras.",
  "- A carga potencial acumulada combina quantidade e composição das escolas.",
  "- Faixas e posições são relativas e não constituem ranking de qualidade.",
  "- Resultados educacionais não devem ser atribuídos às assessoras.",
  "- Agrupamentos não nominais são preservados para rastreabilidade, mas não são comparáveis às carteiras regulares.",
  "- Redistribuições exigem validação qualitativa, territorial e operacional."
)

writeLines(
  enc2utf8(resumo_execucao),
  file.path(pasta_execucao, "15_resumo_execucao.txt"),
  useBytes = TRUE
)

message("Módulo 20 concluído com sucesso.")
message("Fichas individuais: ", pasta_html)
message("HTML compilado: ", arquivo_html_compilado)
message("Base final: ", arquivos_saida[["base_fichas_csv"]])
