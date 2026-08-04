# ===================================================================
# 19_gerar_fichas_escolas.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# ===================================================================
#
# OBJETIVO ANALÍTICO
#
# Produzir fichas descritivas individuais das escolas acompanhadas pelo
# programa de assessoramento da UEF-SMED-PMPA. Cada ficha reúne, em uma
# apresentação gerencial padronizada:
#
#   1. identificação e vínculo administrativo;
#   2. contexto estrutural de 2024;
#   3. volume da escola e características da oferta;
#   4. cobertura, participação e resultados observados em 2025 e 2026;
#   5. quatro dimensões do índice descritivo de carga potencial;
#   6. alertas de composição, cobertura e interpretação;
#   7. resultados detalhados por ano escolar.
#
# As fichas NÃO avaliam a qualidade da escola ou da assessora. Resultados
# educacionais são observacionais e não devem ser interpretados como efeito
# causal do assessoramento. O índice representa carga potencial relativa e
# não constitui ranking de qualidade nem regra automática de redistribuição.
#
# ENTRADAS
#
# dados_finais/perfil_escola_gerencial.rds ou .csv
# dados_finais/perfil_escola_serie_compacto.rds ou .csv
# dados_finais/indice_carga_potencial_escola.rds ou .csv
# dados_finais/carteira_escola_detalhe.rds ou .csv
#
# PRODUTOS PRINCIPAIS
#
# dados_finais/base_fichas_escolas.csv e .rds
# resultados/fichas_escolas/execucao_<data_hora>/html/*.html
# resultados/fichas_escolas/execucao_<data_hora>/graficos/*.png
# resultados/fichas_escolas/execucao_<data_hora>/fichas_escolas_compiladas.html
# resultados/fichas_escolas/execucao_<data_hora>/fichas_escolas_compiladas.pdf
#   (opcional, quando pagedown e navegador compatível estiverem disponíveis)
# documentacao/fichas_escolas/dicionario_base_fichas_escolas.csv
# documentacao/fichas_escolas/execucao_<data_hora>/...
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
pasta_resultados <- here("resultados", "fichas_escolas")
pasta_resultados_execucao <- here(
  "resultados", "fichas_escolas", paste0("execucao_", id_execucao)
)
pasta_html <- file.path(pasta_resultados_execucao, "html")
pasta_graficos <- file.path(pasta_resultados_execucao, "graficos")

pasta_documentacao <- here("documentacao", "fichas_escolas")
pasta_execucao <- here(
  "documentacao", "fichas_escolas", paste0("execucao_", id_execucao)
)
pasta_historico <- here(
  "dados_finais", "historico", "fichas_escolas",
  paste0("execucao_", id_execucao)
)

walk(
  c(
    pasta_dados_finais,
    pasta_resultados,
    pasta_resultados_execucao,
    pasta_html,
    pasta_graficos,
    pasta_documentacao,
    pasta_execucao,
    pasta_historico
  ),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# -------------------------------------------------------------------
# 2. Arquivos de entrada e saída
# -------------------------------------------------------------------

arquivos_entrada_rds <- c(
  perfil_escola = here("dados_finais", "perfil_escola_gerencial.rds"),
  perfil_serie = here("dados_finais", "perfil_escola_serie_compacto.rds"),
  indice_escola = here("dados_finais", "indice_carga_potencial_escola.rds"),
  detalhe_carteira = here("dados_finais", "carteira_escola_detalhe.rds")
)

arquivos_entrada_csv <- c(
  perfil_escola = here("dados_finais", "perfil_escola_gerencial.csv"),
  perfil_serie = here("dados_finais", "perfil_escola_serie_compacto.csv"),
  indice_escola = here("dados_finais", "indice_carga_potencial_escola.csv"),
  detalhe_carteira = here("dados_finais", "carteira_escola_detalhe.csv")
)

arquivos_saida <- c(
  base_fichas_csv = here("dados_finais", "base_fichas_escolas.csv"),
  base_fichas_rds = here("dados_finais", "base_fichas_escolas.rds"),
  dicionario_csv = here(
    "documentacao", "fichas_escolas", "dicionario_base_fichas_escolas.csv"
  )
)

arquivo_html_compilado <- file.path(
  pasta_resultados_execucao,
  "fichas_escolas_compiladas.html"
)

arquivo_pdf_compilado <- file.path(
  pasta_resultados_execucao,
  "fichas_escolas_compiladas.pdf"
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

fmt_delta <- function(x, digits = 1, sufixo = "") {
  if (length(x) == 0 || is.na(x[[1]])) {
    return("Não comparável")
  }

  valor <- as.numeric(x[[1]])
  sinal <- ifelse(valor > 0, "+", "")
  paste0(sinal, fmt_num(valor, digits), sufixo)
}

sim_nao <- function(x) {
  if (length(x) == 0 || is.na(x[[1]])) {
    return("Não informado")
  }

  ifelse(isTRUE(x[[1]]), "Sim", "Não")
}

limpar_observacoes_composicao <- function(x) {
  if (is.na(x) || !nzchar(str_trim(x))) {
    return(NA_character_)
  }

  partes <- str_split(x, "\\s*\\|\\s*")[[1]] |>
    str_trim() |>
    discard(~ !nzchar(.x)) |>
    unique()

  substantivas <- partes[
    partes != "Sem alerta principal de composição"
  ]

  if (length(substantivas) > 0) {
    paste(substantivas, collapse = " | ")
  } else {
    "Sem alerta principal de composição"
  }
}

texto_ou_na <- function(x, padrao = "Não informado") {
  if (length(x) == 0 || is.na(x[[1]]) || !nzchar(str_trim(as.character(x[[1]])))) {
    return(padrao)
  }

  as.character(x[[1]])
}

# -------------------------------------------------------------------
# 4. Leitura das bases e manifesto dos insumos
# -------------------------------------------------------------------

fontes_lidas <- map(
  names(arquivos_entrada_rds),
  ler_base_preferindo_rds
)

names(fontes_lidas) <- names(arquivos_entrada_rds)

perfil_escola <- fontes_lidas$perfil_escola$dados
perfil_serie <- fontes_lidas$perfil_serie$dados
indice_escola <- fontes_lidas$indice_escola$dados
detalhe_carteira <- fontes_lidas$detalhe_carteira$dados

manifesto_entrada <- imap_dfr(
  fontes_lidas,
  function(objeto, fonte) {
    caminho <- objeto$origem

    tibble(
      fonte = fonte,
      formato_utilizado = objeto$formato,
      caminho = caminho,
      tamanho_bytes = file.info(caminho)$size,
      modificado_em = as.character(file.info(caminho)$mtime),
      md5 = calcular_md5(caminho),
      numero_linhas = nrow(objeto$dados),
      numero_colunas = ncol(objeto$dados)
    )
  }
)

write_csv(
  manifesto_entrada,
  file.path(pasta_execucao, "01_manifesto_arquivos_entrada.csv"),
  na = ""
)

estrutura_bases_entrada <- imap_dfr(
  fontes_lidas,
  function(objeto, fonte) {
    tibble(
      fonte = fonte,
      ordem = seq_along(objeto$dados),
      variavel = names(objeto$dados),
      classe = map_chr(objeto$dados, ~ paste(class(.x), collapse = "; ")),
      numero_na = map_int(objeto$dados, ~ sum(is.na(.x))),
      percentual_na = 100 * numero_na / nrow(objeto$dados)
    )
  }
)

write_csv(
  estrutura_bases_entrada,
  file.path(pasta_execucao, "02_estrutura_bases_entrada.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 5. Validação de colunas e chaves
# -------------------------------------------------------------------

colunas_obrigatorias <- list(
  perfil_escola = c(
    "id_escola", "codigo_inep", "nome_canonico", "assessora_gerencial",
    "grupo_administrativo_2024_final", "tipo_vinculo_rede_final",
    "status_rede_2025_final", "observacao_administrativa_final",
    "municipalizada_apos_2024", "escola_nova_recente",
    "matriculas_anos_iniciais", "turmas_anos_iniciais",
    "docentes_anos_iniciais", "alunos_por_turma_anos_iniciais",
    "alunos_por_docente_anos_iniciais", "porte_anos_iniciais",
    "numero_etapas_amplas_ofertadas", "oferta_educacao_infantil",
    "oferta_anos_finais", "oferta_eja", "oferta_educacao_especial",
    "pct_matriculas_anos_iniciais_integral",
    "pct_matriculas_educacao_especial",
    "pct_matriculas_transporte_publico", "indice_infraestrutura_basica",
    "numero_itens_infraestrutura_presentes", "quantidade_sala_utilizada",
    "possui_espaco_leitura", "possui_recurso_acessibilidade",
    "laboratorio_ciencias", "laboratorio_informatica", "quadra_esportes",
    "internet_aprendizagem", "internet_alunos",
    "sala_atendimento_especial", "numero_series_resultado_2025",
    "numero_series_resultado_2026", "numero_series_comparaveis",
    "painel_completo_cinco_series", "previstos_total_2025",
    "avaliados_total_2025", "taxa_participacao_escola_2025",
    "proficiencia_multisserie_ponderada_2025", "previstos_total_2026",
    "avaliados_total_2026", "taxa_participacao_escola_2026",
    "proficiencia_multisserie_ponderada_2026",
    "pct_defasagem_multisserie_ponderado_2026",
    "pct_intermediario_multisserie_ponderado_2026",
    "pct_adequado_multisserie_ponderado_2026",
    "delta_participacao_escola",
    "delta_proficiencia_multisserie_ponderada",
    "possui_alerta_composicao", "observacoes_composicao"
  ),
  perfil_serie = c(
    "id_escola", "codigo_inep", "nome_canonico", "assessora_gerencial",
    "ano_escolar", "componente", "painel_resultado_balanceado",
    "previstos_2025", "avaliados_2025", "taxa_participacao_2025",
    "proficiencia_media_2025", "pct_defasagem_2025",
    "pct_intermediario_2025", "pct_adequado_2025", "previstos_2026",
    "avaliados_2026", "taxa_participacao_2026",
    "proficiencia_media_2026", "pct_defasagem_2026",
    "pct_intermediario_2026", "pct_adequado_2026",
    "delta_participacao", "delta_proficiencia", "alerta_composicao_serie",
    "observacao_composicao"
  ),
  indice_escola = c(
    "id_escola", "score_dimensao_volume", "score_dimensao_estrutural",
    "score_dimensao_educacional", "score_dimensao_administrativa",
    "indice_carga_potencial", "faixa_indice_carga_potencial",
    "qualidade_evidencia_educacional",
    "interpretacao_educacional_cautelosa",
    "interpretacao_indice_cautelosa"
  ),
  detalhe_carteira = c(
    "id_escola", "tipo_carteira", "carteira_nominal",
    "participacao_indice_na_carteira_pct", "sensibilidade_faixa",
    "possui_complexidade_administrativa", "painel_incompleto",
    "caso_para_leitura_detalhada"
  )
)

bases_para_validacao <- list(
  perfil_escola = perfil_escola,
  perfil_serie = perfil_serie,
  indice_escola = indice_escola,
  detalhe_carteira = detalhe_carteira
)

validacao_colunas <- map_dfr(
  seq_along(colunas_obrigatorias),
  function(i) {
    fonte <- names(colunas_obrigatorias)[[i]]
    colunas <- colunas_obrigatorias[[i]]
    dados <- bases_para_validacao[[fonte]]

    tibble(
      fonte = fonte,
      coluna_obrigatoria = colunas,
      presente = colunas %in% names(dados)
    )
  }
)

write_csv(
  validacao_colunas,
  file.path(pasta_execucao, "03_validacao_colunas_obrigatorias.csv"),
  na = ""
)

if (any(!validacao_colunas$presente)) {
  ausentes <- validacao_colunas |>
    filter(!presente) |>
    transmute(texto = paste0(fonte, ": ", coluna_obrigatoria)) |>
    pull(texto)

  stop(
    "Foram encontradas colunas obrigatórias ausentes: ",
    paste(ausentes, collapse = "; ")
  )
}

duplicidades_chaves <- bind_rows(
  perfil_escola |>
    count(id_escola, name = "numero_registros") |>
    filter(numero_registros > 1) |>
    mutate(fonte = "perfil_escola", chave = as.character(id_escola)),
  indice_escola |>
    count(id_escola, name = "numero_registros") |>
    filter(numero_registros > 1) |>
    mutate(fonte = "indice_escola", chave = as.character(id_escola)),
  detalhe_carteira |>
    count(id_escola, name = "numero_registros") |>
    filter(numero_registros > 1) |>
    mutate(fonte = "detalhe_carteira", chave = as.character(id_escola)),
  perfil_serie |>
    count(id_escola, ano_escolar, componente, name = "numero_registros") |>
    filter(numero_registros > 1) |>
    mutate(
      fonte = "perfil_serie",
      chave = paste(id_escola, ano_escolar, componente, sep = " | ")
    )
) |>
  select(fonte, chave, numero_registros)

write_csv(
  duplicidades_chaves,
  file.path(pasta_execucao, "04_duplicidades_chaves.csv"),
  na = ""
)

if (nrow(duplicidades_chaves) > 0) {
  stop("Há duplicidades nas chaves das bases de entrada. Consulte 04_duplicidades_chaves.csv.")
}

conjuntos_escolas <- list(
  perfil_escola = sort(unique(perfil_escola$id_escola)),
  indice_escola = sort(unique(indice_escola$id_escola)),
  detalhe_carteira = sort(unique(detalhe_carteira$id_escola)),
  perfil_serie = sort(unique(perfil_serie$id_escola))
)

ids_referencia <- conjuntos_escolas$perfil_escola

cobertura_insumos <- tibble(id_escola = ids_referencia) |>
  left_join(
    perfil_escola |>
      select(id_escola, codigo_inep, nome_canonico, assessora_gerencial),
    by = "id_escola"
  ) |>
  mutate(
    presente_perfil_escola = id_escola %in% conjuntos_escolas$perfil_escola,
    presente_perfil_serie = id_escola %in% conjuntos_escolas$perfil_serie,
    presente_indice_escola = id_escola %in% conjuntos_escolas$indice_escola,
    presente_detalhe_carteira = id_escola %in% conjuntos_escolas$detalhe_carteira,
    cobertura_completa_insumos = presente_perfil_escola &
      presente_perfil_serie & presente_indice_escola &
      presente_detalhe_carteira
  )

write_csv(
  cobertura_insumos,
  file.path(pasta_execucao, "05_cobertura_insumos_por_escola.csv"),
  na = ""
)

if (any(!cobertura_insumos$cobertura_completa_insumos)) {
  stop(
    "Nem todas as escolas estão presentes nos quatro insumos. Consulte ",
    "05_cobertura_insumos_por_escola.csv."
  )
}

if (!all(map_lgl(conjuntos_escolas, ~ identical(.x, ids_referencia)))) {
  stop("Os conjuntos de escolas diferem entre as bases de entrada.")
}

if (any(!perfil_serie$ano_escolar %in% 1:5, na.rm = TRUE)) {
  stop("Foram encontrados anos escolares fora do intervalo de 1 a 5.")
}

if (n_distinct(perfil_serie$componente, na.rm = TRUE) != 1) {
  stop("A base por série contém mais de um componente curricular.")
}

# -------------------------------------------------------------------
# 6. Consolidação da base das fichas
# -------------------------------------------------------------------

indice_selecionado <- indice_escola |>
  select(
    id_escola,
    score_dimensao_volume,
    score_dimensao_estrutural,
    score_dimensao_educacional,
    score_dimensao_administrativa,
    indice_carga_potencial,
    faixa_indice_carga_potencial,
    qualidade_evidencia_educacional,
    interpretacao_educacional_cautelosa,
    interpretacao_indice_cautelosa
  )

detalhe_selecionado <- detalhe_carteira |>
  select(
    id_escola,
    tipo_carteira,
    carteira_nominal,
    participacao_indice_na_carteira_pct,
    sensibilidade_faixa,
    possui_complexidade_administrativa,
    painel_incompleto,
    caso_para_leitura_detalhada
  )

base_fichas <- perfil_escola |>
  left_join(indice_selecionado, by = "id_escola") |>
  left_join(detalhe_selecionado, by = "id_escola") |>
  mutate(
    observacoes_composicao_limpa = map_chr(
      observacoes_composicao,
      limpar_observacoes_composicao
    ),
    slug_escola = slugificar(nome_canonico),
    arquivo_html = file.path(
      pasta_html,
      paste0(str_pad(id_escola, 3, pad = "0"), "_", slug_escola, ".html")
    ),
    arquivo_grafico_dimensoes = file.path(
      pasta_graficos,
      paste0(str_pad(id_escola, 3, pad = "0"), "_", slug_escola, "_dimensoes.png")
    ),
    arquivo_grafico_participacao = file.path(
      pasta_graficos,
      paste0(str_pad(id_escola, 3, pad = "0"), "_", slug_escola, "_participacao.png")
    ),
    arquivo_grafico_proficiencia = file.path(
      pasta_graficos,
      paste0(str_pad(id_escola, 3, pad = "0"), "_", slug_escola, "_proficiencia.png")
    )
  )

if (anyDuplicated(base_fichas$slug_escola) > 0) {
  base_fichas <- base_fichas |>
    mutate(slug_escola = paste0(slug_escola, "-", str_pad(id_escola, 3, pad = "0"))) |>
    mutate(
      arquivo_html = file.path(
        pasta_html,
        paste0(str_pad(id_escola, 3, pad = "0"), "_", slug_escola, ".html")
      ),
      arquivo_grafico_dimensoes = file.path(
        pasta_graficos,
        paste0(str_pad(id_escola, 3, pad = "0"), "_", slug_escola, "_dimensoes.png")
      ),
      arquivo_grafico_participacao = file.path(
        pasta_graficos,
        paste0(str_pad(id_escola, 3, pad = "0"), "_", slug_escola, "_participacao.png")
      ),
      arquivo_grafico_proficiencia = file.path(
        pasta_graficos,
        paste0(str_pad(id_escola, 3, pad = "0"), "_", slug_escola, "_proficiencia.png")
      )
    )
}

# -------------------------------------------------------------------
# 7. Funções para gráficos
# -------------------------------------------------------------------

tema_ficha <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "#4b5563"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 9),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.margin = margin(8, 12, 8, 8)
    )
}

gerar_grafico_dimensoes <- function(linha_escola, caminho) {
  dados <- tibble(
    dimensao = factor(
      c("Volume", "Estrutural", "Educacional", "Administrativa"),
      levels = rev(c("Volume", "Estrutural", "Educacional", "Administrativa"))
    ),
    escore = c(
      linha_escola$score_dimensao_volume,
      linha_escola$score_dimensao_estrutural,
      linha_escola$score_dimensao_educacional,
      linha_escola$score_dimensao_administrativa
    )
  )

  grafico <- ggplot(dados, aes(x = escore, y = dimensao, fill = dimensao)) +
    geom_col(width = 0.62, show.legend = FALSE) +
    geom_text(
      aes(label = if_else(is.na(escore), "NA", sprintf("%.1f", escore))),
      hjust = -0.12,
      size = 3.5,
      na.rm = TRUE
    ) +
    scale_x_continuous(
      limits = c(0, 108),
      breaks = seq(0, 100, 20),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_fill_manual(
      values = c(
        "Volume" = "#35648f",
        "Estrutural" = "#6b7f3e",
        "Educacional" = "#b16b28",
        "Administrativa" = "#7d5684"
      )
    ) +
    labs(
      title = "Dimensões da carga potencial",
      subtitle = "Escores relativos de 0 a 100; não representam qualidade",
      x = "Escore relativo",
      y = NULL
    ) +
    tema_ficha()

  ggsave(
    caminho,
    grafico,
    width = 7.1,
    height = 3.25,
    dpi = 160,
    bg = "white"
  )
}

gerar_grafico_participacao <- function(dados_serie, caminho) {
  dados <- dados_serie |>
    select(ano_escolar, taxa_participacao_2025, taxa_participacao_2026) |>
    pivot_longer(
      cols = starts_with("taxa_participacao_"),
      names_to = "ano_avaliacao",
      values_to = "participacao"
    ) |>
    mutate(
      ano_avaliacao = recode(
        ano_avaliacao,
        taxa_participacao_2025 = "2025",
        taxa_participacao_2026 = "2026"
      ),
      ano_escolar = factor(
        ano_escolar,
        levels = 1:5,
        labels = paste0(1:5, "º")
      )
    )

  if (all(is.na(dados$participacao))) {
    grafico <- ggplot() +
      annotate("text", x = 1, y = 1, label = "Participação não disponível") +
      xlim(0, 2) + ylim(0, 2) + theme_void()
  } else {
    grafico <- ggplot(
      dados,
      aes(
        x = ano_escolar,
        y = participacao,
        group = ano_avaliacao,
        color = ano_avaliacao
      )
    ) +
      geom_hline(yintercept = c(70, 80, 90), linetype = "dotted", color = "#c7cbd1") +
      geom_line(linewidth = 0.8, na.rm = TRUE) +
      geom_point(size = 2.5, na.rm = TRUE) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      scale_color_manual(values = c("2025" = "#7f8c99", "2026" = "#245b85")) +
      labs(
        title = "Participação por ano escolar",
        subtitle = "Percentual de estudantes avaliados entre os previstos",
        x = "Ano escolar",
        y = "Participação (%)"
      ) +
      tema_ficha()
  }

  ggsave(
    caminho,
    grafico,
    width = 7.1,
    height = 3.25,
    dpi = 160,
    bg = "white"
  )
}

gerar_grafico_proficiencia <- function(dados_serie, caminho) {
  dados <- dados_serie |>
    select(ano_escolar, proficiencia_media_2025, proficiencia_media_2026) |>
    pivot_longer(
      cols = starts_with("proficiencia_media_"),
      names_to = "ano_avaliacao",
      values_to = "proficiencia"
    ) |>
    mutate(
      ano_avaliacao = recode(
        ano_avaliacao,
        proficiencia_media_2025 = "2025",
        proficiencia_media_2026 = "2026"
      ),
      ano_escolar = factor(
        ano_escolar,
        levels = 1:5,
        labels = paste0(1:5, "º")
      )
    )

  if (all(is.na(dados$proficiencia))) {
    grafico <- ggplot() +
      annotate("text", x = 1, y = 1, label = "Proficiência não disponível") +
      xlim(0, 2) + ylim(0, 2) + theme_void()
  } else {
    limites <- range(dados$proficiencia, na.rm = TRUE)
    margem <- max(5, diff(limites) * 0.12)

    grafico <- ggplot(
      dados,
      aes(
        x = ano_escolar,
        y = proficiencia,
        group = ano_avaliacao,
        color = ano_avaliacao
      )
    ) +
      geom_line(linewidth = 0.8, na.rm = TRUE) +
      geom_point(size = 2.5, na.rm = TRUE) +
      scale_y_continuous(
        limits = c(max(0, limites[[1]] - margem), limites[[2]] + margem)
      ) +
      scale_color_manual(values = c("2025" = "#7f8c99", "2026" = "#b45f32")) +
      labs(
        title = "Proficiência média por ano escolar",
        subtitle = "Comparações devem considerar participação e composição dos avaliados",
        x = "Ano escolar",
        y = "Proficiência média"
      ) +
      tema_ficha()
  }

  ggsave(
    caminho,
    grafico,
    width = 7.1,
    height = 3.25,
    dpi = 160,
    bg = "white"
  )
}

# -------------------------------------------------------------------
# 8. Funções para composição das fichas em HTML
# -------------------------------------------------------------------

css_fichas <- "
@page { size: A4; margin: 11mm 10mm 12mm 10mm; }
* { box-sizing: border-box; }
body {
  margin: 0;
  background: #eef1f4;
  color: #1f2933;
  font-family: Arial, Helvetica, sans-serif;
  font-size: 10.5pt;
  line-height: 1.35;
}
.ficha {
  width: 190mm;
  min-height: 270mm;
  margin: 8mm auto;
  padding: 10mm;
  background: white;
  box-shadow: 0 2px 12px rgba(0,0,0,.10);
  page-break-after: always;
  break-after: page;
}
.ficha:last-child { page-break-after: auto; break-after: auto; }
.cabecalho {
  border-bottom: 4px solid #244d70;
  padding-bottom: 5mm;
  margin-bottom: 5mm;
}
.instituicao {
  color: #52616f;
  font-size: 8.5pt;
  letter-spacing: .04em;
  text-transform: uppercase;
}
h1 { margin: 2mm 0 1mm 0; color: #173953; font-size: 20pt; line-height: 1.12; }
.subtitulo { color: #52616f; font-size: 10pt; }
.grade-cards {
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 3mm;
  margin: 4mm 0;
}
.card {
  border: 1px solid #d9e0e6;
  border-radius: 5px;
  padding: 3.5mm;
  min-height: 23mm;
  background: #fafbfc;
}
.card .rotulo { color: #687786; font-size: 8pt; text-transform: uppercase; letter-spacing: .03em; }
.card .valor { color: #173953; font-size: 16pt; font-weight: bold; margin-top: 1.5mm; }
.card .nota { color: #687786; font-size: 7.7pt; margin-top: 1mm; }
.grid-2 { display: grid; grid-template-columns: 1fr 1fr; gap: 5mm; }
.grid-3 { display: grid; grid-template-columns: repeat(3, 1fr); gap: 4mm; }
.bloco {
  border: 1px solid #d9e0e6;
  border-radius: 5px;
  padding: 4mm;
  margin: 4mm 0;
  break-inside: avoid;
}
.bloco h2 {
  margin: 0 0 3mm 0;
  color: #244d70;
  font-size: 12.5pt;
  border-bottom: 1px solid #d9e0e6;
  padding-bottom: 1.5mm;
}
.tabela { width: 100%; border-collapse: collapse; font-size: 8.7pt; }
.tabela th { background: #e8eef3; color: #28485f; text-align: left; }
.tabela th, .tabela td { border: 1px solid #d8dee4; padding: 1.8mm 2mm; vertical-align: top; }
.tabela td.num, .tabela th.num { text-align: right; }
.alertas { margin: 0; padding-left: 5mm; }
.alertas li { margin-bottom: 1.5mm; }
.selo {
  display: inline-block;
  padding: 1.2mm 2.5mm;
  border-radius: 10px;
  font-size: 8pt;
  font-weight: bold;
  margin: .5mm 1mm .5mm 0;
}
.selo-neutro { background: #e8eef3; color: #28485f; }
.selo-atencao { background: #fff0d5; color: #845313; }
.selo-cautela { background: #f8dddd; color: #8a2f2f; }
.selo-ok { background: #e1f1e7; color: #326345; }
.grafico { width: 100%; max-height: 82mm; object-fit: contain; }
.nota-metodologica {
  background: #f3f5f7;
  border-left: 4px solid #7b8995;
  padding: 3mm 4mm;
  color: #53616d;
  font-size: 8.5pt;
  margin-top: 5mm;
}
.rodape { color: #73808c; font-size: 7.5pt; margin-top: 4mm; text-align: right; }
@media print {
  body { background: white; }
  .ficha { margin: 0; box-shadow: none; width: auto; min-height: auto; }
}
"

render_selo <- function(texto, classe = "selo-neutro") {
  paste0(
    '<span class="selo ', classe, '">',
    html_escape(texto),
    "</span>"
  )
}

render_alertas <- function(escola) {
  alertas <- character()

  if (isTRUE(escola$interpretacao_indice_cautelosa)) {
    alertas <- c(
      alertas,
      "O índice geral requer interpretação cautelosa devido à cobertura parcial de componentes."
    )
  }

  if (isTRUE(escola$interpretacao_educacional_cautelosa)) {
    alertas <- c(
      alertas,
      paste0(
        "A evidência educacional é limitada: ",
        texto_ou_na(escola$qualidade_evidencia_educacional),
        "."
      )
    )
  }

  if (isTRUE(escola$painel_incompleto)) {
    alertas <- c(
      alertas,
      paste0(
        "Painel temporal incompleto: ",
        fmt_num(escola$numero_series_comparaveis),
        " de 5 séries comparáveis entre 2025 e 2026."
      )
    )
  }

  if (isTRUE(escola$possui_complexidade_administrativa)) {
    alertas <- c(
      alertas,
      "A escola possui situação administrativa especial considerada no índice de carga potencial."
    )
  }

  if (isTRUE(escola$possui_alerta_composicao)) {
    alerta_textual <- texto_ou_na(
      escola$observacoes_composicao_limpa,
      "Foram identificadas mudanças relevantes de participação ou composição."
    )
    alertas <- c(alertas, alerta_textual)
  }

  if (is.na(escola$assessora_gerencial) ||
      escola$assessora_gerencial == "Sem vinculação informada") {
    alertas <- c(
      alertas,
      "O vínculo administrativo de assessoramento não está informado na base."
    )
  }

  if (length(alertas) == 0) {
    return(
      '<p><span class="selo selo-ok">Sem alerta crítico de cobertura</span></p>'
    )
  }

  itens <- paste0("<li>", html_escape(unique(alertas)), "</li>", collapse = "")
  paste0('<ul class="alertas">', itens, "</ul>")
}

render_tabela_contexto <- function(escola) {
  linhas <- tribble(
    ~rotulo, ~valor,
    "Grupo administrativo em 2024", texto_ou_na(escola$grupo_administrativo_2024_final),
    "Tipo de vínculo com a rede", texto_ou_na(escola$tipo_vinculo_rede_final),
    "Situação operacional em 2025", texto_ou_na(escola$status_rede_2025_final),
    "Porte dos anos iniciais", texto_ou_na(escola$porte_anos_iniciais),
    "Etapas amplas ofertadas", fmt_num(escola$numero_etapas_amplas_ofertadas),
    "Oferta de educação infantil", sim_nao(escola$oferta_educacao_infantil),
    "Oferta de anos finais", sim_nao(escola$oferta_anos_finais),
    "Oferta de EJA", sim_nao(escola$oferta_eja),
    "Oferta de educação especial", sim_nao(escola$oferta_educacao_especial),
    "Tempo integral nos anos iniciais", fmt_pct(escola$pct_matriculas_anos_iniciais_integral),
    "Matrículas de educação especial", fmt_pct(escola$pct_matriculas_educacao_especial),
    "Uso de transporte público", fmt_pct(escola$pct_matriculas_transporte_publico)
  )

  corpo <- linhas |>
    mutate(
      linha = paste0(
        "<tr><td>", html_escape(rotulo), "</td><td>",
        html_escape(valor), "</td></tr>"
      )
    ) |>
    pull(linha) |>
    paste(collapse = "")

  paste0(
    '<table class="tabela"><tbody>',
    corpo,
    "</tbody></table>"
  )
}

render_tabela_infraestrutura <- function(escola) {
  linhas <- tribble(
    ~rotulo, ~valor,
    "Índice de infraestrutura básica", fmt_num(escola$indice_infraestrutura_basica, 1),
    "Itens de infraestrutura presentes", fmt_num(escola$numero_itens_infraestrutura_presentes),
    "Salas utilizadas", fmt_num(escola$quantidade_sala_utilizada),
    "Espaço de leitura", sim_nao(escola$possui_espaco_leitura),
    "Recursos de acessibilidade", sim_nao(escola$possui_recurso_acessibilidade),
    "Laboratório de ciências", sim_nao(escola$laboratorio_ciencias),
    "Laboratório de informática", sim_nao(escola$laboratorio_informatica),
    "Quadra de esportes", sim_nao(escola$quadra_esportes),
    "Internet para aprendizagem", sim_nao(escola$internet_aprendizagem),
    "Internet para estudantes", sim_nao(escola$internet_alunos),
    "Sala de atendimento especial", sim_nao(escola$sala_atendimento_especial)
  )

  corpo <- linhas |>
    mutate(
      linha = paste0(
        "<tr><td>", html_escape(rotulo), "</td><td>",
        html_escape(valor), "</td></tr>"
      )
    ) |>
    pull(linha) |>
    paste(collapse = "")

  paste0('<table class="tabela"><tbody>', corpo, "</tbody></table>")
}

render_tabela_series <- function(dados_serie) {
  dados <- dados_serie |>
    arrange(ano_escolar) |>
    mutate(
      ano_escolar_txt = paste0(ano_escolar, "º"),
      participacao_2025_txt = map_chr(taxa_participacao_2025, ~ fmt_pct(.x)),
      participacao_2026_txt = map_chr(taxa_participacao_2026, ~ fmt_pct(.x)),
      delta_participacao_txt = map_chr(
        delta_participacao,
        ~ fmt_delta(.x, 1, " p.p.")
      ),
      proficiencia_2025_txt = map_chr(proficiencia_media_2025, ~ fmt_num(.x, 1)),
      proficiencia_2026_txt = map_chr(proficiencia_media_2026, ~ fmt_num(.x, 1)),
      delta_proficiencia_txt = map_chr(delta_proficiencia, ~ fmt_delta(.x, 1)),
      padroes_2026_txt = pmap_chr(
        list(pct_defasagem_2026, pct_intermediario_2026, pct_adequado_2026),
        function(defasagem, intermediario, adequado) {
          if (all(is.na(c(defasagem, intermediario, adequado)))) {
            return("Não disponível")
          }

          paste0(
            "D: ", fmt_pct(defasagem),
            " · I: ", fmt_pct(intermediario),
            " · A: ", fmt_pct(adequado)
          )
        }
      ),
      alerta_txt = if_else(
        alerta_composicao_serie %in% TRUE,
        "Sim",
        "Não",
        missing = "Não informado"
      )
    )

  corpo <- dados |>
    transmute(
      linha = paste0(
        "<tr>",
        '<td class="num">', html_escape(ano_escolar_txt), "</td>",
        '<td class="num">', html_escape(participacao_2025_txt), "</td>",
        '<td class="num">', html_escape(participacao_2026_txt), "</td>",
        '<td class="num">', html_escape(delta_participacao_txt), "</td>",
        '<td class="num">', html_escape(proficiencia_2025_txt), "</td>",
        '<td class="num">', html_escape(proficiencia_2026_txt), "</td>",
        '<td class="num">', html_escape(delta_proficiencia_txt), "</td>",
        "<td>", html_escape(padroes_2026_txt), "</td>",
        "<td>", html_escape(alerta_txt), "</td>",
        "</tr>"
      )
    ) |>
    pull(linha) |>
    paste(collapse = "")

  paste0(
    '<table class="tabela">',
    "<thead><tr>",
    '<th class="num">Ano</th>',
    '<th class="num">Part. 2025</th>',
    '<th class="num">Part. 2026</th>',
    '<th class="num">Δ part.</th>',
    '<th class="num">Prof. 2025</th>',
    '<th class="num">Prof. 2026</th>',
    '<th class="num">Δ prof.</th>',
    "<th>Padrões 2026</th>",
    "<th>Alerta</th>",
    "</tr></thead><tbody>",
    corpo,
    "</tbody></table>"
  )
}

render_secao_ficha <- function(escola, dados_serie, caminho_dim, caminho_part, caminho_prof) {
  selos <- c(
    render_selo(
      paste0("Carteira: ", texto_ou_na(escola$assessora_gerencial)),
      "selo-neutro"
    ),
    render_selo(
      texto_ou_na(escola$faixa_indice_carga_potencial),
      "selo-atencao"
    )
  )

  if (isTRUE(escola$interpretacao_indice_cautelosa) ||
      isTRUE(escola$interpretacao_educacional_cautelosa)) {
    selos <- c(selos, render_selo("Interpretação cautelosa", "selo-cautela"))
  }

  if (isTRUE(escola$painel_completo_cinco_series)) {
    selos <- c(selos, render_selo("Painel completo", "selo-ok"))
  } else {
    selos <- c(selos, render_selo("Painel incompleto", "selo-cautela"))
  }

  observacao_administrativa <- texto_ou_na(
    escola$observacao_administrativa_final,
    "Sem observação administrativa específica registrada."
  )

  glue::glue(
    '<section class="ficha">',
    '<div class="cabecalho">',
    '<div class="instituicao">UEF-SMED-PMPA · Estudo descritivo do assessoramento escolar</div>',
    '<h1>{html_escape(escola$nome_canonico)}</h1>',
    '<div class="subtitulo">Código INEP: {html_escape(texto_ou_na(escola$codigo_inep))} · ',
    'Ficha descritiva individual</div>',
    '<div style="margin-top:3mm;">{paste(selos, collapse = "")}</div>',
    '</div>',

    '<div class="grade-cards">',
    '<div class="card"><div class="rotulo">Matrículas — anos iniciais</div>',
    '<div class="valor">{fmt_num(escola$matriculas_anos_iniciais)}</div>',
    '<div class="nota">Contexto de 2024</div></div>',
    '<div class="card"><div class="rotulo">Turmas — anos iniciais</div>',
    '<div class="valor">{fmt_num(escola$turmas_anos_iniciais)}</div>',
    '<div class="nota">Contexto de 2024</div></div>',
    '<div class="card"><div class="rotulo">Participação em 2026</div>',
    '<div class="valor">{fmt_pct(escola$taxa_participacao_escola_2026)}</div>',
    '<div class="nota">Δ 2025–2026: {fmt_delta(escola$delta_participacao_escola, 1, " p.p.")}</div></div>',
    '<div class="card"><div class="rotulo">Carga potencial relativa</div>',
    '<div class="valor">{fmt_num(escola$indice_carga_potencial, 1)}</div>',
    '<div class="nota">Escore descritivo de 0 a 100</div></div>',
    '</div>',

    '<div class="grid-2">',
    '<div class="bloco"><h2>Contexto e oferta</h2>{render_tabela_contexto(escola)}</div>',
    '<div class="bloco"><h2>Infraestrutura e recursos</h2>{render_tabela_infraestrutura(escola)}</div>',
    '</div>',

    '<div class="bloco"><h2>Síntese da carga potencial</h2>',
    '<img class="grafico" src="{html_escape(caminho_dim)}" alt="Dimensões da carga potencial">',
    '<div class="grid-3" style="margin-top:2mm;">',
    '<div><strong>Alunos por turma:</strong><br>{fmt_num(escola$alunos_por_turma_anos_iniciais, 1)}</div>',
    '<div><strong>Alunos por docente:</strong><br>{fmt_num(escola$alunos_por_docente_anos_iniciais, 1)}</div>',
    '<div><strong>Participação na carga da carteira:</strong><br>{fmt_pct(escola$participacao_indice_na_carteira_pct)}</div>',
    '</div></div>',

    '<div class="bloco"><h2>Alertas e cautelas de interpretação</h2>{render_alertas(escola)}</div>',

    '<div class="bloco"><h2>Contexto administrativo registrado</h2>',
    '<p>{html_escape(observacao_administrativa)}</p>',
    '<p><strong>Sensibilidade aos pesos do índice:</strong> ',
    '{html_escape(texto_ou_na(escola$sensibilidade_faixa))}.</p>',
    '</div>',

    '<div class="grid-2">',
    '<div class="bloco"><img class="grafico" src="{html_escape(caminho_part)}" ',
    'alt="Participação por ano escolar"></div>',
    '<div class="bloco"><img class="grafico" src="{html_escape(caminho_prof)}" ',
    'alt="Proficiência por ano escolar"></div>',
    '</div>',

    '<div class="bloco"><h2>Resultados observados por ano escolar</h2>',
    '{render_tabela_series(dados_serie)}',
    '<p style="font-size:8pt;color:#687786;margin-top:2mm;">',
    'D = defasagem; I = intermediário; A = adequado. As diferenças entre 2025 e 2026 ',
    'não constituem estimativas de efeito do assessoramento.</p>',
    '</div>',

    '<div class="nota-metodologica"><strong>Nota metodológica.</strong> ',
    'Esta ficha é descritiva e observacional. As características estruturais são ',
    'referentes ao contexto de 2024. O vínculo com a assessora é administrativo. ',
    'Resultados escolares não devem ser atribuídos à assessora, e o índice de carga ',
    'potencial deve ser utilizado como ferramenta de diagnóstico, em conjunto com ',
    'informações qualitativas, territoriais e operacionais.</div>',
    '<div class="rodape">Execução {id_execucao}</div>',
    '</section>'
  )
}

render_documento_html <- function(titulo, secoes, css) {
  paste0(
    '<!DOCTYPE html><html lang="pt-BR"><head>',
    '<meta charset="UTF-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1.0">',
    "<title>", html_escape(titulo), "</title>",
    "<style>", css, "</style>",
    "</head><body>",
    paste(secoes, collapse = "\n"),
    "</body></html>"
  )
}

# -------------------------------------------------------------------
# 9. Geração de gráficos e fichas individuais
# -------------------------------------------------------------------

resultados_geracao <- vector("list", nrow(base_fichas))
secoes_compiladas <- character(nrow(base_fichas))

for (i in seq_len(nrow(base_fichas))) {
  escola <- base_fichas[i, , drop = FALSE]
  dados_serie_escola <- perfil_serie |>
    filter(id_escola == escola$id_escola[[1]]) |>
    arrange(ano_escolar)

  status <- "gerada"
  mensagem_erro <- NA_character_

  tryCatch(
    {
      gerar_grafico_dimensoes(
        escola,
        escola$arquivo_grafico_dimensoes[[1]]
      )
      gerar_grafico_participacao(
        dados_serie_escola,
        escola$arquivo_grafico_participacao[[1]]
      )
      gerar_grafico_proficiencia(
        dados_serie_escola,
        escola$arquivo_grafico_proficiencia[[1]]
      )

      caminhos_individual <- c(
        dimensoes = paste0(
          "../graficos/",
          basename(escola$arquivo_grafico_dimensoes[[1]])
        ),
        participacao = paste0(
          "../graficos/",
          basename(escola$arquivo_grafico_participacao[[1]])
        ),
        proficiencia = paste0(
          "../graficos/",
          basename(escola$arquivo_grafico_proficiencia[[1]])
        )
      )

      secao_individual <- render_secao_ficha(
        escola,
        dados_serie_escola,
        caminhos_individual[["dimensoes"]],
        caminhos_individual[["participacao"]],
        caminhos_individual[["proficiencia"]]
      )

      html_individual <- render_documento_html(
        paste0("Ficha escolar — ", escola$nome_canonico[[1]]),
        secao_individual,
        css_fichas
      )

      writeLines(
        enc2utf8(html_individual),
        escola$arquivo_html[[1]],
        useBytes = TRUE
      )

      caminhos_compilado <- c(
        dimensoes = paste0(
          "graficos/",
          basename(escola$arquivo_grafico_dimensoes[[1]])
        ),
        participacao = paste0(
          "graficos/",
          basename(escola$arquivo_grafico_participacao[[1]])
        ),
        proficiencia = paste0(
          "graficos/",
          basename(escola$arquivo_grafico_proficiencia[[1]])
        )
      )

      secoes_compiladas[[i]] <- render_secao_ficha(
        escola,
        dados_serie_escola,
        caminhos_compilado[["dimensoes"]],
        caminhos_compilado[["participacao"]],
        caminhos_compilado[["proficiencia"]]
      )
    },
    error = function(e) {
      status <<- "erro"
      mensagem_erro <<- conditionMessage(e)
      secoes_compiladas[[i]] <<- ""
    }
  )

  resultados_geracao[[i]] <- tibble(
    id_escola = escola$id_escola[[1]],
    codigo_inep = escola$codigo_inep[[1]],
    nome_canonico = escola$nome_canonico[[1]],
    assessora_gerencial = escola$assessora_gerencial[[1]],
    status_geracao = status,
    mensagem_erro = mensagem_erro,
    arquivo_html = escola$arquivo_html[[1]],
    html_existe = file.exists(escola$arquivo_html[[1]]),
    grafico_dimensoes_existe = file.exists(escola$arquivo_grafico_dimensoes[[1]]),
    grafico_participacao_existe = file.exists(escola$arquivo_grafico_participacao[[1]]),
    grafico_proficiencia_existe = file.exists(escola$arquivo_grafico_proficiencia[[1]])
  )
}

manifesto_fichas <- bind_rows(resultados_geracao)

write_csv(
  manifesto_fichas,
  file.path(pasta_execucao, "07_manifesto_fichas_individuais.csv"),
  na = ""
)

fichas_nao_geradas <- manifesto_fichas |>
  filter(
    status_geracao != "gerada" |
      !html_existe |
      !grafico_dimensoes_existe |
      !grafico_participacao_existe |
      !grafico_proficiencia_existe
  )

write_csv(
  fichas_nao_geradas,
  file.path(pasta_execucao, "06_fichas_nao_geradas.csv"),
  na = ""
)

# Compilado somente com as fichas geradas corretamente.
html_compilado <- render_documento_html(
  "Fichas escolares — UEF-SMED-PMPA",
  secoes_compiladas[nzchar(secoes_compiladas)],
  css_fichas
)

writeLines(
  enc2utf8(html_compilado),
  arquivo_html_compilado,
  useBytes = TRUE
)

# -------------------------------------------------------------------
# 10. Conversão opcional do compilado para PDF
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
  file.path(pasta_execucao, "08_status_conversao_pdf.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 11. Base final das fichas e alertas consolidados
# -------------------------------------------------------------------

base_fichas_saida <- base_fichas |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_gerencial,
    tipo_carteira,
    grupo_administrativo_2024_final,
    tipo_vinculo_rede_final,
    status_rede_2025_final,
    matriculas_anos_iniciais,
    turmas_anos_iniciais,
    numero_series_resultado_2025,
    numero_series_resultado_2026,
    numero_series_comparaveis,
    taxa_participacao_escola_2025,
    taxa_participacao_escola_2026,
    delta_participacao_escola,
    proficiencia_multisserie_ponderada_2025,
    proficiencia_multisserie_ponderada_2026,
    delta_proficiencia_multisserie_ponderada,
    score_dimensao_volume,
    score_dimensao_estrutural,
    score_dimensao_educacional,
    score_dimensao_administrativa,
    indice_carga_potencial,
    faixa_indice_carga_potencial,
    qualidade_evidencia_educacional,
    interpretacao_educacional_cautelosa,
    interpretacao_indice_cautelosa,
    possui_complexidade_administrativa,
    possui_alerta_composicao,
    painel_incompleto,
    sensibilidade_faixa,
    participacao_indice_na_carteira_pct,
    observacoes_composicao_limpa,
    arquivo_html,
    arquivo_grafico_dimensoes,
    arquivo_grafico_participacao,
    arquivo_grafico_proficiencia
  ) |>
  left_join(
    manifesto_fichas |>
      select(
        id_escola,
        status_geracao,
        mensagem_erro,
        html_existe,
        grafico_dimensoes_existe,
        grafico_participacao_existe,
        grafico_proficiencia_existe
      ),
    by = "id_escola"
  )

alertas_fichas <- base_fichas_saida |>
  transmute(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_gerencial,
    painel_incompleto,
    interpretacao_educacional_cautelosa,
    interpretacao_indice_cautelosa,
    possui_complexidade_administrativa,
    possui_alerta_composicao,
    sem_vinculacao_informada = assessora_gerencial == "Sem vinculação informada",
    sensibilidade_faixa,
    observacoes_composicao_limpa
  ) |>
  filter(
    painel_incompleto |
      interpretacao_educacional_cautelosa |
      interpretacao_indice_cautelosa |
      possui_complexidade_administrativa |
      possui_alerta_composicao |
      sem_vinculacao_informada |
      sensibilidade_faixa == "Elevada"
  )

write_csv(
  alertas_fichas,
  file.path(pasta_execucao, "09_alertas_e_cautelas_por_escola.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 12. Validações finais
# -------------------------------------------------------------------

validacao_final <- tribble(
  ~teste, ~resultado, ~valor_observado, ~valor_esperado, ~criticidade,
  "Escolas únicas na base de fichas",
  n_distinct(base_fichas_saida$id_escola) == nrow(base_fichas_saida),
  n_distinct(base_fichas_saida$id_escola), nrow(base_fichas_saida), "Erro",

  "Número de escolas preservado",
  nrow(base_fichas_saida) == nrow(perfil_escola),
  nrow(base_fichas_saida), nrow(perfil_escola), "Erro",

  "Uma ficha HTML por escola",
  sum(manifesto_fichas$html_existe) == nrow(perfil_escola),
  sum(manifesto_fichas$html_existe), nrow(perfil_escola), "Erro",

  "Três gráficos por escola",
  sum(
    manifesto_fichas$grafico_dimensoes_existe &
      manifesto_fichas$grafico_participacao_existe &
      manifesto_fichas$grafico_proficiencia_existe
  ) == nrow(perfil_escola),
  sum(
    manifesto_fichas$grafico_dimensoes_existe &
      manifesto_fichas$grafico_participacao_existe &
      manifesto_fichas$grafico_proficiencia_existe
  ),
  nrow(perfil_escola), "Erro",

  "HTML compilado criado",
  file.exists(arquivo_html_compilado) && file.info(arquivo_html_compilado)$size > 0,
  ifelse(file.exists(arquivo_html_compilado), file.info(arquivo_html_compilado)$size, 0),
  "> 0 bytes", "Erro",

  "Nenhuma ficha com erro de geração",
  nrow(fichas_nao_geradas) == 0,
  nrow(fichas_nao_geradas), 0, "Erro",

  "Cinco linhas por escola na base por série",
  all((perfil_serie |> count(id_escola))$n == 5),
  paste(sort(unique((perfil_serie |> count(id_escola))$n)), collapse = "; "),
  5, "Erro",

  "Índice de carga entre 0 e 100",
  all(base_fichas_saida$indice_carga_potencial >= 0 &
        base_fichas_saida$indice_carga_potencial <= 100,
      na.rm = TRUE),
  paste0(
    fmt_num(min(base_fichas_saida$indice_carga_potencial, na.rm = TRUE), 2),
    " a ",
    fmt_num(max(base_fichas_saida$indice_carga_potencial, na.rm = TRUE), 2)
  ),
  "0 a 100", "Erro",

  "Participações escolares entre 0 e 100",
  all(
    c(
      base_fichas_saida$taxa_participacao_escola_2025,
      base_fichas_saida$taxa_participacao_escola_2026
    ) >= 0 &
      c(
        base_fichas_saida$taxa_participacao_escola_2025,
        base_fichas_saida$taxa_participacao_escola_2026
      ) <= 100,
    na.rm = TRUE
  ),
  "Verificado", "0 a 100", "Erro",

  "PDF tratado como produto opcional",
  TRUE,
  ifelse(status_pdf$pdf_gerado, "Gerado", "Não gerado"),
  "Não bloqueante", "Informativo"
) |>
  mutate(
    status = if_else(resultado, "Aprovado", "Reprovado")
  )

write_csv(
  validacao_final,
  file.path(pasta_execucao, "10_validacao_final.csv"),
  na = ""
)

erros_criticos <- validacao_final |>
  filter(criticidade == "Erro", !resultado)

# -------------------------------------------------------------------
# 13. Dicionário e estrutura da base de saída
# -------------------------------------------------------------------

descricoes_base_fichas <- c(
  id_escola = "Identificador interno estável da escola.",
  codigo_inep = "Código INEP da escola.",
  nome_canonico = "Nome canônico da escola.",
  assessora_gerencial = "Categoria administrativa utilizada para organizar a carteira.",
  tipo_carteira = "Classificação da carteira como nominal, residual ou sem vinculação.",
  indice_carga_potencial = "Escore relativo e descritivo de carga potencial, de 0 a 100.",
  faixa_indice_carga_potencial = "Quartil relativo do índice de carga potencial.",
  arquivo_html = "Caminho do arquivo HTML individual da ficha escolar.",
  status_geracao = "Situação da geração da ficha individual.",
  observacoes_composicao_limpa = "Síntese textual de alertas de composição, sem mensagens contraditórias."
)

dicionario_saida <- tibble(
  ordem = seq_along(base_fichas_saida),
  variavel = names(base_fichas_saida),
  classe = map_chr(base_fichas_saida, ~ paste(class(.x), collapse = "; ")),
  descricao = map_chr(
    names(base_fichas_saida),
    function(variavel) {
      descricao <- unname(descricoes_base_fichas[variavel])

      if (length(descricao) == 0 || is.na(descricao[[1]]) || !nzchar(descricao[[1]])) {
        paste0(
          "Variável ",
          str_replace_all(variavel, "_", " "),
          ". Consulte os módulos 16 a 19 para a definição operacional."
        )
      } else {
        descricao[[1]]
      }
    }
  ),
  numero_na = map_int(base_fichas_saida, ~ sum(is.na(.x))),
  percentual_na = 100 * numero_na / nrow(base_fichas_saida)
)

estrutura_saida <- tibble(
  produto = "base_fichas_escolas",
  numero_linhas = nrow(base_fichas_saida),
  numero_colunas = ncol(base_fichas_saida),
  variavel = names(base_fichas_saida),
  classe = map_chr(base_fichas_saida, ~ paste(class(.x), collapse = "; ")),
  numero_na = map_int(base_fichas_saida, ~ sum(is.na(.x))),
  percentual_na = 100 * numero_na / nrow(base_fichas_saida)
)

write_csv(
  estrutura_saida,
  file.path(pasta_execucao, "11_estrutura_base_fichas.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 14. Exportação dos produtos canônicos
# -------------------------------------------------------------------

if (nrow(erros_criticos) > 0) {
  stop(
    "A exportação canônica foi interrompida porque há erros críticos. Consulte ",
    file.path(pasta_execucao, "10_validacao_final.csv")
  )
}

walk(arquivos_saida, arquivar_arquivo_existente)

write_csv(base_fichas_saida, arquivos_saida[["base_fichas_csv"]], na = "")
saveRDS(base_fichas_saida, arquivos_saida[["base_fichas_rds"]])
write_csv(dicionario_saida, arquivos_saida[["dicionario_csv"]], na = "")

# -------------------------------------------------------------------
# 15. Manifesto final, session info e resumo
# -------------------------------------------------------------------

produtos_principais <- c(
  arquivos_saida,
  html_compilado = arquivo_html_compilado
)

if (isTRUE(status_pdf$pdf_gerado)) {
  produtos_principais <- c(
    produtos_principais,
    pdf_compilado = arquivo_pdf_compilado
  )
}

manifesto_produtos <- enframe(
  produtos_principais,
  name = "produto",
  value = "caminho"
) |>
  mutate(
    existe = file.exists(caminho),
    tamanho_bytes = if_else(
      existe,
      as.numeric(file.info(caminho)$size),
      NA_real_
    ),
    modificado_em = if_else(
      existe,
      as.character(file.info(caminho)$mtime),
      NA_character_
    ),
    md5 = map_chr(caminho, calcular_md5)
  )

write_csv(
  manifesto_produtos,
  file.path(pasta_execucao, "12_manifesto_produtos_modulo_19.csv"),
  na = ""
)

capture.output(
  sessionInfo(),
  file = file.path(pasta_execucao, "13_session_info.txt")
)

resumo_execucao <- c(
  paste0("Execução: ", id_execucao),
  paste0("Escolas nas bases de entrada: ", nrow(perfil_escola)),
  paste0("Fichas HTML individuais geradas: ", sum(manifesto_fichas$html_existe)),
  paste0("Gráficos gerados: ", sum(
    manifesto_fichas$grafico_dimensoes_existe,
    manifesto_fichas$grafico_participacao_existe,
    manifesto_fichas$grafico_proficiencia_existe
  )),
  paste0("HTML compilado: ", arquivo_html_compilado),
  paste0("PDF compilado gerado: ", ifelse(status_pdf$pdf_gerado, "Sim", "Não")),
  paste0("Escolas com painel incompleto: ", sum(base_fichas_saida$painel_incompleto, na.rm = TRUE)),
  paste0("Escolas com interpretação cautelosa do índice: ", sum(
    base_fichas_saida$interpretacao_indice_cautelosa,
    na.rm = TRUE
  )),
  paste0("Escolas com complexidade administrativa: ", sum(
    base_fichas_saida$possui_complexidade_administrativa,
    na.rm = TRUE
  )),
  paste0("Fichas com erro: ", nrow(fichas_nao_geradas)),
  paste0("Erros críticos: ", nrow(erros_criticos)),
  "",
  "Observações metodológicas:",
  "- As fichas são descritivas e observacionais.",
  "- Resultados educacionais não representam efeito causal do assessoramento.",
  "- O índice de carga potencial não representa qualidade da escola ou da assessora.",
  "- As faixas do índice são quartis relativos ao universo analisado.",
  "- Mudanças de participação e composição condicionam comparações temporais.",
  "- A conversão para PDF é opcional; o HTML compilado é sempre o produto principal imprimível."
)

writeLines(
  enc2utf8(resumo_execucao),
  file.path(pasta_execucao, "14_resumo_execucao.txt"),
  useBytes = TRUE
)

message("Módulo 19 concluído com sucesso.")
message("Fichas individuais: ", pasta_html)
message("HTML compilado: ", arquivo_html_compilado)

if (isTRUE(status_pdf$pdf_gerado)) {
  message("PDF compilado: ", arquivo_pdf_compilado)
} else {
  message("PDF não gerado automaticamente. Consulte 08_status_conversao_pdf.csv.")
}
