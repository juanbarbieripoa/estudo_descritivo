# ===================================================================
# 21_tabelas_graficos_relatorio.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# ===================================================================
#
# OBJETIVO
#
# Preparar as tabelas e os gráficos padronizados que alimentarão o
# relatório técnico (módulo 22) e os anexos técnicos (módulo 24), sem
# recalcular produtos analíticos já homologados nos módulos 01 a 20.
#
# O módulo:
#   1. valida a estrutura real dos insumos consolidados;
#   2. inventaria tabelas e gráficos já existentes;
#   3. reutiliza, sem recálculo, gráficos homologados ainda adequados;
#   4. produz apenas as lacunas visuais necessárias ao relatório;
#   5. separa produtos destinados ao corpo e aos anexos;
#   6. registra títulos, notas, fontes, escalas e formatos numéricos;
#   7. gera manifestos, diagnósticos e lista de homologação.
#
# PRINCÍPIOS METODOLÓGICOS
#
# - O estudo é observacional e descritivo; não há identificação causal.
# - Resultados escolares não são atribuídos às assessoras.
# - O índice representa carga potencial relativa, não qualidade.
# - Volume, complexidade estrutural, desafio educacional e complexidade
#   administrativa permanecem visíveis separadamente.
# - Faixas e posições são relativas e não constituem ranking de qualidade.
# - Mudanças de participação e composição condicionam comparações temporais.
# - Redistribuições exigem validação qualitativa, territorial e operacional.
# - "Outras" e "Sem vinculação informada" são preservadas nos anexos,
#   mas não entram nas referências comparativas das carteiras nominais.
#
# ARQUITETURA DE SAÍDA
#
# resultados/relatorio/
#   corpo/tabelas/     tabelas sintéticas para o relatório técnico
#   corpo/graficos/    gráficos principais, novos ou reutilizados
#   anexos/tabelas/    bases detalhadas para os anexos técnicos
#   historico/         cópias dos produtos canônicos antes de nova execução
#
# documentacao/relatorio/execucao_<data_hora>/
#   inventários, matriz de produtos, validações, manifestos e sessão.
#
# O módulo usa caminhos canônicos estáveis para permitir que os módulos
# 22 e 24 consumam os produtos sem depender do carimbo de execução.
# ===================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(forcats)
  library(ggplot2)
  library(purrr)
  library(readr)
  library(scales)
  library(stringr)
  library(tibble)
  library(tidyr)
})

# -------------------------------------------------------------------
# 1. Configuração, parâmetros e diretórios
# -------------------------------------------------------------------

options(
  scipen = 999,
  OutDec = ","
)

carimbo_execucao <- format(Sys.time(), "%Y%m%d_%H%M%S")

diretorio_projeto <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

if (!file.exists(file.path(diretorio_projeto, "estudo_descritivo.Rproj"))) {
  stop(
    "Execute o módulo 21 a partir da raiz do projeto estudo_descritivo."
  )
}

caminho_projeto <- function(...) {
  file.path(diretorio_projeto, ...)
}

dir_relatorio <- caminho_projeto("resultados", "relatorio")
dir_corpo_tabelas <- file.path(dir_relatorio, "corpo", "tabelas")
dir_corpo_graficos <- file.path(dir_relatorio, "corpo", "graficos")
dir_anexos_tabelas <- file.path(dir_relatorio, "anexos", "tabelas")
dir_historico <- file.path(dir_relatorio, "historico")

dir_documentacao <- caminho_projeto(
  "documentacao",
  "relatorio",
  paste0("execucao_", carimbo_execucao)
)

walk(
  c(
    dir_corpo_tabelas,
    dir_corpo_graficos,
    dir_anexos_tabelas,
    dir_historico,
    dir_documentacao
  ),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

parametros_esperados <- tibble(
  parametro = c(
    "escolas",
    "observacoes_escola_serie",
    "observacoes_comparaveis",
    "carteiras_nominais",
    "categorias_administrativas",
    "series_escolares",
    "componente_curricular"
  ),
  valor_esperado = c(
    "56",
    "280",
    "265",
    "11",
    "13",
    "1 a 5",
    "Língua Portuguesa"
  ),
  justificativa = c(
    "Universo escolar homologado nos módulos 15 a 20.",
    "Uma observação potencial por escola e por ano escolar.",
    "Painel principal com resultado disponível em 2025 e 2026.",
    "Referência gerencial para comparações entre carteiras nominais.",
    "Inclui 11 carteiras, agrupamento residual e escola sem vínculo.",
    "Escopo dos anos iniciais do Ensino Fundamental.",
    "Componente comparável no painel atual."
  )
)

write_csv(
  parametros_esperados,
  file.path(dir_documentacao, "01_parametros_escopo_homologado.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 2. Padrão visual e funções auxiliares
# -------------------------------------------------------------------

cores_relatorio <- c(
  azul_escuro = "#17324D",
  azul = "#2F6B8A",
  azul_claro = "#79A9C2",
  verde = "#2A9D8F",
  amarelo = "#E9C46A",
  laranja = "#F4A261",
  vermelho = "#C44E52",
  roxo = "#7D5684",
  cinza_escuro = "#4A5560",
  cinza = "#87929D",
  cinza_claro = "#DCE3E8",
  fundo = "#F7F9FA"
)

padrao_formatacao <- tribble(
  ~tipo_variavel, ~casas_decimais, ~marca_decimal, ~marca_milhar, ~sufixo, ~regra,
  "Contagem", 0L, ",", ".", "", "Inteiro com separador de milhar.",
  "Percentual", 1L, ",", ".", "%", "Uma casa decimal; escala de 0 a 100.",
  "Pontos percentuais", 1L, ",", ".", " p.p.", "Uma casa decimal e sinal quando variação.",
  "Proficiência", 1L, ",", ".", "", "Uma casa decimal.",
  "Índice", 1L, ",", ".", "", "Uma casa decimal; escala relativa de 0 a 100.",
  "Razão", 2L, ",", ".", "", "Duas casas decimais."
)

write_csv(
  padrao_formatacao,
  file.path(dir_documentacao, "02_padrao_visual_formatacao.csv"),
  na = ""
)

tema_relatorio <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title = element_text(
        color = cores_relatorio[["azul_escuro"]],
        face = "bold",
        size = rel(1.30),
        margin = margin(b = 7)
      ),
      plot.subtitle = element_text(
        color = cores_relatorio[["cinza_escuro"]],
        size = rel(1.00),
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        color = cores_relatorio[["cinza_escuro"]],
        size = rel(0.78),
        hjust = 0,
        margin = margin(t = 12)
      ),
      axis.title = element_text(
        color = cores_relatorio[["cinza_escuro"]],
        face = "bold"
      ),
      axis.text = element_text(color = cores_relatorio[["cinza_escuro"]]),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(
        color = cores_relatorio[["cinza_claro"]],
        linewidth = 0.35
      ),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(14, 18, 14, 14)
    )
}

rotulo_numero <- label_number(
  accuracy = 1,
  decimal.mark = ",",
  big.mark = "."
)

rotulo_numero_1 <- label_number(
  accuracy = 0.1,
  decimal.mark = ",",
  big.mark = "."
)

rotulo_percentual <- label_number(
  accuracy = 0.1,
  decimal.mark = ",",
  big.mark = ".",
  suffix = "%"
)

rotulo_indice <- label_number(
  accuracy = 0.1,
  decimal.mark = ",",
  big.mark = "."
)

arredondar <- function(x, casas = 1) {
  ifelse(is.na(x), NA_real_, round(as.numeric(x), casas))
}

normalizar_caminho <- function(x) {
  str_replace_all(normalizePath(x, winslash = "/", mustWork = FALSE), "\\\\", "/")
}

caminho_relativo <- function(x) {
  raiz <- paste0(normalizar_caminho(diretorio_projeto), "/")
  str_remove(normalizar_caminho(x), fixed(raiz))
}

hash_md5 <- function(x) {
  if (!file.exists(x)) {
    return(NA_character_)
  }
  unname(tools::md5sum(x)[[1]])
}

ler_csv <- function(x) {
  read_csv(
    x,
    show_col_types = FALSE,
    na = c("", "NA", "NaN")
  )
}

validar_colunas <- function(dados, colunas, nome_base) {
  ausentes <- setdiff(colunas, names(dados))
  tibble(
    base = nome_base,
    coluna = colunas,
    presente = colunas %in% names(dados),
    status = if_else(colunas %in% names(dados), "OK", "AUSENTE")
  ) |>
    mutate(
      detalhe = if_else(
        coluna %in% ausentes,
        paste0("Coluna obrigatória ausente em ", nome_base, "."),
        "Coluna disponível."
      )
    )
}

estrutura_base <- function(dados, nome_base) {
  tibble(
    base = nome_base,
    variavel = names(dados),
    classe = map_chr(dados, ~ paste(class(.x), collapse = " | ")),
    numero_linhas = nrow(dados),
    numero_colunas = ncol(dados),
    valores_ausentes = map_int(dados, ~ sum(is.na(.x))),
    percentual_ausente = map_dbl(
      dados,
      ~ if (length(.x) == 0) NA_real_ else 100 * mean(is.na(.x))
    )
  )
}

duplicidades_chave <- function(dados, chave, nome_base) {
  dados |>
    count(across(all_of(chave)), name = "numero_registros") |>
    filter(numero_registros > 1) |>
    mutate(base = nome_base, .before = 1)
}

salvar_grafico <- function(grafico, caminho, largura = 10, altura = 6) {
  ggsave(
    filename = caminho,
    plot = grafico,
    width = largura,
    height = altura,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

copiar_com_historico <- function(origem, destino) {
  if (!file.exists(origem)) {
    stop("Produto homologado não localizado para reutilização: ", origem)
  }
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  resultado <- file.copy(origem, destino, overwrite = TRUE)
  if (!isTRUE(resultado)) {
    stop("Não foi possível copiar o produto homologado: ", origem)
  }
  invisible(destino)
}

# -------------------------------------------------------------------
# 3. Arquivamento de produtos canônicos anteriores
# -------------------------------------------------------------------

arquivos_canonicos_anteriores <- list.files(
  dir_relatorio,
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE
)

arquivos_canonicos_anteriores <- arquivos_canonicos_anteriores[
  !str_detect(
    normalizar_caminho(arquivos_canonicos_anteriores),
    fixed("/historico/")
  )
]

if (length(arquivos_canonicos_anteriores) > 0) {
  dir_backup <- file.path(
    dir_historico,
    paste0("antes_execucao_", carimbo_execucao)
  )

  walk(
    arquivos_canonicos_anteriores,
    function(origem) {
      relativo <- str_remove(
        normalizar_caminho(origem),
        paste0("^", normalizar_caminho(dir_relatorio), "/")
      )
      destino <- file.path(dir_backup, relativo)
      dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
      file.copy(origem, destino, overwrite = FALSE)
    }
  )
}

# -------------------------------------------------------------------
# 4. Arquivos de entrada
# -------------------------------------------------------------------

arquivos_entrada <- c(
  base_final = caminho_projeto(
    "dados_finais",
    "base_analitica_final_escola_serie.csv"
  ),
  perfil_escolas = caminho_projeto(
    "dados_finais",
    "perfil_escola_gerencial.csv"
  ),
  indice_escolas = caminho_projeto(
    "dados_finais",
    "indice_carga_potencial_escola.csv"
  ),
  carteiras = caminho_projeto(
    "dados_finais",
    "analise_carteiras_assessoras.csv"
  ),
  detalhe_carteiras = caminho_projeto(
    "dados_finais",
    "carteira_escola_detalhe.csv"
  ),
  cenarios_carteiras = caminho_projeto(
    "dados_finais",
    "analise_carteiras_cenarios.csv"
  ),
  componentes_indice = caminho_projeto(
    "dados_finais",
    "componentes_indice_carga_potencial.csv"
  ),
  sintese_geral = caminho_projeto(
    "resultados",
    "sintese",
    "01_sintese_geral_painel_corrigida.csv"
  ),
  sintese_series = caminho_projeto(
    "resultados",
    "sintese",
    "02_sintese_por_serie_corrigida.csv"
  ),
  modelos_contextuais = caminho_projeto(
    "resultados",
    "sintese",
    "03_comparacao_modelos_contextuais.csv"
  ),
  consistencia_modelos = caminho_projeto(
    "resultados",
    "sintese",
    "04_consistencia_associacoes_contextuais.csv"
  ),
  quadro_interpretativo = caminho_projeto(
    "resultados",
    "sintese",
    "06_quadro_interpretativo.csv"
  ),
  grafico_participacao_series = caminho_projeto(
    "resultados",
    "graficos",
    "sintese",
    "participacao_por_serie_2025_2026_corrigida.png"
  ),
  grafico_variacao_proficiencia_series = caminho_projeto(
    "resultados",
    "graficos",
    "sintese",
    "variacao_proficiencia_por_serie_corrigida.png"
  ),
  grafico_distribuicao_variacao = caminho_projeto(
    "resultados",
    "graficos",
    "distribuicao_delta_proficiencia_escolas.png"
  ),
  grafico_participacao_composicao = caminho_projeto(
    "resultados",
    "graficos",
    "participacao_composicao_proficiencia.png"
  )
)

existencia_entradas <- tibble(
  base = names(arquivos_entrada),
  caminho = unname(arquivos_entrada),
  existe = file.exists(arquivos_entrada)
)

if (any(!existencia_entradas$existe)) {
  write_csv(
    existencia_entradas,
    file.path(dir_documentacao, "03_manifesto_arquivos_entrada.csv"),
    na = ""
  )
  stop(
    "Há arquivos obrigatórios ausentes. Consulte 03_manifesto_arquivos_entrada.csv."
  )
}

# -------------------------------------------------------------------
# 5. Leitura das bases e manifesto de entrada
# -------------------------------------------------------------------

bases <- list(
  base_final = ler_csv(arquivos_entrada[["base_final"]]),
  perfil_escolas = ler_csv(arquivos_entrada[["perfil_escolas"]]),
  indice_escolas = ler_csv(arquivos_entrada[["indice_escolas"]]),
  carteiras = ler_csv(arquivos_entrada[["carteiras"]]),
  detalhe_carteiras = ler_csv(arquivos_entrada[["detalhe_carteiras"]]),
  cenarios_carteiras = ler_csv(arquivos_entrada[["cenarios_carteiras"]]),
  componentes_indice = ler_csv(arquivos_entrada[["componentes_indice"]]),
  sintese_geral = ler_csv(arquivos_entrada[["sintese_geral"]]),
  sintese_series = ler_csv(arquivos_entrada[["sintese_series"]]),
  modelos_contextuais = ler_csv(arquivos_entrada[["modelos_contextuais"]]),
  consistencia_modelos = ler_csv(arquivos_entrada[["consistencia_modelos"]]),
  quadro_interpretativo = ler_csv(arquivos_entrada[["quadro_interpretativo"]])
)

manifesto_entrada <- existencia_entradas |>
  mutate(
    caminho_relativo = map_chr(caminho, caminho_relativo),
    tipo = str_to_lower(tools::file_ext(caminho)),
    tamanho_bytes = file.info(caminho)$size,
    modificacao = format(
      file.info(caminho)$mtime,
      "%Y-%m-%d %H:%M:%S"
    ),
    hash_md5 = map_chr(caminho, hash_md5),
    numero_linhas = map_int(
      base,
      ~ if (.x %in% names(bases)) nrow(bases[[.x]]) else NA_integer_
    ),
    numero_colunas = map_int(
      base,
      ~ if (.x %in% names(bases)) ncol(bases[[.x]]) else NA_integer_
    )
  ) |>
  select(
    base,
    caminho_relativo,
    tipo,
    existe,
    tamanho_bytes,
    modificacao,
    hash_md5,
    numero_linhas,
    numero_colunas
  )

write_csv(
  manifesto_entrada,
  file.path(dir_documentacao, "03_manifesto_arquivos_entrada.csv"),
  na = ""
)

estrutura_entradas <- imap_dfr(
  bases,
  ~ estrutura_base(.x, .y)
)

write_csv(
  estrutura_entradas,
  file.path(dir_documentacao, "04_estrutura_bases_entrada.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 6. Validação de colunas e chaves
# -------------------------------------------------------------------

colunas_obrigatorias <- list(
  base_final = c(
    "id_escola", "codigo_inep", "nome_canonico", "assessora_gerencial",
    "ano_escolar", "componente", "painel_resultado_balanceado",
    "amostra_principal_descritiva", "grupo_administrativo_2024_final",
    "previstos_2025", "avaliados_2025", "taxa_participacao_2025",
    "proficiencia_media_2025", "pct_defasagem_2025", "pct_adequado_2025",
    "previstos_2026", "avaliados_2026", "taxa_participacao_2026",
    "proficiencia_media_2026", "pct_defasagem_2026", "pct_adequado_2026",
    "delta_participacao", "delta_proficiencia", "delta_pct_defasagem",
    "delta_pct_adequado", "observacao_composicao"
  ),
  perfil_escolas = c(
    "id_escola", "codigo_inep", "nome_canonico", "assessora_gerencial",
    "grupo_administrativo_2024_final", "tipo_vinculo_rede_final",
    "status_rede_2025_final", "matriculas_anos_iniciais",
    "turmas_anos_iniciais", "docentes_anos_iniciais",
    "alunos_por_turma_anos_iniciais", "alunos_por_docente_anos_iniciais",
    "porte_anos_iniciais", "pct_matriculas_anos_iniciais_integral",
    "pct_matriculas_educacao_especial", "indice_infraestrutura_basica",
    "numero_series_comparaveis", "painel_completo_cinco_series",
    "taxa_participacao_escola_2025", "taxa_participacao_escola_2026",
    "delta_participacao_escola", "proficiencia_multisserie_ponderada_2025",
    "proficiencia_multisserie_ponderada_2026",
    "delta_proficiencia_multisserie_ponderada", "possui_alerta_composicao",
    "requer_revisao_tecnica", "divergencia_censo_cadastro",
    "possivel_municipalizacao_recente", "escola_nova_recente",
    "observacao_administrativa_final", "observacoes_composicao"
  ),
  indice_escolas = c(
    "id_escola", "nome_canonico", "assessora_gerencial",
    "score_dimensao_volume", "score_dimensao_estrutural",
    "score_dimensao_educacional", "score_dimensao_administrativa",
    "indice_carga_potencial", "faixa_indice_carga_potencial",
    "percentil_indice_carga_potencial", "qualidade_evidencia_educacional",
    "interpretacao_educacional_cautelosa", "interpretacao_indice_cautelosa"
  ),
  carteiras = c(
    "assessora_gerencial", "tipo_carteira", "carteira_nominal",
    "numero_escolas", "matriculas_anos_iniciais_total",
    "turmas_anos_iniciais_total", "series_comparaveis_total",
    "carga_potencial_total", "indice_carga_potencial_medio",
    "indice_carga_potencial_mediano", "dimensao_volume_media",
    "dimensao_estrutural_media", "dimensao_educacional_media",
    "dimensao_administrativa_media", "numero_escolas_faixa_4",
    "numero_escolas_complexidade_administrativa",
    "numero_escolas_interpretacao_cautelosa",
    "numero_escolas_alerta_composicao",
    "faixa_carga_total_entre_carteiras_nominais",
    "faixa_indice_medio_entre_carteiras_nominais"
  ),
  detalhe_carteiras = c(
    "assessora_gerencial", "tipo_carteira", "carteira_nominal",
    "ordem_interna_carga_potencial", "id_escola", "nome_canonico",
    "indice_carga_potencial"
  ),
  cenarios_carteiras = c(
    "cenario", "assessora_gerencial", "tipo_carteira",
    "carteira_nominal", "carga_potencial_total_cenario",
    "indice_medio_cenario", "mudou_faixa_carga_total_carteira"
  ),
  componentes_indice = c(
    "id_escola", "nome_canonico", "assessora_gerencial", "dimensao",
    "componente", "valor_bruto", "score_componente_0_100",
    "peso_declarado_na_dimensao", "peso_declarado_no_indice_principal"
  ),
  sintese_geral = c(
    "escolas", "observacoes_escola_serie", "participacao_2025",
    "participacao_2026", "delta_participacao",
    "proficiencia_2025_ponderada", "proficiencia_2026_ponderada",
    "delta_proficiencia_ponderada"
  ),
  sintese_series = c(
    "ano_escolar", "escolas", "previstos_total_2025",
    "previstos_total_2026", "avaliados_total_2025",
    "avaliados_total_2026", "participacao_2025", "participacao_2026",
    "delta_participacao", "proficiencia_2025_ponderada",
    "proficiencia_2026_ponderada", "delta_proficiencia_ponderada",
    "adequado_2025_ponderado", "adequado_2026_ponderado",
    "delta_adequado_ponderado", "defasagem_2025_ponderada",
    "defasagem_2026_ponderada", "delta_defasagem_ponderada"
  ),
  modelos_contextuais = c(
    "especificacao", "termo", "estimativa", "erro_padrao_cluster_escola",
    "valor_p_cluster", "direcao", "classificacao_evidencia"
  ),
  consistencia_modelos = c(
    "termo", "numero_especificacoes", "estimativa_minima",
    "estimativa_maxima", "sinal_consistente", "evidencia_consistente"
  ),
  quadro_interpretativo = c(
    "tema", "conclusao_tecnica", "grau_de_seguranca",
    "limite_de_interpretacao"
  )
)

validacao_colunas <- imap_dfr(
  colunas_obrigatorias,
  ~ validar_colunas(bases[[.y]], .x, .y)
)

write_csv(
  validacao_colunas,
  file.path(dir_documentacao, "05_validacao_colunas_obrigatorias.csv"),
  na = ""
)

if (any(!validacao_colunas$presente)) {
  stop(
    "Há colunas obrigatórias ausentes. Consulte 05_validacao_colunas_obrigatorias.csv."
  )
}

duplicidades <- bind_rows(
  duplicidades_chave(
    bases$base_final,
    c("id_escola", "ano_escolar", "componente"),
    "base_final"
  ),
  duplicidades_chave(
    bases$perfil_escolas,
    "id_escola",
    "perfil_escolas"
  ),
  duplicidades_chave(
    bases$indice_escolas,
    "id_escola",
    "indice_escolas"
  ),
  duplicidades_chave(
    bases$carteiras,
    "assessora_gerencial",
    "carteiras"
  )
)

if (nrow(duplicidades) == 0) {
  duplicidades <- tibble(
    base = character(),
    id_escola = character(),
    ano_escolar = integer(),
    componente = character(),
    assessora_gerencial = character(),
    numero_registros = integer()
  )
}

write_csv(
  duplicidades,
  file.path(dir_documentacao, "06_duplicidades_chaves.csv"),
  na = ""
)

if (nrow(duplicidades) > 0) {
  stop("Foram encontradas duplicidades nas chaves analíticas obrigatórias.")
}

# -------------------------------------------------------------------
# 7. Inventário de produtos existentes e matriz de decisão
# -------------------------------------------------------------------

caminhos_inventario <- c(
  list.files(
    caminho_projeto("resultados"),
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  ),
  list.files(
    caminho_projeto("dados_finais"),
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )
)

caminhos_inventario <- caminhos_inventario[
  !str_detect(normalizar_caminho(caminhos_inventario), fixed("/relatorio/"))
]

inventario_produtos <- tibble(caminho = caminhos_inventario) |>
  mutate(
    caminho_relativo = map_chr(caminho, caminho_relativo),
    extensao = str_to_lower(tools::file_ext(caminho)),
    tipo_produto = case_when(
      extensao == "csv" ~ "Tabela ou base tabular",
      extensao == "png" ~ "Gráfico raster",
      extensao == "pdf" ~ "Documento PDF",
      extensao == "html" ~ "Documento HTML",
      extensao == "rds" ~ "Objeto R serializado",
      TRUE ~ "Outro"
    ),
    tamanho_bytes = file.info(caminho)$size,
    modificacao = format(file.info(caminho)$mtime, "%Y-%m-%d %H:%M:%S"),
    hash_md5 = map_chr(caminho, hash_md5),
    uso_relatorio = case_when(
      caminho_relativo %in% map_chr(
        arquivos_entrada,
        caminho_relativo
      ) ~ "Insumo direto do módulo 21",
      str_detect(caminho_relativo, "resultados/fichas_escolas/") ~
        "Referência complementar; não duplicar no corpo",
      str_detect(caminho_relativo, "resultados/fichas_carteiras/") ~
        "Referência complementar; não duplicar no corpo",
      TRUE ~ "Produto inventariado"
    )
  ) |>
  select(
    caminho_relativo,
    extensao,
    tipo_produto,
    tamanho_bytes,
    modificacao,
    hash_md5,
    uso_relatorio
  ) |>
  arrange(tipo_produto, caminho_relativo)

write_csv(
  inventario_produtos,
  file.path(dir_documentacao, "07_inventario_produtos_existentes.csv"),
  na = ""
)

matriz_produtos <- tribble(
  ~id_produto, ~destino, ~formato, ~titulo, ~decisao, ~origem_analitica, ~justificativa,
  "T01", "Corpo", "CSV", "Escopo e cobertura do estudo", "Produzir", "Bases finais homologadas", "Síntese institucional ainda não existia em formato único.",
  "T02", "Corpo", "CSV", "Perfil estrutural da rede", "Produzir", "Perfil escolar gerencial", "Consolida contexto de 2024 sem reproduzir fichas escolares.",
  "T03", "Corpo", "CSV", "Resultados observados por ano escolar", "Reformatar sem recalcular", "Síntese corrigida do módulo 14A", "Preserva os cálculos homologados e padroniza campos para o relatório.",
  "T04", "Corpo", "CSV", "Síntese das carteiras nominais", "Produzir", "Análise de carteiras do módulo 18", "Seleciona indicadores gerenciais essenciais sem criar ranking de qualidade.",
  "T05", "Corpo", "CSV", "Quatro dimensões por carteira nominal", "Produzir", "Análise de carteiras do módulo 18", "Mantém as quatro dimensões explicitamente visíveis.",
  "T06", "Corpo", "CSV", "Cautelas de interpretação", "Produzir", "Princípios metodológicos e quadro interpretativo", "Centraliza limites que devem acompanhar tabelas e gráficos.",
  "G01", "Corpo", "PNG", "Participação por ano escolar, 2025–2026", "Reutilizar sem recálculo", "Módulo 14A", "Gráfico corrigido já homologado.",
  "G02", "Corpo", "PNG", "Variação da proficiência por ano escolar", "Reutilizar sem recálculo", "Módulo 14A", "Gráfico corrigido já homologado.",
  "G03", "Corpo", "PNG", "Distribuição das variações de proficiência", "Reutilizar sem recálculo", "Módulo 07", "Produto já existente e adequado ao corpo.",
  "G04", "Corpo", "PNG", "Participação, composição e proficiência", "Reutilizar sem recálculo", "Módulo 08", "Produto de sensibilidade já existente.",
  "G05", "Corpo", "PNG", "Composição administrativa da rede", "Produzir", "Perfil escolar gerencial", "Lacuna visual para caracterização da rede.",
  "G06", "Corpo", "PNG", "Carga potencial acumulada por carteira", "Produzir", "Análise de carteiras do módulo 18", "Lacuna gerencial; compara apenas carteiras nominais.",
  "G07", "Corpo", "PNG", "Extensão e intensidade das carteiras", "Produzir", "Análise de carteiras do módulo 18", "Distingue número de escolas de complexidade média.",
  "G08", "Corpo", "PNG", "Quatro dimensões por carteira", "Produzir", "Análise de carteiras do módulo 18", "Torna a composição multidimensional visível sem colapsá-la no índice.",
  "A01", "Anexo", "CSV", "Perfil detalhado das escolas", "Selecionar", "Módulo 16", "Base para consulta das especificidades escolares.",
  "A02", "Anexo", "CSV", "Resultados escola × ano escolar", "Selecionar", "Módulo 15", "Mantém resultados e alertas de composição em nível detalhado.",
  "A03", "Anexo", "CSV", "Índice de carga potencial por escola", "Selecionar", "Módulo 17", "Documenta dimensões, índice, faixa e cautelas.",
  "A04", "Anexo", "CSV", "Detalhamento das carteiras", "Selecionar", "Módulo 18", "Preserva categorias não nominais e composição interna.",
  "A05", "Anexo", "CSV", "Cenários de sensibilidade das carteiras", "Selecionar", "Módulo 18", "Expõe dependência dos resultados aos pesos.",
  "A06", "Anexo", "CSV", "Componentes do índice", "Reutilizar", "Módulo 17", "Rastreia variáveis, escores e pesos declarados.",
  "A07", "Anexo", "CSV", "Modelos contextuais descritivos", "Reutilizar", "Módulos 13A e 14", "Mantém ajuste descritivo e erros-padrão agrupados por escola.",
  "A08", "Anexo", "CSV", "Consistência das associações contextuais", "Reutilizar", "Módulo 14", "Resume estabilidade entre especificações sem interpretação causal.",
  "A09", "Anexo", "CSV", "Casos para validação qualitativa", "Produzir", "Módulos 16 a 18", "Organiza alertas administrativos, educacionais e de composição.",
  "A10", "Anexo", "CSV", "Catálogo dos produtos preexistentes", "Produzir", "Inventário do repositório", "Documenta reuso e evita duplicação futura."
)

write_csv(
  matriz_produtos,
  file.path(dir_documentacao, "08_matriz_produtos_relatorio.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 8. Tabelas destinadas ao corpo do relatório
# -------------------------------------------------------------------

base_final <- bases$base_final
perfil <- bases$perfil_escolas
indice <- bases$indice_escolas
carteiras <- bases$carteiras

tabela_escopo <- tribble(
  ~ordem, ~indicador, ~valor, ~unidade, ~observacao,
  1L, "Escolas acompanhadas", n_distinct(base_final$id_escola), "escolas", "Universo escolar consolidado.",
  2L, "Observações escola × ano escolar", nrow(base_final), "observações", "Cinco anos escolares por escola no universo potencial.",
  3L, "Observações comparáveis", sum(base_final$painel_resultado_balanceado, na.rm = TRUE), "observações", "Resultado disponível em 2025 e 2026.",
  4L, "Anos escolares", n_distinct(base_final$ano_escolar), "anos escolares", "1º ao 5º ano.",
  5L, "Carteiras nominais", sum(carteiras$carteira_nominal, na.rm = TRUE), "carteiras", "Comparáveis entre si na análise gerencial.",
  6L, "Categorias administrativas", nrow(carteiras), "categorias", "Inclui agrupamento residual e ausência de vínculo.",
  7L, "Escolas com contexto de 2024", n_distinct(base_final$id_escola[which(base_final$contexto_2024_encontrado)]), "escolas", "Contexto estrutural pré-programa disponível.",
  8L, "Componente curricular comparável", 1, "componente", "Língua Portuguesa."
) |>
  mutate(valor = as.numeric(valor))

tabela_perfil_rede <- tibble(
  indicador = c(
    "Escolas",
    "Matrículas nos anos iniciais",
    "Turmas dos anos iniciais",
    "Docentes dos anos iniciais",
    "Mediana de matrículas por escola",
    "Média de estudantes por turma",
    "Média de estudantes por docente",
    "Matrículas em tempo integral",
    "Matrículas da educação especial",
    "Índice médio de infraestrutura",
    "Escolas com painel completo nas cinco séries",
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
      perfil$alunos_por_docente_anos_iniciais,
      perfil$docentes_anos_iniciais,
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
    sum(perfil$painel_completo_cinco_series, na.rm = TRUE),
    sum(perfil$possui_alerta_composicao, na.rm = TRUE)
  ),
  tipo_formato = c(
    "Contagem", "Contagem", "Contagem", "Contagem", "Contagem",
    "Razão", "Razão", "Percentual", "Percentual", "Índice",
    "Contagem", "Contagem"
  ),
  fonte_temporal = c(
    rep("Censo Escolar 2024", 10),
    rep("CAEd 2025–2026", 2)
  )
) |>
  mutate(
    valor = case_when(
      tipo_formato %in% c("Contagem") ~ arredondar(valor, 0),
      tipo_formato %in% c("Razão") ~ arredondar(valor, 2),
      TRUE ~ arredondar(valor, 1)
    )
  )

tabela_resultados_series <- bases$sintese_series |>
  transmute(
    ano_escolar = as.integer(ano_escolar),
    escolas = as.integer(escolas),
    previstos_2025 = as.integer(previstos_total_2025),
    previstos_2026 = as.integer(previstos_total_2026),
    avaliados_2025 = as.integer(avaliados_total_2025),
    avaliados_2026 = as.integer(avaliados_total_2026),
    participacao_2025 = arredondar(participacao_2025, 1),
    participacao_2026 = arredondar(participacao_2026, 1),
    delta_participacao_pp = arredondar(delta_participacao, 1),
    proficiencia_2025 = arredondar(proficiencia_2025_ponderada, 1),
    proficiencia_2026 = arredondar(proficiencia_2026_ponderada, 1),
    delta_proficiencia = arredondar(delta_proficiencia_ponderada, 1),
    adequado_2025 = arredondar(adequado_2025_ponderado, 1),
    adequado_2026 = arredondar(adequado_2026_ponderado, 1),
    delta_adequado_pp = arredondar(delta_adequado_ponderado, 1),
    defasagem_2025 = arredondar(defasagem_2025_ponderada, 1),
    defasagem_2026 = arredondar(defasagem_2026_ponderada, 1),
    delta_defasagem_pp = arredondar(delta_defasagem_ponderada, 1)
  ) |>
  arrange(ano_escolar)

tabela_carteiras <- carteiras |>
  filter(carteira_nominal) |>
  transmute(
    carteira = str_to_title(assessora_gerencial),
    numero_escolas = as.integer(numero_escolas),
    matriculas_anos_iniciais = as.integer(matriculas_anos_iniciais_total),
    turmas_anos_iniciais = as.integer(turmas_anos_iniciais_total),
    series_comparaveis = as.integer(series_comparaveis_total),
    carga_potencial_total = arredondar(carga_potencial_total, 1),
    intensidade_media = arredondar(indice_carga_potencial_medio, 1),
    intensidade_mediana = arredondar(indice_carga_potencial_mediano, 1),
    escolas_faixa_superior = as.integer(numero_escolas_faixa_4),
    escolas_complexidade_administrativa = as.integer(
      numero_escolas_complexidade_administrativa
    ),
    escolas_interpretacao_cautelosa = as.integer(
      numero_escolas_interpretacao_cautelosa
    ),
    escolas_alerta_composicao = as.integer(numero_escolas_alerta_composicao),
    faixa_carga_relativa = faixa_carga_total_entre_carteiras_nominais,
    faixa_intensidade_relativa = faixa_indice_medio_entre_carteiras_nominais
  ) |>
  arrange(desc(carga_potencial_total), carteira)

tabela_dimensoes <- carteiras |>
  filter(carteira_nominal) |>
  select(
    assessora_gerencial,
    dimensao_volume_media,
    dimensao_estrutural_media,
    dimensao_educacional_media,
    dimensao_administrativa_media
  ) |>
  pivot_longer(
    cols = starts_with("dimensao_"),
    names_to = "dimensao",
    values_to = "escore_medio"
  ) |>
  mutate(
    carteira = str_to_title(assessora_gerencial),
    dimensao = recode(
      dimensao,
      dimensao_volume_media = "Volume",
      dimensao_estrutural_media = "Complexidade estrutural",
      dimensao_educacional_media = "Desafio educacional",
      dimensao_administrativa_media = "Complexidade administrativa"
    ),
    escore_medio = arredondar(escore_medio, 1)
  ) |>
  select(carteira, dimensao, escore_medio) |>
  arrange(carteira, dimensao)

cautelas_institucionais <- tribble(
  ~tema, ~orientacao, ~aplicacao,
  "Causalidade", "O desenho não identifica efeitos causais do assessoramento.", "Usar linguagem de variação observada e associação.",
  "Assessora", "Resultados escolares não medem qualidade ou efetividade individual.", "Tratar o vínculo apenas como organização administrativa da carteira.",
  "Índice", "O índice mede carga potencial relativa e não qualidade.", "Exibir as quatro dimensões e as cautelas junto ao valor sintético.",
  "Participação", "Mudanças de participação e do público previsto podem alterar a composição.", "Apresentar alertas de composição nas comparações 2025–2026.",
  "Faixas", "Faixas, percentis e posições são relativos à rede observada.", "Não denominar as posições como ranking de qualidade.",
  "Redistribuição", "O diagnóstico quantitativo não define automaticamente novas carteiras.", "Submeter hipóteses à validação qualitativa, territorial e operacional.",
  "Categorias não nominais", "Outras e Sem vinculação informada não são comparáveis diretamente às carteiras nominais.", "Preservar nos totais e anexos, mas excluir das referências nominais."
)

tabela_cautelas <- bind_rows(
  cautelas_institucionais,
  bases$quadro_interpretativo |>
    transmute(
      tema = paste0("Síntese técnica — ", tema),
      orientacao = conclusao_tecnica,
      aplicacao = paste0(
        "Grau de segurança: ", grau_de_seguranca,
        ". Limite: ", limite_de_interpretacao
      )
    )
)

arquivos_tabelas_corpo <- c(
  T01 = file.path(dir_corpo_tabelas, "01_escopo_cobertura_estudo.csv"),
  T02 = file.path(dir_corpo_tabelas, "02_perfil_estrutural_rede.csv"),
  T03 = file.path(dir_corpo_tabelas, "03_resultados_rede_por_ano_escolar.csv"),
  T04 = file.path(dir_corpo_tabelas, "04_sintese_carteiras_nominais.csv"),
  T05 = file.path(dir_corpo_tabelas, "05_dimensoes_carteiras_nominais.csv"),
  T06 = file.path(dir_corpo_tabelas, "06_cautelas_interpretacao.csv")
)

walk2(
  list(
    tabela_escopo,
    tabela_perfil_rede,
    tabela_resultados_series,
    tabela_carteiras,
    tabela_dimensoes,
    tabela_cautelas
  ),
  arquivos_tabelas_corpo,
  ~ write_csv(.x, .y, na = "")
)

# -------------------------------------------------------------------
# 9. Tabelas destinadas aos anexos técnicos
# -------------------------------------------------------------------

anexo_perfil_escolas <- perfil |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_gerencial,
    grupo_administrativo_2024_final,
    tipo_vinculo_rede_final,
    status_rede_2025_final,
    observacao_administrativa_final,
    matriculas_anos_iniciais,
    turmas_anos_iniciais,
    docentes_anos_iniciais,
    alunos_por_turma_anos_iniciais,
    alunos_por_docente_anos_iniciais,
    porte_anos_iniciais,
    pct_matriculas_anos_iniciais_integral,
    pct_matriculas_educacao_especial,
    indice_infraestrutura_basica,
    numero_series_comparaveis,
    painel_completo_cinco_series,
    taxa_participacao_escola_2025,
    taxa_participacao_escola_2026,
    delta_participacao_escola,
    proficiencia_multisserie_ponderada_2025,
    proficiencia_multisserie_ponderada_2026,
    delta_proficiencia_multisserie_ponderada,
    possui_alerta_composicao,
    requer_revisao_tecnica
  ) |>
  arrange(nome_canonico)

anexo_resultados_escola_serie <- base_final |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_gerencial,
    ano_escolar,
    componente,
    painel_resultado_balanceado,
    amostra_principal_descritiva,
    previstos_2025,
    avaliados_2025,
    taxa_participacao_2025,
    proficiencia_media_2025,
    pct_defasagem_2025,
    pct_adequado_2025,
    previstos_2026,
    avaliados_2026,
    taxa_participacao_2026,
    proficiencia_media_2026,
    pct_defasagem_2026,
    pct_adequado_2026,
    delta_participacao,
    delta_proficiencia,
    delta_pct_defasagem,
    delta_pct_adequado,
    observacao_composicao
  ) |>
  arrange(nome_canonico, ano_escolar, componente)

anexo_indice_escolas <- indice |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_gerencial,
    score_dimensao_volume,
    score_dimensao_estrutural,
    score_dimensao_educacional,
    score_dimensao_administrativa,
    indice_carga_potencial,
    percentil_indice_carga_potencial,
    faixa_indice_carga_potencial,
    qualidade_evidencia_educacional,
    interpretacao_educacional_cautelosa,
    interpretacao_indice_cautelosa
  ) |>
  arrange(desc(indice_carga_potencial), nome_canonico)

anexo_detalhe_carteiras <- bases$detalhe_carteiras |>
  arrange(
    desc(carteira_nominal),
    assessora_gerencial,
    ordem_interna_carga_potencial
  )

anexo_cenarios <- bases$cenarios_carteiras |>
  arrange(desc(carteira_nominal), assessora_gerencial, cenario)

anexo_componentes_indice <- bases$componentes_indice |>
  arrange(nome_canonico, dimensao, componente)

anexo_modelos <- bases$modelos_contextuais |>
  arrange(especificacao, termo)

anexo_consistencia <- bases$consistencia_modelos |>
  arrange(termo)

casos_validacao <- perfil |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    assessora_gerencial,
    grupo_administrativo_2024_final,
    tipo_vinculo_rede_final,
    status_rede_2025_final,
    requer_revisao_tecnica,
    divergencia_censo_cadastro,
    possivel_municipalizacao_recente,
    escola_nova_recente,
    possui_alerta_composicao,
    painel_completo_cinco_series,
    observacao_administrativa_final,
    observacoes_composicao
  ) |>
  left_join(
    indice |>
      select(
        id_escola,
        qualidade_evidencia_educacional,
        interpretacao_educacional_cautelosa,
        interpretacao_indice_cautelosa
      ),
    by = "id_escola"
  ) |>
  filter(
    coalesce(requer_revisao_tecnica, FALSE) |
      coalesce(divergencia_censo_cadastro, FALSE) |
      coalesce(possivel_municipalizacao_recente, FALSE) |
      coalesce(escola_nova_recente, FALSE) |
      coalesce(possui_alerta_composicao, FALSE) |
      !coalesce(painel_completo_cinco_series, FALSE) |
      coalesce(interpretacao_educacional_cautelosa, FALSE) |
      coalesce(interpretacao_indice_cautelosa, FALSE)
  ) |>
  arrange(assessora_gerencial, nome_canonico)

catalogo_preexistentes <- inventario_produtos |>
  filter(
    caminho_relativo %in% map_chr(arquivos_entrada, caminho_relativo) |
      str_detect(
        caminho_relativo,
        "resultados/(graficos|fichas_escolas|fichas_carteiras)/"
      )
  )

arquivos_tabelas_anexos <- c(
  A01 = file.path(dir_anexos_tabelas, "A01_perfil_detalhado_escolas.csv"),
  A02 = file.path(dir_anexos_tabelas, "A02_resultados_escola_ano_escolar.csv"),
  A03 = file.path(dir_anexos_tabelas, "A03_indice_carga_potencial_escolas.csv"),
  A04 = file.path(dir_anexos_tabelas, "A04_detalhamento_carteiras.csv"),
  A05 = file.path(dir_anexos_tabelas, "A05_cenarios_sensibilidade_carteiras.csv"),
  A06 = file.path(dir_anexos_tabelas, "A06_componentes_indice.csv"),
  A07 = file.path(dir_anexos_tabelas, "A07_modelos_contextuais_descritivos.csv"),
  A08 = file.path(dir_anexos_tabelas, "A08_consistencia_associacoes_contextuais.csv"),
  A09 = file.path(dir_anexos_tabelas, "A09_casos_validacao_qualitativa.csv"),
  A10 = file.path(dir_anexos_tabelas, "A10_catalogo_produtos_preexistentes.csv")
)

walk2(
  list(
    anexo_perfil_escolas,
    anexo_resultados_escola_serie,
    anexo_indice_escolas,
    anexo_detalhe_carteiras,
    anexo_cenarios,
    anexo_componentes_indice,
    anexo_modelos,
    anexo_consistencia,
    casos_validacao,
    catalogo_preexistentes
  ),
  arquivos_tabelas_anexos,
  ~ write_csv(.x, .y, na = "")
)

# -------------------------------------------------------------------
# 10. Gráficos homologados reutilizados sem recálculo
# -------------------------------------------------------------------

arquivos_graficos_corpo <- c(
  G01 = file.path(
    dir_corpo_graficos,
    "01_participacao_por_ano_escolar_2025_2026.png"
  ),
  G02 = file.path(
    dir_corpo_graficos,
    "02_variacao_proficiencia_por_ano_escolar.png"
  ),
  G03 = file.path(
    dir_corpo_graficos,
    "03_distribuicao_variacoes_proficiencia.png"
  ),
  G04 = file.path(
    dir_corpo_graficos,
    "04_participacao_composicao_proficiencia.png"
  ),
  G05 = file.path(
    dir_corpo_graficos,
    "05_composicao_administrativa_rede.png"
  ),
  G06 = file.path(
    dir_corpo_graficos,
    "06_carga_potencial_acumulada_carteiras.png"
  ),
  G07 = file.path(
    dir_corpo_graficos,
    "07_extensao_intensidade_carteiras.png"
  ),
  G08 = file.path(
    dir_corpo_graficos,
    "08_quatro_dimensoes_carteiras.png"
  )
)

fontes_graficos_reutilizados <- c(
  G01 = arquivos_entrada[["grafico_participacao_series"]],
  G02 = arquivos_entrada[["grafico_variacao_proficiencia_series"]],
  G03 = arquivos_entrada[["grafico_distribuicao_variacao"]],
  G04 = arquivos_entrada[["grafico_participacao_composicao"]]
)

walk2(
  fontes_graficos_reutilizados,
  arquivos_graficos_corpo[names(fontes_graficos_reutilizados)],
  copiar_com_historico
)

registro_reuso <- tibble(
  id_produto = names(fontes_graficos_reutilizados),
  origem = map_chr(fontes_graficos_reutilizados, caminho_relativo),
  destino = map_chr(
    arquivos_graficos_corpo[names(fontes_graficos_reutilizados)],
    caminho_relativo
  ),
  hash_origem = map_chr(fontes_graficos_reutilizados, hash_md5),
  hash_destino = map_chr(
    arquivos_graficos_corpo[names(fontes_graficos_reutilizados)],
    hash_md5
  )
) |>
  mutate(
    copia_identica = hash_origem == hash_destino,
    status = if_else(
      copia_identica,
      "Reutilizado sem recálculo",
      "ERRO: cópia divergente"
    )
  )

write_csv(
  registro_reuso,
  file.path(dir_documentacao, "09_registro_produtos_reutilizados.csv"),
  na = ""
)

if (any(!registro_reuso$copia_identica)) {
  stop("Um ou mais gráficos reutilizados divergiram do arquivo homologado.")
}

# -------------------------------------------------------------------
# 11. Novos gráficos necessários ao corpo do relatório
# -------------------------------------------------------------------

composicao_administrativa <- perfil |>
  mutate(
    grupo_administrativo_2024_final = coalesce(
      grupo_administrativo_2024_final,
      "Não informado"
    )
  ) |>
  count(grupo_administrativo_2024_final, name = "numero_escolas") |>
  mutate(
    percentual_escolas = 100 * numero_escolas / sum(numero_escolas),
    grupo_rotulo = str_wrap(grupo_administrativo_2024_final, width = 34),
    grupo_rotulo = fct_reorder(grupo_rotulo, numero_escolas)
  )

grafico_composicao_administrativa <- ggplot(
  composicao_administrativa,
  aes(x = numero_escolas, y = grupo_rotulo)
) +
  geom_col(width = 0.68, fill = cores_relatorio[["azul"]]) +
  geom_text(
    aes(
      label = paste0(
        numero_escolas,
        " (",
        rotulo_percentual(percentual_escolas),
        ")"
      )
    ),
    hjust = -0.08,
    color = cores_relatorio[["azul_escuro"]],
    fontface = "bold",
    size = 3.6
  ) +
  scale_x_continuous(
    labels = rotulo_numero,
    expand = expansion(mult = c(0, 0.24))
  ) +
  labs(
    title = "Composição administrativa da rede acompanhada",
    subtitle = "Situação observada no contexto pré-programa de 2024",
    x = "Número de escolas",
    y = NULL,
    caption = paste(
      "Fonte: Censo Escolar 2024 e classificação administrativa validada pela UEF-SMED-PMPA.",
      "As categorias descrevem vínculos e transições; não representam qualidade escolar."
    )
  ) +
  tema_relatorio()

salvar_grafico(
  grafico_composicao_administrativa,
  arquivos_graficos_corpo[["G05"]],
  largura = 10,
  altura = 5.8
)

carteiras_nominais <- carteiras |>
  filter(carteira_nominal) |>
  mutate(
    carteira = str_to_title(assessora_gerencial),
    carteira_ordem = fct_reorder(carteira, carga_potencial_total)
  )

referencia_carga <- mean(
  carteiras_nominais$carga_potencial_total,
  na.rm = TRUE
)

grafico_carga_carteiras <- ggplot(
  carteiras_nominais,
  aes(x = carga_potencial_total, y = carteira_ordem)
) +
  geom_vline(
    xintercept = referencia_carga,
    color = cores_relatorio[["laranja"]],
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  geom_col(
    aes(fill = indice_carga_potencial_medio),
    width = 0.67
  ) +
  geom_text(
    aes(label = rotulo_indice(carga_potencial_total)),
    hjust = -0.10,
    size = 3.5,
    color = cores_relatorio[["azul_escuro"]],
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = cores_relatorio[["azul_claro"]],
    high = cores_relatorio[["azul_escuro"]],
    name = "Intensidade média"
  ) +
  scale_x_continuous(
    labels = rotulo_indice,
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    title = "Carga potencial acumulada das carteiras nominais",
    subtitle = "A soma combina extensão da carteira e composição das escolas",
    x = "Carga potencial total (soma dos índices escolares)",
    y = NULL,
    caption = paste0(
      "Fonte: índice descritivo de carga potencial — módulos 17 e 18. ",
      "Linha tracejada: média das 11 carteiras nominais (",
      rotulo_indice(referencia_carga),
      "). O indicador não mede qualidade ou efetividade da assessora."
    )
  ) +
  tema_relatorio()

salvar_grafico(
  grafico_carga_carteiras,
  arquivos_graficos_corpo[["G06"]],
  largura = 10,
  altura = 6.4
)

grafico_extensao_intensidade <- ggplot(
  carteiras_nominais,
  aes(
    x = numero_escolas,
    y = indice_carga_potencial_medio,
    size = carga_potencial_total,
    label = carteira
  )
) +
  geom_hline(
    yintercept = mean(
      carteiras_nominais$indice_carga_potencial_medio,
      na.rm = TRUE
    ),
    color = cores_relatorio[["cinza"]],
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_vline(
    xintercept = mean(carteiras_nominais$numero_escolas, na.rm = TRUE),
    color = cores_relatorio[["cinza"]],
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_point(
    color = cores_relatorio[["azul"]],
    fill = cores_relatorio[["azul_claro"]],
    shape = 21,
    stroke = 1.0,
    alpha = 0.92
  ) +
  geom_text(
    nudge_y = 1.8,
    size = 3.25,
    color = cores_relatorio[["azul_escuro"]],
    check_overlap = TRUE
  ) +
  scale_size_continuous(
    range = c(4, 10),
    labels = rotulo_indice,
    name = "Carga potencial total"
  ) +
  scale_x_continuous(
    breaks = sort(unique(carteiras_nominais$numero_escolas)),
    labels = rotulo_numero,
    expand = expansion(mult = c(0.12, 0.12))
  ) +
  scale_y_continuous(
    labels = rotulo_indice,
    limits = c(0, 100),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  labs(
    title = "Extensão e intensidade das carteiras nominais",
    subtitle = "Número de escolas e complexidade média são dimensões complementares da carga",
    x = "Número de escolas na carteira",
    y = "Índice médio de carga potencial",
    caption = paste(
      "Fonte: módulos 17 e 18.",
      "O tamanho dos pontos representa a carga acumulada.",
      "As linhas tracejadas são médias descritivas das carteiras nominais."
    )
  ) +
  tema_relatorio()

salvar_grafico(
  grafico_extensao_intensidade,
  arquivos_graficos_corpo[["G07"]],
  largura = 10,
  altura = 6.5
)

ordem_carteiras_dimensoes <- carteiras_nominais |>
  arrange(desc(carga_potencial_total)) |>
  pull(carteira)

dimensoes_grafico <- tabela_dimensoes |>
  mutate(
    carteira = factor(carteira, levels = rev(ordem_carteiras_dimensoes)),
    dimensao = factor(
      dimensao,
      levels = c(
        "Volume",
        "Complexidade estrutural",
        "Desafio educacional",
        "Complexidade administrativa"
      )
    )
  )

grafico_dimensoes <- ggplot(
  dimensoes_grafico,
  aes(x = dimensao, y = carteira, fill = escore_medio)
) +
  geom_tile(color = "white", linewidth = 1.1) +
  geom_text(
    aes(
      label = rotulo_indice(escore_medio),
      color = escore_medio >= 55
    ),
    fontface = "bold",
    size = 3.4
  ) +
  scale_color_manual(values = c(`TRUE` = "white", `FALSE` = cores_relatorio[["azul_escuro"]])) +
  scale_fill_gradientn(
    colors = c(
      cores_relatorio[["fundo"]],
      cores_relatorio[["azul_claro"]],
      cores_relatorio[["azul"]],
      cores_relatorio[["azul_escuro"]]
    ),
    limits = c(0, 100),
    labels = rotulo_indice,
    name = "Escore médio"
  ) +
  scale_x_discrete(labels = ~ str_wrap(.x, width = 18)) +
  guides(color = "none") +
  labs(
    title = "Composição da carga potencial nas quatro dimensões",
    subtitle = "Escores médios por carteira nominal; escala relativa de 0 a 100",
    x = NULL,
    y = NULL,
    caption = paste(
      "Fonte: módulos 17 e 18.",
      "As quatro dimensões devem ser interpretadas em conjunto.",
      "Os escores não constituem ranking de qualidade."
    )
  ) +
  tema_relatorio() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom"
  )

salvar_grafico(
  grafico_dimensoes,
  arquivos_graficos_corpo[["G08"]],
  largura = 10.5,
  altura = 6.8
)

# -------------------------------------------------------------------
# 12. Catálogo editorial: títulos, notas, fontes, escalas e destinos
# -------------------------------------------------------------------

catalogo_editorial <- tribble(
  ~id_produto, ~arquivo, ~destino, ~tipo, ~titulo, ~subtitulo, ~nota_metodologica, ~fonte, ~escala_formato, ~status_producao,
  "T01", caminho_relativo(arquivos_tabelas_corpo[["T01"]]), "Corpo", "Tabela", "Escopo e cobertura do estudo", "Universo analítico consolidado", "A comparabilidade exige resultado disponível nos dois anos; não implica amostra causal.", "Bases finais dos módulos 15 a 20.", "Contagens inteiras.", "Novo produto editorial",
  "T02", caminho_relativo(arquivos_tabelas_corpo[["T02"]]), "Corpo", "Tabela", "Perfil estrutural da rede", "Características de contexto e cobertura", "Indicadores estruturais referem-se a 2024; participação e painel referem-se a 2025–2026.", "Censo Escolar 2024 e CAEd 2025–2026.", "Contagens inteiras; razões com duas casas; percentuais e índices com uma casa.", "Novo produto editorial",
  "T03", caminho_relativo(arquivos_tabelas_corpo[["T03"]]), "Corpo", "Tabela", "Resultados observados por ano escolar", "Comparação descritiva 2025–2026", "Variações não identificam efeito causal e são condicionadas pela participação.", "Síntese corrigida do módulo 14A.", "Contagens inteiras; percentuais, p.p. e proficiência com uma casa.", "Reformatado sem recálculo",
  "T04", caminho_relativo(arquivos_tabelas_corpo[["T04"]]), "Corpo", "Tabela", "Síntese das carteiras nominais", "Extensão, carga acumulada e intensidade", "Não incluir agrupamentos residuais nas referências comparativas nominais.", "Módulo 18.", "Contagens inteiras; índices com uma casa.", "Novo produto editorial",
  "T05", caminho_relativo(arquivos_tabelas_corpo[["T05"]]), "Corpo", "Tabela", "Quatro dimensões por carteira nominal", "Composição média da carga potencial", "Dimensões relativas; não constituem avaliação da assessora.", "Módulos 17 e 18.", "Escala 0–100; uma casa decimal.", "Novo produto editorial",
  "T06", caminho_relativo(arquivos_tabelas_corpo[["T06"]]), "Corpo", "Tabela", "Cautelas de interpretação", "Regras para leitura institucional", "Deve acompanhar a redação dos módulos 22, 24, 26 e 27.", "Princípios homologados e módulo 14.", "Texto.", "Novo produto editorial",
  "G01", caminho_relativo(arquivos_graficos_corpo[["G01"]]), "Corpo", "Gráfico", "Participação por ano escolar", "2025–2026", "Mudanças de participação condicionam a composição dos avaliados.", "Módulo 14A; CAEd 2025–2026.", "Percentual; uma casa decimal.", "Reutilizado sem recálculo",
  "G02", caminho_relativo(arquivos_graficos_corpo[["G02"]]), "Corpo", "Gráfico", "Variação da proficiência por ano escolar", "Diferença observada entre 2025 e 2026", "A variação não representa efeito causal do assessoramento.", "Módulo 14A; CAEd 2025–2026.", "Proficiência; uma casa decimal.", "Reutilizado sem recálculo",
  "G03", caminho_relativo(arquivos_graficos_corpo[["G03"]]), "Corpo", "Gráfico", "Distribuição das variações de proficiência", "Heterogeneidade entre escolas", "Resultados extremos exigem leitura conjunta de cobertura e composição.", "Módulo 07; CAEd 2025–2026.", "Proficiência.", "Reutilizado sem recálculo",
  "G04", caminho_relativo(arquivos_graficos_corpo[["G04"]]), "Corpo", "Gráfico", "Participação, composição e proficiência", "Sensibilidade das comparações temporais", "Associação descritiva; a padronização não cria contrafactual causal.", "Módulo 08; CAEd 2025–2026.", "Pontos percentuais e proficiência.", "Reutilizado sem recálculo",
  "G05", caminho_relativo(arquivos_graficos_corpo[["G05"]]), "Corpo", "Gráfico", "Composição administrativa da rede", "Contexto pré-programa de 2024", "Categorias descrevem vínculos administrativos e transições.", "Censo Escolar 2024 e classificação validada.", "Contagem e percentual.", "Novo gráfico",
  "G06", caminho_relativo(arquivos_graficos_corpo[["G06"]]), "Corpo", "Gráfico", "Carga potencial acumulada por carteira", "Comparação entre carteiras nominais", "Carga acumulada combina extensão e composição; não mede qualidade.", "Módulos 17 e 18.", "Índice acumulado; uma casa decimal.", "Novo gráfico",
  "G07", caminho_relativo(arquivos_graficos_corpo[["G07"]]), "Corpo", "Gráfico", "Extensão e intensidade das carteiras", "Número de escolas, intensidade média e carga acumulada", "Médias são referências descritivas, não metas automáticas.", "Módulos 17 e 18.", "Contagem e índice 0–100.", "Novo gráfico",
  "G08", caminho_relativo(arquivos_graficos_corpo[["G08"]]), "Corpo", "Gráfico", "Quatro dimensões por carteira", "Volume, estrutura, desafio educacional e administração", "As dimensões devem permanecer visíveis e ser validadas qualitativamente.", "Módulos 17 e 18.", "Escala relativa 0–100.", "Novo gráfico"
)

catalogo_anexos <- matriz_produtos |>
  filter(destino == "Anexo") |>
  mutate(
    arquivo = map_chr(
      id_produto,
      ~ caminho_relativo(arquivos_tabelas_anexos[[.x]])
    ),
    tipo = "Tabela",
    subtitulo = justificativa,
    nota_metodologica = case_when(
      id_produto %in% c("A02", "A07", "A08") ~
        "Resultados e associações são descritivos e não causais.",
      id_produto %in% c("A03", "A04", "A05", "A06") ~
        "Índices, cenários e posições são relativos e não medem qualidade.",
      TRUE ~ "Produto de consulta e rastreabilidade técnica."
    ),
    fonte = origem_analitica,
    escala_formato = "Valores numéricos preservados para uso técnico.",
    status_producao = decisao
  ) |>
  select(
    id_produto,
    arquivo,
    destino,
    tipo,
    titulo,
    subtitulo,
    nota_metodologica,
    fonte,
    escala_formato,
    status_producao
  )

catalogo_produtos <- bind_rows(
  catalogo_editorial,
  catalogo_anexos
) |>
  arrange(destino, id_produto)

caminho_catalogo <- file.path(
  dir_relatorio,
  "catalogo_produtos_relatorio.csv"
)

write_csv(catalogo_produtos, caminho_catalogo, na = "")

write_csv(
  catalogo_produtos,
  file.path(dir_documentacao, "10_catalogo_editorial_produtos.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 13. Validações finais
# -------------------------------------------------------------------

arquivos_saida_principais <- c(
  arquivos_tabelas_corpo,
  arquivos_graficos_corpo,
  arquivos_tabelas_anexos,
  catalogo = caminho_catalogo
)

validacoes_finais <- tribble(
  ~teste, ~aprovado, ~valor_observado, ~valor_esperado, ~observacao,
  "Universo de 56 escolas", n_distinct(base_final$id_escola) == 56L, as.character(n_distinct(base_final$id_escola)), "56", "Contagem de escolas na base final.",
  "Base com 280 observações", nrow(base_final) == 280L, as.character(nrow(base_final)), "280", "Chave escola × ano escolar × componente.",
  "Painel principal com 265 observações", sum(base_final$painel_resultado_balanceado, na.rm = TRUE) == 265L, as.character(sum(base_final$painel_resultado_balanceado, na.rm = TRUE)), "265", "Resultados disponíveis em ambos os anos.",
  "Onze carteiras nominais", sum(carteiras$carteira_nominal, na.rm = TRUE) == 11L, as.character(sum(carteiras$carteira_nominal, na.rm = TRUE)), "11", "Referência comparativa nominal.",
  "Treze categorias administrativas", nrow(carteiras) == 13L, as.character(nrow(carteiras)), "13", "Inclui duas categorias não nominais.",
  "Uma escola por linha no perfil", nrow(perfil) == 56L && n_distinct(perfil$id_escola) == 56L, paste(nrow(perfil), n_distinct(perfil$id_escola), sep = " / "), "56 / 56", "Linhas e IDs únicos.",
  "Uma escola por linha no índice", nrow(indice) == 56L && n_distinct(indice$id_escola) == 56L, paste(nrow(indice), n_distinct(indice$id_escola), sep = " / "), "56 / 56", "Linhas e IDs únicos.",
  "Índice entre 0 e 100", all(indice$indice_carga_potencial >= 0 & indice$indice_carga_potencial <= 100, na.rm = TRUE), paste0(round(min(indice$indice_carga_potencial, na.rm = TRUE), 2), " a ", round(max(indice$indice_carga_potencial, na.rm = TRUE), 2)), "0 a 100", "Escala relativa declarada.",
  "Quatro dimensões visíveis", setequal(unique(tabela_dimensoes$dimensao), c("Volume", "Complexidade estrutural", "Desafio educacional", "Complexidade administrativa")), paste(sort(unique(tabela_dimensoes$dimensao)), collapse = " | "), "4 dimensões homologadas", "Não colapsar a leitura no índice único.",
  "Carteiras não nominais excluídas das comparações", nrow(tabela_carteiras) == 11L && !any(str_to_lower(tabela_carteiras$carteira) %in% c("outras", "sem vinculação informada")), as.character(nrow(tabela_carteiras)), "11 carteiras nominais", "Categorias residuais permanecem nos anexos.",
  "Gráficos reutilizados idênticos", all(registro_reuso$copia_identica), as.character(sum(registro_reuso$copia_identica)), "4", "Hash MD5 preservado.",
  "Seis tabelas no corpo", all(file.exists(arquivos_tabelas_corpo)), as.character(sum(file.exists(arquivos_tabelas_corpo))), "6", "Arquivos canônicos.",
  "Oito gráficos no corpo", all(file.exists(arquivos_graficos_corpo)), as.character(sum(file.exists(arquivos_graficos_corpo))), "8", "Quatro reutilizados e quatro novos.",
  "Dez tabelas nos anexos", all(file.exists(arquivos_tabelas_anexos)), as.character(sum(file.exists(arquivos_tabelas_anexos))), "10", "Arquivos canônicos.",
  "Todos os produtos possuem conteúdo", all(file.info(arquivos_saida_principais)$size > 0), as.character(sum(file.info(arquivos_saida_principais)$size > 0)), as.character(length(arquivos_saida_principais)), "Arquivos não vazios.",
  "Catálogo editorial completo", nrow(catalogo_produtos) == 24L && !any(is.na(catalogo_produtos$titulo)), as.character(nrow(catalogo_produtos)), "24", "6 tabelas + 8 gráficos + 10 anexos."
)

write_csv(
  validacoes_finais,
  file.path(dir_documentacao, "11_validacao_final.csv"),
  na = ""
)

if (any(!validacoes_finais$aprovado)) {
  stop(
    "Uma ou mais validações finais falharam. Consulte 11_validacao_final.csv."
  )
}

# -------------------------------------------------------------------
# 14. Manifesto de saídas e produtos para homologação
# -------------------------------------------------------------------

manifesto_saida <- tibble(
  id_produto = c(
    names(arquivos_tabelas_corpo),
    names(arquivos_graficos_corpo),
    names(arquivos_tabelas_anexos),
    "CATALOGO"
  ),
  caminho = unname(arquivos_saida_principais)
) |>
  mutate(
    caminho_relativo = map_chr(caminho, caminho_relativo),
    destino = case_when(
      str_detect(id_produto, "^T") ~ "Corpo — tabela",
      str_detect(id_produto, "^G") ~ "Corpo — gráfico",
      str_detect(id_produto, "^A") ~ "Anexo — tabela",
      TRUE ~ "Catálogo"
    ),
    existe = file.exists(caminho),
    tamanho_bytes = file.info(caminho)$size,
    modificacao = format(file.info(caminho)$mtime, "%Y-%m-%d %H:%M:%S"),
    hash_md5 = map_chr(caminho, hash_md5)
  ) |>
  select(
    id_produto,
    destino,
    caminho_relativo,
    existe,
    tamanho_bytes,
    modificacao,
    hash_md5
  )

write_csv(
  manifesto_saida,
  file.path(dir_documentacao, "12_manifesto_produtos_modulo_21.csv"),
  na = ""
)

produtos_homologacao <- bind_rows(
  manifesto_saida |>
    filter(str_detect(id_produto, "^(T|G)")) |>
    mutate(
      prioridade = if_else(str_detect(id_produto, "^G"), "Alta", "Alta"),
      criterio_homologacao = case_when(
        str_detect(id_produto, "^T") ~
          "Conferir seleção de indicadores, arredondamentos, rótulos e cautelas.",
        id_produto %in% c("G01", "G02", "G03", "G04") ~
          "Confirmar adequação editorial do gráfico homologado reutilizado.",
        TRUE ~
          "Conferir legibilidade, ordem, escalas, cores, títulos, notas e fontes."
      )
    ),
  manifesto_saida |>
    filter(id_produto == "CATALOGO") |>
    mutate(
      prioridade = "Alta",
      criterio_homologacao =
        "Confirmar arquitetura corpo/anexos e metadados editoriais."
    ),
  tibble(
    id_produto = c("DIAG_MATRIZ", "DIAG_VALIDACAO", "DIAG_MANIFESTO"),
    destino = "Documentação",
    caminho_relativo = c(
      caminho_relativo(file.path(dir_documentacao, "08_matriz_produtos_relatorio.csv")),
      caminho_relativo(file.path(dir_documentacao, "11_validacao_final.csv")),
      caminho_relativo(file.path(dir_documentacao, "12_manifesto_produtos_modulo_21.csv"))
    ),
    existe = TRUE,
    tamanho_bytes = NA_real_,
    modificacao = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    hash_md5 = NA_character_,
    prioridade = "Alta",
    criterio_homologacao = c(
      "Confirmar decisões de reuso, produção e separação corpo/anexos.",
      "Confirmar aprovação de todos os testes.",
      "Confirmar existência, tamanho e hashes dos produtos."
    )
  )
) |>
  arrange(desc(prioridade), id_produto)

write_csv(
  produtos_homologacao,
  file.path(dir_documentacao, "13_produtos_para_homologacao.csv"),
  na = ""
)

# -------------------------------------------------------------------
# 15. Informações da sessão e resumo de execução
# -------------------------------------------------------------------

writeLines(
  capture.output(sessionInfo()),
  file.path(dir_documentacao, "14_session_info.txt"),
  useBytes = TRUE
)

resumo_execucao <- c(
  "MÓDULO 21 — TABELAS E GRÁFICOS PARA O RELATÓRIO",
  paste0("Execução: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("Diretório do projeto: ", diretorio_projeto),
  "",
  "Escopo validado:",
  paste0("- Escolas: ", n_distinct(base_final$id_escola)),
  paste0("- Observações escola × ano escolar: ", nrow(base_final)),
  paste0(
    "- Observações comparáveis: ",
    sum(base_final$painel_resultado_balanceado, na.rm = TRUE)
  ),
  paste0("- Carteiras nominais: ", sum(carteiras$carteira_nominal, na.rm = TRUE)),
  paste0("- Categorias administrativas: ", nrow(carteiras)),
  "",
  "Produtos:",
  paste0("- Tabelas para o corpo: ", length(arquivos_tabelas_corpo)),
  paste0("- Gráficos para o corpo: ", length(arquivos_graficos_corpo)),
  paste0("  - reutilizados sem recálculo: ", nrow(registro_reuso)),
  paste0("  - novos: ", length(arquivos_graficos_corpo) - nrow(registro_reuso)),
  paste0("- Tabelas para anexos: ", length(arquivos_tabelas_anexos)),
  "",
  "Cautelas:",
  "- O estudo é observacional e descritivo; não há identificação causal.",
  "- Resultados escolares não devem ser atribuídos às assessoras.",
  "- O índice representa carga potencial relativa, não qualidade.",
  "- As quatro dimensões devem permanecer visíveis.",
  "- Faixas e posições são relativas e não constituem ranking de qualidade.",
  "- Redistribuições exigem validação qualitativa, territorial e operacional.",
  "",
  "Próximo passo:",
  "- Devolver os produtos indicados em 13_produtos_para_homologacao.csv.",
  "- Aguardar a homologação antes de iniciar o módulo 22."
)

writeLines(
  resumo_execucao,
  file.path(dir_documentacao, "15_resumo_execucao.txt"),
  useBytes = TRUE
)

cat(
  paste0(
    "\nMódulo 21 concluído com sucesso.\n",
    "Tabelas do corpo: ", length(arquivos_tabelas_corpo), "\n",
    "Gráficos do corpo: ", length(arquivos_graficos_corpo), "\n",
    "Tabelas dos anexos: ", length(arquivos_tabelas_anexos), "\n",
    "Validações aprovadas: ", sum(validacoes_finais$aprovado),
    " de ", nrow(validacoes_finais), "\n",
    "Documentação: ", caminho_relativo(dir_documentacao), "\n",
    "Aguarde a homologação antes de avançar ao módulo 22.\n"
  )
)
