# ===================================================================
# 11C_consolidar_cadastro_exposicao_assessoramento.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# ===================================================================
#
# OBJETIVO
#
# Construir e validar o cadastro canônico escola × período que separa:
#
#   1. presença observada nas avaliações;
#   2. elegibilidade institucional ao assessoramento;
#   3. exposição institucional ao programa;
#   4. vínculo gerencial da escola;
#   5. inclusão no índice operacional de carga.
#
# PRINCÍPIOS
#
# - Presença no CAEd não comprova assessoramento.
# - Rede/dependência administrativa não determina exposição.
# - A primeira avaliação formativa de 2025 é linha de base pré-programa.
# - Exposição binária não representa dose observada.
# - Escolas não assessoradas não entram no índice de carga.
# - O grupo não exposto é apenas referência descritiva, não controle causal.
# - Novas ondas pós-início exigem homologação escola × período.
#
# ENTRADAS
#
# dados_manuais/cadastro_exposicao_assessoramento.csv
# dados_intermediarios/dim_escola.csv
# dados_processados/painel_escola_serie_ano.csv
#
# PRODUTOS PRINCIPAIS
#
# dados_intermediarios/cadastro_exposicao_assessoramento.csv
# dados_intermediarios/cadastro_exposicao_assessoramento.rds
# documentacao/cadastro_exposicao/execucao_<data_hora>/...
#
# REVISAO DESTA VERSAO
#
# - Corrigido o sombreamento de nomes dentro de tibble() que quebrava a
#   validacao de colunas obrigatorias (erro "nenhum indice no nivel 2").
# - Transliteracao de acentos deixou de depender do locale (iconv).
# - Datas passam por parser explicito, sem interpretacao ambigua.
# - Sinalizadores de inclusao nunca ficam NA; validacoes tratam ausencia.
# - As invariantes homologadas de 2025-2026 permanecem bloqueantes.
# - Encoding de cada entrada e detectado e registrado no manifesto.
# - Duplicidades da dimensao ganharam diagnostico proprio sem renumerar
#   os produtos que ja compoem o protocolo de devolucao do modulo.
# ===================================================================

library(here)
library(tidyverse)

# -------------------------------------------------------------------
# 1. Identificação da execução e diretórios
# -------------------------------------------------------------------

id_execucao <- format(Sys.time(), "%Y%m%d_%H%M%S")

pasta_documentacao <- here("documentacao", "cadastro_exposicao")
pasta_execucao <- here(
  "documentacao",
  "cadastro_exposicao",
  paste0("execucao_", id_execucao)
)
pasta_historico <- here(
  "dados_intermediarios",
  "historico",
  "cadastro_exposicao",
  paste0("execucao_", id_execucao)
)

walk(
  c(
    here("dados_intermediarios"),
    pasta_documentacao,
    pasta_execucao,
    pasta_historico
  ),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# -------------------------------------------------------------------
# 2. Arquivos de entrada e saída
# -------------------------------------------------------------------

arquivos_entrada <- c(
  cadastro_manual = here(
    "dados_manuais",
    "cadastro_exposicao_assessoramento.csv"
  ),
  dim_escola = here(
    "dados_intermediarios",
    "dim_escola.csv"
  ),
  painel = here(
    "dados_processados",
    "painel_escola_serie_ano.csv"
  )
)

arquivos_saida <- c(
  cadastro_csv = here(
    "dados_intermediarios",
    "cadastro_exposicao_assessoramento.csv"
  ),
  cadastro_rds = here(
    "dados_intermediarios",
    "cadastro_exposicao_assessoramento.rds"
  )
)

# Snapshot institucional homologado. Novas ondas podem ser acrescentadas
# ao cadastro manual sem alterar estes parâmetros. Eles só devem mudar
# quando houver nova decisão institucional formal sobre 2025-2026.

snapshot_homologado <- list(
  ano_linha_base = 2025L,
  ano_pos_inicio = 2026L,
  avaliacao_id = "1a_avaliacao_formativa",
  escolas_avaliadas_linha_base = 54L,
  escolas_avaliadas_pos_inicio = 56L,
  escolas_assessoradas_pos_inicio = 53L,
  escolas_inelegiveis_pos_inicio = 3L,
  escolas_indice_carga_pos_inicio = 53L,
  codigos_inelegiveis_pos_inicio = sort(
    c("43105416", "43105300", "43189768")
  ),
  data_homologacao_pos_inicio = as.Date("2026-07-24")
)

arquivos_ausentes <- arquivos_entrada[
  !file.exists(arquivos_entrada)
]

if (length(arquivos_ausentes) > 0) {
  stop(
    "Arquivos necessários não encontrados:\n",
    paste(arquivos_ausentes, collapse = "\n")
  )
}

# -------------------------------------------------------------------
# 3. Funções auxiliares
# -------------------------------------------------------------------

# Transliteracao independente de locale. iconv(from = "", to =
# "ASCII//TRANSLIT") produzia resultados diferentes conforme a maquina:
# em locale UTF-8 devolve "1a Avaliacao"; em locale C devolve
# "1? Avalia??o", corrompendo silenciosamente todas as chaves.

transliterar_ascii <- function(x) {
  x <- as.character(x)

  # Indicadores ordinais nao sao convertidos pelos transliteradores
  # padrao, e o projeto depende deles: "1\u00aa Avalia\u00e7\u00e3o" precisa
  # gerar a chave 1a_avaliacao_formativa, e nao 1_avaliacao_formativa.

  x <- chartr("\u00aa\u00ba", "ao", x)

  if (requireNamespace("stringi", quietly = TRUE)) {
    return(stringi::stri_trans_general(x, "Latin-ASCII"))
  }

  acentuados <- paste0(
    "\u00e1\u00e0\u00e2\u00e3\u00e4\u00e9\u00e8\u00ea\u00eb",
    "\u00ed\u00ec\u00ee\u00ef\u00f3\u00f2\u00f4\u00f5\u00f6",
    "\u00fa\u00f9\u00fb\u00fc\u00e7\u00f1",
    "\u00c1\u00c0\u00c2\u00c3\u00c4\u00c9\u00c8\u00ca\u00cb",
    "\u00cd\u00cc\u00ce\u00cf\u00d3\u00d2\u00d4\u00d5\u00d6",
    "\u00da\u00d9\u00db\u00dc\u00c7\u00d1",
    "\u00aa\u00ba"
  )

  simples <- paste0(
    "aaaaaeeee",
    "iiiiooooo",
    "uuuucn",
    "AAAAAEEEE",
    "IIIIOOOOO",
    "UUUUCN",
    "ao"
  )

  chartr(acentuados, simples, x)
}

normalizar_chave <- function(x) {
  x |>
    as.character() |>
    transliterar_ascii() |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_+|_+$", "") |>
    na_if("")
}

normalizar_texto_comparacao <- function(x) {
  x |>
    as.character() |>
    transliterar_ascii() |>
    str_to_upper() |>
    str_squish() |>
    na_if("")
}

limpar_texto <- function(x) {
  x |>
    as.character() |>
    str_squish() |>
    na_if("")
}

# as.Date() sem format nao avisa e nao falha diante de "24/07/2026":
# devolve 0024-07-20. O parser abaixo e explicito e prioriza ISO;
# o que nao converter fica NA e e capturado pelo diagnostico de
# metadados incompletos da fonte manual.

converter_data <- function(x) {
  texto <- limpar_texto(x)
  resultado <- rep(as.Date(NA), length(texto))

  formatos <- c(
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%d-%m-%Y",
    "%Y/%m/%d"
  )

  for (formato in formatos) {
    pendentes <- is.na(resultado) & !is.na(texto)

    if (!any(pendentes)) {
      break
    }

    resultado[pendentes] <- as.Date(
      texto[pendentes],
      format = formato
    )
  }

  resultado
}

# Detecta o encoding de cada entrada. CSV salvo pelo Excel em pt-BR
# costuma vir em Latin-1; lido como UTF-8, quebra os acentos e produz
# divergencias de nome que nao existem na realidade.

detectar_encoding_arquivo <- function(caminho) {
  if (!file.exists(caminho)) {
    return(NA_character_)
  }

  linhas <- readLines(
    caminho,
    warn = FALSE,
    skipNul = TRUE,
    encoding = "bytes"
  )

  if (all(validUTF8(linhas))) "UTF-8" else "Latin1"
}

como_texto <- function(x) {
  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = " | ")
}

# Os parametros nao repetem os nomes das colunas criadas: tibble()
# avalia os argumentos em sequencia e cada um enxerga as colunas ja
# criadas, de modo que "fonte = fonte" faria o argumento desaparecer
# atras da coluna homonima nos argumentos seguintes.

inventariar_estrutura <- function(dados, nome_fonte) {
  map_dfr(
    names(dados),
    function(nome_variavel) {
      valores <- dados[[nome_variavel]]

      tibble(
        fonte = nome_fonte,
        ordem_coluna = match(nome_variavel, names(dados)),
        variavel = nome_variavel,
        classe_r = paste(class(valores), collapse = " | "),
        numero_na = sum(is.na(valores)),
        numero_distintos = n_distinct(valores, na.rm = TRUE)
      )
    }
  )
}

registrar_validacao <- function(
  codigo_teste,
  texto_descricao,
  valor_observado,
  valor_esperado,
  condicao_aprovacao,
  nivel_criticidade = "ERRO"
) {
  tibble(
    id_teste = codigo_teste,
    descricao = texto_descricao,
    observado = como_texto(valor_observado),
    esperado = como_texto(valor_esperado),
    criticidade = nivel_criticidade,
    status = if (isTRUE(condicao_aprovacao)) {
      "OK"
    } else {
      nivel_criticidade
    }
  )
}

valor_resumo <- function(dados, nome_variavel) {
  if (nrow(dados) != 1 || !nome_variavel %in% names(dados)) {
    return(NA_integer_)
  }

  as.integer(dados[[nome_variavel]][[1]])
}

hash_md5 <- function(caminho) {
  if (!file.exists(caminho)) {
    return(NA_character_)
  }

  unname(tools::md5sum(caminho))
}

# -------------------------------------------------------------------
# 4. Leitura e contrato de colunas
# -------------------------------------------------------------------

encodings_entrada <- map_chr(
  arquivos_entrada,
  detectar_encoding_arquivo
)

cadastro_manual_bruto <- read_csv(
  arquivos_entrada[["cadastro_manual"]],
  show_col_types = FALSE,
  col_types = cols(.default = col_character()),
  locale = locale(
    encoding = encodings_entrada[["cadastro_manual"]]
  ),
  na = c("", "NA")
)

dim_escola_bruta <- read_csv(
  arquivos_entrada[["dim_escola"]],
  show_col_types = FALSE,
  col_types = cols(.default = col_character()),
  locale = locale(
    encoding = encodings_entrada[["dim_escola"]]
  ),
  na = c("", "NA")
)

painel_bruto <- read_csv(
  arquivos_entrada[["painel"]],
  show_col_types = FALSE,
  col_types = cols(
    ano = col_integer(),
    id_escola = col_character(),
    avaliacao = col_character(),
    periodo_programa = col_character(),
    .default = col_guess()
  ),
  locale = locale(
    encoding = encodings_entrada[["painel"]]
  ),
  na = c("", "NA")
)

colunas_obrigatorias <- list(
  cadastro_manual = c(
    "id_escola",
    "codigo_inep",
    "nome_canonico",
    "ano",
    "avaliacao_id",
    "elegivel_assessoramento",
    "recebe_assessoramento",
    "assessora_gerencial",
    "status_carga_operacional",
    "fonte_validacao",
    "data_validacao",
    "responsavel_validacao",
    "observacao_validacao",
    "status_homologacao",
    "versao_regra"
  ),
  dim_escola = c(
    "id_escola",
    "codigo_inep",
    "nome_canonico",
    "assessora"
  ),
  painel = c(
    "ano",
    "id_escola",
    "avaliacao",
    "periodo_programa"
  )
)

bases_brutas <- list(
  cadastro_manual = cadastro_manual_bruto,
  dim_escola = dim_escola_bruta,
  painel = painel_bruto
)

# O parametro se chama nome_fonte, e nao fonte, e a consulta a
# bases_brutas acontece fora do tibble(). Na versao anterior,
# "fonte = fonte" criava a coluna fonte, ja reciclada para o
# comprimento de coluna; no argumento seguinte, bases_brutas[[fonte]]
# recebia um vetor longo e virava indexacao recursiva, falhando com
# "nenhum indice no nivel 2".

validacao_colunas <- imap_dfr(
  colunas_obrigatorias,
  function(colunas, nome_fonte) {
    colunas_presentes <- names(bases_brutas[[nome_fonte]])

    tibble(
      fonte = nome_fonte,
      coluna = colunas,
      presente = colunas %in% colunas_presentes
    )
  }
)

write_csv(
  validacao_colunas,
  file.path(
    pasta_execucao,
    "03_validacao_colunas_obrigatorias.csv"
  ),
  na = ""
)

if (any(!validacao_colunas$presente)) {
  stop(
    "Há colunas obrigatórias ausentes. Consulte 03_validacao_",
    "colunas_obrigatorias.csv em:\n",
    pasta_execucao
  )
}

# -------------------------------------------------------------------
# 5. Padronização das fontes
# -------------------------------------------------------------------

cadastro_manual <- cadastro_manual_bruto |>
  transmute(
    id_escola = limpar_texto(id_escola),
    codigo_inep_manual = limpar_texto(codigo_inep),
    nome_canonico_manual = limpar_texto(nome_canonico),
    ano = suppressWarnings(as.integer(ano)),
    avaliacao_id = normalizar_chave(avaliacao_id),
    elegivel_manual = normalizar_texto_comparacao(
      elegivel_assessoramento
    ),
    recebe_manual = normalizar_texto_comparacao(
      recebe_assessoramento
    ),
    assessora_manual = normalizar_chave(assessora_gerencial),
    status_carga_manual = normalizar_texto_comparacao(
      status_carga_operacional
    ),
    fonte_manual = limpar_texto(fonte_validacao),
    data_validacao_manual = converter_data(data_validacao),
    responsavel_manual = limpar_texto(responsavel_validacao),
    observacao_manual = limpar_texto(observacao_validacao),
    status_homologacao_manual = normalizar_texto_comparacao(
      status_homologacao
    ),
    versao_regra_manual = limpar_texto(versao_regra)
  )

dim_escola <- dim_escola_bruta |>
  transmute(
    id_escola = limpar_texto(id_escola),
    codigo_inep = limpar_texto(codigo_inep),
    nome_canonico = limpar_texto(nome_canonico),
    assessora_vinculo_administrativo = normalizar_chave(assessora)
  ) |>
  distinct()

periodos_observados_brutos <- painel_bruto |>
  transmute(
    ano = as.integer(ano),
    avaliacao_id = normalizar_chave(avaliacao),
    avaliacao_rotulo = limpar_texto(avaliacao),
    periodo_programa = normalizar_chave(periodo_programa)
  ) |>
  distinct()

conflitos_periodos <- periodos_observados_brutos |>
  count(
    ano,
    avaliacao_id,
    name = "numero_combinacoes_rotulo_periodo"
  ) |>
  filter(numero_combinacoes_rotulo_periodo > 1)

periodos_observados <- periodos_observados_brutos |>
  group_by(ano, avaliacao_id) |>
  summarise(
    avaliacao_rotulo = first(avaliacao_rotulo),
    periodo_programa = first(periodo_programa),
    .groups = "drop"
  )

presenca_avaliativa <- painel_bruto |>
  transmute(
    id_escola = limpar_texto(id_escola),
    ano = as.integer(ano),
    avaliacao_id = normalizar_chave(avaliacao)
  ) |>
  distinct() |>
  mutate(presente_avaliacao = TRUE)

# -------------------------------------------------------------------
# 6. Diagnósticos das chaves e da homologação manual
# -------------------------------------------------------------------

duplicidades_manual <- cadastro_manual |>
  count(
    id_escola,
    ano,
    avaliacao_id,
    name = "numero_registros"
  ) |>
  filter(numero_registros > 1)

duplicidades_dim <- dim_escola |>
  count(id_escola, name = "numero_registros") |>
  filter(numero_registros > 1)

registros_dim_chave_incompleta <- dim_escola |>
  filter(
    is.na(id_escola) |
      is.na(codigo_inep) |
      is.na(nome_canonico)
  )

ids_manuais_desconhecidos <- cadastro_manual |>
  anti_join(dim_escola, by = "id_escola")

ids_painel_desconhecidos <- presenca_avaliativa |>
  distinct(id_escola) |>
  anti_join(
    dim_escola |>
      distinct(id_escola),
    by = "id_escola"
  )

divergencias_identificacao <- cadastro_manual |>
  inner_join(dim_escola, by = "id_escola") |>
  mutate(
    codigo_divergente =
      codigo_inep_manual != codigo_inep,
    nome_divergente =
      normalizar_texto_comparacao(nome_canonico_manual) !=
        normalizar_texto_comparacao(nome_canonico)
  ) |>
  filter(codigo_divergente | nome_divergente) |>
  select(
    id_escola,
    codigo_inep_manual,
    codigo_inep,
    nome_canonico_manual,
    nome_canonico,
    codigo_divergente,
    nome_divergente
  )

registros_manuais_sem_periodo <- cadastro_manual |>
  anti_join(
    periodos_observados,
    by = c("ano", "avaliacao_id")
  )

inconsistencias_manual <- cadastro_manual |>
  mutate(
    motivo = case_when(
      is.na(id_escola) |
        is.na(ano) |
        is.na(avaliacao_id) ~
        "Chave incompleta",

      !elegivel_manual %in% c("SIM", "NAO") ~
        "Elegibilidade fora do domínio SIM/NAO",

      !recebe_manual %in% c("SIM", "NAO") ~
        "Recebimento fora do domínio SIM/NAO",

      status_homologacao_manual != "HOMOLOGADO" ~
        "Registro pós-início não homologado",

      is.na(fonte_manual) |
        is.na(data_validacao_manual) |
        is.na(responsavel_manual) |
        is.na(versao_regra_manual) ~
        "Metadados de validação incompletos",

      elegivel_manual == "NAO" &
        recebe_manual != "NAO" ~
        "Escola inelegível marcada como assessorada",

      recebe_manual == "SIM" &
        (
          elegivel_manual != "SIM" |
            is.na(assessora_manual) |
            status_carga_manual != "ATIVA"
        ) ~
        "Exposição ativa sem elegibilidade, assessora ou carga ativa",

      recebe_manual == "NAO" &
        (
          !is.na(assessora_manual) |
            status_carga_manual !=
              "ZERO_OU_TENDENTE_A_ZERO"
        ) ~
        "Não exposição com assessora ou carga incompatível",

      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(motivo))

# -------------------------------------------------------------------
# 7. Construção do cadastro escola × período
# -------------------------------------------------------------------

grade_escola_periodo <- crossing(
  dim_escola,
  periodos_observados
) |>
  left_join(
    presenca_avaliativa,
    by = c(
      "id_escola",
      "ano",
      "avaliacao_id"
    )
  ) |>
  mutate(
    presente_avaliacao = coalesce(
      presente_avaliacao,
      FALSE
    ),
    linha_base_pre_programa = coalesce(
      periodo_programa == "linha_base_pre_programa",
      FALSE
    )
  )

grade_integrada <- grade_escola_periodo |>
  left_join(
    cadastro_manual,
    by = c(
      "id_escola",
      "ano",
      "avaliacao_id"
    )
  )

periodos_pos_sem_homologacao <- grade_integrada |>
  filter(
    !linha_base_pre_programa,
    is.na(status_homologacao_manual)
  ) |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    ano,
    avaliacao_id,
    periodo_programa
  )

cadastro_canonico <- grade_integrada |>
  mutate(
    pertence_universo_avaliativo = if_else(
      presente_avaliacao,
      "SIM",
      "NAO"
    ),

    elegivel_assessoramento = case_when(
      linha_base_pre_programa ~ "NA_APLICAVEL",
      TRUE ~ elegivel_manual
    ),

    recebe_assessoramento = case_when(
      linha_base_pre_programa ~ "NAO",
      TRUE ~ recebe_manual
    ),

    assessora_gerencial = case_when(
      linha_base_pre_programa ~ NA_character_,
      recebe_assessoramento == "SIM" ~ assessora_manual,
      TRUE ~ NA_character_
    ),

    status_carga_operacional = case_when(
      linha_base_pre_programa ~ "SEM_CARGA_DO_PROGRAMA",
      TRUE ~ status_carga_manual
    ),

    fonte_validacao = case_when(
      linha_base_pre_programa ~
        "regra temporal pre-programa registrada no painel",
      TRUE ~ fonte_manual
    ),

    data_validacao = case_when(
      linha_base_pre_programa ~ as.Date(NA),
      TRUE ~ data_validacao_manual
    ),

    responsavel_validacao = case_when(
      linha_base_pre_programa ~
        "regra temporal preexistente do projeto",
      TRUE ~ responsavel_manual
    ),

    observacao_validacao = case_when(
      linha_base_pre_programa ~
        "Primeira avaliação formativa anterior ao início do assessoramento.",
      TRUE ~ observacao_manual
    ),

    status_homologacao = case_when(
      linha_base_pre_programa ~
        "REGRA_TEMPORAL_CONFIRMADA",
      TRUE ~ status_homologacao_manual
    ),

    versao_regra = case_when(
      linha_base_pre_programa ~
        paste0(
          ano,
          "_",
          avaliacao_id,
          "_linha_base"
        ),
      TRUE ~ versao_regra_manual
    ),

    exposicao_programa_binaria = if_else(
      recebe_assessoramento == "SIM",
      1L,
      0L,
      missing = NA_integer_
    ),

    grupo_exposicao = case_when(
      linha_base_pre_programa ~
        "LINHA_BASE_PRE_PROGRAMA",
      recebe_assessoramento == "SIM" ~
        "ASSESSORADA",
      recebe_assessoramento == "NAO" &
        elegivel_assessoramento == "NAO" ~
        "NAO_EXPOSTA_INELEGIVEL",
      recebe_assessoramento == "NAO" &
        elegivel_assessoramento == "SIM" ~
        "NAO_EXPOSTA_ELEGIVEL",
      TRUE ~
        "STATUS_INCONSISTENTE"
    ),

    # Sinalizadores de inclusao nunca ficam NA: o desconhecido nao
    # entra em recorte algum. A ausencia de homologacao continua
    # sendo denunciada por V07, e nao silenciada aqui.

    incluir_diagnostico_avaliativo_ampliado = coalesce(
      pertence_universo_avaliativo == "SIM",
      FALSE
    ),

    incluir_resultados_rede_assessorada = coalesce(
      pertence_universo_avaliativo == "SIM" &
        recebe_assessoramento == "SIM",
      FALSE
    ),

    incluir_indice_carga = coalesce(
      elegivel_assessoramento == "SIM" &
        recebe_assessoramento == "SIM" &
        status_carga_operacional == "ATIVA" &
        status_homologacao == "HOMOLOGADO",
      FALSE
    ),

    incluir_nao_exposto_descritivo = coalesce(
      !linha_base_pre_programa &
        pertence_universo_avaliativo == "SIM" &
        recebe_assessoramento == "NAO",
      FALSE
    ),

    chave_periodo = paste(
      ano,
      avaliacao_id,
      sep = "_"
    ),

    id_escola_periodo = paste(
      id_escola,
      chave_periodo,
      sep = "__"
    )
  ) |>
  select(
    id_escola_periodo,
    id_escola,
    codigo_inep,
    nome_canonico,
    ano,
    avaliacao_id,
    avaliacao_rotulo,
    chave_periodo,
    periodo_programa,
    pertence_universo_avaliativo,
    elegivel_assessoramento,
    recebe_assessoramento,
    exposicao_programa_binaria,
    assessora_vinculo_administrativo,
    assessora_gerencial,
    status_carga_operacional,
    grupo_exposicao,
    incluir_diagnostico_avaliativo_ampliado,
    incluir_resultados_rede_assessorada,
    incluir_indice_carga,
    incluir_nao_exposto_descritivo,
    fonte_validacao,
    data_validacao,
    responsavel_validacao,
    observacao_validacao,
    status_homologacao,
    versao_regra
  ) |>
  arrange(ano, avaliacao_id, id_escola)

# -------------------------------------------------------------------
# 8. Diagnósticos substantivos
# -------------------------------------------------------------------

duplicidades_canonico <- cadastro_canonico |>
  count(
    id_escola,
    ano,
    avaliacao_id,
    name = "numero_registros"
  ) |>
  filter(numero_registros > 1)

inconsistencias_canonico <- cadastro_canonico |>
  mutate(
    motivo = case_when(
      elegivel_assessoramento == "NAO" &
        recebe_assessoramento != "NAO" ~
        "Inelegível marcada como assessorada",

      recebe_assessoramento == "SIM" &
        (
          elegivel_assessoramento != "SIM" |
            is.na(assessora_gerencial) |
            status_carga_operacional != "ATIVA"
        ) ~
        "Exposição ativa inconsistente",

      recebe_assessoramento == "NAO" &
        incluir_indice_carga ~
        "Não assessorada incluída no índice de carga",

      grupo_exposicao == "NAO_EXPOSTA_INELEGIVEL" &
        incluir_resultados_rede_assessorada ~
        "Não exposta incluída na rede assessorada",

      periodo_programa == "linha_base_pre_programa" &
        exposicao_programa_binaria != 0L ~
        "Linha de base com exposição positiva",

      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(motivo))

resumo_universos <- cadastro_canonico |>
  group_by(
    ano,
    avaliacao_id,
    avaliacao_rotulo,
    periodo_programa,
    chave_periodo
  ) |>
  summarise(
    escolas_cadastradas = n_distinct(id_escola),
    escolas_universo_avaliativo = sum(
      pertence_universo_avaliativo == "SIM"
    ),
    escolas_assessoradas = sum(
      pertence_universo_avaliativo == "SIM" &
        recebe_assessoramento == "SIM"
    ),
    escolas_avaliadas_nao_assessoradas = sum(
      pertence_universo_avaliativo == "SIM" &
        recebe_assessoramento == "NAO"
    ),
    escolas_elegiveis = sum(
      elegivel_assessoramento == "SIM"
    ),
    escolas_inelegiveis = sum(
      elegivel_assessoramento == "NAO"
    ),
    escolas_indice_carga = sum(incluir_indice_carga),
    identidade_universos_valida =
      escolas_universo_avaliativo ==
      escolas_assessoradas +
      escolas_avaliadas_nao_assessoradas,
    .groups = "drop"
  )

conveniadas_2026 <- cadastro_canonico |>
  filter(
    ano == snapshot_homologado$ano_pos_inicio,
    codigo_inep %in%
      snapshot_homologado$codigos_inelegiveis_pos_inicio
  ) |>
  select(
    id_escola,
    codigo_inep,
    nome_canonico,
    elegivel_assessoramento,
    recebe_assessoramento,
    status_carga_operacional,
    incluir_indice_carga,
    grupo_exposicao,
    data_validacao,
    versao_regra
  )

# -------------------------------------------------------------------
# 9. Validação final
# -------------------------------------------------------------------

resumo_2025 <- resumo_universos |>
  filter(
    ano == snapshot_homologado$ano_linha_base,
    avaliacao_id == snapshot_homologado$avaliacao_id
  )

resumo_2026 <- resumo_universos |>
  filter(
    ano == snapshot_homologado$ano_pos_inicio,
    avaliacao_id == snapshot_homologado$avaliacao_id
  )

codigos_inelegiveis_2026 <- cadastro_canonico |>
  filter(
    ano == snapshot_homologado$ano_pos_inicio,
    elegivel_assessoramento == "NAO"
  ) |>
  pull(codigo_inep) |>
  sort()

codigos_inelegiveis_esperados <-
  snapshot_homologado$codigos_inelegiveis_pos_inicio

datas_validacao_2026 <- cadastro_canonico |>
  filter(
    ano == snapshot_homologado$ano_pos_inicio,
    periodo_programa != "linha_base_pre_programa"
  ) |>
  pull(data_validacao)

exposicoes_linha_base <- cadastro_canonico |>
  filter(ano == snapshot_homologado$ano_linha_base) |>
  pull(exposicao_programa_binaria)

validacao_final <- bind_rows(
  registrar_validacao(
    "V01",
    "Dimensão de escolas possui chave única",
    nrow(duplicidades_dim),
    0,
    nrow(duplicidades_dim) == 0
  ),
  registrar_validacao(
    "V01A",
    "Dimensão de escolas possui chaves e identificação completas",
    nrow(registros_dim_chave_incompleta),
    0,
    nrow(registros_dim_chave_incompleta) == 0
  ),
  registrar_validacao(
    "V02",
    "Fonte manual possui chave escola × período única",
    nrow(duplicidades_manual),
    0,
    nrow(duplicidades_manual) == 0
  ),
  registrar_validacao(
    "V03",
    "Fonte manual contém somente escolas conhecidas",
    nrow(ids_manuais_desconhecidos),
    0,
    nrow(ids_manuais_desconhecidos) == 0
  ),
  registrar_validacao(
    "V03A",
    "Painel contém somente escolas conhecidas na dimensão",
    nrow(ids_painel_desconhecidos),
    0,
    nrow(ids_painel_desconhecidos) == 0
  ),
  registrar_validacao(
    "V04",
    "Código INEP e nome coincidem com a dimensão",
    nrow(divergencias_identificacao),
    0,
    nrow(divergencias_identificacao) == 0
  ),
  registrar_validacao(
    "V05",
    "Períodos do painel possuem definição temporal única",
    nrow(conflitos_periodos),
    0,
    nrow(conflitos_periodos) == 0
  ),
  registrar_validacao(
    "V06",
    "Fonte manual não contém período inexistente no painel",
    nrow(registros_manuais_sem_periodo),
    0,
    nrow(registros_manuais_sem_periodo) == 0
  ),
  registrar_validacao(
    "V07",
    "Todas as escolas em períodos pós-início possuem homologação",
    nrow(periodos_pos_sem_homologacao),
    0,
    nrow(periodos_pos_sem_homologacao) == 0
  ),
  registrar_validacao(
    "V08",
    "Regras da fonte manual são internamente coerentes",
    nrow(inconsistencias_manual),
    0,
    nrow(inconsistencias_manual) == 0
  ),
  registrar_validacao(
    "V09",
    "Cadastro canônico possui chave única",
    nrow(duplicidades_canonico),
    0,
    nrow(duplicidades_canonico) == 0
  ),
  registrar_validacao(
    "V10",
    "Cadastro canônico cobre escola × todos os períodos observados",
    nrow(cadastro_canonico),
    n_distinct(dim_escola$id_escola) * nrow(periodos_observados),
    nrow(cadastro_canonico) ==
      n_distinct(dim_escola$id_escola) * nrow(periodos_observados)
  ),
  registrar_validacao(
    "V11",
    "Cadastro canônico não contém inconsistências substantivas",
    nrow(inconsistencias_canonico),
    0,
    nrow(inconsistencias_canonico) == 0
  ),
  registrar_validacao(
    "V12",
    "Identidade avaliada = assessorada + avaliada não assessorada",
    sum(
      !coalesce(
        resumo_universos$identidade_universos_valida,
        FALSE
      )
    ),
    0,
    all(
      coalesce(
        resumo_universos$identidade_universos_valida,
        FALSE
      )
    )
  ),
  registrar_validacao(
    "V13",
    "Universo avaliativo observado em 2025",
    valor_resumo(
      resumo_2025,
      "escolas_universo_avaliativo"
    ),
    snapshot_homologado$escolas_avaliadas_linha_base,
    nrow(resumo_2025) == 1 &&
      resumo_2025$escolas_universo_avaliativo ==
        snapshot_homologado$escolas_avaliadas_linha_base
  ),
  registrar_validacao(
    "V14",
    "Universo avaliativo observado em 2026",
    valor_resumo(
      resumo_2026,
      "escolas_universo_avaliativo"
    ),
    snapshot_homologado$escolas_avaliadas_pos_inicio,
    nrow(resumo_2026) == 1 &&
      resumo_2026$escolas_universo_avaliativo ==
        snapshot_homologado$escolas_avaliadas_pos_inicio
  ),
  registrar_validacao(
    "V15",
    "Escolas efetivamente assessoradas em 2026",
    valor_resumo(
      resumo_2026,
      "escolas_assessoradas"
    ),
    snapshot_homologado$escolas_assessoradas_pos_inicio,
    nrow(resumo_2026) == 1 &&
      resumo_2026$escolas_assessoradas ==
        snapshot_homologado$escolas_assessoradas_pos_inicio
  ),
  registrar_validacao(
    "V16",
    "Escolas inelegíveis em 2026",
    valor_resumo(
      resumo_2026,
      "escolas_inelegiveis"
    ),
    snapshot_homologado$escolas_inelegiveis_pos_inicio,
    nrow(resumo_2026) == 1 &&
      resumo_2026$escolas_inelegiveis ==
        snapshot_homologado$escolas_inelegiveis_pos_inicio
  ),
  registrar_validacao(
    "V17",
    "Inelegíveis de 2026 são exatamente as três conveniadas",
    paste(codigos_inelegiveis_2026, collapse = " | "),
    paste(codigos_inelegiveis_esperados, collapse = " | "),
    identical(
      unique(codigos_inelegiveis_2026),
      codigos_inelegiveis_esperados
    )
  ),
  registrar_validacao(
    "V18",
    "Nenhuma escola inelegível entra no índice de carga",
    sum(
      cadastro_canonico$elegivel_assessoramento == "NAO" &
        cadastro_canonico$incluir_indice_carga,
      na.rm = TRUE
    ),
    0,
    !any(
      cadastro_canonico$elegivel_assessoramento == "NAO" &
        cadastro_canonico$incluir_indice_carga,
      na.rm = TRUE
    )
  ),
  registrar_validacao(
    "V18A",
    "Índice de carga contém as 53 escolas assessoradas homologadas",
    valor_resumo(
      resumo_2026,
      "escolas_indice_carga"
    ),
    snapshot_homologado$escolas_indice_carga_pos_inicio,
    nrow(resumo_2026) == 1 &&
      resumo_2026$escolas_indice_carga ==
        snapshot_homologado$escolas_indice_carga_pos_inicio
  ),
  registrar_validacao(
    "V19",
    "Linha de base de 2025 possui exposição zero",
    sum(
      exposicoes_linha_base != 0L,
      na.rm = TRUE
    ),
    0,
    length(exposicoes_linha_base) > 0 &&
      !anyNA(exposicoes_linha_base) &&
      all(exposicoes_linha_base == 0L)
  ),
  registrar_validacao(
    "V20",
    "Data institucional da homologação de 2026",
    paste(
      format(
        sort(unique(datas_validacao_2026), na.last = TRUE),
        "%Y-%m-%d"
      ),
      collapse = " | "
    ),
    format(
      snapshot_homologado$data_homologacao_pos_inicio,
      "%Y-%m-%d"
    ),
    length(datas_validacao_2026) > 0 &&
      !anyNA(datas_validacao_2026) &&
      all(
        datas_validacao_2026 ==
          snapshot_homologado$data_homologacao_pos_inicio
      )
  )
)

# -------------------------------------------------------------------
# 10. Diagnósticos, manifestos e bloqueio de erros
# -------------------------------------------------------------------

estrutura_entradas <- imap_dfr(
  bases_brutas,
  ~ inventariar_estrutura(.x, .y)
)

manifesto_entradas <- tibble(
  fonte = names(arquivos_entrada),
  caminho = unname(arquivos_entrada),
  encoding_detectado = unname(encodings_entrada),
  existe = file.exists(unname(arquivos_entrada)),
  tamanho_bytes = map_dbl(
    unname(arquivos_entrada),
    ~ if (file.exists(.x)) file.info(.x)$size else NA_real_
  ),
  modificado_em = map_chr(
    unname(arquivos_entrada),
    ~ if (file.exists(.x)) {
      format(file.info(.x)$mtime, "%Y-%m-%d %H:%M:%S")
    } else {
      NA_character_
    }
  ),
  md5 = map_chr(unname(arquivos_entrada), hash_md5)
)

# A numeracao dos produtos pactuados e preservada. O novo diagnostico
# 04A documenta as duplicidades da dimensao sem deslocar os arquivos
# 12 a 17 que compoem o protocolo de devolucao desta etapa.

diagnosticos <- list(
  "01_manifesto_arquivos_entrada.csv" = manifesto_entradas,
  "02_estrutura_bases_entrada.csv" = estrutura_entradas,
  "04_duplicidades_chaves_manual.csv" = duplicidades_manual,
  "04A_duplicidades_chave_dimensao.csv" = duplicidades_dim,
  "04B_registros_dim_chave_incompleta.csv" =
    registros_dim_chave_incompleta,
  "05_ids_manuais_desconhecidos.csv" = ids_manuais_desconhecidos,
  "05A_ids_painel_desconhecidos.csv" =
    ids_painel_desconhecidos,
  "06_divergencias_identificacao.csv" = divergencias_identificacao,
  "07_conflitos_definicao_periodos.csv" = conflitos_periodos,
  "08_registros_manuais_sem_periodo.csv" =
    registros_manuais_sem_periodo,
  "09_periodos_pos_sem_homologacao.csv" =
    periodos_pos_sem_homologacao,
  "10_inconsistencias_fonte_manual.csv" =
    inconsistencias_manual,
  "11_inconsistencias_cadastro_canonico.csv" =
    inconsistencias_canonico,
  "12_resumo_universos_por_periodo.csv" = resumo_universos,
  "13_conveniadas_2026.csv" = conveniadas_2026,
  "14_validacao_final.csv" = validacao_final
)

iwalk(
  diagnosticos,
  ~ write_csv(
    .x,
    file.path(pasta_execucao, .y),
    na = ""
  )
)

writeLines(
  capture.output(sessionInfo()),
  file.path(pasta_execucao, "15_session_info.txt")
)

erros_finais <- validacao_final |>
  filter(
    criticidade == "ERRO",
    status == "ERRO"
  )

avisos_finais <- validacao_final |>
  filter(status == "AVISO")

if (nrow(erros_finais) > 0) {
  stop(
    "O cadastro de exposição não foi gravado: ",
    nrow(erros_finais),
    " validação(ões) crítica(s) falharam. Consulte ",
    "14_validacao_final.csv em:\n",
    pasta_execucao
  )
}

# -------------------------------------------------------------------
# 11. Histórico e exportação dos produtos canônicos
# -------------------------------------------------------------------

walk(
  arquivos_saida[file.exists(arquivos_saida)],
  function(caminho) {
    file.copy(
      caminho,
      file.path(pasta_historico, basename(caminho)),
      overwrite = FALSE
    )
  }
)

write_csv(
  cadastro_canonico,
  arquivos_saida[["cadastro_csv"]],
  na = ""
)

saveRDS(
  cadastro_canonico,
  arquivos_saida[["cadastro_rds"]]
)

manifesto_produtos <- tibble(
  produto = names(arquivos_saida),
  caminho = unname(arquivos_saida),
  existe = file.exists(unname(arquivos_saida)),
  tamanho_bytes = map_dbl(
    unname(arquivos_saida),
    ~ if (file.exists(.x)) file.info(.x)$size else NA_real_
  ),
  md5 = map_chr(unname(arquivos_saida), hash_md5)
)

write_csv(
  manifesto_produtos,
  file.path(
    pasta_execucao,
    "16_manifesto_produtos_modulo_11C.csv"
  ),
  na = ""
)

# valor_resumo() evita que um filtro vazio devolva character(0) e
# suprima a linha inteira do resumo sem qualquer aviso.

resumo_texto <- c(
  "Módulo 11C — cadastro canônico de exposição ao assessoramento",
  paste0("Execução: ", id_execucao),
  paste0(
    "Escolas na dimensão: ",
    n_distinct(dim_escola$id_escola)
  ),
  paste0("Períodos observados: ", nrow(periodos_observados)),
  paste0("Registros escola × período: ", nrow(cadastro_canonico)),
  paste0(
    "Universo avaliativo 2025: ",
    valor_resumo(resumo_2025, "escolas_universo_avaliativo")
  ),
  paste0(
    "Universo avaliativo 2026: ",
    valor_resumo(resumo_2026, "escolas_universo_avaliativo")
  ),
  paste0(
    "Escolas assessoradas em 2026: ",
    valor_resumo(resumo_2026, "escolas_assessoradas")
  ),
  paste0(
    "Escolas inelegíveis em 2026: ",
    valor_resumo(resumo_2026, "escolas_inelegiveis")
  ),
  paste0(
    "Avisos não bloqueantes: ",
    nrow(avisos_finais),
    if (nrow(avisos_finais) > 0) {
      paste0(
        " (",
        paste(avisos_finais$id_teste, collapse = ", "),
        ")"
      )
    } else {
      ""
    }
  ),
  "Módulo 21: não executado.",
  "Próximo passo: homologar esta execução antes de refatorar os módulos 15 a 21."
)

writeLines(
  resumo_texto,
  file.path(pasta_execucao, "17_resumo_execucao.txt")
)

message(
  "Módulo 11C concluído. Cadastro canônico gravado em:\n",
  arquivos_saida[["cadastro_csv"]],
  "\nAvisos não bloqueantes: ",
  nrow(avisos_finais),
  "\nDiagnósticos em:\n",
  pasta_execucao
)
