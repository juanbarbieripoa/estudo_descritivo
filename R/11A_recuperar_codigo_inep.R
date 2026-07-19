library(here)
library(tidyverse)
library(basedosdados)

# -------------------------------------------------------------------
# 1. Configuração
# -------------------------------------------------------------------

arquivo_config <- here("config_bigquery.R")

if (!file.exists(arquivo_config)) {
  stop(
    "Arquivo config_bigquery.R não encontrado na raiz do projeto."
  )
}

source(arquivo_config)

if (
  !exists("billing_project_id") ||
  is.na(billing_project_id) ||
  billing_project_id == "" ||
  str_detect(
    billing_project_id,
    "SUBSTITUA"
  )
) {
  stop(
    "Informe o ID do projeto Google Cloud em config_bigquery.R."
  )
}

basedosdados::set_billing_id(
  billing_project_id
)

dir.create(
  here("dados_intermediarios"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("documentacao", "codigo_inep"),
  recursive = TRUE,
  showWarnings = FALSE
)

# -------------------------------------------------------------------
# 2. Funções auxiliares
# -------------------------------------------------------------------

normalizar_nome <- function(x) {
  
  x |>
    as.character() |>
    iconv(
      from = "",
      to = "ASCII//TRANSLIT"
    ) |>
    str_to_upper() |>
    str_replace_all(
      "[^A-Z0-9 ]",
      " "
    ) |>
    str_replace_all(
      "\\bEMEF\\b",
      ""
    ) |>
    str_replace_all(
      "\\bEEF\\b",
      ""
    ) |>
    str_replace_all(
      "\\bESCOLA MUNICIPAL DE ENSINO FUNDAMENTAL\\b",
      ""
    ) |>
    str_replace_all(
      "\\bESCOLA ESTADUAL DE ENSINO FUNDAMENTAL\\b",
      ""
    ) |>
    str_replace_all(
      "\\bESCOLA\\b",
      ""
    ) |>
    str_replace_all(
      "\\bMUNICIPAL\\b",
      ""
    ) |>
    str_replace_all(
      "\\bESTADUAL\\b",
      ""
    ) |>
    str_replace_all(
      "\\bENSINO FUNDAMENTAL\\b",
      ""
    ) |>
    str_squish()
}

primeiro_nao_vazio <- function(x) {
  
  x <- as.character(x)
  
  x <- x[
    !is.na(x) &
      str_squish(x) != ""
  ]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  x[[1]]
}

# -------------------------------------------------------------------
# 3. Dimensão local
# -------------------------------------------------------------------

arquivo_dim <- here(
  "dados_intermediarios",
  "dim_escola.csv"
)

if (!file.exists(arquivo_dim)) {
  stop(
    "Arquivo dim_escola.csv não encontrado."
  )
}

dim_local <- read_csv(
  arquivo_dim,
  show_col_types = FALSE,
  col_types = cols(
    .default = col_character()
  )
) |>
  mutate(
    chave_nome_local = normalizar_nome(
      nome_canonico
    )
  )

# -------------------------------------------------------------------
# 4. Consulta ao diretório nacional de escolas
# -------------------------------------------------------------------
#
# Código IBGE de Porto Alegre: 4314902.
# Mantemos todas as dependências administrativas nesta etapa,
# pois pode haver escola recentemente municipalizada.
# -------------------------------------------------------------------

query_diretorio <- "
SELECT
  id_escola,
  nome,
  id_municipio
FROM
  `basedosdados.br_bd_diretorios_brasil.escola`
WHERE
  id_municipio = '4314902'
"

diretorio_poa <- basedosdados::read_sql(
  query_diretorio
) |>
  as_tibble() |>
  mutate(
    id_escola = as.character(id_escola),
    nome = as.character(nome),
    chave_nome_inep = normalizar_nome(nome)
  ) |>
  distinct(
    id_escola,
    nome,
    id_municipio,
    chave_nome_inep
  )

write_csv(
  diretorio_poa,
  here(
    "dados_intermediarios",
    "diretorio_escolas_poa_inep.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 5. Correspondência exata normalizada
# -------------------------------------------------------------------

correspondencia_exata <- dim_local |>
  left_join(
    diretorio_poa,
    by = c(
      "chave_nome_local" =
        "chave_nome_inep"
    )
  ) |>
  mutate(
    status_codigo_inep = case_when(
      !is.na(id_escola.y) ~
        "correspondencia_exata",
      
      TRUE ~
        "sem_correspondencia_exata"
    )
  ) |>
  rename(
    id_escola_local = id_escola.x,
    codigo_inep_candidato = id_escola.y,
    nome_inep_candidato = nome
  )

# -------------------------------------------------------------------
# 6. Diagnóstico de múltiplas correspondências
# -------------------------------------------------------------------

multiplas_correspondencias <- correspondencia_exata |>
  filter(
    status_codigo_inep ==
      "correspondencia_exata"
  ) |>
  count(
    id_escola_local,
    nome_canonico,
    name = "numero_correspondencias"
  ) |>
  filter(
    numero_correspondencias > 1
  )

write_csv(
  multiplas_correspondencias,
  here(
    "documentacao",
    "codigo_inep",
    "multiplas_correspondencias_exatas.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 7. Candidatos aproximados
# -------------------------------------------------------------------

sem_correspondencia <- correspondencia_exata |>
  filter(
    status_codigo_inep ==
      "sem_correspondencia_exata"
  ) |>
  distinct(
    id_escola_local,
    nome_canonico,
    chave_nome_local,
    assessora,
    observacao_historico
  )

gerar_candidatos <- function(
    chave_origem,
    nomes_referencia,
    numero_candidatos = 5
) {
  
  distancias <- adist(
    chave_origem,
    nomes_referencia
  )
  
  ordem <- order(
    distancias
  )
  
  ordem <- ordem[
    seq_len(
      min(
        numero_candidatos,
        length(ordem)
      )
    )
  ]
  
  tibble(
    indice_candidato = ordem,
    distancia_edicao =
      as.numeric(
        distancias[ordem]
      )
  )
}

candidatos_aproximados <- sem_correspondencia |>
  mutate(
    candidatos = map(
      chave_nome_local,
      gerar_candidatos,
      nomes_referencia =
        diretorio_poa$chave_nome_inep
    )
  ) |>
  unnest(candidatos) |>
  mutate(
    codigo_inep_candidato =
      diretorio_poa$id_escola[
        indice_candidato
      ],
    
    nome_inep_candidato =
      diretorio_poa$nome[
        indice_candidato
      ],
    
    chave_nome_inep =
      diretorio_poa$chave_nome_inep[
        indice_candidato
      ],
    
    comprimento_maximo = pmax(
      nchar(chave_nome_local),
      nchar(chave_nome_inep)
    ),
    
    distancia_relativa = if_else(
      comprimento_maximo > 0,
      distancia_edicao /
        comprimento_maximo,
      NA_real_
    ),
    
    decisao_manual = "",
    observacao_validacao = ""
  ) |>
  select(
    id_escola_local,
    nome_canonico,
    assessora,
    chave_nome_local,
    codigo_inep_candidato,
    nome_inep_candidato,
    distancia_edicao,
    distancia_relativa,
    decisao_manual,
    observacao_validacao,
    observacao_historico
  ) |>
  arrange(
    nome_canonico,
    distancia_relativa
  )

# -------------------------------------------------------------------
# 8. Mapa de correspondências exatas
# -------------------------------------------------------------------

mapa_exato <- correspondencia_exata |>
  filter(
    status_codigo_inep ==
      "correspondencia_exata"
  ) |>
  group_by(
    id_escola_local,
    nome_canonico
  ) |>
  summarise(
    codigo_inep = primeiro_nao_vazio(
      codigo_inep_candidato
    ),
    
    nome_inep = primeiro_nao_vazio(
      nome_inep_candidato
    ),
    
    metodo_correspondencia_inep =
      "exata_normalizada",
    
    .groups = "drop"
  )

# -------------------------------------------------------------------
# 9. Arquivo para revisão
# -------------------------------------------------------------------

write_csv(
  candidatos_aproximados,
  here(
    "documentacao",
    "codigo_inep",
    "revisao_manual_codigo_inep.csv"
  ),
  na = ""
)

write_csv(
  mapa_exato,
  here(
    "dados_intermediarios",
    "mapa_codigo_inep_exato.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------
# 10. Resumo
# -------------------------------------------------------------------

resumo <- tibble(
  escolas_dimensao_local =
    n_distinct(
      dim_local$id_escola
    ),
  
  escolas_diretorio_poa =
    n_distinct(
      diretorio_poa$id_escola
    ),
  
  correspondencias_exatas =
    n_distinct(
      mapa_exato$id_escola_local
    ),
  
  escolas_sem_correspondencia =
    n_distinct(
      sem_correspondencia$id_escola_local
    ),
  
  casos_com_correspondencia_multipla =
    nrow(
      multiplas_correspondencias
    )
)

cat(
  "\nRecuperação inicial dos códigos INEP concluída.\n"
)

print(resumo)

cat(
  "\nArquivos gerados:\n",
  "- dados_intermediarios/diretorio_escolas_poa_inep.csv\n",
  "- dados_intermediarios/mapa_codigo_inep_exato.csv\n",
  "- documentacao/codigo_inep/revisao_manual_codigo_inep.csv\n",
  "- documentacao/codigo_inep/multiplas_correspondencias_exatas.csv\n"
)