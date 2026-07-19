Sim, é seguro e provavelmente conveniente iniciar uma nova conversa. Esta já acumulou muitas correções intermediárias; em uma nova conversa, reduzimos o risco de reutilizar versões superadas dos scripts.

O essencial é que o novo prompt registre:

* objetivo e limites metodológicos;
* estrutura atual do projeto;
* módulos já concluídos;
* correções importantes já incorporadas;
* arquivos que devem ser considerados válidos;
* ponto exato de continuidade.

Sugiro anexar à nova conversa, no mínimo:

* `revisao_manual_codigo_inep.csv`;
* `dim_escola.csv`;
* `mapa_nomes_escolas.csv`;
* `variacao_escola_serie_com_qualidade.csv`;
* `resultado_geral_carteiras.csv`;
* `fato_contexto_caed_5ano_2026.csv`, caso já tenha sido criado;
* eventualmente os scripts `03` a `11A`, sobretudo as versões finais.

Use este prompt:

---

## Prompt para prosseguimento

Estou desenvolvendo no RStudio um estudo observacional de assessoramento pedagógico da SMED de Porto Alegre, com dados da 1ª avaliação formativa CAEd de Língua Portuguesa dos anos iniciais do Ensino Fundamental.

### 1. Objetivo do estudo

O objetivo é construir uma plataforma modular e reprodutível de monitoramento da evolução das escolas municipais entre 2025 e 2026, com foco em:

* participação nas avaliações;
* proficiência média;
* percentuais de defasagem, aprendizagem intermediária e aprendizagem adequada;
* resultados por escola, ano escolar e carteira de assessora;
* habilidades avaliadas;
* características contextuais das escolas.

Não há pretensão de identificação causal.

A 1ª avaliação formativa de 2025 ocorreu antes do início da equipe de assessoramento e da consultoria CAEd. Portanto:

* 2025 é a linha de base pré-programa;
* 2026 é o período posterior ao início do programa;
* a exposição em 2025 é zero;
* toda a rede foi abrangida, sem grupo de controle;
* ainda não temos acesso adequado à planilha de visitas;
* não há medida confiável de dose de assessoramento;
* a vinculação escola–assessora representa apenas uma carteira administrativa;
* não devem ser atribuídos efeitos ou resultados individuais às assessoras.

Uma hipótese importante é que a pressão institucional do programa tenha elevado muito a participação em 2026. Assim, a eventual queda de proficiência pode refletir parcialmente a entrada de estudantes de menor desempenho que antes não participavam.

### 2. Acervo

Há dez CSVs CAEd:

* 2025 e 2026;
* 1º ao 5º ano;
* Língua Portuguesa;
* 1ª avaliação formativa;
* unidade original no nível da turma.

As quantidades de habilidades variam entre arquivos. Por isso, foram criadas duas tabelas:

* resultados gerais por turma;
* habilidades em formato longo.

Há também:

* planilha de vinculação escola–assessora;
* planilha contextual CAEd do 5º ano de 2026, com NSE, raça/cor e sexo;
* referência contextual agregada do município;
* arquivos de conciliação de nomes;
* revisão manual dos códigos INEP.

### 3. Estrutura do projeto

```text
estudo_descritivo/
├── dados_brutos/
├── dados_intermediarios/
├── dados_processados/
├── documentacao/
├── resultados/
├── R/
├── config_bigquery.R
└── estudo_descritivo.Rproj
```

### 4. Módulos já executados

Foram executados e corrigidos:

```text
R/01_inventario.R
R/02_dim_escola.R
R/03_consolidar_dim_escola.R
R/04_importar_caed.R
R/05_construir_painel.R
R/06_qualidade_composicao.R
R/07_resultados_descritivos.R
R/08_sensibilidade_participacao.R
R/08A_diagnosticar_painel.R
R/09_analise_assessoras.R
R/10_contexto_caed.R
R/11A_recuperar_codigo_inep.R
```

### 5. Correções estruturais importantes

Os CSVs CAEd foram inicialmente lidos inteiramente como texto para evitar conflitos de tipo.

A chave longitudinal correta é:

```text
id_escola × ano_escolar × componente
```

A agregação de turmas para escola–série–ano precisou ser corrigida. Não se deve criar `avaliados = sum(avaliados)` e depois usar `avaliados` como peso dentro do mesmo `summarise()`, pois o nome é sobrescrito.

A versão válida calcula previamente numeradores e pesos, por exemplo:

```r
num_proficiencia = proficiencia_media * avaliados
peso_proficiencia = avaliados
```

e somente depois agrega:

```r
proficiencia_media =
  sum(num_proficiencia) / sum(peso_proficiencia)
```

O mesmo cuidado foi aplicado à agregação da rede e aos dados contextuais.

Após a correção, o diagnóstico longitudinal ficou consistente:

| Ano escolar | Presentes nos dois anos | Resultado nos dois anos |
| ----------- | ----------------------: | ----------------------: |
| 1º          |                      54 |                      54 |
| 2º          |                      53 |                      53 |
| 3º          |                      53 |                      53 |
| 4º          |                      53 |                      53 |
| 5º          |                      52 |                      52 |

Portanto, há aproximadamente 265 observações escola–série comparáveis, e não apenas 19 como aparecia antes da correção.

### 6. Bases processadas principais

Foram produzidos, entre outros:

```text
dados_intermediarios/dim_escola.csv
dados_intermediarios/mapa_nomes_escolas.csv
dados_processados/fato_turma_avaliacao.csv
dados_processados/fato_habilidade_turma.csv
dados_processados/painel_escola_serie_ano.csv
dados_processados/variacao_escola_serie_2025_2026.csv
dados_processados/variacao_escola_serie_com_qualidade.csv
dados_processados/classificacao_diagnostica_escolas.csv
dados_processados/fato_contexto_caed_5ano_2026.csv
resultados/assessoramento/resultado_geral_carteiras.csv
resultados/assessoramento/resultado_assessora_por_serie.csv
resultados/assessoramento/sensibilidade_resultados_carteiras.csv
```

### 7. Situação da vinculação aos códigos INEP

Foi executado o módulo `11A_recuperar_codigo_inep.R`, que:

* consultou o diretório de escolas de Porto Alegre;
* fez correspondências exatas normalizadas;
* gerou candidatos aproximados;
* produziu o arquivo:

```text
documentacao/codigo_inep/revisao_manual_codigo_inep.csv
```

Eu revisei esse arquivo.

Para escolas sem candidato correto no diretório consultado, localizei manualmente o código em uma base INEP atualizada e registrei a informação no arquivo anexado.

É preciso agora:

1. inspecionar cuidadosamente o arquivo `revisao_manual_codigo_inep.csv`;
2. identificar exatamente quais colunas e convenções foram usadas nas decisões manuais;
3. criar um módulo robusto para consolidar:

   * correspondências exatas;
   * candidatos aceitos;
   * códigos INEP informados manualmente;
   * casos não encontrados;
4. impedir múltiplos códigos aceitos para a mesma escola;
5. atualizar `dim_escola.csv` sem perder:

   * o identificador interno;
   * os nomes originais;
   * o nome canônico;
   * a assessora;
   * observações sobre municipalização ou mudança administrativa;
6. produzir diagnósticos de pendências e duplicidades.

### 8. Próxima etapa substantiva

Depois da consolidação dos códigos INEP, quero recuperar características estruturais pré-programa, preferencialmente de 2024, para caracterizar escolas e carteiras.

Possíveis variáveis:

* dependência administrativa;
* situação de funcionamento;
* localização;
* matrículas;
* número de turmas;
* porte;
* etapas ofertadas;
* tempo integral;
* infraestrutura;
* educação especial;
* número e características dos docentes;
* complexidade da gestão;
* regularidade docente;
* adequação da formação docente;
* distorção idade–série e outros indicadores disponíveis.

Essas variáveis devem ser tratadas como contexto pré-programa, não como resultados do assessoramento.

### 9. Orientação para o prosseguimento

Antes de escrever o próximo script:

* analise o arquivo anexado;
* informe o que encontrou nas decisões manuais;
* identifique possíveis inconsistências;
* proponha a estrutura da dimensão final;
* depois apresente apenas o próximo módulo executável em R.

Não reutilize versões antigas dos scripts caso entrem em conflito com as correções descritas acima.

---

Na nova conversa, o ponto exato de retomada será: **consolidação definitiva dos códigos INEP a partir do arquivo revisado e preparação da integração com o contexto escolar de 2024**.
