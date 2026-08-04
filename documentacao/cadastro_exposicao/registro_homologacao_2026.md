# Registro de homologação do cadastro de exposição — 2026

## Finalidade

Separar, por escola e período avaliativo, o universo observado nas avaliações do universo efetivamente exposto ao assessoramento da UEF-SMED-PMPA.

## Fonte institucional

- Instrumento recebido: `homologacao_cadastro_exposicao_assessoramento_preenchido.xlsx`.
- SHA-256 do instrumento recebido: `50fea7cc8c6fb462045d7432eba441c90aa12d3f2e0b04e11db92231fe4405c2`.
- Aba utilizada: `Homologacao_2026`.
- Responsável informado: Marian Dante — assessora UEF/SMED.
- Fonte informada: declaração formal.
- Data institucional correta: 2026-07-24.

O arquivo preenchido continha 56 linhas marcadas como `HOMOLOGADO`, 53 escolas com assessoramento ativo e três escolas sem assessoramento e com carga zero ou tendente a zero.

## Correções confirmadas após o preenchimento

Em confirmação posterior, o responsável pelo projeto determinou:

1. `EEF PEQUENA CASA DA CRIANCA` (`43105416`), `ESCOLA DE ENSINO FUNDAMENTAL MADRE RAFFO` (`43105300`) e `EEF ALDEIA LUMIAR` (`43189768`) são formalmente inelegíveis ao assessoramento. Para elas, `elegivel_assessoramento = NAO`, `recebe_assessoramento = NAO` e `status_carga_operacional = ZERO_OU_TENDENTE_A_ZERO`.
2. A data `2024-07-24` armazenada no instrumento preenchido era erro material. A data correta é `2026-07-24`.

Essas correções foram aplicadas na fonte manual versionada `dados_manuais/cadastro_exposicao_assessoramento.csv`. O arquivo preenchido original foi preservado fora do pipeline analítico como evidência de entrada; nenhuma informação institucional não confirmada foi inferida.

## Regras metodológicas

- Presença no CAEd não comprova exposição ao programa.
- O rótulo de rede ou dependência administrativa não determina elegibilidade.
- A primeira avaliação formativa de 2025 é linha de base pré-programa; exposição nessa onda é zero.
- A partir de 2026, toda onda pós-início exige registro homologado escola × período.
- Escolas inelegíveis ou não assessoradas não entram no índice de carga nem nas sínteses de carteiras.
- Escolas avaliadas e não assessoradas podem compor análise descritiva separada, sem serem tratadas automaticamente como grupo de controle causal.
