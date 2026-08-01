# SRAG_VSR_CLIMA_BRA

Análise da sazonalidade da Síndrome Respiratória Aguda Grave (SRAG) por Vírus Sincicial Respiratório (VSR) e sua relação com variáveis climáticas nas macrorregiões brasileiras (2013–2025).

## Autoria e Desenvolvimento de Pesquisa

Este repositório reúne o código-fonte utilizado nas análises desenvolvidas no âmbito da tese de doutorado de **Felipe Cotrim de Carvalho**.
A pesquisa é desenvolvida no **Pós-Graduação em Biotecnologia em Saúde e Medicina Investigativa (PgBSMI) da Fundação Oswaldo Cruz – Bahia (Fiocruz Bahia)**, sob orientação do **Prof. Dr. Mitermayer G. Reis**.

Este projeto disponibiliza um fluxo analítico reproduzível para a preparação das bases de dados, análise da sazonalidade do VSR, decomposição de séries temporais, correlação com variáveis climáticas, modelagem da defasagem clima-VSR e avaliação das mudanças no padrão epidemiológico antes e após a pandemia de COVID-19.

---

## Objetivos

- Caracterizar a sazonalidade do VSR nas cinco macrorregiões brasileiras;
- Avaliar a associação entre temperatura, precipitação e umidade relativa do ar com a circulação do VSR;
- Modelar a estrutura de defasagem (lag) entre temperatura e VSR por região, com pooling nacional (DLNM + meta-análise multivariada);
- Investigar possíveis alterações na sazonalidade após a pandemia de COVID-19;
- Disponibilizar um pipeline totalmente reproduzível para pesquisadores e gestores em saúde pública.

---

## Pipeline: dois scripts, dois estágios

As análises são organizadas em **dois scripts numerados, executados em sequência**.

- O primeiro é engenharia de dados + estatística descritiva/clássica (harmonização de duas bases de vigilância distintas, decomposição de série temporal, correlação); 
- O segundo é modelagem avançada (DLNM + meta-análise), que depende de pacotes próprios (`dlnm`, `mixmeta`) desnecessários para quem só quer a parte descritiva. 

OBS: Cada script consome o que o anterior produziu, o segundo não refaz nenhuma etapa de carregamento ou limpeza de dados.

### `01_VSR_SAZONALIDADE_CLIMA.R` — harmonização e sazonalidade

**Entrada:** bases brutas de vigilância (OPENDATASUS) (`BD/base_VSR_2013_2018.RData`, SINAN; `BD/base_DEF_VSR_2019_2025.RData`, SIVEP-Gripe) + dados climáticos da NASA POWER (baixados ou cacheados em `BD/`).

**O que faz:**
- harmoniza as duas bases de vigilância (incluindo a conversão de código de UF numérico → sigla, necessária só no período 2013–2018) em uma série única por macrorregião;
- constrói séries semanais e mensais de casos e clima;
- decomposição STL, autocorrelação (ACF/PACF);
- correlação de Spearman (contemporânea, mensal) e correlação cruzada (CCF, semanal) entre clima e casos;
- classificação de estações climáticas regionais (quente/frio × seco/úmido) e perfis sazonais;
- comparação de indicadores epidêmicos pré vs. pós-pandemia (Wilcoxon).

**Saída:** todas as tabelas e gráficos numerados em `Resultados/`, incluindo as bases processadas em `Resultados/Bases_processadas/` (em especial `base_vsr_clima_semanal.rds`, que é a entrada do script seguinte).

### `02_DLNM_MIXMETA_VSR_CLIMA.R` — modelo de defasagem (DLNM + mixmeta)

**Pré-requisito:** rodar o `01` antes (lê `Resultados/Bases_processadas/base_vsr_clima_semanal.rds`).

**O que faz:**
- ajusta um modelo de defasagem distribuída não-linear (DLNM, Gasparrini) de temperatura por macrorregião, em GLM quasi-Poisson com spline de tendência de longo prazo;
- escolhe os graus de liberdade da spline exposição-resposta por QAIC (comum entre regiões, requisito para o pooling);
- agrupa os coeficientes reduzidos das 5 regiões em uma meta-análise multivariada de efeitos aleatórios (`mixmeta`), produzindo uma curva pooled nacional e curvas regionais BLUP, com teste formal de heterogeneidade (Q, I²);
- ajusta um modelo bivariado adicional (temperatura + umidade) para o Nordeste.

**Saída:** `Resultados/Tabelas/22_...` a `25_...` e `Resultados/Graficos/04_DLNM_Mixmeta/` — usados como Figura 2, Tabela S3 e Figura S3 do manuscrito.

Essa divisão espelha a própria estrutura de Métodos do manuscrito: uma seção para sazonalidade/correlação clássica, outra para o modelo de defasagem DLNM-mixmeta.

---

## Estrutura do projeto

```
SRAG_VSR_CLIMA_BRA
│
├── BD/
│   ├── base_VSR_2013_2018.RData              # SINAN, 2013–2018
│   ├── base_DEF_VSR_2019_2025.RData          # SIVEP-Gripe, 2019–2025
│   ├── clima_nasa_power_*.rds                # cache climático (gerado, gitignored)
│   └── projecoes_2024_tab1_idade_simples.xlsx
│
├── Resultados/                                # gerado pelos scripts, gitignored
│   ├── Bases_processadas/
│   ├── Tabelas/
│   ├── Graficos/
│   │   ├── 01_Sazonalidade/
│   │   ├── 02_Clima/
│   │   ├── 03_Anomalias/
│   │   └── 04_DLNM_Mixmeta/
│   └── Logs/
│
├── 01_VSR_SAZONALIDADE_CLIMA.R
├── 02_DLNM_MIXMETA_VSR_CLIMA.R
│
├── SRAG_VSR_CLIMA_BRA.Rproj
├── .gitignore
└── README.md
```

---

## Bases de dados

As análises utilizam dados públicos provenientes de:

- OpenDataSUS – SRAG;
- NASA POWER Project (variáveis meteorológicas);
- Instituto Brasileiro de Geografia e Estatística (IBGE).

A base consolidada de toda a série histórica da SRAG e o cache climático da NASA POWER não acompanham o repositório devido ao tamanho, mas são reconstruídos automaticamente na primeira execução a partir dos dados públicos.

---

## Como executar

Clone o repositório:

```bash
git clone https://github.com/felipecotrim/SRAG_VSR_CLIMA_BRA.git
```

Abra o projeto no RStudio (`SRAG_VSR_CLIMA_BRA.Rproj`) e execute os dois scripts **nesta ordem**:

```r
source("01_VSR_SAZONALIDADE_CLIMA.R")
source("02_DLNM_MIXMETA_VSR_CLIMA.R")
```

O `02` falha imediatamente com uma mensagem clara se o `01` ainda não tiver sido executado (a base intermediária não existe).

Pacotes exigidos apenas pelo `02`: `dlnm`, `mixmeta`, `MASS` (instalados automaticamente se ausentes).

---

## Reprodutibilidade

O fluxo analítico completo é executado a partir de dois scripts, cada um correspondendo a um estágio distinto da análise (descrito acima). Figuras, tabelas e bases intermediárias são geradas automaticamente durante a execução e, por esse motivo, não fazem parte do repositório.

---

## Autor

**Felipe Cotrim**

Biomédico • Epidemiologista • Cientista de Dados
Doutorando em Epidemiologia – Fiocruz Bahia

---

## Licença

Este projeto está disponível sob a licença MIT.
