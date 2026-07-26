# SRAG_VSR_CLIMA_BRA

Análise da sazonalidade da Síndrome Respiratória Aguda Grave (SRAG) associada ao Vírus Sincicial Respiratório (VSR) e sua relação com variáveis climáticas nas macrorregiões brasileiras (2013–2025).

# Autoria e Desenvolvimento de Pesquisa

Este repositório reúne o código-fonte utilizado nas análises desenvolvidas no âmbito da tese de doutorado de **Felipe Cotrim de Carvalho**.
A pesquisa é desenvolvida no **Pós-Graduação em Biotecnologia em Saúde e Medicina Investigativa (PgBSMI) da Fundação Oswaldo Cruz – Bahia (Fiocruz Bahia)**, sob orientação do **Prof. Dr. Mitermayer G. Reis**.

Este projeto tem como objetivo disponibilizar um fluxo analítico totalmente reproduzível para a preparação das bases de dados, análise da sazonalidade do VSR, decomposição de séries temporais, autocorrelação, correlação cruzada com variáveis climáticas e avaliação das mudanças no padrão epidemiológico antes e após a pandemia de COVID-19.

---

## Objetivos

- Caracterizar a sazonalidade do VSR nas cinco macrorregiões brasileiras;
- Avaliar a associação entre temperatura, precipitação e umidade relativa do ar com a circulação do VSR;
- Investigar possíveis alterações na sazonalidade após a pandemia de COVID-19;
- Disponibilizar um pipeline totalmente reproduzível para pesquisadores e gestores em saúde pública.

---

## Estrutura do projeto

```
SRAG_VSR_CLIMA_BRA
│
├── BD/
│   ├── base_VSR_2013_2018.RData
│   ├── base_DEF_VSR_2019_2025.RData
│   └── projecoes_2024_tab1_idade_simples.xlsx
│
├── Resultados/
│
├── 01_VSR_SAZONALIDADE_CLIMA.R
│
├── SRAG_VSR_CLIMA_BRA.Rproj
│
├── .gitignore
└── README.md
```

---

## Bases de dados

As análises utilizam dados públicos provenientes de:

- OpenDataSUS – SRAG;
- NASA POWER Project (variáveis meteorológicas);
- Instituto Brasileiro de Geografia e Estatística (IBGE).

A base consolidada de toda a série histórica da SRAG não acompanha o repositório devido ao seu tamanho, mas pode ser reconstruída a partir dos dados públicos disponibilizados pelo OpenDataSUS.

---

## Principais análises

O script executa:

- preparação e harmonização das bases;
- séries temporais semanais e mensais;
- decomposição STL;
- autocorrelação (ACF);
- autocorrelação parcial (PACF);
- correlação de Spearman;
- correlação cruzada (CCF);
- perfis sazonais;
- heatmaps;
- comparação entre os períodos pré e pós-pandemia;
- análise de anomalias climáticas.

Todos os resultados são exportados automaticamente para a pasta **Resultados/**.

---

## Como executar

Clone o repositório

```bash
git clone https://github.com/felipecotrim/SRAG_VSR_CLIMA_BRA.git
```

Abra o projeto no RStudio

```
SRAG_VSR_CLIMA_BRA.Rproj
```

Execute

```r
source("01_VSR_SAZONALIDADE_CLIMA.R")
```

---

## Reprodutibilidade

Todo o fluxo analítico é executado a partir de um único script.

As figuras, tabelas e bases intermediárias são geradas automaticamente durante a execução e, por esse motivo, não fazem parte do repositório.

---

## Autor

**Felipe Cotrim**

Biomédico • Epidemiologista • Cientista de Dados
Doutorando em Epidemiologia – Fiocruz Bahia

---

## Licença

Este projeto está disponível sob a licença MIT.
