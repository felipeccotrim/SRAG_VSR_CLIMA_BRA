#### PROJETO SRAG_VSR_CLIMA_BRA ####
#### ANÁLISES DE SAZONALIDADE DO VSR E ASSOCIAÇÃO COM O CLIMA ####

# Autor: Felipe Cotrim
# Objetivo: executar as análises de sazonalidade da SRAG por VSR e sua associação com temperatura, umidade relativa e precipitação.


##Estrutura esperada do projeto:
  # SRAG_VSR_CLIMA_BRA/
  # ├── SRAG_VSR_CLIMA_BRA.Rproj
  # ├── 01_VSR_SAZONALIDADE_CLIMA.R
  # ├── BD/
  # │   ├── base_VSR_2013_2018.RData
  # │   └── base_DEF_VSR_2019_2025.RData
  # └── Resultados/
  #
  # Execução:
  # source(here::here("01_VSR_SAZONALIDADE_CLIMA.R"))
  
  #### 0. CONFIGURAÇÃO INICIAL ####

rm(list = ls())
gc()
options(stringsAsFactors = FALSE, scipen = 999)
set.seed(1234)

pacotes <- c(
  "here", "dplyr", "tidyr", "purrr", "stringr", "lubridate",
  "ggplot2", "forecast", "slider", "nasapower", "janitor",
  "scales", "writexl", "readr", "tibble", "rlang"
)

pacotes_ausentes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]

if (length(pacotes_ausentes) > 0) {
  stop(
    paste0(
      "Instale os seguintes pacotes antes de executar o script:\n",
      paste0("install.packages(c(", paste(sprintf('"%s"', pacotes_ausentes), collapse = ", "), "))")
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(forecast)
  library(slider)
  library(nasapower)
  library(janitor)
  library(scales)
  library(writexl)
  library(readr)
  library(tibble)
  library(rlang)
})

#### 0.1. PARÂMETROS GERAIS ####

DATA_INICIO <- as.Date("2013-01-01")
DATA_FIM <- as.Date("2025-12-31")
ANOS_PANDEMIA <- c(2020L, 2021L)
ANOS_PRE <- 2013:2019
ANOS_POS <- 2022:2025
FREQUENCIA_SEMANAL <- 52
MAX_LAG_MENSAL <- 6
MAX_LAG_SEMANAL <- 8
PERCENTIL_TEMPORADA <- 0.75
ATUALIZAR_CLIMA <- FALSE
VERSAO_SCRIPT <- "2.4.0"
message("Executando 01_VSR_SAZONALIDADE_CLIMA.R — versão ", VERSAO_SCRIPT)

ORDEM_REGIOES <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")
MESES_ABREV_PT <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

MAP_UF_REGIAO <- c(
  AC = "Norte", AL = "Nordeste", AM = "Norte", AP = "Norte",
  BA = "Nordeste", CE = "Nordeste", DF = "Centro-Oeste",
  ES = "Sudeste", GO = "Centro-Oeste", MA = "Nordeste",
  MG = "Sudeste", MS = "Centro-Oeste", MT = "Centro-Oeste",
  PA = "Norte", PB = "Nordeste", PE = "Nordeste", PI = "Nordeste",
  PR = "Sul", RJ = "Sudeste", RN = "Nordeste", RO = "Norte",
  RR = "Norte", RS = "Sul", SC = "Sul", SE = "Nordeste",
  SP = "Sudeste", TO = "Norte"
)

COD_UF_SIGLA <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA",
  "16" = "AP", "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE",
  "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE",
  "29" = "BA", "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS", "51" = "MT",
  "52" = "GO", "53" = "DF"
)

COORD_REGIOES <- tibble::tribble(
  ~REGIAO,         ~lon,  ~lat,
  "Norte",        -60.0,  -3.5,
  "Nordeste",     -40.0,  -9.0,
  "Centro-Oeste", -55.0, -15.0,
  "Sudeste",      -45.0, -22.0,
  "Sul",          -51.0, -27.0
)

#### 0.2. CAMINHOS ####

DIR_BD <- here::here("BD")
DIR_RESULTADOS <- here::here("Resultados")
DIR_BASES <- file.path(DIR_RESULTADOS, "Bases_processadas")
DIR_TABELAS <- file.path(DIR_RESULTADOS, "Tabelas")
DIR_GRAFICOS <- file.path(DIR_RESULTADOS, "Graficos")
DIR_GRAFICOS_SAZ <- file.path(DIR_GRAFICOS, "01_Sazonalidade")
DIR_GRAFICOS_CLIMA <- file.path(DIR_GRAFICOS, "02_Clima")
DIR_GRAFICOS_ANOM <- file.path(DIR_GRAFICOS, "03_Anomalias")
DIR_LOGS <- file.path(DIR_RESULTADOS, "Logs")

pastas <- c(
  DIR_BD, DIR_RESULTADOS, DIR_BASES, DIR_TABELAS, DIR_GRAFICOS,
  DIR_GRAFICOS_SAZ, DIR_GRAFICOS_CLIMA, DIR_GRAFICOS_ANOM, DIR_LOGS
)

invisible(lapply(pastas, dir.create, recursive = TRUE, showWarnings = FALSE))

ARQ_BASE_1318 <- file.path(DIR_BD, "base_VSR_2013_2018.RData")
ARQ_BASE_1925 <- file.path(DIR_BD, "base_DEF_VSR_2019_2025.RData")
ARQ_CLIMA_MENSAL <- file.path(DIR_BD, "clima_nasa_power_mensal_regioes_2013_2025.rds")
ARQ_CLIMA_DIARIO <- file.path(DIR_BD, "clima_nasa_power_diario_regioes_2013_2025.rds")

#### 0.3. FUNÇÕES GERAIS ####

salvar_grafico <- function(grafico, nome, pasta, largura = 12, altura = 8, dpi = 300) {
  caminho <- file.path(pasta, paste0(nome, ".png"))
  ggplot2::ggsave(
    filename = caminho,
    plot = grafico,
    width = largura,
    height = altura,
    dpi = dpi,
    units = "in",
    bg = "white"
  )
  invisible(caminho)
}

salvar_csv <- function(objeto, nome) {
  readr::write_csv(objeto, file.path(DIR_TABELAS, paste0(nome, ".csv")), na = "")
}

normalizar_uf <- function(x) {
  x <- stringr::str_trim(toupper(as.character(x)))
  x <- ifelse(x %in% names(COD_UF_SIGLA), unname(COD_UF_SIGLA[x]), x)
  x[!x %in% names(MAP_UF_REGIAO)] <- NA_character_
  x
}

converter_data_segura <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
  
  x_chr <- stringr::str_trim(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  
  suppressWarnings(
    dplyr::coalesce(
      as.Date(x_chr, format = "%Y-%m-%d"),
      as.Date(x_chr, format = "%d/%m/%Y"),
      as.Date(x_chr, format = "%Y/%m/%d"),
      as.Date(x_chr, format = "%d-%m-%Y")
    )
  )
}

carregar_rdata_objeto <- function(caminho, nomes_preferidos = character()) {
  if (!file.exists(caminho)) {
    stop("Arquivo não encontrado: ", caminho, call. = FALSE)
  }
  
  ambiente_temp <- new.env(parent = emptyenv())
  objetos <- load(caminho, envir = ambiente_temp)
  
  preferido <- intersect(nomes_preferidos, objetos)
  nome_escolhido <- if (length(preferido) > 0) preferido[1] else objetos[1]
  objeto <- get(nome_escolhido, envir = ambiente_temp)
  
  if (!inherits(objeto, c("data.frame", "tbl_df", "data.table"))) {
    stop("O objeto carregado de ", basename(caminho), " não é uma tabela.", call. = FALSE)
  }
  
  message("Carregado: ", basename(caminho), " | objeto: ", nome_escolhido,
          " | linhas: ", format(nrow(objeto), big.mark = "."))
  tibble::as_tibble(objeto)
}

harmonizar_base_vsr <- function(df, origem) {
  nomes <- names(df)
  
  coluna_uf <- intersect(c("SG_UF", "SG_UF_NOT", "SG_UF_INTE"), nomes)
  if (length(coluna_uf) == 0) {
    stop("Nenhuma coluna de UF foi encontrada na base ", origem, ".", call. = FALSE)
  }
  
  if (!"DT_SIN_PRI" %in% nomes) {
    stop("A coluna DT_SIN_PRI não foi encontrada na base ", origem, ".", call. = FALSE)
  }
  
  df %>%
    transmute(
      DT_SIN_PRI = converter_data_segura(.data$DT_SIN_PRI),
      SG_UF = normalizar_uf(.data[[coluna_uf[1]]]),
      ORIGEM_BASE = origem
    ) %>%
    filter(
      !is.na(DT_SIN_PRI),
      DT_SIN_PRI >= DATA_INICIO,
      DT_SIN_PRI <= DATA_FIM
    ) %>%
    mutate(
      REGIAO = unname(MAP_UF_REGIAO[SG_UF]),
      REGIAO = factor(REGIAO, levels = ORDEM_REGIOES),
      ANO = year(DT_SIN_PRI),
      MES_NUM = month(DT_SIN_PRI),
      MES = floor_date(DT_SIN_PRI, "month"),
      SEMANA = floor_date(DT_SIN_PRI, "week", week_start = 1)
    )
}

zscore_seguro <- function(x) {
  if (all(is.na(x)) || stats::sd(x, na.rm = TRUE) == 0) return(rep(NA_real_, length(x)))
  as.numeric(scale(x))
}

rescale_seguro <- function(x) {
  if (all(is.na(x)) || diff(range(x, na.rm = TRUE)) == 0) return(rep(0, length(x)))
  scales::rescale(x, to = c(0, 1))
}

intensidade_correlacao <- function(r) {
  case_when(
    is.na(r) ~ NA_character_,
    abs(r) < 0.10 ~ "Muito fraca",
    abs(r) < 0.30 ~ "Fraca",
    abs(r) < 0.50 ~ "Moderada",
    abs(r) < 0.70 ~ "Forte",
    TRUE ~ "Muito forte"
  )
}

#### 1. CARREGAMENTO E HARMONIZAÇÃO DAS BASES DE VSR ####

base_2013_2018 <- carregar_rdata_objeto(
  ARQ_BASE_1318,
  nomes_preferidos = c("base_VSR_2013_2018")
)

base_2019_2025 <- carregar_rdata_objeto(
  ARQ_BASE_1925,
  nomes_preferidos = c("base_DEF_VSR_2019_2025")
)

base_vsr <- bind_rows(
  harmonizar_base_vsr(base_2013_2018, "2013_2018"),
  harmonizar_base_vsr(base_2019_2025, "2019_2025")
) %>%
  arrange(DT_SIN_PRI)

qa_base_vsr <- base_vsr %>%
  summarise(
    data_inicio = min(DT_SIN_PRI),
    data_fim = max(DT_SIN_PRI),
    registros = n(),
    uf_ausente = sum(is.na(SG_UF)),
    regiao_ausente = sum(is.na(REGIAO)),
    duplicados_exatos = sum(duplicated(base_vsr))
  )

print(qa_base_vsr)
salvar_csv(qa_base_vsr, "00_qa_base_vsr")
saveRDS(base_vsr, file.path(DIR_BASES, "base_vsr_harmonizada_2013_2025.rds"))

#### 2. CONSTRUÇÃO DAS SÉRIES TEMPORAIS ####

grade_mensal <- seq(floor_date(DATA_INICIO, "month"), floor_date(DATA_FIM, "month"), by = "month")
grade_semanal <- seq(floor_date(DATA_INICIO, "week", week_start = 1), floor_date(DATA_FIM, "week", week_start = 1), by = "1 week")

serie_mensal_br <- base_vsr %>%
  count(MES, name = "casos_vsr") %>%
  complete(MES = grade_mensal, fill = list(casos_vsr = 0L)) %>%
  arrange(MES) %>%
  mutate(ANO = year(MES), MES_NUM = month(MES))

serie_mensal_regiao <- base_vsr %>%
  filter(!is.na(REGIAO)) %>%
  count(REGIAO, MES, name = "casos_vsr") %>%
  group_by(REGIAO) %>%
  complete(MES = grade_mensal, fill = list(casos_vsr = 0L)) %>%
  ungroup() %>%
  mutate(
    REGIAO = factor(REGIAO, levels = ORDEM_REGIOES),
    ANO = year(MES),
    MES_NUM = month(MES)
  ) %>%
  arrange(REGIAO, MES)

serie_semanal_br <- base_vsr %>%
  count(SEMANA, name = "casos_vsr") %>%
  complete(SEMANA = grade_semanal, fill = list(casos_vsr = 0L)) %>%
  arrange(SEMANA) %>%
  mutate(ANO = year(SEMANA), SEMANA_EPI = epiweek(SEMANA))

serie_semanal_regiao <- base_vsr %>%
  filter(!is.na(REGIAO)) %>%
  count(REGIAO, SEMANA, name = "casos_vsr") %>%
  group_by(REGIAO) %>%
  complete(SEMANA = grade_semanal, fill = list(casos_vsr = 0L)) %>%
  ungroup() %>%
  mutate(
    REGIAO = factor(REGIAO, levels = ORDEM_REGIOES),
    ANO = year(SEMANA),
    SEMANA_EPI = epiweek(SEMANA)
  ) %>%
  arrange(REGIAO, SEMANA)

salvar_csv(serie_mensal_br, "01_serie_mensal_brasil")
salvar_csv(serie_mensal_regiao, "02_serie_mensal_regiao")
salvar_csv(serie_semanal_br, "03_serie_semanal_brasil")
salvar_csv(serie_semanal_regiao, "04_serie_semanal_regiao")

#### 3. SAZONALIDADE: DECOMPOSIÇÃO STL ####

criar_ts_semanal <- function(df, coluna_data = "SEMANA", coluna_valor = "casos_vsr") {
  ts(
    df[[coluna_valor]],
    frequency = FREQUENCIA_SEMANAL,
    start = c(year(min(df[[coluna_data]])), isoweek(min(df[[coluna_data]])))
  )
}

stl_br <- stl(criar_ts_semanal(serie_semanal_br), s.window = "periodic", robust = TRUE)

g_stl_br <- forecast::autoplot(stl_br) +
  labs(title = "Decomposição STL da série semanal de SRAG por VSR — Brasil") +
  theme_minimal(base_size = 12)

salvar_grafico(g_stl_br, "01_stl_brasil", DIR_GRAFICOS_SAZ, 12, 8)

stl_regioes <- serie_semanal_regiao %>%
  split(.$REGIAO) %>%
  purrr::imap(function(df, regiao) {
    objeto <- stl(criar_ts_semanal(df), s.window = "periodic", robust = TRUE)
    grafico <- forecast::autoplot(objeto) +
      labs(title = paste0("Decomposição STL da série semanal de SRAG por VSR — ", regiao)) +
      theme_minimal(base_size = 12)
    salvar_grafico(
      grafico,
      paste0("02_stl_", janitor::make_clean_names(regiao)),
      DIR_GRAFICOS_SAZ,
      12,
      8
    )
    objeto
  })

calcular_forca_componentes <- function(stl_obj, serie) {
  comp <- stl_obj$time.series
  var_resto <- var(comp[, "remainder"], na.rm = TRUE)
  forca_tendencia <- max(0, 1 - var_resto / var(comp[, "trend"] + comp[, "remainder"], na.rm = TRUE))
  forca_sazonalidade <- max(0, 1 - var_resto / var(comp[, "seasonal"] + comp[, "remainder"], na.rm = TRUE))
  
  tibble(
    serie = serie,
    forca_tendencia = round(forca_tendencia, 3),
    forca_sazonalidade = round(forca_sazonalidade, 3)
  )
}

forca_stl <- bind_rows(
  calcular_forca_componentes(stl_br, "Brasil"),
  purrr::imap_dfr(stl_regioes, ~ calcular_forca_componentes(.x, .y))
)

salvar_csv(forca_stl, "05_forca_componentes_stl")

#### 4. SAZONALIDADE: ACF, PACF E TESTES ####

criar_ts_mensal <- function(df) {
  ts(
    df$casos_vsr,
    frequency = 12,
    start = c(year(min(df$MES)), month(min(df$MES)))
  )
}

resumir_acf <- function(ts_obj, serie) {
  acf_obj <- acf(ts_obj, lag.max = 48, plot = FALSE)
  banda <- 1.96 / sqrt(length(ts_obj))
  
  extrair_lag <- function(lag_desejado) {
    idx <- which(round(acf_obj$lag * frequency(ts_obj)) == lag_desejado)[1]
    if (is.na(idx)) return(NA_real_)
    as.numeric(acf_obj$acf[idx])
  }
  
  diferenca_12 <- diff(ts_obj, lag = 12)
  ljung <- Box.test(diferenca_12, lag = min(24, floor(length(diferenca_12) / 5)), type = "Ljung-Box")
  
  tibble(
    serie = serie,
    n = length(ts_obj),
    banda_95 = banda,
    acf_12 = extrair_lag(12),
    acf_24 = extrair_lag(24),
    acf_36 = extrair_lag(36),
    significativo_12 = abs(acf_12) > banda_95,
    significativo_24 = abs(acf_24) > banda_95,
    significativo_36 = abs(acf_36) > banda_95,
    ljung_box_p_diferenca_12 = ljung$p.value
  )
}

ts_mensal_br <- criar_ts_mensal(serie_mensal_br)

g_acf_br <- forecast::ggAcf(ts_mensal_br, lag.max = 48) +
  labs(title = "ACF mensal da SRAG por VSR — Brasil") +
  theme_minimal(base_size = 12)

g_pacf_br <- forecast::ggPacf(ts_mensal_br, lag.max = 48) +
  labs(title = "PACF mensal da SRAG por VSR — Brasil") +
  theme_minimal(base_size = 12)

g_acf_dif_br <- forecast::ggAcf(diff(ts_mensal_br, lag = 12), lag.max = 48) +
  labs(title = "ACF após diferença sazonal de 12 meses — Brasil") +
  theme_minimal(base_size = 12)

salvar_grafico(g_acf_br, "03_acf_mensal_brasil", DIR_GRAFICOS_SAZ, 10, 6)
salvar_grafico(g_pacf_br, "04_pacf_mensal_brasil", DIR_GRAFICOS_SAZ, 10, 6)
salvar_grafico(g_acf_dif_br, "05_acf_diferenca_sazonal_brasil", DIR_GRAFICOS_SAZ, 10, 6)

acf_resumo <- bind_rows(
  resumir_acf(ts_mensal_br, "Brasil"),
  serie_mensal_regiao %>%
    split(.$REGIAO) %>%
    purrr::imap_dfr(~ resumir_acf(criar_ts_mensal(.x), .y))
) %>%
  mutate(
    conclusao = case_when(
      significativo_12 ~ "Sazonalidade anual evidente",
      significativo_24 ~ "Periodicidade de 24 meses evidente",
      TRUE ~ "Sem autocorrelação sazonal clara nos lags avaliados"
    )
  )

salvar_csv(acf_resumo, "06_resumo_acf")

mensal_teste <- serie_mensal_br %>%
  mutate(MES_NUM = factor(MES_NUM, levels = 1:12))

kruskal_meses <- kruskal.test(casos_vsr ~ MES_NUM, data = mensal_teste)

tabela_kruskal <- tibble(
  teste = "Kruskal-Wallis",
  estatistica = unname(kruskal_meses$statistic),
  gl = unname(kruskal_meses$parameter),
  p_valor = kruskal_meses$p.value
)

salvar_csv(tabela_kruskal, "07_teste_kruskal_sazonalidade_mensal")

#### 5. DADOS CLIMÁTICOS NASA POWER — REPRODUÇÃO DO SCRIPT SARI ####

# IMPORTANTE:
# Este bloco replica os parâmetros, coordenadas e a estrutura de tratamento
# utilizados no SARI_RSV_13_25.Rmd. A base bruta baixada é arquivada em BD/
# para que as próximas execuções usem exatamente os mesmos valores climáticos.
# Defina ATUALIZAR_CLIMA <- TRUE apenas quando desejar substituir esse arquivo.

COORD_REGIOES <- tibble::tribble(
  ~REGIAO,         ~lon,    ~lat,
  "Norte",        -60.0,   -3.5,
  "Nordeste",     -40.0,   -9.0,
  "Sudeste",      -45.0,  -22.0,
  "Sul",          -51.0,  -27.0,
  "Centro-Oeste", -55.0,  -15.0
)

ARQ_CLIMA_MENSAL_BRUTO <- file.path(
  DIR_BD, "clima_nasa_power_SARI_mensal_bruto_2013_2025.rds"
)
ARQ_CLIMA_DIARIO_BRUTO <- file.path(
  DIR_BD, "clima_nasa_power_SARI_diario_bruto_2013_2025.rds"
)

baixar_clima_regiao <- function(regiao, lon, lat) {
  nasapower::get_power(
    community = "AG",
    lonlat = c(lon, lat),
    pars = c("T2M", "RH2M", "PRECTOTCORR"),
    dates = c("2013-01-01", "2025-12-31"),
    temporal_api = "MONTHLY"
  ) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    mutate(REGIAO = regiao)
}

baixar_clima_diario <- function(regiao, lon, lat) {
  dados <- nasapower::get_power(
    community = "AG",
    lonlat = c(lon, lat),
    pars = c("T2M", "RH2M", "PRECTOTCORR"),
    dates = c("2013-01-01", "2025-12-31"),
    temporal_api = "DAILY"
  )
  dados$REGIAO <- regiao
  dados
}

# Base mensal bruta: mesma chamada do SARI.
if (ATUALIZAR_CLIMA || !file.exists(ARQ_CLIMA_MENSAL_BRUTO)) {
  message("Baixando a base climática mensal com a rotina original do SARI...")
  clima_regioes <- purrr::pmap_dfr(
    COORD_REGIOES,
    ~ baixar_clima_regiao(..1, ..2, ..3)
  )
  saveRDS(clima_regioes, ARQ_CLIMA_MENSAL_BRUTO)
} else {
  clima_regioes <- readRDS(ARQ_CLIMA_MENSAL_BRUTO)
  message("Base climática mensal SARI carregada: ", ARQ_CLIMA_MENSAL_BRUTO)
}

# Tratamento mensal equivalente ao RMarkdown original.
clima_mensal <- clima_regioes %>%
  select(REGIAO, parameter, year, jan:dec) %>%
  pivot_longer(
    cols = jan:dec,
    names_to = "mes_nome",
    values_to = "valor"
  ) %>%
  mutate(
    ANO = as.integer(year),
    MES_NUM = match(mes_nome, tolower(month.abb)),
    MES = as.Date(paste0(ANO, "-", stringr::str_pad(MES_NUM, 2, pad = "0"), "-01"))
  ) %>%
  select(REGIAO, MES, ANO, MES_NUM, parameter, valor) %>%
  pivot_wider(
    names_from = parameter,
    values_from = valor
  ) %>%
  janitor::clean_names() %>%
  rename(
    REGIAO = regiao,
    MES = mes,
    ANO = ano,
    MES_NUM = mes_num,
    temp_media = t2m,
    umidade_relativa = rh2m,
    precipitacao = prectotcorr
  ) %>%
  mutate(REGIAO = factor(REGIAO, levels = ORDEM_REGIOES)) %>%
  arrange(REGIAO, MES)

# Base diária bruta: mesma chamada usada na CCF semanal do SARI.
if (ATUALIZAR_CLIMA || !file.exists(ARQ_CLIMA_DIARIO_BRUTO)) {
  message("Baixando a base climática diária com a rotina original do SARI...")
  clima_diario <- purrr::pmap_dfr(
    COORD_REGIOES,
    ~ baixar_clima_diario(..1, ..2, ..3)
  )
  saveRDS(clima_diario, ARQ_CLIMA_DIARIO_BRUTO)
} else {
  clima_diario <- readRDS(ARQ_CLIMA_DIARIO_BRUTO)
  message("Base climática diária SARI carregada: ", ARQ_CLIMA_DIARIO_BRUTO)
}

# O SARI utilizou YEAR/MM/DD na correção robusta do clima semanal.
if (all(c("YEAR", "MM", "DD") %in% names(clima_diario))) {
  clima_diario <- clima_diario %>%
    mutate(
      YEAR = as.integer(YEAR),
      MM = as.integer(MM),
      DD = as.integer(DD),
      DATE = as.Date(sprintf("%04d-%02d-%02d", YEAR, MM, DD))
    )
} else if ("YYYYMMDD" %in% names(clima_diario)) {
  clima_diario <- clima_diario %>%
    mutate(DATE = as.Date(as.character(YYYYMMDD), format = "%Y%m%d"))
} else {
  stop("Não foi possível identificar a data na base diária da NASA POWER.", call. = FALSE)
}

clima_semanal <- clima_diario %>%
  mutate(
    REGIAO = stringr::str_trim(as.character(REGIAO)),
    SEMANA = lubridate::floor_date(DATE, unit = "week", week_start = 1)
  ) %>%
  filter(
    !is.na(DATE),
    SEMANA >= min(grade_semanal),
    SEMANA <= max(grade_semanal)
  ) %>%
  group_by(REGIAO, SEMANA) %>%
  summarise(
    temp_media = mean(T2M, na.rm = TRUE),
    umidade_relativa = mean(RH2M, na.rm = TRUE),
    precipitacao = sum(PRECTOTCORR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(REGIAO = factor(REGIAO, levels = ORDEM_REGIOES)) %>%
  arrange(REGIAO, SEMANA)

# Arquivos tratados e cópias em CSV para auditoria entre versões.
saveRDS(clima_mensal, file.path(DIR_BASES, "clima_mensal_SARI_2013_2025.rds"))
saveRDS(clima_semanal, file.path(DIR_BASES, "clima_semanal_SARI_2013_2025.rds"))
readr::write_csv(clima_mensal, file.path(DIR_TABELAS, "00_clima_mensal_usado_na_analise.csv"))
readr::write_csv(clima_semanal, file.path(DIR_TABELAS, "00_clima_semanal_usado_na_analise.csv"))

#### 6. INTEGRAÇÃO ENTRE VSR E CLIMA ####

base_vsr_clima_mensal <- serie_mensal_regiao %>%
  left_join(clima_mensal, by = c("REGIAO", "MES", "ANO", "MES_NUM")) %>%
  mutate(
    MES_NOME = factor(MESES_ABREV_PT[MES_NUM], levels = MESES_ABREV_PT),
    PERIODO = case_when(
      ANO %in% ANOS_PRE ~ "Pré-pandemia",
      ANO %in% ANOS_PANDEMIA ~ "Pandemia",
      ANO %in% ANOS_POS ~ "Pós-pandemia",
      TRUE ~ NA_character_
    )
  )

base_vsr_clima_semanal <- serie_semanal_regiao %>%
  left_join(clima_semanal, by = c("REGIAO", "SEMANA")) %>%
  mutate(
    PERIODO = case_when(
      ANO %in% ANOS_PRE ~ "Pré-pandemia",
      ANO %in% ANOS_PANDEMIA ~ "Pandemia",
      ANO %in% ANOS_POS ~ "Pós-pandemia",
      TRUE ~ NA_character_
    )
  )

qa_integracao <- bind_rows(
  base_vsr_clima_mensal %>%
    summarise(
      base = "Mensal",
      linhas = n(),
      sem_temperatura = sum(is.na(temp_media)),
      sem_umidade = sum(is.na(umidade_relativa)),
      sem_precipitacao = sum(is.na(precipitacao)),
      casos = sum(casos_vsr, na.rm = TRUE)
    ),
  base_vsr_clima_semanal %>%
    summarise(
      base = "Semanal",
      linhas = n(),
      sem_temperatura = sum(is.na(temp_media)),
      sem_umidade = sum(is.na(umidade_relativa)),
      sem_precipitacao = sum(is.na(precipitacao)),
      casos = sum(casos_vsr, na.rm = TRUE)
    )
)

print(qa_integracao)
salvar_csv(qa_integracao, "08_qa_integracao_vsr_clima")
saveRDS(base_vsr_clima_mensal, file.path(DIR_BASES, "base_vsr_clima_mensal.rds"))
saveRDS(base_vsr_clima_semanal, file.path(DIR_BASES, "base_vsr_clima_semanal.rds"))

base_mensal_sazonal <- base_vsr_clima_mensal %>%
  filter(!ANO %in% ANOS_PANDEMIA)

base_semanal_sazonal <- base_vsr_clima_semanal %>%
  filter(!ANO %in% ANOS_PANDEMIA)

#### 7. PERFIL SAZONAL MÉDIO E CORRELAÇÕES DE SPEARMAN ####

resumo_sazonal_clima <- base_mensal_sazonal %>%
  group_by(REGIAO, MES_NUM, MES_NOME) %>%
  summarise(
    media_casos_vsr = mean(casos_vsr, na.rm = TRUE),
    mediana_casos_vsr = median(casos_vsr, na.rm = TRUE),
    media_temp = mean(temp_media, na.rm = TRUE),
    media_umidade = mean(umidade_relativa, na.rm = TRUE),
    media_precipitacao = mean(precipitacao, na.rm = TRUE),
    .groups = "drop"
  )

calcular_spearman <- function(x, y) {
  pares <- complete.cases(x, y)
  if (sum(pares) < 4) return(tibble(rho = NA_real_, p_valor = NA_real_, n = sum(pares)))
  teste <- suppressWarnings(cor.test(x[pares], y[pares], method = "spearman", exact = FALSE))
  tibble(rho = unname(teste$estimate), p_valor = teste$p.value, n = sum(pares))
}

cor_spearman <- base_mensal_sazonal %>%
  group_by(REGIAO) %>%
  group_modify(~ bind_rows(
    calcular_spearman(.x$casos_vsr, .x$temp_media) %>% mutate(variavel = "Temperatura média"),
    calcular_spearman(.x$casos_vsr, .x$umidade_relativa) %>% mutate(variavel = "Umidade relativa"),
    calcular_spearman(.x$casos_vsr, .x$precipitacao) %>% mutate(variavel = "Precipitação")
  )) %>%
  ungroup() %>%
  mutate(
    rho = round(rho, 3),
    p_valor = signif(p_valor, 4),
    intensidade = intensidade_correlacao(rho)
  ) %>%
  select(REGIAO, variavel, n, rho, p_valor, intensidade)

salvar_csv(resumo_sazonal_clima, "09_resumo_sazonal_clima")
salvar_csv(cor_spearman, "10_correlacao_spearman_mensal")

#### 8. CORRELAÇÃO CRUZADA MENSAL E SEMANAL ####

calcular_ccf <- function(df, coluna_data, var_clima, max_lag, unidade) {
  df_valida <- df %>%
    arrange(.data[[coluna_data]]) %>%
    filter(!is.na(casos_vsr), !is.na(.data[[var_clima]]))
  
  if (nrow(df_valida) < 20) {
    return(tibble(lag = NA_real_, ccf = NA_real_, variavel = var_clima, unidade = unidade))
  }
  
  objeto <- ccf(
    x = df_valida[[var_clima]],
    y = df_valida$casos_vsr,
    lag.max = max_lag,
    plot = FALSE,
    na.action = na.omit
  )
  
  tibble(
    lag = as.numeric(objeto$lag),
    ccf = as.numeric(objeto$acf),
    variavel = var_clima,
    unidade = unidade
  )
}

ccf_mensal <- base_mensal_sazonal %>%
  group_by(REGIAO) %>%
  group_modify(~ bind_rows(
    calcular_ccf(.x, "MES", "temp_media", MAX_LAG_MENSAL, "meses"),
    calcular_ccf(.x, "MES", "umidade_relativa", MAX_LAG_MENSAL, "meses"),
    calcular_ccf(.x, "MES", "precipitacao", MAX_LAG_MENSAL, "meses")
  )) %>%
  ungroup()

ccf_semanal <- base_semanal_sazonal %>%
  group_by(REGIAO) %>%
  group_modify(~ bind_rows(
    calcular_ccf(.x, "SEMANA", "temp_media", MAX_LAG_SEMANAL, "semanas"),
    calcular_ccf(.x, "SEMANA", "umidade_relativa", MAX_LAG_SEMANAL, "semanas"),
    calcular_ccf(.x, "SEMANA", "precipitacao", MAX_LAG_SEMANAL, "semanas")
  )) %>%
  ungroup()

rotulos_variaveis <- c(
  temp_media = "Temperatura média",
  umidade_relativa = "Umidade relativa",
  precipitacao = "Precipitação"
)

resumir_ccf <- function(df) {
  df %>%
    filter(!is.na(ccf)) %>%
    group_by(REGIAO, variavel, unidade) %>%
    slice_max(abs(ccf), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      variavel = recode(variavel, !!!rotulos_variaveis),
      ccf = round(ccf, 3),
      intensidade = intensidade_correlacao(ccf),
      interpretacao = case_when(
        lag > 0 & ccf > 0 ~ "Aumento da variável climática antecede aumento do VSR",
        lag > 0 & ccf < 0 ~ "Redução da variável climática antecede aumento do VSR",
        lag < 0 ~ "VSR antecede a variável climática; interpretar com cautela",
        TRUE ~ "Associação contemporânea"
      )
    )
}

ccf_mensal_resumo <- resumir_ccf(ccf_mensal)
ccf_semanal_resumo <- resumir_ccf(ccf_semanal)

salvar_csv(ccf_mensal, "11_ccf_mensal_completa")
salvar_csv(ccf_mensal_resumo, "12_ccf_mensal_resumo")
salvar_csv(ccf_semanal, "13_ccf_semanal_completa")
salvar_csv(ccf_semanal_resumo, "14_ccf_semanal_resumo")

g_ccf_semanal <- ccf_semanal %>%
  filter(!is.na(ccf)) %>%
  mutate(variavel = recode(variavel, !!!rotulos_variaveis)) %>%
  ggplot(aes(lag, ccf)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_col(width = 0.75, alpha = 0.8) +
  geom_point(
    data = ccf_semanal_resumo,
    aes(lag, ccf),
    size = 2.5,
    inherit.aes = FALSE
  ) +
  facet_grid(REGIAO ~ variavel) +
  scale_x_continuous(breaks = seq(-MAX_LAG_SEMANAL, MAX_LAG_SEMANAL, 2)) +
  labs(
    title = "Correlação cruzada semanal entre clima e SRAG por VSR",
    subtitle = "Anos de 2020 e 2021 excluídos",
    x = "Defasagem em semanas",
    y = "Correlação cruzada",
    caption = "Lag positivo: a variável climática antecede a série de VSR."
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

salvar_grafico(g_ccf_semanal, "01_ccf_semanal", DIR_GRAFICOS_CLIMA, 14, 10)

#### 9. GRÁFICOS DO PERFIL SAZONAL E CLIMÁTICO ####

base_perfil <- resumo_sazonal_clima %>%
  group_by(REGIAO) %>%
  mutate(
    `SRAG por VSR` = rescale_seguro(media_casos_vsr),
    `Temperatura média` = rescale_seguro(media_temp),
    `Umidade relativa` = rescale_seguro(media_umidade),
    `Precipitação` = rescale_seguro(media_precipitacao)
  ) %>%
  ungroup() %>%
  select(REGIAO, MES_NUM, MES_NOME, `SRAG por VSR`, `Temperatura média`, `Umidade relativa`, `Precipitação`) %>%
  pivot_longer(
    cols = c(`SRAG por VSR`, `Temperatura média`, `Umidade relativa`, `Precipitação`),
    names_to = "variavel",
    values_to = "valor_padronizado"
  )

g_perfil_linhas <- ggplot(base_perfil, aes(MES_NUM, valor_padronizado, color = variavel, group = variavel)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  facet_wrap(~ REGIAO, ncol = 2) +
  scale_x_continuous(breaks = 1:12, labels = MESES_ABREV_PT) +
  labs(
    title = "Perfil sazonal médio da SRAG por VSR e variáveis climáticas",
    subtitle = "Valores reescalados entre 0 e 1 dentro de cada região; 2020–2021 excluídos",
    x = NULL,
    y = "Intensidade relativa",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

salvar_grafico(g_perfil_linhas, "02_perfil_sazonal_linhas", DIR_GRAFICOS_CLIMA, 13, 9)

g_perfil_radar <- ggplot(base_perfil, aes(MES_NOME, valor_padronizado, color = variavel, group = variavel)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.7) +
  coord_polar() +
  facet_wrap(~ REGIAO, ncol = 2) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  labs(
    title = "Perfil sazonal circular da SRAG por VSR e variáveis climáticas",
    subtitle = "Valores reescalados entre 0 e 1; 2020–2021 excluídos",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

salvar_grafico(g_perfil_radar, "03_perfil_sazonal_radar", DIR_GRAFICOS_CLIMA, 12, 10)

g_heatmap <- ggplot(base_perfil, aes(MES_NOME, variavel, fill = valor_padronizado)) +
  geom_tile(linewidth = 0.5, color = "white") +
  facet_wrap(~ REGIAO, ncol = 1) +
  scale_fill_gradient(low = "white", high = "black", labels = scales::percent_format()) +
  labs(
    title = "Heatmap sazonal da SRAG por VSR e variáveis climáticas",
    subtitle = "Intensidade relativa mensal por região; 2020–2021 excluídos",
    x = NULL,
    y = NULL,
    fill = "Intensidade"
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold", hjust = 0), panel.grid = element_blank())

salvar_grafico(g_heatmap, "04_heatmap_sazonal", DIR_GRAFICOS_CLIMA, 13, 10)


#### 9.1. PERFIS SAZONAIS COM CLASSIFICAÇÃO CLIMÁTICA ####

# A classificação climática combina temperatura e precipitação médias.
# Dentro de cada região, os meses são classificados em relação às medianas
# regionais de temperatura e precipitação:
#   - Frio e chuvoso
#   - Frio e seco
#   - Quente e chuvoso
#   - Quente e seco

# Paleta de alto contraste para projeção em datashow.
# A transparência é aplicada nos geom_rect(), preservando a legibilidade
# das linhas e permitindo distinguir as quatro classificações climáticas.
CORES_ESTACOES <- c(
  "Frio e chuvoso" = "#2E86C1",   # azul
  "Frio e seco" = "#AAB7B8",      # cinza azulado
  "Quente e chuvoso" = "#27AE60", # verde
  "Quente e seco" = "#F39C12"     # laranja
)

ALPHA_ESTACOES_SAZONAL <- 0.22
ALPHA_ESTACOES_TEMPORAL <- 0.20

classificar_estacao_climatica <- function(df, coluna_temp, coluna_prec) {
  df %>%
    group_by(REGIAO) %>%
    mutate(
      mediana_temp_classificacao = median(.data[[coluna_temp]], na.rm = TRUE),
      mediana_prec_classificacao = median(.data[[coluna_prec]], na.rm = TRUE),
      estacao_climatica = case_when(
        .data[[coluna_temp]] < mediana_temp_classificacao &
          .data[[coluna_prec]] >= mediana_prec_classificacao ~ "Frio e chuvoso",
        .data[[coluna_temp]] < mediana_temp_classificacao &
          .data[[coluna_prec]] < mediana_prec_classificacao ~ "Frio e seco",
        .data[[coluna_temp]] >= mediana_temp_classificacao &
          .data[[coluna_prec]] >= mediana_prec_classificacao ~ "Quente e chuvoso",
        .data[[coluna_temp]] >= mediana_temp_classificacao &
          .data[[coluna_prec]] < mediana_prec_classificacao ~ "Quente e seco",
        TRUE ~ NA_character_
      ),
      estacao_climatica = factor(
        estacao_climatica,
        levels = c("Frio e chuvoso", "Frio e seco", "Quente e chuvoso", "Quente e seco")
      )
    ) %>%
    ungroup()
}

perfil_sazonal_estacoes <- resumo_sazonal_clima %>%
  classificar_estacao_climatica("media_temp", "media_precipitacao") %>%
  group_by(REGIAO) %>%
  mutate(
    vsr_z = zscore_seguro(media_casos_vsr),
    temperatura_z = zscore_seguro(media_temp),
    umidade_z = zscore_seguro(media_umidade),
    precipitacao_z = zscore_seguro(media_precipitacao),
    xmin = MES_NUM - 0.5,
    xmax = MES_NUM + 0.5
  ) %>%
  ungroup()

salvar_csv(
  perfil_sazonal_estacoes %>%
    select(
      REGIAO, MES_NUM, MES_NOME, media_casos_vsr, media_temp,
      media_umidade, media_precipitacao, estacao_climatica
    ),
  "15_perfil_sazonal_classificacao_climatica"
)

criar_grafico_sazonal_estacoes <- function(
    base,
    coluna_clima,
    titulo_clima,
    nome_legenda_clima,
    nome_arquivo
) {
  base_longa <- base %>%
    transmute(
      REGIAO,
      MES_NUM,
      MES_NOME,
      estacao_climatica,
      xmin,
      xmax,
      `SRAG por VSR` = vsr_z,
      !!nome_legenda_clima := .data[[coluna_clima]]
    ) %>%
    pivot_longer(
      cols = c(`SRAG por VSR`, all_of(nome_legenda_clima)),
      names_to = "serie",
      values_to = "valor_padronizado"
    )
  
  grafico <- ggplot() +
    geom_rect(
      data = base,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = -Inf,
        ymax = Inf,
        fill = estacao_climatica
      ),
      alpha = ALPHA_ESTACOES_SAZONAL,
      inherit.aes = FALSE
    ) +
    geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
    geom_line(
      data = base_longa,
      aes(
        x = MES_NUM,
        y = valor_padronizado,
        color = serie,
        linetype = serie,
        group = serie
      ),
      linewidth = 0.95
    ) +
    geom_point(
      data = base_longa %>% filter(serie == "SRAG por VSR"),
      aes(x = MES_NUM, y = valor_padronizado, color = serie),
      size = 2,
      inherit.aes = FALSE
    ) +
    facet_wrap(~ REGIAO, ncol = 3) +
    scale_x_continuous(
      breaks = 1:12,
      labels = MESES_ABREV_PT,
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_fill_manual(values = CORES_ESTACOES, drop = FALSE) +
    scale_color_manual(
      values = setNames(
        c(
          "#5E2CA5",
          dplyr::case_when(
            nome_legenda_clima == "Temperatura média" ~ "#D55E00",
            nome_legenda_clima == "Umidade relativa" ~ "#009E73",
            nome_legenda_clima == "Precipitação" ~ "#0072B2",
            TRUE ~ "#4D4D4D"
          )
        ),
        c("SRAG por VSR", nome_legenda_clima)
      )
    ) +
    scale_linetype_manual(
      values = setNames(c("solid", "dashed"), c("SRAG por VSR", nome_legenda_clima))
    ) +
    labs(
      title = paste0("Sazonalidade média de SRAG por VSR e ", titulo_clima, " por região"),
      subtitle = paste0(
        "Séries mensais padronizadas por região, Brasil, ",
        year(DATA_INICIO), "–", year(DATA_FIM),
        " (excluindo 2020–2021)"
      ),
      x = NULL,
      y = "Valor padronizado",
      color = NULL,
      linetype = NULL,
      fill = "Estação climática",
      caption = paste0(
        "Linha contínua: SRAG por VSR; linha tracejada: ",
        tolower(nome_legenda_clima),
        ". Estações climáticas definidas pelas medianas regionais de temperatura e precipitação."
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "gray35"),
      legend.position = "top",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.caption = element_text(size = 8, color = "gray40")
    )
  
  salvar_grafico(grafico, nome_arquivo, DIR_GRAFICOS_CLIMA, 14, 8)
  grafico
}

g_sazonal_temp_estacoes <- criar_grafico_sazonal_estacoes(
  base = perfil_sazonal_estacoes,
  coluna_clima = "temperatura_z",
  titulo_clima = "temperatura média",
  nome_legenda_clima = "Temperatura média",
  nome_arquivo = "06_sazonalidade_media_vsr_temperatura_estacoes"
)

g_sazonal_umidade_estacoes <- criar_grafico_sazonal_estacoes(
  base = perfil_sazonal_estacoes,
  coluna_clima = "umidade_z",
  titulo_clima = "umidade relativa",
  nome_legenda_clima = "Umidade relativa",
  nome_arquivo = "07_sazonalidade_media_vsr_umidade_estacoes"
)

g_sazonal_prec_estacoes <- criar_grafico_sazonal_estacoes(
  base = perfil_sazonal_estacoes,
  coluna_clima = "precipitacao_z",
  titulo_clima = "precipitação mensal",
  nome_legenda_clima = "Precipitação",
  nome_arquivo = "08_sazonalidade_media_vsr_precipitacao_estacoes"
)

#### 9.1.1. PERFIL SAZONAL INTEGRADO: VSR E TRÊS VARIÁVEIS CLIMÁTICAS ####

base_sazonal_integrada <- perfil_sazonal_estacoes %>%
  transmute(
    REGIAO,
    MES_NUM,
    MES_NOME,
    estacao_climatica,
    xmin,
    xmax,
    `SRAG por VSR` = vsr_z,
    `Temperatura média` = temperatura_z,
    `Umidade relativa` = umidade_z,
    `Precipitação` = precipitacao_z
  ) %>%
  pivot_longer(
    cols = c(
      `SRAG por VSR`,
      `Temperatura média`,
      `Umidade relativa`,
      `Precipitação`
    ),
    names_to = "serie",
    values_to = "valor_padronizado"
  ) %>%
  mutate(
    serie = factor(
      serie,
      levels = c(
        "SRAG por VSR",
        "Temperatura média",
        "Umidade relativa",
        "Precipitação"
      )
    )
  )

g_sazonal_integrada <- ggplot() +
  geom_rect(
    data = perfil_sazonal_estacoes,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = -Inf,
      ymax = Inf,
      fill = estacao_climatica
    ),
    alpha = ALPHA_ESTACOES_SAZONAL,
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
  geom_line(
    data = base_sazonal_integrada,
    aes(
      x = MES_NUM,
      y = valor_padronizado,
      color = serie,
      linetype = serie,
      group = serie
    ),
    linewidth = 0.95
  ) +
  geom_point(
    data = base_sazonal_integrada %>% filter(serie == "SRAG por VSR"),
    aes(x = MES_NUM, y = valor_padronizado, color = serie),
    size = 2,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ REGIAO, ncol = 3) +
  scale_x_continuous(
    breaks = 1:12,
    labels = MESES_ABREV_PT,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(values = CORES_ESTACOES, drop = FALSE) +
  scale_color_manual(
    values = c(
      "SRAG por VSR" = "#5E2CA5",
      "Temperatura média" = "#D55E00",
      "Umidade relativa" = "#009E73",
      "Precipitação" = "#0072B2"
    ),
    drop = FALSE
  ) +
  scale_linetype_manual(
    values = c(
      "SRAG por VSR" = "solid",
      "Temperatura média" = "longdash",
      "Umidade relativa" = "dotdash",
      "Precipitação" = "dotted"
    ),
    drop = FALSE
  ) +
  labs(
    title = "Sazonalidade média de SRAG por VSR e variáveis climáticas por região",
    subtitle = paste0(
      "Séries mensais padronizadas por região, Brasil, ",
      year(DATA_INICIO), "–", year(DATA_FIM),
      " (excluindo 2020–2021)"
    ),
    x = NULL,
    y = "Valor padronizado",
    color = NULL,
    linetype = NULL,
    fill = "Estação climática",
    caption = paste0(
      "Linha contínua com pontos: SRAG por VSR. Linhas tracejadas: temperatura média, ",
      "umidade relativa e precipitação. Estações climáticas definidas pelas medianas ",
      "regionais de temperatura e precipitação."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray35"),
    legend.position = "top",
    legend.box = "vertical",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 8, color = "gray40")
  )

salvar_grafico(
  g_sazonal_integrada,
  "09_sazonalidade_media_vsr_todas_variaveis_estacoes",
  DIR_GRAFICOS_CLIMA,
  15,
  9
)

#### 9.2. RELAÇÃO TEMPORAL ENTRE VSR E TEMPERATURA POR PERÍODO ####

criar_grafico_temporal_temperatura <- function(
    base,
    data_inicio,
    data_fim,
    titulo_periodo,
    nome_arquivo
) {
  base_periodo <- base %>%
    filter(
      MES >= as.Date(data_inicio),
      MES <= as.Date(data_fim),
      !is.na(temp_media),
      !is.na(precipitacao)
    )
  
  if (nrow(base_periodo) == 0) {
    warning("Nenhum registro encontrado para o período ", titulo_periodo, ".")
    return(NULL)
  }
  
  base_periodo <- base_periodo %>%
    classificar_estacao_climatica("temp_media", "precipitacao") %>%
    group_by(REGIAO) %>%
    mutate(
      `SRAG por VSR` = zscore_seguro(casos_vsr),
      `Temperatura média` = zscore_seguro(temp_media),
      xmin = MES,
      xmax = MES %m+% months(1)
    ) %>%
    ungroup()
  
  base_longa <- base_periodo %>%
    select(REGIAO, MES, `SRAG por VSR`, `Temperatura média`) %>%
    pivot_longer(
      cols = c(`SRAG por VSR`, `Temperatura média`),
      names_to = "serie",
      values_to = "valor_padronizado"
    )
  
  grafico <- ggplot() +
    geom_rect(
      data = base_periodo,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = -Inf,
        ymax = Inf,
        fill = estacao_climatica
      ),
      alpha = ALPHA_ESTACOES_TEMPORAL,
      inherit.aes = FALSE
    ) +
    geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
    geom_line(
      data = base_longa,
      aes(x = MES, y = valor_padronizado, color = serie),
      linewidth = 0.8
    ) +
    facet_wrap(~ REGIAO, ncol = 1) +
    scale_color_manual(
      values = c("SRAG por VSR" = "#5E2CA5", "Temperatura média" = "#D55E00")
    ) +
    scale_fill_manual(values = CORES_ESTACOES, drop = FALSE) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(
      title = paste0(
        "Relação temporal entre SRAG por VSR e temperatura média mensal — ",
        titulo_periodo
      ),
      subtitle = "Séries padronizadas por região com classificação climática mensal baseada em temperatura e precipitação",
      x = NULL,
      y = "Valor padronizado",
      color = NULL,
      fill = "Estação climática",
      caption = "Fonte: SIVEP-Gripe e NASA POWER."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "gray35"),
      legend.position = "top",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.caption = element_text(size = 8, color = "gray40")
    )
  
  salvar_grafico(grafico, nome_arquivo, DIR_GRAFICOS_CLIMA, 14, 9)
  
  salvar_csv(
    base_periodo %>%
      select(
        REGIAO, MES, casos_vsr, temp_media, precipitacao,
        estacao_climatica, `SRAG por VSR`, `Temperatura média`
      ),
    paste0("16_base_temporal_temperatura_", stringr::str_replace_all(titulo_periodo, "[^0-9]+", "_"))
  )
  
  grafico
}

g_temporal_temp_pre <- criar_grafico_temporal_temperatura(
  base = base_vsr_clima_mensal,
  data_inicio = paste0(min(ANOS_PRE), "-01-01"),
  data_fim = paste0(max(ANOS_PRE), "-12-31"),
  titulo_periodo = paste0(min(ANOS_PRE), "–", max(ANOS_PRE)),
  nome_arquivo = "10_relacao_temporal_vsr_temperatura_2013_2019"
)

g_temporal_temp_pos <- criar_grafico_temporal_temperatura(
  base = base_vsr_clima_mensal,
  data_inicio = paste0(min(ANOS_PANDEMIA), "-01-01"),
  data_fim = as.character(DATA_FIM),
  titulo_periodo = paste0(min(ANOS_PANDEMIA), "–", year(DATA_FIM)),
  nome_arquivo = paste0(
    "11_relacao_temporal_vsr_temperatura_",
    min(ANOS_PANDEMIA), "_", year(DATA_FIM)
  )
)


base_mm4 <- base_semanal_sazonal %>%
  arrange(REGIAO, SEMANA) %>%
  group_by(REGIAO) %>%
  mutate(
    casos_vsr_mm4 = slider::slide_dbl(casos_vsr, mean, .before = 3, .complete = FALSE),
    temp_media_mm4 = slider::slide_dbl(temp_media, mean, .before = 3, .complete = FALSE),
    umidade_relativa_mm4 = slider::slide_dbl(umidade_relativa, mean, .before = 3, .complete = FALSE),
    precipitacao_mm4 = slider::slide_dbl(precipitacao, mean, .before = 3, .complete = FALSE)
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(temp_media_mm4, umidade_relativa_mm4, precipitacao_mm4),
    names_to = "variavel_climatica",
    values_to = "valor_clima"
  ) %>%
  mutate(
    variavel_climatica = recode(
      variavel_climatica,
      temp_media_mm4 = "Temperatura média",
      umidade_relativa_mm4 = "Umidade relativa",
      precipitacao_mm4 = "Precipitação"
    )
  ) %>%
  group_by(REGIAO, variavel_climatica) %>%
  mutate(
    VSR = zscore_seguro(casos_vsr_mm4),
    Clima = zscore_seguro(valor_clima)
  ) %>%
  ungroup() %>%
  select(REGIAO, SEMANA, variavel_climatica, VSR, Clima) %>%
  pivot_longer(c(VSR, Clima), names_to = "serie", values_to = "valor_padronizado")

g_series_mm4 <- ggplot(base_mm4, aes(SEMANA, valor_padronizado, color = serie)) +
  geom_line(linewidth = 0.45, alpha = 0.85) +
  facet_grid(REGIAO ~ variavel_climatica) +
  labs(
    title = "Séries semanais de SRAG por VSR e clima",
    subtitle = "Média móvel de quatro semanas e escore-z; 2020–2021 excluídos",
    x = NULL,
    y = "Escore-z",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

salvar_grafico(g_series_mm4, "05_series_semanais_mm4", DIR_GRAFICOS_CLIMA, 15, 10)

#### 10. ANOMALIAS CLIMÁTICAS E INDICADORES DA TEMPORADA ####

climatologia_semanal <- base_semanal_sazonal %>%
  group_by(REGIAO, SEMANA_EPI) %>%
  summarise(
    temp_esperada = mean(temp_media, na.rm = TRUE),
    umidade_esperada = mean(umidade_relativa, na.rm = TRUE),
    precipitacao_esperada = mean(precipitacao, na.rm = TRUE),
    .groups = "drop"
  )

base_anomalias <- base_semanal_sazonal %>%
  left_join(climatologia_semanal, by = c("REGIAO", "SEMANA_EPI")) %>%
  mutate(
    anomalia_temp = temp_media - temp_esperada,
    anomalia_umidade = umidade_relativa - umidade_esperada,
    anomalia_precipitacao = precipitacao - precipitacao_esperada,
    semana_mais_quente = anomalia_temp > 0,
    semana_mais_umida = anomalia_umidade > 0,
    semana_mais_chuvosa = anomalia_precipitacao > 0,
    semana_mais_seca = anomalia_precipitacao < 0
  )

limiares_temporada <- base_anomalias %>%
  group_by(REGIAO) %>%
  summarise(
    limiar_temporada = quantile(casos_vsr, PERCENTIL_TEMPORADA, na.rm = TRUE),
    .groups = "drop"
  )

base_temporada <- base_anomalias %>%
  left_join(limiares_temporada, by = "REGIAO") %>%
  mutate(semana_temporada = casos_vsr >= limiar_temporada)

calcular_indicadores_ano <- function(df) {
  df <- arrange(df, SEMANA_EPI)
  semanas_ativas <- df$SEMANA_EPI[df$semana_temporada %in% TRUE]
  idx_pico <- which.max(df$casos_vsr)
  
  tibble(
    semana_inicio = if (length(semanas_ativas) > 0) min(semanas_ativas) else NA_integer_,
    semana_fim = if (length(semanas_ativas) > 0) max(semanas_ativas) else NA_integer_,
    duracao_temporada = if (length(semanas_ativas) > 0) max(semanas_ativas) - min(semanas_ativas) + 1 else NA_integer_,
    semana_pico = df$SEMANA_EPI[idx_pico],
    intensidade_pico = max(df$casos_vsr, na.rm = TRUE),
    total_anual = sum(df$casos_vsr, na.rm = TRUE),
    media_anomalia_temp = mean(df$anomalia_temp, na.rm = TRUE),
    media_anomalia_umidade = mean(df$anomalia_umidade, na.rm = TRUE),
    media_anomalia_precipitacao = mean(df$anomalia_precipitacao, na.rm = TRUE),
    semanas_mais_quentes = sum(df$semana_mais_quente, na.rm = TRUE),
    semanas_mais_umidas = sum(df$semana_mais_umida, na.rm = TRUE),
    semanas_mais_chuvosas = sum(df$semana_mais_chuvosa, na.rm = TRUE),
    semanas_mais_secas = sum(df$semana_mais_seca, na.rm = TRUE)
  )
}

indicadores_sazonalidade <- base_temporada %>%
  group_by(REGIAO, ANO) %>%
  group_modify(~ calcular_indicadores_ano(.x)) %>%
  ungroup() %>%
  mutate(
    periodo = case_when(
      ANO %in% ANOS_PRE ~ "Pré-pandemia",
      ANO %in% ANOS_POS ~ "Pós-pandemia",
      TRUE ~ NA_character_
    ),
    periodo = factor(periodo, levels = c("Pré-pandemia", "Pós-pandemia"))
  )

salvar_csv(climatologia_semanal, "15_climatologia_semanal")
salvar_csv(base_anomalias, "16_base_anomalias_semanais")
salvar_csv(indicadores_sazonalidade, "17_indicadores_sazonalidade_anuais")

#### 11. ASSOCIAÇÃO ENTRE ANOMALIAS E INDICADORES SAZONAIS ####

correlacionar_indicadores <- function(df, x, y) {
  pares <- complete.cases(df[[x]], df[[y]])
  if (sum(pares) < 4) {
    return(tibble(indicador_clima = x, indicador_vsr = y, rho = NA_real_, p_valor = NA_real_, n = sum(pares)))
  }
  teste <- suppressWarnings(cor.test(df[[x]][pares], df[[y]][pares], method = "spearman", exact = FALSE))
  tibble(
    indicador_clima = x,
    indicador_vsr = y,
    rho = unname(teste$estimate),
    p_valor = teste$p.value,
    n = sum(pares)
  )
}

variaveis_clima_ind <- c(
  "media_anomalia_temp", "media_anomalia_umidade", "media_anomalia_precipitacao",
  "semanas_mais_quentes", "semanas_mais_umidas", "semanas_mais_chuvosas", "semanas_mais_secas"
)

variaveis_vsr_ind <- c(
  "semana_inicio", "semana_pico", "duracao_temporada", "intensidade_pico", "total_anual"
)

# A construção acima é reescrita abaixo de forma explícita para evitar ambiguidade
# entre o pronome .x do group_modify e o pronome .x do map.
cor_anomalias_sazonalidade <- indicadores_sazonalidade %>%
  group_by(REGIAO) %>%
  group_modify(function(dados_regiao, chave) {
    purrr::map_dfr(variaveis_clima_ind, function(var_clima) {
      purrr::map_dfr(variaveis_vsr_ind, function(var_vsr) {
        correlacionar_indicadores(dados_regiao, var_clima, var_vsr)
      })
    })
  }) %>%
  ungroup() %>%
  mutate(
    rho = round(rho, 3),
    p_valor = signif(p_valor, 4),
    intensidade = intensidade_correlacao(rho)
  )

salvar_csv(cor_anomalias_sazonalidade, "18_correlacoes_anomalias_indicadores")

#### 12. COMPARAÇÃO PRÉ E PÓS-PANDEMIA ####

indicadores_comparacao <- indicadores_sazonalidade %>%
  filter(!is.na(periodo))

resumo_pre_pos <- indicadores_comparacao %>%
  group_by(REGIAO, periodo) %>%
  summarise(
    n_anos = n(),
    media_semana_inicio = mean(semana_inicio, na.rm = TRUE),
    media_semana_pico = mean(semana_pico, na.rm = TRUE),
    media_duracao = mean(duracao_temporada, na.rm = TRUE),
    media_intensidade_pico = mean(intensidade_pico, na.rm = TRUE),
    media_total_anual = mean(total_anual, na.rm = TRUE),
    media_anomalia_temp = mean(media_anomalia_temp, na.rm = TRUE),
    media_anomalia_umidade = mean(media_anomalia_umidade, na.rm = TRUE),
    media_anomalia_precipitacao = mean(media_anomalia_precipitacao, na.rm = TRUE),
    media_semanas_quentes = mean(semanas_mais_quentes, na.rm = TRUE),
    .groups = "drop"
  )

comparacao_pre_pos <- resumo_pre_pos %>%
  pivot_longer(
    cols = -c(REGIAO, periodo, n_anos),
    names_to = "indicador",
    values_to = "media"
  ) %>%
  select(-n_anos) %>%
  pivot_wider(names_from = periodo, values_from = media) %>%
  mutate(diferenca_pos_menos_pre = `Pós-pandemia` - `Pré-pandemia`)

executar_wilcoxon <- function(df, indicador) {
  df_teste <- df %>% filter(!is.na(.data[[indicador]]), !is.na(periodo))
  
  if (n_distinct(df_teste$periodo) < 2) {
    return(tibble(indicador = indicador, estatistica = NA_real_, p_valor = NA_real_))
  }
  
  teste <- suppressWarnings(wilcox.test(df_teste[[indicador]] ~ df_teste$periodo, exact = FALSE))
  tibble(
    indicador = indicador,
    estatistica = unname(teste$statistic),
    p_valor = teste$p.value
  )
}

indicadores_teste <- c(
  "semana_inicio", "semana_pico", "duracao_temporada", "intensidade_pico",
  "total_anual", "media_anomalia_temp", "media_anomalia_umidade",
  "media_anomalia_precipitacao", "semanas_mais_quentes"
)

testes_pre_pos <- indicadores_comparacao %>%
  group_by(REGIAO) %>%
  group_modify(function(dados_regiao, chave) {
    purrr::map_dfr(indicadores_teste, ~ executar_wilcoxon(dados_regiao, .x))
  }) %>%
  ungroup() %>%
  mutate(
    p_valor = signif(p_valor, 4),
    significativo_5pct = p_valor < 0.05
  )

salvar_csv(resumo_pre_pos, "19_resumo_pre_pos")
salvar_csv(comparacao_pre_pos, "20_diferencas_pos_menos_pre")
salvar_csv(testes_pre_pos, "21_testes_wilcoxon_pre_pos")

#### 13. GRÁFICOS DE ANOMALIAS E COMPARAÇÃO ####

g_temp_pico <- ggplot(indicadores_comparacao, aes(media_anomalia_temp, semana_pico, color = periodo)) +
  geom_point(size = 2.7, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  facet_wrap(~ REGIAO) +
  labs(
    title = "Anomalia de temperatura e semana do pico de VSR",
    x = "Anomalia média de temperatura (°C)",
    y = "Semana epidemiológica do pico",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", strip.text = element_text(face = "bold"))

salvar_grafico(g_temp_pico, "01_anomalia_temperatura_semana_pico", DIR_GRAFICOS_ANOM, 12, 8)

g_quentes_intensidade <- ggplot(indicadores_comparacao, aes(semanas_mais_quentes, intensidade_pico, color = periodo)) +
  geom_point(size = 2.7, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  facet_wrap(~ REGIAO, scales = "free_y") +
  labs(
    title = "Semanas mais quentes e intensidade do pico de VSR",
    x = "Número anual de semanas com anomalia térmica positiva",
    y = "Maior número semanal de casos",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", strip.text = element_text(face = "bold"))

salvar_grafico(g_quentes_intensidade, "02_semanas_quentes_intensidade_pico", DIR_GRAFICOS_ANOM, 12, 8)

indicadores_long <- indicadores_comparacao %>%
  select(
    REGIAO, ANO, periodo, semana_inicio, semana_pico, duracao_temporada,
    intensidade_pico, media_anomalia_temp, media_anomalia_precipitacao,
    media_anomalia_umidade
  ) %>%
  pivot_longer(
    cols = -c(REGIAO, ANO, periodo),
    names_to = "indicador",
    values_to = "valor"
  ) %>%
  mutate(
    indicador = recode(
      indicador,
      semana_inicio = "Início da temporada",
      semana_pico = "Semana do pico",
      duracao_temporada = "Duração da temporada",
      intensidade_pico = "Intensidade do pico",
      media_anomalia_temp = "Anomalia de temperatura",
      media_anomalia_precipitacao = "Anomalia de precipitação",
      media_anomalia_umidade = "Anomalia de umidade"
    )
  )

g_indicadores_tempo <- ggplot(indicadores_long, aes(ANO, valor, color = periodo, group = periodo)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  facet_grid(indicador ~ REGIAO, scales = "free_y") +
  scale_x_continuous(breaks = sort(unique(indicadores_long$ANO))) +
  labs(
    title = "Indicadores anuais da sazonalidade do VSR e anomalias climáticas",
    subtitle = "Anos de 2020 e 2021 excluídos",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

salvar_grafico(g_indicadores_tempo, "03_indicadores_anuais", DIR_GRAFICOS_ANOM, 16, 12)

criar_boxplot_pre_pos <- function(indicador, titulo, eixo_y, nome_arquivo) {
  grafico <- ggplot(indicadores_comparacao, aes(periodo, .data[[indicador]], fill = periodo)) +
    geom_boxplot(alpha = 0.75, outlier.shape = NA) +
    geom_jitter(width = 0.12, alpha = 0.7, size = 1.7) +
    facet_wrap(~ REGIAO, scales = "free_y") +
    labs(title = titulo, x = NULL, y = eixo_y, fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", strip.text = element_text(face = "bold"))
  
  salvar_grafico(grafico, nome_arquivo, DIR_GRAFICOS_ANOM, 12, 8)
  grafico
}

criar_boxplot_pre_pos("semana_pico", "Semana do pico de VSR: pré versus pós-pandemia", "Semana epidemiológica", "04_semana_pico_pre_pos")
criar_boxplot_pre_pos("intensidade_pico", "Intensidade do pico de VSR: pré versus pós-pandemia", "Casos na semana de pico", "05_intensidade_pico_pre_pos")
criar_boxplot_pre_pos("duracao_temporada", "Duração da temporada de VSR: pré versus pós-pandemia", "Duração em semanas", "06_duracao_temporada_pre_pos")
criar_boxplot_pre_pos("media_anomalia_temp", "Anomalia de temperatura: pré versus pós-pandemia", "Anomalia média de temperatura (°C)", "07_anomalia_temperatura_pre_pos")

#### 14. EXPORTAÇÃO CONSOLIDADA ####

arquivo_excel <- file.path(DIR_RESULTADOS, "Resultados_VSR_Sazonalidade_Clima.xlsx")

writexl::write_xlsx(
  list(
    QA_base_VSR = qa_base_vsr,
    QA_integracao = qa_integracao,
    Forca_STL = forca_stl,
    Resumo_ACF = acf_resumo,
    Kruskal_meses = tabela_kruskal,
    Resumo_sazonal_clima = resumo_sazonal_clima,
    Classificacao_climatica = perfil_sazonal_estacoes %>%
      select(REGIAO, MES_NUM, MES_NOME, media_casos_vsr, media_temp,
             media_umidade, media_precipitacao, estacao_climatica),
    Spearman_mensal = cor_spearman,
    CCF_mensal = ccf_mensal_resumo,
    CCF_semanal = ccf_semanal_resumo,
    Indicadores_anuais = indicadores_sazonalidade,
    Cor_anomalias = cor_anomalias_sazonalidade,
    Resumo_pre_pos = resumo_pre_pos,
    Diferencas_pre_pos = comparacao_pre_pos,
    Wilcoxon_pre_pos = testes_pre_pos
  ),
  path = arquivo_excel
)

objetos_finais <- list(
  parametros = list(
    data_inicio = DATA_INICIO,
    data_fim = DATA_FIM,
    anos_pandemia = ANOS_PANDEMIA,
    anos_pre = ANOS_PRE,
    anos_pos = ANOS_POS,
    percentil_temporada = PERCENTIL_TEMPORADA
  ),
  base_vsr = base_vsr,
  base_vsr_clima_mensal = base_vsr_clima_mensal,
  base_vsr_clima_semanal = base_vsr_clima_semanal,
  stl_brasil = stl_br,
  stl_regioes = stl_regioes,
  acf_resumo = acf_resumo,
  cor_spearman = cor_spearman,
  perfil_sazonal_estacoes = perfil_sazonal_estacoes,
  ccf_mensal_resumo = ccf_mensal_resumo,
  ccf_semanal_resumo = ccf_semanal_resumo,
  indicadores_sazonalidade = indicadores_sazonalidade,
  comparacao_pre_pos = comparacao_pre_pos,
  testes_pre_pos = testes_pre_pos
)

saveRDS(objetos_finais, file.path(DIR_RESULTADOS, "objetos_analise_vsr_sazonalidade_clima.rds"))

#### 15. REGISTRO DA EXECUÇÃO ####

capture.output(
  sessionInfo(),
  file = file.path(DIR_LOGS, "sessionInfo.txt")
)

log_execucao <- tibble(
  data_hora_execucao = Sys.time(),
  data_inicio_analise = DATA_INICIO,
  data_fim_analise = DATA_FIM,
  registros_vsr = nrow(base_vsr),
  casos_vsr = nrow(base_vsr),
  arquivo_excel = arquivo_excel,
  atualizar_clima = ATUALIZAR_CLIMA
)

readr::write_csv(log_execucao, file.path(DIR_LOGS, "log_execucao.csv"))

message("\n============================================================")
message("ANÁLISE CONCLUÍDA COM SUCESSO")
message("Projeto: SRAG_VSR_CLIMA_BRA")
message("Resultados: ", DIR_RESULTADOS)
message("Excel consolidado: ", arquivo_excel)
message("============================================================\n")


#### 16. VERIFICAÇÃO DOS NOVOS PRODUTOS CLIMÁTICOS ####

arquivos_climaticos_esperados <- file.path(
  DIR_GRAFICOS_CLIMA,
  paste0(
    c(
      "06_sazonalidade_media_vsr_temperatura_estacoes",
      "07_sazonalidade_media_vsr_umidade_estacoes",
      "08_sazonalidade_media_vsr_precipitacao_estacoes",
      "09_sazonalidade_media_vsr_todas_variaveis_estacoes",
      "10_relacao_temporal_vsr_temperatura_2013_2019",
      paste0("11_relacao_temporal_vsr_temperatura_", min(ANOS_PANDEMIA), "_", year(DATA_FIM))
    ),
    ".png"
  )
)

verificacao_figuras_climaticas <- tibble::tibble(
  arquivo = arquivos_climaticos_esperados,
  criado = file.exists(arquivos_climaticos_esperados)
)

print(verificacao_figuras_climaticas)

if (!all(verificacao_figuras_climaticas$criado)) {
  warning(
    "A execução terminou sem gerar todas as seis figuras climáticas novas. ",
    "Confira as mensagens de erro anteriores e a pasta: ",
    normalizePath(DIR_GRAFICOS_CLIMA, winslash = "/", mustWork = FALSE)
  )
} else {
  message(
    "OK: as seis figuras climáticas novas foram criadas em: ",
    normalizePath(DIR_GRAFICOS_CLIMA, winslash = "/", mustWork = FALSE)
  )
}

