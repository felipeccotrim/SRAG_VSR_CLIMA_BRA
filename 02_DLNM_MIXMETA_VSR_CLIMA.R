#### PROJETO SRAG_VSR_CLIMA_BRA ####
#### DLNM + MIXMETA: MODELO DE DEFASAGEM DISTRIBUIDA NAO-LINEAR POR REGIAO ####

# Autor: Felipe Cotrim
# Objetivo: substituir a CCF (escolha do "melhor lag" entre 17 testados, sem
#   correcao de multiplicidade) por uma curva continua de associacao
#   temperatura-VSR com IC, via DLNM (Gasparrini) por regiao + pooling
#   multivariado (mixmeta), no mesmo espirito dos estudos multi-cidade de
#   temperatura-mortalidade.
#
# Metodologia (Gasparrini et al., two-stage DLNM meta-analysis):
#   Estagio 1: um modelo DLNM por macrorregiao (crossbasis de temperatura +
#              controle de tendencia de longo prazo), reduzido a uma
#              associacao cumulativa resumida (crossreduce).
#   Estagio 2: os coeficientes reduzidos de cada regiao entram como "outcome"
#              multivariado num mixmeta -> curva pooled nacional + curvas
#              regionais "encolhidas" (BLUP).
#
# Pre-requisito: rode antes o 01_VSR_SAZONALIDADE_CLIMA.R (gera
#   Resultados/Bases_processadas/base_vsr_clima_semanal.rds, usado abaixo).

#### 0. CONFIGURACAO INICIAL ####

rm(list = ls())
gc()
options(stringsAsFactors = FALSE, scipen = 999)

pacotes <- c("here", "dplyr", "purrr", "tidyr", "readr", "ggplot2",
             "splines", "dlnm", "mixmeta", "MASS")
pacotes_ausentes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(pacotes_ausentes) > 0) {
  stop(
    paste0("Instale os seguintes pacotes antes de executar o script:\n",
           paste0("install.packages(c(", paste(sprintf('"%s"', pacotes_ausentes), collapse = ", "), "))")),
    call. = FALSE
  )
}
suppressPackageStartupMessages(invisible(lapply(pacotes, library, character.only = TRUE)))

#### 0.1. PARAMETROS DO MODELO ####

LAG_MAX     <- 8            # mesma janela usada na CCF original (+-8 semanas)
DF_LAG      <- 4            # graus de liberdade da spline lag-resposta
DF_TIME_ANO <- 7            # graus de liberdade/ano p/ tendencia de longo prazo
DF_VAR_CANDIDATOS <- c(3, 4)  # graus de liberdade da spline exposicao-resposta a testar

REGIOES <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")

#### 0.2. CAMINHOS ####

DIR_RESULTADOS <- here::here("Resultados")
DIR_BASES      <- file.path(DIR_RESULTADOS, "Bases_processadas")
DIR_TABELAS    <- file.path(DIR_RESULTADOS, "Tabelas")
DIR_GRAFICOS   <- file.path(DIR_RESULTADOS, "Graficos", "04_DLNM_Mixmeta")
dir.create(DIR_GRAFICOS, recursive = TRUE, showWarnings = FALSE)

ARQ_BASE_SEMANAL <- file.path(DIR_BASES, "base_vsr_clima_semanal.rds")

#### 1. CARREGAR AS SERIES SEMANAIS POR REGIAO ####

if (!file.exists(ARQ_BASE_SEMANAL)) {
  stop("Nao encontrei ", ARQ_BASE_SEMANAL, ". Rode antes o 01_VSR_SAZONALIDADE_CLIMA.R.", call. = FALSE)
}

base_semanal <- readRDS(ARQ_BASE_SEMANAL) %>%
  dplyr::transmute(
    regiao         = as.character(REGIAO),
    data           = SEMANA,
    casos          = casos_vsr,
    temp_media     = temp_media,
    umidade_media  = umidade_relativa,
    precipitacao   = precipitacao
  ) %>%
  dplyr::arrange(regiao, data)

lista_regioes <- split(base_semanal, base_semanal$regiao)[REGIOES]

purrr::iwalk(lista_regioes, function(df, reg) {
  cat(sprintf("%-14s | n=%d semanas | %s a %s | casos totais=%d\n",
              reg, nrow(df), min(df$data), max(df$data), sum(df$casos)))
})

#### 2. ESCOLHA DO DF COMUM DA SPLINE DE EXPOSICAO (via QAIC, quasi-Poisson) ##
##
## O mixmeta so pode combinar os coeficientes reduzidos das 5 regioes se eles
## tiverem a MESMA dimensao -- ou seja, a crossbasis de temperatura precisa
## usar o mesmo df em todas as regioes (nao da pra "escolher o melhor df por
## regiao" independentemente, isso quebra o pooling). Comparamos df=3 vs df=4
## por QAIC (Peng, Dominici & Louis, 2006) somado nas 5 regioes, sob
## quasi-Poisson -- a familia padrao nos estudos multi-cidade de Gasparrini
## para series de contagem com sobredispersao de forma desconhecida (ver
## secao 2.1 sobre a comparacao com binomial negativa).

qaic <- function(model) {
  fitted_vals <- model$fitted.values
  y <- model$y
  phi <- summary(model)$dispersion
  loglik <- sum(dpois(y, fitted_vals, log = TRUE))
  edf <- summary(model)$df[3]
  -2 * loglik / phi + 2 * edf
}

ajustar_temp_qp <- function(df, df_var, df_time_total) {
  cb_temp <- crossbasis(df$temp_media, lag = LAG_MAX,
                         argvar = list(fun = "ns", df = df_var),
                         arglag = list(fun = "ns", df = DF_LAG))
  tempo <- as.numeric(df$data - min(df$data)) / 7
  modelo <- glm(casos ~ cb_temp + ns(tempo, df = df_time_total),
                family = quasipoisson(), data = df, na.action = na.exclude)
  list(modelo = modelo, cb = cb_temp, qaic = qaic(modelo))
}

grade_df <- tidyr::expand_grid(regiao = REGIOES, df_var = DF_VAR_CANDIDATOS)
grade_df$qaic <- purrr::pmap_dbl(grade_df, function(regiao, df_var) {
  df <- lista_regioes[[regiao]]
  n_anos <- as.numeric(diff(range(df$data))) / 365.25
  ajustar_temp_qp(df, df_var = df_var, df_time_total = round(DF_TIME_ANO * n_anos))$qaic
})

cat("\n---- QAIC por regiao e df da spline de exposicao (quasi-Poisson) ----\n")
print(tidyr::pivot_wider(grade_df, names_from = df_var, values_from = qaic, names_prefix = "df"), n = Inf)

totais_df <- grade_df %>% dplyr::group_by(df_var) %>% dplyr::summarise(qaic_total = sum(qaic), .groups = "drop")
cat("\nQAIC total (soma das 5 regioes) por df candidato:\n"); print(totais_df)

DF_VAR_COMUM <- totais_df$df_var[which.min(totais_df$qaic_total)]
cat("\nDF comum escolhido para a crossbasis de temperatura: df=", DF_VAR_COMUM,
    " (menor QAIC total; ver tabela acima para preferencias individuais por regiao)\n", sep = "")

#### 2.1 CHECAGEM DE ROBUSTEZ: quasi-Poisson vs. binomial negativa ###########
## Nao usamos essa comparacao para escolher a familia -- QAIC (quasi-
## verossimilhanca, dividida pela dispersao) e AIC (log-verossimilhanca plena
## da binomial negativa) nao estao na mesma escala, entao comparar os dois
## numeros diretamente seria enganoso. Em vez disso, ajustamos as duas
## familias no df comum escolhido e comparamos os RR estimados (p10 vs p90)
## para confirmar que a estimativa pontual nao muda de forma relevante com a
## familia -- so a incerteza (erro padrao) tende a diferir.

comparar_familias_regiao <- function(reg) {
  df <- lista_regioes[[reg]]
  n_anos <- as.numeric(diff(range(df$data))) / 365.25
  df_time_total <- round(DF_TIME_ANO * n_anos)
  cb <- crossbasis(df$temp_media, lag = LAG_MAX,
                    argvar = list(fun = "ns", df = DF_VAR_COMUM),
                    arglag = list(fun = "ns", df = DF_LAG))
  tempo <- as.numeric(df$data - min(df$data)) / 7
  centro <- median(df$temp_media, na.rm = TRUE)

  mod_qp <- glm(df$casos ~ cb + ns(tempo, df = df_time_total), family = quasipoisson(), na.action = na.exclude)
  red_qp <- crossreduce(cb, mod_qp, cen = centro, type = "overall")

  mod_nb <- tryCatch(
    MASS::glm.nb(df$casos ~ cb + ns(tempo, df = df_time_total), na.action = na.exclude),
    error = function(e) NULL
  )

  p10 <- quantile(df$temp_media, 0.10, na.rm = TRUE)
  p90 <- quantile(df$temp_media, 0.90, na.rm = TRUE)
  pred_qp <- crosspred(cb, mod_qp, cen = centro, at = c(p10, p90))

  if (is.null(mod_nb)) {
    return(data.frame(regiao = reg, rr_p10_qp = pred_qp$allRRfit[1], rr_p90_qp = pred_qp$allRRfit[2],
                       rr_p10_nb = NA, rr_p90_nb = NA, obs = "binomial negativa nao convergiu"))
  }
  red_nb <- crossreduce(cb, mod_nb, cen = centro, type = "overall")
  pred_nb <- crosspred(cb, mod_nb, cen = centro, at = c(p10, p90))
  data.frame(regiao = reg, rr_p10_qp = pred_qp$allRRfit[1], rr_p90_qp = pred_qp$allRRfit[2],
             rr_p10_nb = pred_nb$allRRfit[1], rr_p90_nb = pred_nb$allRRfit[2], obs = "")
}

comparacao_familias <- purrr::map_dfr(REGIOES, comparar_familias_regiao)
cat("\n---- Robustez: RR (p10/p90) quasi-Poisson vs. binomial negativa, df comum ----\n")
print(comparacao_familias)
readr::write_csv(comparacao_familias, file.path(DIR_TABELAS, "25_dlnm_robustez_familia_qp_vs_nb.csv"), na = "")

#### 3. ESTAGIO 1 (FINAL): DLNM POR REGIAO, QUASI-POISSON, DF COMUM ##########

ajustar_dlnm_regiao <- function(df, df_var, df_time_total) {
  cb_temp <- crossbasis(df$temp_media, lag = LAG_MAX,
                         argvar = list(fun = "ns", df = df_var),
                         arglag = list(fun = "ns", df = DF_LAG))
  tempo <- as.numeric(df$data - min(df$data)) / 7
  modelo <- glm(casos ~ cb_temp + ns(tempo, df = df_time_total),
                family = quasipoisson(), data = df, na.action = na.exclude)

  centro <- median(df$temp_media, na.rm = TRUE)
  red <- crossreduce(cb_temp, modelo, cen = centro, type = "overall")

  list(modelo = modelo, cb = cb_temp, reduced = red,
       coef = coef(red), vcov = vcov(red), centro = centro,
       df_var = df_var, familia = "quasipoisson", n_semanas = nrow(df))
}

resultados_regionais <- purrr::map(REGIOES, function(reg) {
  df <- lista_regioes[[reg]]
  n_anos <- as.numeric(diff(range(df$data))) / 365.25
  ajustar_dlnm_regiao(df, df_var = DF_VAR_COMUM, df_time_total = round(DF_TIME_ANO * n_anos))
}) %>% setNames(REGIOES)

cat("\n---- Estagio 1 final: quasi-Poisson, df=", DF_VAR_COMUM, " (comum), por regiao ----\n", sep = "")
purrr::iwalk(resultados_regionais, function(r, reg) {
  cat(sprintf("%-14s | n=%d semanas | centro=%.1fC | dispersao=%.1f | coefs=%d\n",
              reg, r$n_semanas, r$centro, summary(r$modelo)$dispersion, length(r$coef)))
})

#### 4. ESTAGIO 2: POOLING VIA MIXMETA ########################################

coef_matrix <- do.call(rbind, lapply(resultados_regionais, `[[`, "coef"))
vcov_list   <- lapply(resultados_regionais, `[[`, "vcov")
rownames(coef_matrix) <- REGIOES

mv <- mixmeta(coef_matrix, S = vcov_list, method = "reml")
resumo_mv <- summary(mv)
cat("\n---- mixmeta: resumo (inclui teste de heterogeneidade Q / I^2) ----\n")
print(resumo_mv)

#### 5. PREDICAO: CURVA POOLED NACIONAL + CURVAS REGIONAIS (BLUP) ############

temp_todas <- unlist(lapply(lista_regioes, function(d) d$temp_media))
grade_temp <- seq(quantile(temp_todas, 0.01, na.rm = TRUE),
                   quantile(temp_todas, 0.99, na.rm = TRUE), length.out = 100)

## onebasis generica com o mesmo df comum usado em todas as crossbasis
## regionais (DF_VAR_COMUM), necessaria para aplicar os coeficientes
## pooled/BLUP fora do crossreduce original de cada regiao.
bvar_pooled <- onebasis(grade_temp, fun = "ns", df = DF_VAR_COMUM)
centro_nacional <- median(temp_todas, na.rm = TRUE)

## "at = grade_temp" (nao "by = 0.5") para a grade de predicao ser EXATAMENTE
## grade_temp -- com "by" o crosspred gera sua propria grade a partir do
## range/passo e ignora os pontos de grade_temp, o que descasa os vetores
## usados mais adiante (df_plot, resumo_rr) e quebra por tamanho distinto.
pred_pooled <- crosspred(bvar_pooled, coef = coef(mv), vcov = vcov(mv),
                          model.link = "log", cen = centro_nacional, at = grade_temp)

blups <- blup(mv, vcov = TRUE)

pred_regionais <- purrr::imap(blups, function(b, reg) {
  crosspred(bvar_pooled, coef = b$blup, vcov = b$vcov,
            model.link = "log", cen = centro_nacional, at = grade_temp)
})

#### 6. FIGURA: CURVAS POOLED + BLUP (substitui a Figura 2 de CCF) ###########

## as curvas BLUP sao preditas numa grade comum (grade_temp, faixa nacional
## 1o-99o percentil combinando as 5 regioes) para poderem ser sobrepostas na
## mesma escala -- mas cada regiao so tem dados observados numa faixa mais
## estreita dessa grade. Fora do proprio intervalo observado, a curva regional
## e extrapolacao do modelo, nao suportada pelos dados; isso e o que causa
## instabilidade nos extremos (ex.: Sul, regiao mais fria, extrapolando para
## as temperaturas mais quentes da grade nacional). Seguindo a pratica padrao
## de Gasparrini nos estudos multi-cidade, truncamos cada curva regional ao
## seu proprio intervalo observado (min-max de temp_media na regiao).
df_plot <- purrr::imap_dfr(pred_regionais, function(p, reg) {
  faixa_reg <- range(lista_regioes[[reg]]$temp_media, na.rm = TRUE)
  data.frame(regiao = reg, temp = grade_temp, rr = p$allRRfit, lo = p$allRRlow, hi = p$allRRhigh) %>%
    dplyr::filter(temp >= faixa_reg[1], temp <= faixa_reg[2])
})
df_pooled_plot <- data.frame(regiao = "Pooled (nacional)", temp = grade_temp,
                              rr = pred_pooled$allRRfit, lo = pred_pooled$allRRlow, hi = pred_pooled$allRRhigh)
df_fig <- dplyr::bind_rows(df_plot, df_pooled_plot) %>%
  dplyr::mutate(regiao = factor(regiao, levels = c(REGIOES, "Pooled (nacional)")))

## rotulos em ingles (figura destinada ao manuscrito RSBMT, que e em ingles)
REGIOES_EN <- c(Norte = "North", Nordeste = "Northeast", `Centro-Oeste` = "Midwest",
                Sudeste = "Southeast", Sul = "South")
df_fig <- df_fig %>%
  dplyr::mutate(regiao_en = dplyr::recode(as.character(regiao), !!!REGIOES_EN,
                                           `Pooled (nacional)` = "Pooled (national)"),
                regiao_en = factor(regiao_en, levels = c(unname(REGIOES_EN), "Pooled (national)")))

fig_dlnm <- ggplot(df_fig, aes(x = temp, y = rr, color = regiao_en, fill = regiao_en)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
  geom_line(aes(linewidth = regiao_en == "Pooled (national)")) +
  scale_linewidth_manual(values = c(`TRUE` = 1.4, `FALSE` = 0.8), guide = "none") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  labs(x = "Mean weekly temperature (°C)", y = "Relative risk of RSV SARI",
       title = "Exposure-response association between temperature and RSV (reduced DLNM): pooled vs. regional BLUP curves",
       subtitle = paste0("Shaded area = 95% CI. Thicker line = national pooled curve (mixmeta). ",
                          "Centered on the national median (", round(centro_nacional, 1), "°C)."),
       color = "Region", fill = "Region") +
  theme_minimal()

ggsave(file.path(DIR_GRAFICOS, "22_dlnm_mixmeta_pooled_blup.png"),
       fig_dlnm, width = 10, height = 7, dpi = 300, units = "in", bg = "white")

#### 7. TABELA-RESUMO PARA O ARTIGO (RR em p10/mediana/p90 por regiao) #######

resumo_rr <- purrr::imap_dfr(pred_regionais, function(p, reg) {
  refs <- quantile(lista_regioes[[reg]]$temp_media, c(0.10, 0.50, 0.90), na.rm = TRUE)
  idx <- vapply(refs, function(x) which.min(abs(grade_temp - x)), integer(1))
  data.frame(
    regiao        = reg,
    familia       = resultados_regionais[[reg]]$familia,
    df_var        = resultados_regionais[[reg]]$df_var,
    temp_p10      = round(refs[1], 1),  rr_p10 = round(p$allRRfit[idx[1]], 2),
    rr_p10_lo     = round(p$allRRlow[idx[1]], 2), rr_p10_hi = round(p$allRRhigh[idx[1]], 2),
    temp_mediana  = round(refs[2], 1),  rr_mediana = round(p$allRRfit[idx[2]], 2),
    rr_mediana_lo = round(p$allRRlow[idx[2]], 2), rr_mediana_hi = round(p$allRRhigh[idx[2]], 2),
    temp_p90      = round(refs[3], 1),  rr_p90 = round(p$allRRfit[idx[3]], 2),
    rr_p90_lo     = round(p$allRRlow[idx[3]], 2), rr_p90_hi = round(p$allRRhigh[idx[3]], 2)
  )
})

refs_nac <- quantile(temp_todas, c(0.10, 0.50, 0.90), na.rm = TRUE)
idx_nac <- vapply(refs_nac, function(x) which.min(abs(grade_temp - x)), integer(1))
resumo_pooled <- data.frame(
  regiao = "Pooled (nacional)", familia = "quasipoisson", df_var = DF_VAR_COMUM,
  temp_p10 = round(refs_nac[1], 1), rr_p10 = round(pred_pooled$allRRfit[idx_nac[1]], 2),
  rr_p10_lo = round(pred_pooled$allRRlow[idx_nac[1]], 2), rr_p10_hi = round(pred_pooled$allRRhigh[idx_nac[1]], 2),
  temp_mediana = round(refs_nac[2], 1), rr_mediana = round(pred_pooled$allRRfit[idx_nac[2]], 2),
  rr_mediana_lo = round(pred_pooled$allRRlow[idx_nac[2]], 2), rr_mediana_hi = round(pred_pooled$allRRhigh[idx_nac[2]], 2),
  temp_p90 = round(refs_nac[3], 1), rr_p90 = round(pred_pooled$allRRfit[idx_nac[3]], 2),
  rr_p90_lo = round(pred_pooled$allRRlow[idx_nac[3]], 2), rr_p90_hi = round(pred_pooled$allRRhigh[idx_nac[3]], 2)
)

tabela_rr_final <- dplyr::bind_rows(resumo_rr, resumo_pooled)
cat("\n---- Tabela-resumo RR por regiao (p10 / mediana / p90 de temperatura) ----\n")
print(tabela_rr_final)
readr::write_csv(tabela_rr_final, file.path(DIR_TABELAS, "22_dlnm_mixmeta_rr_por_regiao.csv"), na = "")

## qstat$Q/df/pvalue/i2stat sao vetores nomeados (.all = teste global; b1/b2/b3 =
## por coeficiente da spline) -- usamos so o ".all" (teste global de heterogeneidade).
heterog <- data.frame(
  estatistica = c("Q", "df", "p_valor", "I2_pct"),
  valor = c(resumo_mv$qstat$Q[".all"], resumo_mv$qstat$df[".all"],
            resumo_mv$qstat$pvalue[".all"], resumo_mv$i2stat[".all"])
)
readr::write_csv(heterog, file.path(DIR_TABELAS, "23_dlnm_mixmeta_heterogeneidade.csv"), na = "")

#### 8. SUPLEMENTAR: CROSSBASIS DE UMIDADE PARA O NORDESTE ###################
## A CCF ja apontou umidade como co-driver no Nordeste (lag de 1 semana). Aqui
## testamos uma crossbasis paralela de umidade (bivariada, temp + umidade) so
## para essa regiao, dado que as demais regioes tem N insuficiente para dois
## graus de liberdade extras sem sobreajuste.

df_ne <- lista_regioes[["Nordeste"]]

cb_temp_ne <- crossbasis(df_ne$temp_media, lag = LAG_MAX,
                          argvar = list(fun = "ns", df = DF_VAR_COMUM),
                          arglag = list(fun = "ns", df = DF_LAG))
cb_umid_ne <- crossbasis(df_ne$umidade_media, lag = LAG_MAX,
                          argvar = list(fun = "ns", df = 3),
                          arglag = list(fun = "ns", df = DF_LAG))
tempo_ne <- as.numeric(df_ne$data - min(df_ne$data)) / 7
n_anos_ne <- as.numeric(diff(range(df_ne$data))) / 365.25

modelo_ne_bivariado <- glm(
  casos ~ cb_temp_ne + cb_umid_ne + ns(tempo_ne, df = round(DF_TIME_ANO * n_anos_ne)),
  family = quasipoisson(), data = df_ne, na.action = na.exclude
)

centro_umid_ne <- median(df_ne$umidade_media, na.rm = TRUE)
red_umid_ne <- crossreduce(cb_umid_ne, modelo_ne_bivariado, cen = centro_umid_ne, type = "overall")

grade_umid <- seq(quantile(df_ne$umidade_media, 0.01, na.rm = TRUE),
                   quantile(df_ne$umidade_media, 0.99, na.rm = TRUE), length.out = 100)
bvar_umid <- onebasis(grade_umid, fun = "ns", df = 3)
pred_umid_ne <- crosspred(bvar_umid, coef = coef(red_umid_ne), vcov = vcov(red_umid_ne),
                           model.link = "log", cen = centro_umid_ne, at = grade_umid)

fig_umid_ne <- ggplot(data.frame(umid = grade_umid, rr = pred_umid_ne$allRRfit,
                                  lo = pred_umid_ne$allRRlow, hi = pred_umid_ne$allRRhigh),
                       aes(x = umid, y = rr)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  labs(x = "Mean weekly relative humidity (%)", y = "Relative risk of RSV SARI",
       title = "Northeast: humidity-RSV association\n(overall cumulative, reduced DLNM, adjusted for temperature)",
       subtitle = paste0("Centered on the regional median (", round(centro_umid_ne, 1), "%).")) +
  theme_minimal()

ggsave(file.path(DIR_GRAFICOS, "24_dlnm_umidade_nordeste.png"),
       fig_umid_ne, width = 10, height = 6.5, dpi = 300, units = "in", bg = "white")

cat("\n---- Nordeste: modelo bivariado (temp + umidade) vs. univariado (so temp) ----\n")
crit_uni_ne <- qaic(resultados_regionais[["Nordeste"]]$modelo)
crit_bi_ne  <- qaic(modelo_ne_bivariado)
cat(sprintf("Univariado (so temp): %.1f | Bivariado (temp+umidade): %.1f -> %s\n",
            crit_uni_ne, crit_bi_ne,
            ifelse(crit_bi_ne < crit_uni_ne, "umidade melhora o ajuste", "umidade NAO melhora o ajuste")))

#### 9. SALVAR OBJETOS PARA REUSO (figuras/tabelas do manuscrito) ############

saveRDS(list(df_var_comum = DF_VAR_COMUM,
             grade_df_qaic = grade_df,
             comparacao_familias = comparacao_familias,
             resultados_regionais = resultados_regionais,
             mixmeta = mv, resumo_mv = resumo_mv,
             pred_pooled = pred_pooled, pred_regionais = pred_regionais,
             tabela_rr_final = tabela_rr_final,
             nordeste_umidade = list(modelo_bivariado = modelo_ne_bivariado,
                                      pred_umidade = pred_umid_ne,
                                      criterio_uni = crit_uni_ne, criterio_bi = crit_bi_ne)),
        file.path(DIR_BASES, "objetos_dlnm_mixmeta.rds"))

cat("\nConcluido. Saidas: \n",
    " - ", file.path(DIR_GRAFICOS, "22_dlnm_mixmeta_pooled_blup.png"), "\n",
    " - ", file.path(DIR_GRAFICOS, "24_dlnm_umidade_nordeste.png"), "\n",
    " - ", file.path(DIR_TABELAS, "22_dlnm_mixmeta_rr_por_regiao.csv"), "\n",
    " - ", file.path(DIR_TABELAS, "23_dlnm_mixmeta_heterogeneidade.csv"), "\n",
    " - ", file.path(DIR_TABELAS, "25_dlnm_robustez_familia_qp_vs_nb.csv"), "\n",
    " - ", file.path(DIR_BASES, "objetos_dlnm_mixmeta.rds"), "\n", sep = "")
