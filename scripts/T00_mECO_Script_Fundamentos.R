# =============================================================================
# TEMA 0 — FUNDAMENTOS DE PROBABILIDAD, INFERENCIA Y ECONOMETRÍA: SCRIPT INTERACTIVO
# =============================================================================
# Autor: Carlos de Anta Puig
#        Economista · Perito Financiero
#        Miembro del Colegio de Economistas de Madrid
#        Miembro del Instituto Español de Analistas Financieros (IEAF)
#        Profesor de Econometría y Microeconometría
#        carlos@cwconsultores.com
#
# Manual de Microeconometría - Digital Reasons (2026)
# https://github.com/carlanta/MicroEconometrics
#
# Versión: 1.0
# Fecha:   2026-03-27
#
# Ejecutar bloque a bloque en RStudio.
# =============================================================================

# Autor: Carlos de Anta Puig
#        Economista · Perito Financiero
#        Miembro del Colegio de Economistas de Madrid
#        Miembro del Instituto Español de Analistas Financieros (IEAF)
#        Profesor de Econometría y Microeconometría
#        carlosmaria.deanta@unir.net
#
# Curso de Microeconometría
#
# Versión: 1.0
# Fecha:   2026-03-16
#
# Este script recorre paso a paso los fundamentos de probabilidad,
# inferencia y econometría necesarios para el curso.
# Ejecutar bloque a bloque en RStudio.
# =============================================================================

rm(list = ls())

pausa <- function() {
  readline("\n>>> Pulsa ENTER para continuar...")
}

cat("\014")

cat("============================================================\n")
cat("TEMA 0: FUNDAMENTOS PARA MICROECONOMETRIA\n")
cat("============================================================\n")
cat("Este script ejecuta paso a paso los conceptos fundamentales.\n")
cat("Cada bloque mostrara resultados e interpretacion.\n")
cat("============================================================\n")

pausa()

# =============================================================================
# CARGA DE LIBRERIAS
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(sandwich)
  library(lmtest)
  library(car)
  library(DiagrammeR)
  library(DT)
})

cat("\nLibrerias cargadas correctamente (incluye DT para tablas exportables).\n")

# Funcion auxiliar: tabla interactiva con botones de exportacion
tabla_dt <- function(datos, titulo = "", decimales = 2, filas = TRUE) {
  if (is.numeric(datos) && !is.null(names(datos))) {
    datos <- data.frame(Nombre = names(datos), Valor = round(datos, decimales))
    filas <- FALSE
  }
  DT::datatable(
    datos,
    caption = titulo,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel'),
      pageLength = 15,
      scrollX = TRUE
    ),
    rownames = filas
  )
}

pausa()

# =============================================================================
# BLOQUE 1: DISTRIBUCIONES DE PROBABILIDAD
# =============================================================================

cat("\014")
cat("============================================================\n")
cat("BLOQUE 1: DISTRIBUCIONES DE PROBABILIDAD\n")
cat("============================================================\n\n")

# --- Normal estandar ---
cat("--- Distribucion Normal estandar ---\n\n")

z <- seq(-4, 4, length.out = 500)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(z, dnorm(z), type = "l", lwd = 2, col = "steelblue",
     main = "Densidad phi(z)", xlab = "z", ylab = "phi(z)")
abline(h = 0, col = "grey70")
z_shade <- z[z <= -1.6]
polygon(c(z_shade, -1.6), c(dnorm(z_shade), 0),
        col = rgb(0.27, 0.51, 0.71, 0.3), border = NA)
text(-2.8, 0.08, "P(Z<-1.6)\n= 0.0548", cex = 0.8)

plot(z, pnorm(z), type = "l", lwd = 2, col = "darkred",
     main = "Acumulada Phi(z)", xlab = "z", ylab = "Phi(z)")
abline(h = c(0, 0.5, 1), col = "grey70", lty = 3)

cat("Propiedades esenciales:\n")
cat("  phi(-z) = phi(z)        (simetria de la densidad)\n")
cat("  Phi(-z) = 1 - Phi(z)    (simetria de la acumulada)\n")
cat("  Phi'(z) = phi(z)        (derivada de la acumulada = densidad)\n\n")

cat("Ejemplo: P(renta < 1200) con media=2000, sd=500\n")
prob <- pnorm(1200, mean = 2000, sd = 500)
cat("  P(X < 1200) =", round(prob, 4), "\n")
cat("  Interpretacion: el", round(prob * 100, 1),
    "% de los hogares tiene renta < 1200 EUR\n")

pausa()

# --- Normal vs Logistica ---
cat("\n--- Comparacion Normal vs Logistica ---\n\n")

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(z, dnorm(z), type = "l", lwd = 2, col = "steelblue",
     main = "Densidad: Normal vs Logistica",
     xlab = "z", ylab = "f(z)", ylim = c(0, 0.42))
lines(z, dlogis(z), lwd = 2, col = "darkorange")
legend("topright", c("Normal", "Logistica"),
       col = c("steelblue", "darkorange"), lwd = 2, cex = 0.9)

plot(z, pnorm(z), type = "l", lwd = 2, col = "steelblue",
     main = "CDF: Normal vs Logistica", xlab = "z", ylab = "F(z)")
lines(z, plogis(z), lwd = 2, col = "darkorange")
legend("bottomright", c("Phi(z)", "Lambda(z)"),
       col = c("steelblue", "darkorange"), lwd = 2, cex = 0.9)

cat("Observacion: la Logistica tiene colas mas pesadas.\n")
cat("En la practica, Logit y Probit dan resultados muy similares.\n")

pausa()

# --- Poisson ---
cat("\n--- Distribucion de Poisson ---\n\n")

par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))
x <- 0:20
plot(x, dpois(x, 1), type = "b", pch = 16, col = "steelblue",
     main = "Distribucion de Poisson", xlab = "k", ylab = "P(X=k)",
     ylim = c(0, 0.4))
lines(x, dpois(x, 3), type = "b", pch = 17, col = "darkorange")
lines(x, dpois(x, 7), type = "b", pch = 15, col = "darkred")
lines(x, dpois(x, 12), type = "b", pch = 18, col = "darkgreen")
legend("topright", c("lambda=1", "lambda=3", "lambda=7", "lambda=12"),
       col = c("steelblue", "darkorange", "darkred", "darkgreen"),
       pch = c(16, 17, 15, 18), lwd = 1, cex = 0.9)

cat("Propiedad clave: E[X] = Var(X) = lambda (equidispersion).\n")
cat("Si Var > Media => sobredispersion => usar Binomial Negativa.\n")

pausa()

# =============================================================================
# BLOQUE 2: SIMULACION MONTE CARLO
# =============================================================================

cat("\014")
cat("============================================================\n")
cat("BLOQUE 2: SIMULACION MONTE CARLO\n")
cat("============================================================\n\n")
cat("Demostramos insesgadez y consistencia de la media muestral.\n\n")

set.seed(2026)
mu_real <- 5
n_sim <- 5000
tamanos <- c(10, 30, 100, 500, 2000)

resultados <- data.frame()
for (n in tamanos) {
  medias <- replicate(n_sim, mean(rnorm(n, mean = mu_real, sd = 2)))
  resultados <- rbind(resultados,
    data.frame(n = paste0("n=", n), media = medias))
}
resultados$n <- factor(resultados$n,
  levels = paste0("n=", tamanos))

print(
  ggplot(resultados, aes(x = media)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50,
                   fill = "steelblue", alpha = 0.7) +
    geom_vline(xintercept = mu_real, colour = "red",
               linewidth = 1, linetype = "dashed") +
    facet_wrap(~n, scales = "free_y", ncol = 5) +
    labs(title = paste("Convergencia de la media muestral (mu =", mu_real, ")"),
         subtitle = paste(n_sim, "simulaciones por tamano muestral"),
         x = "Media muestral", y = "Densidad") +
    theme_minimal(base_size = 11)
)

cat("\nInterpretacion:\n")
cat("  - Linea roja = valor real mu =", mu_real, "\n")
cat("  - Distribucion centrada en mu para todos los n => INSESGADEZ\n")
cat("  - Se estrecha al aumentar n => CONSISTENCIA\n\n")

for (n in tamanos) {
  sub <- resultados$media[resultados$n == paste0("n=", n)]
  cat("  n =", sprintf("%5d", n),
      "| Media =", sprintf("%.4f", mean(sub)),
      "| Desv. tip. =", sprintf("%.4f", sd(sub)), "\n")
}

cat("\n[Abriendo tabla de simulacion en el Viewer...]\n")
mc_df <- data.frame(
  n = tamanos,
  Media = round(sapply(tamanos, function(n) mean(resultados$media[resultados$n == paste0("n=", n)])), 4),
  Desv_Tip = round(sapply(tamanos, function(n) sd(resultados$media[resultados$n == paste0("n=", n)])), 4),
  Sesgo = round(sapply(tamanos, function(n) mean(resultados$media[resultados$n == paste0("n=", n)])) - mu_real, 4)
)
print(tabla_dt(mc_df, "Simulacion Monte Carlo: convergencia (exportable)", filas = FALSE))

pausa()

# =============================================================================
# BLOQUE 3: MAXIMA VEROSIMILITUD MANUAL
# =============================================================================

cat("\014")
cat("============================================================\n")
cat("BLOQUE 3: MAXIMA VEROSIMILITUD MANUAL\n")
cat("============================================================\n\n")

set.seed(2026)
n <- 300
x1 <- rnorm(n, mean = 10, sd = 3)
x2 <- rnorm(n, mean = 5, sd = 2)
epsilon <- rnorm(n, mean = 0, sd = 2)

beta_real <- c(3, 0.5, -0.8)
sigma_real <- 2
y <- beta_real[1] + beta_real[2] * x1 + beta_real[3] * x2 + epsilon
datos <- data.frame(y = y, x1 = x1, x2 = x2)

cat("Datos generados:\n")
cat("  y = 3 + 0.5*x1 - 0.8*x2 + epsilon\n")
cat("  epsilon ~ N(0, 4)\n")
cat("  n =", n, "\n\n")

# Log-verosimilitud negativa
neg_loglik <- function(params, y, X) {
  beta  <- params[1:ncol(X)]
  sigma <- params[ncol(X) + 1]
  if (sigma <= 0) return(1e10)
  n <- length(y)
  res <- y - X %*% beta
  ll <- -n/2 * log(2 * pi) - n * log(sigma) - sum(res^2) / (2 * sigma^2)
  return(-ll)
}

X <- cbind(1, x1, x2)
inicio <- c(0, 0, 0, 1)
resultado <- optim(par = inicio, fn = neg_loglik,
                   y = y, X = X, method = "BFGS", hessian = TRUE)

beta_mv  <- resultado$par[1:3]
sigma_mv <- resultado$par[4]

cat("=== ESTIMACION MV (manual) ===\n\n")
cat("  beta0:", sprintf("%8.4f", beta_mv[1]), " (real:", beta_real[1], ")\n")
cat("  beta1:", sprintf("%8.4f", beta_mv[2]), " (real:", beta_real[2], ")\n")
cat("  beta2:", sprintf("%8.4f", beta_mv[3]), " (real:", beta_real[3], ")\n")
cat("  sigma:", sprintf("%8.4f", sigma_mv),   " (real:", sigma_real, ")\n")

pausa()

# Comparar con MCO
modelo_mco <- lm(y ~ x1 + x2, data = datos)

cat("\n=== COMPARACION MV vs MCO ===\n\n")
cat(sprintf("  %-10s %10s %10s %10s\n", "Param", "Real", "MV", "MCO"))
cat(sprintf("  %-10s %10.4f %10.4f %10.4f\n", "beta0",
            beta_real[1], beta_mv[1], coef(modelo_mco)[1]))
cat(sprintf("  %-10s %10.4f %10.4f %10.4f\n", "beta1",
            beta_real[2], beta_mv[2], coef(modelo_mco)[2]))
cat(sprintf("  %-10s %10.4f %10.4f %10.4f\n", "beta2",
            beta_real[3], beta_mv[3], coef(modelo_mco)[3]))
cat(sprintf("  %-10s %10.4f %10.4f %10.4f\n", "sigma",
            sigma_real, sigma_mv, summary(modelo_mco)$sigma))

cat("\n[Abriendo tabla comparativa MV vs MCO en el Viewer...]\n")
mv_mco_df <- data.frame(
  Parametro = c("beta0", "beta1", "beta2", "sigma"),
  Real = c(beta_real, sigma_real),
  MV = round(c(beta_mv, sigma_mv), 4),
  MCO = round(c(coef(modelo_mco), summary(modelo_mco)$sigma), 4)
)
print(tabla_dt(mv_mco_df, "Comparacion MV vs MCO (exportable)", filas = FALSE))

cat("\nInterpretacion:\n")
cat("  MV y MCO coinciden (bajo normalidad, son equivalentes).\n")
cat("  La unica diferencia es en sigma: MCO corrige por grados de libertad.\n")

pausa()

# =============================================================================
# BLOQUE 4: MCO — ESTIMACION Y DIAGNOSTICO
# =============================================================================

cat("\014")
cat("============================================================\n")
cat("BLOQUE 4: MCO — ESTIMACION E INFERENCIA\n")
cat("============================================================\n\n")

cat("=== RESUMEN DEL MODELO ===\n\n")
print(summary(modelo_mco))

pausa()

# =============================================================================
# BLOQUE 5: ERRORES ROBUSTOS
# =============================================================================

cat("\014")
cat("============================================================\n")
cat("BLOQUE 5: ERRORES ROBUSTOS\n")
cat("============================================================\n\n")

cat("=== ERRORES ESTANDAR CLASICOS ===\n\n")
print(coeftest(modelo_mco))

cat("\n=== ERRORES ESTANDAR ROBUSTOS (HC1 = White) ===\n\n")
print(coeftest(modelo_mco, vcov = vcovHC(modelo_mco, type = "HC1")))

se_clas <- sqrt(diag(vcov(modelo_mco)))
se_rob  <- sqrt(diag(vcovHC(modelo_mco, type = "HC1")))

cat("\nComparacion:\n")
cat(sprintf("  %-15s %10s %10s %10s\n", "Variable", "SE clasico", "SE robusto", "Ratio"))
for (i in seq_along(se_clas)) {
  cat(sprintf("  %-15s %10.4f %10.4f %10.3f\n",
              names(se_clas)[i], se_clas[i], se_rob[i], se_rob[i] / se_clas[i]))
}

cat("\n[Abriendo tabla de errores estandar en el Viewer...]\n")
se_df <- data.frame(
  Variable = names(se_clas),
  SE_Clasico = round(se_clas, 4),
  SE_Robusto = round(se_rob, 4),
  Ratio = round(se_rob / se_clas, 3)
)
print(tabla_dt(se_df, "Errores estandar: clasicos vs robustos (exportable)", filas = FALSE))

cat("\nInterpretacion:\n")
cat("  Ratio cercano a 1 => no hay indicios graves de heteroscedasticidad.\n")
cat("  Ratio muy distinto de 1 => errores clasicos NO son fiables.\n")

pausa()

# =============================================================================
# BLOQUE 6: CONTRASTES DE HIPOTESIS (Wald, LR, LM)
# =============================================================================

cat("\014")
cat("============================================================\n")
cat("BLOQUE 6: CONTRASTES DE HIPOTESIS\n")
cat("============================================================\n\n")
cat("H0: beta2 = 0 (x2 no es significativa)\n\n")

mod_sr <- lm(y ~ x1 + x2, data = datos)
mod_r  <- lm(y ~ x1, data = datos)

# --- Wald ---
cat("--- TEST DE WALD ---\n\n")
wald <- waldtest(mod_sr, mod_r)
print(wald)
p_wald <- wald$`Pr(>F)`[2]

if (p_wald < 0.05) {
  message("\n=> Rechazamos H0: x2 ES significativa (p=", round(p_wald, 4), ")")
} else {
  message("\n=> No rechazamos H0 (p=", round(p_wald, 4), ")")
}

pausa()

# --- LR ---
cat("\n--- TEST DE RAZON DE VEROSIMILITUDES (LR) ---\n\n")
ll_sr <- logLik(mod_sr)
ll_r  <- logLik(mod_r)
LR_stat <- as.numeric(2 * (ll_sr - ll_r))
p_lr <- 1 - pchisq(LR_stat, df = 1)
cat("Log-lik sin restricciones:", round(as.numeric(ll_sr), 2), "\n")
cat("Log-lik restringida:     ", round(as.numeric(ll_r), 2), "\n")
cat("LR = 2[l(SR) - l(R)] =", round(LR_stat, 4), "\n")
cat("p-valor =", round(p_lr, 4), "\n")

if (p_lr < 0.05) {
  message("\n=> Rechazamos H0: x2 ES significativa (p=", round(p_lr, 4), ")")
} else {
  message("\n=> No rechazamos H0 (p=", round(p_lr, 4), ")")
}

pausa()

# --- LM ---
cat("\n--- TEST DEL MULTIPLICADOR DE LAGRANGE (LM) ---\n\n")
residuos_r <- residuals(mod_r)
aux <- lm(residuos_r ~ x1 + x2, data = datos)
r2_aux <- summary(aux)$r.squared
LM_stat <- nrow(datos) * r2_aux
p_lm <- 1 - pchisq(LM_stat, df = 1)

cat("R2 auxiliar:", round(r2_aux, 6), "\n")
cat("LM = n * R2 =", round(LM_stat, 4), "\n")
cat("p-valor =", round(p_lm, 4), "\n")

if (p_lm < 0.05) {
  message("\n=> Rechazamos H0: x2 ES significativa (p=", round(p_lm, 4), ")")
} else {
  message("\n=> No rechazamos H0 (p=", round(p_lm, 4), ")")
}

pausa()

# Comparacion
cat("\n=== COMPARACION DE LOS TRES CONTRASTES ===\n\n")
cat(sprintf("  %-15s %12s %12s %15s\n", "Test", "Estadistico", "p-valor", "Decision"))
cat(sprintf("  %-15s %12.4f %12.4f %15s\n", "Wald (F)",
            wald$F[2], p_wald, ifelse(p_wald < 0.05, "Rechaza H0", "No rechaza")))
cat(sprintf("  %-15s %12.4f %12.4f %15s\n", "LR (chi2)",
            LR_stat, p_lr, ifelse(p_lr < 0.05, "Rechaza H0", "No rechaza")))
cat(sprintf("  %-15s %12.4f %12.4f %15s\n", "LM (chi2)",
            LM_stat, p_lm, ifelse(p_lm < 0.05, "Rechaza H0", "No rechaza")))

cat("\n[Abriendo tabla comparativa de tests en el Viewer...]\n")
tests_df <- data.frame(
  Test = c("Wald (F)", "LR (chi2)", "LM (chi2)"),
  Estadistico = round(c(wald$F[2], LR_stat, LM_stat), 4),
  p_valor = round(c(p_wald, p_lr, p_lm), 4),
  Decision = c(
    ifelse(p_wald < 0.05, "Rechaza H0", "No rechaza"),
    ifelse(p_lr < 0.05, "Rechaza H0", "No rechaza"),
    ifelse(p_lm < 0.05, "Rechaza H0", "No rechaza"))
)
print(tabla_dt(tests_df, "Comparacion de contrastes: Wald, LR, LM (exportable)", filas = FALSE))

cat("\nInterpretacion:\n")
cat("  Los tres contrastes son asintoticamente equivalentes.\n")
cat("  En muestras finitas pueden diferir ligeramente.\n")

pausa()

# =============================================================================
# BLOQUE 7: DIAGNOSTICO DEL MODELO
# =============================================================================

cat("\014")
cat("============================================================\n")
cat("BLOQUE 7: DIAGNOSTICO COMPLETO DEL MODELO\n")
cat("============================================================\n\n")

# --- Graficos de residuos ---
cat("--- PASO 1: Analisis grafico de residuos ---\n\n")

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(fitted(mod_sr), residuals(mod_sr), pch = 16,
     col = rgb(0.3, 0.3, 0.3, 0.5),
     xlab = "Valores ajustados", ylab = "Residuos",
     main = "Residuos vs Ajustados")
abline(h = 0, col = "red", lwd = 2)
lines(lowess(fitted(mod_sr), residuals(mod_sr)), col = "steelblue", lwd = 2)

qqnorm(residuals(mod_sr), pch = 16, col = rgb(0.3, 0.3, 0.3, 0.5),
       main = "QQ-Plot de los residuos")
qqline(residuals(mod_sr), col = "red", lwd = 2)

plot(datos$x1, residuals(mod_sr), pch = 16,
     col = rgb(0.3, 0.3, 0.3, 0.5),
     xlab = "x1", ylab = "Residuos", main = "Residuos vs x1")
abline(h = 0, col = "red", lwd = 2)

plot(datos$x2, residuals(mod_sr), pch = 16,
     col = rgb(0.3, 0.3, 0.3, 0.5),
     xlab = "x2", ylab = "Residuos", main = "Residuos vs x2")
abline(h = 0, col = "red", lwd = 2)

cat("Que buscamos:\n")
cat("  - Residuos vs Ajustados: sin patron (embudo = heterosced.)\n")
cat("  - QQ-plot: puntos sobre la linea roja (normalidad)\n")
cat("  - Residuos vs X: sin curvas (linealidad)\n")

pausa()

# --- RESET ---
cat("\n--- PASO 2: Test RESET (forma funcional) ---\n\n")
reset <- resettest(mod_sr, power = 2:3, type = "fitted")
print(reset)

if (reset$p.value < 0.05) {
  message("=> PROBLEMA: forma funcional inadecuada.")
} else {
  message("=> OK: no hay evidencia de mala especificacion funcional.")
}

pausa()

# --- VIF ---
cat("\n--- PASO 3: Multicolinealidad (VIF) ---\n\n")
vif_val <- vif(mod_sr)
for (i in seq_along(vif_val)) {
  etiqueta <- ifelse(vif_val[i] > 10, "GRAVE",
              ifelse(vif_val[i] > 5, "CUIDADO", "OK"))
  cat("  ", names(vif_val)[i], ": VIF =",
      round(vif_val[i], 2), "->", etiqueta, "\n")
}

pausa()

# --- Breusch-Pagan ---
cat("\n--- PASO 4: Homocedasticidad (Breusch-Pagan) ---\n\n")
bp <- bptest(mod_sr)
print(bp)

if (bp$p.value < 0.05) {
  message("=> PROBLEMA: heteroscedasticidad detectada. Usar errores robustos.")
} else {
  message("=> OK: no hay evidencia de heteroscedasticidad.")
}

pausa()

# --- Normalidad ---
cat("\n--- PASO 5: Normalidad de los residuos (Shapiro-Wilk) ---\n\n")
sw <- shapiro.test(residuals(mod_sr))
print(sw)

if (sw$p.value < 0.05) {
  message("=> Los residuos NO son normales.")
  message("   Nota: con muestras grandes, esto es menos preocupante.")
} else {
  message("=> OK: no hay evidencia contra la normalidad.")
}

pausa()

# --- Bondad de ajuste ---
cat("\n--- PASO 6: Bondad de ajuste ---\n\n")
cat("  R2         =", round(summary(mod_sr)$r.squared, 4), "\n")
cat("  R2 ajust.  =", round(summary(mod_sr)$adj.r.squared, 4), "\n")
cat("  AIC        =", round(AIC(mod_sr), 2), "\n")
cat("  BIC        =", round(BIC(mod_sr), 2), "\n")
cat("  Log-lik    =", round(as.numeric(logLik(mod_sr)), 2), "\n")

pausa()

# --- Tabla resumen ---
cat("\n=== TABLA RESUMEN DE DIAGNOSTICO ===\n\n")
cat(sprintf("  %-35s %10s %10s %10s\n",
            "Test", "Estadist.", "p-valor", "Resultado"))
cat(sprintf("  %-35s %10.3f %10.4f %10s\n",
            "Forma funcional (RESET)",
            reset$statistic, reset$p.value,
            ifelse(reset$p.value < 0.05, "PROBLEMA", "OK")))
cat(sprintf("  %-35s %10.2f %10s %10s\n",
            "Multicolinealidad (VIF max.)",
            max(vif_val), "---",
            ifelse(max(vif_val) > 10, "GRAVE",
                   ifelse(max(vif_val) > 5, "CUIDADO", "OK"))))
cat(sprintf("  %-35s %10.3f %10.4f %10s\n",
            "Homocedasticidad (Breusch-Pagan)",
            bp$statistic, bp$p.value,
            ifelse(bp$p.value < 0.05, "PROBLEMA", "OK")))
cat(sprintf("  %-35s %10.3f %10.4f %10s\n",
            "Normalidad (Shapiro-Wilk)",
            sw$statistic, sw$p.value,
            ifelse(sw$p.value < 0.05, "PROBLEMA", "OK")))

cat("\n[Abriendo tabla de diagnostico en el Viewer...]\n")
diag_df <- data.frame(
  Test = c("RESET (forma func.)", "VIF max. (multicol.)",
           "Breusch-Pagan (heterosc.)", "Shapiro-Wilk (normalidad)"),
  Estadistico = round(c(reset$statistic, max(vif_val), bp$statistic, sw$statistic), 3),
  p_valor = c(round(reset$p.value, 4), NA, round(bp$p.value, 4), round(sw$p.value, 4)),
  Resultado = c(
    ifelse(reset$p.value < 0.05, "PROBLEMA", "OK"),
    ifelse(max(vif_val) > 10, "GRAVE", ifelse(max(vif_val) > 5, "CUIDADO", "OK")),
    ifelse(bp$p.value < 0.05, "PROBLEMA", "OK"),
    ifelse(sw$p.value < 0.05, "PROBLEMA", "OK"))
)
print(tabla_dt(diag_df, "Bateria de diagnostico MCO (exportable)", filas = FALSE))

pausa()

# --- Diagrama de flujo ---
cat("\n--- DIAGRAMA DE FLUJO DIAGNOSTICO ---\n\n")
cat("Generando diagrama...\n")

DiagrammeR::grViz("
digraph diagnostico_general {
  graph [rankdir=TB, fontname='Helvetica', bgcolor='white', nodesep=0.6, ranksep=0.7]
  node [shape=box, style='filled,rounded', fontname='Helvetica', fontsize=11, margin='0.2,0.1']
  edge [fontname='Helvetica', fontsize=9]

  est  [label='1. Estimar el modelo', fillcolor='#4682B4', fontcolor='white']
  res  [label='2. Analizar residuos\n(graficos)', fillcolor='#B0C4DE']
  ff   [label='3. Forma funcional\n(test RESET)', fillcolor='#B0C4DE']
  mc   [label='4. Multicolinealidad\n(VIF)', fillcolor='#B0C4DE']
  het  [label='5. Homocedasticidad\n(Breusch-Pagan)', fillcolor='#B0C4DE']
  nor  [label='6. Normalidad residuos\n(Shapiro-Wilk / JB)', fillcolor='#B0C4DE']
  sig  [label='7. Significatividad\n(Wald, LR, LM)', fillcolor='#B0C4DE']
  ba   [label='8. Bondad de ajuste\n(AIC, BIC, logLik)', fillcolor='#B0C4DE']

  ok   [label='MODELO VALIDADO', fillcolor='#2E8B57', fontcolor='white', shape=ellipse]
  fix  [label='RESPECIFICAR', fillcolor='#CD5C5C', fontcolor='white', shape=ellipse]

  est -> res -> ff -> mc -> het -> nor -> sig -> ba
  ba  -> ok  [label='Todo correcto']
  res -> fix [label='Patron detectado', style=dashed, color='#CD5C5C']
  ff  -> fix [label='Rechaza H0', style=dashed, color='#CD5C5C']
  mc  -> fix [label='VIF > 10', style=dashed, color='#CD5C5C']
  het -> fix [label='Rechaza H0', style=dashed, color='#CD5C5C']
  fix -> est [label='Corregir y reestimar', style=dashed, color='#CD5C5C']
}
")

pausa()

# =============================================================================
# BLOQUE 8: MCO VS DATOS LIMITADOS
# =============================================================================

cat("\014")
cat("============================================================\n")
cat("BLOQUE 8: POR QUE MCO FALLA CON VD LIMITADA\n")
cat("============================================================\n\n")

# --- Caso 1: Binaria ---
cat("--- CASO 1: Variable dependiente BINARIA ---\n\n")

set.seed(2026)
n <- 500
x <- rnorm(n, 0, 1.5)
prob_real <- plogis(0.3 + 1.5 * x)
y_bin <- rbinom(n, 1, prob_real)
mod_lin <- lm(y_bin ~ x)

par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))
plot(x, y_bin, pch = 16, col = rgb(0.3, 0.3, 0.3, 0.3),
     xlab = "x", ylab = "y / P(y=1|x)",
     main = "MCO vs Logit: variable dependiente binaria",
     ylim = c(-0.3, 1.3))
abline(mod_lin, col = "red", lwd = 2)
x_seq <- seq(min(x), max(x), length.out = 300)
lines(x_seq, plogis(0.3 + 1.5 * x_seq), col = "steelblue", lwd = 2)
abline(h = c(0, 1), lty = 3, col = "grey50")
legend("right", c("MCO (lineal)", "Logit (correcto)"),
       col = c("red", "steelblue"), lwd = 2, cex = 0.9)

cat("PROBLEMA: MCO predice probabilidades fuera de [0, 1].\n")
cat("SOLUCION: Logit o Probit (Temas 1-2).\n")

pausa()

# --- Caso 2: Censurada ---
cat("\n--- CASO 2: Variable dependiente CENSURADA ---\n\n")

set.seed(2026)
x_c <- rnorm(n, 5, 2)
y_lat <- -3 + 1.2 * x_c + rnorm(n, 0, 2)
y_cens <- pmax(y_lat, 0)
mod_cens <- lm(y_cens ~ x_c)

par(mar = c(4, 4, 3, 1))
plot(x_c, y_cens, pch = 16,
     col = ifelse(y_cens == 0, "red", rgb(0.3, 0.3, 0.3, 0.4)),
     xlab = "x", ylab = "y",
     main = "MCO con datos censurados: sesgo de atenuacion")
abline(mod_cens, col = "red", lwd = 2, lty = 2)
abline(a = -3, b = 1.2, col = "steelblue", lwd = 2)
abline(h = 0, col = "grey50", lty = 3)
legend("topleft",
       c("MCO (sesgado)", "Relacion real (y*)", "Censuradas (y=0)"),
       col = c("red", "steelblue", "red"),
       lty = c(2, 1, NA), pch = c(NA, NA, 16), lwd = 2, cex = 0.9)

cat("Pendiente real:  beta1 = 1.200\n")
cat("Pendiente MCO:   beta1 =", round(coef(mod_cens)[2], 3), "\n")
cat("Sesgo:          ", round(coef(mod_cens)[2] - 1.2, 3), "\n\n")
cat("PROBLEMA: MCO SUBESTIMA el efecto (sesgo de atenuacion).\n")
cat("SOLUCION: Modelo Tobit (Tema 3).\n")

pausa()

# --- Caso 3: Recuento ---
cat("\n--- CASO 3: Variable dependiente de RECUENTO ---\n\n")

set.seed(2026)
x_r <- rnorm(n, 2, 1)
lambda <- exp(0.5 + 0.4 * x_r)
y_count <- rpois(n, lambda)
mod_count <- lm(y_count ~ x_r)

par(mar = c(4, 4, 3, 1))
plot(x_r, y_count, pch = 16, col = rgb(0.3, 0.3, 0.3, 0.3),
     xlab = "x", ylab = "y (recuento)",
     main = "MCO vs Poisson: variable de recuento")
abline(mod_count, col = "red", lwd = 2)
x_seq <- seq(min(x_r), max(x_r), length.out = 300)
lines(x_seq, exp(0.5 + 0.4 * x_seq), col = "steelblue", lwd = 2)
abline(h = 0, lty = 3, col = "grey50")
legend("topleft", c("MCO (lineal)", "Poisson (correcto)"),
       col = c("red", "steelblue"), lwd = 2, cex = 0.9)

cat("PROBLEMA: MCO asume relacion lineal y puede predecir negativos.\n")
cat("SOLUCION: Modelo Poisson / Binomial Negativa (Tema 4).\n")

pausa()

# --- Resumen visual ---
cat("\n--- RESUMEN: ARBOL DE DECISION ---\n\n")

DiagrammeR::grViz("
digraph arbol {
  graph [rankdir=TB, fontname='Helvetica', bgcolor='white', nodesep=0.6, ranksep=0.7]
  node [shape=box, style='filled,rounded', fontname='Helvetica', fontsize=11, margin='0.2,0.1']
  edge [fontname='Helvetica', fontsize=9]

  inicio [label='Tipo de variable\ndependiente?', fillcolor='#4682B4', fontcolor='white', shape=diamond]
  cont   [label='Continua\nsin restricciones', fillcolor='#B0C4DE']
  bin    [label='Binaria\n(0/1)', fillcolor='#B0C4DE']
  cens   [label='Censurada\n(masa en un limite)', fillcolor='#B0C4DE']
  rec    [label='Recuento\n(0, 1, 2, ...)', fillcolor='#B0C4DE']

  mco    [label='MCO\n(Tema 0)', fillcolor='#2E8B57', fontcolor='white']
  logpro [label='Logit / Probit (MV)\n(Temas 1-2)', fillcolor='#DAA520', fontcolor='white']
  tobit  [label='Tobit (MV)\n(Tema 3)', fillcolor='#DAA520', fontcolor='white']
  pois   [label='Poisson / BN (MV)\n(Tema 4)', fillcolor='#DAA520', fontcolor='white']

  inicio -> cont
  inicio -> bin
  inicio -> cens
  inicio -> rec
  cont -> mco
  bin  -> logpro
  cens -> tobit
  rec  -> pois
}
")

cat("\n")
cat("============================================================\n")
cat("FIN DEL SCRIPT — TEMA 0\n")
cat("============================================================\n")
cat("Has completado el repaso de fundamentos.\n")
cat("Estas preparado para comenzar el curso de microeconometria.\n")
cat("============================================================\n")
