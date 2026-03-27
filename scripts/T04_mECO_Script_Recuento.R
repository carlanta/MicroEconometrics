# =============================================================================
# TEMA 4 — MODELOS PARA DATOS DE RECUENTO: SCRIPT INTERACTIVO
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

#
# Autor:    Carlos de Anta Puig
#           Economista · Perito Financiero
#           Miembro del Colegio de Economistas de Madrid
#           Miembro del Instituto Español de Analistas Financieros (IEAF)
#           Profesor de Econometría y Microeconometría
# Email:    carlosmaria.deanta@unir.net
# Version:  1.0 | Fecha: Marzo 2026
#
# Descripcion: Estimacion, comparacion y diagnostico de modelos Poisson
#              y Binomial Negativo con datos simulados de patentes.
# ============================================================================

rm(list = ls())
gc()

# ============================================================================
# CARGA DE LIBRERIAS
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(MASS)
  library(DT)
})

# Funcion auxiliar para tablas interactivas (DT)
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

# ============================================================================
# SECCION 1: CREACION Y EXPLORACION DEL DATASET
# ============================================================================

cat("\n")
cat("========================================================\n")
cat("  TEMA 4: MODELOS POISSON Y BINOMIAL NEGATIVO\n")
cat("  Analisis de Patentes Empresariales\n")
cat("========================================================\n")

set.seed(42)
n <- 200

# Generar datos sinteticos: patentes, gasto en I+D, empleados, region
datos <- data.frame(
  id        = 1:n,
  gasto_id  = rnorm(n, mean = 50, sd = 25),
  empleados = round(rlnorm(n, meanlog = log(120), sdlog = 0.8)),
  region    = rep(c("Madrid", "Barcelona", "Valencia", "Bilbao"), length.out = n)
)

# Variable dependiente con sobredispersion intencionada
lambda <- with(datos, exp(-1.5 + 0.015 * gasto_id + 0.008 * log(empleados)))
datos$patentes <- rpois(n, lambda * runif(n, 0.5, 2.5))

cat("\n--- ESTADISTICA DESCRIPTIVA ---\n")
cat("Dimensiones:", nrow(datos), "obs. x", ncol(datos), "variables\n")
cat("Media de patentes:   ", round(mean(datos$patentes), 3), "\n")
cat("Varianza de patentes:", round(var(datos$patentes), 3), "\n")
cat("Ratio Var/Media:     ", round(var(datos$patentes) / mean(datos$patentes), 3), "\n")

cat("\n[Abriendo tabla en el Viewer...]\n")
tabla_dt(head(datos, 20), titulo = "Primeras 20 observaciones del dataset")

readline(prompt = "\nPulsa ENTER para continuar con la exploracion grafica...")

# ============================================================================
# SECCION 2: EXPLORACION GRAFICA
# ============================================================================

cat("\n--- EXPLORACION GRAFICA ---\n")

# Histograma de la variable dependiente
hist(datos$patentes, breaks = max(datos$patentes),
     col = "steelblue", border = "white",
     main = "Distribucion de patentes (variable dependiente)",
     xlab = "Numero de patentes", ylab = "Frecuencia")

readline(prompt = "\nPulsa ENTER para continuar con el modelo Poisson...")

# ============================================================================
# SECCION 3: MODELO POISSON
# ============================================================================

cat("\n--- MODELO POISSON ---\n")

modelo_poisson <- glm(
  patentes ~ gasto_id + log(empleados) + region,
  family = poisson(link = "log"),
  data = datos
)

cat("\nResultados del modelo Poisson:\n")
print(summary(modelo_poisson))

# Coeficientes exponenciados
coef_p   <- coef(modelo_poisson)
se_p     <- summary(modelo_poisson)$coefficients[, "Std. Error"]
pval_p   <- summary(modelo_poisson)$coefficients[, "Pr(>|z|)"]

tabla_coef_poisson <- data.frame(
  Variable    = names(coef_p),
  Coeficiente = round(coef_p, 4),
  IRR         = round(exp(coef_p), 4),
  SE          = round(se_p, 4),
  p_valor     = round(pval_p, 4),
  Cambio_Pct  = paste0(round((exp(coef_p) - 1) * 100, 2), "%"),
  row.names   = NULL
)

cat("\n[Abriendo tabla de coeficientes Poisson en el Viewer...]\n")
tabla_dt(tabla_coef_poisson, titulo = "Coeficientes del modelo Poisson (con IRR)")

readline(prompt = "\nPulsa ENTER para continuar con la prueba de sobredispersion...")

# ============================================================================
# SECCION 4: PRUEBA DE SOBREDISPERSION
# ============================================================================

cat("\n--- PRUEBA DE SOBREDISPERSION ---\n")

residuos_pearson <- residuals(modelo_poisson, type = "pearson")
X2_pearson <- sum(residuos_pearson^2)
gl <- modelo_poisson$df.residual
phi_est <- X2_pearson / gl

cat("Chi-cuadrado de Pearson:", round(X2_pearson, 3), "\n")
cat("Grados de libertad:    ", gl, "\n")
cat("Estimador de dispersion (phi):", round(phi_est, 3), "\n")

if (phi_est > 1.5) {
  message("\n>>> SOBREDISPERSION SEVERA (phi > 1.5)")
  message("    El modelo Poisson NO es adecuado. Usar Binomial Negativa.")
} else if (phi_est > 1.1) {
  message("\n>>> SOBREDISPERSION MODERADA (phi > 1.1)")
  message("    Considerar Binomial Negativa.")
} else {
  message("\n>>> EQUIDISPERSION (phi aprox. 1)")
  message("    El modelo Poisson es adecuado.")
}

readline(prompt = "\nPulsa ENTER para continuar con el modelo Binomial Negativo...")

# ============================================================================
# SECCION 5: MODELO BINOMIAL NEGATIVO
# ============================================================================

cat("\n--- MODELO BINOMIAL NEGATIVO ---\n")

modelo_bn <- glm.nb(
  patentes ~ gasto_id + log(empleados) + region,
  data = datos
)

cat("\nResultados del modelo Binomial Negativo:\n")
print(summary(modelo_bn))
cat("Parametro theta:", round(modelo_bn$theta, 3), "\n")
cat("Alpha (1/theta):", round(1 / modelo_bn$theta, 3), "\n")

# Tabla de coeficientes NB
coef_bn  <- coef(modelo_bn)
se_bn    <- summary(modelo_bn)$coefficients[, "Std. Error"]
pval_bn  <- summary(modelo_bn)$coefficients[, "Pr(>|z|)"]

tabla_coef_bn <- data.frame(
  Variable    = names(coef_bn),
  Coeficiente = round(coef_bn, 4),
  IRR         = round(exp(coef_bn), 4),
  SE          = round(se_bn, 4),
  p_valor     = round(pval_bn, 4),
  Cambio_Pct  = paste0(round((exp(coef_bn) - 1) * 100, 2), "%"),
  row.names   = NULL
)

cat("\n[Abriendo tabla de coeficientes NB en el Viewer...]\n")
tabla_dt(tabla_coef_bn, titulo = "Coeficientes del modelo Binomial Negativo (con IRR)")

readline(prompt = "\nPulsa ENTER para continuar con la comparacion de modelos...")

# ============================================================================
# SECCION 6: COMPARACION DE MODELOS
# ============================================================================

cat("\n--- COMPARACION DE MODELOS ---\n")

ll_p  <- as.numeric(logLik(modelo_poisson))
ll_bn <- as.numeric(logLik(modelo_bn))
aic_p <- AIC(modelo_poisson)
aic_bn <- AIC(modelo_bn)
bic_p <- BIC(modelo_poisson)
bic_bn <- BIC(modelo_bn)

# Test de razon de verosimilitud (manual)
lr_stat <- 2 * (ll_bn - ll_p)
p_valor_lr <- pchisq(lr_stat, df = 1, lower.tail = FALSE)

cat("\nTest de Razon de Verosimilitud (NB vs Poisson):\n")
cat("  Log-verosimilitud Poisson:", round(ll_p, 3), "\n")
cat("  Log-verosimilitud NB:     ", round(ll_bn, 3), "\n")
cat("  Estadistico LR:           ", round(lr_stat, 4), "\n")
cat("  P-valor:                  ", round(p_valor_lr, 6), "\n")

if (p_valor_lr < 0.05) {
  message("  >>> Rechazamos Poisson: BN es significativamente mejor")
} else {
  message("  >>> No rechazamos Poisson")
}

# Tabla comparativa
comp_modelos <- data.frame(
  Modelo     = c("Poisson", "Binomial Negativo"),
  LogLik     = round(c(ll_p, ll_bn), 3),
  AIC        = round(c(aic_p, aic_bn), 3),
  BIC        = round(c(bic_p, bic_bn), 3),
  Deviance   = round(c(deviance(modelo_poisson), deviance(modelo_bn)), 3),
  Dev_gl     = round(c(deviance(modelo_poisson) / modelo_poisson$df.residual,
                        deviance(modelo_bn) / modelo_bn$df.residual), 3)
)

cat("\n[Abriendo tabla comparativa en el Viewer...]\n")
tabla_dt(comp_modelos, titulo = "Comparacion de modelos: Poisson vs Binomial Negativo",
         filas = FALSE)

readline(prompt = "\nPulsa ENTER para continuar con la comparacion de coeficientes...")

# Comparacion de coeficientes lado a lado
comp_coefs <- data.frame(
  Variable      = names(coef_p),
  Coef_Poisson  = round(coef_p, 4),
  Coef_NB       = round(coef_bn, 4),
  IRR_Poisson   = round(exp(coef_p), 4),
  IRR_NB        = round(exp(coef_bn), 4),
  Dif_Pct       = paste0(round((coef_bn - coef_p) / abs(coef_p) * 100, 1), "%"),
  row.names     = NULL
)

cat("\n[Abriendo tabla de coeficientes comparados en el Viewer...]\n")
tabla_dt(comp_coefs, titulo = "Comparacion de coeficientes: Poisson vs NB")

readline(prompt = "\nPulsa ENTER para continuar con las predicciones...")

# ============================================================================
# SECCION 7: PREDICCIONES
# ============================================================================

cat("\n--- PREDICCIONES ---\n")

nuevas_empresas <- expand.grid(
  gasto_id  = seq(30, 70, by = 20),
  empleados = c(100, 250, 500),
  region    = "Madrid"
)

pred_poisson <- predict(modelo_poisson, newdata = nuevas_empresas, type = "response")
pred_bn      <- predict(modelo_bn, newdata = nuevas_empresas, type = "response")

predicciones <- cbind(
  nuevas_empresas,
  Pred_Poisson = round(pred_poisson, 2),
  Pred_BN      = round(pred_bn, 2),
  Diferencia   = round(abs(pred_poisson - pred_bn), 2)
)

cat("\n[Abriendo tabla de predicciones en el Viewer...]\n")
tabla_dt(predicciones, titulo = "Predicciones de patentes (empresas de Madrid)",
         filas = FALSE)

readline(prompt = "\nPulsa ENTER para continuar con los diagnosticos...")

# ============================================================================
# SECCION 8: DIAGNOSTICOS
# ============================================================================

cat("\n--- DIAGNOSTICOS DEL MODELO SELECCIONADO ---\n")

# Seleccionar modelo por AIC
modelo_sel <- if (aic_bn < aic_p) modelo_bn else modelo_poisson
nombre_sel <- if (aic_bn < aic_p) "Binomial Negativo" else "Poisson"
cat("Modelo seleccionado por AIC:", nombre_sel, "\n")

# Residuos
res_pearson  <- residuals(modelo_sel, type = "pearson")
res_deviance <- residuals(modelo_sel, type = "deviance")
ajustados    <- fitted(modelo_sel)

par(mfrow = c(2, 2))

plot(ajustados, res_pearson, pch = 16, col = "steelblue", cex = 0.6,
     xlab = "Valores ajustados", ylab = "Residuos de Pearson",
     main = "1. Ajustados vs Residuos")
abline(h = 0, col = "red", lty = 2)

qqnorm(res_deviance, pch = 16, col = "steelblue", cex = 0.6,
       main = "2. QQ-plot Residuos Deviance")
qqline(res_deviance, col = "red")

plot(datos$patentes, ajustados, pch = 16, col = "steelblue", cex = 0.6,
     xlab = "Patentes (real)", ylab = "Patentes (ajustado)",
     main = "3. Real vs Ajustado")
abline(a = 0, b = 1, col = "red", lty = 2)

hist(res_pearson, breaks = 30, col = "steelblue", border = "white",
     main = "4. Histograma Res. Pearson", xlab = "Residuo")

par(mfrow = c(1, 1))

readline(prompt = "\nPulsa ENTER para continuar con la tabla de diagnosticos...")

# Tabla resumen de diagnosticos
diag <- data.frame(
  Prueba = c("Chi-cuadrado de Pearson (phi)",
             "LR Test (NB vs Poisson)",
             "Deviance/gl (modelo seleccionado)",
             "Ratio Var/Media (datos)",
             "AIC Poisson",
             "AIC Binomial Negativo",
             "Theta (NB)"),
  Valor = c(round(phi_est, 3),
            round(lr_stat, 4),
            round(deviance(modelo_sel) / modelo_sel$df.residual, 3),
            round(var(datos$patentes) / mean(datos$patentes), 3),
            round(aic_p, 3),
            round(aic_bn, 3),
            round(modelo_bn$theta, 3)),
  Interpretacion = c(
    ifelse(phi_est > 1.5, "Sobredispersion severa",
           ifelse(phi_est > 1.1, "Sobredispersion moderada", "Equidispersion")),
    ifelse(p_valor_lr < 0.05, "NB significativamente mejor", "No se rechaza Poisson"),
    ifelse(deviance(modelo_sel) / modelo_sel$df.residual < 1.2, "Buen ajuste", "Posible mal ajuste"),
    ifelse(var(datos$patentes) / mean(datos$patentes) > 1.2, "Sobredispersion", "Equidispersion"),
    "", "",
    paste0("alpha = ", round(1 / modelo_bn$theta, 3))
  )
)

cat("\n[Abriendo tabla de diagnosticos en el Viewer...]\n")
tabla_dt(diag, titulo = "Resumen de diagnosticos del modelo", filas = FALSE)

readline(prompt = "\nPulsa ENTER para ver el resumen final...")

# ============================================================================
# RESUMEN FINAL
# ============================================================================

cat("\n========================================================\n")
cat("  RESUMEN Y CONCLUSIONES\n")
cat("========================================================\n")

cat("\n1. EXPLORACION:\n")
cat("   Media de patentes: ", round(mean(datos$patentes), 2), "\n")
cat("   Varianza:          ", round(var(datos$patentes), 2), "\n")
cat("   Sobredispersion:   ",
    ifelse(phi_est > 1.1, "SI", "NO"),
    "(phi =", round(phi_est, 3), ")\n")

cat("\n2. MODELOS:\n")
cat("   Poisson: Log-veros. =", round(ll_p, 3), ", AIC =", round(aic_p, 3), "\n")
cat("   BN:      Log-veros. =", round(ll_bn, 3), ", AIC =", round(aic_bn, 3), "\n")

cat("\n3. DECISION:\n")
if (aic_bn < aic_p) {
  message("   Binomial Negativa es MEJOR (menor AIC)")
} else {
  message("   Poisson es MEJOR (menor AIC)")
}

cat("\n4. INTERPRETACION ECONOMICA:\n")
cat("   - Mayor gasto en I+D -> mas patentes\n")
cat("   - Mayor tamano empresarial -> mas patentes\n")
cat("   - La region influye en el nivel medio de patentes\n")

cat("\n========================================================\n")
cat("  FIN DEL ANALISIS\n")
cat("========================================================\n\n")
