# =============================================================================
# TEMA 2 — ELECCIÓN DISCRETA (LOGIT Y PROBIT): SCRIPT INTERACTIVO
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
# Autor:  Carlos de Anta Puig
#         Economista - Perito Financiero
#         Miembro del Colegio de Economistas de Madrid
#         Miembro del Instituto Espanol de Analistas Financieros (IEAF)
#         Profesor de Econometría y Microeconometría
#         carlosmaria.deanta@unir.net
#
# Version: 1.0 | Fecha: Marzo 2026
#
# Descripcion:
#   Script pedagogico completo del Tema 2. Cubre:
#   - Generacion del dataset simulado de participacion laboral
#   - Modelo Lineal de Probabilidad (MLP) y sus limitaciones
#   - Modelo Probit: estimacion, efectos marginales, bondad de ajuste
#   - Modelo Logit: odds ratios, efectos marginales, comparacion
#   - Comparacion completa MLP vs Probit vs Logit
#
# Uso:
#   Ejecutar el script completo en RStudio. Se detiene entre secciones
#   con readline() para que el alumno pueda leer los resultados.
# ============================================================================


# ============================================================================
# CARGA DE LIBRERIAS
# ============================================================================
suppressPackageStartupMessages({
  library(lmtest)    # test de heterocedasticidad (bptest)
  library(sandwich)  # errores estandar robustos
  library(mfx)       # efectos marginales: probitmfx(), logitmfx()
  library(aod)       # test de Wald: wald.test()
  library(DT)        # tablas interactivas
})

# Funcion auxiliar para pausas interactivas
pausa <- function() invisible(readline(prompt = "Pulsa ENTER para continuar..."))

# Funcion auxiliar para tablas interactivas DT
tabla_dt <- function(datos, titulo = "", decimales = 2, filas = TRUE) {
  if (is.numeric(datos) && !is.null(names(datos))) {
    datos <- data.frame(Nombre = names(datos), Valor = round(datos, decimales))
    filas <- FALSE
  }
  cat("[Abriendo tabla en el Viewer...]\n")
  print(DT::datatable(
    datos,
    caption  = titulo,
    extensions = 'Buttons',
    options  = list(
      dom        = 'Bfrtip',
      buttons    = c('copy', 'csv', 'excel'),
      pageLength = 15,
      scrollX    = TRUE
    ),
    rownames = filas
  ))
}

# Funcion auxiliar para transparencias en graficos
alpha_color <- function(col, alpha) {
  rgb_vals <- col2rgb(col) / 255
  rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], alpha = alpha)
}


# ============================================================================
# SECCION 1: GENERACION DEL DATASET
# ============================================================================
# Generamos un dataset simulado de participacion laboral en Espana.
# Variable dependiente: trabaja (1 = participa, 0 = no participa)
# El proceso generador de datos (DGP) usa una funcion logistica.
# ============================================================================

set.seed(2024)
N <- 500

# --- Variables explicativas ---
educacion       <- round(runif(N, min = 6, max = 20))
edad            <- round(runif(N, min = 18, max = 65))
sexo            <- rbinom(N, size = 1, prob = 0.50)
experiencia     <- pmax(0, round(edad - educacion - 6 + rnorm(N, mean = 0, sd = 3)))
hijos           <- sample(0:4, N, replace = TRUE, prob = c(0.35, 0.30, 0.20, 0.10, 0.05))
ingreso_familiar <- round(pmax(0, rnorm(N, mean = 10, sd = 6)), 1)

# --- Indice latente y probabilidad (DGP verdadero) ---
b0 <- -2.00; b1 <- 0.20; b2 <- 0.03; b3 <- -0.60
b4 <-  0.05; b5 <- -0.35; b6 <- -0.08

z <- b0 + b1*educacion + b2*edad + b3*sexo +
     b4*experiencia + b5*hijos + b6*ingreso_familiar

prob_trabaja <- 1 / (1 + exp(-z))
trabaja <- rbinom(N, size = 1, prob = prob_trabaja)

# --- Construccion del data frame ---
datos <- data.frame(
  id               = 1:N,
  trabaja          = trabaja,
  educacion        = educacion,
  edad             = edad,
  sexo             = factor(sexo, levels = c(0, 1), labels = c("Hombre", "Mujer")),
  experiencia      = experiencia,
  hijos            = hijos,
  ingreso_familiar = ingreso_familiar
)

# --- Exploracion basica ---
cat("=== DATASET GENERADO ===\n")
cat("Observaciones:", N, "\n")
cat("Proporcion que trabaja:", round(mean(datos$trabaja), 3), "\n\n")
print(summary(datos[, -1]))

cat("\nTasa de participacion por sexo:\n")
print(tapply(datos$trabaja, datos$sexo, mean))

# Tabla interactiva del dataset
tabla_dt(datos, titulo = "Dataset: participacion laboral simulada")

# --- Grafico exploratorio ---
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

barplot(table(datos$trabaja),
        names.arg = c("No trabaja (0)", "Trabaja (1)"),
        col = c("#E74C3C", "#2ECC71"),
        main = "Distribucion de 'trabaja'",
        ylab = "Frecuencia", xlab = "")

boxplot(educacion ~ trabaja, data = datos,
        names = c("No trabaja", "Trabaja"),
        col = c("#E74C3C", "#2ECC71"),
        main = "Educacion segun participacion",
        ylab = "Anos de educacion")

barplot(tapply(datos$trabaja, datos$sexo, mean),
        col = c("#3498DB", "#E91E8C"),
        main = "Tasa de participacion por sexo",
        ylab = "Proporcion que trabaja", ylim = c(0, 1))
abline(h = mean(datos$trabaja), lty = 2, col = "gray50")

hist(prob_trabaja, breaks = 20, col = "#9B59B6",
     main = "Distribucion de P(trabaja=1) verdadera",
     xlab = "Probabilidad", ylab = "Frecuencia")
abline(v = 0.5, lty = 2, col = "red")

par(mfrow = c(1, 1))

# Para que se pare
pausa()



# ============================================================================
# SECCION 2: MODELO LINEAL DE PROBABILIDAD (MLP)
# ============================================================================
# P(trabaja = 1 | x) = b0 + b1*educacion + ... (estimacion por MCO)
# Demostramos sus tres limitaciones principales.
# ============================================================================

mlp_modelo <- lm(trabaja ~ educacion + edad + sexo +
                   experiencia + hijos + ingreso_familiar,
                 data = datos)

cat("=== RESULTADOS DEL MLP (MCO) ===\n")
print(summary(mlp_modelo))

# --- Tabla de coeficientes ---
coefs <- summary(mlp_modelo)$coefficients
tabla_mlp <- data.frame(
  Variable    = rownames(coefs),
  Coeficiente = round(coefs[, 1], 4),
  Std_Error   = round(coefs[, 2], 4),
  t_valor     = round(coefs[, 3], 3),
  p_valor     = round(coefs[, 4], 4),
  Significativo = ifelse(coefs[, 4] < 0.001, "***",
                  ifelse(coefs[, 4] < 0.01,  "**",
                  ifelse(coefs[, 4] < 0.05,  "*",
                  ifelse(coefs[, 4] < 0.10,  ".",  ""))))
)
rownames(tabla_mlp) <- NULL
tabla_dt(tabla_mlp, titulo = "Coeficientes del MLP", filas = FALSE)

# --- Interpretacion ---
cat("\n=== INTERPRETACION (MLP: coefs = efectos marginales) ===\n")
b <- coef(mlp_modelo)
cat(sprintf("educacion      : +1 ano -> +%.1f pp en P(trabaja)\n", b["educacion"]*100))
cat(sprintf("sexo (Mujer)   : ser mujer -> %.1f pp\n", b["sexoMujer"]*100))
cat(sprintf("hijos          : +1 hijo  -> %.1f pp\n", b["hijos"]*100))
cat(sprintf("ingreso_fam    : +1.000 EUR -> %.1f pp\n", b["ingreso_familiar"]*100))

# --- Limitacion 1: predicciones fuera de [0,1] ---
datos$prob_mlp <- predict(mlp_modelo, type = "response")
n_fuera <- sum(datos$prob_mlp < 0 | datos$prob_mlp > 1)
cat(sprintf("\nPredicciones fuera de [0,1]: %d de %d (%.1f%%)\n",
            n_fuera, nrow(datos), 100 * n_fuera / nrow(datos)))

# --- Limitacion 2: heterocedasticidad ---
cat("\n=== TEST DE HETEROCEDASTICIDAD (Breusch-Pagan) ===\n")
bp_test <- bptest(mlp_modelo)
print(bp_test)
if (bp_test$p.value < 0.05) {
  message("-> Rechazamos H0: HAY heterocedasticidad (p < 0.05)")
} else {
  message("-> No se rechaza H0: sin evidencia de heterocedasticidad")
}

# --- Errores robustos ---
cat("\n=== COEFICIENTES CON ERRORES ROBUSTOS (HC1) ===\n")
print(coeftest(mlp_modelo, vcov = vcovHC(mlp_modelo, type = "HC1")))

# --- Graficos MLP ---
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

hist(datos$prob_mlp, breaks = 30, col = "#3498DB", border = "white",
     main = "MLP: Distribucion de probabilidades predichas",
     xlab = "P(trabaja = 1)", ylab = "Frecuencia")
abline(v = c(0, 1), col = "red", lty = 2, lwd = 2)

mlp_simple <- lm(trabaja ~ educacion, data = datos)
plot(datos$educacion + runif(nrow(datos), -0.3, 0.3),
     datos$trabaja,
     pch = 16, col = alpha_color("#2ECC71", 0.3),
     main = "MLP: Recta ajustada (trabaja ~ educacion)",
     xlab = "Anos de educacion", ylab = "P(trabaja = 1)")
abline(mlp_simple, col = "#E74C3C", lwd = 2)
abline(h = c(0, 1), col = "gray50", lty = 2)

plot(mlp_modelo$fitted.values, mlp_modelo$residuals,
     pch = 16, col = alpha_color("#9B59B6", 0.4),
     main = "MLP: Residuos vs Valores Ajustados",
     xlab = "Valores ajustados", ylab = "Residuos")
abline(h = 0, col = "red", lty = 2, lwd = 2)

edu_seq <- seq(6, 20, length.out = 100)
pred_mlp_line <- coef(mlp_simple)[1] + coef(mlp_simple)[2] * edu_seq
logit_simple <- glm(trabaja ~ educacion, data = datos, family = binomial)
pred_logit_line <- predict(logit_simple,
                            newdata = data.frame(educacion = edu_seq),
                            type = "response")
plot(edu_seq, pred_mlp_line,
     type = "l", col = "#E74C3C", lwd = 2, ylim = c(-0.1, 1.1),
     main = "MLP vs Logit: forma funcional",
     xlab = "Anos de educacion", ylab = "P(trabaja = 1)")
lines(edu_seq, pred_logit_line, col = "#2980B9", lwd = 2, lty = 2)
abline(h = c(0, 1), col = "gray70", lty = 3)
legend("topleft", legend = c("MLP (lineal)", "Logit (no lineal)"),
       col = c("#E74C3C", "#2980B9"), lty = c(1, 2), lwd = 2, bty = "n")

par(mfrow = c(1, 1))

cat("\n=== RESUMEN: LIMITACIONES DEL MLP ===\n")
cat("1. Probabilidades fuera de [0,1]\n")
cat("2. Heterocedasticidad del error\n")
cat("3. Efecto marginal constante (no realista)\n")
cat("-> Solucion: Probit o Logit\n")

pausa()


# ============================================================================
# SECCION 3: MODELO PROBIT
# ============================================================================
# P(trabaja = 1 | x) = Phi(x'beta)
# Phi = CDF de la Normal Estandar. Estimacion por Maxima Verosimilitud.
# ============================================================================

probit_modelo <- glm(trabaja ~ educacion + edad + sexo +
                       experiencia + hijos + ingreso_familiar,
                     data = datos, family = binomial(link = "probit"))

cat("=== RESULTADOS DEL MODELO PROBIT ===\n")
print(summary(probit_modelo))

# --- Tabla de coeficientes ---
coefs_p <- summary(probit_modelo)$coefficients
tabla_probit <- data.frame(
  Variable      = rownames(coefs_p),
  Coeficiente   = round(coefs_p[, 1], 4),
  Std_Error     = round(coefs_p[, 2], 4),
  z_valor       = round(coefs_p[, 3], 3),
  p_valor       = round(coefs_p[, 4], 4),
  Significativo = ifelse(coefs_p[, 4] < 0.001, "***",
                  ifelse(coefs_p[, 4] < 0.01,  "**",
                  ifelse(coefs_p[, 4] < 0.05,  "*",
                  ifelse(coefs_p[, 4] < 0.10,  ".",  ""))))
)
rownames(tabla_probit) <- NULL
tabla_dt(tabla_probit, titulo = "Coeficientes del modelo Probit", filas = FALSE)

# --- Interpretacion de coeficientes (solo signo) ---
cat("\nATENCION: En Probit los coeficientes NO son efectos marginales.\n")
cat("Solo informan la DIRECCION del efecto:\n\n")
b <- coef(probit_modelo)
for (nm in names(b)[-1]) {
  signo <- ifelse(b[nm] > 0, "AUMENTA (+)", "DISMINUYE (-)")
  cat(sprintf("  %-20s -> P(trabaja=1) %s\n", nm, signo))
}

pausa()

# --- Efectos marginales ---
cat("\n=== EFECTOS MARGINALES (en la media muestral) ===\n")
probit_mfx <- probitmfx(trabaja ~ educacion + edad + sexo +
                          experiencia + hijos + ingreso_familiar,
                        data = datos, atmean = TRUE)
print(probit_mfx)

cat("\n=== EFECTOS MARGINALES (promedio - AME) ===\n")
probit_mfx_avg <- probitmfx(trabaja ~ educacion + edad + sexo +
                               experiencia + hijos + ingreso_familiar,
                             data = datos, atmean = FALSE)
print(probit_mfx_avg)

# --- Interpretacion de efectos marginales ---
cat("\n=== INTERPRETACION DE EFECTOS MARGINALES ===\n")
em <- probit_mfx$mfxest
cat(sprintf("educacion   : +1 ano -> +%.2f pp en P(trabaja)\n", em["educacion","dF/dx"]*100))
cat(sprintf("sexo (Mujer): ser mujer -> %.2f pp\n", em["sexoMujer","dF/dx"]*100))
cat(sprintf("hijos       : +1 hijo -> %.2f pp\n", em["hijos","dF/dx"]*100))

pausa()

# --- Bondad de ajuste ---
cat("\n=== BONDAD DE AJUSTE ===\n")
pseudoR2_mcfadden <- 1 - (probit_modelo$deviance / probit_modelo$null.deviance)
cat(sprintf("Pseudo-R2 de McFadden: %.4f\n", pseudoR2_mcfadden))
cat("  (Valores entre 0.2 y 0.4 = buen ajuste en modelos binarios)\n\n")
cat(sprintf("AIC: %.2f\n", AIC(probit_modelo)))
cat(sprintf("BIC: %.2f\n\n", BIC(probit_modelo)))

# --- Contrastes ---
cat("=== TEST DE WALD (significacion conjunta) ===\n")
cat("H0: todos los coeficientes (excepto constante) = 0\n\n")
wald_res <- wald.test(b = coef(probit_modelo), Sigma = vcov(probit_modelo), Terms = 2:7)
print(wald_res)

cat("\n=== TEST DE RATIO DE VEROSIMILITUD (LR) ===\n")
probit_nulo <- glm(trabaja ~ 1, data = datos, family = binomial(link = "probit"))
LR_stat     <- 2 * (logLik(probit_modelo) - logLik(probit_nulo))
LR_df       <- length(coef(probit_modelo)) - 1
LR_pval     <- pchisq(LR_stat, df = LR_df, lower.tail = FALSE)
cat(sprintf("LR = %.2f, gl = %d, p-valor = %.6f\n", LR_stat, LR_df, LR_pval))
if (LR_pval < 0.05) message("-> Rechazamos H0: modelo conjuntamente significativo.")

# --- Clasificacion ---
cat("\n=== TABLA DE CLASIFICACION (umbral = 0.5) ===\n")
datos$prob_probit <- predict(probit_modelo, type = "response")
datos$pred_probit <- ifelse(datos$prob_probit > 0.5, 1, 0)
tabla_clasif_p <- table(Observado = datos$trabaja, Predicho = datos$pred_probit)
print(tabla_clasif_p)
cat(sprintf("Tasa de aciertos: %.1f%%\n", 100 * sum(diag(tabla_clasif_p)) / sum(tabla_clasif_p)))

# --- Graficos Probit ---
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

edu_seq <- seq(min(datos$educacion), max(datos$educacion), length.out = 100)
datos_pred <- data.frame(
  educacion = edu_seq, edad = mean(datos$edad),
  sexo = factor("Hombre", levels = levels(datos$sexo)),
  experiencia = mean(datos$experiencia),
  hijos = median(datos$hijos), ingreso_familiar = mean(datos$ingreso_familiar)
)
prob_hombre <- predict(probit_modelo, newdata = datos_pred, type = "response")
datos_pred$sexo <- factor("Mujer", levels = levels(datos$sexo))
prob_mujer  <- predict(probit_modelo, newdata = datos_pred, type = "response")

plot(edu_seq, prob_hombre, type = "l", col = "#3498DB", lwd = 2,
     ylim = c(0, 1), xlab = "Anos de educacion", ylab = "P(trabaja = 1)",
     main = "Probit: P(trabaja) vs Educacion")
lines(edu_seq, prob_mujer, col = "#E91E8C", lwd = 2, lty = 2)
abline(h = 0.5, col = "gray60", lty = 3)
legend("topleft", legend = c("Hombre", "Mujer"),
       col = c("#3498DB", "#E91E8C"), lty = c(1, 2), lwd = 2, bty = "n")

em_df <- as.data.frame(probit_mfx$mfxest)
em_df$variable <- rownames(em_df)
em_df <- em_df[order(em_df$`dF/dx`), ]
barplot(em_df$`dF/dx` * 100, names.arg = em_df$variable,
        horiz = TRUE, las = 1,
        col = ifelse(em_df$`dF/dx` > 0, "#2ECC71", "#E74C3C"),
        main = "Probit: Efectos marginales (pp)",
        xlab = "Efecto marginal (pp)", cex.names = 0.75)
abline(v = 0, col = "black", lwd = 1)

hist(datos$prob_probit, breaks = 30, col = "#9B59B6", border = "white",
     main = "Probit: Distribucion P(trabaja=1)",
     xlab = "Probabilidad predicha", ylab = "Frecuencia")
abline(v = 0.5, col = "red", lty = 2, lwd = 2)

boxplot(prob_probit ~ trabaja, data = datos,
        names = c("No trabaja (y=0)", "Trabaja (y=1)"),
        col = c("#E74C3C", "#2ECC71"),
        main = "Probit: Probabilidades por grupo",
        ylab = "P(trabaja = 1)")
abline(h = 0.5, col = "blue", lty = 2)

par(mfrow = c(1, 1))

pausa()


# ============================================================================
# SECCION 4: MODELO LOGIT
# ============================================================================
# P(trabaja = 1 | x) = Lambda(x'beta) = 1 / (1 + exp(-x'beta))
# Estimacion por MV. Ventaja: interpretacion via odds ratios.
# ============================================================================

logit_modelo <- glm(trabaja ~ educacion + edad + sexo +
                      experiencia + hijos + ingreso_familiar,
                    data = datos, family = binomial(link = "logit"))

cat("=== RESULTADOS DEL MODELO LOGIT ===\n")
print(summary(logit_modelo))

# --- Tabla de coeficientes ---
coefs_l <- summary(logit_modelo)$coefficients
tabla_logit <- data.frame(
  Variable      = rownames(coefs_l),
  Coeficiente   = round(coefs_l[, 1], 4),
  Std_Error     = round(coefs_l[, 2], 4),
  z_valor       = round(coefs_l[, 3], 3),
  p_valor       = round(coefs_l[, 4], 4),
  Significativo = ifelse(coefs_l[, 4] < 0.001, "***",
                  ifelse(coefs_l[, 4] < 0.01,  "**",
                  ifelse(coefs_l[, 4] < 0.05,  "*",
                  ifelse(coefs_l[, 4] < 0.10,  ".",  ""))))
)
rownames(tabla_logit) <- NULL
tabla_dt(tabla_logit, titulo = "Coeficientes del modelo Logit", filas = FALSE)

pausa()

# --- Odds Ratios ---
cat("\n=== ODDS RATIOS e IC al 95% ===\n")
OR       <- exp(coef(logit_modelo))
IC_lower <- exp(confint.default(logit_modelo)[, 1])
IC_upper <- exp(confint.default(logit_modelo)[, 2])

tabla_OR <- data.frame(
  Variable  = names(OR),
  OR        = round(OR, 4),
  IC_inf_95 = round(IC_lower, 4),
  IC_sup_95 = round(IC_upper, 4),
  Efecto    = ifelse(OR > 1, "Aumenta odds (+)",
               ifelse(OR < 1, "Reduce odds (-)", "Sin efecto"))
)
rownames(tabla_OR) <- NULL
tabla_dt(tabla_OR, titulo = "Odds Ratios del modelo Logit", filas = FALSE)

cat("\nINTERPRETACION:\n")
OR_edu <- exp(coef(logit_modelo)["educacion"])
cat(sprintf("  educacion: OR = %.3f -> +1 ano aumenta odds en %.1f%%\n",
            OR_edu, (OR_edu - 1) * 100))
OR_sexo <- exp(coef(logit_modelo)["sexoMujer"])
cat(sprintf("  sexo (Mujer): OR = %.3f -> odds de mujer son %.1f%% menores\n",
            OR_sexo, abs(1 - OR_sexo) * 100))

pausa()

# --- Efectos marginales ---
cat("\n=== EFECTOS MARGINALES LOGIT (en la media) ===\n")
logit_mfx <- logitmfx(trabaja ~ educacion + edad + sexo +
                        experiencia + hijos + ingreso_familiar,
                      data = datos, atmean = TRUE)
print(logit_mfx)

cat("\n=== EFECTOS MARGINALES LOGIT (promedio - AME) ===\n")
logit_mfx_avg <- logitmfx(trabaja ~ educacion + edad + sexo +
                             experiencia + hijos + ingreso_familiar,
                           data = datos, atmean = FALSE)
print(logit_mfx_avg)

# --- Bondad de ajuste ---
cat("\n=== BONDAD DE AJUSTE ===\n")
pseudoR2_logit <- 1 - (logit_modelo$deviance / logit_modelo$null.deviance)
cat(sprintf("Pseudo-R2 McFadden: %.4f\n", pseudoR2_logit))
cat(sprintf("AIC: %.2f  |  BIC: %.2f\n\n", AIC(logit_modelo), BIC(logit_modelo)))

# --- Test de Wald ---
cat("=== TEST DE WALD ===\n")
wald_logit <- wald.test(b = coef(logit_modelo), Sigma = vcov(logit_modelo), Terms = 2:7)
print(wald_logit)

# --- Clasificacion ---
cat("\n=== TABLA DE CLASIFICACION (umbral = 0.5) ===\n")
datos$prob_logit <- predict(logit_modelo, type = "response")
datos$pred_logit <- ifelse(datos$prob_logit > 0.5, 1, 0)
tabla_clasif_l <- table(Observado = datos$trabaja, Predicho = datos$pred_logit)
print(tabla_clasif_l)
cat(sprintf("Tasa de aciertos: %.1f%%\n",
            100 * sum(diag(tabla_clasif_l)) / sum(tabla_clasif_l)))

# --- Graficos Logit ---
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Curva sigmoide
edu_seq <- seq(min(datos$educacion), max(datos$educacion), length.out = 100)
datos_pred_h <- data.frame(
  educacion = edu_seq, edad = mean(datos$edad),
  sexo = factor("Hombre", levels = levels(datos$sexo)),
  experiencia = mean(datos$experiencia),
  hijos = median(datos$hijos), ingreso_familiar = mean(datos$ingreso_familiar)
)
datos_pred_m <- datos_pred_h
datos_pred_m$sexo <- factor("Mujer", levels = levels(datos$sexo))

prob_h <- predict(logit_modelo, newdata = datos_pred_h, type = "response")
prob_m <- predict(logit_modelo, newdata = datos_pred_m, type = "response")

plot(edu_seq, prob_h, type = "l", col = "#3498DB", lwd = 2.5,
     ylim = c(0, 1), xlab = "Anos de educacion", ylab = "P(trabaja = 1)",
     main = "Logit: Funcion sigmoide")
lines(edu_seq, prob_m, col = "#E91E8C", lwd = 2.5, lty = 2)
abline(h = 0.5, col = "gray60", lty = 3)
legend("topleft", legend = c("Hombre", "Mujer"),
       col = c("#3498DB", "#E91E8C"), lty = c(1, 2), lwd = 2, bty = "n")

# Forest plot OR
OR_vars <- OR[-1]
OR_low  <- IC_lower[-1]
OR_high <- IC_upper[-1]
n_vars  <- length(OR_vars)
plot(OR_vars, 1:n_vars, pch = 16, col = "#2C3E50",
     xlim = c(min(OR_low)*0.8, max(OR_high)*1.2),
     xlab = "Odds Ratio (exp(beta))", ylab = "",
     yaxt = "n", main = "Logit: Odds Ratios con IC 95%", log = "x")
segments(OR_low, 1:n_vars, OR_high, 1:n_vars, col = "#2C3E50", lwd = 2)
axis(2, at = 1:n_vars, labels = names(OR_vars), las = 1, cex.axis = 0.8)
abline(v = 1, col = "red", lty = 2, lwd = 2)

# Efectos marginales Logit
em_df_l <- as.data.frame(logit_mfx$mfxest)
em_df_l$variable <- rownames(em_df_l)
em_df_l <- em_df_l[order(em_df_l$`dF/dx`), ]
barplot(em_df_l$`dF/dx` * 100, names.arg = em_df_l$variable,
        horiz = TRUE, las = 1,
        col = ifelse(em_df_l$`dF/dx` > 0, "#27AE60", "#C0392B"),
        main = "Logit: Efectos marginales (pp)",
        xlab = "Cambio en P(trabaja) en pp", cex.names = 0.75)
abline(v = 0, col = "black")

# Comparacion Logit vs Probit (scatter)
em_comp <- data.frame(
  em_logit  = logit_mfx$mfxest[, "dF/dx"] * 100,
  em_probit = probit_mfx$mfxest[, "dF/dx"] * 100
)
plot(em_comp$em_probit, em_comp$em_logit,
     pch = 16, col = "#8E44AD", cex = 1.5,
     xlab = "EM Probit (pp)", ylab = "EM Logit (pp)",
     main = "Logit vs Probit: Efectos marginales")
abline(0, 1, col = "red", lty = 2, lwd = 2)
text(em_comp$em_probit, em_comp$em_logit,
     labels = rownames(em_comp), pos = 4, cex = 0.7)

par(mfrow = c(1, 1))

pausa()


# ============================================================================
# SECCION 5: COMPARACION COMPLETA MLP vs PROBIT vs LOGIT
# ============================================================================

cat("=== TABLA 1: COMPARACION DE COEFICIENTES ===\n")
cat("(Los coeficientes NO son comparables entre modelos; importan signos y significatividad)\n\n")

coef_mlp    <- coef(mlp_modelo)
coef_probit <- coef(probit_modelo)
coef_logit  <- coef(logit_modelo)
pval_mlp    <- summary(mlp_modelo)$coefficients[, 4]
pval_probit <- summary(probit_modelo)$coefficients[, 4]
pval_logit  <- summary(logit_modelo)$coefficients[, 4]

sig <- function(p) ifelse(p < 0.001, "***",
                   ifelse(p < 0.01, "**",
                   ifelse(p < 0.05, "*",
                   ifelse(p < 0.10, ".", ""))))

tabla_coef <- data.frame(
  Variable = names(coef_mlp),
  MLP      = paste0(round(coef_mlp, 4), sig(pval_mlp)),
  Probit   = paste0(round(coef_probit, 4), sig(pval_probit)),
  Logit    = paste0(round(coef_logit, 4), sig(pval_logit))
)
tabla_dt(tabla_coef, titulo = "Comparacion de coeficientes: MLP vs Probit vs Logit", filas = FALSE)

pausa()

# --- Efectos marginales comparados ---
cat("\n=== TABLA 2: EFECTOS MARGINALES (pp) ===\n")
em_mlp    <- coef_mlp[-1] * 100
em_probit <- probit_mfx_avg$mfxest[, "dF/dx"] * 100
em_logit  <- logit_mfx_avg$mfxest[, "dF/dx"] * 100

tabla_em <- data.frame(
  Variable       = names(em_mlp),
  MLP_pp         = round(em_mlp, 3),
  Probit_pp      = round(em_probit, 3),
  Logit_pp       = round(em_logit, 3),
  Dif_Prob_Logit = round(em_probit - em_logit, 4)
)
rownames(tabla_em) <- NULL
tabla_dt(tabla_em, titulo = "Efectos marginales comparados (pp)", filas = FALSE)

cat("\nNOTA: Los EM de Probit y Logit son practicamente identicos.\n")

pausa()

# --- Bondad de ajuste comparada ---
cat("\n=== TABLA 3: METRICAS DE AJUSTE ===\n")
R2_mlp     <- summary(mlp_modelo)$r.squared
pseudoR2_p <- 1 - probit_modelo$deviance / probit_modelo$null.deviance
pseudoR2_l <- 1 - logit_modelo$deviance  / logit_modelo$null.deviance

aciertos_mlp <- mean(ifelse(datos$prob_mlp > 0.5, 1, 0) == datos$trabaja) * 100
aciertos_p   <- mean(ifelse(datos$prob_probit > 0.5, 1, 0) == datos$trabaja) * 100
aciertos_l   <- mean(ifelse(datos$prob_logit  > 0.5, 1, 0) == datos$trabaja) * 100
fuera_rango  <- sum(datos$prob_mlp < 0 | datos$prob_mlp > 1)

tabla_ajuste <- data.frame(
  Metrica = c("R2 / Pseudo-R2 McFadden", "AIC", "BIC",
              "Tasa de aciertos (%)", "Predicciones fuera [0,1]"),
  MLP    = c(round(R2_mlp, 4), round(AIC(mlp_modelo), 1),
             round(BIC(mlp_modelo), 1), round(aciertos_mlp, 1), fuera_rango),
  Probit = c(round(pseudoR2_p, 4), round(AIC(probit_modelo), 1),
             round(BIC(probit_modelo), 1), round(aciertos_p, 1), 0),
  Logit  = c(round(pseudoR2_l, 4), round(AIC(logit_modelo), 1),
             round(BIC(logit_modelo), 1), round(aciertos_l, 1), 0)
)
tabla_dt(tabla_ajuste, titulo = "Metricas de ajuste: MLP vs Probit vs Logit", filas = FALSE)

# --- Grafico principal: curvas de probabilidad predicha ---
edu_seq <- seq(6, 20, length.out = 200)
datos_grid <- data.frame(
  educacion = edu_seq, edad = mean(datos$edad),
  sexo = factor("Hombre", levels = levels(datos$sexo)),
  experiencia = mean(datos$experiencia),
  hijos = 1, ingreso_familiar = mean(datos$ingreso_familiar)
)

pred_mlp_c    <- predict(mlp_modelo,    newdata = datos_grid)
pred_probit_c <- predict(probit_modelo, newdata = datos_grid, type = "response")
pred_logit_c  <- predict(logit_modelo,  newdata = datos_grid, type = "response")

par(mfrow = c(1, 1), mar = c(5, 5, 4, 2))
set.seed(42)
plot(datos$educacion + runif(nrow(datos), -0.4, 0.4),
     datos$trabaja + runif(nrow(datos), -0.03, 0.03),
     pch = 16, cex = 0.5, col = rgb(0.5, 0.5, 0.5, 0.3),
     xlab = "Anos de educacion", ylab = "P(trabaja = 1)",
     main = "MLP, Probit y Logit: Probabilidades predichas",
     ylim = c(-0.15, 1.15))
lines(edu_seq, pred_mlp_c,    col = "#E74C3C", lwd = 2.5)
lines(edu_seq, pred_probit_c, col = "#2ECC71", lwd = 2.5, lty = 2)
lines(edu_seq, pred_logit_c,  col = "#3498DB", lwd = 2.5, lty = 3)
abline(h = c(0, 1), col = "gray40", lty = 2, lwd = 1.5)
legend("topleft",
       legend = c("Datos observados", "MLP (lineal)", "Probit", "Logit"),
       pch = c(16, NA, NA, NA), lty = c(NA, 1, 2, 3),
       lwd = c(NA, 2.5, 2.5, 2.5),
       col = c(rgb(0.5, 0.5, 0.5, 0.5), "#E74C3C", "#2ECC71", "#3498DB"),
       bty = "n", cex = 0.85)

# --- Diferencias entre predicciones ---
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

diff_mlp_logit <- pred_mlp_c - pred_logit_c
plot(edu_seq, diff_mlp_logit, type = "l", col = "#E74C3C", lwd = 2,
     xlab = "Anos de educacion", ylab = "P_MLP - P_Logit",
     main = "Diferencia MLP - Logit")
abline(h = 0, lty = 2, col = "gray50")

diff_probit_logit <- pred_probit_c - pred_logit_c
plot(edu_seq, diff_probit_logit, type = "l", col = "#2ECC71", lwd = 2,
     xlab = "Anos de educacion", ylab = "P_Probit - P_Logit",
     main = "Diferencia Probit - Logit")
abline(h = 0, lty = 2, col = "gray50")

par(mfrow = c(1, 1))

cat("\n=== RESUMEN COMPARATIVO ===\n")
cat("MLP   : Simple pero con problemas teoricos (P fuera [0,1], EM constantes)\n")
cat("PROBIT: Basado en Normal estandar. Solido teoricamente. Uso: error ~ N(0,1).\n")
cat("LOGIT : Basado en distribucion logistica. Odds ratios interpretables.\n")
cat("Conclusion: Probit y Logit dan resultados practicamente identicos.\n")

cat("\n=== FIN DEL SCRIPT TEMA 2 ===\n")
