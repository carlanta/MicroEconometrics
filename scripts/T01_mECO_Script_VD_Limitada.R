# =============================================================================
# TEMA 1 — VARIABLE DEPENDIENTE LIMITADA: SCRIPT INTERACTIVO
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

# Autor:   Carlos de Anta Puig
#          Economista · Perito Financiero
#          Miembro del Colegio de Economistas de Madrid
#          Miembro del Instituto Español de Analistas Financieros (IEAF)
#          Profesor de Econometría y Microeconometría
#          carlosmaria.deanta@unir.net
#
# Curso:    Microeconometría — CdeA
# Objetivo: Explorar los distintos tipos de variables dependientes limitadas
#           (binarias, multinomiales, censuradas, truncadas), entender por
#           qué MCO falla y familiarizarse con las distribuciones asociadas.
#
# Version: 1.1
# Fecha:   2026-03-16
#
# INSTRUCCIONES:
#   1. Ejecuta este script línea a línea con Ctrl+Enter.
#   2. Los resultados aparecerán en la CONSOLA (abajo).
#   3. Los gráficos aparecerán en el panel PLOTS (abajo-derecha).
#   4. Las líneas que empiezan por # son COMENTARIOS (no se ejecutan).
# ============================================================================


# ============================================================================
# SECCION 1: INTRODUCCION
# ============================================================================

# Funcion de pausa interactiva
pausa <- function(msg = "\n>>> Pulsa ENTER para continuar...") {
  readline(msg)
}

# Carga de paquetes
suppressPackageStartupMessages({
  library(DT)
})

# Funcion auxiliar: tabla interactiva con exportacion
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

cat("\n")
cat("============================================================\n")
cat("║                                                                ║\n")
cat("║   TEMA 1 — Datos con Variable Dependiente Limitada            ║\n")
cat("║   Script compañero del manual de apuntes                      ║\n")
cat("║                                                                ║\n")
cat("║   Profesor: Carlos de Anta Puig                                ║\n")
cat("║   Curso 2025-2026 | Q2                                        ║\n")
cat("║                                                                ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n")
cat("\n")


pausa()

# ============================================================================
# SECCION 2: VARIABLES BINARIAS Y DISTRIBUCION DE BERNOULLI
# ============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SECCION 2: VARIABLES BINARIAS Y DISTRIBUCION DE BERNOULLI\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# --- 2.1 Simulacion de una variable Bernoulli ---
cat("--- 2.1 Simulación de una variable Bernoulli ---\n\n")

# Fijamos la semilla para reproducibilidad
set.seed(2026)

# Simulamos 100 ensayos Bernoulli con p = 0.72
# (Ej: 72% de probabilidad de que una persona trabaje)
p <- 0.72
n <- 100
y_bernoulli <- rbinom(n, size = 1, prob = p)

# Veamos las primeras observaciones
cat("Primeras 20 observaciones (0 = no trabaja, 1 = trabaja):\n")
print(y_bernoulli[1:20])

# Tabla de frecuencias
cat("\nTabla de frecuencias:\n")
print(table(y_bernoulli))

# Proporcion muestral (debe estar cerca de p = 0.72)
cat("\nProporción muestral de y = 1:", mean(y_bernoulli), "\n")
cat("Valor teórico de p:         ", p, "\n")

# --- 2.2 Gráfico de la distribución Bernoulli ---
cat("\n--- 2.2 Gráfico de la distribución Bernoulli ---\n\n")

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
for (p_val in c(0.2, 0.5, 0.8)) {
  bp <- barplot(c(1 - p_val, p_val),
                names.arg = c("y = 0", "y = 1"),
                col = c("#E74C3C", "#2ECC71"),
                ylim = c(0, 1),
                ylab = "Probabilidad",
                main = paste0("Bernoulli(p = ", p_val, ")"),
                border = "white")
  abline(h = 0)
  text(bp[1], 1 - p_val + 0.05, sprintf("%.1f", 1 - p_val), font = 2)
  text(bp[2], p_val + 0.05, sprintf("%.1f", p_val), font = 2)
}
par(mfrow = c(1, 1))

# --- 2.3 Momentos de la Bernoulli ---
cat("--- 2.3 Momentos de la distribución Bernoulli ---\n\n")

cat("Para p = 0.72:\n")
cat("  Esperanza:  E(y)   = p         =", p, "\n")
cat("  Varianza:   Var(y) = p*(1-p)   =", p * (1 - p), "\n")
cat("  Desv. típ.: SD(y)  = sqrt(Var) =", sqrt(p * (1 - p)), "\n")

# Verificamos con los datos simulados
cat("\nVerificación con datos simulados (n =", n, "):\n")
cat("  Media muestral:     ", round(mean(y_bernoulli), 4), "\n")
cat("  Varianza muestral:  ", round(var(y_bernoulli), 4), "\n")

# NOTA: La varianza depende de p, que a su vez depende de x_i
# Esto VIOLA la homocedasticidad → MCO no funciona bien


pausa()

# ============================================================================
# SECCION 3: POR QUE MCO FALLA CON VARIABLES BINARIAS (MLP)
# ============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  SECCION 3: POR QUE MCO FALLA (MODELO LINEAL DE PROBABILIDAD)\n")
cat("============================================================\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# --- 3.1 Generar datos con estructura Probit ---
cat("--- 3.1 Simulación del Modelo Lineal de Probabilidad ---\n\n")

set.seed(2026)
n <- 200
x <- runif(n, -3, 6)

# Probabilidad real de y=1 (modelo Probit)
p_real <- pnorm(0.5 + 0.8 * x)

# Generar variable binaria
y <- rbinom(n, 1, p_real)

# Estimar con MCO (Modelo Lineal de Probabilidad)
mlp <- lm(y ~ x)
cat("Resultados del Modelo Lineal de Probabilidad (MCO):\n\n")
print(summary(mlp))

# --- 3.2 Problemas del MLP ---
cat("\n--- 3.2 Problemas del MLP: predicciones fuera de [0, 1] ---\n\n")

predicciones <- predict(mlp)
cat("Rango de predicciones del MLP:\n")
cat("  Mínimo:", round(min(predicciones), 4), "\n")
cat("  Máximo:", round(max(predicciones), 4), "\n")
n_fuera <- sum(predicciones < 0 | predicciones > 1)
cat("  Predicciones fuera de [0,1]:", n_fuera, "de", n,
    sprintf("(%.1f%%)\n", 100 * n_fuera / n))

# --- 3.3 Gráfico comparativo MLP vs Probit ---
cat("\n--- 3.3 Gráfico comparativo ---\n\n")

par(mar = c(4.5, 4.5, 3, 1))
plot(x, y, pch = 19, col = adjustcolor("gray40", alpha = 0.4),
     xlab = "Variable explicativa (x)",
     ylab = "y / Probabilidad predicha",
     main = "MCO (MLP) vs Modelo correcto (Probit)",
     ylim = c(-0.3, 1.3))
abline(mlp, col = "#E74C3C", lwd = 3)
abline(h = c(0, 1), lty = 2, col = "gray60")
x_ord <- sort(x)
lines(x_ord, pnorm(0.5 + 0.8 * x_ord), col = "#2ECC71", lwd = 3)
legend("bottomright",
       legend = c("MCO (MLP)", "Probit", "Límites 0 y 1"),
       col    = c("#E74C3C", "#2ECC71", "gray60"),
       lwd    = c(3, 3, 1), lty = c(1, 1, 2), bty = "n")
# Zonas imposibles
rect(-4, -0.5, 8, 0,
     col = adjustcolor("#E74C3C", alpha = 0.08), border = NA)
rect(-4, 1, 8, 1.5,
     col = adjustcolor("#E74C3C", alpha = 0.08), border = NA)

# --- 3.4 Heterocedasticidad inherente ---
cat("--- 3.4 La heterocedasticidad es inherente al MLP ---\n\n")
cat("En el MLP, Var(u_i) = p_i * (1 - p_i)\n")
cat("Como p_i depende de x_i, la varianza NO es constante.\n\n")

# Mostramos cómo la varianza cambia
p_ejemplo <- seq(0.05, 0.95, by = 0.05)
var_ejemplo <- p_ejemplo * (1 - p_ejemplo)

par(mar = c(4.5, 4.5, 3, 1))
plot(p_ejemplo, var_ejemplo, type = "b", pch = 19,
     col = "#3498DB", lwd = 2,
     xlab = "p (probabilidad)", ylab = "Var(u) = p(1-p)",
     main = "Varianza del error en el MLP (depende de p)")
abline(v = 0.5, lty = 2, col = "#E74C3C")
text(0.55, 0.24, "Máxima varianza\nen p = 0.5",
     col = "#C0392B", cex = 0.8)


pausa()

# ============================================================================
# SECCION 4: DISTRIBUCION BINOMIAL
# ============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  SECCION 4: DISTRIBUCION BINOMIAL\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# --- 4.1 La Binomial como suma de Bernoullis ---
cat("--- 4.1 La Binomial como suma de Bernoullis ---\n\n")

# Si n personas tienen cada una probabilidad p = 0.3 de comprar,
# ¿cuántas comprarán en total?
n_b <- 20
p_b <- 0.3
k <- 0:n_b

cat("Binomial(n =", n_b, ", p =", p_b, "):\n")
cat("  Esperanza: E(k)  = n*p  =", n_b * p_b, "\n")
cat("  Varianza:  Var(k) = n*p*(1-p) =", n_b * p_b * (1 - p_b), "\n\n")

# Tabla de probabilidades (primeros valores)
prob_bin <- dbinom(k, n_b, p_b)
cat("Probabilidades P(k) para k = 0, 1, ..., 10:\n")
for (i in 0:10) {
  cat(sprintf("  P(k = %2d) = %.4f\n", i, prob_bin[i + 1]))
}

# --- 4.2 Gráficos de la Binomial ---
cat("\n--- 4.2 Gráficos de la Binomial ---\n\n")

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

# Caso 1: n=10, p=0.3
n1 <- 10; p1 <- 0.3
barplot(dbinom(0:n1, n1, p1), names.arg = 0:n1,
        col = "#3498DB", border = "white",
        ylab = "P(k)", main = paste0("Bin(n=", n1, ", p=", p1, ")"))

# Caso 2: n=20, p=0.5
n2 <- 20; p2 <- 0.5
barplot(dbinom(0:n2, n2, p2), names.arg = 0:n2,
        col = "#9B59B6", border = "white",
        ylab = "P(k)", main = paste0("Bin(n=", n2, ", p=", p2, ")"),
        cex.names = 0.6)

# Caso 3: n=50, p=0.7
n3 <- 50; p3 <- 0.7
barplot(dbinom(0:n3, n3, p3), names.arg = 0:n3,
        col = "#E67E22", border = "white",
        ylab = "P(k)", main = paste0("Bin(n=", n3, ", p=", p3, ")"),
        cex.names = 0.4)

par(mfrow = c(1, 1))


pausa()

# ============================================================================
# SECCION 5: VARIABLE LATENTE
# ============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  SECCION 5: EL CONCEPTO DE VARIABLE LATENTE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# --- 5.1 Simulación de la variable latente ---
cat("--- 5.1 Simulación: variable latente vs observada ---\n\n")

set.seed(42)
n <- 150
x_lat <- seq(-3, 3, length.out = n)

# Variable latente (utilidad neta de trabajar)
y_star <- 0.5 + 1.2 * x_lat + rnorm(n, 0, 1.5)

# Variable observada (decisión)
y_obs <- ifelse(y_star > 0, 1, 0)

cat("Resumen de la variable latente y*:\n")
print(summary(y_star))

cat("\nTabla de la variable observada y:\n")
print(table(y_obs))

cat("\nInterpretación:\n")
cat("  y* > 0 => la utilidad de trabajar > utilidad de no trabajar => y = 1\n")
cat("  y* ≤ 0 => la utilidad de trabajar ≤ utilidad de no trabajar => y = 0\n")

# --- 5.2 Gráfico doble panel ---
cat("\n--- 5.2 Gráfico: latente vs observada ---\n\n")

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

# Panel izquierdo: variable latente
plot(x_lat, y_star, pch = 19,
     col = ifelse(y_star > 0,
                  adjustcolor("#2ECC71", 0.6),
                  adjustcolor("#E74C3C", 0.6)),
     xlab = "x (educación, experiencia...)",
     ylab = "y* (utilidad latente)",
     main = "Variable latente y*")
abline(h = 0, lty = 2, lwd = 2, col = "gray30")
abline(a = 0.5, b = 1.2, col = "#3498DB", lwd = 2)
legend("bottomright",
       legend = c("y* > 0 (trabaja)", "y* ≤ 0 (no trabaja)"),
       col = c("#2ECC71", "#E74C3C"), pch = 19, bty = "n", cex = 0.8)

# Panel derecho: variable observada
plot(x_lat, y_obs, pch = 19,
     col = ifelse(y_obs == 1,
                  adjustcolor("#2ECC71", 0.5),
                  adjustcolor("#E74C3C", 0.5)),
     xlab = "x", ylab = "y (observada)",
     main = "Variable observada y", yaxt = "n", ylim = c(-0.1, 1.1))
axis(2, at = c(0, 1))
lines(sort(x_lat), pnorm(0.5 + 1.2 * sort(x_lat), 0, 1.5),
      col = "#3498DB", lwd = 3)
legend("right", legend = "P(y=1|x)", col = "#3498DB",
       lwd = 3, bty = "n")

par(mfrow = c(1, 1))


pausa()

# ============================================================================
# SECCION 6: VARIABLES MULTINOMIALES
# ============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  SECCION 6: VARIABLES MULTINOMIALES\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# --- 6.1 Simulación de una variable multinomial no ordenada ---
cat("--- 6.1 Elección de medio de transporte (no ordenada) ---\n\n")

set.seed(2026)
probs_transp <- c(Coche = 0.45, Autobus = 0.25, Tren = 0.20, Avion = 0.10)
cat("Probabilidades teóricas:\n")
print(probs_transp)

# Simulamos 1000 elecciones
elecciones <- sample(names(probs_transp), 1000,
                     replace = TRUE, prob = probs_transp)

cat("\nFrecuencias observadas (n = 1000):\n")
freq <- table(factor(elecciones, levels = names(probs_transp)))
print(freq)

cat("\nProporciones observadas:\n")
print(round(prop.table(freq), 3))

# Gráfico de barras
par(mar = c(4.5, 4.5, 3, 1))
bp <- barplot(prop.table(freq),
              col = c("#3498DB", "#2ECC71", "#E67E22", "#9B59B6"),
              border = "white", ylim = c(0, 0.55),
              ylab = "Proporción",
              main = "Elección de medio de transporte (n = 1000)")
text(bp, as.numeric(prop.table(freq)) + 0.02,
     sprintf("%.1f%%", as.numeric(prop.table(freq)) * 100), font = 2)

# --- 6.2 Variable multinomial ordenada ---
cat("\n--- 6.2 Nivel de satisfacción (ordenada) ---\n\n")

set.seed(2026)
# Simulamos satisfacción: 0=Muy insatisfecho, ..., 4=Muy satisfecho
probs_satis <- c(0.05, 0.10, 0.25, 0.35, 0.25)
nombres_satis <- c("Muy insatisf.", "Insatisfecho",
                    "Neutro", "Satisfecho", "Muy satisf.")
satisfaccion <- sample(0:4, 500, replace = TRUE, prob = probs_satis)

cat("Tabla de frecuencias:\n")
freq_s <- table(factor(satisfaccion, levels = 0:4, labels = nombres_satis))
print(freq_s)

# Gráfico
par(mar = c(6, 4.5, 3, 1))
bp2 <- barplot(prop.table(freq_s),
               col = c("#E74C3C", "#E67E22", "#F1C40F", "#2ECC71", "#27AE60"),
               border = "white", ylim = c(0, 0.45),
               ylab = "Proporción",
               main = "Nivel de satisfacción (variable ordenada)",
               las = 2, cex.names = 0.8)
text(bp2, as.numeric(prop.table(freq_s)) + 0.02,
     sprintf("%.0f%%", as.numeric(prop.table(freq_s)) * 100), font = 2)

# --- 6.3 Distribución Multinomial en R ---
cat("\n--- 6.3 La distribución Multinomial en R ---\n\n")

# La función dmultinom calcula P(k1, k2, ..., kJ)
# Ejemplo: P(exactamente 45 coches, 25 autobuses, 20 trenes, 10 aviones)
# en una muestra de 100
prob_exacta <- dmultinom(c(45, 25, 20, 10), prob = probs_transp)
cat("P(k = [45, 25, 20, 10] | n=100, p=[0.45, 0.25, 0.20, 0.10]):\n")
cat("  ", formatC(prob_exacta, format = "e", digits = 4), "\n")

# Simulación de la multinomial
cat("\nSimulación de 5 muestras multinomiales (n = 100):\n")
for (i in 1:5) {
  muestra <- rmultinom(1, 100, probs_transp)
  cat(sprintf("  Muestra %d: Coche=%d, Autobús=%d, Tren=%d, Avión=%d\n",
              i, muestra[1], muestra[2], muestra[3], muestra[4]))
}


pausa()

# ============================================================================
# SECCION 7: DATOS CENSURADOS
# ============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  SECCION 7: DATOS CENSURADOS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# --- 7.1 Simulación de datos censurados ---
cat("--- 7.1 Simulación: gasto en seguros médicos (censurado en 0) ---\n\n")

set.seed(2026)
n <- 300
renta <- runif(n, 15, 80)  # renta en miles de euros

# Variable latente: gasto potencial
gasto_latente <- -20 + 0.6 * renta + rnorm(n, 0, 8)

# Variable observada: censurada en 0
gasto_observado <- pmax(gasto_latente, 0)

cat("Resumen de la variable latente (gasto potencial):\n")
print(round(summary(gasto_latente), 2))

cat("\nResumen de la variable observada (censurada en 0):\n")
print(round(summary(gasto_observado), 2))

n_censurados <- sum(gasto_observado == 0)
cat("\nObservaciones censuradas (gasto = 0):", n_censurados,
    sprintf("(%.1f%%)\n", 100 * n_censurados / n))

# --- 7.2 MCO sesgado con datos censurados ---
cat("\n--- 7.2 MCO está sesgado con datos censurados ---\n\n")

# MCO con datos censurados (SESGADO)
mco_censurado <- lm(gasto_observado ~ renta)
cat("MCO con datos censurados:\n")
cat("  Intercepto:", round(coef(mco_censurado)[1], 4), "\n")
cat("  Pendiente: ", round(coef(mco_censurado)[2], 4), "\n")

cat("\nValores verdaderos:\n")
cat("  Intercepto: -20\n")
cat("  Pendiente:   0.6\n")

cat("\n¡MCO subestima la pendiente y sobreestima el intercepto!\n")
cat("El modelo Tobit (Tema 3) corrige este sesgo.\n")

# --- 7.3 Gráfico de censura ---
cat("\n--- 7.3 Gráfico ---\n\n")

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

# Panel 1: Variable latente
plot(renta, gasto_latente, pch = 19,
     col = ifelse(gasto_latente > 0,
                  adjustcolor("#2ECC71", 0.5),
                  adjustcolor("#E74C3C", 0.5)),
     xlab = "Renta (miles €)", ylab = "Gasto latente (miles €)",
     main = "Variable latente y*")
abline(h = 0, lty = 2, lwd = 2)
abline(a = -20, b = 0.6, col = "#3498DB", lwd = 2)

# Panel 2: Variable censurada
plot(renta, gasto_observado, pch = 19,
     col = ifelse(gasto_observado > 0,
                  adjustcolor("#2ECC71", 0.5),
                  adjustcolor("#E74C3C", 0.5)),
     xlab = "Renta (miles €)", ylab = "Gasto observado (miles €)",
     main = "Variable censurada y")
abline(h = 0, lty = 2, lwd = 2)
abline(mco_censurado, col = "#E74C3C", lwd = 2)
abline(a = -20, b = 0.6, col = "#2ECC71", lwd = 2, lty = 2)
legend("topleft",
       legend = c("MCO (sesgado)", "Verdadera"),
       col = c("#E74C3C", "#2ECC71"), lwd = 2, lty = c(1, 2),
       bty = "n", cex = 0.8)

par(mfrow = c(1, 1))

# --- 7.4 Histograma de la variable censurada ---
cat("--- 7.4 Histograma de la variable censurada ---\n\n")

par(mar = c(4.5, 4.5, 3, 1))
hist(gasto_observado, breaks = 30,
     col = adjustcolor("#3498DB", 0.6),
     border = "white",
     main = "Distribución de la variable censurada",
     xlab = "Gasto observado (miles €)", ylab = "Frecuencia")

cat("La distribución tiene dos componentes:\n")
cat("  1. Componente DISCRETO: masa de probabilidad en y = 0\n")
cat("  2. Componente CONTINUO: para los valores y > 0\n")
cat("Esta mezcla es la razón por la que MCO no funciona.\n")

# --- 7.5 Tipos de censura ---
cat("\n--- 7.5 Tipos de censura (izquierda y derecha) ---\n\n")

set.seed(123)
y_star_c <- rnorm(500, 5, 3)

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

# Censura por la izquierda
y_cens_izq <- pmax(y_star_c, 0)
hist(y_cens_izq, breaks = 30,
     col = adjustcolor("#3498DB", 0.6), border = "white",
     main = "Censura izquierda (c = 0)",
     xlab = "y", ylab = "Frecuencia")
abline(v = 0, col = "#E74C3C", lwd = 2, lty = 2)

# Censura por la derecha
y_cens_der <- pmin(y_star_c, 10)
hist(y_cens_der, breaks = 30,
     col = adjustcolor("#E67E22", 0.6), border = "white",
     main = "Censura derecha (c = 10)",
     xlab = "y", ylab = "Frecuencia")
abline(v = 10, col = "#E74C3C", lwd = 2, lty = 2)

par(mfrow = c(1, 1))

cat("Censura izquierda: y = max(y*, c) → no observamos valores < c\n")
cat("Censura derecha:   y = min(y*, c) → no observamos valores > c\n")


pausa()

# ============================================================================
# SECCION 8: DATOS TRUNCADOS
# ============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  SECCION 8: DATOS TRUNCADOS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# --- 8.1 Simulación de datos truncados ---
cat("--- 8.1 Truncamiento vs. censura ---\n\n")

set.seed(2026)
n <- 300
x_tr <- runif(n, 0, 10)
y_star_tr <- -2 + 1.5 * x_tr + rnorm(n, 0, 2)

# Censura: observamos todos los x, pero y se fija en 0
y_censurada <- pmax(y_star_tr, 0)
n_cens <- sum(y_star_tr <= 0)

# Truncamiento: PERDEMOS las observaciones con y* <= 0
seleccion <- y_star_tr > 0
x_truncado <- x_tr[seleccion]
y_truncado <- y_star_tr[seleccion]
n_trunc_perdidos <- sum(!seleccion)

cat("Datos completos:   n =", n, "\n")
cat("Censura:          ", n_cens, "obs con y=0,",
    n - n_cens, "obs con y>0 (total:", n, ")\n")
cat("Truncamiento:     ", n_trunc_perdidos, "obs PERDIDAS,",
    sum(seleccion), "obs restantes (total:", sum(seleccion), ")\n")

cat("\n¡En el truncamiento PERDEMOS datos e información!\n")
cat("En la censura al menos conservamos las X de todos.\n")

# --- 8.2 Gráfico comparativo ---
cat("\n--- 8.2 Gráfico comparativo ---\n\n")

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

# Censura
plot(x_tr, y_censurada, pch = 19,
     col = ifelse(y_star_tr > 0,
                  adjustcolor("#2ECC71", 0.5),
                  adjustcolor("#E74C3C", 0.5)),
     xlab = "x", ylab = "y", main = "Censura en 0")
abline(h = 0, lty = 2, lwd = 2)
legend("topleft", legend = c("y > 0", "y = 0 (censurados)"),
       col = c("#2ECC71", "#E74C3C"), pch = 19, bty = "n", cex = 0.8)

# Truncamiento
plot(x_truncado, y_truncado, pch = 19,
     col = adjustcolor("#2ECC71", 0.5),
     xlab = "x", ylab = "y", main = "Truncamiento en 0",
     xlim = range(x_tr), ylim = range(y_star_tr))
abline(h = 0, lty = 2, lwd = 2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], 0,
     col = adjustcolor("#E74C3C", 0.08), border = NA)
text(8, -3, "Datos perdidos", col = "#C0392B", font = 3, cex = 0.85)

par(mfrow = c(1, 1))


pausa()

# ============================================================================
# SECCION 9: DISTRIBUCIONES CENSURADA Y TRUNCADA
# ============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  SECCION 9: DISTRIBUCIONES CENSURADA Y TRUNCADA\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# --- 9.1 Normal censurada ---
cat("--- 9.1 Distribución Normal censurada ---\n\n")

mu <- 2; sigma <- 3
y_seq <- seq(-8, 12, length.out = 500)
dens <- dnorm(y_seq, mu, sigma)

# Masa en 0
p0 <- pnorm(0, mu, sigma)
cat("Para y* ~ N(mu=2, sigma=3), censurada en 0:\n")
cat("  P(y = 0) = Phi(-mu/sigma) = Phi(", round(-mu/sigma, 4), ") =",
    round(p0, 4), "\n")
cat("  Es decir, el", sprintf("%.1f%%", p0 * 100),
    "de las observaciones quedan censuradas en 0.\n")

par(mar = c(4.5, 4.5, 3, 1))
plot(y_seq[y_seq >= 0], dens[y_seq >= 0], type = "l",
     lwd = 3, col = "#3498DB",
     xlab = "y", ylab = "Densidad / Probabilidad",
     main = "Normal censurada: mu=2, sigma=3",
     xlim = c(-2, 12), ylim = c(0, max(dens) * 1.3))
polygon(c(0, y_seq[y_seq >= 0], max(y_seq)),
        c(0, dens[y_seq >= 0], 0),
        col = adjustcolor("#3498DB", 0.2), border = NA)
segments(0, 0, 0, p0, col = "#E74C3C", lwd = 4)
points(0, p0, pch = 19, col = "#E74C3C", cex = 2)
text(0.5, p0 + 0.01, sprintf("P(y=0) = %.3f", p0),
     col = "#C0392B", font = 2, cex = 0.8, pos = 4)
lines(y_seq[y_seq < 0], dens[y_seq < 0],
      lwd = 2, lty = 3, col = "gray60")

# --- 9.2 Normal truncada ---
cat("\n--- 9.2 Distribución Normal truncada ---\n\n")

y_pos <- seq(0.001, 12, length.out = 500)
dens_norm <- dnorm(y_pos, mu, sigma)
dens_trunc <- dens_norm / (1 - pnorm(0, mu, sigma))

cat("La densidad truncada se reescala dividiendo por P(y > 0):\n")
cat("  P(y > 0) = 1 - Phi(-mu/sigma) =", round(1 - p0, 4), "\n")
cat("  Factor de reescalado: 1 /", round(1 - p0, 4), "=",
    round(1 / (1 - p0), 4), "\n")

par(mar = c(4.5, 4.5, 3, 1))
plot(y_pos, dens_trunc, type = "l", lwd = 3, col = "#E67E22",
     xlab = "y", ylab = "Densidad",
     main = "Normal truncada vs. normal original",
     ylim = c(0, max(dens_trunc) * 1.1))
lines(y_pos, dens_norm, lwd = 2, lty = 2, col = "#3498DB")
polygon(c(y_pos, rev(y_pos)),
        c(dens_trunc, rep(0, length(y_pos))),
        col = adjustcolor("#E67E22", 0.15), border = NA)
legend("topright",
       legend = c("Normal truncada", "Normal original"),
       col = c("#E67E22", "#3498DB"), lwd = c(3, 2), lty = c(1, 2),
       bty = "n")
abline(v = 0, lty = 3, col = "gray50")


pausa()

# ============================================================================
# SECCION 10: RESUMEN
# ============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  SECCION 10: RESUMEN\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Tipos de variables dependientes limitadas:\n\n")
cat("  1. BINARIAS (y ∈ {0,1}):\n")
cat("     - Distribución: Bernoulli\n")
cat("     - Modelos: Probit, Logit (Tema 2)\n")
cat("     - Concepto clave: variable latente\n\n")
cat("  2. MULTINOMIALES (y ∈ {0,1,...,J}):\n")
cat("     - Distribución: Multinomial\n")
cat("     - Tipos: ordenadas y no ordenadas\n")
cat("     - Modelos: Logit/Probit multinomial (avanzado)\n\n")
cat("  3. CONTINUAS LIMITADAS:\n")
cat("     - CENSURADAS: observamos X de todos, pero y fijada en un punto\n")
cat("       Modelo: Tobit (Tema 3)\n")
cat("     - TRUNCADAS: perdemos TODA la información fuera del rango\n")
cat("       Modelo: Heckman (selección muestral)\n\n")
cat("PUNTO CLAVE: MCO no funciona con ninguno de estos tipos de datos.\n")
cat("             Necesitamos modelos específicos para cada caso.\n")

cat("\n╔══════════════════════════════════════════════════════════════════╗\n")
cat("║  FIN del Script — Tema 1                                      ║\n")
cat("║  Siguiente: Tema 2 — Modelos Probit y Logit                  ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n")
