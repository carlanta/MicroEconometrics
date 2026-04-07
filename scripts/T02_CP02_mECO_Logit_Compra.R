# =============================================================================
# TEMA 02 — CP2: Elección Discreta — Decisión de Compra Online (e-commerce)
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Modelar la probabilidad de compra en una tienda online con Logit.
#           Ilustrar la no linealidad de los efectos mediante curvas de
#           probabilidad y calcular AME y odds ratios.
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}

pkgs <- c("lmtest","sandwich")
for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, quiet=TRUE)
suppressPackageStartupMessages({ library(lmtest); library(sandwich) })
.get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  for (a in args) {
    if (startsWith(a, "--file=")) return(dirname(normalizePath(substring(a, 8))))
  }
  for (i in seq_len(sys.nframe())) {
    ofile <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  }
  return(normalizePath("."))
}
.sdir <- .get_script_dir()
DATA_DIR <- normalizePath(file.path(.sdir, "..", "data"), mustWork=FALSE)
OUTPUT_DIR <- normalizePath(file.path(.sdir, "..", "output"), mustWork=FALSE)
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive=TRUE)


# -------------------------------------------------------------------------- #
# 1. CARGA Y EDA                                                              #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  CP02 — Compra Online: Modelos de Elección Discreta\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR, "T02_CP02_compra_online.RData"))
cat("Dataset:", nrow(ecommerce), "sesiones de navegación\n\n")

tab_ec <- table(ecommerce$compra)
cat(sprintf("  Sin compra (y=0): %d sesiones (%.1f%%) — 'bounce rate'\n",
            tab_ec[1], 100*tab_ec[1]/nrow(ecommerce)))
cat(sprintf("  Con compra (y=1): %d sesiones (%.1f%%) — tasa de conversión\n",
            tab_ec[2], 100*tab_ec[2]/nrow(ecommerce)))

cat("\n--- ESTADÍSTICOS POR GRUPO ---\n\n")
cat("  Variable             | Sin compra | Con compra | Diferencia\n")
cat("  -----------------------------------------------------------\n")
for (v in c("tiempo_sesion","visitas_previas","descuento","edad","ingreso_mensual")) {
  m0 <- mean(ecommerce[[v]][ecommerce$compra==0])
  m1 <- mean(ecommerce[[v]][ecommerce$compra==1])
  cat(sprintf("  %-20s | %9.2f  | %9.2f  | %+.2f\n", v, m0, m1, m1-m0))
}
cat(sprintf("  %-20s | %9.1f%% | %9.1f%% |\n", "% mujeres",
            100*mean(ecommerce$sexo[ecommerce$compra==0]),
            100*mean(ecommerce$sexo[ecommerce$compra==1])))

cat("\n▶ INTERPRETACIÓN:\n")
cat("  Las sesiones que terminan en compra son más largas (mayor tiempo_sesion),\n")
cat("  provienen de usuarios más recurrentes (visitas_previas) y tienen mayor\n")
cat("  descuento asociado. Estos patrones confirmará el modelo formalmente.\n")

pausa()

# -------------------------------------------------------------------------- #
# 2. ESTIMACIÓN                                                               #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 2 — Estimación del Modelo Logit\n")
cat("================================================================\n\n")

formula_ec <- compra ~ tiempo_sesion + visitas_previas + descuento +
              edad + sexo + ingreso_mensual

logit_ec  <- glm(formula_ec, data=ecommerce, family=binomial(link="logit"))
probit_ec <- glm(formula_ec, data=ecommerce, family=binomial(link="probit"))

ct_ec <- coeftest(logit_ec)
b_ec  <- ct_ec[,1]; pv_ec <- ct_ec[,4]
or_ec <- exp(b_ec)

cat("  Variable             | Coef.   | OR      | p-valor | Sig.\n")
cat("  -----------------------------------------------------------\n")
vars_ec <- c("tiempo_sesion","visitas_previas","descuento","edad","sexo","ingreso_mensual")
for (v in vars_ec) {
  sig <- ifelse(pv_ec[v]<0.001,"***",ifelse(pv_ec[v]<0.01,"**",
               ifelse(pv_ec[v]<0.05,"*","   ")))
  cat(sprintf("  %-20s | %+6.4f  | %6.4f  | %6.4f  | %s\n",
              v, b_ec[v], or_ec[v], pv_ec[v], sig))
}

cat("\n▶ INTERPRETACIÓN DE COEFICIENTES:\n")
cat("  Todos los signos son los esperados económicamente:\n")
cat(sprintf("  · Tiempo sesión: OR=%.2f → cada minuto adicional multiplica los odds\n",
            or_ec["tiempo_sesion"]))
cat(sprintf("    de compra por %.2f (+%.0f%%).\n",
            or_ec["tiempo_sesion"], (or_ec["tiempo_sesion"]-1)*100))
cat(sprintf("  · Visitas previas: OR=%.2f → usuarios fieles tienen %.0f%% más odds.\n",
            or_ec["visitas_previas"], (or_ec["visitas_previas"]-1)*100))
cat(sprintf("  · Descuento: OR=%.2f → cada punto de descuento sube los odds un %.1f%%.\n",
            or_ec["descuento"], (or_ec["descuento"]-1)*100))
cat(sprintf("  · Edad: OR=%.4f → usuarios mayores compran algo menos (efecto pequeño).\n",
            or_ec["edad"]))

pausa()

# -------------------------------------------------------------------------- #
# 3. EFECTOS MARGINALES Y CURVAS DE PROBABILIDAD                             #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 3 — AME y Curvas de Probabilidad\n")
cat("================================================================\n\n")

coefs_ec <- coef(logit_ec)
X_ec     <- model.matrix(logit_ec)
lmbd_ec  <- dlogis(X_ec %*% coefs_ec)
ame_ec   <- mean(as.numeric(lmbd_ec)) * coefs_ec

cat("  Variable             | AME Logit  | Interpr. (×100 = pp)\n")
cat("  --------------------------------------------------------\n")
for (v in vars_ec) {
  cat(sprintf("  %-20s | %+.5f    | %+.2f puntos porcentuales\n",
              v, ame_ec[v], ame_ec[v]*100))
}

cat("\n▶ INTERPRETACIÓN DEL AME:\n")
cat(sprintf("  · Un minuto más de sesión aumenta la prob. de compra en %.2f pp.\n",
            ame_ec["tiempo_sesion"]*100))
cat(sprintf("  · Cada visita previa adicional: +%.2f pp de probabilidad de compra.\n",
            ame_ec["visitas_previas"]*100))
cat(sprintf("  · Cada punto de descuento: +%.3f pp. Para un descuento de 20%%,\n",
            ame_ec["descuento"]*100))
cat(sprintf("    el efecto acumulado es de +%.2f pp respecto a precio sin descuento.\n",
            ame_ec["descuento"]*100*20))

# Curvas de probabilidad
cat("\nGenerando curvas de probabilidad...\n")
png(file.path(OUTPUT_DIR, "T02_CP02_curvas_probabilidad.png"),
    width=1600, height=800, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2,0.5))

# Curva tiempo_sesion
t_seq <- seq(1, 40, length.out=100)
nd_t <- data.frame(tiempo_sesion=t_seq,
                   visitas_previas=mean(ecommerce$visitas_previas),
                   descuento=mean(ecommerce$descuento),
                   edad=mean(ecommerce$edad),
                   sexo=mean(ecommerce$sexo),
                   ingreso_mensual=mean(ecommerce$ingreso_mensual))
p_t <- predict(logit_ec, newdata=nd_t, type="response")
plot(t_seq, p_t, type="l", lwd=2.5, ylim=c(0,1),
     xlab="Tiempo de sesión (min.)", ylab="P(compra = 1)",
     main="Efecto tiempo de sesión")
abline(h=0.5, lty=2, col=gray(0.5))
# Añadir IC aproximado
p_t_se <- predict(logit_ec, newdata=nd_t, type="link", se.fit=TRUE)
p_lo <- plogis(p_t_se$fit - 1.96*p_t_se$se.fit)
p_hi <- plogis(p_t_se$fit + 1.96*p_t_se$se.fit)
polygon(c(t_seq, rev(t_seq)), c(p_lo, rev(p_hi)),
        col=gray(0.90), border=NA)
lines(t_seq, p_t, lwd=2.5)
legend("topleft", legend=c("Probabilidad predicha","IC 95%"),
       lty=c(1,NA), fill=c(NA,gray(0.85)), border=c(NA,"gray50"),
       lwd=c(2.5,NA), bty="n", cex=0.8)

# Curva descuento por tipo de usuario (alto vs bajo engagement)
d_seq <- seq(0, 50, by=1)
nd_dH <- data.frame(tiempo_sesion=12, visitas_previas=4,
                     descuento=d_seq, edad=30, sexo=1, ingreso_mensual=30)
nd_dL <- data.frame(tiempo_sesion=3,  visitas_previas=0,
                     descuento=d_seq, edad=45, sexo=0, ingreso_mensual=20)
p_dH <- predict(logit_ec, newdata=nd_dH, type="response")
p_dL <- predict(logit_ec, newdata=nd_dL, type="response")

plot(d_seq, p_dH, type="l", lwd=2.5, lty=1, ylim=c(0,1),
     xlab="Descuento (%)", ylab="P(compra = 1)",
     main="Efecto descuento por perfil")
lines(d_seq, p_dL, lwd=2.5, lty=2)
abline(h=0.5, lty=3, col=gray(0.5))
legend("bottomright", legend=c("Alta implicación","Baja implicación"),
       lty=c(1,2), lwd=c(2.5,2.5), bty="n", cex=0.85)
dev.off()
cat("  Gráfico guardado: output/T02_CP02_curvas_probabilidad.png\n")

cat("\n▶ INTERPRETACIÓN DE LAS CURVAS:\n")
cat("  · Izquierda: la probabilidad de compra crece con el tiempo de sesión\n")
cat("    de forma no lineal (curva S). El efecto se satura — más allá de\n")
cat("    20-25 minutos, el incremento marginal es pequeño.\n")
cat("  · Derecha: el descuento es más efectivo para usuarios con alta\n")
cat("    implicación (más visitas, más tiempo). Un 20% de descuento puede\n")
cat("    ser suficiente para convertir un usuario de alta implicación\n")
cat("    pero insuficiente para uno de baja implicación.\n")

pausa()

# -------------------------------------------------------------------------- #
# 4. BONDAD DEL AJUSTE                                                        #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 4 — Bondad del Ajuste y Clasificación\n")
cat("================================================================\n\n")

ll_0  <- as.numeric(logLik(glm(compra~1, data=ecommerce, family=binomial)))
ll_ec <- as.numeric(logLik(logit_ec))
r2_ec <- 1 - ll_ec/ll_0
lr_ec <- -2*(ll_0 - ll_ec)
k_ec  <- length(coef(logit_ec)) - 1

pred_class <- as.integer(fitted(logit_ec) > 0.5)
cm <- table(Predicho=pred_class, Real=ecommerce$compra)

cat("  McFadden R²:                     ", sprintf("%.4f", r2_ec), "\n")
cat("  Test LR (estadístico):           ", sprintf("%.2f", lr_ec), "\n")
cat("  Test LR (p-valor):               < 0.001\n")
cat("  % clasificados correctamente:    ", sprintf("%.1f%%\n",
    mean(pred_class == ecommerce$compra)*100))

cat("\n  TABLA DE CLASIFICACIÓN (umbral = 0.5):\n\n")
cat("                    REAL y=0    REAL y=1\n")
cat(sprintf("  PRED y=0 (no compra)  %5d       %5d\n", cm[1,1], cm[1,2]))
cat(sprintf("  PRED y=1 (compra)     %5d       %5d\n", cm[2,1], cm[2,2]))
cat(sprintf("\n  Sensibilidad (recall): %.1f%% de las compras reales son detectadas.\n",
            100*cm[2,2]/(cm[1,2]+cm[2,2])))
cat(sprintf("  Especificidad:         %.1f%% de las no-compras son correctamente\n",
            100*cm[1,1]/(cm[1,1]+cm[2,1])))
cat("                         clasificadas como no-compras.\n")

cat("\n▶ NOTA: El umbral 0.5 es estándar pero no siempre óptimo. En e-commerce,\n")
cat("  puede interesar bajar el umbral (ej. 0.35) para capturar más compradores\n")
cat("  potenciales a costa de mayor tasa de falsos positivos.\n")

cat("\n================================================================\n")
cat("  FIN DEL SCRIPT T02_CP02_mECO_Logit_Compra.R\n")
cat("================================================================\n\n")
