# =============================================================================
# TEMA 02 — CP1: Modelos de Elección Discreta — Concesión de Crédito Bancario
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Estimar MPL, Probit y Logit para modelar la probabilidad de que
#           un banco apruebe una solicitud de crédito. Calcular e interpretar
#           efectos marginales (AME, MEM) y odds ratios.
#
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}

pkgs <- c("lmtest","sandwich")
for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, quiet=TRUE)
suppressPackageStartupMessages({ library(lmtest); library(sandwich) })

DATA_DIR <- "../data"; OUTPUT_DIR <- "output"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

# -------------------------------------------------------------------------- #
# 1. CARGA Y EDA BREVE                                                        #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  CP01 — Concesión de Crédito Bancario: MPL, Probit y Logit\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR, "T02_CP01_credito_bancario.RData"))
cat("Dataset cargado:", nrow(credito), "solicitudes de crédito\n")
cat("Variables:", paste(names(credito), collapse=", "), "\n\n")

cat("--- VARIABLE DEPENDIENTE ---\n")
tab_c <- table(credito$concesion)
cat(sprintf("  Denegado (y=0): %d solicitudes (%.1f%%)\n", tab_c[1], 100*tab_c[1]/nrow(credito)))
cat(sprintf("  Aprobado (y=1): %d solicitudes (%.1f%%)\n", tab_c[2], 100*tab_c[2]/nrow(credito)))

cat("\n--- ESTADÍSTICOS DESCRIPTIVOS ---\n\n")
vars_c <- c("ingreso","historial_crediticio","ratio_deuda","experiencia_laboral","garantia","edad")
for (v in vars_c) {
  x <- credito[[v]]
  cat(sprintf("  %-24s | Media: %5.2f | DT: %5.2f | Min: %5.2f | Max: %5.2f\n",
              v, mean(x), sd(x), min(x), max(x)))
}

cat("\n▶ INTERPRETACIÓN:\n")
cat("  El 42.4% de solicitudes son aprobadas. El historial crediticio y la garantía\n")
cat("  son variables binarias. El ratio deuda/ingresos es la variable de riesgo\n")
cat("  más importante desde el punto de vista bancario.\n")

pausa()

# -------------------------------------------------------------------------- #
# 2. ESTIMACIÓN DE LOS TRES MODELOS                                           #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 2 — Estimación: MPL, Probit y Logit\n")
cat("================================================================\n\n")

formula_c <- concesion ~ ingreso + historial_crediticio + ratio_deuda +
             experiencia_laboral + garantia + edad

mpl_c    <- lm(formula_c, data=credito)
probit_c <- glm(formula_c, data=credito, family=binomial(link="probit"))
logit_c  <- glm(formula_c, data=credito, family=binomial(link="logit"))

cat("--- MODELO LINEAL DE PROBABILIDAD (con EE robustos) ---\n\n")
ct_mpl <- coeftest(mpl_c, vcov=vcovHC(mpl_c, type="HC1"))
b_mpl  <- ct_mpl[,1]; se_mpl <- ct_mpl[,2]; pv_mpl <- ct_mpl[,4]
for (i in seq_along(b_mpl)) {
  sig <- ifelse(pv_mpl[i]<0.001,"***",ifelse(pv_mpl[i]<0.01,"**",
               ifelse(pv_mpl[i]<0.05,"*","   ")))
  cat(sprintf("  %-26s %8.4f  (EE=%7.4f)  p=%6.4f %s\n",
              names(b_mpl)[i], b_mpl[i], se_mpl[i], pv_mpl[i], sig))
}

cat("\n--- MODELO PROBIT ---\n\n")
ct_p <- coeftest(probit_c)
b_p  <- ct_p[,1]; se_p <- ct_p[,2]; pv_p <- ct_p[,4]
for (i in seq_along(b_p)) {
  sig <- ifelse(pv_p[i]<0.001,"***",ifelse(pv_p[i]<0.01,"**",
               ifelse(pv_p[i]<0.05,"*","   ")))
  cat(sprintf("  %-26s %8.4f  (EE=%7.4f)  p=%6.4f %s\n",
              names(b_p)[i], b_p[i], se_p[i], pv_p[i], sig))
}

cat("\n--- MODELO LOGIT ---\n\n")
ct_l <- coeftest(logit_c)
b_l  <- ct_l[,1]; se_l <- ct_l[,2]; pv_l <- ct_l[,4]
for (i in seq_along(b_l)) {
  sig <- ifelse(pv_l[i]<0.001,"***",ifelse(pv_l[i]<0.01,"**",
               ifelse(pv_l[i]<0.05,"*","   ")))
  cat(sprintf("  %-26s %8.4f  (EE=%7.4f)  p=%6.4f %s\n",
              names(b_l)[i], b_l[i], se_l[i], pv_l[i], sig))
}

cat("\n▶ RECORDATORIO IMPORTANTE:\n")
cat("  Los coeficientes del MPL son efectos sobre la PROBABILIDAD directamente.\n")
cat("  Los coeficientes del Probit y Logit son efectos sobre el ÍNDICE LINEAL.\n")
cat("  NO se pueden comparar las magnitudes de Probit vs Logit entre sí.\n")
cat("  Para comparar efectos sobre la probabilidad hay que usar efectos marginales.\n")

pausa()

# -------------------------------------------------------------------------- #
# 3. EFECTOS MARGINALES AME Y MEM (PROBIT Y LOGIT)                           #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 3 — Efectos Marginales AME y MEM\n")
cat("================================================================\n\n")

vars_eff <- c("ingreso","historial_crediticio","ratio_deuda",
              "experiencia_laboral","garantia","edad")

# AME Probit
coefs_p <- coef(probit_c)
X_mat_p <- model.matrix(probit_c)
phi_p   <- dnorm(X_mat_p %*% coefs_p)
ame_p   <- mean(as.numeric(phi_p)) * coefs_p

# MEM Probit
x_bar_p  <- colMeans(X_mat_p)
phi_bar_p <- dnorm(sum(x_bar_p * coefs_p))
mem_p    <- phi_bar_p * coefs_p

# AME Logit
coefs_l <- coef(logit_c)
X_mat_l <- model.matrix(logit_c)
lmbd_l  <- dlogis(X_mat_l %*% coefs_l)
ame_l   <- mean(as.numeric(lmbd_l)) * coefs_l

cat("  Variable               | MPL (EE rob.) | AME Probit | AME Logit\n")
cat("  -----------------------------------------------------------------\n")
for (v in vars_eff) {
  cat(sprintf("  %-22s | %+.4f        | %+.4f     | %+.4f\n",
              v, b_mpl[v], ame_p[v], ame_l[v]))
}

cat("\n▶ INTERPRETACIÓN DE LOS EFECTOS MARGINALES:\n")
cat(sprintf("  · Ingreso: cada €1.000 adicionales de ingreso mensual aumenta la\n"))
cat(sprintf("    probabilidad de aprobación en %.1f pp (AME Probit).\n", ame_p["ingreso"]*100))
cat(sprintf("  · Historial crediticio bueno: aumenta la probabilidad en %.1f pp.\n",
            ame_p["historial_crediticio"]*100))
cat(sprintf("  · Ratio deuda/ingreso: cada punto adicional REDUCE la probabilidad\n"))
cat(sprintf("    en %.1f pp — es el factor de riesgo más penalizador.\n",
            abs(ame_p["ratio_deuda"])*100))
cat(sprintf("  · Garantía: tener garantía aumenta la probabilidad en %.1f pp.\n",
            ame_p["garantia"]*100))
cat("\n  Las tres estimaciones (MPL, Probit, Logit) son muy similares,\n")
cat("  lo que es típico cuando las probabilidades no están cerca de 0 o 1.\n")

pausa()

# -------------------------------------------------------------------------- #
# 4. ODDS RATIOS (LOGIT)                                                     #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 4 — Odds Ratios del Modelo Logit\n")
cat("================================================================\n\n")

or_c <- exp(coef(logit_c))
cat("  Variable               | Coef. Logit | Odds Ratio | Interpretación\n")
cat("  ------------------------------------------------------------------\n")
for (v in vars_eff) {
  interp <- ifelse(or_c[v]>1,
    sprintf("+%.0f%% odds", (or_c[v]-1)*100),
    sprintf("-%.0f%% odds", (1-or_c[v])*100))
  cat(sprintf("  %-22s | %+.4f      | %.4f     | %s\n",
              v, coef(logit_c)[v], or_c[v], interp))
}

cat("\n▶ INTERPRETACIÓN DE LOS ODDS RATIOS:\n")
cat(sprintf("  · Historial bueno: OR = %.2f → multiplica por %.2f los odds de\n",
            or_c["historial_crediticio"], or_c["historial_crediticio"]))
cat("    aprobación. Es decir, los odds son", sprintf("%.0f%%", (or_c["historial_crediticio"]-1)*100),
    "mayores con buen historial.\n")
cat(sprintf("  · Garantía: OR = %.2f → los odds de aprobación son un %.0f%% mayores\n",
            or_c["garantia"], (or_c["garantia"]-1)*100))
cat("    cuando existe garantía.\n")
cat(sprintf("  · Ratio deuda: OR = %.4f → por cada punto adicional en el ratio,\n",
            or_c["ratio_deuda"]))
cat("    los odds de aprobación se multiplican por un valor < 1 (se reducen).\n")

pausa()

# -------------------------------------------------------------------------- #
# 5. BONDAD DEL AJUSTE                                                        #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 5 — Bondad del Ajuste\n")
cat("================================================================\n\n")

ll_null_c <- as.numeric(logLik(glm(concesion~1, data=credito, family=binomial)))
ll_p_c    <- as.numeric(logLik(probit_c))
ll_l_c    <- as.numeric(logLik(logit_c))

r2_p_c <- 1 - ll_p_c / ll_null_c
r2_l_c <- 1 - ll_l_c / ll_null_c

lr_p_c <- -2*(ll_null_c - ll_p_c)
lr_l_c <- -2*(ll_null_c - ll_l_c)
k_c    <- length(coef(probit_c)) - 1

class_mpl_c    <- mean(as.integer(fitted(mpl_c)>0.5) == credito$concesion)*100
class_probit_c <- mean(as.integer(fitted(probit_c)>0.5) == credito$concesion)*100
class_logit_c  <- mean(as.integer(fitted(logit_c)>0.5) == credito$concesion)*100

cat("  Medida                      | MPL      | Probit   | Logit\n")
cat("  ----------------------------------------------------------\n")
cat(sprintf("  McFadden R²                 | %-8s | %-8.4f | %-8.4f\n", "—", r2_p_c, r2_l_c))
cat(sprintf("  Test LR vs. nulo            | %-8s | %-8.1f | %-8.1f\n", "—", lr_p_c, lr_l_c))
cat(sprintf("  p-valor LR                  | %-8s | %-8s | %-8s\n", "—",
            ifelse(pchisq(lr_p_c,k_c,lower.tail=FALSE)<0.001,"<0.001",
                   sprintf("%.4f",pchisq(lr_p_c,k_c,lower.tail=FALSE))),
            ifelse(pchisq(lr_l_c,k_c,lower.tail=FALSE)<0.001,"<0.001",
                   sprintf("%.4f",pchisq(lr_l_c,k_c,lower.tail=FALSE)))))
cat(sprintf("  %% clasificados (umbral 0.5) | %-8.1f | %-8.1f | %-8.1f\n",
            class_mpl_c, class_probit_c, class_logit_c))
cat(sprintf("  AIC                         | %-8.1f | %-8.1f | %-8.1f\n",
            AIC(mpl_c), AIC(probit_c), AIC(logit_c)))

cat("\n▶ INTERPRETACIÓN:\n")
cat(sprintf("  El Pseudo-R² de McFadden del Logit es %.3f. En modelos binarios,\n", r2_l_c))
cat("  valores entre 0.20 y 0.40 se consideran un buen ajuste.\n")
cat(sprintf("  El test LR rechaza con p<0.001 la hipótesis de que las variables\n"))
cat("  son conjuntamente no significativas. Los tres modelos clasifican\n")
cat(sprintf("  correctamente alrededor del %.0f%% de las solicitudes.\n", class_logit_c))

# Gráfico: predicciones
png(file.path(OUTPUT_DIR, "T02_CP01_predicciones.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2,0.5))
hist(fitted(logit_c), breaks=20, col=gray(0.70), border="white",
     xlab="Probabilidad predicha", ylab="Frecuencia", main="Predicciones Logit",
     xlim=c(0,1))
abline(v=0.5, lty=2, lwd=2)
plot(fitted(probit_c), fitted(logit_c), pch=1, col=gray(0.5), cex=0.7,
     xlab="Prob. Probit", ylab="Prob. Logit", main="Probit vs Logit")
abline(0,1,lwd=2,lty=1)
dev.off()
cat("\n  Gráfico guardado: output/T02_CP01_predicciones.png\n")

cat("\n================================================================\n")
cat("  FIN DEL SCRIPT T02_CP01_mECO_Logit_Credito.R\n")
cat("================================================================\n\n")
