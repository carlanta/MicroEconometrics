# =============================================================================
# TEMA 03 — CP1: Modelo Tobit — Gasto en Seguros Médicos
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Estimar el modelo Tobit para gasto en seguros médicos (censurado
#           en 0). Comparar con MCO, calcular efectos marginales McDonald-Moffitt
#           y contrastar los supuestos del modelo.
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
pkgs <- c("AER","lmtest","kableExtra")
for (p in pkgs) if (!requireNamespace(p,quietly=TRUE)) install.packages(p,quiet=TRUE)
suppressPackageStartupMessages({ library(AER); library(lmtest) })
DATA_DIR <- "../data"; OUTPUT_DIR <- "output"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

# ── 1. CARGA Y EDA ───────────────────────────────────────────────────────────
cat("\n================================================================\n")
cat("  CP01 — Gasto en Seguros Médicos: Modelo Tobit\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR,"T03_CP01_gasto_seguros.RData"))
cat("Dataset:", nrow(seguros), "hogares |", ncol(seguros), "variables\n\n")

n_c <- sum(seguros$gasto_seguros==0)
cat(sprintf("  Gasto = 0 (censurados): %d hogares (%.1f%%)\n",
            n_c, 100*n_c/nrow(seguros)))
cat(sprintf("  Gasto > 0 (positivos):  %d hogares (%.1f%%)\n",
            nrow(seguros)-n_c, 100*(nrow(seguros)-n_c)/nrow(seguros)))
cat(sprintf("  Media condicional (y>0): %.1f €/mes\n",
            mean(seguros$gasto_seguros[seguros$gasto_seguros>0])))
cat(sprintf("  Media incondicional:     %.1f €/mes\n\n",
            mean(seguros$gasto_seguros)))

cat("▶ DIAGNÓSTICO PREVIO:\n")
cat("  Una proporción del 33.8% de ceros indica censura sustancial.\n")
cat("  MCO será consistentemente sesgado hacia cero. El Tobit es necesario.\n")

pausa()

# ── 2. ESTIMACIÓN MCO Y TOBIT ────────────────────────────────────────────────
cat("\n================================================================\n")
cat("  SECCIÓN 2 — Estimación MCO vs Tobit\n")
cat("================================================================\n\n")

fml <- gasto_seguros ~ renta + edad + num_miembros + educacion + zona_urbana + cronico
mco <- lm(fml, data=seguros)
tob <- tobit(fml, left=0, data=seguros)

cat("--- MCO (con datos censurados — estimaciones SESGADAS) ---\n\n")
ct_m <- coef(summary(mco))
for (v in rownames(ct_m)) {
  sig <- ifelse(ct_m[v,4]<0.001,"***",ifelse(ct_m[v,4]<0.01,"**",
               ifelse(ct_m[v,4]<0.05,"*","   ")))
  cat(sprintf("  %-18s %8.3f  (EE=%6.3f)  p=%6.4f  %s\n",
              v, ct_m[v,1], ct_m[v,2], ct_m[v,4], sig))
}

cat("\n--- TOBIT (estimación consistente) ---\n\n")
ct_t <- coef(summary(tob))
vars_s <- c("(Intercept)","renta","edad","num_miembros","educacion","zona_urbana","cronico")
for (v in vars_s) {
  sig <- ifelse(ct_t[v,4]<0.001,"***",ifelse(ct_t[v,4]<0.01,"**",
               ifelse(ct_t[v,4]<0.05,"*","   ")))
  cat(sprintf("  %-18s %8.3f  (EE=%6.3f)  p=%6.4f  %s\n",
              v, ct_t[v,1], ct_t[v,2], ct_t[v,4], sig))
}
cat(sprintf("  %-18s %8.3f\n","Log(sigma)", log(tob$scale)))
cat(sprintf("  sigma (escala):    %8.3f\n\n", tob$scale))

vars_cov <- c("renta","edad","num_miembros","educacion","zona_urbana","cronico")
cat("  COMPARACIÓN RATIO TOBIT/MCO:\n\n")
cat("  Variable           |  MCO   | Tobit  | Ratio (T/M)\n")
cat("  -------------------------------------------------------\n")
for (v in vars_cov) {
  r <- coef(tob)[v]/coef(mco)[v]
  cat(sprintf("  %-18s | %6.3f | %6.3f | %.2f\n", v, coef(mco)[v], coef(tob)[v], r))
}
cat("\n▶ INTERPRETACIÓN:\n")
cat("  Todos los coeficientes Tobit son MAYORES en valor absoluto que los MCO.\n")
cat("  Esto es SIEMPRE el caso con datos censurados: MCO sesga hacia cero.\n")
cat("  El sesgo es mayor cuanto mayor es la proporción de ceros en la muestra.\n")
cat("  Los coeficientes Tobit miden el efecto de x sobre la variable LATENTE y*\n")
cat("  (el 'gasto deseado' o 'demanda óptima'), no sobre el gasto observado.\n")

pausa()

# ── 3. EFECTOS MARGINALES MCDONALD-MOFFITT ───────────────────────────────────
cat("\n================================================================\n")
cat("  SECCIÓN 3 — Efectos Marginales (McDonald-Moffitt 1980)\n")
cat("================================================================\n\n")

beta <- coef(tob); sigma <- tob$scale
xb   <- predict(tob, newdata=seguros, type="lp")
z    <- xb / sigma
Phi  <- pnorm(z); phi <- dnorm(z)
imr  <- phi/Phi
ey_pos <- as.numeric(xb) + sigma*as.numeric(imr)

# AME sobre E[y|x] (efecto total sobre gasto esperado)
ame_obs  <- mean(Phi) * beta[vars_cov]
# AME sobre E[y|y>0] (efecto condicional en tener seguro)
ame_cond <- mean(1 - as.numeric(z)*as.numeric(imr) - as.numeric(imr)^2) * beta[vars_cov]
# Descomposición
ame_int  <- mean(as.numeric(Phi)*(1-as.numeric(z)*as.numeric(imr)-as.numeric(imr)^2)) * beta[vars_cov]
ame_ext  <- ame_obs - ame_int

cat("  Variable           | AME E[y|x] | AME E[y|y>0] | Intensivo | Extensivo\n")
cat("  -------------------------------------------------------------------------\n")
for (v in vars_cov) {
  cat(sprintf("  %-18s | %9.3f  | %11.3f  | %9.3f | %9.3f\n",
              v, ame_obs[v], ame_cond[v], ame_int[v], ame_ext[v]))
}

cat("\n▶ INTERPRETACIÓN:\n")
cat(sprintf("  · Renta: un incremento de €1.000/mes aumenta el gasto esperado en\n"))
cat(sprintf("    SEGUROS en %.2f€/mes (efecto total). De este efecto, %.2f€\n",
            ame_obs["renta"], ame_int["renta"]))
cat(sprintf("    corresponden al canal intensivo y %.2f€ al extensivo.\n",
            ame_ext["renta"]))
cat(sprintf("  · Cronicidad: tener enfermedad crónica aumenta el gasto esperado\n"))
cat(sprintf("    en %.2f€/mes. El efecto extensivo (%.2f€) domina sobre el\n",
            ame_obs["cronico"], ame_ext["cronico"]))
cat(sprintf("    intensivo (%.2f€): la cronicidad opera principalmente empujando\n",
            ame_int["cronico"]))
cat("    a hogares a contratar algún seguro, más que a gastar más.\n")

pausa()

# ── 4. CONTRASTES DE ESPECIFICACIÓN ─────────────────────────────────────────
cat("\n================================================================\n")
cat("  SECCIÓN 4 — Contrastes de Especificación\n")
cat("================================================================\n\n")

e_hat <- seguros$gasto_seguros - as.numeric(Phi)*(as.numeric(xb) + sigma*as.numeric(imr))

skew_r <- mean(((e_hat-mean(e_hat))/sd(e_hat))^3)
kurt_r  <- mean(((e_hat-mean(e_hat))/sd(e_hat))^4)
jb_stat <- length(e_hat)/6*(skew_r^2+(kurt_r-3)^2/4)
jb_pval <- 1-pchisq(jb_stat,2)

bp_aux  <- lm(e_hat^2 ~ renta+edad+educacion+zona_urbana+cronico, data=seguros)
bp_stat <- summary(bp_aux)$r.squared * length(e_hat)
bp_pval <- 1-pchisq(bp_stat,5)

cat(sprintf("  Jarque-Bera (normalidad): JB=%.3f, p=%.4f\n", jb_stat, jb_pval))
if (jb_pval>0.05)
  cat("  → No se rechaza la normalidad. Supuesto SATISFECHO.\n\n")
else
  cat("  → Se rechaza la normalidad. Considerar transformación log(y).\n\n")

cat(sprintf("  Breusch-Pagan (homoced.): BP=%.3f, p=%.4f\n", bp_stat, bp_pval))
if (bp_pval>0.05)
  cat("  → No se rechaza la homocedasticidad. Supuesto SATISFECHO.\n\n")
else
  cat("  → Indicios de heterocedasticidad. Los errores estándar pueden\n")
  cat("    no ser válidos. Considerar Tobit heterocedástico.\n\n")

cat("  Log-verosimilitud del Tobit:", sprintf("%.2f\n", as.numeric(logLik(tob))))
cat("  AIC:", sprintf("%.2f\n", AIC(tob)))

# Gráfico diagnóstico
png(file.path(OUTPUT_DIR,"T03_CP01_diagnostico.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2,0.5))
hist(e_hat, breaks=30, col=gray(0.70), border="white",
     main="Residuos del Tobit", xlab="Residuos", ylab="Frecuencia")
qqnorm(e_hat, pch=1, col=gray(0.5), cex=0.7, main="Q-Q Plot residuos")
qqline(e_hat, lwd=2)
dev.off()
cat("\n  Gráfico diagnóstico: output/T03_CP01_diagnostico.png\n")

cat("\n================================================================\n")
cat("  FIN DEL SCRIPT T03_CP01_mECO_Tobit_Seguros.R\n")
cat("================================================================\n\n")
