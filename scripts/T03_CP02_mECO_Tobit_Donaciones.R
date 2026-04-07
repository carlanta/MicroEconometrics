# =============================================================================
# TEMA 03 — CP2: Modelo Tobit — Donaciones Benéficas Anuales
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Analizar los determinantes de las donaciones benéficas con Tobit.
#           El 46.4% de la muestra no dona nada (censura en 0).
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
pkgs <- c("AER","lmtest")
for (p in pkgs) if (!requireNamespace(p,quietly=TRUE)) install.packages(p,quiet=TRUE)
suppressPackageStartupMessages({ library(AER); library(lmtest) })
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

cat("\n================================================================\n")
cat("  CP02 — Donaciones Benéficas: Modelo Tobit\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR,"T03_CP02_donaciones.RData"))
cat("Dataset:", nrow(donaciones), "obs. |", ncol(donaciones), "variables\n")
n_d <- sum(donaciones$donacion==0)
cat(sprintf("  Ceros: %d (%.1f%%) | Media positivos: %.1f€\n\n",
            n_d, 100*n_d/nrow(donaciones),
            mean(donaciones$donacion[donaciones$donacion>0])))

cat("--- EDA RÁPIDA: medias por grupos ---\n\n")
for (v in c("ingreso","edad","religiosidad","educacion")) {
  m0 <- mean(donaciones[[v]][donaciones$donacion==0])
  m1 <- mean(donaciones[[v]][donaciones$donacion>0])
  cat(sprintf("  %-15s | No dona: %5.2f | Dona: %5.2f | Dif: %+.2f\n",
              v, m0, m1, m1-m0))
}
cat(sprintf("  %-15s | No dona: %4.1f%%  | Dona: %4.1f%%\n",
            "% con hijos",
            100*mean(donaciones$tiene_hijos[donaciones$donacion==0]),
            100*mean(donaciones$tiene_hijos[donaciones$donacion>0])))

pausa()

cat("\n--- ESTIMACIÓN MCO vs TOBIT ---\n\n")
fml_d <- donacion ~ ingreso + edad + religiosidad + educacion + tiene_hijos + sexo
mco_d <- lm(fml_d, data=donaciones)
tob_d <- AER::tobit(fml_d, left=0, data=donaciones)

ct_d  <- coef(summary(tob_d))
vars_d <- c("ingreso","edad","religiosidad","educacion","tiene_hijos","sexo")
cat("  Variable       | MCO     | Tobit   | Ratio\n")
cat("  -----------------------------------------------\n")
for (v in vars_d) {
  sig <- ifelse(ct_d[v,4]<0.001,"***",ifelse(ct_d[v,4]<0.01,"**",
               ifelse(ct_d[v,4]<0.05,"*","   ")))
  cat(sprintf("  %-14s | %7.3f | %7.3f %s | %.2f\n", v,
              coef(mco_d)[v], coef(tob_d)[v], sig,
              coef(tob_d)[v]/coef(mco_d)[v]))
}
cat(sprintf("  sigma:          | %-7s | %7.3f\n", "—", tob_d$scale))

pausa()

cat("\n--- EFECTOS MARGINALES AME ---\n\n")
beta_d <- coef(tob_d); sig_d <- tob_d$scale
xb_d   <- predict(tob_d, newdata=donaciones, type="lp")
z_d    <- xb_d / sig_d
Phi_d  <- pnorm(z_d); imr_d <- dnorm(z_d)/Phi_d

ame_d    <- mean(Phi_d) * beta_d[vars_d]
ame_cond <- mean(1-as.numeric(z_d)*as.numeric(imr_d)-as.numeric(imr_d)^2) * beta_d[vars_d]

cat("  Variable       | AME E[y|x]  | AME E[y|y>0]\n")
cat("  ---------------------------------------------------\n")
for (v in vars_d) {
  cat(sprintf("  %-14s | %+10.3f  | %+10.3f\n", v, ame_d[v], ame_cond[v]))
}

cat("\n▶ INTERPRETACIÓN CLAVE:\n")
cat(sprintf("  · Religiosidad: el predictor más potente (AME=%.2f€/año).\n",
            ame_d["religiosidad"]))
cat("    Un punto más en el índice 0-5 aumenta la donación esperada\n")
cat(sprintf("    en %.2f€ por año. El efecto es muy superior al del ingreso.\n",
            ame_d["religiosidad"]))
cat(sprintf("  · Ingreso: AME=%.2f€/año por cada €1.000 adicionales.\n",
            ame_d["ingreso"]))
cat(sprintf("  · Hijos: tener hijos REDUCE la donación en %.2f€/año. Las\n",
            abs(ame_d["tiene_hijos"])))
cat("    cargas familiares desplazan el gasto discrecional en donaciones.\n")

pausa()

cat("\n--- BONDAD DEL AJUSTE ---\n\n")
ll_0 <- as.numeric(logLik(glm(as.integer(donacion>0)~1, family=binomial, data=donaciones))) * 0
ll_n  <- as.numeric(logLik(AER::tobit(donacion~1, left=0, data=donaciones)))
ll_m  <- as.numeric(logLik(tob_d))
r2_mf <- 1 - ll_m/ll_n
lr_st <- -2*(ll_n - ll_m)
cat(sprintf("  Log-verosimilitud nulo:    %.2f\n", ll_n))
cat(sprintf("  Log-verosimilitud modelo:  %.2f\n", ll_m))
cat(sprintf("  McFadden R²:               %.4f\n", r2_mf))
cat(sprintf("  Test LR (vs nulo):         %.2f (p < 0.001)\n", lr_st))
cat(sprintf("  AIC:                       %.2f\n", AIC(tob_d)))

cat("\n================================================================\n")
cat("  FIN DEL SCRIPT T03_CP02_mECO_Tobit_Donaciones.R\n")
cat("================================================================\n\n")
