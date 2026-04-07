# =============================================================================
# TEMA 04 — CP1: Poisson y NB — Delitos por Municipio
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Modelar el número de delitos registrados por municipio.
#           Comparar Poisson y NB, diagnosticar sobredispersión (VMR=6.8).
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
pkgs <- c("AER","MASS")
for (p in pkgs) if (!requireNamespace(p,quietly=TRUE)) install.packages(p,quiet=TRUE)
suppressPackageStartupMessages({ library(AER); library(MASS) })
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

cat("\n================================================================\n")
cat("  CP01 — Delitos por Municipio: Poisson y Binomial Negativa\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR,"T04_CP01_criminalidad.RData"))
cat("Dataset:", nrow(crimen), "municipios\n")
mu_c  <- mean(crimen$delitos); var_c <- var(crimen$delitos)
cat(sprintf("  Media: %.2f | Varianza: %.2f | VMR: %.2f\n\n", mu_c, var_c, var_c/mu_c))

cat("▶ VMR = %.2f > 1: hay sobredispersión. Diagnóstico antes de modelar.\n",
    var_c/mu_c)
cat("  Con VMR > 1, Poisson subestimará errores estándar → inferencia inválida.\n\n")

png(file.path(OUTPUT_DIR,"T04_CP01_eda.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2,0.5))
hist(crimen$delitos, breaks=25, col=gray(0.70), border="white",
     main="Distribución delitos", xlab="N.º delitos", ylab="Frecuencia")
plot(crimen$ingresos_medios, crimen$delitos, pch=1, col=gray(0.5), cex=0.7,
     xlab="Ingresos medios (€m)", ylab="N.º delitos", main="Delitos vs Ingresos")
abline(lm(delitos~ingresos_medios, data=crimen), lwd=2, lty=1)
dev.off()
cat("  Gráfico EDA: output/T04_CP01_eda.png\n")
pausa()

cat("\n--- TEST DE SOBREDISPERSIÓN (Cameron-Trivedi) ---\n\n")
fml_c <- delitos ~ densidad_pob + tasa_desempleo + ingresos_medios + policia_per_capita + zona_urbana
p_c   <- glm(fml_c, data=crimen, family=poisson)
dt_c  <- dispersiontest(p_c, trafo=2)
cat(sprintf("  alpha estimado:  %.4f\n", dt_c$estimate))
cat(sprintf("  Estadístico z:   %.3f\n", dt_c$statistic))
cat(sprintf("  p-valor:         %.6f\n\n", dt_c$p.value))
if (dt_c$p.value < 0.05)
  cat("  ✓ SOBREDISPERSIÓN CONFIRMADA (p<0.05). La NB es necesaria.\n\n") else
  cat("  ✗ No se rechaza equidispersión. Poisson podría ser suficiente.\n\n")
pausa()

cat("\n--- ESTIMACIÓN: POISSON vs BINOMIAL NEGATIVA ---\n\n")
qp_c  <- glm(fml_c, data=crimen, family=quasipoisson)
nb_c  <- glm.nb(fml_c, data=crimen)

vars_c <- c("densidad_pob","tasa_desempleo","ingresos_medios","policia_per_capita","zona_urbana")
labs_c <- c("Densidad pob.","Tasa desempleo","Ingresos medios","Policías/cápita","Zona urbana")

ct_p  <- coef(summary(p_c));  ct_nb <- coef(summary(nb_c))
cat("  Variable          | Coef.Pois | EE Pois | EE QPois | Coef.NB   | EE NB\n")
cat("  -----------------------------------------------------------------------\n")
for (v in vars_c) {
  sig <- ifelse(ct_nb[v,4]<0.001,"***",ifelse(ct_nb[v,4]<0.01,"**",
               ifelse(ct_nb[v,4]<0.05,"*","   ")))
  cat(sprintf("  %-17s | %9.4f | %7.4f | %8.4f | %9.4f %s | %5.4f\n", v,
              ct_p[v,1], ct_p[v,2], coef(summary(qp_c))[v,2],
              ct_nb[v,1], sig, ct_nb[v,2]))
}
cat(sprintf("\n  theta (NB):  %.4f (sobredispersión: Var = Media + Media²/%.4f)\n",
            nb_c$theta, nb_c$theta))

pausa()

cat("\n--- IRR Y EFECTOS MARGINALES (modelo NB) ---\n\n")
irr_c <- exp(coef(nb_c)[vars_c])
ame_c <- mean(fitted(nb_c)) * coef(nb_c)[vars_c]

cat("  Variable          | IRR    | Interp.                         | AME\n")
cat("  -------------------------------------------------------------------\n")
for (v in vars_c) {
  interp <- if (irr_c[v]>1) sprintf("+%.0f%% más delitos", (irr_c[v]-1)*100)
             else            sprintf("-%.0f%% menos delitos", (1-irr_c[v])*100)
  cat(sprintf("  %-17s | %.3f  | %-32s | %.3f\n", v, irr_c[v], interp, ame_c[v]))
}

cat("\n▶ INTERPRETACIÓN CLAVE:\n")
cat(sprintf("  · Tasa desempleo: IRR=%.3f → cada punto porcentual adicional de desempleo\n",
            irr_c["tasa_desempleo"]))
cat(sprintf("    multiplica los delitos por %.3f (+%.0f%%).\n",
            irr_c["tasa_desempleo"], (irr_c["tasa_desempleo"]-1)*100))
cat(sprintf("  · Ingresos medios: IRR=%.3f → los municipios más ricos tienen MENOS\n",
            irr_c["ingresos_medios"]))
cat("    delitos — efecto protector económico.\n")
cat(sprintf("  · Policías per cápita: IRR=%.3f → mayor presencia policial reduce\n",
            irr_c["policia_per_capita"]))
cat(sprintf("    los delitos en un %.0f%% por unidad adicional.\n",
            (1-irr_c["policia_per_capita"])*100))

cat("\n--- COMPARACIÓN AIC ---\n\n")
cat(sprintf("  AIC Poisson:            %.2f\n", AIC(p_c)))
cat(sprintf("  AIC Binomial Negativa:  %.2f\n", AIC(nb_c)))
cat(sprintf("  Diferencia AIC:         %.2f (NB mejor si > 0)\n", AIC(p_c)-AIC(nb_c)))
cat("\n================================================================\n")
cat("  FIN DEL SCRIPT T04_CP01_mECO_Poisson_Criminalidad.R\n")
cat("================================================================\n\n")
