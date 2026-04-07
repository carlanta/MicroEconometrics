# =============================================================================
# TEMA 07 — CP2: Supervivencia de Empresas
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Analizar la supervivencia de 600 empresas.
#           El endeudamiento aumenta el riesgo de cierre; la innovación lo reduce.
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
if (!requireNamespace("survival", quietly=TRUE)) install.packages("survival", quiet=TRUE)
suppressPackageStartupMessages(library(survival))
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
cat("  CP02 — Supervivencia de Empresas\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR, "T07_CP02_supervivencia_empresas.RData"))
cat("Dataset:", nrow(emp_dur), "empresas\n")
cat(sprintf("  Cierres: %d | Activas (censuradas): %d (%.0f%%)\n\n",
            sum(emp_dur$cierre), sum(1-emp_dur$cierre), 100*mean(1-emp_dur$cierre)))

# --- DESCRIPTIVOS ---
cat("--- ESTADÍSTICOS DESCRIPTIVOS ---\n\n")
vars <- c("anios_activa","capital_inicial","num_empleados","endeudamiento","innovacion")
labs <- c("Años activa","Capital inic.","Empleados","Endeudamiento","Innovación")
cat(sprintf("  %-16s %8s %8s %8s %8s\n", "Variable","Media","D.E.","Mín.","Máx."))
cat(sprintf("  %s\n", paste(rep("-",54), collapse="")))
for (i in seq_along(vars)) {
  x <- emp_dur[[vars[i]]]
  cat(sprintf("  %-16s %8.2f %8.2f %8.2f %8.2f\n", labs[i],
              mean(x), sd(x), min(x), max(x)))
}
cat(sprintf("\n  Sectores: %s\n", paste(names(table(emp_dur$sector)), collapse=", ")))
pausa()

# --- EDA ---
png(file.path(OUTPUT_DIR, "T07_CP02_eda.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)
hist(emp_dur$anios_activa, breaks=30, col=gray(0.70), border="white",
     main="Distribución duración", xlab="Años activa", ylab="Frecuencia")
surv_obj <- Surv(emp_dur$anios_activa, emp_dur$cierre)
km_inn <- survfit(surv_obj ~ emp_dur$innovacion)
plot(km_inn, lwd=2, lty=c(1,2), col=gray(c(0.2,0.5)),
     xlab="Años", ylab="S(t)", main="KM por innovación", conf.int=FALSE)
legend("topright", c("Sin I+D","Con I+D"),
       lty=c(1,2), lwd=2, col=gray(c(0.2,0.5)), cex=0.8, bty="n")
dev.off()
cat("\n  Gráfico: output/T07_CP02_eda.png\n")
pausa()

# --- COX ---
cat("\n--- MODELO DE COX ---\n\n")
cox <- coxph(surv_obj ~ capital_inicial + num_empleados + innovacion + endeudamiento,
             data=emp_dur)
s <- summary(cox)
cat(sprintf("  %-16s %9s %9s %8s %10s\n", "Variable","HR","Coef.","EE","p-valor"))
cat(sprintf("  %s\n", paste(rep("-",58), collapse="")))
for (i in seq_len(nrow(s$coef))) {
  nm <- rownames(s$coef)[i]
  sig <- ifelse(s$coef[i,5]<0.001,"***",ifelse(s$coef[i,5]<0.01,"**",
               ifelse(s$coef[i,5]<0.05,"*","   ")))
  cat(sprintf("  %-16s %9.4f %9.4f %8.4f %10.6f %s\n",
              nm, s$coef[i,2], s$coef[i,1], s$coef[i,3], s$coef[i,5], sig))
}
cat(sprintf("\n  Concordancia: %.3f\n", s$concordance[1]))
cat(sprintf("\n  Innovación: HR=%.3f → innovar REDUCE el riesgo de cierre en %.0f%%.\n",
            s$coef["innovacion",2], abs(100*(s$coef["innovacion",2]-1))))
cat(sprintf("  Endeudamiento: HR=%.3f → +10pp deuda AUMENTA el riesgo en %.0f%%.\n",
            s$coef["endeudamiento",2], abs(100*(s$coef["endeudamiento",2]^0.1-1))))
pausa()

# --- PROPORCIONALIDAD ---
cat("\n--- PROPORCIONALIDAD (SCHOENFELD) ---\n\n")
zph <- cox.zph(cox)
for (i in seq_len(nrow(zph$table))) {
  cat(sprintf("  %-16s p = %.4f %s\n", rownames(zph$table)[i],
              zph$table[i,"p"], ifelse(zph$table[i,"p"]>0.05,"✓","")))
}
pausa()

# --- WEIBULL ---
cat("\n--- WEIBULL ---\n\n")
dur_pos <- pmax(emp_dur$anios_activa, 0.01)
wei <- survreg(Surv(dur_pos, emp_dur$cierre) ~ capital_inicial + num_empleados +
               innovacion + endeudamiento, data=emp_dur, dist="weibull")
cat(sprintf("  Shape: %.3f → hazard %s\n", 1/wei$scale,
            ifelse(1/wei$scale>1,"creciente","decreciente")))
cat(sprintf("  AIC: %.1f\n", AIC(wei)))
pausa()

cat("\n================================================================\n")
cat("  RESUMEN CP02\n")
cat("================================================================\n")
cat("  La innovación protege; el endeudamiento destruye.\n")
cat(sprintf("  Weibull shape=%.2f: %s\n", 1/wei$scale,
            ifelse(1/wei$scale<1,"el riesgo de cierre DECRECE con la edad",
                   "el riesgo CRECE con la edad")))
cat("================================================================\n")
