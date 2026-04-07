# =============================================================================
# TEMA 07 — CP3: Tiempo hasta Impago Crediticio
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Modelizar el riesgo de impago de 1000 créditos (credit scoring).
#           Alta censura (~67%). Ratio deuda/ingreso como principal predictor.
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
cat("  CP03 — Tiempo hasta Impago Crediticio\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR, "T07_CP03_impago_crediticio.RData"))
cat("Dataset:", nrow(cred_dur), "créditos\n")
cat(sprintf("  Impagos: %d | Sin impago (censurados): %d (%.0f%%)\n\n",
            sum(cred_dur$impago), sum(1-cred_dur$impago), 100*mean(1-cred_dur$impago)))

# --- DESCRIPTIVOS ---
cat("--- ESTADÍSTICOS DESCRIPTIVOS ---\n\n")
vars <- c("meses_hasta_evento","importe","tipo_interes","ratio_deuda_ingreso",
          "historial_credito","hipoteca")
labs <- c("Meses hasta evento","Importe","Tipo interés","Ratio deuda/ing.",
          "Historial cred.","Hipoteca")
cat(sprintf("  %-18s %10s %10s %10s %10s\n", "Variable","Media","D.E.","Mín.","Máx."))
cat(sprintf("  %s\n", paste(rep("-",62), collapse="")))
for (i in seq_along(vars)) {
  x <- cred_dur[[vars[i]]]
  cat(sprintf("  %-18s %10.2f %10.2f %10.2f %10.2f\n", labs[i],
              mean(x), sd(x), min(x), max(x)))
}
pausa()

# --- EDA ---
png(file.path(OUTPUT_DIR, "T07_CP03_eda.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)
hist(cred_dur$meses_hasta_evento, breaks=30, col=gray(0.70), border="white",
     main="Distribución duración", xlab="Meses", ylab="Frecuencia")
surv_obj <- Surv(cred_dur$meses_hasta_evento, cred_dur$impago)
# KM por ratio alto/bajo
cred_dur$ratio_alto <- ifelse(cred_dur$ratio_deuda_ingreso > median(cred_dur$ratio_deuda_ingreso), 1, 0)
km_ratio <- survfit(surv_obj ~ cred_dur$ratio_alto)
plot(km_ratio, lwd=2, lty=c(1,2), col=gray(c(0.2,0.5)),
     xlab="Meses", ylab="S(t)", main="KM: ratio deuda/ingreso", conf.int=FALSE)
legend("bottomleft", c("Ratio bajo","Ratio alto"),
       lty=c(1,2), lwd=2, col=gray(c(0.2,0.5)), cex=0.8, bty="n")
dev.off()
cat("\n  Gráfico: output/T07_CP03_eda.png\n")
cat("  Los créditos con ratio alto impagan más rápido.\n")
pausa()

# --- COX ---
cat("\n--- MODELO DE COX ---\n\n")
cox <- coxph(surv_obj ~ importe + tipo_interes + ratio_deuda_ingreso +
             historial_credito + hipoteca, data=cred_dur)
s <- summary(cox)
cat(sprintf("  %-20s %9s %9s %8s %10s\n", "Variable","HR","Coef.","EE","p-valor"))
cat(sprintf("  %s\n", paste(rep("-",62), collapse="")))
for (i in seq_len(nrow(s$coef))) {
  nm <- rownames(s$coef)[i]
  sig <- ifelse(s$coef[i,5]<0.001,"***",ifelse(s$coef[i,5]<0.01,"**",
               ifelse(s$coef[i,5]<0.05,"*","   ")))
  cat(sprintf("  %-20s %9.4f %9.4f %8.4f %10.6f %s\n",
              nm, s$coef[i,2], s$coef[i,1], s$coef[i,3], s$coef[i,5], sig))
}
cat(sprintf("\n  Concordancia: %.3f\n", s$concordance[1]))
cat(sprintf("\n  Ratio deuda/ingreso: HR=%.3f → el principal predictor de impago.\n",
            s$coef["ratio_deuda_ingreso",2]))
cat(sprintf("  Historial: HR=%.3f → mejor historial REDUCE el riesgo.\n",
            s$coef["historial_credito",2]))
pausa()

# --- PROPORCIONALIDAD ---
cat("\n--- PROPORCIONALIDAD (SCHOENFELD) ---\n\n")
zph <- cox.zph(cox)
for (i in seq_len(nrow(zph$table))) {
  cat(sprintf("  %-20s p = %.4f %s\n", rownames(zph$table)[i],
              zph$table[i,"p"], ifelse(zph$table[i,"p"]>0.05,"✓","")))
}
pausa()

# --- WEIBULL ---
cat("\n--- WEIBULL ---\n\n")
dur_pos <- pmax(cred_dur$meses_hasta_evento, 0.01)
wei <- survreg(Surv(dur_pos, cred_dur$impago) ~ importe + tipo_interes +
               ratio_deuda_ingreso + historial_credito + hipoteca,
               data=cred_dur, dist="weibull")
cat(sprintf("  Shape: %.3f | AIC: %.1f\n", 1/wei$scale, AIC(wei)))
pausa()

cat("\n================================================================\n")
cat("  RESUMEN CP03 — Impago Crediticio\n")
cat("================================================================\n")
cat(sprintf("  Concordancia Cox: %.3f\n", s$concordance[1]))
cat("  Alto ratio deuda/ingreso = principal factor de riesgo.\n")
cat("  Buen historial crediticio = factor protector.\n")
cat("  Modelo con alta censura (67%) — típico de credit scoring.\n")
cat("================================================================\n")
