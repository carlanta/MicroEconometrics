# =============================================================================
# TEMA 08 — CP2: Satisfacción Laboral (Probit/Logit Ordenado)
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================
pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
if (!requireNamespace("MASS", quietly=TRUE)) install.packages("MASS", quiet=TRUE)
suppressPackageStartupMessages(library(MASS))
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
cat("  CP02 — Satisfacción Laboral (Probit/Logit Ordenado)\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR, "T08_CP02_satisfaccion_laboral.RData"))
satisf_lab$satisfaccion <- factor(satisf_lab$satisfaccion, ordered=TRUE)
cat("Dataset:", nrow(satisf_lab), "trabajadores\n")
print(table(satisf_lab$satisfaccion))
cat("\n")

# --- DESCRIPTIVOS ---
cat("--- ESTADÍSTICOS DESCRIPTIVOS ---\n\n")
vars <- c("salario","horas_semanales","autonomia","antiguedad")
labs <- c("Salario (miles)","Horas semanales","Autonomía (1-10)","Antigüedad")
cat(sprintf("  %-16s %8s %8s %8s %8s\n", "Variable","Media","D.E.","Mín.","Máx."))
cat(sprintf("  %s\n", paste(rep("-",54), collapse="")))
for (i in seq_along(vars)) {
  x <- satisf_lab[[vars[i]]]
  cat(sprintf("  %-16s %8.2f %8.2f %8.1f %8.1f\n", labs[i],
              mean(x), sd(x), min(x), max(x)))
}
pausa()

# --- EDA ---
png(file.path(OUTPUT_DIR, "T08_CP02_eda.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)
barplot(table(satisf_lab$satisfaccion), col=gray(seq(0.2,0.8,length.out=5)),
        border="white", main="Distribución satisfacción",
        ylab="Frecuencia", names.arg=1:5)
boxplot(salario ~ satisfaccion, data=satisf_lab,
        col=gray(seq(0.2,0.8,length.out=5)), border=gray(0.2),
        main="Salario por nivel", ylab="Salario", xlab="Satisfacción")
dev.off()
cat("\n  Gráfico: output/T08_CP02_eda.png\n")
cat("  Mayor satisfacción se asocia con mayor salario.\n")
pausa()

# --- PROBIT ORDENADO ---
cat("\n--- PROBIT ORDENADO ---\n\n")
op <- polr(satisfaccion ~ salario + horas_semanales + autonomia + antiguedad,
           data=satisf_lab, method="probit")
s_op <- summary(op)
ct <- s_op$coefficients
cat(sprintf("  %-16s %9s %8s %8s %10s\n", "Variable","Coef.","EE","t","p-valor"))
cat(sprintf("  %s\n", paste(rep("-",56), collapse="")))
for (i in seq_len(nrow(ct))) {
  pv <- 2*pnorm(-abs(ct[i,"t value"]))
  sig <- ifelse(pv<0.001,"***",ifelse(pv<0.01,"**",ifelse(pv<0.05,"*","   ")))
  cat(sprintf("  %-16s %9.4f %8.4f %8.2f %10.6f %s\n",
              rownames(ct)[i], ct[i,1], ct[i,2], ct[i,3], pv, sig))
}
cat(sprintf("\n  Umbrales: %s\n", paste(sprintf("%.3f", op$zeta), collapse=" | ")))
cat("\n  INTERPRETACIÓN:\n")
cat("  Salario (+) y autonomía (+) aumentan la satisfacción.\n")
cat("  Horas (-) la reduce. Antigüedad tiene efecto menor.\n")
pausa()

# --- LOGIT ORDENADO ---
cat("\n--- LOGIT ORDENADO ---\n\n")
ol <- polr(satisfaccion ~ salario + horas_semanales + autonomia + antiguedad,
           data=satisf_lab, method="logistic")
ct2 <- summary(ol)$coefficients
cat(sprintf("  %-16s %9s %8s %10s\n", "Variable","Coef.","EE","p-valor"))
cat(sprintf("  %s\n", paste(rep("-",48), collapse="")))
for (i in seq_len(nrow(ct2))) {
  pv <- 2*pnorm(-abs(ct2[i,"t value"]))
  cat(sprintf("  %-16s %9.4f %8.4f %10.6f\n",
              rownames(ct2)[i], ct2[i,1], ct2[i,2], pv))
}
cat("\n  Los coefs. Logit son ~1.7× los del Probit (ratio teórico ~1.8).\n")
cat("  Ambos modelos dan conclusiones cualitativas idénticas.\n")
pausa()

# --- CLASIFICACIÓN ---
cat("\n--- BONDAD DEL AJUSTE ---\n\n")
pred_op <- predict(op)
tab_op <- table(Obs=satisf_lab$satisfaccion, Pred=pred_op)
cat("  Tabla de clasificación (Probit):\n")
print(tab_op)
cat(sprintf("\n  %% correcto: %.1f%%\n", 100*sum(diag(tab_op))/sum(tab_op)))

cat("\n================================================================\n")
cat("  RESUMEN CP02\n")
cat("================================================================\n")
cat("  Salario y autonomía son los predictores más fuertes.\n")
cat("  El signo de beta no siempre coincide con el AME en cat. intermedias.\n")
cat("================================================================\n")
