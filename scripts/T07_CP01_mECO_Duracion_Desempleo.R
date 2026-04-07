# =============================================================================
# TEMA 07 — CP1: Duración del Desempleo
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Analizar la duración del desempleo de 800 individuos.
#           KM por grupos, Cox con 4 covariables, proporcionalidad, Weibull.
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
pkgs <- c("survival")
for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, quiet=TRUE)
suppressPackageStartupMessages({ library(survival) })
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
cat("  CP01 — Duración del Desempleo\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR, "T07_CP01_duracion_desempleo.RData"))
cat("Dataset:", nrow(desemp_dur), "individuos\n")
cat(sprintf("  Eventos: %d | Censurados: %d (%.0f%%)\n\n",
            sum(desemp_dur$evento), sum(1-desemp_dur$evento),
            100*mean(1-desemp_dur$evento)))

# --- DESCRIPTIVOS ---
cat("--- ESTADÍSTICOS DESCRIPTIVOS ---\n\n")
vars <- c("meses_desempleo","edad","educacion","prestacion","sexo")
labs <- c("Meses desempleo","Edad","Educación","Prestación","Sexo (mujer)")
cat(sprintf("  %-16s %8s %8s %8s %8s\n", "Variable","Media","D.E.","Mín.","Máx."))
cat(sprintf("  %s\n", paste(rep("-",54), collapse="")))
for (i in seq_along(vars)) {
  x <- desemp_dur[[vars[i]]]
  cat(sprintf("  %-16s %8.2f %8.2f %8.2f %8.2f\n", labs[i],
              mean(x), sd(x), min(x), max(x)))
}
pausa()

# --- EDA GRÁFICO ---
cat("\n--- ANÁLISIS EXPLORATORIO ---\n\n")
png(file.path(OUTPUT_DIR, "T07_CP01_eda.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)
hist(desemp_dur$meses_desempleo, breaks=30, col=gray(0.70), border="white",
     main="Distribución duración", xlab="Meses", ylab="Frecuencia")
surv_obj <- Surv(desemp_dur$meses_desempleo, desemp_dur$evento)
km_prest <- survfit(surv_obj ~ desemp_dur$prestacion)
plot(km_prest, lwd=2, lty=c(1,2), col=gray(c(0.2,0.5)),
     xlab="Meses", ylab="S(t)", main="KM por prestación", conf.int=FALSE)
legend("topright", c("Sin prestación","Con prestación"),
       lty=c(1,2), lwd=2, col=gray(c(0.2,0.5)), cex=0.8, bty="n")
abline(h=0.5, lty=3, col=gray(0.6))
dev.off()
cat("  Gráfico: output/T07_CP01_eda.png\n")
cat("  Los que cobran prestación tienen duración mayor (curva más alta).\n")
pausa()

# --- LOG-RANK ---
cat("\n--- TEST DE LOG-RANK: Prestación ---\n\n")
lr <- survdiff(surv_obj ~ desemp_dur$prestacion)
p_lr <- pchisq(lr$chisq, df=1, lower.tail=FALSE)
cat(sprintf("  Chi² = %.2f | p = %.4f\n", lr$chisq, p_lr))
if (p_lr < 0.05) cat("  ✓ Diferencia significativa.\n") else cat("  ✗ No significativa.\n")
pausa()

# --- MODELO DE COX ---
cat("\n--- MODELO DE COX ---\n\n")
cox <- coxph(surv_obj ~ edad + educacion + prestacion + sexo, data=desemp_dur)
s <- summary(cox)
cat(sprintf("  %-16s %9s %9s %8s %10s\n", "Variable","HR","Coef.","EE","p-valor"))
cat(sprintf("  %s\n", paste(rep("-",58), collapse="")))
for (i in seq_len(nrow(s$coefficients))) {
  nm <- rownames(s$coefficients)[i]
  sig <- ifelse(s$coef[i,5]<0.001,"***",ifelse(s$coef[i,5]<0.01,"**",
               ifelse(s$coef[i,5]<0.05,"*","   ")))
  cat(sprintf("  %-16s %9.4f %9.4f %8.4f %10.6f %s\n",
              nm, s$coef[i,2], s$coef[i,1], s$coef[i,3], s$coef[i,5], sig))
}
cat(sprintf("\n  Concordancia: %.3f\n", s$concordance[1]))

cat("\n  INTERPRETACIÓN:\n")
cat(sprintf("  - Educación: HR=%.3f → cada año de educación %s el riesgo\n",
            s$coef["educacion",2],
            ifelse(s$coef["educacion",2]>1,"AUMENTA","REDUCE")))
cat(sprintf("    de salir del desempleo en un %.1f%%.\n",
            abs(100*(s$coef["educacion",2]-1))))
cat(sprintf("  - Prestación: HR=%.3f → cobrar prestación %s el riesgo\n",
            s$coef["prestacion",2],
            ifelse(s$coef["prestacion",2]>1,"AUMENTA","REDUCE")))
cat(sprintf("    de salir en un %.1f%% (efecto desincentivador).\n",
            abs(100*(s$coef["prestacion",2]-1))))
pausa()

# --- PROPORCIONALIDAD ---
cat("\n--- TEST DE PROPORCIONALIDAD (SCHOENFELD) ---\n\n")
zph <- cox.zph(cox)
for (i in seq_len(nrow(zph$table))) {
  lab <- rownames(zph$table)[i]
  p <- zph$table[i,"p"]
  cat(sprintf("  %-16s p = %.4f %s\n", lab, p, ifelse(p>0.05,"✓","")))
}
pausa()

# --- WEIBULL ---
cat("\n--- MODELO WEIBULL ---\n\n")
dur_pos <- pmax(desemp_dur$meses_desempleo, 0.01)
surv_pos <- Surv(dur_pos, desemp_dur$evento)
wei <- survreg(surv_pos ~ edad + educacion + prestacion + sexo,
               data=desemp_dur, dist="weibull")
shape <- 1/wei$scale
cat(sprintf("  Shape (alpha): %.3f → hazard %s\n", shape,
            ifelse(shape>1,"CRECIENTE","DECRECIENTE")))
cat(sprintf("  AIC: %.1f\n\n", AIC(wei)))
ct_w <- summary(wei)$table
for (i in 2:(nrow(ct_w)-1)) {
  cat(sprintf("  %-16s Coef: %8.4f | p: %.4f\n",
              rownames(ct_w)[i], ct_w[i,1], ct_w[i,4]))
}
pausa()

cat("\n================================================================\n")
cat("  RESUMEN CP01 — Duración del Desempleo\n")
cat("================================================================\n\n")
cat(sprintf("  Observaciones: %d | Censurados: %.0f%%\n",
            nrow(desemp_dur), 100*mean(1-desemp_dur$evento)))
cat(sprintf("  Log-rank (prestación): p = %.4f\n", p_lr))
cat(sprintf("  Cox: concordancia = %.3f\n", s$concordance[1]))
cat(sprintf("  Weibull shape = %.3f (%s)\n", shape,
            ifelse(shape>1,"creciente","decreciente")))
cat("  La prestación alarga la duración; la educación la acorta.\n")
cat("================================================================\n")
