# =============================================================================
# TEMA 06 — CP2: IV — Gasto Público y Crecimiento
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Estimar el efecto del gasto público sobre el crecimiento del PIB.
#           La calidad institucional (omitida) genera endogeneidad.
#           Instrumento: gasto en defensa de países vecinos.
#           Identificación exacta → Sargan NO aplicable.
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
pkgs <- c("AER","lmtest","sandwich")
for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, quiet=TRUE)
suppressPackageStartupMessages({ library(AER); library(lmtest); library(sandwich) })
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
cat("  CP02 — Gasto Público y Crecimiento (IV con panel)\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR, "T06_CP02_crecimiento_paises.RData"))
cat("Dataset:", nrow(crec_panel), "obs |",
    length(unique(crec_panel$id_pais)), "países ×",
    length(unique(crec_panel$anio)), "años\n\n")

# --- DESCRIPTIVOS ---
cat("--- ESTADÍSTICOS DESCRIPTIVOS ---\n\n")
vars <- c("crecimiento_pib","gasto_publico_pct","gasto_defensa_vecinos")
labs <- c("Crecimiento PIB %","Gasto público %PIB","Gasto def. vecinos")
cat(sprintf("  %-20s %8s %8s %8s %8s\n", "Variable","Media","D.E.","Mín.","Máx."))
cat(sprintf("  %s\n", paste(rep("-",58), collapse="")))
for (i in seq_along(vars)) {
  x <- crec_panel[[vars[i]]]
  cat(sprintf("  %-20s %8.2f %8.2f %8.2f %8.2f\n", labs[i],
              mean(x), sd(x), min(x), max(x)))
}
cat("\n  NOTA: El instrumento es el gasto en defensa de países vecinos.\n")
cat("  La presión geopolítica fuerza gasto público, pero no afecta\n")
cat("  directamente al crecimiento del país instrumentado.\n")
pausa()

# --- EDA ---
cat("\n--- ANÁLISIS EXPLORATORIO GRÁFICO ---\n\n")
png(file.path(OUTPUT_DIR, "T06_CP02_eda.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)

plot(crec_panel$gasto_publico_pct, crec_panel$crecimiento_pib,
     pch=1, col=gray(0.5), cex=0.3,
     xlab="Gasto público (%PIB)", ylab="Crecimiento PIB (%)",
     main="Crec. vs Gasto público")
abline(lm(crecimiento_pib ~ gasto_publico_pct, data=crec_panel), lwd=2, lty=1)
text(15, 12, "MCO (sesgado)", cex=0.7, font=3)

plot(crec_panel$gasto_defensa_vecinos, crec_panel$gasto_publico_pct,
     pch=1, col=gray(0.5), cex=0.3,
     xlab="Gasto defensa vecinos", ylab="Gasto público (%PIB)",
     main="1a etapa: relevancia")
abline(lm(gasto_publico_pct ~ gasto_defensa_vecinos, data=crec_panel), lwd=2, lty=1)
r2_1 <- summary(lm(gasto_publico_pct ~ gasto_defensa_vecinos, data=crec_panel))$r.squared
text(0, 28, sprintf("R² = %.3f", r2_1), cex=0.8, font=2)
dev.off()
cat("  Gráfico: output/T06_CP02_eda.png\n")
pausa()

# --- MCO vs IV ---
cat("\n--- ESTIMACIÓN MCO vs MC2E ---\n\n")
mco <- lm(crecimiento_pib ~ gasto_publico_pct, data=crec_panel)
iv  <- ivreg(crecimiento_pib ~ gasto_publico_pct | gasto_defensa_vecinos,
             data=crec_panel)
s_iv <- summary(iv, diagnostics=TRUE)

cat(sprintf("  %-20s | %9s %8s | %9s %8s\n", "","MCO","EE","IV","EE"))
cat(sprintf("  %s\n", paste(rep("-",58), collapse="")))
cat(sprintf("  %-20s | %9.4f %8.4f | %9.4f %8.4f\n", "Gasto público",
            coef(summary(mco))["gasto_publico_pct",1],
            coef(summary(mco))["gasto_publico_pct",2],
            coef(s_iv)["gasto_publico_pct",1],
            coef(s_iv)["gasto_publico_pct",2]))

cat(sprintf("\n  MCO: +1pp gasto público → +%.2f pp crecimiento\n",
            coef(mco)["gasto_publico_pct"]))
cat(sprintf("  IV:  +1pp gasto público → +%.2f pp crecimiento\n",
            coef(iv)["gasto_publico_pct"]))
cat("\n  MCO sobreestima porque la calidad institucional (omitida)\n")
cat("  afecta positivamente a AMBOS: gasto público y crecimiento.\n")
pausa()

# --- DIAGNÓSTICOS ---
cat("\n--- DIAGNÓSTICOS ---\n\n")
f1 <- summary(lm(gasto_publico_pct ~ gasto_defensa_vecinos,
                  data=crec_panel))$fstatistic
cat(sprintf("  F primera etapa: %.2f", f1[1]))
if (f1[1] > 10) cat(" ✓ Instrumento fuerte.\n\n") else cat(" ✗ Débil.\n\n")

diag_iv <- s_iv$diagnostics
if ("Wu-Hausman" %in% rownames(diag_iv)) {
  cat(sprintf("  Wu-Hausman: F = %.3f | p = %.4f\n",
              diag_iv["Wu-Hausman",1], diag_iv["Wu-Hausman",4]))
  if (diag_iv["Wu-Hausman",4] < 0.05) {
    cat("  ✓ Gasto público ES endógeno. IV necesario.\n\n")
  } else {
    cat("  ✗ No se rechaza exogeneidad.\n\n")
  }
}
cat("  NOTA: Con identificación exacta (1 instrumento, 1 endógena),\n")
cat("  el test de Sargan NO es aplicable. No podemos contrastar la\n")
cat("  validez del instrumento — debemos confiar en el argumento teórico.\n")
pausa()

# --- RESUMEN ---
cat("\n================================================================\n")
cat("  RESUMEN CP02 — Gasto Público y Crecimiento\n")
cat("================================================================\n\n")
cat(sprintf("  MCO: coef = %.4f | IV: coef = %.4f\n",
            coef(mco)["gasto_publico_pct"], coef(iv)["gasto_publico_pct"]))
cat(sprintf("  F primera etapa: %.0f (instrumento fuerte)\n", f1[1]))
cat("  El gasto en defensa de los vecinos es exógeno al crecimiento\n")
cat("  del país instrumentado (argumento geopolítico).\n")
cat("================================================================\n")
