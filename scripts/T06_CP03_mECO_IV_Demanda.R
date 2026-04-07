# =============================================================================
# TEMA 06 — CP3: IV — Estimación de la Curva de Demanda
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Estimar la elasticidad-precio de la demanda en presencia de
#           simultaneidad (precio endógeno). Instrumentos: coste transporte
#           y clima (desplazadores de oferta). Sobreidentificado → Sargan.
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
cat("  CP03 — Curva de Demanda: Simultaneidad Oferta-Demanda\n")
cat("================================================================\n\n")
cat("  En el equilibrio de mercado, precio y cantidad se determinan\n")
cat("  conjuntamente. Un shock de demanda sube precio Y cantidad,\n")
cat("  haciendo que MCO SUBESTIME la pendiente negativa de la demanda.\n")
cat("  Instrumentamos el precio con desplazadores de oferta.\n\n")

load(file.path(DATA_DIR, "T06_CP03_oferta_demanda.RData"))
cat("Dataset:", nrow(mercado_iv), "observaciones de mercado\n\n")

# --- DESCRIPTIVOS ---
cat("--- ESTADÍSTICOS DESCRIPTIVOS ---\n\n")
vars <- c("cantidad","precio","ingreso","coste_transporte","clima")
labs <- c("Cantidad","Precio","Ingreso","Coste transporte","Clima")
cat(sprintf("  %-16s %8s %8s %8s %8s\n", "Variable","Media","D.E.","Mín.","Máx."))
cat(sprintf("  %s\n", paste(rep("-",54), collapse="")))
for (i in seq_along(vars)) {
  x <- mercado_iv[[vars[i]]]
  cat(sprintf("  %-16s %8.2f %8.2f %8.2f %8.2f\n", labs[i],
              mean(x), sd(x), min(x), max(x)))
}
pausa()

# --- EDA ---
cat("\n--- ANÁLISIS EXPLORATORIO GRÁFICO ---\n\n")
png(file.path(OUTPUT_DIR, "T06_CP03_eda.png"), width=2000, height=700, res=150)
par(mfrow=c(1,3), mar=c(4.5,4.5,2.5,0.5), cex.main=0.82)

plot(mercado_iv$precio, mercado_iv$cantidad, pch=1, col=gray(0.5), cex=0.5,
     xlab="Precio", ylab="Cantidad", main="Cantidad vs Precio")
abline(lm(cantidad ~ precio, data=mercado_iv), lwd=2, lty=1)
text(5, 45, "MCO (sesgo simult.)", cex=0.7, font=3)

plot(mercado_iv$coste_transporte, mercado_iv$precio,
     pch=1, col=gray(0.5), cex=0.5,
     xlab="Coste transporte", ylab="Precio",
     main="1a etapa: coste transp.")
abline(lm(precio ~ coste_transporte, data=mercado_iv), lwd=2, lty=1)

plot(mercado_iv$clima, mercado_iv$precio,
     pch=1, col=gray(0.5), cex=0.5,
     xlab="Clima", ylab="Precio",
     main="1a etapa: clima")
abline(lm(precio ~ clima, data=mercado_iv), lwd=2, lty=1)
dev.off()
cat("  Gráfico: output/T06_CP03_eda.png\n")
cat("  Ambos instrumentos (coste, clima) correlacionan con el precio.\n")
pausa()

# --- MCO vs IV ---
cat("\n--- ESTIMACIÓN: MCO vs MC2E ---\n\n")
mco <- lm(cantidad ~ precio + ingreso, data=mercado_iv)
iv  <- ivreg(cantidad ~ precio + ingreso | coste_transporte + clima + ingreso,
             data=mercado_iv)
s_iv <- summary(iv, diagnostics=TRUE)

cat(sprintf("  %-12s | %9s %8s | %9s %8s\n", "","MCO","EE","IV","EE"))
cat(sprintf("  %s\n", paste(rep("-",52), collapse="")))
for (v in c("precio","ingreso")) {
  cat(sprintf("  %-12s | %9.4f %8.4f | %9.4f %8.4f\n", v,
              coef(summary(mco))[v,1], coef(summary(mco))[v,2],
              coef(s_iv)[v,1], coef(s_iv)[v,2]))
}
cat(sprintf("\n  MCO: elasticidad-precio = %.4f\n", coef(mco)["precio"]))
cat(sprintf("  IV:  elasticidad-precio = %.4f\n", coef(iv)["precio"]))
cat("\n  MCO subestima la pendiente negativa (en valor absoluto)\n")
cat("  porque la simultaneidad genera sesgo positivo.\n")
pausa()

# --- DIAGNÓSTICOS ---
cat("\n--- DIAGNÓSTICOS ---\n\n")
f1 <- summary(lm(precio ~ coste_transporte + clima + ingreso,
                  data=mercado_iv))$fstatistic
cat(sprintf("  F primera etapa: %.2f", f1[1]))
if (f1[1] > 10) cat(" ✓ Instrumentos fuertes.\n\n") else cat(" ✗ Débiles.\n\n")

diag_iv <- s_iv$diagnostics
if ("Sargan" %in% rownames(diag_iv)) {
  cat(sprintf("  Sargan (sobreidentificación): stat = %.3f | p = %.4f\n",
              diag_iv["Sargan",1], diag_iv["Sargan",4]))
  if (diag_iv["Sargan",4] > 0.05) {
    cat("  ✓ No se rechaza: AMBOS instrumentos son válidos.\n\n")
  } else {
    cat("  ✗ Se rechaza: al menos un instrumento no es exógeno.\n\n")
  }
}
if ("Wu-Hausman" %in% rownames(diag_iv)) {
  cat(sprintf("  Wu-Hausman: F = %.3f | p = %.4f\n",
              diag_iv["Wu-Hausman",1], diag_iv["Wu-Hausman",4]))
  if (diag_iv["Wu-Hausman",4] < 0.05) {
    cat("  ✓ Precio ES endógeno (simultaneidad confirmada).\n\n")
  } else {
    cat("  ✗ No se rechaza exogeneidad del precio.\n\n")
  }
}
pausa()

# --- GRÁFICO FINAL ---
png(file.path(OUTPUT_DIR, "T06_CP03_demanda.png"), width=1200, height=600, res=150)
par(mar=c(4.5, 4.5, 2.5, 1))
plot(mercado_iv$precio, mercado_iv$cantidad, pch=1, col=gray(0.6), cex=0.4,
     xlab="Precio", ylab="Cantidad demandada",
     main="Curva de demanda: MCO vs IV")
abline(mco, lwd=2.5, lty=1)
abline(iv, lwd=2.5, lty=2)
legend("topright",
       c(sprintf("MCO (pend. = %.2f)", coef(mco)["precio"]),
         sprintf("IV  (pend. = %.2f)", coef(iv)["precio"])),
       lty=c(1,2), lwd=2.5, cex=0.8, bty="n")
dev.off()
cat("  Gráfico: output/T06_CP03_demanda.png\n")
cat("  La recta IV tiene mayor pendiente negativa (en valor absoluto):\n")
cat("  refleja la verdadera respuesta de la demanda al precio.\n")
pausa()

# --- RESUMEN ---
cat("\n================================================================\n")
cat("  RESUMEN CP03 — Curva de Demanda\n")
cat("================================================================\n\n")
cat(sprintf("  MCO: elasticidad-precio = %.4f (sesgada)\n", coef(mco)["precio"]))
cat(sprintf("  IV:  elasticidad-precio = %.4f (corregida)\n", coef(iv)["precio"]))
cat(sprintf("  F primera etapa: %.0f\n", f1[1]))
cat("  La simultaneidad subestima la respuesta negativa de la demanda.\n")
cat("  Coste de transporte y clima son instrumentos válidos (Sargan).\n")
cat("================================================================\n")
