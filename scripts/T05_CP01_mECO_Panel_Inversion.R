# =============================================================================
# TEMA 05 — CP1: Panel — Inversión Empresarial
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Modelar la inversión de 80 empresas × 10 años con datos de panel.
#           EF correlacionados con tamaño → Hausman rechazará EA.
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
pkgs <- c("plm","lmtest","sandwich")
for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, quiet=TRUE)
suppressPackageStartupMessages({ library(plm); library(lmtest); library(sandwich) })
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
cat("  CP01 — Inversión Empresarial: Panel 80 empresas × 10 años\n")
cat("================================================================\n\n")

# --- CARGA DE DATOS ---
load(file.path(DATA_DIR, "T05_CP01_inversion_empresas.RData"))
cat("Dataset:", nrow(panel_inv), "observaciones\n")
cat("  Empresas:", length(unique(panel_inv$id_empresa)), "\n")
cat("  Años:", min(panel_inv$anio), "-", max(panel_inv$anio), "\n\n")

# --- DESCRIPTIVOS ---
cat("--- ESTADÍSTICOS DESCRIPTIVOS ---\n\n")
vars <- c("inversion","ventas","stock_capital","deuda")
labs <- c("Inversión","Ventas","Stock capital","Deuda")
cat(sprintf("  %-14s %10s %10s %10s %10s\n", "Variable", "Media", "D.E.", "Mín.", "Máx."))
cat(sprintf("  %s\n", paste(rep("-", 58), collapse="")))
for (i in seq_along(vars)) {
  x <- panel_inv[[vars[i]]]
  cat(sprintf("  %-14s %10.1f %10.1f %10.1f %10.1f\n", labs[i], mean(x), sd(x), min(x), max(x)))
}
cat(sprintf("\n  Sectores: %s\n", paste(unique(panel_inv$sector), collapse=", ")))
pausa()

# --- EDA GRÁFICO ---
cat("\n--- ANÁLISIS EXPLORATORIO GRÁFICO ---\n\n")
png(file.path(OUTPUT_DIR, "T05_CP01_eda.png"), width=1600, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)

# Histograma inversión
hist(panel_inv$inversion, breaks=30, col=gray(0.70), border="white",
     main="Distribución inversión", xlab="Inversión", ylab="Frecuencia")

# Inversión vs ventas
plot(panel_inv$ventas, panel_inv$inversion, pch=1, col=gray(0.5), cex=0.5,
     xlab="Ventas", ylab="Inversión", main="Inversión vs Ventas")
abline(lm(inversion ~ ventas, data=panel_inv), lwd=2, lty=1)
dev.off()
cat("  Gráfico guardado: output/T05_CP01_eda.png\n")
pausa()

# --- DESCOMPOSICIÓN WITHIN/BETWEEN ---
cat("\n--- DESCOMPOSICIÓN WITHIN/BETWEEN ---\n\n")
for (v in vars) {
  total_v <- var(panel_inv[[v]])
  medias_i <- tapply(panel_inv[[v]], panel_inv$id_empresa, mean)
  between_v <- var(medias_i)
  within_v <- var(panel_inv[[v]] - rep(medias_i, each=10))
  cat(sprintf("  %-14s  Total: %10.1f | Between: %10.1f | Within: %10.1f\n",
              v, total_v, between_v, within_v))
}
cat("\n  La inversión tiene más variación between que within:\n")
cat("  las diferencias entre empresas dominan sobre los cambios temporales.\n")
pausa()

# --- ESTIMACIÓN ---
cat("\n--- ESTIMACIÓN: POOLED, EF, EA ---\n\n")
pdata <- pdata.frame(panel_inv, index=c("id_empresa","anio"))
fml <- inversion ~ ventas + stock_capital + deuda
mod_p  <- plm(fml, data=pdata, model="pooling")
mod_fe <- plm(fml, data=pdata, model="within")
mod_re <- plm(fml, data=pdata, model="random")

vv <- c("ventas","stock_capital","deuda")
ct_p <- coef(summary(mod_p)); ct_f <- coef(summary(mod_fe)); ct_r <- coef(summary(mod_re))
cat(sprintf("  %-14s | %9s %8s | %9s %8s | %9s %8s\n",
            "Variable","Pooled","EE","EF","EE","EA","EE"))
cat(sprintf("  %s\n", paste(rep("-",72), collapse="")))
for (v in vv) {
  cat(sprintf("  %-14s | %9.4f %8.4f | %9.4f %8.4f | %9.4f %8.4f\n",
              v, ct_p[v,1], ct_p[v,2], ct_f[v,1], ct_f[v,2], ct_r[v,1], ct_r[v,2]))
}
pausa()

# --- TESTS ---
cat("\n--- TEST F: EFECTOS INDIVIDUALES ---\n\n")
ft <- pFtest(mod_fe, mod_p)
cat(sprintf("  F = %.2f | p-valor = %.6f\n", ft$statistic, ft$p.value))
if (ft$p.value < 0.05)
  cat("  ✓ Efectos individuales significativos. Pooled OLS descartado.\n\n")
pausa()

cat("--- TEST DE HAUSMAN: EF vs EA ---\n\n")
ht <- phtest(mod_fe, mod_re)
cat(sprintf("  H = %.2f | g.l. = %d | p-valor = %.4f\n",
            ht$statistic, ht$parameter, ht$p.value))
if (ht$p.value < 0.05) {
  cat("  ✓ Se rechaza H0: EFECTOS FIJOS es el estimador correcto.\n")
  cat("    Los efectos individuales correlacionan con los regresores.\n")
  cat("    (esperado: alfa_i depende del tamaño, correlacionado con ventas)\n\n")
} else {
  cat("  ✗ No se rechaza H0: EA es adecuado.\n\n")
}
pausa()

cat("--- TEST DE WOOLDRIDGE: AUTOCORRELACIÓN ---\n\n")
wt <- pwartest(mod_fe)
cat(sprintf("  F = %.3f | p-valor = %.4f\n", wt$statistic, wt$p.value))
if (wt$p.value < 0.05) cat("  ✓ Hay autocorrelación → usar errores clustered.\n\n") else cat("  ✗ No hay evidencia de autocorrelación serial.\n\n")
pausa()

# --- EF CON ERRORES CLUSTERED ---
cat("--- EFECTOS FIJOS CON ERRORES CLUSTERED ---\n\n")
rob <- coeftest(mod_fe, vcov=vcovHC(mod_fe, type="HC1", cluster="group"))
cat(sprintf("  %-14s | %9s %8s %8s %10s\n", "Variable","Coef.","EE rob.","t","p-valor"))
cat(sprintf("  %s\n", paste(rep("-",56), collapse="")))
for (v in vv) {
  sig <- ifelse(rob[v,4]<0.001,"***",ifelse(rob[v,4]<0.01,"**",
               ifelse(rob[v,4]<0.05,"*","   ")))
  cat(sprintf("  %-14s | %9.4f %8.4f %8.3f %10.6f %s\n",
              v, rob[v,1], rob[v,2], rob[v,3], rob[v,4], sig))
}
pausa()

# --- EFECTOS FIJOS INDIVIDUALES ---
cat("\n--- EFECTOS FIJOS INDIVIDUALES (primeros 15) ---\n\n")
ef <- fixef(mod_fe)
cat(sprintf("  %-10s  %10s\n", "Empresa", "alpha_i"))
cat(sprintf("  %s\n", paste(rep("-",24), collapse="")))
for (i in 1:min(15, length(ef)))
  cat(sprintf("  %-10s  %10.2f\n", names(ef)[i], ef[i]))

png(file.path(OUTPUT_DIR, "T05_CP01_efectos_fijos.png"), width=1400, height=600, res=150)
par(mar=c(5,5,2.5,1))
barplot(sort(ef), col=gray(seq(0.25,0.85,length.out=length(ef))),
        border="white", las=2, ylab="Efecto fijo estimado",
        main="Efectos individuales — 80 empresas", cex.names=0.45)
abline(h=0, lty=2, col=gray(0.4), lwd=1.5)
dev.off()
cat("\n  Gráfico: output/T05_CP01_efectos_fijos.png\n")
pausa()

# --- RESUMEN ---
cat("\n================================================================\n")
cat("  RESUMEN CP01 — Inversión Empresarial\n")
cat("================================================================\n\n")
cat("  1. Panel de 80 empresas × 10 años (800 obs).\n")
cat("  2. Efectos individuales altamente significativos.\n")
cat("  3. Hausman rechaza EA → EFECTOS FIJOS es el estimador correcto.\n")
cat("  4. Las ventas y el stock de capital aumentan la inversión.\n")
cat("  5. La deuda tiene efecto negativo sobre la inversión.\n\n")
cat("================================================================\n")
