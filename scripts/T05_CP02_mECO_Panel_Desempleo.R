# =============================================================================
# TEMA 05 — CP2: Panel — Desempleo Regional
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Modelar la tasa de desempleo de 50 provincias × 15 años.
#           Variable invariante en t (costa) → comparar EF vs EA.
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
cat("  CP02 — Desempleo Regional: Panel 50 provincias × 15 años\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR, "T05_CP02_desempleo_regional.RData"))
cat("Dataset:", nrow(panel_desemp), "observaciones\n")
cat("  Provincias:", length(unique(panel_desemp$id_provincia)), "\n")
cat("  Período:", min(panel_desemp$anio), "-", max(panel_desemp$anio), "\n\n")

# --- DESCRIPTIVOS ---
cat("--- ESTADÍSTICOS DESCRIPTIVOS ---\n\n")
vars <- c("tasa_desempleo","pib_pc","gasto_educacion","poblacion_activa")
labs <- c("Tasa desempleo","PIB per cápita","Gasto educación","Pob. activa")
cat(sprintf("  %-16s %10s %10s %10s %10s\n", "Variable", "Media", "D.E.", "Mín.", "Máx."))
cat(sprintf("  %s\n", paste(rep("-", 60), collapse="")))
for (i in seq_along(vars)) {
  x <- panel_desemp[[vars[i]]]
  cat(sprintf("  %-16s %10.2f %10.2f %10.2f %10.2f\n", labs[i], mean(x), sd(x), min(x), max(x)))
}
cat(sprintf("\n  Provincias costeras: %d (%.0f%%)\n",
            sum(panel_desemp$costa[!duplicated(panel_desemp$id_provincia)]),
            100*mean(panel_desemp$costa[!duplicated(panel_desemp$id_provincia)])))
pausa()

# --- EDA GRÁFICO ---
cat("\n--- ANÁLISIS EXPLORATORIO GRÁFICO ---\n\n")
png(file.path(OUTPUT_DIR, "T05_CP02_eda.png"), width=1600, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)

# Boxplot desempleo por año
medias_anio <- tapply(panel_desemp$tasa_desempleo, panel_desemp$anio, mean)
plot(as.numeric(names(medias_anio)), medias_anio, type="b", pch=16,
     col=gray(0.3), lwd=2, xlab="Año", ylab="Tasa desempleo media (%)",
     main="Evolución del desempleo")

# Desempleo vs PIB
plot(panel_desemp$pib_pc, panel_desemp$tasa_desempleo, pch=1, col=gray(0.5), cex=0.5,
     xlab="PIB per cápita (miles €)", ylab="Tasa desempleo (%)",
     main="Desempleo vs PIB pc")
abline(lm(tasa_desempleo ~ pib_pc, data=panel_desemp), lwd=2, lty=1)
dev.off()
cat("  Gráfico guardado: output/T05_CP02_eda.png\n")
pausa()

# --- ESTIMACIÓN ---
cat("\n--- ESTIMACIÓN: POOLED, EF, EA ---\n\n")
pdata <- pdata.frame(panel_desemp, index=c("id_provincia","anio"))
fml <- tasa_desempleo ~ pib_pc + gasto_educacion + poblacion_activa
mod_p  <- plm(fml, data=pdata, model="pooling")
mod_fe <- plm(fml, data=pdata, model="within")
mod_re <- plm(fml, data=pdata, model="random")

# Nota: 'costa' es invariante en t → EF no puede estimarla
fml_re <- tasa_desempleo ~ pib_pc + gasto_educacion + poblacion_activa + costa
mod_re2 <- plm(fml_re, data=pdata, model="random")

vv <- c("pib_pc","gasto_educacion","poblacion_activa")
ct_p <- coef(summary(mod_p)); ct_f <- coef(summary(mod_fe)); ct_r <- coef(summary(mod_re))
cat(sprintf("  %-16s | %9s %8s | %9s %8s | %9s %8s\n",
            "Variable","Pooled","EE","EF","EE","EA","EE"))
cat(sprintf("  %s\n", paste(rep("-",76), collapse="")))
for (v in vv) {
  cat(sprintf("  %-16s | %9.4f %8.4f | %9.4f %8.4f | %9.4f %8.4f\n",
              v, ct_p[v,1], ct_p[v,2], ct_f[v,1], ct_f[v,2], ct_r[v,1], ct_r[v,2]))
}

cat("\n  NOTA: 'costa' es invariante en el tiempo → EF la elimina.\n")
cat("  Solo EA puede estimar su efecto:\n")
ct_re2 <- coef(summary(mod_re2))
cat(sprintf("  costa (en EA): Coef. = %.4f | EE = %.4f | p = %.4f\n",
            ct_re2["costa",1], ct_re2["costa",2], ct_re2["costa",4]))
pausa()

# --- TESTS ---
cat("\n--- TEST F: EFECTOS INDIVIDUALES ---\n\n")
ft <- pFtest(mod_fe, mod_p)
cat(sprintf("  F = %.2f | p-valor = %.6f\n", ft$statistic, ft$p.value))
if (ft$p.value < 0.05)
  cat("  ✓ Efectos individuales significativos.\n\n")
pausa()

cat("--- TEST DE HAUSMAN: EF vs EA ---\n\n")
ht <- phtest(mod_fe, mod_re)
cat(sprintf("  H = %.2f | g.l. = %d | p-valor = %.4f\n",
            ht$statistic, ht$parameter, ht$p.value))
if (ht$p.value < 0.05) {
  cat("  ✓ Se rechaza H0 → EFECTOS FIJOS.\n")
  cat("    Problema: EF no estima 'costa'. Considerar Mundlak o Hausman-Taylor.\n\n")
} else {
  cat("  ✗ No se rechaza H0 → EFECTOS ALEATORIOS es adecuado.\n")
  cat("    Ventaja: EA puede estimar el efecto de 'costa'.\n\n")
}
pausa()

# --- EF CON ERRORES CLUSTERED ---
cat("--- EFECTOS FIJOS CON ERRORES CLUSTERED ---\n\n")
rob <- coeftest(mod_fe, vcov=vcovHC(mod_fe, type="HC1", cluster="group"))
cat(sprintf("  %-16s | %9s %8s %8s %10s\n", "Variable","Coef.","EE rob.","t","p-valor"))
cat(sprintf("  %s\n", paste(rep("-",58), collapse="")))
for (v in vv) {
  sig <- ifelse(rob[v,4]<0.001,"***",ifelse(rob[v,4]<0.01,"**",
               ifelse(rob[v,4]<0.05,"*","   ")))
  cat(sprintf("  %-16s | %9.4f %8.4f %8.3f %10.6f %s\n",
              v, rob[v,1], rob[v,2], rob[v,3], rob[v,4], sig))
}
pausa()

# --- EF BIDIRECCIONALES ---
cat("\n--- EFECTOS FIJOS BIDIRECCIONALES (individuo + tiempo) ---\n\n")
mod_fe2 <- plm(fml, data=pdata, model="within", effect="twoways")
ct_fe2 <- coef(summary(mod_fe2))
cat(sprintf("  %-16s | %9s %8s %10s\n", "Variable","Coef.","EE","p-valor"))
cat(sprintf("  %s\n", paste(rep("-",50), collapse="")))
for (v in vv) {
  cat(sprintf("  %-16s | %9.4f %8.4f %10.6f\n", v, ct_fe2[v,1], ct_fe2[v,2], ct_fe2[v,4]))
}
cat("\n  Los EF bidireccionales añaden dummies temporales que capturan\n")
cat("  shocks comunes a todas las provincias (crisis, cambios legislativos).\n")
pausa()

# --- GRÁFICO EFECTOS FIJOS ---
ef <- fixef(mod_fe)
png(file.path(OUTPUT_DIR, "T05_CP02_efectos_fijos.png"), width=1400, height=600, res=150)
par(mar=c(5,5,2.5,1))
barplot(sort(ef), col=gray(seq(0.25,0.85,length.out=length(ef))),
        border="white", las=2, ylab="Efecto fijo estimado",
        main="Efectos individuales — 50 provincias", cex.names=0.5)
abline(h=0, lty=2, col=gray(0.4), lwd=1.5)
dev.off()
cat("  Gráfico: output/T05_CP02_efectos_fijos.png\n")
pausa()

cat("\n================================================================\n")
cat("  RESUMEN CP02 — Desempleo Regional\n")
cat("================================================================\n\n")
cat("  1. Panel de 50 provincias × 15 años (750 obs).\n")
cat("  2. Mayor PIB pc y gasto en educación reducen el desempleo.\n")
cat("  3. 'costa' (invariante en t) solo estimable con EA.\n")
cat("  4. Si Hausman rechaza → EF, pero perdemos 'costa'.\n")
cat("  5. EF bidireccionales capturan shocks temporales comunes.\n\n")
cat("================================================================\n")
