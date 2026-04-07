# =============================================================================
# TEMA 05 — Datos de Panel: Efectos Fijos, Aleatorios y Diagnósticos
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Estimar modelos de panel (Pooled, EF, EA) con datos de Grunfeld.
#           Realizar test de Hausman, diagnósticos y comparar estimadores.
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
pkgs <- c("plm","lmtest","sandwich","knitr")
for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, quiet=TRUE)
suppressPackageStartupMessages({

  library(plm); library(lmtest); library(sandwich)
})
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
OUTPUT_DIR <- normalizePath(file.path(.sdir, "..", "output"), mustWork=FALSE)
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive=TRUE)
cat("\n================================================================\n")
cat("  TEMA 05 — Datos de Panel: Grunfeld (1958)\n")
cat("================================================================\n\n")

# --- CARGA DE DATOS ---
data("Grunfeld", package="plm")
cat("Dataset de Grunfeld: inversión de", length(unique(Grunfeld$firm)),
    "empresas durante", length(unique(Grunfeld$year)), "años (",
    min(Grunfeld$year), "-", max(Grunfeld$year), ")\n")
cat("  N total:", nrow(Grunfeld), "observaciones (panel balanceado)\n\n")

cat("Variables:\n")
cat("  inv    — Inversión bruta (millones USD)\n")
cat("  value  — Valor de mercado de la empresa (millones USD)\n")
cat("  capital — Stock de capital (planta y equipo, millones USD)\n\n")

# --- DESCRIPTIVOS ---
cat("--- ESTADÍSTICOS DESCRIPTIVOS ---\n\n")
vars <- c("inv","value","capital")
cat(sprintf("  %-10s %10s %10s %10s %10s\n", "Variable", "Media", "D.E.", "Mín.", "Máx."))
cat(sprintf("  %s\n", paste(rep("-", 54), collapse="")))
for (v in vars) {
  x <- Grunfeld[[v]]
  cat(sprintf("  %-10s %10.1f %10.1f %10.1f %10.1f\n", v, mean(x), sd(x), min(x), max(x)))
}
pausa()

# --- DESCOMPOSICIÓN WITHIN/BETWEEN ---
cat("\n--- DESCOMPOSICIÓN WITHIN/BETWEEN ---\n\n")
pdata <- pdata.frame(Grunfeld, index=c("firm","year"))
for (v in vars) {
  ov <- var(Grunfeld[[v]])
  medias_i <- tapply(Grunfeld[[v]], Grunfeld$firm, mean)
  bv <- var(medias_i)
  within_vals <- Grunfeld[[v]] - rep(medias_i, each=length(unique(Grunfeld$year)))
  wv <- var(within_vals)
  cat(sprintf("  %-8s  Total: %10.1f | Between: %10.1f | Within: %10.1f\n", v, ov, bv, wv))
}
cat("\n  La variación between mide diferencias entre empresas.\n")
cat("  La variación within mide cambios en el tiempo de cada empresa.\n")
pausa()

# --- GRÁFICO EDA ---
cat("\n--- GRÁFICO: INVERSIÓN POR EMPRESA A LO LARGO DEL TIEMPO ---\n\n")
firmas <- unique(Grunfeld$firm)
ltys <- rep(1:4, length.out=length(firmas))
pchs <- rep(c(16,1,17,2,15,0,18,3,4,8), length.out=length(firmas))

png(file.path(OUTPUT_DIR, "T05_panel_eda.png"), width=1600, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)

# Panel 1: series temporales por empresa
plot(NA, xlim=range(Grunfeld$year), ylim=range(Grunfeld$inv),
     xlab="Año", ylab="Inversión (mill. USD)", main="Inversión por empresa")
for (i in seq_along(firmas)) {
  sub <- Grunfeld[Grunfeld$firm==firmas[i],]
  lines(sub$year, sub$inv, lty=ltys[i], col=gray(0.1+0.07*i), lwd=1.3)
  points(sub$year, sub$inv, pch=pchs[i], col=gray(0.1+0.07*i), cex=0.5)
}

# Panel 2: within vs between
medias_inv <- tapply(Grunfeld$inv, Grunfeld$firm, mean)
plot(medias_inv, xlab="Empresa (ordenada)", ylab="Inversión media",
     main="Variación between", pch=16, col=gray(0.3), cex=1.3)
segments(1:10, medias_inv-15, 1:10, medias_inv+15, lwd=2, col=gray(0.6))
abline(h=mean(Grunfeld$inv), lty=2, lwd=1.5)
text(8, mean(Grunfeld$inv)+20, "Media global", cex=0.75, font=3)
dev.off()
cat("  Gráfico guardado: output/T05_panel_eda.png\n")
pausa()

# --- ESTIMACIÓN: POOLED, EF, EA ---
cat("\n--- ESTIMACIÓN DE MODELOS ---\n\n")
fml <- inv ~ value + capital
mod_pooled <- plm(fml, data=pdata, model="pooling")
mod_fe     <- plm(fml, data=pdata, model="within")
mod_re     <- plm(fml, data=pdata, model="random")

vars_m <- c("value","capital")
labs_m <- c("Valor mercado","Stock capital")
ct_p  <- coef(summary(mod_pooled))
ct_fe <- coef(summary(mod_fe))
ct_re <- coef(summary(mod_re))

cat(sprintf("  %-14s | %9s %8s | %9s %8s | %9s %8s\n",
            "Variable", "Pooled","EE","EF","EE","EA","EE"))
cat(sprintf("  %s\n", paste(rep("-",72), collapse="")))
for (i in seq_along(vars_m)) {
  v <- vars_m[i]
  cat(sprintf("  %-14s | %9.4f %8.4f | %9.4f %8.4f | %9.4f %8.4f\n",
              labs_m[i], ct_p[v,1], ct_p[v,2], ct_fe[v,1], ct_fe[v,2],
              ct_re[v,1], ct_re[v,2]))
}

cat("\n  INTERPRETACIÓN:\n")
cat("  El coeficiente de 'value' en EF indica que un aumento de 1 millón\n")
cat("  USD en el valor de mercado se asocia con un aumento de",
    sprintf("%.4f", coef(mod_fe)["value"]), "millones\n")
cat("  en inversión, controlando por el efecto fijo de cada empresa.\n")
pausa()

# --- TEST DE EFECTOS INDIVIDUALES (F-test) ---
cat("\n--- TEST F: ¿EXISTEN EFECTOS INDIVIDUALES? ---\n\n")
f_test <- pFtest(mod_fe, mod_pooled)
cat(sprintf("  H0: No hay efectos individuales (Pooled OLS es suficiente)\n"))
cat(sprintf("  H1: Existen efectos individuales significativos\n\n"))
cat(sprintf("  Estadístico F: %.2f\n", f_test$statistic))
cat(sprintf("  p-valor:       %.6f\n\n", f_test$p.value))
if (f_test$p.value < 0.05) {
  cat("  ✓ Se RECHAZA H0 (p<0.05). Los efectos individuales son significativos.\n")
  cat("    Pooled OLS NO es adecuado. Debemos usar EF o EA.\n\n")
} else {
  cat("  ✗ No se rechaza H0. Pooled OLS podría ser suficiente.\n\n")
}
pausa()

# --- TEST LM BREUSCH-PAGAN ---
cat("\n--- TEST LM DE BREUSCH-PAGAN ---\n\n")
bp_test <- plmtest(mod_pooled, type="bp")
cat(sprintf("  H0: sigma_alpha² = 0 (no hay heterogeneidad individual)\n"))
cat(sprintf("  H1: sigma_alpha² > 0\n\n"))
cat(sprintf("  Estadístico chi²: %.2f\n", bp_test$statistic))
cat(sprintf("  p-valor:          %.6f\n\n", bp_test$p.value))
if (bp_test$p.value < 0.05) {
  cat("  ✓ Se RECHAZA H0: existe heterogeneidad individual no observada.\n\n")
}
pausa()

# --- TEST DE HAUSMAN ---
cat("\n--- TEST DE HAUSMAN: EF vs EA ---\n\n")
hausman <- phtest(mod_fe, mod_re)
cat(sprintf("  H0: Efectos Aleatorios es consistente y eficiente\n"))
cat(sprintf("      (alpha_i NO correlacionado con x_it)\n"))
cat(sprintf("  H1: Solo Efectos Fijos es consistente\n"))
cat(sprintf("      (alpha_i SÍ correlacionado con x_it)\n\n"))
cat(sprintf("  Estadístico H: %.2f\n", hausman$statistic))
cat(sprintf("  g.l.:          %d\n", hausman$parameter))
cat(sprintf("  p-valor:       %.4f\n\n", hausman$p.value))
if (hausman$p.value < 0.05) {
  cat("  ✓ Se RECHAZA H0 (p<0.05). EFECTOS FIJOS es el estimador correcto.\n")
  cat("    Los efectos individuales están correlacionados con los regresores.\n\n")
} else {
  cat("  ✗ No se rechaza H0. EFECTOS ALEATORIOS es adecuado y más eficiente.\n\n")
}
pausa()

# --- TEST DE AUTOCORRELACIÓN (WOOLDRIDGE) ---
cat("\n--- TEST DE WOOLDRIDGE: AUTOCORRELACIÓN SERIAL ---\n\n")
wo_test <- pwartest(mod_fe)
cat(sprintf("  H0: No hay autocorrelación de primer orden en los errores\n"))
cat(sprintf("  H1: Existe autocorrelación AR(1)\n\n"))
cat(sprintf("  Estadístico F: %.3f\n", wo_test$statistic))
cat(sprintf("  p-valor:       %.4f\n\n", wo_test$p.value))
if (wo_test$p.value < 0.05) {
  cat("  ✓ Se RECHAZA H0: hay autocorrelación serial.\n")
  cat("    Es imprescindible usar errores clustered.\n\n")
} else {
  cat("  ✗ No se rechaza H0. No hay evidencia de autocorrelación serial.\n\n")
}
pausa()

# --- ERRORES CLUSTERED ---
cat("\n--- EFECTOS FIJOS CON ERRORES CLUSTERED (HC1) ---\n\n")
rob_fe <- coeftest(mod_fe, vcov=vcovHC(mod_fe, type="HC1", cluster="group"))
cat(sprintf("  %-14s | %9s %8s %8s %10s\n", "Variable","Coef.","EE rob.","t","p-valor"))
cat(sprintf("  %s\n", paste(rep("-",56), collapse="")))
for (v in vars_m) {
  sig <- ifelse(rob_fe[v,4]<0.001,"***",ifelse(rob_fe[v,4]<0.01,"**",
               ifelse(rob_fe[v,4]<0.05,"*","   ")))
  cat(sprintf("  %-14s | %9.4f %8.4f %8.3f %10.6f %s\n",
              v, rob_fe[v,1], rob_fe[v,2], rob_fe[v,3], rob_fe[v,4], sig))
}
cat("\n  Los errores robustos clustered por empresa corrigen por\n")
cat("  heterocedasticidad y autocorrelación dentro de cada empresa.\n")
pausa()

# --- EFECTOS FIJOS INDIVIDUALES ---
cat("\n--- EFECTOS FIJOS INDIVIDUALES ESTIMADOS ---\n\n")
ef <- fixef(mod_fe)
cat(sprintf("  %-10s  %10s\n", "Empresa", "alpha_i"))
cat(sprintf("  %s\n", paste(rep("-",24), collapse="")))
for (i in seq_along(ef)) {
  cat(sprintf("  %-10s  %10.2f\n", names(ef)[i], ef[i]))
}

png(file.path(OUTPUT_DIR, "T05_efectos_fijos.png"), width=1200, height=600, res=150)
par(mar=c(5,5,2.5,1))
bp <- barplot(sort(ef), col=gray(seq(0.3,0.8,length.out=length(ef))),
              border="white", las=2, ylab="Efecto fijo estimado",
              main="Efectos individuales (Grunfeld)", cex.names=0.8)
abline(h=0, col=gray(0.4), lty=2, lwd=1.5)
dev.off()
cat("\n  Gráfico guardado: output/T05_efectos_fijos.png\n")
cat("\n  Cada barra representa el nivel base de inversión de cada empresa,\n")
cat("  una vez descontado el efecto de las covariables.\n")
pausa()

# --- GRÁFICO: COMPARACIÓN POOLED vs EF ---
cat("\n--- GRÁFICO: POOLED OLS vs EFECTOS FIJOS ---\n\n")
png(file.path(OUTPUT_DIR, "T05_pooled_vs_fe.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)

# Pooled
plot(Grunfeld$value, Grunfeld$inv, pch=1, col=gray(0.6), cex=0.6,
     xlab="Valor de mercado", ylab="Inversión", main="Pooled OLS")
abline(coef(mod_pooled)[1], coef(mod_pooled)["value"], lwd=2, lty=1)

# EF (una recta por empresa)
plot(Grunfeld$value, Grunfeld$inv, pch=1, col=gray(0.6), cex=0.6,
     xlab="Valor de mercado", ylab="Inversión", main="Efectos Fijos")
for (i in seq_along(firmas)) {
  abline(ef[i], coef(mod_fe)["value"], lty=ltys[i], col=gray(0.3+0.04*i), lwd=0.8)
}
dev.off()
cat("  Gráfico guardado: output/T05_pooled_vs_fe.png\n")
cat("  En el panel EF, cada empresa tiene su propio intercepto pero la\n")
cat("  pendiente es común: el efecto de 'value' sobre 'inv' es el mismo.\n")
pausa()

# --- RESUMEN FINAL ---
cat("\n================================================================\n")
cat("  RESUMEN DEL ANÁLISIS\n")
cat("================================================================\n\n")
cat("  1. Los efectos individuales son significativos (F-test y BP-LM).\n")
cat("     Pooled OLS NO es adecuado.\n\n")
cat("  2. El test de Hausman indica que", ifelse(hausman$p.value < 0.05,
    "EFECTOS FIJOS es el estimador correcto.", "EFECTOS ALEATORIOS es adecuado."), "\n\n")
cat("  3. Los coeficientes EF con errores clustered:\n")
cat(sprintf("     value:   %.4f (EE=%.4f) → +1M USD en valor → +%.2f miles en inversión\n",
            rob_fe["value",1], rob_fe["value",2], rob_fe["value",1]*1000))
cat(sprintf("     capital: %.4f (EE=%.4f) → +1M USD en capital → +%.2f miles en inversión\n",
            rob_fe["capital",1], rob_fe["capital",2], rob_fe["capital",1]*1000))
cat("\n  4. Las 10 empresas tienen interceptos muy distintos, reflejando\n")
cat("     la heterogeneidad no observada (tamaño, sector, cultura).\n\n")
cat("================================================================\n")
cat("  Fin del script T05\n")
cat("================================================================\n")
