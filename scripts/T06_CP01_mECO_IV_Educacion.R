# =============================================================================
# TEMA 06 — CP1: IV — Retorno de la Educación
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Estimar el retorno de la educación controlando la endogeneidad
#           por habilidad no observada. Instrumentos: educ. padre y madre.
#           Modelo sobreidentificado → test de Sargan aplicable.
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
cat("  CP01 — Retorno de la Educación con IV\n")
cat("================================================================\n\n")

# --- CARGA Y DESCRIPCIÓN ---
load(file.path(DATA_DIR, "T06_CP01_retorno_educacion.RData"))
cat("Dataset:", nrow(educ_iv), "individuos\n\n")

cat("--- ESTADÍSTICOS DESCRIPTIVOS ---\n\n")
vars <- c("log_salario","anios_educacion","experiencia","educ_padre","educ_madre")
labs <- c("Log(salario)","Años educación","Experiencia","Educ. padre","Educ. madre")
cat(sprintf("  %-16s %8s %8s %8s %8s\n", "Variable","Media","D.E.","Mín.","Máx."))
cat(sprintf("  %s\n", paste(rep("-",54), collapse="")))
for (i in seq_along(vars)) {
  x <- educ_iv[[vars[i]]]
  cat(sprintf("  %-16s %8.2f %8.2f %8.1f %8.1f\n", labs[i],
              mean(x), sd(x), min(x), max(x)))
}
cat(sprintf("\n  Mujeres: %d (%.1f%%)\n",
            sum(educ_iv$sexo), 100*mean(educ_iv$sexo)))
cat(sprintf("  Regiones: %s\n", paste(names(table(educ_iv$region)), collapse=", ")))
pausa()

# --- EDA GRÁFICO ---
cat("\n--- ANÁLISIS EXPLORATORIO GRÁFICO ---\n\n")
png(file.path(OUTPUT_DIR, "T06_CP01_eda.png"), width=2000, height=700, res=150)
par(mfrow=c(1,3), mar=c(4.5,4.5,2.5,0.5), cex.main=0.82)

plot(educ_iv$anios_educacion, educ_iv$log_salario, pch=1, col=gray(0.5), cex=0.3,
     xlab="Años de educación", ylab="Log(salario)", main="Salario vs Educación")
abline(lm(log_salario ~ anios_educacion, data=educ_iv), lwd=2, lty=1)

plot(educ_iv$educ_padre, educ_iv$anios_educacion, pch=1, col=gray(0.5), cex=0.3,
     xlab="Educ. padre (años)", ylab="Educación individuo",
     main="1a etapa: padre")
abline(lm(anios_educacion ~ educ_padre, data=educ_iv), lwd=2, lty=1)

plot(educ_iv$educ_madre, educ_iv$anios_educacion, pch=1, col=gray(0.5), cex=0.3,
     xlab="Educ. madre (años)", ylab="Educación individuo",
     main="1a etapa: madre")
abline(lm(anios_educacion ~ educ_madre, data=educ_iv), lwd=2, lty=1)
dev.off()
cat("  Gráfico: output/T06_CP01_eda.png\n")
cat("  Los dos instrumentos muestran correlación positiva con educación.\n")
pausa()

# --- ESTIMACIÓN MCO ---
cat("\n--- ESTIMACIÓN MCO ---\n\n")
mco <- lm(log_salario ~ anios_educacion + experiencia + I(experiencia^2) + sexo,
           data=educ_iv)
s_mco <- summary(mco)
vv <- c("anios_educacion","experiencia","sexo")
labs_v <- c("Educación","Experiencia","Sexo (mujer)")
cat(sprintf("  %-16s | %9s %8s %8s %10s\n", "Variable","Coef.","EE","t","p-valor"))
cat(sprintf("  %s\n", paste(rep("-",58), collapse="")))
for (i in seq_along(vv)) {
  v <- vv[i]
  ct <- s_mco$coefficients
  sig <- ifelse(ct[v,4]<0.001,"***",ifelse(ct[v,4]<0.01,"**",
               ifelse(ct[v,4]<0.05,"*","   ")))
  cat(sprintf("  %-16s | %9.4f %8.4f %8.3f %10.6f %s\n",
              labs_v[i], ct[v,1], ct[v,2], ct[v,3], ct[v,4], sig))
}
cat(sprintf("\n  MCO: +1 año educ. = +%.1f%% salario\n", 100*coef(mco)["anios_educacion"]))
cat("  Este coeficiente INCLUYE el sesgo por habilidad omitida.\n")
pausa()

# --- PRIMERA ETAPA ---
cat("\n--- PRIMERA ETAPA (relevancia de los instrumentos) ---\n\n")
primera <- lm(anios_educacion ~ educ_padre + educ_madre + experiencia +
               I(experiencia^2) + sexo, data=educ_iv)
s_1st <- summary(primera)
cat(sprintf("  Coef. educ_padre: %.4f (p = %.4f)\n",
            coef(primera)["educ_padre"],
            s_1st$coefficients["educ_padre",4]))
cat(sprintf("  Coef. educ_madre: %.4f (p = %.4f)\n",
            coef(primera)["educ_madre"],
            s_1st$coefficients["educ_madre",4]))
cat(sprintf("  R² primera etapa: %.4f\n", s_1st$r.squared))
cat(sprintf("  F global: %.2f\n", s_1st$fstatistic[1]))
cat("\n  Ambos instrumentos son significativos individualmente.\n")
pausa()

# --- ESTIMACIÓN IV / MC2E ---
cat("\n--- ESTIMACIÓN IV (MC2E) ---\n\n")
iv <- ivreg(log_salario ~ anios_educacion + experiencia + I(experiencia^2) + sexo |
             educ_padre + educ_madre + experiencia + I(experiencia^2) + sexo,
             data=educ_iv)
s_iv <- summary(iv, diagnostics=TRUE)
ct_iv <- s_iv$coefficients

cat(sprintf("  %-16s | %9s %8s | %9s %8s\n", "Variable","MCO","EE","IV","EE"))
cat(sprintf("  %s\n", paste(rep("-",56), collapse="")))
for (i in seq_along(vv)) {
  v <- vv[i]
  cat(sprintf("  %-16s | %9.4f %8.4f | %9.4f %8.4f\n",
              labs_v[i], s_mco$coef[v,1], s_mco$coef[v,2],
              ct_iv[v,1], ct_iv[v,2]))
}
cat(sprintf("\n  IV: +1 año educ. = +%.1f%% salario (vs MCO: %.1f%%)\n",
            100*coef(iv)["anios_educacion"],
            100*coef(mco)["anios_educacion"]))
sesgo_pct <- 100*(coef(mco)["anios_educacion"] - coef(iv)["anios_educacion"])/
             coef(iv)["anios_educacion"]
cat(sprintf("  MCO sobreestima el retorno en un %.0f%%.\n", abs(sesgo_pct)))
pausa()

# --- DIAGNÓSTICOS ---
cat("\n--- DIAGNÓSTICOS IV ---\n\n")
diag_iv <- s_iv$diagnostics

cat("  1. INSTRUMENTOS DÉBILES (F primera etapa):\n")
if ("Weak instruments" %in% rownames(diag_iv)) {
  cat(sprintf("     F = %.2f (regla: F > 10)\n",
              diag_iv["Weak instruments",1]))
  if (diag_iv["Weak instruments",3] > 10)
    cat("     ✓ Instrumentos FUERTES.\n\n")
} else {
  cat(sprintf("     F primera etapa = %.2f\n", s_1st$fstatistic[1]))
  if (s_1st$fstatistic[1] > 10) cat("     ✓ Instrumentos FUERTES.\n\n")
}

cat("  2. TEST DE SARGAN (sobreidentificación):\n")
cat("     H0: Todos los instrumentos son válidos\n")
if ("Sargan" %in% rownames(diag_iv)) {
  cat(sprintf("     Estadístico: %.3f | p-valor: %.4f\n",
              diag_iv["Sargan",1], diag_iv["Sargan",4]))
  if (diag_iv["Sargan",4] > 0.05) {
    cat("     ✓ No se rechaza H0: instrumentos válidos.\n\n")
  } else {
    cat("     ✗ Se rechaza: al menos un instrumento no es válido.\n\n")
  }
}

cat("  3. TEST DE WU-HAUSMAN (endogeneidad):\n")
cat("     H0: educación es exógena (MCO suficiente)\n")
if ("Wu-Hausman" %in% rownames(diag_iv)) {
  cat(sprintf("     F = %.3f | p-valor: %.4f\n",
              diag_iv["Wu-Hausman",1], diag_iv["Wu-Hausman",4]))
  if (diag_iv["Wu-Hausman",4] < 0.05) {
    cat("     ✓ Se rechaza: educación ES endógena. IV necesario.\n\n")
  } else {
    cat("     ✗ No se rechaza. MCO podría ser suficiente.\n\n")
  }
}
pausa()

# --- GRÁFICO FINAL ---
png(file.path(OUTPUT_DIR, "T06_CP01_comparacion.png"), width=1200, height=600, res=150)
par(mar=c(5,8,2.5,1))
coefs_comp <- c(coef(mco)["anios_educacion"], coef(iv)["anios_educacion"])
ses_comp <- c(s_mco$coef["anios_educacion",2], ct_iv["anios_educacion",2])
bp <- barplot(rev(coefs_comp), horiz=TRUE, names.arg=rev(c("MCO","IV (2SLS)")),
              col=gray(c(0.4,0.7)), border="white",
              xlim=c(0, max(coefs_comp+2*ses_comp)*1.15),
              xlab="Coeficiente de educación", las=1,
              main="Sesgo de endogeneidad")
segments(rev(coefs_comp-1.96*ses_comp), bp,
         rev(coefs_comp+1.96*ses_comp), bp, lwd=2)
abline(v=0.07, lty=2, lwd=2, col=gray(0.3))
text(0.07, max(bp)+0.5, "Verdadero = 0.07", cex=0.7, pos=4)
dev.off()
cat("  Gráfico: output/T06_CP01_comparacion.png\n")
pausa()

# --- RESUMEN ---
cat("\n================================================================\n")
cat("  RESUMEN CP01 — Retorno de la Educación\n")
cat("================================================================\n\n")
cat(sprintf("  MCO: +1 año educ. = +%.1f%% salario (SESGADO)\n",
            100*coef(mco)["anios_educacion"]))
cat(sprintf("  IV:  +1 año educ. = +%.1f%% salario (CORREGIDO)\n",
            100*coef(iv)["anios_educacion"]))
cat(sprintf("  F primera etapa: %.0f (instrumentos fuertes)\n",
            s_1st$fstatistic[1]))
cat("  Los instrumentos (educ. padres) son fuertes y válidos.\n")
cat("  La habilidad omitida sesga MCO hacia arriba.\n")
cat("================================================================\n")
