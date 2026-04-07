# =============================================================================
# TEMA 06 — Variables Instrumentales: MC2E y Diagnósticos
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Ilustrar el problema de endogeneidad, la estimación IV/2SLS,
#           y la batería completa de diagnósticos, con datos simulados
#           donde se conoce el valor verdadero de los parámetros.
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
OUTPUT_DIR <- normalizePath(file.path(.sdir, "..", "output"), mustWork=FALSE)
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive=TRUE)

cat("\n================================================================\n")
cat("  TEMA 06 — Variables Instrumentales\n")
cat("================================================================\n\n")

# =============================================================================
# PARTE 1: SIMULACIÓN DIDÁCTICA CON PARÁMETROS CONOCIDOS
# =============================================================================

cat("--- SIMULACIÓN: MODELO CON ENDOGENEIDAD ---\n\n")
cat("  Generamos datos donde conocemos los parámetros verdaderos:\n")
cat("    salario = 5 + 0.07*educación + 0.4*habilidad + error\n")
cat("    educación = 12 + 0.8*habilidad + ruido\n")
cat("    La habilidad NO se observa → educación es endógena.\n")
cat("    Instrumento: educ_padre (correlaciona con educación, no con error)\n\n")

set.seed(2026)
n <- 2000
habilidad <- rnorm(n)
educ_padre <- sample(6:18, n, replace=TRUE)
educacion <- 12 + 0.25*educ_padre + 0.8*habilidad + rnorm(n, 0, 1.5)
salario <- 5 + 0.07*educacion + 0.4*habilidad + rnorm(n, 0, 0.3)

cat(sprintf("  n = %d observaciones\n", n))
cat(sprintf("  beta_verdadero (educación) = 0.07\n"))
cat(sprintf("  Cor(educación, habilidad) = %.3f (endogeneidad)\n",
            cor(educacion, habilidad)))
cat(sprintf("  Cor(educ_padre, educación) = %.3f (relevancia)\n",
            cor(educ_padre, educacion)))
cat(sprintf("  Cor(educ_padre, habilidad) = %.3f (exogeneidad: ~0)\n\n",
            cor(educ_padre, habilidad)))
pausa()

# --- ANÁLISIS EXPLORATORIO GRÁFICO ---
cat("\n--- ANÁLISIS EXPLORATORIO ---\n\n")

png(file.path(OUTPUT_DIR, "T06_eda.png"), width=2000, height=700, res=150)
par(mfrow=c(1,3), mar=c(4.5,4.5,2.5,0.5), cex.main=0.82)

# Panel 1: Salario vs Educación (relación contaminada)
plot(educacion, salario, pch=1, col=gray(0.6), cex=0.3,
     xlab="Años de educación", ylab="Log(salario)",
     main="Salario vs Educación")
abline(lm(salario ~ educacion), lwd=2, lty=1)
text(9, max(salario)*0.95, "Pendiente MCO\n(sesgada)", cex=0.7)

# Panel 2: Instrumento vs Endógena (relevancia)
plot(educ_padre, educacion, pch=1, col=gray(0.6), cex=0.3,
     xlab="Educación del padre", ylab="Educación del individuo",
     main="Relevancia del instrumento")
abline(lm(educacion ~ educ_padre), lwd=2, lty=1)
r2_1st <- summary(lm(educacion ~ educ_padre))$r.squared
text(8, 18, sprintf("R² = %.3f", r2_1st), cex=0.8, font=2)

# Panel 3: Instrumento vs Habilidad (exogeneidad)
plot(educ_padre, habilidad, pch=1, col=gray(0.6), cex=0.3,
     xlab="Educación del padre", ylab="Habilidad (no observada)",
     main="Exogeneidad del instrumento")
abline(lm(habilidad ~ educ_padre), lwd=2, lty=2)
text(8, 2.5, sprintf("Cor = %.3f", cor(educ_padre, habilidad)),
     cex=0.8, font=2)
dev.off()
cat("  Gráfico EDA: output/T06_eda.png\n")
cat("  Panel 1: La relación salario-educación tiene pendiente MCO inflada.\n")
cat("  Panel 2: educ_padre predice educación (relevancia confirmada).\n")
cat("  Panel 3: educ_padre apenas correlaciona con habilidad (exogeneidad).\n")
pausa()

# =============================================================================
# PARTE 2: ESTIMACIÓN MCO vs IV
# =============================================================================

cat("\n--- ESTIMACIÓN MCO (SESGADA) ---\n\n")
mco <- lm(salario ~ educacion)
s_mco <- summary(mco)
cat(sprintf("  Coef. educación (MCO): %.4f (EE = %.4f)\n",
            coef(mco)[2], s_mco$coef[2,2]))
cat(sprintf("  R² = %.4f\n", s_mco$r.squared))
cat(sprintf("\n  INTERPRETACIÓN: MCO estima que +1 año de educación\n"))
cat(sprintf("  aumenta el salario en %.1f%%. Pero el verdadero es 7.0%%.\n",
            100*coef(mco)[2]))
cat(sprintf("  Sesgo: +%.1f%% (MCO sobreestima en un %.0f%%).\n",
            100*(coef(mco)[2] - 0.07),
            100*(coef(mco)[2] - 0.07)/0.07))
cat("  La habilidad omitida infla el coeficiente de educación.\n")
pausa()

cat("\n--- PRIMERA ETAPA DE MC2E ---\n\n")
primera <- lm(educacion ~ educ_padre)
s_1st <- summary(primera)
cat(sprintf("  educación = %.2f + %.4f × educ_padre\n",
            coef(primera)[1], coef(primera)[2]))
cat(sprintf("  R² = %.4f\n", s_1st$r.squared))
cat(sprintf("  F = %.2f (umbral Staiger-Stock: F > 10)\n",
            s_1st$fstatistic[1]))
if (s_1st$fstatistic[1] > 10) {
  cat("  ✓ INSTRUMENTO FUERTE. La primera etapa es sólida.\n")
} else {
  cat("  ✗ INSTRUMENTO DÉBIL. Los resultados IV no son fiables.\n")
}
cat(sprintf("\n  INTERPRETACIÓN: Por cada año adicional de educación del padre,\n"))
cat(sprintf("  la educación del hijo aumenta en %.2f años.\n",
            coef(primera)[2]))
pausa()

cat("\n--- ESTIMACIÓN IV / MC2E ---\n\n")
iv <- ivreg(salario ~ educacion | educ_padre)
s_iv <- summary(iv, diagnostics=TRUE)
cat(sprintf("  Coef. educación (IV): %.4f (EE = %.4f)\n",
            coef(iv)[2], s_iv$coef[2,2]))
cat(sprintf("\n  COMPARACIÓN:\n"))
cat(sprintf("    MCO:       %.4f (sesgado)\n", coef(mco)[2]))
cat(sprintf("    IV:        %.4f (corregido)\n", coef(iv)[2]))
cat(sprintf("    Verdadero: 0.0700\n"))
cat(sprintf("\n  IV se acerca mucho más al valor verdadero que MCO.\n"))
cat(sprintf("  El error estándar IV (%.4f) es mayor que MCO (%.4f):\n",
            s_iv$coef[2,2], s_mco$coef[2,2]))
cat("  este es el precio de la consistencia.\n")
pausa()

# --- GRÁFICO: MCO vs IV ---
png(file.path(OUTPUT_DIR, "T06_mco_vs_iv.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)

# Panel 1: Rectas MCO vs IV
plot(educacion, salario, pch=1, col=gray(0.6), cex=0.3,
     xlab="Educación", ylab="Log(salario)", main="MCO vs IV")
abline(mco, lwd=2.5, lty=1)
abline(iv, lwd=2.5, lty=2)
abline(a=5+0.4*mean(habilidad), b=0.07, lwd=2, lty=3, col=gray(0.4))
legend("topleft",
       c(sprintf("MCO (%.3f)", coef(mco)[2]),
         sprintf("IV  (%.3f)", coef(iv)[2]),
         "Verdadera (0.070)"),
       lty=c(1,2,3), lwd=c(2.5,2.5,2), col=c("black","black",gray(0.4)),
       cex=0.7, bty="n")

# Panel 2: Comparación de coeficientes con IC
coefs <- c(coef(mco)[2], coef(iv)[2])
ses <- c(s_mco$coef[2,2], s_iv$coef[2,2])
bp <- barplot(coefs, names.arg=c("MCO","IV"), col=gray(c(0.5,0.75)),
              border="white", ylim=c(0, max(coefs+2*ses)*1.1),
              ylab="Coeficiente de educación", main="Coefs. con IC 95%")
segments(bp, coefs-1.96*ses, bp, coefs+1.96*ses, lwd=2)
abline(h=0.07, lty=2, lwd=2, col=gray(0.3))
text(mean(bp), 0.075, "Verdadero = 0.07", cex=0.7, pos=3)
dev.off()
cat("\n  Gráfico: output/T06_mco_vs_iv.png\n")
pausa()

# =============================================================================
# PARTE 3: DIAGNÓSTICOS COMPLETOS
# =============================================================================

cat("\n--- DIAGNÓSTICOS DE LA ESTIMACIÓN IV ---\n\n")

cat("  1. TEST DE INSTRUMENTOS DÉBILES\n")
cat("     (F de la primera etapa)\n\n")
f1st <- s_1st$fstatistic[1]
cat(sprintf("     F = %.2f\n", f1st))
cat(sprintf("     Umbral Staiger-Stock: F > 10\n"))
if (f1st > 10) {
  cat(sprintf("     ✓ Instrumento FUERTE (F = %.0f >> 10).\n", f1st))
  cat("       Los resultados IV son fiables.\n\n")
} else {
  cat("     ✗ Instrumento DÉBIL. Los resultados IV pueden tener\n")
  cat("       sesgo en muestras finitas peor que MCO.\n\n")
}
pausa()

cat("  2. TEST DE ENDOGENEIDAD (Wu-Hausman)\n")
cat("     H0: educación es exógena (MCO es consistente)\n")
cat("     H1: educación es endógena (solo IV es consistente)\n\n")
# Test manual con residuos de primera etapa
res_1st <- residuals(primera)
hausman_aux <- lm(salario ~ educacion + res_1st)
p_haus <- summary(hausman_aux)$coefficients["res_1st",4]
t_haus <- summary(hausman_aux)$coefficients["res_1st",3]
cat(sprintf("     t(residuos 1ª etapa) = %.3f\n", t_haus))
cat(sprintf("     p-valor = %.4f\n", p_haus))
if (p_haus < 0.05) {
  cat("     ✓ Se RECHAZA H0: educación ES endógena.\n")
  cat("       IV es necesario. MCO está sesgado.\n\n")
} else {
  cat("     ✗ No se rechaza H0. No hay evidencia de endogeneidad.\n")
  cat("       MCO podría ser suficiente (y más eficiente).\n")
  cat("       NOTA: Sabemos que SÍ hay endogeneidad por construcción.\n")
  cat("       El test puede carecer de potencia en esta muestra.\n\n")
}
pausa()

cat("  3. ERRORES ROBUSTOS (heterocedasticidad)\n\n")
rob_iv <- coeftest(iv, vcov=vcovHC(iv, type="HC1"))
cat(sprintf("     %-12s | %9s %9s %9s %10s\n",
            "","Coef.","EE conv.","EE rob.","p rob."))
cat(sprintf("     %s\n", paste(rep("-",56), collapse="")))
cat(sprintf("     %-12s | %9.4f %9.4f %9.4f %10.6f\n",
            "educación", coef(iv)[2], s_iv$coef[2,2],
            rob_iv[2,2], rob_iv[2,4]))
cat("\n     Los errores robustos corrigen por heterocedasticidad.\n")
cat("     Si difieren mucho de los convencionales, hay heterocedasticidad.\n")
pausa()

# =============================================================================
# PARTE 4: RESUMEN Y GUÍA DE COMANDOS R
# =============================================================================

cat("\n================================================================\n")
cat("  RESUMEN DEL ANÁLISIS\n")
cat("================================================================\n\n")
cat(sprintf("  Valor verdadero:  beta_educ = 0.0700\n"))
cat(sprintf("  Estimación MCO:   beta_educ = %.4f (sesgo: +%.0f%%)\n",
            coef(mco)[2], 100*(coef(mco)[2]-0.07)/0.07))
cat(sprintf("  Estimación IV:    beta_educ = %.4f\n", coef(iv)[2]))
cat(sprintf("  F primera etapa:  %.1f (>> 10, instrumento fuerte)\n", f1st))
cat(sprintf("  Wu-Hausman p:     %.4f\n", p_haus))
cat("\n  MCO confunde el efecto de la educación con el de la habilidad.\n")
cat("  IV corrige el sesgo usando solo la variación exógena del instrumento.\n")

cat("\n\n--- GUÍA DE COMANDOS R PARA IV ---\n\n")

cat("  ivreg(y ~ x_endog + w | z + w, data)\n")
cat("    Estima IV/2SLS. A la izquierda de | la ecuación estructural;\n")
cat("    a la derecha, instrumentos + exógenas. Las exógenas van a ambos lados.\n\n")

cat("  summary(modelo_iv, diagnostics = TRUE)\n")
cat("    Muestra coeficientes, EE, y además tres diagnósticos:\n")
cat("    - Weak instruments: F de la 1ª etapa (> 10)\n")
cat("    - Wu-Hausman: test de endogeneidad (p < 0.05 → IV necesario)\n")
cat("    - Sargan: validez de instrumentos (solo con sobreidentificación)\n\n")

cat("  coeftest(modelo_iv, vcov = vcovHC(modelo_iv, type='HC1'))\n")
cat("    Errores robustos a heterocedasticidad para IV.\n\n")

cat("================================================================\n")
cat("  Fin del script T06\n")
cat("================================================================\n")
