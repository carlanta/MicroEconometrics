# =============================================================================
# TEMA 01 — Caso Práctico 3: EDA de Visitas al Médico (Variable de Recuento)
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Economista · Perito Financiero · Profesor de Econometría — UNIR
# https://github.com/carlanta/MicroEconometrics   Versión 1.0 — 2026
#
# OBJETIVO: Análisis exploratorio de datos de 600 individuos. Variable dependiente
#           de RECUENTO: número de visitas al médico en el último año.
#           Se analiza la sobredispersión y los determinantes del uso sanitario.
#
# INSTRUCCIONES:
#   1. Abre este script en RStudio.
#   2. Session > Set Working Directory > To Source File Location
#   3. Ejecuta con SOURCE o bloque a bloque con Ctrl+Enter.
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}

pkgs <- c("kableExtra")
for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, quiet=TRUE)

DATA_DIR   <- "../data"
OUTPUT_DIR <- "output"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

# -------------------------------------------------------------------------- #
# 1. CARGA Y PRIMERA INSPECCIÓN                                               #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  CASO PRÁCTICO 3 — Visitas al Médico: Variable de Recuento\n")
cat("================================================================\n")
cat("\nCargando datos de visitas al médico...\n")

load(file.path(DATA_DIR, "T01_CP03_visitas_medico.RData"))

cat("\n--- ESTRUCTURA DEL DATASET ---\n")
str(medico)

cat("\n--- PRIMERAS 8 OBSERVACIONES ---\n")
print(head(medico, 8))

cat("\n▶ INTERPRETACIÓN:\n")
cat(sprintf("  El dataset contiene %d individuos con %d variables.\n",
            nrow(medico), ncol(medico)))
cat("  La variable dependiente 'visitas' es de RECUENTO: solo toma valores\n")
cat("  enteros no negativos {0, 1, 2, ...}. Esto la hace fundamentalmente\n")
cat("  diferente de una variable continua normal:\n")
cat("  · No puede tomar valores negativos.\n")
cat("  · Los valores pequeños son muy frecuentes (muchas personas no van al médico).\n")
cat("  · La distribución es asimétrica a la derecha (unos pocos van muchas veces).\n")
cat("  · La varianza suele ser mayor que la media (sobredispersión).\n")
cat("  Por estas razones, los modelos de Poisson y Binomial Negativa (Cap. 4)\n")
cat("  son los apropiados para este tipo de datos.\n")

pausa()

# -------------------------------------------------------------------------- #
# 2. ESTADÍSTICOS DESCRIPTIVOS Y PRUEBA DE SOBREDISPERSIÓN                  #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 2 — Estadísticos Descriptivos y Sobredispersión\n")
cat("================================================================\n\n")

mu_v  <- mean(medico$visitas)
var_v <- var(medico$visitas)
mdn_v <- median(medico$visitas)
ratio_d <- var_v / mu_v

cat("--- ESTADÍSTICOS DE LA VARIABLE DE RECUENTO (visitas) ---\n\n")
cat(sprintf("  Media (μ):               %7.3f\n", mu_v))
cat(sprintf("  Mediana:                 %7.3f\n", mdn_v))
cat(sprintf("  Varianza (σ²):           %7.3f\n", var_v))
cat(sprintf("  Desv. típica (σ):        %7.3f\n", sqrt(var_v)))
cat(sprintf("  Ratio Var/Media (VMR):   %7.3f\n", ratio_d))
cat(sprintf("  Proporción de ceros:     %7.1f%%\n", 100*mean(medico$visitas==0)))
cat(sprintf("  Valor máximo:            %7.0f visitas\n", max(medico$visitas)))

cat("\n--- DIAGNÓSTICO DE SOBREDISPERSIÓN ---\n\n")
cat("  Bajo el modelo de Poisson, E(y) = Var(y), es decir, el ratio\n")
cat("  Var/Media (VMR) debería ser igual a 1.\n\n")
if (ratio_d > 3) {
  cat(sprintf("  ⚠ VMR = %.2f >> 1: Sobredispersión SEVERA.\n", ratio_d))
  cat("    El modelo de Poisson subestimará seriamente los errores estándar,\n")
  cat("    produciendo inferencia estadística INVÁLIDA (p-valores demasiado\n")
  cat("    pequeños, intervalos de confianza demasiado estrechos).\n")
  cat("    El modelo Binomial Negativa (NBM) es imprescindible.\n")
} else if (ratio_d > 1.5) {
  cat(sprintf("  ⚠ VMR = %.2f > 1: Sobredispersión MODERADA.\n", ratio_d))
  cat("    Se recomienda el modelo Binomial Negativa o Quasi-Poisson.\n")
} else {
  cat(sprintf("  ✓ VMR = %.2f ≈ 1: Equidispersión aproximada.\n", ratio_d))
  cat("    El modelo de Poisson es apropiado.\n")
}

cat("\n--- DISTRIBUCIÓN DE FRECUENCIAS ---\n\n")
tab_vis <- table(medico$visitas)
for (v_val in min(medico$visitas):min(max(medico$visitas), 15)) {
  freq_v <- if (v_val %in% as.integer(names(tab_vis)))
              tab_vis[as.character(v_val)] else 0
  cat(sprintf("  %3d visitas: %3d obs. (%4.1f%%) %s\n",
              v_val, freq_v, 100*freq_v/nrow(medico),
              paste(rep("|", min(round(freq_v/4), 30)), collapse="")))
}
if (max(medico$visitas) > 15) {
  mas <- sum(medico$visitas > 15)
  cat(sprintf("  >15 visitas: %3d obs. (%4.1f%%)\n", mas, 100*mas/nrow(medico)))
}

pausa()

# -------------------------------------------------------------------------- #
# 3. ANÁLISIS VISUAL: DISTRIBUCIÓN DEL RECUENTO                             #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 3 — Visualización de la Distribución de Recuento\n")
cat("================================================================\n\n")

png(file.path(OUTPUT_DIR, "T01_CP03_distribucion_recuento.png"),
    width=1800, height=900, res=150)
par(mfrow=c(1,3), mar=c(4.5,4.5,2.5,0.5))

# Panel 1: distribución observada
tab_v2 <- table(factor(pmin(medico$visitas, 20), levels=0:20))
bp_v <- barplot(tab_v2, col=gray(0.65), border="white",
        main="Distribución de visitas",
        xlab="N.º de visitas (20 = 20+)", ylab="Frecuencia",
        names.arg=c(as.character(0:19),"20+"))
abline(v=mu_v/diff(range(medico$visitas))*length(bp_v), lty=2, lwd=2)
text(max(bp_v)*0.6, max(tab_v2)*0.8,
     sprintf("Media=%.1f\nVar=%.1f\nVMR=%.1f", mu_v, var_v, ratio_d),
     cex=0.8, font=2)

# Panel 2: comparación con distribución de Poisson
x_range <- 0:25
obs_freq  <- table(factor(medico$visitas, levels=x_range)) / nrow(medico)
pois_freq <- dpois(x_range, lambda=mu_v)
plot(x_range, as.numeric(obs_freq), type="h", lwd=3, col=gray(0.25),
     xlab="N.º de visitas", ylab="Proporción / Prob.",
     main="Observado vs Poisson(μ)")
lines(x_range+0.3, pois_freq, type="h", lwd=2, lty=2, col=gray(0.65))
legend("topright", legend=c("Observado","Poisson teórico"),
       lwd=c(3,2), lty=c(1,2), col=c(gray(0.25),gray(0.65)), bty="n", cex=0.8)

# Panel 3: QQ-plot (visitas vs Normal)
qqnorm(medico$visitas, main="Q-Q Plot (vs Normal)",
       pch=1, col=gray(0.4), cex=0.7)
qqline(medico$visitas, lwd=2, lty=1)
text(-2, max(medico$visitas)*0.85,
     "Desviación de la Normal\n= uso de conteos inadecuado\ncon distribución Normal",
     cex=0.7, font=3, pos=4)

dev.off()
cat("  Gráfico guardado: output/T01_CP03_distribucion_recuento.png\n")

cat("\n▶ INTERPRETACIÓN:\n")
cat("  · Panel 1 (barras): La distribución es claramente asimétrica a la derecha.\n")
cat("    La gran mayoría de personas realiza pocas visitas, pero algunos individuos\n")
cat("    presentan un número muy elevado, generando una cola larga.\n\n")
cat("  · Panel 2 (Observado vs Poisson): La distribución observada (barras oscuras)\n")
cat("    diverge sistemáticamente de lo que predice Poisson (barras grises):\n")
cat("    hay más ceros y más valores altos que lo que Poisson predice.\n")
cat("    Esto confirma la sobredispersión y la necesidad del modelo Binomial Negativa.\n\n")
cat("  · Panel 3 (Q-Q Plot Normal): La desviación respecto a la línea diagonal\n")
cat("    demuestra que los datos NO siguen una distribución Normal. Asumir\n")
cat("    normalidad con datos de recuento produce inferencia incorrecta.\n")

pausa()

# -------------------------------------------------------------------------- #
# 4. DETERMINANTES DEL USO SANITARIO                                         #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 4 — Determinantes del Número de Visitas\n")
cat("================================================================\n\n")

png(file.path(OUTPUT_DIR, "T01_CP03_determinantes.png"),
    width=1800, height=900, res=150)
par(mfrow=c(2,3), mar=c(4,4.5,2.5,0.5))

# Visitas por cronicidad
boxplot(visitas ~ cronico, data=medico,
        names=c("Sin crónico","Con crónico"),
        col=gray(c(0.80,0.40)), border=gray(0.2),
        main="Visitas por cronicidad", ylab="N.º visitas")
wt_c <- wilcox.test(visitas ~ cronico, data=medico)
mtext(sprintf("p=%.4f ***", wt_c$p.value), side=1, line=3, cex=0.7)

# Visitas por seguro privado
boxplot(visitas ~ seguro_privado, data=medico,
        names=c("Sin seguro priv.","Con seguro priv."),
        col=gray(c(0.80,0.40)), border=gray(0.2),
        main="Visitas por seguro", ylab="N.º visitas")
wt_s <- wilcox.test(visitas ~ seguro_privado, data=medico)
mtext(sprintf("p=%.4f", wt_s$p.value), side=1, line=3, cex=0.7)

# Visitas por limitación funcional
boxplot(visitas ~ limitacion_funcional, data=medico,
        names=c("Sin limitación","Con limitación"),
        col=gray(c(0.80,0.40)), border=gray(0.2),
        main="Visitas por limitación", ylab="N.º visitas")
wt_l <- wilcox.test(visitas ~ limitacion_funcional, data=medico)
mtext(sprintf("p=%.4f", wt_l$p.value), side=1, line=3, cex=0.7)

# Visitas vs edad (scatter)
plot(medico$edad, medico$visitas, pch=1, col=gray(0.45), cex=0.7,
     xlab="Edad (años)", ylab="N.º visitas",
     main="Visitas vs Edad")
abline(lm(visitas ~ edad, data=medico), lwd=2, lty=1, col="black")

# Visitas vs ingreso
plot(medico$ingreso, medico$visitas, pch=1, col=gray(0.45), cex=0.7,
     xlab="Ingreso (€ miles)", ylab="N.º visitas",
     main="Visitas vs Ingreso")
abline(lm(visitas ~ ingreso, data=medico), lwd=2, lty=2, col="black")

# Medias de visitas por grupo
medias <- c(
  sin_cronico  = mean(medico$visitas[medico$cronico==0]),
  con_cronico  = mean(medico$visitas[medico$cronico==1]),
  sin_seguro   = mean(medico$visitas[medico$seguro_privado==0]),
  con_seguro   = mean(medico$visitas[medico$seguro_privado==1]),
  sin_limit    = mean(medico$visitas[medico$limitacion_funcional==0]),
  con_limit    = mean(medico$visitas[medico$limitacion_funcional==1])
)
bp_m <- barplot(medias,
        col   = gray(rep(c(0.75, 0.35), 3)),
        border = "white",
        names.arg = c("S/crón","C/crón","S/seg","C/seg","S/lim","C/lim"),
        ylab = "Media de visitas",
        main = "Medias por subgrupo",
        las = 2, cex.names = 0.75)
text(bp_m, medias+0.2, sprintf("%.1f", medias), cex=0.75, font=2)
dev.off()
cat("  Gráfico guardado: output/T01_CP03_determinantes.png\n")

cat("\n--- MEDIAS DE VISITAS POR SUBGRUPO ---\n\n")
cat(sprintf("  Pacientes sin enfermedad crónica:   %.2f visitas/año\n",
            mean(medico$visitas[medico$cronico==0])))
cat(sprintf("  Pacientes con enfermedad crónica:   %.2f visitas/año\n",
            mean(medico$visitas[medico$cronico==1])))
cat(sprintf("  Ratio:  %.2fx más visitas con enfermedad crónica\n\n",
            mean(medico$visitas[medico$cronico==1])/mean(medico$visitas[medico$cronico==0])))

cat(sprintf("  Sin seguro privado:  %.2f visitas/año\n",
            mean(medico$visitas[medico$seguro_privado==0])))
cat(sprintf("  Con seguro privado:  %.2f visitas/año\n",
            mean(medico$visitas[medico$seguro_privado==1])))

cat("\n▶ INTERPRETACIÓN:\n")
cat("  La enfermedad crónica es, con diferencia, el determinante más importante\n")
cat("  del número de visitas al médico. Los pacientes crónicos visitan al médico\n")
cat("  aproximadamente el doble que los no crónicos. Este efecto es altamente\n")
cat("  significativo (p<0.001 en el test de Wilcoxon).\n")
cat("  El seguro privado y la limitación funcional también aumentan el número de\n")
cat("  visitas. El efecto del ingreso es moderado y de signo negativo (a mayor\n")
cat("  ingreso, ligeramente menos visitas, posiblemente por mejores condiciones\n")
cat("  de vida y salud preventiva).\n")

pausa()

# -------------------------------------------------------------------------- #
# 5. DIAGNÓSTICO FINAL                                                        #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  DIAGNÓSTICO FINAL\n")
cat("================================================================\n\n")

cat("  TIPO DE VDL: Variable de RECUENTO (visitas al médico).\n")
cat("  Modelo apropiado: Poisson o Binomial Negativa (Cap. 4).\n\n")
cat(sprintf("  Ratio Var/Media = %.2f >> 1: SOBREDISPERSIÓN SEVERA.\n", ratio_d))
cat("  → El modelo de Poisson producirá errores estándar sesgados a la baja.\n")
cat("    Se recomienda el modelo Binomial Negativa (NB2) como primera opción.\n\n")
cat("  PREDICTORES RELEVANTES DETECTADOS EN EL EDA:\n")
cat("    1. Cronicidad:          efecto muy fuerte, p<0.001 (***).\n")
cat("    2. Limitación funcional: efecto significativo.\n")
cat("    3. Seguro privado:       aumenta el uso de servicios.\n")
cat("    4. Edad:                 relación positiva moderada.\n")
cat("    5. Ingreso:              relación negativa leve.\n\n")
cat("  ✓ EDA completado. Outputs en scripts/output/:\n")
cat("    - T01_CP03_distribucion_recuento.png\n")
cat("    - T01_CP03_determinantes.png\n\n")
cat("================================================================\n")
cat("  FIN DEL SCRIPT T01_CP03_mECO_EDA_Visitas.R\n")
cat("================================================================\n\n")
