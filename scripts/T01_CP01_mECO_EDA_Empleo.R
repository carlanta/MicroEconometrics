# =============================================================================
# TEMA 01 — Caso Práctico 1: EDA de Datos de Mercado Laboral (Variable Binaria)
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Economista · Perito Financiero
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics
# Versión 1.0 — 2026
#
# OBJETIVO: Análisis exploratorio de datos (EDA) de una muestra de 600 personas
#           en edad activa. Variable dependiente BINARIA: trabaja (0/1).
#
# INSTRUCCIONES:
#   1. Abre este script en RStudio.
#   2. Establece el directorio de trabajo en la carpeta 'scripts' del proyecto:
#      Session > Set Working Directory > To Source File Location
#   3. Ejecuta con el botón SOURCE o bloque a bloque con Ctrl+Enter.
#   4. Los gráficos y tablas se guardarán en scripts/output/.
# =============================================================================

# -------------------------------------------------------------------------- #
# 0. CONFIGURACIÓN INICIAL                                                    #
# -------------------------------------------------------------------------- #

# Función de pausa interactiva entre secciones
pausa <- function(msg = "\n>>> Pulsa ENTER para continuar con la siguiente sección...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}

# Auto-instalación de paquetes necesarios
pkgs <- c("kableExtra", "corrplot")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message("Instalando paquete: ", p)
    install.packages(p, quiet = TRUE)
  }
}

# Directorios
DATA_DIR   <- "../data"
OUTPUT_DIR <- "output"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

# -------------------------------------------------------------------------- #
# 1. CARGA Y PRIMERA INSPECCIÓN DE LOS DATOS                                 #
# -------------------------------------------------------------------------- #
cat("\n")
cat("================================================================\n")
cat("  CASO PRÁCTICO 1 — Mercado Laboral: Variable Dependiente Binaria\n")
cat("================================================================\n")
cat("\nCargando datos del mercado laboral...\n")

load(file.path(DATA_DIR, "T01_CP01_empleo_basico.RData"))

cat("\n--- ESTRUCTURA DEL DATASET ---\n")
str(empleo)

cat("\n--- PRIMERAS 6 OBSERVACIONES ---\n")
print(head(empleo))

cat("\n▶ INTERPRETACIÓN:\n")
cat("  El dataset contiene", nrow(empleo), "individuos y", ncol(empleo), "variables.\n")
cat("  La variable dependiente 'trabaja' es binaria (0 = no trabaja, 1 = trabaja).\n")
cat("  Las variables 'salario_bruto' y 'sector' solo están disponibles para los\n")
cat("  individuos que trabajan (", sum(empleo$trabaja), "personas, el",
    round(mean(empleo$trabaja)*100,1), "% de la muestra).\n")
cat("  Los", sum(!empleo$trabaja), "individuos no empleados presentan NA en salario y sector.\n")
cat("  Esto NO es un error de datos, sino un patrón esperable: no podemos observar\n")
cat("  el salario de quien no trabaja. Es la naturaleza de la variable dependiente.\n")

pausa()

# -------------------------------------------------------------------------- #
# 2. ESTADÍSTICOS DESCRIPTIVOS                                                #
# -------------------------------------------------------------------------- #
cat("\n")
cat("================================================================\n")
cat("  SECCIÓN 2 — Estadísticos Descriptivos\n")
cat("================================================================\n")

vars_num <- c("edad", "educacion", "experiencia", "hijos")
cat("\n--- RESUMEN NUMÉRICO (variables disponibles para todos) ---\n\n")
for (v in vars_num) {
  x <- empleo[[v]]
  cat(sprintf("  %-14s | Media: %5.1f | Mediana: %5.1f | DT: %5.1f | Min: %3.0f | Max: %3.0f\n",
              v, mean(x), median(x), sd(x), min(x), max(x)))
}

cat("\n--- VARIABLE DEPENDIENTE: trabaja ---\n")
tab_trab <- table(empleo$trabaja)
cat(sprintf("  No trabaja (y=0): %d personas (%.1f%%)\n",
            tab_trab[1], 100*tab_trab[1]/nrow(empleo)))
cat(sprintf("  Sí trabaja (y=1): %d personas (%.1f%%)\n",
            tab_trab[2], 100*tab_trab[2]/nrow(empleo)))

cat("\n--- TASA DE EMPLEO POR SEXO ---\n")
cat(sprintf("  Hombres (sexo=1): %.1f%% de empleo\n",
            100*mean(empleo$trabaja[empleo$sexo==1])))
cat(sprintf("  Mujeres (sexo=0): %.1f%% de empleo\n",
            100*mean(empleo$trabaja[empleo$sexo==0])))
cat("\n▶ INTERPRETACIÓN:\n")
cat("  La tasa de empleo del", round(mean(empleo$trabaja)*100,1), "% es coherente con una\n")
cat("  economía con desempleo moderado (incluyendo inactivos). La brecha de empleo por\n")
cat("  sexo sugiere que las mujeres tienen menor probabilidad de estar empleadas, lo que\n")
cat("  puede reflejar efectos de capital humano, preferencias o discriminación. El modelo\n")
cat("  Probit/Logit nos permitirá cuantificar esta brecha controlando el resto de factores.\n")

pausa()

# -------------------------------------------------------------------------- #
# 3. ANÁLISIS VISUAL: DISTRIBUCIONES UNIVARIANTES                            #
# -------------------------------------------------------------------------- #
cat("\n")
cat("================================================================\n")
cat("  SECCIÓN 3 — Análisis Visual de Distribuciones\n")
cat("================================================================\n")
cat("\nGenerando gráficos de distribuciones... (se guardará en output/)\n")

png(file.path(OUTPUT_DIR, "T01_CP01_distribuciones.png"),
    width=1800, height=1200, res=150)
par(mfrow=c(2,3), mar=c(4,4,2.5,0.5))

hist(empleo$edad, col=gray(0.75), border="white", main="Edad",
     xlab="Años", ylab="Frecuencia", breaks=15)
abline(v=mean(empleo$edad), lty=2, lwd=2)
legend("topright", legend=sprintf("Media=%.0f", mean(empleo$edad)),
       lty=2, lwd=2, bty="n", cex=0.85)

hist(empleo$educacion, col=gray(0.75), border="white", main="Educación",
     xlab="Años", ylab="Frecuencia", breaks=10)
abline(v=mean(empleo$educacion), lty=2, lwd=2)

hist(empleo$experiencia, col=gray(0.75), border="white", main="Experiencia laboral",
     xlab="Años", ylab="Frecuencia", breaks=15)
abline(v=mean(empleo$experiencia), lty=2, lwd=2)

barplot(table(empleo$hijos), col=gray(0.70), border="white",
        main="Número de hijos", xlab="Hijos", ylab="Frecuencia")

sal_emp <- empleo$salario_bruto[!is.na(empleo$salario_bruto)]
hist(sal_emp, col=gray(0.75), border="white", main="Salario bruto (empleados)",
     xlab="€/hora", ylab="Frecuencia", breaks=15)
abline(v=mean(sal_emp), lty=2, lwd=2)

boxplot(salario_bruto ~ sexo, data=empleo[!is.na(empleo$salario_bruto),],
        names=c("Mujer","Hombre"), col=gray(c(0.80,0.55)), border=gray(0.2),
        main="Salario por sexo", ylab="€/hora")

dev.off()
cat("  Gráfico guardado en output/T01_CP01_distribuciones.png\n")

cat("\n▶ INTERPRETACIÓN:\n")
cat("  · Edad: distribución aproximadamente uniforme entre 20 y 65 años.\n")
cat("  · Educación: concentrada en torno a los 12 años (bachillerato/FP),\n")
cat("    con una pequeña cola hacia estudios superiores.\n")
cat("  · Experiencia: sesgo positivo marcado — muchos trabajadores jóvenes\n")
cat("    con poca experiencia y una cola de trabajadores muy experimentados.\n")
cat("    Un modelo podría beneficiarse de usar log(experiencia) para reducir\n")
cat("    la asimetría.\n")
cat("  · Salario: brecha de género visible en el boxplot — los hombres presentan\n")
cat("    un salario mediano superior al de las mujeres. Este dato sugiere la\n")
cat("    conveniencia de incluir 'sexo' en el modelo.\n")

pausa()

# -------------------------------------------------------------------------- #
# 4. ANÁLISIS DE LA VARIABLE DEPENDIENTE BINARIA                             #
# -------------------------------------------------------------------------- #
cat("\n")
cat("================================================================\n")
cat("  SECCIÓN 4 — Análisis de la Variable Dependiente Binaria\n")
cat("================================================================\n")
cat("\nPatrones de empleo por subgrupos...\n")

png(file.path(OUTPUT_DIR, "T01_CP01_vd_binaria.png"),
    width=1600, height=800, res=150)
par(mfrow=c(1,3), mar=c(4,4.5,2.5,0.5))

# Distribución marginal
prop_trab <- prop.table(table(empleo$trabaja))
bp1 <- barplot(prop_trab, names.arg=c("No trabaja\n(y=0)","Trabaja\n(y=1)"),
        col=gray(c(0.75,0.30)), border="white", ylim=c(0,0.75),
        ylab="Proporción", main="Variable dependiente")
abline(h=0)
text(bp1, as.numeric(prop_trab)+0.03,
     sprintf("%.1f%%", as.numeric(prop_trab)*100), font=2, cex=0.9)

# Por nivel educativo
empleo$educ_g <- cut(empleo$educacion, breaks=c(5,9,12,16,22),
                      labels=c("≤9 años","10-12","13-16","17+"))
tasa_e <- tapply(empleo$trabaja, empleo$educ_g, mean)
bp2 <- barplot(tasa_e, col=gray(seq(0.75,0.25,length.out=4)), border="white",
        ylim=c(0,1), ylab="Tasa de empleo", xlab="Años de educación",
        main="Empleo por educación")
abline(h=0)
text(bp2, tasa_e+0.03, sprintf("%.0f%%", tasa_e*100), font=2, cex=0.85)

# Por número de hijos
empleo$hijos_g <- ifelse(empleo$hijos==0,"0 hijos",
                  ifelse(empleo$hijos==1,"1 hijo",
                  ifelse(empleo$hijos==2,"2 hijos","3+hijos")))
empleo$hijos_g <- factor(empleo$hijos_g, levels=c("0 hijos","1 hijo","2 hijos","3+hijos"))
tasa_h <- tapply(empleo$trabaja, empleo$hijos_g, mean)
bp3 <- barplot(tasa_h, col=gray(seq(0.30,0.75,length.out=4)), border="white",
        ylim=c(0,1), ylab="Tasa de empleo", xlab="Número de hijos",
        main="Empleo por hijos")
abline(h=0)
text(bp3, tasa_h+0.03, sprintf("%.0f%%", tasa_h*100), font=2, cex=0.85)

dev.off()
cat("  Gráfico guardado en output/T01_CP01_vd_binaria.png\n")

cat("\n▶ INTERPRETACIÓN:\n")
cat("  · La tasa de empleo crece monótonamente con el nivel educativo: desde el\n")
cat("    nivel básico (≤9 años) hasta el universitario (17+ años). Este patrón\n")
cat("    indica que la educación tiene un efecto positivo y significativo sobre la\n")
cat("    probabilidad de empleo.\n")
cat("  · La tasa de empleo decrece con el número de hijos. Este efecto es\n")
cat("    consistente con la literatura sobre economía familiar: los hijos aumentan\n")
cat("    el coste de oportunidad de trabajar (especialmente para las mujeres) y\n")
cat("    reducen la probabilidad de participación laboral.\n")
cat("  · Estos patrones son exactamente los que el modelo Probit o Logit capturará\n")
cat("    de forma formal, permitiéndonos cuantificar el efecto de cada variable\n")
cat("    manteniendo constantes las demás (efecto ceteris paribus).\n")

pausa()

# -------------------------------------------------------------------------- #
# 5. ANÁLISIS BIVARIANTE: RELACIÓN VD ~ COVARIABLES                          #
# -------------------------------------------------------------------------- #
cat("\n")
cat("================================================================\n")
cat("  SECCIÓN 5 — Análisis Bivariante\n")
cat("================================================================\n")
cat("\nComparando distribuciones de covariables entre empleados y no empleados...\n")

png(file.path(OUTPUT_DIR, "T01_CP01_bivariante.png"),
    width=1800, height=700, res=150)
par(mfrow=c(1,4), mar=c(4,4,2.5,0.5))

for (vv in c("edad","educacion","experiencia","hijos")) {
  boxplot(empleo[[vv]] ~ empleo$trabaja,
          col=gray(c(0.80,0.40)), border=gray(0.2),
          names=c("No emp.","Empleado"),
          ylab="Años", main=switch(vv,
            edad="Edad", educacion="Educación",
            experiencia="Experiencia", hijos="Hijos"),
          xlab="")
  # Test de Wilcoxon (no paramétrico)
  wt <- wilcox.test(empleo[[vv]] ~ empleo$trabaja)
  mtext(sprintf("p=%.3f", wt$p.value), side=1, line=3, cex=0.7,
        col=ifelse(wt$p.value<0.05,"black","gray50"))
}
dev.off()
cat("  Gráfico guardado en output/T01_CP01_bivariante.png\n")

cat("\n▶ INTERPRETACIÓN ESTADÍSTICA (Test de Wilcoxon, p-valores bajo cada gráfico):\n")
for (vv in c("edad","educacion","experiencia","hijos")) {
  wt <- wilcox.test(empleo[[vv]] ~ empleo$trabaja)
  sig <- ifelse(wt$p.value<0.001,"***",ifelse(wt$p.value<0.01,"**",
               ifelse(wt$p.value<0.05,"*","n.s.")))
  med0 <- median(empleo[[vv]][empleo$trabaja==0])
  med1 <- median(empleo[[vv]][empleo$trabaja==1])
  cat(sprintf("  %-14s | Med(y=0)=%5.1f | Med(y=1)=%5.1f | p=%.4f %s\n",
              vv, med0, med1, wt$p.value, sig))
}
cat("\n  (***p<0.001, **p<0.01, *p<0.05, n.s.=no significativo)\n")
cat("\n  Una diferencia estadísticamente significativa en la distribución de una\n")
cat("  covariable entre los dos grupos indica que esa variable está asociada con\n")
cat("  la probabilidad de empleo y probablemente será un predictor útil en el\n")
cat("  modelo Probit/Logit.\n")

pausa()

# -------------------------------------------------------------------------- #
# 6. DIAGNÓSTICO FINAL                                                        #
# -------------------------------------------------------------------------- #
cat("\n")
cat("================================================================\n")
cat("  SECCIÓN 6 — Diagnóstico y Conclusiones del EDA\n")
cat("================================================================\n\n")

cat("  RESUMEN DEL ANÁLISIS EXPLORATORIO:\n\n")
cat("  1. TIPO DE VDL: Variable dependiente BINARIA (trabaja = 0/1).\n")
cat("     Modelo apropiado: Probit o Logit (Capítulo 2).\n\n")
cat("  2. DISTRIBUCIÓN: Tasa de empleo del",
    round(mean(empleo$trabaja)*100,1), "%. No hay desequilibrio\n")
cat("     extremo de clases, lo que facilita la estimación.\n\n")
cat("  3. VARIABLES PREDICTORAS RELEVANTES (según EDA bivariante):\n")
cat("     - Educación: fuerte relación positiva con el empleo (***).\n")
cat("     - Experiencia: diferencia significativa entre grupos (***).\n")
cat("     - Sexo: brecha de empleo de ~5 puntos porcentuales.\n")
cat("     - Número de hijos: efecto negativo sobre el empleo.\n\n")
cat("  4. NOTAS PARA EL MODELADO:\n")
cat("     - Considerar transformación logarítmica de 'experiencia' por su\n")
cat("       distribución asimétrica.\n")
cat("     - 'salario_bruto' NO debe incluirse en el modelo de participación\n")
cat("       laboral: es endógena (solo observable para quien trabaja).\n")
cat("     - Los sectores (Servicios/Industria/Agricultura) están disponibles\n")
cat("       para los empleados y podrían usarse en un análisis de salarios.\n\n")
cat("  ✓ EDA completado. Los datos están listos para la estimación en Cap. 2.\n")
cat("\n  Outputs guardados en: scripts/output/\n")
cat("    - T01_CP01_distribuciones.png\n")
cat("    - T01_CP01_vd_binaria.png\n")
cat("    - T01_CP01_bivariante.png\n")
cat("\n================================================================\n")
cat("  FIN DEL SCRIPT T01_CP01_mECO_EDA_Empleo.R\n")
cat("================================================================\n\n")
