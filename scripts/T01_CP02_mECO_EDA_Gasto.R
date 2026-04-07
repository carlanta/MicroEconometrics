# =============================================================================
# TEMA 01 — Caso Práctico 2: EDA de Gasto en Seguros del Hogar (Var. Censurada)
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Economista · Perito Financiero · Profesor de Econometría — UNIR
# https://github.com/carlanta/MicroEconometrics   Versión 1.0 — 2026
#
# OBJETIVO: Análisis exploratorio de datos de 500 hogares. Variable dependiente
#           CENSURADA en cero: gasto mensual en seguros médicos privados (€).
#           El 48% de hogares presenta gasto cero (censurados).
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

# -------------------------------------------------------------------------- #
# 1. CARGA Y PRIMERA INSPECCIÓN                                               #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  CASO PRÁCTICO 2 — Gasto en Seguros: Variable Dependiente Censurada\n")
cat("================================================================\n")
cat("\nCargando datos de gasto en seguros del hogar...\n")

load(file.path(DATA_DIR, "T01_CP02_gasto_hogar.RData"))

cat("\n--- ESTRUCTURA DEL DATASET ---\n")
str(hogar)

cat("\n--- PRIMERAS 8 OBSERVACIONES ---\n")
print(head(hogar, 8))

n_ceros <- sum(hogar$gasto_seguros == 0)
n_pos   <- sum(hogar$gasto_seguros > 0)
cat("\n▶ INTERPRETACIÓN:\n")
cat(sprintf("  El dataset contiene %d hogares con %d variables.\n", nrow(hogar), ncol(hogar)))
cat(sprintf("  Gasto = 0 (censurados): %d hogares (%.1f%%).\n",
            n_ceros, 100*n_ceros/nrow(hogar)))
cat(sprintf("  Gasto > 0 (positivos):  %d hogares (%.1f%%).\n",
            n_pos,   100*n_pos/nrow(hogar)))
cat("\n  Esta distribución mixta —con una masa discreta en 0 y una distribución\n")
cat("  continua para los valores positivos— es la firma característica de una\n")
cat("  variable CENSURADA. MCO no puede manejar correctamente esta estructura:\n")
cat("  trata los ceros como valores reales y produce estimaciones sesgadas.\n")
cat("  El modelo TOBIT (Capítulo 3) está específicamente diseñado para este caso.\n")

pausa()

# -------------------------------------------------------------------------- #
# 2. ESTADÍSTICOS DESCRIPTIVOS                                                #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 2 — Estadísticos Descriptivos\n")
cat("================================================================\n\n")

cat("--- ESTADÍSTICOS DE LA VARIABLE DEPENDIENTE (gasto_seguros) ---\n\n")
cat(sprintf("  Todos los hogares    | Media: %6.1f | Mediana: %5.1f | DT: %6.1f\n",
            mean(hogar$gasto_seguros), median(hogar$gasto_seguros),
            sd(hogar$gasto_seguros)))
cat(sprintf("  Solo positivos (>0)  | Media: %6.1f | Mediana: %5.1f | DT: %6.1f\n",
            mean(hogar$gasto_seguros[hogar$gasto_seguros>0]),
            median(hogar$gasto_seguros[hogar$gasto_seguros>0]),
            sd(hogar$gasto_seguros[hogar$gasto_seguros>0])))

cat("\n--- COVARIABLES ---\n\n")
vars_h <- c("renta","edad_cabeza","num_miembros","educacion_cabeza")
for (v in vars_h) {
  x <- hogar[[v]]
  cat(sprintf("  %-22s | Media: %5.1f | DT: %5.1f | Min: %4.0f | Max: %4.0f\n",
              v, mean(x), sd(x), min(x), max(x)))
}

cat("\n--- VARIABLES BINARIAS ---\n\n")
cat(sprintf("  Zona urbana:     %.1f%% de hogares en zona urbana\n",
            100*mean(hogar$zona_urbana)))
cat(sprintf("  Tiene vehículo:  %.1f%% de hogares con vehículo propio\n",
            100*mean(hogar$tiene_vehiculo)))

cat("\n▶ NOTA SOBRE LA MEDIA:\n")
cat("  La media de gasto_seguros (calculada sobre TODOS los hogares, incluidos\n")
cat("  los que no gastan nada) es una estadística engañosa para variables censuradas.\n")
cat("  La media condicional —calculada sólo sobre los hogares con gasto positivo—\n")
cat("  es más informativa sobre el nivel de gasto habitual entre quienes se aseguran.\n")
cat("  El modelo Tobit estima AMBAS distribuciones simultáneamente.\n")

pausa()

# -------------------------------------------------------------------------- #
# 3. ANÁLISIS VISUAL: LA ESTRUCTURA DE LA CENSURA                            #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 3 — Estructura de la Variable Censurada\n")
cat("================================================================\n\n")

png(file.path(OUTPUT_DIR, "T01_CP02_estructura_censura.png"),
    width=1800, height=900, res=150)
par(mfrow=c(1,3), mar=c(4.5,4.5,2.5,0.5))

# Panel 1: histograma completo con masa en 0 resaltada
hist(hogar$gasto_seguros, breaks=35, col=gray(0.70), border="white",
     main="Distribución gasto seguros",
     xlab="Gasto mensual (€)", ylab="Frecuencia")
rect(par("usr")[1], 0, 1, n_ceros, col=gray(0.20), border=NA)
text(2, n_ceros*0.65,
     sprintf("%d hogares\ncon gasto=0\n(%.0f%%)", n_ceros, 100*n_ceros/nrow(hogar)),
     pos=4, cex=0.8, font=2)

# Panel 2: densidad de los valores positivos
plot(density(hogar$gasto_seguros[hogar$gasto_seguros>0]),
     col="black", lwd=2, main="Densidad: solo gasto > 0",
     xlab="Gasto mensual (€)", ylab="Densidad")
abline(v=mean(hogar$gasto_seguros[hogar$gasto_seguros>0]),
       lty=2, lwd=1.5, col="black")
abline(v=median(hogar$gasto_seguros[hogar$gasto_seguros>0]),
       lty=3, lwd=1.5, col="black")
legend("topright", legend=c("Media","Mediana"), lty=c(2,3), lwd=c(1.5,1.5), bty="n", cex=0.8)

# Panel 3: gasto vs renta (scatter con indicador de censura)
plot(hogar$renta, hogar$gasto_seguros,
     pch  = ifelse(hogar$gasto_seguros==0, 1, 16),
     col  = ifelse(hogar$gasto_seguros==0, gray(0.60), gray(0.20)),
     xlab = "Renta mensual (€ miles)", ylab="Gasto seguros (€)",
     main = "Gasto vs Renta")
abline(h=0, lty=2, col=gray(0.4))
abline(lm(gasto_seguros ~ renta, data=hogar), lwd=2, lty=1)
legend("topleft", legend=c("Gasto>0","Gasto=0","MCO (sesgado)"),
       pch=c(16,1,NA), lty=c(NA,NA,1), lwd=c(NA,NA,2),
       col=c(gray(0.20),gray(0.60),"black"), bty="n", cex=0.75)
dev.off()
cat("  Gráfico guardado: output/T01_CP02_estructura_censura.png\n")

cat("\n▶ INTERPRETACIÓN PANEL A PANEL:\n")
cat("  · Panel 1 (histograma): La barra negra en 0 representa la masa de\n")
cat(sprintf("    probabilidad discreta (%d hogares, %.0f%%). La parte derecha\n",
            n_ceros, 100*n_ceros/nrow(hogar)))
cat("    muestra la distribución continua de los gastos positivos.\n")
cat("    Esta mezcla discreta+continua hace inválido MCO.\n\n")
cat("  · Panel 2 (densidad positivos): Entre los hogares que SÍ tienen seguro,\n")
cat(sprintf("    el gasto sigue una distribución con media %.1f€ y mediana %.1f€.\n",
            mean(hogar$gasto_seguros[hogar$gasto_seguros>0]),
            median(hogar$gasto_seguros[hogar$gasto_seguros>0])))
cat("    La media > mediana confirma sesgo positivo.\n\n")
cat("  · Panel 3 (scatter): Los puntos huecos (gasto=0) se concentran en rentas\n")
cat("    bajas, confirmando que la renta es un determinante clave. MCO subestima\n")
cat("    la pendiente verdadera por incluir los ceros como valores reales.\n")

pausa()

# -------------------------------------------------------------------------- #
# 4. COMPARACIÓN: HOGARES CON/SIN SEGURO                                     #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 4 — Comparación Hogares Con/Sin Seguro\n")
cat("================================================================\n\n")

hogar$tiene_seguro <- hogar$gasto_seguros > 0

png(file.path(OUTPUT_DIR, "T01_CP02_comparacion_grupos.png"),
    width=1800, height=700, res=150)
par(mfrow=c(1,4), mar=c(4,4.5,2.5,0.5))

for (vv in c("renta","edad_cabeza","num_miembros","educacion_cabeza")) {
  boxplot(hogar[[vv]] ~ hogar$tiene_seguro,
          col=gray(c(0.80,0.40)), border=gray(0.2),
          names=c("Sin seguro","Con seguro"),
          ylab=switch(vv, renta="€ miles", edad_cabeza="Años",
                      num_miembros="Miembros", educacion_cabeza="Años"),
          main=switch(vv, renta="Renta", edad_cabeza="Edad",
                      num_miembros="Miembros hogar", educacion_cabeza="Educación"))
  wt <- wilcox.test(hogar[[vv]] ~ hogar$tiene_seguro)
  mtext(sprintf("p=%.4f", wt$p.value), side=1, line=3.2, cex=0.7)
}
dev.off()
cat("  Gráfico guardado: output/T01_CP02_comparacion_grupos.png\n")

cat("\n--- TEST DE DIFERENCIAS (Wilcoxon) ---\n\n")
for (vv in c("renta","edad_cabeza","num_miembros","educacion_cabeza")) {
  wt <- wilcox.test(hogar[[vv]] ~ hogar$tiene_seguro)
  sig <- ifelse(wt$p.value<0.001,"***",ifelse(wt$p.value<0.01,"**",
               ifelse(wt$p.value<0.05,"*","n.s.")))
  m0 <- mean(hogar[[vv]][!hogar$tiene_seguro])
  m1 <- mean(hogar[[vv]][ hogar$tiene_seguro])
  cat(sprintf("  %-22s | Sin seguro: %5.1f | Con seguro: %5.1f | p=%.4f %s\n",
              vv, m0, m1, wt$p.value, sig))
}

cat("\n▶ INTERPRETACIÓN:\n")
cat("  La renta es la variable con diferencias más marcadas entre grupos. Los\n")
cat(sprintf("  hogares con seguro tienen una renta media de %.1f€ miles frente a\n",
            mean(hogar$renta[hogar$tiene_seguro])))
cat(sprintf("  %.1f€ miles de los hogares sin seguro. Esta diferencia es altamente\n",
            mean(hogar$renta[!hogar$tiene_seguro])))
cat("  significativa (***) y confirma que la renta es el predictor principal de\n")
cat("  la decisión de asegurarse, lo que el modelo Tobit cuantificará en Cap. 3.\n")

pausa()

# -------------------------------------------------------------------------- #
# 5. DIAGNÓSTICO MCO VS TOBIT                                                 #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 5 — El Problema de MCO con Datos Censurados\n")
cat("================================================================\n\n")

mco_todos  <- lm(gasto_seguros ~ renta, data=hogar)
mco_pos    <- lm(gasto_seguros ~ renta, data=hogar[hogar$gasto_seguros>0,])

cat("  MCO con TODOS los datos (incluidos ceros):\n")
cat(sprintf("    Intercepto = %.3f   | Pendiente renta = %.4f\n",
            coef(mco_todos)[1], coef(mco_todos)[2]))
cat("\n  MCO solo con datos POSITIVOS (gasto > 0):\n")
cat(sprintf("    Intercepto = %.3f   | Pendiente renta = %.4f\n",
            coef(mco_pos)[1], coef(mco_pos)[2]))

cat("\n▶ INTERPRETACIÓN:\n")
cat("  La pendiente de MCO varía según se incluyan o no los ceros. Ambas\n")
cat("  versiones son sesgadas:\n")
cat("  · MCO con todos los datos: infravalora el efecto real de la renta porque\n")
cat("    los ceros 'empujan' la recta hacia abajo.\n")
cat("  · MCO solo positivos: sobrevalora el efecto porque selecciona únicamente\n")
cat("    hogares con alta utilidad por los seguros.\n")
cat("  El modelo Tobit resuelve este dilema estimando conjuntamente la probabilidad\n")
cat("  de tener gasto positivo Y el nivel de gasto condicional a que sea positivo.\n")

cat("\n================================================================\n")
cat("  DIAGNÓSTICO FINAL\n")
cat("================================================================\n\n")
cat("  TIPO DE VDL: Variable CENSURADA en 0 (gasto_seguros).\n")
cat("  Modelo apropiado: TOBIT (Capítulo 3).\n\n")
cat(sprintf("  Proporción censurada: %.1f%% (240 hogares con gasto=0).\n",
            100*n_ceros/nrow(hogar)))
cat("  Principal predictor detectado: renta mensual del hogar (****).\n")
cat("  Los efectos de zona urbana y vehículo también serán relevantes.\n\n")
cat("  ✓ EDA completado. Outputs en scripts/output/:\n")
cat("    - T01_CP02_estructura_censura.png\n")
cat("    - T01_CP02_comparacion_grupos.png\n\n")
cat("================================================================\n")
cat("  FIN DEL SCRIPT T01_CP02_mECO_EDA_Gasto.R\n")
cat("================================================================\n\n")
