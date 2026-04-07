# =============================================================================
# TEMA 3 — MODELOS PARA DATOS CENSURADOS (TOBIT): SCRIPT INTERACTIVO
# =============================================================================
# Autor: Carlos de Anta Puig
#        Economista · Perito Financiero
#        Miembro del Colegio de Economistas de Madrid
#        Miembro del Instituto Español de Analistas Financieros (IEAF)
#        Profesor de Econometría y Microeconometría
#        carlos@cwconsultores.com
#
# Manual de Microeconometría - Digital Reasons (2026)
# https://github.com/carlanta/MicroEconometrics
#
# Versión: 1.0
# Fecha:   2026-03-27
#
# Ejecutar bloque a bloque en RStudio.
# =============================================================================

#
# Autor:  Carlos de Anta Puig
#         Economista - Perito Financiero
#         Miembro del Colegio de Economistas de Madrid
#         Miembro del Instituto Espanol de Analistas Financieros (IEAF)
#         Profesor de Econometría y Microeconometría
#         carlosmaria.deanta@unir.net
#
# Version: 1.0 | Fecha: Marzo 2026
#
# Descripcion:
#   Script pedagogico del Tema 3. Analiza la participacion laboral
#   de mujeres casadas (Mroz 1987) con el modelo Tobit.
#   Cubre: MCO y sus limitaciones, estimacion Tobit, efectos marginales,
#   tests de especificacion y diagnosticos.
#
# Datos: PSID1976 (paquete AER)
# ============================================================================

rm(list=ls())

pausa <- function(){
  readline("\n>>> Pulsa ENTER para continuar...")
}

cat("\014")

cat("============================================================\n")
cat("ANALISIS ECONOMETRICO: MODELO TOBIT (MROZ 1987)\n")
cat("============================================================\n")
cat("Este script ejecuta paso a paso toda la practica.\n")
cat("Cada bloque mostrara resultados e interpretacion.\n")

pausa()

# =============================================================================
# PAQUETES
# =============================================================================

suppressPackageStartupMessages({
  library(AER)
  library(ggplot2)
  library(censReg)
  library(VGAM)
  library(DT)
})

# Funcion auxiliar para tablas interactivas DT
tabla_dt <- function(datos, titulo = "", decimales = 2, filas = TRUE) {
  if (is.numeric(datos) && !is.null(names(datos))) {
    datos <- data.frame(Nombre = names(datos), Valor = round(datos, decimales))
    filas <- FALSE
  }
  cat("[Abriendo tabla en el Viewer...]\n")
  DT::datatable(
    datos, caption = titulo, extensions = 'Buttons',
    options = list(dom = 'Bfrtip', buttons = c('copy','csv','excel'),
                   pageLength = 15, scrollX = TRUE),
    rownames = filas
  )
}

cat("\nPASO 1: CARGA DE DATOS\n")
cat("------------------------------------------------------------\n")

data("PSID1976", package="AER")
datos <- PSID1976

cat("Dataset cargado correctamente.\n")
cat("Numero de observaciones:",nrow(datos),"\n")
cat("Numero de variables:",ncol(datos),"\n")

cat("\nVariables relevantes del analisis:\n")
print(names(datos)[c(1,5,7,2,3,4)])

cat("\nInterpretacion:\n")
cat("Trabajamos con el dataset de Mroz (1987) sobre participacion\n")
cat("laboral de mujeres casadas.\n")

tabla_dt(datos, titulo = "Dataset PSID1976 (Mroz 1987)")

pausa()

# =============================================================================
# CENSURA
# =============================================================================

cat("\nPASO 2: ANALISIS DE CENSURA\n")
cat("------------------------------------------------------------\n")

n_total <- nrow(datos)
n_censuradas <- sum(datos$hours==0)
n_positivas <- sum(datos$hours>0)

cat("Observaciones totales:",n_total,"\n")
cat("Observaciones censuradas (hours=0):",n_censuradas,"\n")
cat("Observaciones positivas:",n_positivas,"\n")

prop <- round(n_censuradas/n_total*100,1)

cat("Proporcion censurada:",prop,"%\n")

cat("\nInterpretacion econometrica:\n")

if(prop>40){
  cat("La censura es ALTA.\n")
  cat("Esto significa que MCO producira sesgo de atenuacion.\n")
}else{
  cat("La censura es moderada.\n")
}

pausa()

# =============================================================================
# GRAFICO DISTRIBUCION
# =============================================================================

cat("\nPASO 3: VISUALIZAR LA CENSURA\n")
cat("------------------------------------------------------------\n")

p1 <- ggplot(datos,aes(x=hours))+
  geom_histogram(binwidth=100,fill="steelblue",color="white")+
  geom_vline(xintercept=0,color="red",linetype="dashed")+
  theme_minimal()

print(p1)

cat("\nInterpretacion:\n")
cat("Observamos una gran masa de probabilidad en cero.\n")
cat("Esto confirma la presencia de censura.\n")

pausa()

# =============================================================================
# MCO
# =============================================================================

cat("\nPASO 4: ESTIMACION MCO\n")
cat("------------------------------------------------------------\n")

mco_todos <- lm(hours ~ education + age + youngkids + oldkids + fincome,data=datos)

summary(mco_todos)

cat("\nInterpretacion:\n")
cat("MCO esta tratando los ceros como valores reales.\n")
cat("Esto reduce el valor absoluto de los coeficientes.\n")

cat("\nILUSTRACION DEL SESGO DE ATENUACION\n")
cat("------------------------------------------------------------\n")

set.seed(123)

n <- 400

x <- runif(n,0,10)

beta0 <- 0
beta1 <- 1.5

eps <- rnorm(n,0,2)

y_latente <- beta0 + beta1*x + eps

y_obs <- ifelse(y_latente < 0,0,y_latente)

modelo_mco <- lm(y_obs ~ x)

pend_mco <- coef(modelo_mco)[2]

cens <- y_latente < 0

plot(x,y_obs,
     pch=16,
     col=ifelse(cens,"red","steelblue"),
     xlab="x",
     ylab="y",
     main="Datos censurados: MCO vs recta verdadera")

abline(beta0,beta1,
       col="darkgreen",
       lwd=3)

abline(modelo_mco,
       col="orange",
       lwd=3,
       lty=2)

legend("topleft",
       legend=c(
         "Recta verdadera (β = 1.5)",
         paste0("MCO (β = ",round(pend_mco,3),")"),
         "Obs. censuradas (y = 0)",
         "Obs. no censuradas"
       ),
       col=c("darkgreen","orange","red","steelblue"),
       lwd=c(3,3,NA,NA),
       pch=c(NA,NA,16,16),
       lty=c(1,2,NA,NA))

cat("\nInterpretacion:\n")
cat("La linea verde representa la relacion verdadera.\n")
cat("La linea naranja es la estimacion MCO con datos censurados.\n")
cat("Observa que la pendiente MCO es menor.\n")

cat("\nEsto ocurre porque los valores negativos se censuran en 0.\n")
cat("Esto empuja la nube de puntos hacia abajo y reduce la pendiente.\n")

cat("\nEste fenomeno se llama SESGO DE ATENUACION.\n")

pausa()

# =============================================================================
# =============================================================================
# MCO SOLO POSITIVOS — Comparación con MCO completo
# =============================================================================

cat("\nPASO 5: MCO SOLO CON HORAS POSITIVAS\n")
cat("------------------------------------------------------------\n\n")

datos_pos <- datos[datos$hours>0,]
cat(sprintf("  Muestra completa: %d obs | Solo positivas: %d obs (%.1f%%)\n\n",
            nrow(datos), nrow(datos_pos), 100*nrow(datos_pos)/nrow(datos)))

mco_pos <- lm(hours ~ education + age + youngkids + oldkids + fincome, data=datos_pos)

# --- TABLA COMPARATIVA ---
cat("  Comparación MCO (toda la muestra) vs MCO (solo horas > 0):\n\n")
vv <- c("education","age","youngkids","oldkids","fincome")
labs <- c("Educación","Edad","Hijos pequeños","Hijos mayores","Renta familiar")
ct_all <- coef(summary(mco_todos))
ct_pos <- coef(summary(mco_pos))
cat(sprintf("  %-16s | %9s %8s | %9s %8s\n",
            "Variable", "MCO todo","EE", "MCO y>0","EE"))
cat(sprintf("  %s\n", paste(rep("-",58), collapse="")))
for (i in seq_along(vv)) {
  v <- vv[i]
  cat(sprintf("  %-16s | %9.3f %8.3f | %9.3f %8.3f\n",
              labs[i], ct_all[v,1], ct_all[v,2], ct_pos[v,1], ct_pos[v,2]))
}

cat("\n  INTERPRETACIÓN:\n")
cat("  Al eliminar las observaciones censuradas (hours=0), los coeficientes\n")
cat("  cambian porque la muestra está TRUNCADA: solo observamos a quienes\n")
cat("  decidieron trabajar. Esto introduce SESGO DE SELECCIÓN.\n")
cat("  Ni MCO con todos ni MCO con positivos estima correctamente el efecto.\n")
cat("  Para eso necesitamos el modelo Tobit (Paso 6).\n")
pausa()

# --- GRÁFICO ACTUALIZADO: añadir recta MCO-positivos a la simulación ---
cat("\n  Actualizando gráfico con MCO solo positivos...\n\n")

x_pos <- x[!cens]
y_pos <- y_obs[!cens]
mco_pos_sim <- lm(y_pos ~ x_pos)

plot(x, y_obs,
     pch=16,
     col=ifelse(cens, gray(0.7), gray(0.3)),
     cex=0.7,
     xlab="x", ylab="y",
     main="Comparación: MCO completo vs MCO truncado vs Verdadera")

abline(beta0, beta1, col=gray(0.0), lwd=3, lty=1)        # Verdadera
abline(modelo_mco, col=gray(0.4), lwd=2.5, lty=2)         # MCO todos
abline(coef(mco_pos_sim)[1], coef(mco_pos_sim)[2],
       col=gray(0.6), lwd=2.5, lty=3)                      # MCO positivos

legend("topleft",
       legend=c(
         sprintf("Verdadera (pend = %.3f)", beta1),
         sprintf("MCO todos (pend = %.3f)", coef(modelo_mco)[2]),
         sprintf("MCO y>0  (pend = %.3f)", coef(mco_pos_sim)[2]),
         "Censuradas (y = 0)",
         "No censuradas"
       ),
       col=c(gray(0.0), gray(0.4), gray(0.6), gray(0.7), gray(0.3)),
       lwd=c(3, 2.5, 2.5, NA, NA),
       pch=c(NA, NA, NA, 16, 16),
       lty=c(1, 2, 3, NA, NA),
       cex=0.75, bty="n")

cat("  El gráfico muestra tres rectas:\n")
cat("  - Verdadera (sólida): la relación real.\n")
cat("  - MCO todos (discontinua): sesgada hacia abajo por la censura.\n")
cat("  - MCO y>0 (punteada): menos sesgada pero aún incorrecta\n")
cat("    (sesgo de selección por truncamiento).\n")
pausa()

# TOBIT
# =============================================================================
# TOBIT — Estimación y comparación con MCO
# =============================================================================

cat("\nPASO 6: MODELO TOBIT\n")
cat("------------------------------------------------------------\n\n")

tobit_mroz <- censReg(hours ~ education + age + youngkids + oldkids + fincome,
                      left=0, data=datos)

sigma <- exp(coef(tobit_mroz)["logSigma"])
beta_t <- coef(tobit_mroz)[1:6]

# --- TABLA: MCO todos vs MCO positivos vs Tobit ---
cat("  Comparación de estimadores: MCO (todos) vs MCO (y>0) vs Tobit:\n\n")
vv <- c("(Intercept)","education","age","youngkids","oldkids","fincome")
labs <- c("Intercepto","Educación","Edad","Hijos pequeños","Hijos mayores","Renta familiar")
ct_all <- coef(summary(mco_todos))
ct_pos <- coef(summary(mco_pos))

cat(sprintf("  %-16s | %9s | %9s | %9s\n", "Variable", "MCO todo", "MCO y>0", "Tobit"))
cat(sprintf("  %s\n", paste(rep("-",54), collapse="")))
for (i in seq_along(vv)) {
  v <- vv[i]
  cat(sprintf("  %-16s | %9.2f | %9.2f | %9.2f\n",
              labs[i], ct_all[v,1], ct_pos[v,1], beta_t[v]))
}
cat(sprintf("  %-16s | %9s | %9s | %9.2f\n", "Sigma", "-", "-", sigma))

cat("\n  INTERPRETACIÓN:\n")
cat("  Los coeficientes Tobit son mayores (en valor absoluto) que los de MCO,\n")
cat("  porque MCO está sesgado hacia cero por la censura.\n")
cat("  Los coeficientes Tobit miden el efecto sobre la VARIABLE LATENTE\n")
cat("  (el deseo de trabajar), no directamente sobre las horas observadas.\n")
cat("  Para obtener el efecto sobre las horas observadas, necesitamos\n")
cat("  los efectos marginales (Paso 7).\n")
cat(sprintf("\n  Sigma = %.2f es la desviación estándar del error del modelo latente.\n", sigma))

pausa()

# EFECTOS MARGINALES
# =============================================================================

cat("\nPASO 7: EFECTOS MARGINALES PROMEDIO\n")
cat("------------------------------------------------------------\n")

beta <- coef(tobit_mroz)[1:6]

X <- model.matrix(~ education + age + youngkids + oldkids + fincome,data=datos)

xb <- as.numeric(X%*%beta)

Phi <- pnorm(xb/sigma)

beta_vars <- beta[-1]

ME <- outer(Phi,beta_vars,"*")

AME <- colMeans(ME)

tabla <- data.frame(
  Variable=names(beta_vars),
  Beta=round(beta_vars,3),
  AME=round(AME,3)
)

print(tabla)
tabla_dt(tabla, titulo = "Efectos marginales medios (AME)", filas = FALSE)

phi_media <- round(mean(Phi),3)

cat("\nPhi media:",phi_media,"\n")

cat("\nInterpretacion:\n")
cat("Los coeficientes beta miden el efecto sobre la variable latente.\n")
cat("Los AME miden el efecto real sobre hours observada.\n")
cat("Solo",round(phi_media*100,1),"% del efecto latente se materializa.\n")

pausa()

# =============================================================================
# TEST LR
# =============================================================================

cat("\nPASO 8: TEST DE RAZON DE VEROSIMILITUD\n")
cat("------------------------------------------------------------\n")

tobit_sin_kids <- censReg(hours ~ education + age + fincome,left=0,data=datos)

ll1 <- suppressWarnings(logLik(tobit_mroz))
ll0 <- suppressWarnings(logLik(tobit_sin_kids))

lr <- abs(2*(as.numeric(ll1)-as.numeric(ll0)))

pval <- pchisq(lr,df=2,lower.tail=FALSE)

cat("Estadistico LR:",round(lr,4),"\n")
cat("p-valor:",round(pval,5),"\n")

if(pval<0.05){
  cat("Se rechaza H0: las variables de hijos son significativas.\n")
}else{
  cat("No se rechaza H0.\n")
}

pausa()

# =============================================================================
# =============================================================================
# DIAGNÓSTICO DE RESIDUOS
# =============================================================================

cat("\nPASO 9: DIAGNÓSTICO DE RESIDUOS\n")
cat("------------------------------------------------------------\n\n")

tobit_vg <- vglm(hours ~ education + age + youngkids + oldkids + fincome,
                 VGAM::tobit(Lower=0), data=datos)

datos$yhat <- fitted(tobit_vg)[,1]
datos$res  <- resid(tobit_vg, type="response")

par(mfrow=c(2,2), mar=c(4.5,4.5,2.5,0.5))

# Panel 1: Residuos vs Valores ajustados
plot(datos$yhat, datos$res, pch=1, col=gray(0.4), cex=0.5,
     xlab="Valores ajustados (horas predichas)",
     ylab="Residuos",
     main="Residuos vs Ajustados")
abline(h=0, lty=2, lwd=1.5, col=gray(0.2))

# Panel 2: QQ-plot de normalidad
qqnorm(datos$res, pch=1, col=gray(0.4), cex=0.5,
       main="QQ-Plot (normalidad)")
qqline(datos$res, lwd=1.5, lty=2, col=gray(0.2))

# Panel 3: Residuos vs Horas observadas
plot(datos$hours, datos$res, pch=1, col=gray(0.4), cex=0.5,
     xlab="Horas observadas",
     ylab="Residuos",
     main="Residuos vs Observado")
abline(h=0, lty=2, lwd=1.5, col=gray(0.2))

# Panel 4: Ajustados vs Observados
plot(datos$hours, datos$yhat, pch=1, col=gray(0.4), cex=0.5,
     xlab="Horas observadas",
     ylab="Horas predichas",
     main="Predicho vs Observado")
abline(0, 1, lty=2, lwd=1.5, col=gray(0.2))

par(mfrow=c(1,1))

cat("  INTERPRETACIÓN de los 4 paneles:\n\n")
cat("  1. Residuos vs Ajustados: si hay patrón (embudo), hay heterocedasticidad.\n")
cat("  2. QQ-Plot: si los puntos se desvían de la diagonal, los errores\n")
cat("     no son normales (el Tobit asume normalidad).\n")
cat("  3. Residuos vs Observado: la acumulación en hours=0 es esperable\n")
cat("     (son las observaciones censuradas).\n")
cat("  4. Predicho vs Observado: los puntos deben seguir la diagonal.\n")
cat("     Desviaciones indican mala especificación.\n")

pausa()

# =============================================================================
# TESTS
# =============================================================================

cat("\nPASO 10: TESTS FORMALES\n")
cat("------------------------------------------------------------\n")

no_cens <- datos$hours>0

shap <- shapiro.test(datos$res[no_cens])

print(shap)

bart <- bartlett.test(res ~ (education>=13),data=datos[no_cens,])

print(bart)

cat("\nInterpretacion:\n")

if(shap$p.value<0.05){
  cat("Se rechaza normalidad de errores.\n")
}else{
  cat("Errores compatibles con normalidad.\n")
}

if(bart$p.value<0.05){
  cat("Existe heterocedasticidad.\n")
}else{
  cat("No hay evidencia de heterocedasticidad.\n")
}

cat("\n============================================================\n")
cat("FIN DEL ANALISIS\n")
cat("============================================================\n")