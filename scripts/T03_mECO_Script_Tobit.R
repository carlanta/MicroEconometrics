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
# MCO SOLO POSITIVOS
# =============================================================================

cat("\nPASO 5: MCO SOLO CON HORAS POSITIVAS\n")
cat("------------------------------------------------------------\n")

datos_pos <- datos[datos$hours>0,]

mco_pos <- lm(hours ~ education + age + youngkids + oldkids + fincome,data=datos_pos)

summary(mco_pos)

cat("\nInterpretacion:\n")
cat("Ahora eliminamos la censura.\n")
cat("Los coeficientes cambian porque la muestra esta truncada.\n")

pausa()

# =============================================================================
# TOBIT
# =============================================================================

cat("\nPASO 6: MODELO TOBIT\n")
cat("------------------------------------------------------------\n")

tobit_mroz <- censReg(hours ~ education + age + youngkids + oldkids + fincome,
                      left=0,data=datos)

suppressWarnings(summary(tobit_mroz))

sigma <- exp(coef(tobit_mroz)["logSigma"])

cat("\nSigma estimado:",round(sigma,2),"\n")

cat("\nInterpretacion:\n")
cat("Los coeficientes Tobit representan el efecto sobre la\n")
cat("variable latente (deseo de trabajar).\n")

pausa()

# =============================================================================
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
# DIAGNOSTICO
# =============================================================================

cat("\nPASO 9: DIAGNOSTICO DE RESIDUOS\n")
cat("------------------------------------------------------------\n")

tobit_vg <- vglm(hours ~ education + age + youngkids + oldkids + fincome,
                 VGAM::tobit(Lower=0),data=datos)

datos$yhat <- fitted(tobit_vg)[,1]
datos$res <- resid(tobit_vg,type="response")

par(mfrow=c(2,2))

plot(datos$yhat,datos$res)
abline(h=0,col="red")

qqnorm(datos$res)
qqline(datos$res)

plot(datos$hours,datos$res)

plot(datos$hours,datos$yhat)
abline(0,1,col="red")

par(mfrow=c(1,1))

cat("\nInterpretacion:\n")
cat("Los graficos permiten detectar heterocedasticidad\n")
cat("y desviaciones de normalidad.\n")

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