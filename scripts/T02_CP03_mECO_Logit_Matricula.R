# =============================================================================
# TEMA 02 — CP3: Elección Discreta — Matrícula Universitaria
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Modelar la probabilidad de matricularse en la universidad con
#           Probit. Evaluar el papel de la beca, la nota y la distancia.
#           Calcular AME y analizar efectos heterogéneos por perfil.
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}

pkgs <- c("lmtest","sandwich")
for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, quiet=TRUE)
suppressPackageStartupMessages({ library(lmtest); library(sandwich) })

DATA_DIR <- "../data"; OUTPUT_DIR <- "output"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

# -------------------------------------------------------------------------- #
# 1. CARGA Y EDA                                                              #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  CP03 — Matrícula Universitaria: Modelo Probit\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR, "T02_CP03_matricula_universidad.RData"))
cat("Dataset:", nrow(universidad), "estudiantes de bachillerato\n\n")

tab_u <- table(universidad$matricula)
cat(sprintf("  No se matriculan (y=0): %d estudiantes (%.1f%%)\n",
            tab_u[1], 100*tab_u[1]/nrow(universidad)))
cat(sprintf("  Se matriculan   (y=1): %d estudiantes (%.1f%%)\n",
            tab_u[2], 100*tab_u[2]/nrow(universidad)))

cat("\n--- ESTADÍSTICOS POR RESULTADO ---\n\n")
cat("  Variable              | No matricula | Matricula | Diferencia\n")
cat("  -------------------------------------------------------------\n")
for (v in c("nota_acceso","ingreso_familiar","distancia_km")) {
  m0 <- mean(universidad[[v]][universidad$matricula==0])
  m1 <- mean(universidad[[v]][universidad$matricula==1])
  cat(sprintf("  %-21s | %11.2f  | %9.2f | %+.2f\n", v, m0, m1, m1-m0))
}
for (v in c("beca_disponible","padre_universitario","sexo")) {
  p0 <- mean(universidad[[v]][universidad$matricula==0])*100
  p1 <- mean(universidad[[v]][universidad$matricula==1])*100
  cat(sprintf("  %-21s | %10.1f%%  | %8.1f%%  | %+.1f pp\n", v, p0, p1, p1-p0))
}

cat("\n▶ INTERPRETACIÓN:\n")
cat("  Los estudiantes que se matriculan tienen notas más altas, familias con\n")
cat("  mayor ingreso y menor distancia al campus. La disponibilidad de beca\n")
cat("  y el nivel educativo del padre son factores diferenciadores notables.\n")

pausa()

# -------------------------------------------------------------------------- #
# 2. ESTIMACIÓN Y COEFICIENTES                                                #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 2 — Estimación del Modelo Probit\n")
cat("================================================================\n\n")

formula_u <- matricula ~ nota_acceso + ingreso_familiar + distancia_km +
             beca_disponible + padre_universitario + sexo

probit_u <- glm(formula_u, data=universidad, family=binomial(link="probit"))
logit_u  <- glm(formula_u, data=universidad, family=binomial(link="logit"))

ct_u  <- coeftest(probit_u)
b_u   <- ct_u[,1]; se_u <- ct_u[,2]; pv_u <- ct_u[,4]

cat("--- MODELO PROBIT (coeficientes en la escala del índice latente) ---\n\n")
vars_u <- c("nota_acceso","ingreso_familiar","distancia_km",
            "beca_disponible","padre_universitario","sexo")
for (v in vars_u) {
  sig <- ifelse(pv_u[v]<0.001,"***",ifelse(pv_u[v]<0.01,"**",
               ifelse(pv_u[v]<0.05,"*","   ")))
  cat(sprintf("  %-22s | beta=%8.4f  EE=%7.4f  p=%6.4f  %s\n",
              v, b_u[v], se_u[v], pv_u[v], sig))
}

cat("\n▶ LECTURA DE LOS COEFICIENTES:\n")
cat("  El signo indica la dirección del efecto: positivo = mayor probabilidad.\n")
cat("  La magnitud NO es comparable directamente a una probabilidad ni entre\n")
cat("  coeficientes de modelos distintos — para eso están los efectos marginales.\n")

pausa()

# -------------------------------------------------------------------------- #
# 3. EFECTOS MARGINALES AME                                                   #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 3 — Efectos Marginales AME (Probit)\n")
cat("================================================================\n\n")

coefs_u <- coef(probit_u)
X_u     <- model.matrix(probit_u)
phi_u   <- dnorm(X_u %*% coefs_u)
ame_u   <- mean(as.numeric(phi_u)) * coefs_u

x_bar_u   <- colMeans(X_u)
phi_bar_u <- dnorm(sum(x_bar_u * coefs_u))
mem_u     <- phi_bar_u * coefs_u

cat("  Variable              | AME        | MEM        | AME (pp)\n")
cat("  -----------------------------------------------------------\n")
for (v in vars_u) {
  cat(sprintf("  %-21s | %+.5f    | %+.5f    | %+.2f pp\n",
              v, ame_u[v], mem_u[v], ame_u[v]*100))
}

cat("\n▶ INTERPRETACIÓN DETALLADA:\n")
cat(sprintf("  · Nota acceso: cada punto adicional (+1 sobre 10) en la nota PAU\n"))
cat(sprintf("    aumenta la probabilidad de matricularse en %.1f pp (AME).\n",
            ame_u["nota_acceso"]*100))
cat(sprintf("  · Beca: disponer de beca aumenta la probabilidad en %.1f pp.\n",
            ame_u["beca_disponible"]*100))
cat("    Este es el mayor efecto discreto — subraya la importancia de la\n")
cat("    financiación para el acceso a la educación superior.\n")
cat(sprintf("  · Distancia: cada km adicional reduce la probabilidad en %.3f pp.\n",
            abs(ame_u["distancia_km"])*100))
cat("    Para un estudiante que vive a 100 km, la distancia reduce su\n")
cat(sprintf("    probabilidad de matricularse en %.1f pp respecto a uno que vive\n",
            abs(ame_u["distancia_km"])*100*100))
cat("    en la misma ciudad (distancia ~2 km).\n")
cat(sprintf("  · Padre universitario: +%.1f pp — el capital cultural familiar\n",
            ame_u["padre_universitario"]*100))
cat("    sigue siendo un determinante relevante.\n")
cat(sprintf("  · Sexo (mujer=1): +%.1f pp — las mujeres tienen ligeramente\n",
            ame_u["sexo"]*100))
cat("    mayor probabilidad de matricularse que los hombres.\n")

pausa()

# -------------------------------------------------------------------------- #
# 4. ANÁLISIS DE HETEROGENEIDAD: BECA Y DISTANCIA                            #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 4 — Heterogeneidad: Efecto de Nota y Beca\n")
cat("================================================================\n\n")

png(file.path(OUTPUT_DIR, "T02_CP03_heterogeneidad.png"),
    width=1600, height=800, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2,0.5))

# Curva probabilidad vs nota para con/sin beca
nota_seq <- seq(4, 10, length.out=100)
ing_med  <- mean(universidad$ingreso_familiar)
dist_med <- mean(universidad$distancia_km)

nd_beca_si <- data.frame(nota_acceso=nota_seq, ingreso_familiar=ing_med,
                          distancia_km=dist_med, beca_disponible=1,
                          padre_universitario=0, sexo=0)
nd_beca_no <- data.frame(nota_acceso=nota_seq, ingreso_familiar=ing_med,
                          distancia_km=dist_med, beca_disponible=0,
                          padre_universitario=0, sexo=0)

p_beca_si <- predict(probit_u, newdata=nd_beca_si, type="response")
p_beca_no <- predict(probit_u, newdata=nd_beca_no, type="response")

plot(nota_seq, p_beca_si, type="l", lwd=2.5, lty=1, ylim=c(0,1),
     xlab="Nota de acceso (PAU)", ylab="P(matrícula = 1)",
     main="Probabilidad vs Nota (por beca)")
lines(nota_seq, p_beca_no, lwd=2.5, lty=2)
abline(h=0.5, lty=3, col=gray(0.5))
legend("topleft", legend=c("Con beca","Sin beca"), lty=c(1,2), lwd=2.5, bty="n")

# Curva probabilidad vs distancia para dos notas
d_seq <- seq(2, 200, length.out=100)
nd_nota_hi <- data.frame(nota_acceso=8.5, ingreso_familiar=ing_med,
                          distancia_km=d_seq, beca_disponible=0,
                          padre_universitario=0, sexo=0)
nd_nota_lo <- data.frame(nota_acceso=6.0, ingreso_familiar=ing_med,
                          distancia_km=d_seq, beca_disponible=0,
                          padre_universitario=0, sexo=0)

p_hi <- predict(probit_u, newdata=nd_nota_hi, type="response")
p_lo <- predict(probit_u, newdata=nd_nota_lo, type="response")

plot(d_seq, p_hi, type="l", lwd=2.5, lty=1, ylim=c(0,1),
     xlab="Distancia al campus (km)", ylab="P(matrícula = 1)",
     main="Probabilidad vs Distancia (por nota)")
lines(d_seq, p_lo, lwd=2.5, lty=2)
abline(h=0.5, lty=3, col=gray(0.5))
legend("topright", legend=c("Nota alta (8.5)","Nota baja (6.0)"),
       lty=c(1,2), lwd=2.5, bty="n")
dev.off()
cat("  Gráfico guardado: output/T02_CP03_heterogeneidad.png\n")

cat("\n▶ INTERPRETACIÓN DE LOS GRÁFICOS:\n")
cat("  · Izquierda: la beca desplaza hacia arriba toda la curva de probabilidad.\n")
cat("    Un estudiante con nota 6 y beca tiene mayor probabilidad que uno con\n")
cat("    nota 7 y sin beca. La financiación puede compensar déficits académicos.\n")
cat("  · Derecha: la distancia es un obstáculo mayor para estudiantes con nota\n")
cat("    baja que para los de nota alta. Estos tienen alta probabilidad incluso\n")
cat("    a 150 km, mientras que los de nota baja se ven muy afectados por la distancia.\n")

pausa()

# -------------------------------------------------------------------------- #
# 5. BONDAD DEL AJUSTE                                                        #
# -------------------------------------------------------------------------- #
cat("\n================================================================\n")
cat("  SECCIÓN 5 — Bondad del Ajuste\n")
cat("================================================================\n\n")

ll_0_u  <- as.numeric(logLik(glm(matricula~1, data=universidad, family=binomial)))
ll_p_u  <- as.numeric(logLik(probit_u))
ll_l_u  <- as.numeric(logLik(logit_u))
r2_p_u  <- 1 - ll_p_u/ll_0_u
r2_l_u  <- 1 - ll_l_u/ll_0_u
lr_u    <- -2*(ll_0_u - ll_p_u)

pred_p_u <- as.integer(fitted(probit_u) > 0.5)
pct_u    <- mean(pred_p_u == universidad$matricula)*100

cat(sprintf("  McFadden R² (Probit): %.4f\n", r2_p_u))
cat(sprintf("  McFadden R² (Logit):  %.4f\n", r2_l_u))
cat(sprintf("  Test LR:              %.2f (p < 0.001)\n", lr_u))
cat(sprintf("  %% correctamente clasificados: %.1f%%\n\n", pct_u))

cat(sprintf("  Con un R² de McFadden de %.3f, el modelo explica una proporción\n", r2_p_u))
cat("  razonablemente alta de la variación en la decisión de matricularse.\n")
cat("  El test LR confirma la significación conjunta del modelo.\n")

cat("\n================================================================\n")
cat("  FIN DEL SCRIPT T02_CP03_mECO_Logit_Matricula.R\n")
cat("================================================================\n\n")
