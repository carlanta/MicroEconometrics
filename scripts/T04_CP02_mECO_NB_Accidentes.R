# TEMA 04 — CP2: NB — Accidentes de Tráfico por Tramo de Carretera
# Manual de Microeconometría — Carlos de Anta Puig — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
for (p in c("AER","MASS")) if (!requireNamespace(p,quietly=TRUE)) install.packages(p,quiet=TRUE)
suppressPackageStartupMessages({ library(AER); library(MASS) })
DATA_DIR <- "../data"; OUTPUT_DIR <- "output"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

cat("\n================================================================\n")
cat("  CP02 — Accidentes de Tráfico: Poisson y Binomial Negativa\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR,"T04_CP02_accidentes.RData"))
mu_a <- mean(accid$accidentes); var_a <- var(accid$accidentes)
cat(sprintf("  n=%d | Media=%.2f | Varianza=%.2f | VMR=%.2f | Ceros: %.1f%%\n\n",
            nrow(accid), mu_a, var_a, var_a/mu_a, 100*mean(accid$accidentes==0)))
cat("  La ILUMINACIÓN y la VELOCIDAD MÁXIMA son los factores de riesgo\n")
cat("  más conocidos en la literatura de seguridad vial.\n")

pausa()

cat("\n--- TEST DE SOBREDISPERSIÓN Y ESTIMACIÓN ---\n\n")
fml_a <- accidentes ~ trafico_diario + velocidad_max + num_curvas + dias_lluvia + iluminacion + longitud_km
p_a   <- glm(fml_a, data=accid, family=poisson)
nb_a  <- glm.nb(fml_a, data=accid)
dt_a  <- dispersiontest(p_a, trafo=2)

cat(sprintf("  Test Cameron-Trivedi: alpha=%.4f, z=%.2f, p=%.6f\n",
            dt_a$estimate, dt_a$statistic, dt_a$p.value))
cat(sprintf("  → %s\n\n", if(dt_a$p.value<0.05) "✓ Sobredispersión confirmada. NB preferida."
             else "Equidispersión no rechazada."))

vars_a <- c("trafico_diario","velocidad_max","num_curvas","dias_lluvia","iluminacion","longitud_km")
ct_a   <- coef(summary(nb_a))
irr_a  <- exp(coef(nb_a)[vars_a])
ame_a  <- mean(fitted(nb_a)) * coef(nb_a)[vars_a]

cat("  Variable          | IRR     | AME  | Sig.\n")
cat("  -----------------------------------------------\n")
for (v in vars_a) {
  sig <- ifelse(ct_a[v,4]<0.001,"***",ifelse(ct_a[v,4]<0.01,"**",
               ifelse(ct_a[v,4]<0.05,"*","   ")))
  cat(sprintf("  %-17s | %6.3f  | %5.3f | %s\n", v, irr_a[v], ame_a[v], sig))
}

cat(sprintf("\n  theta (NB): %.4f | AIC NB=%.1f vs AIC Pois=%.1f\n",
            nb_a$theta, AIC(nb_a), AIC(p_a)))
cat("\n▶ INTERPRETACIONES PRINCIPALES:\n")
cat(sprintf("  · Iluminación (=1): IRR=%.3f → tramos iluminados tienen un %.0f%% MENOS\n",
            irr_a["iluminacion"], (1-irr_a["iluminacion"])*100))
cat("    accidentes que los tramos sin iluminar.\n")
cat(sprintf("  · Velocidad máxima: IRR=%.4f por km/h → a 120 vs 80 km/h se espera un\n",
            irr_a["velocidad_max"]))
cat(sprintf("    %.0f%% más de accidentes.\n", (irr_a["velocidad_max"]^40-1)*100))
cat(sprintf("  · Días de lluvia: IRR=%.4f → más días de lluvia = más accidentes.\n",
            irr_a["dias_lluvia"]))

png(file.path(OUTPUT_DIR,"T04_CP02_predichos.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2,0.5))
plot(fitted(p_a), fitted(nb_a), pch=1, col=gray(0.5), cex=0.7,
     xlab="Predichos Poisson", ylab="Predichos NB", main="Poisson vs NB predichos")
abline(0,1,lwd=2); legend("topleft","Línea 45°",lty=1,lwd=2,bty="n",cex=0.85)
resid_p <- residuals(p_a, type="pearson")
hist(resid_p, breaks=30, col=gray(0.70), border="white",
     main="Residuos Pearson (Poisson)", xlab="Residuo Pearson", ylab="Frecuencia")
dev.off()
cat("\n  Gráfico diagnóstico: output/T04_CP02_predichos.png\n")
cat("\n================================================================\n")
cat("  FIN T04_CP02_mECO_NB_Accidentes.R\n")
cat("================================================================\n\n")
