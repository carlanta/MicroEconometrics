# =============================================================================
# TEMA 03 — CP3: Modelo Tobit — Gasto en Formación Profesional
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Analizar el gasto mensual en formación profesional con Tobit.
#           El 39.3% no invierte nada. Comparar MCO y Tobit e interpretar AME.
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
pkgs <- c("AER","lmtest")
for (p in pkgs) if (!requireNamespace(p,quietly=TRUE)) install.packages(p,quiet=TRUE)
suppressPackageStartupMessages({ library(AER); library(lmtest) })
DATA_DIR <- "../data"; OUTPUT_DIR <- "output"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

cat("\n================================================================\n")
cat("  CP03 — Gasto en Formación Profesional: Modelo Tobit\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR,"T03_CP03_gasto_formacion.RData"))
cat("Dataset:", nrow(formacion), "trabajadores\n")
n_f <- sum(formacion$gasto_formacion==0)
cat(sprintf("  Sin inversión en formación: %d (%.1f%%)\n",
            n_f, 100*n_f/nrow(formacion)))
cat(sprintf("  Con inversión: %d (%.1f%%) | Media: %.1f€/mes\n\n",
            nrow(formacion)-n_f, 100*(nrow(formacion)-n_f)/nrow(formacion),
            mean(formacion$gasto_formacion[formacion$gasto_formacion>0])))

cat("--- EDA: INVERSIÓN MEDIA POR PERFIL ---\n\n")
cat(sprintf("  Sector privado vs público: %.1f vs %.1f €/mes\n",
            mean(formacion$gasto_formacion[formacion$sector_privado==1]),
            mean(formacion$gasto_formacion[formacion$sector_privado==0])))
cat(sprintf("  Con desempleo previo vs sin: %.1f vs %.1f €/mes\n",
            mean(formacion$gasto_formacion[formacion$desempleo_previo==1]),
            mean(formacion$gasto_formacion[formacion$desempleo_previo==0])))
cat(sprintf("  Hombres vs mujeres: %.1f vs %.1f €/mes\n",
            mean(formacion$gasto_formacion[formacion$sexo==0]),
            mean(formacion$gasto_formacion[formacion$sexo==1])))

png(file.path(OUTPUT_DIR,"T03_CP03_eda.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2,0.5))
hist(formacion$gasto_formacion, breaks=30, col=gray(0.70), border="white",
     main="Distribución gasto formación", xlab="€/mes", ylab="Frecuencia")
n_fc <- sum(formacion$gasto_formacion==0)
rect(par("usr")[1],0,0.8,n_fc,col=gray(0.25),border=NA)
text(1,n_fc*0.6,sprintf("%d sin\ninversión\n(%.1f%%)",
     n_fc,100*n_fc/nrow(formacion)),pos=4,cex=0.75,font=2)
boxplot(gasto_formacion ~ sector_privado, data=formacion,
        names=c("Público","Privado"), col=gray(c(0.80,0.45)), border=gray(0.2),
        main="Gasto por sector", ylab="€/mes")
dev.off()
cat("\n  Gráfico: output/T03_CP03_eda.png\n")

pausa()

cat("\n--- ESTIMACIÓN MCO vs TOBIT ---\n\n")
fml_f <- gasto_formacion ~ educacion + ingreso + edad + sector_privado +
          desempleo_previo + sexo + antiguedad
mco_f <- lm(fml_f, data=formacion)
tob_f <- tobit(fml_f, left=0, data=formacion)

ct_f   <- coef(summary(tob_f))
vars_f <- c("educacion","ingreso","edad","sector_privado","desempleo_previo","sexo","antiguedad")
cat("  Variable          | MCO     | Tobit   | Sig. | Ratio\n")
cat("  -----------------------------------------------------------\n")
for (v in vars_f) {
  sig <- ifelse(ct_f[v,4]<0.001,"***",ifelse(ct_f[v,4]<0.01,"**",
               ifelse(ct_f[v,4]<0.05,"*","   ")))
  r_val <- if (abs(coef(mco_f)[v])>0.001) coef(tob_f)[v]/coef(mco_f)[v] else NA
  cat(sprintf("  %-17s | %7.3f | %7.3f | %3s  | %s\n", v,
              coef(mco_f)[v], coef(tob_f)[v], sig,
              if (!is.na(r_val)) sprintf("%.2f",r_val) else "—"))
}
cat(sprintf("  sigma:             | %-7s | %7.3f\n","—",tob_f$scale))

pausa()

cat("\n--- EFECTOS MARGINALES AME ---\n\n")
beta_f <- coef(tob_f); sig_f <- tob_f$scale
xb_f   <- predict(tob_f, newdata=formacion, type="lp")
z_f    <- xb_f / sig_f
Phi_f  <- pnorm(z_f)
ame_f  <- mean(Phi_f) * beta_f[vars_f]

cat("  Variable          | AME (€/mes) | Efecto\n")
cat("  -----------------------------------------------\n")
for (v in vars_f) {
  efecto <- ifelse(ame_f[v]>0, sprintf("+%.2f€/mes",ame_f[v]),
                               sprintf("%.2f€/mes",ame_f[v]))
  cat(sprintf("  %-17s | %+10.3f  | %s\n", v, ame_f[v], efecto))
}

cat("\n▶ HALLAZGOS PRINCIPALES:\n")
cat(sprintf("  · Sector privado: trabajar en empresa privada aumenta la inversión\n"))
cat(sprintf("    en formación en %.2f€/mes (AME). La competencia del mercado\n", ame_f["sector_privado"]))
cat("    privado incentiva la formación continua.\n")
cat(sprintf("  · Desempleo previo: quienes han sufrido desempleo invierten %.2f€/mes\n", ame_f["desempleo_previo"]))
cat("    más en formación — estrategia preventiva ante nuevos episodios.\n")
cat(sprintf("  · Sexo: las mujeres invierten %.2f€/mes %s que los hombres,\n",
            abs(ame_f["sexo"]), ifelse(ame_f["sexo"]<0,"menos","más")))
cat("    una diferencia que puede reflejar menor disponibilidad de tiempo.\n")

cat("\n--- BONDAD DEL AJUSTE ---\n\n")
ll_n <- as.numeric(logLik(tobit(gasto_formacion~1, left=0, data=formacion)))
ll_m <- as.numeric(logLik(tob_f))
cat(sprintf("  McFadden R²: %.4f\n", 1-ll_m/ll_n))
cat(sprintf("  AIC:         %.2f\n", AIC(tob_f)))
cat(sprintf("  sigma:       %.3f\n\n", tob_f$scale))

cat("================================================================\n")
cat("  FIN DEL SCRIPT T03_CP03_mECO_Tobit_Formacion.R\n")
cat("================================================================\n\n")
