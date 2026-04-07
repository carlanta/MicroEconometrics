# =============================================================================
# TEMA 07 — Modelos de Duración: Kaplan-Meier, Cox y Paramétricos
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Ilustrar el análisis de supervivencia completo con datos
#           simulados: KM, log-rank, Cox, proporcionalidad, paramétricos.
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
pkgs <- c("survival","lmtest")
for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, quiet=TRUE)
suppressPackageStartupMessages({ library(survival) })
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
cat("  TEMA 07 — Modelos de Duración\n")
cat("================================================================\n\n")

# =============================================================================
# PARTE 1: DATOS SIMULADOS
# =============================================================================
cat("--- SIMULACIÓN: DURACIÓN CON DOS GRUPOS ---\n\n")
cat("  Generamos duraciones Weibull con un tratamiento que cambia el riesgo.\n")
cat("  Grupo control: lambda=0.10, shape=1.2 (riesgo creciente)\n")
cat("  Grupo tratado: lambda=0.15, shape=1.2 (mayor riesgo → menor duración)\n\n")

set.seed(2026)
n <- 600
grupo <- rbinom(n, 1, 0.5)
t_real <- rweibull(n, shape=1.2, scale=1/ifelse(grupo==1, 0.15, 0.10))
t_cens <- runif(n, 5, 30)
tiempo <- pmin(t_real, t_cens)
evento <- as.integer(t_real <= t_cens)

cat(sprintf("  n = %d observaciones\n", n))
cat(sprintf("  Eventos: %d (%.0f%%) | Censurados: %d (%.0f%%)\n",
            sum(evento), 100*mean(evento), sum(1-evento), 100*mean(1-evento)))
cat(sprintf("  Grupo control: %d | Grupo tratado: %d\n\n",
            sum(grupo==0), sum(grupo==1)))
pausa()

# --- DESCRIPTIVOS ---
cat("--- ESTADÍSTICOS DESCRIPTIVOS POR GRUPO ---\n\n")
cat(sprintf("  %-12s | %8s %8s %8s %8s\n", "Grupo","Media","Mediana","Mín.","Máx."))
cat(sprintf("  %s\n", paste(rep("-",50), collapse="")))
for (g in 0:1) {
  ti <- tiempo[grupo==g]
  cat(sprintf("  %-12s | %8.2f %8.2f %8.2f %8.2f\n",
              ifelse(g==0,"Control","Tratado"),
              mean(ti), median(ti), min(ti), max(ti)))
}
cat(sprintf("\n  La duración media del grupo tratado (%.1f) es menor\n",
            mean(tiempo[grupo==1])))
cat(sprintf("  que la del control (%.1f), como esperamos.\n",
            mean(tiempo[grupo==0])))
pausa()

# =============================================================================
# PARTE 2: KAPLAN-MEIER
# =============================================================================
cat("\n--- KAPLAN-MEIER ---\n\n")
surv_obj <- Surv(tiempo, evento)
km_all <- survfit(surv_obj ~ 1)
km_grp <- survfit(surv_obj ~ grupo)

cat("  Supervivencia global:\n")
cat(sprintf("    Mediana: %.2f meses\n", summary(km_all)$table["median"]))
cat(sprintf("    S(6 meses): %.3f (%.1f%% siguen en el estado a los 6 meses)\n",
            summary(km_all, times=6)$surv, 100*summary(km_all, times=6)$surv))

cat("\n  Supervivencia por grupo:\n")
med_grp <- summary(km_grp)$table[,"median"]
cat(sprintf("    Control:  mediana = %.2f meses\n", med_grp[1]))
cat(sprintf("    Tratado:  mediana = %.2f meses\n", med_grp[2]))
pausa()

# --- TEST LOG-RANK ---
cat("\n--- TEST DE LOG-RANK ---\n\n")
lr <- survdiff(surv_obj ~ grupo)
p_lr <- pchisq(lr$chisq, df=1, lower.tail=FALSE)
cat(sprintf("  H0: Las curvas de supervivencia son iguales\n"))
cat(sprintf("  Chi² = %.2f | p-valor = %.4f\n", lr$chisq, p_lr))
if (p_lr < 0.05) {
  cat("  ✓ Se RECHAZA H0: las duraciones difieren entre grupos.\n")
} else {
  cat("  ✗ No se rechaza H0.\n")
}
pausa()

# --- GRÁFICO KM ---
cat("\n--- GRÁFICO KAPLAN-MEIER ---\n\n")
png(file.path(OUTPUT_DIR, "T07_kaplan_meier.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)

plot(km_all, lwd=2, col=gray(0.3), mark.time=TRUE,
     xlab="Tiempo (meses)", ylab="S(t)", main="KM global",
     conf.int=TRUE)
abline(h=0.5, lty=3, col=gray(0.6))
text(max(tiempo)*0.6, 0.55, "Mediana", cex=0.7, font=3)

plot(km_grp, lwd=2, lty=c(1,2), col=gray(c(0.2,0.5)),
     xlab="Tiempo (meses)", ylab="S(t)", main="KM por grupo")
legend("topright", c("Control","Tratado"),
       lty=c(1,2), lwd=2, col=gray(c(0.2,0.5)), cex=0.8, bty="n")
abline(h=0.5, lty=3, col=gray(0.6))
text(2, 0.1, sprintf("Log-rank p = %.4f", p_lr), cex=0.7, font=2)
dev.off()
cat("  Gráfico: output/T07_kaplan_meier.png\n")
cat("  La curva del grupo tratado desciende más rápido: mayor riesgo.\n")
pausa()

# =============================================================================
# PARTE 3: MODELO DE COX
# =============================================================================
cat("\n--- MODELO DE COX ---\n\n")
cox <- coxph(surv_obj ~ grupo)
s_cox <- summary(cox)

cat(sprintf("  Coeficiente (grupo): %.4f\n", coef(cox)))
cat(sprintf("  Hazard Ratio:        %.4f  (= exp(%.4f))\n",
            exp(coef(cox)), coef(cox)))
cat(sprintf("  IC 95%% del HR:       [%.3f, %.3f]\n",
            exp(confint(cox))[1], exp(confint(cox))[2]))
cat(sprintf("  EE:                  %.4f\n", s_cox$coef[1,"se(coef)"]))
cat(sprintf("  p-valor:             %.4f\n", s_cox$coef[1,"Pr(>|z|)"]))
cat(sprintf("  Concordancia:        %.3f\n", s_cox$concordance[1]))

hr <- exp(coef(cox))
cat(sprintf("\n  INTERPRETACIÓN: El grupo tratado tiene un riesgo instantáneo\n"))
cat(sprintf("  %.0f%% %s que el control en cada momento.\n",
            abs(100*(hr-1)), ifelse(hr>1, "MAYOR", "MENOR")))
cat(sprintf("  Esto implica una duración %s para el grupo tratado.\n",
            ifelse(hr>1, "MENOR", "MAYOR")))
pausa()

# --- PROPORCIONALIDAD ---
cat("\n--- DIAGNÓSTICO: PROPORCIONALIDAD ---\n\n")
zph <- cox.zph(cox)
cat(sprintf("  Test de Schoenfeld para 'grupo':\n"))
cat(sprintf("    rho = %.4f | chi² = %.3f | p = %.4f\n",
            zph$table[1,1], zph$table[1,2], zph$table[1,3]))
if (zph$table[1,3] > 0.05) {
  cat("  ✓ No se rechaza proporcionalidad. Cox es adecuado.\n")
} else {
  cat("  ✗ Se rechaza. Considerar estratificación o interacción con t.\n")
}
pausa()

# =============================================================================
# PARTE 4: MODELOS PARAMÉTRICOS
# =============================================================================
cat("\n--- MODELOS PARAMÉTRICOS ---\n\n")
tiempo_pos <- pmax(tiempo, 0.01)
surv_pos <- Surv(tiempo_pos, evento)

exp_mod <- survreg(surv_pos ~ grupo, dist="exponential")
wei_mod <- survreg(surv_pos ~ grupo, dist="weibull")
lnorm_mod <- survreg(surv_pos ~ grupo, dist="lognormal")

cat(sprintf("  %-14s | %9s %9s %9s\n", "Modelo","Coef(grupo)","AIC","Shape"))
cat(sprintf("  %s\n", paste(rep("-",48), collapse="")))
cat(sprintf("  %-14s | %9.4f %9.1f %9s\n", "Exponencial",
            coef(exp_mod)["grupo"], AIC(exp_mod), "-"))
cat(sprintf("  %-14s | %9.4f %9.1f %9.3f\n", "Weibull",
            coef(wei_mod)["grupo"], AIC(wei_mod), 1/wei_mod$scale))
cat(sprintf("  %-14s | %9.4f %9.1f %9s\n", "Log-normal",
            coef(lnorm_mod)["grupo"], AIC(lnorm_mod), "-"))
cat(sprintf("  %-14s | %9.4f %9s %9s\n", "Cox",
            coef(cox), "-", "-"))

mejor <- which.min(c(AIC(exp_mod), AIC(wei_mod), AIC(lnorm_mod)))
nombres <- c("Exponencial","Weibull","Log-normal")
cat(sprintf("\n  Mejor AIC: %s (%.1f)\n", nombres[mejor],
            min(c(AIC(exp_mod), AIC(wei_mod), AIC(lnorm_mod)))))

shape_w <- 1/wei_mod$scale
cat(sprintf("\n  Weibull shape = %.3f: ", shape_w))
if (shape_w > 1) {
  cat("hazard CRECIENTE (dependencia positiva).\n")
  cat("  Cuanto más tiempo llevas, más probable es el evento.\n")
} else if (shape_w < 1) {
  cat("hazard DECRECIENTE (dependencia negativa).\n")
  cat("  Cuanto más tiempo llevas, más difícil es que ocurra.\n")
} else {
  cat("hazard CONSTANTE (equivale al exponencial).\n")
}
pausa()

# --- GRÁFICO SUPERVIVENCIA AJUSTADA ---
png(file.path(OUTPUT_DIR, "T07_supervivencia_ajustada.png"),
    width=1200, height=600, res=150)
par(mar=c(4.5, 4.5, 2.5, 1))
plot(km_grp, lwd=1.5, lty=c(1,2), col=gray(c(0.5,0.5)),
     xlab="Tiempo (meses)", ylab="S(t)",
     main="KM (empírica) vs Weibull (paramétrica)", conf.int=FALSE)
# Superponer Weibull ajustado
t_seq <- seq(0.01, max(tiempo), length.out=200)
for (g in 0:1) {
  lam_g <- exp(-predict(wei_mod, newdata=data.frame(grupo=g), type="lp"))
  s_g <- exp(-(lam_g * t_seq)^(1/wei_mod$scale))
  lines(t_seq, s_g, lwd=2.5, lty=ifelse(g==0,1,2), col=gray(0.2))
}
legend("topright", c("KM Control","KM Tratado",
                      "Weibull Control","Weibull Tratado"),
       lty=c(1,2,1,2), lwd=c(1.5,1.5,2.5,2.5),
       col=gray(c(0.5,0.5,0.2,0.2)), cex=0.7, bty="n")
dev.off()
cat("\n  Gráfico: output/T07_supervivencia_ajustada.png\n")
cat("  Las curvas Weibull (sólidas oscuras) se ajustan bien a las KM.\n")
pausa()

# =============================================================================
# PARTE 5: RESUMEN Y GUÍA DE COMANDOS
# =============================================================================
cat("\n================================================================\n")
cat("  RESUMEN DEL ANÁLISIS\n")
cat("================================================================\n\n")
cat(sprintf("  Mediana KM control: %.1f | tratado: %.1f\n",
            med_grp[1], med_grp[2]))
cat(sprintf("  Log-rank: p = %.4f (diferencia significativa)\n", p_lr))
cat(sprintf("  Cox HR: %.3f (grupo tratado +%.0f%% riesgo)\n",
            hr, 100*(hr-1)))
cat(sprintf("  Weibull shape: %.3f (%s)\n", shape_w,
            ifelse(shape_w>1,"creciente","decreciente")))
cat(sprintf("  Proporcionalidad: %s\n",
            ifelse(zph$table[1,3]>0.05,"OK","Violada")))

cat("\n\n--- GUÍA DE COMANDOS R PARA DURACIÓN ---\n\n")
cat("  Surv(tiempo, evento)         Crear objeto de supervivencia\n")
cat("  survfit(surv ~ grupo)        Kaplan-Meier (global o por grupos)\n")
cat("  survdiff(surv ~ grupo)       Test de log-rank\n")
cat("  coxph(surv ~ x1 + x2)       Modelo de Cox\n")
cat("  cox.zph(modelo_cox)          Test de proporcionalidad (Schoenfeld)\n")
cat("  survreg(surv ~ x, dist=...)  Modelo paramétrico (weibull, lognormal)\n")
cat("  exp(coef(cox))               Hazard ratios\n")
cat("  exp(confint(cox))            IC 95% de los hazard ratios\n\n")

cat("================================================================\n")
cat("  Fin del script T07\n")
cat("================================================================\n")
