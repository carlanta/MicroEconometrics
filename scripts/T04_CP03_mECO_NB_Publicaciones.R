# TEMA 04 — CP3: NB — Publicaciones Académicas de Investigadores
# Manual de Microeconometría — Carlos de Anta Puig — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
for (p in c("AER","MASS")) if (!requireNamespace(p,quietly=TRUE)) install.packages(p,quiet=TRUE)
suppressPackageStartupMessages({ library(AER); library(MASS) })
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

cat("\n================================================================\n")
cat("  CP03 — Publicaciones Académicas: Sobredispersión Severa\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR,"T04_CP03_publicaciones.RData"))
mu_i <- mean(invest$publicaciones); var_i <- var(invest$publicaciones)
cat(sprintf("  n=%d | Media=%.2f | Varianza=%.2f | VMR=%.1f\n\n",
            nrow(invest), mu_i, var_i, var_i/mu_i))

cat("  VMR = %.1f >> 1: sobredispersión MUY SEVERA.\n", var_i/mu_i)
cat("  Usar Poisson aquí sería un error grave: EE subestimados en un factor\n")
cat(sprintf("  de aproximadamente sqrt(%.1f) ≈ %.1f.\n\n", var_i/mu_i, sqrt(var_i/mu_i)))

pausa()

fml_i  <- publicaciones ~ experiencia + financiacion + doctorado + h_index_previo + sexo + colaboradores
p_i    <- glm(fml_i, data=invest, family=poisson)
nb_i   <- glm.nb(fml_i, data=invest)
dt_i   <- dispersiontest(p_i, trafo=2)

cat("--- TEST DE SOBREDISPERSIÓN ---\n\n")
cat(sprintf("  Cameron-Trivedi: alpha=%.4f, z=%.2f, p < 0.001\n\n",
            dt_i$estimate, dt_i$statistic))

vars_i <- c("experiencia","financiacion","doctorado","h_index_previo","sexo","colaboradores")
ct_p   <- coef(summary(p_i)); ct_nb <- coef(summary(nb_i))
irr_i  <- exp(coef(nb_i)[vars_i])
ame_i  <- mean(fitted(nb_i)) * coef(nb_i)[vars_i]

cat("--- COMPARACIÓN EE: POISSON INFLA SIGNIFICATIVIDAD ---\n\n")
cat("  Variable          | EE Poisson | EE NB   | Factor  | NB sig.\n")
cat("  -------------------------------------------------------------------\n")
for (v in vars_i) {
  sig_nb <- ifelse(ct_nb[v,4]<0.001,"***",ifelse(ct_nb[v,4]<0.01,"**",
                  ifelse(ct_nb[v,4]<0.05,"*","n.s.")))
  cat(sprintf("  %-17s | %10.4f | %7.4f | x%.2f  | %s\n", v,
              ct_p[v,2], ct_nb[v,2],
              ct_nb[v,2]/ct_p[v,2], sig_nb))
}

cat("\n--- IRR Y AME (modelo NB) ---\n\n")
cat("  Variable          | IRR     | AME (pub/año)\n")
cat("  ------------------------------------------------\n")
for (v in vars_i) {
  cat(sprintf("  %-17s | %6.3f  | %+.3f\n", v, irr_i[v], ame_i[v]))
}

cat("\n▶ HALLAZGOS PRINCIPALES:\n")
cat(sprintf("  · Doctorado: IRR=%.3f → los investigadores doctores publican un\n",
            irr_i["doctorado"]))
cat(sprintf("    %.0f%% más que los no doctores (manteniendo todo lo demás constante).\n",
            (irr_i["doctorado"]-1)*100))
cat(sprintf("  · H-index previo: IRR=%.3f por unidad → la reputación acumulada\n",
            irr_i["h_index_previo"]))
cat("    amplifica la producción futura (efecto Matthew).\n")
cat(sprintf("  · Sexo (mujer=1): IRR=%.3f → las investigadoras publican un %.0f%% menos,\n",
            irr_i["sexo"], (1-irr_i["sexo"])*100))
cat("    una brecha de género estadísticamente significativa.\n")

cat(sprintf("\n  AIC Poisson: %.1f | AIC NB: %.1f | Δ AIC: %.1f → NB claramente mejor.\n",
            AIC(p_i), AIC(nb_i), AIC(p_i)-AIC(nb_i)))

png(file.path(OUTPUT_DIR,"T04_CP03_distribucion.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2,0.5))
k_max <- min(max(invest$publicaciones),40)
obs_f <- table(factor(pmin(invest$publicaciones,40),levels=0:40))/nrow(invest)
pred_nb_f <- dnbinom(0:40, mu=mean(fitted(nb_i)), size=nb_i$theta)
plot(0:40-0.2, as.numeric(obs_f), type="h", lwd=3, lty=1,
     xlab="Publicaciones", ylab="Proporción",
     main="Obs. vs Poisson vs NB")
lines(0:40, dpois(0:40, mean(invest$publicaciones)), type="h",
      lwd=2, lty=3, col=gray(0.7))
lines(0:40+0.2, pred_nb_f, type="h", lwd=2, lty=2, col=gray(0.4))
legend("topright", legend=c("Observado","Poisson","NB"),
       lty=c(1,3,2), lwd=c(3,2,2), col=c("black",gray(0.7),gray(0.4)), bty="n", cex=0.8)
boxplot(publicaciones ~ doctorado, data=invest,
        names=c("Sin doctorado","Con doctorado"),
        col=gray(c(0.80,0.45)), border=gray(0.2),
        main="Publicaciones por doctorado", ylab="N.º publicaciones")
dev.off()
cat("\n  Gráfico: output/T04_CP03_distribucion.png\n")
cat("\n================================================================\n")
cat("  FIN T04_CP03_mECO_NB_Publicaciones.R\n")
cat("================================================================\n\n")
