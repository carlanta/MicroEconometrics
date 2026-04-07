# =============================================================================
# TEMA 08 — Datos Cualitativos: Dummies, ANOVA, Multinomial y Ordenado
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Ilustrar el uso de variables cualitativas como regresores
#           (dummies, ANOVA, ANCOVA) y como variable dependiente
#           (Logit Multinomial, Probit Ordenado).
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
pkgs <- c("nnet","MASS","marginaleffects")
for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, quiet=TRUE)
suppressPackageStartupMessages({ library(nnet); library(MASS) })
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
cat("  TEMA 08 — Datos Cualitativos\n")
cat("================================================================\n\n")

# =============================================================================
# PARTE I: VARIABLES CUALITATIVAS COMO REGRESORES
# =============================================================================
cat("=== PARTE I: Variables cualitativas como regresores ===\n\n")

# --- DATOS SIMULADOS ---
set.seed(2026)
n <- 300
sector <- sample(c("Industria","Servicios","Tecnología","Construcción"), n, replace=TRUE)
experiencia <- runif(n, 1, 30)
sexo <- sample(c("Hombre","Mujer"), n, replace=TRUE)
salario <- 1200 + 400*(sector=="Tecnología") + 100*(sector=="Industria") -
           100*(sector=="Construcción") + 30*experiencia -
           150*(sexo=="Mujer") + rnorm(n, 0, 200)
datos <- data.frame(salario, sector=factor(sector), experiencia, sexo=factor(sexo))
cat("  Datos simulados: salario en función de sector, experiencia y sexo.\n")
cat(sprintf("  n = %d observaciones\n\n", n))

# --- ANOVA ---
cat("--- ANOVA: ¿Difiere el salario por sector? ---\n\n")
mod_anova <- lm(salario ~ sector, data=datos)
tab_anova <- anova(mod_anova)
cat("  Tabla ANOVA:\n")
cat(sprintf("  %-16s GL=%d  SC=%.0f  F=%.2f  p=%.6f\n",
            "sector", tab_anova$Df[1], tab_anova$"Sum Sq"[1],
            tab_anova$"F value"[1], tab_anova$"Pr(>F)"[1]))
if (tab_anova$"Pr(>F)"[1] < 0.05) {
  cat("  ✓ Se rechaza H0: al menos un sector tiene media distinta.\n\n")
} else {
  cat("  ✗ No se rechaza H0.\n\n")
}

cat("  Coeficientes (base = Construcción):\n")
ct <- coef(summary(mod_anova))
for (i in seq_len(nrow(ct))) {
  cat(sprintf("    %-24s %8.1f (p = %.4f)\n",
              rownames(ct)[i], ct[i,1], ct[i,4]))
}
cat("\n  Tecnología gana significativamente más que la base.\n")
pausa()

# --- GRÁFICO ANOVA ---
png(file.path(OUTPUT_DIR, "T08_anova.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)
boxplot(salario ~ sector, data=datos, col=gray(c(0.3,0.5,0.7,0.85)),
        border=gray(0.2), ylab="Salario (EUR/mes)", main="ANOVA: por sector",
        cex.axis=0.75)
# ANCOVA
plot(experiencia, salario, pch=ifelse(sexo=="Hombre",16,1),
     col=gray(ifelse(sexo=="Hombre",0.3,0.6)), cex=0.5,
     xlab="Experiencia (años)", ylab="Salario", main="ANCOVA: sexo + exp.")
m_anc <- lm(salario ~ experiencia + sexo, data=datos)
abline(a=coef(m_anc)[1], b=coef(m_anc)[2], lwd=2, lty=1)
abline(a=coef(m_anc)[1]+coef(m_anc)[3], b=coef(m_anc)[2], lwd=2, lty=2)
legend("topleft", c("Hombres","Mujeres"), pch=c(16,1),
       col=gray(c(0.3,0.6)), lty=c(1,2), cex=0.75, bty="n")
dev.off()
cat("\n  Gráfico: output/T08_anova.png\n")
pausa()

# --- ANCOVA ---
cat("\n--- ANCOVA: Sector + Experiencia + Sexo ---\n\n")
mod_ancova <- lm(salario ~ sector + experiencia + sexo, data=datos)
ct_anc <- coef(summary(mod_ancova))
cat(sprintf("  %-24s %9s %8s %10s\n", "Variable","Coef.","EE","p-valor"))
cat(sprintf("  %s\n", paste(rep("-",56), collapse="")))
for (i in seq_len(nrow(ct_anc))) {
  sig <- ifelse(ct_anc[i,4]<0.001,"***",ifelse(ct_anc[i,4]<0.01,"**",
               ifelse(ct_anc[i,4]<0.05,"*","   ")))
  cat(sprintf("  %-24s %9.1f %8.1f %10.6f %s\n",
              rownames(ct_anc)[i], ct_anc[i,1], ct_anc[i,2], ct_anc[i,4], sig))
}
cat(sprintf("\n  R² = %.4f\n", summary(mod_ancova)$r.squared))
cat("  ANCOVA controla por experiencia al comparar sectores y sexos.\n")
cat("  La brecha de género tras controlar por experiencia y sector:\n")
cat(sprintf("  %.0f EUR/mes.\n", coef(mod_ancova)["sexoMujer"]))
pausa()

# =============================================================================
# PARTE II: VD CUALITATIVA — MULTINOMIAL
# =============================================================================
cat("\n\n=== PARTE II: Variable dependiente cualitativa ===\n\n")

cat("--- LOGIT MULTINOMIAL: Elección de Transporte ---\n\n")
load(file.path(DATA_DIR, "T08_CP01_transporte.RData"))
cat("Dataset:", nrow(transporte), "individuos, 3 alternativas\n")
cat(sprintf("  %s\n\n", paste(names(table(transporte$eleccion)),
                              table(transporte$eleccion), sep=": ", collapse=" | ")))

ml <- multinom(eleccion ~ ingreso + distancia + edad + zona_urbana,
               data=transporte, trace=FALSE)
cat("  Coeficientes (base = coche):\n\n")
cf <- coef(ml)
cat(sprintf("  %-12s | %9s %9s %9s %9s %9s\n", "",
            "Intercept","ingreso","distancia","edad","urbana"))
cat(sprintf("  %s\n", paste(rep("-",64), collapse="")))
for (alt in rownames(cf)) {
  cat(sprintf("  %-12s | %9.4f %9.4f %9.4f %9.4f %9.4f\n",
              alt, cf[alt,1], cf[alt,2], cf[alt,3], cf[alt,4], cf[alt,5]))
}

cat("\n  INTERPRETACIÓN:\n")
cat("  ingreso negativo en bus y tren: los más ricos prefieren coche.\n")
cat("  distancia positiva: mayor distancia favorece transporte público.\n")
cat("  zona_urbana positiva en bus: la oferta urbana favorece el autobús.\n")
pausa()

# --- EFECTOS MARGINALES ---
cat("\n--- EFECTOS MARGINALES (AME) ---\n\n")
if (requireNamespace("marginaleffects", quietly=TRUE)) {
  me <- marginaleffects::avg_slopes(ml)
  me_df <- as.data.frame(me)
  for (grp in unique(me_df$group)) {
    cat(sprintf("  --- %s ---\n", grp))
    sub <- me_df[me_df$group == grp,]
    for (i in seq_len(nrow(sub))) {
      sig <- ifelse(sub$p.value[i]<0.05,"*","")
      cat(sprintf("    %-12s AME = %8.4f (p = %.4f) %s\n",
                  sub$term[i], sub$estimate[i], sub$p.value[i], sig))
    }
  }
}
pausa()

# --- TABLA DE CLASIFICACIÓN ---
cat("\n--- BONDAD DEL AJUSTE ---\n\n")
pred <- predict(ml)
tab_cl <- table(Observado=transporte$eleccion, Predicho=pred)
cat("  Tabla de clasificación:\n")
print(tab_cl)
cat(sprintf("\n  %% correcto: %.1f%%\n", 100*sum(diag(tab_cl))/sum(tab_cl)))
pausa()

# =============================================================================
# PARTE III: VD CUALITATIVA — ORDENADO
# =============================================================================
cat("\n--- PROBIT ORDENADO: Satisfacción Laboral ---\n\n")
load(file.path(DATA_DIR, "T08_CP02_satisfaccion_laboral.RData"))
satisf_lab$satisfaccion <- factor(satisf_lab$satisfaccion, ordered=TRUE)
cat("Dataset:", nrow(satisf_lab), "trabajadores, 5 niveles\n")
print(table(satisf_lab$satisfaccion))
cat("\n")

oprobit <- polr(satisfaccion ~ salario + horas_semanales + autonomia + antiguedad,
                data=satisf_lab, method="probit")
s_op <- summary(oprobit)
ct_op <- s_op$coefficients
cat("  Coeficientes del Probit Ordenado:\n\n")
for (i in seq_len(nrow(ct_op))) {
  pval <- 2*pnorm(-abs(ct_op[i,"t value"]))
  sig <- ifelse(pval<0.001,"***",ifelse(pval<0.01,"**",ifelse(pval<0.05,"*","   ")))
  cat(sprintf("  %-16s Coef: %8.4f | EE: %6.4f | t: %6.2f %s\n",
              rownames(ct_op)[i], ct_op[i,1], ct_op[i,2], ct_op[i,3], sig))
}
cat(sprintf("\n  Umbrales: %s\n",
            paste(sprintf("%.3f", oprobit$zeta), collapse=" | ")))
cat("\n  INTERPRETACIÓN:\n")
cat("  salario (+): mayor salario → mayor satisfacción.\n")
cat("  horas (-): más horas → menor satisfacción.\n")
cat("  autonomía (+): más autonomía → mayor satisfacción.\n")
cat("  NOTA: El signo de beta solo determina el efecto en las categorías\n")
cat("  extremas (1 y 5). En las intermedias, el efecto puede invertirse.\n")
pausa()

# =============================================================================
# RESUMEN Y GUÍA DE COMANDOS
# =============================================================================
cat("\n================================================================\n")
cat("  RESUMEN\n")
cat("================================================================\n\n")
cat("  PARTE I: Variables cualitativas como regresores\n")
cat("  - Las dummies permiten medir diferencias entre grupos.\n")
cat("  - ANOVA = solo dummies. ANCOVA = dummies + continuas.\n")
cat("  - R crea dummies automáticamente con factor().\n\n")
cat("  PARTE II: VD cualitativa multinomial\n")
cat("  - Logit Multinomial: multinom() del paquete nnet.\n")
cat("  - Los coeficientes son log-odds relativos a la base.\n")
cat("  - Los AME (avg_slopes) dan el efecto sobre probabilidades.\n\n")
cat("  PARTE III: VD cualitativa ordenada\n")
cat("  - Probit/Logit Ordenado: polr() del paquete MASS.\n")
cat("  - Variable latente + umbrales → probabilidades por categoría.\n")

cat("\n\n--- GUÍA DE COMANDOS R ---\n\n")
cat("  factor(x)                  Convertir a factor (crea dummies)\n")
cat("  relevel(x, ref='base')     Cambiar categoría de referencia\n")
cat("  lm(y ~ factor_var)         ANOVA (solo dummies)\n")
cat("  lm(y ~ factor + continua)  ANCOVA\n")
cat("  lm(y ~ factor * continua)  ANCOVA con interacción\n")
cat("  anova(modelo)              Tabla ANOVA (F-test por variable)\n")
cat("  multinom(y ~ x, trace=F)   Logit Multinomial (nnet)\n")
cat("  polr(y ~ x, method=...)    Probit/Logit Ordenado (MASS)\n")
cat("  avg_slopes(modelo)         Efectos marginales (marginaleffects)\n\n")

cat("================================================================\n")
cat("  Fin del script T08\n")
cat("================================================================\n")
