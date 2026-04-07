# =============================================================================
# TEMA 05 — CP3: Panel — Salarios y Formación
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# Profesor de Econometría y Microeconometría — UNIR
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
#
# OBJETIVO: Modelar log(salario) de 500 trabajadores × 5 años.
#           Habilidad (no observada) correlaciona con formación → EF necesario.
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================

pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
pkgs <- c("plm","lmtest","sandwich")
for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, quiet=TRUE)
suppressPackageStartupMessages({ library(plm); library(lmtest); library(sandwich) })
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
cat("  CP03 — Salarios y Formación: Panel 500 trabajadores × 5 años\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR, "T05_CP03_salarios_formacion.RData"))
cat("Dataset:", nrow(panel_sal), "observaciones\n")
cat("  Trabajadores:", length(unique(panel_sal$id_trabajador)), "\n")
cat("  Período:", min(panel_sal$anio), "-", max(panel_sal$anio), "\n\n")

# --- DESCRIPTIVOS ---
cat("--- ESTADÍSTICOS DESCRIPTIVOS ---\n\n")
vars <- c("log_salario","experiencia","horas_formacion","antiguedad")
labs <- c("Log(salario)","Experiencia","Horas formación","Antigüedad")
cat(sprintf("  %-16s %10s %10s %10s %10s\n", "Variable","Media","D.E.","Mín.","Máx."))
cat(sprintf("  %s\n", paste(rep("-",60), collapse="")))
for (i in seq_along(vars)) {
  x <- panel_sal[[vars[i]]]
  cat(sprintf("  %-16s %10.3f %10.3f %10.3f %10.3f\n", labs[i], mean(x), sd(x), min(x), max(x)))
}
cat(sprintf("\n  Mujeres: %d (%.1f%%)\n",
            sum(panel_sal$sexo[!duplicated(panel_sal$id_trabajador)]),
            100*mean(panel_sal$sexo[!duplicated(panel_sal$id_trabajador)])))
pausa()

# --- EDA GRÁFICO ---
cat("\n--- ANÁLISIS EXPLORATORIO GRÁFICO ---\n\n")
png(file.path(OUTPUT_DIR, "T05_CP03_eda.png"), width=1600, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)

hist(panel_sal$log_salario, breaks=35, col=gray(0.70), border="white",
     main="Distribución log(salario)", xlab="Log(salario)", ylab="Frecuencia")

plot(panel_sal$horas_formacion, panel_sal$log_salario, pch=1, col=gray(0.5), cex=0.4,
     xlab="Horas de formación", ylab="Log(salario)",
     main="Salario vs Formación")
abline(lm(log_salario ~ horas_formacion, data=panel_sal), lwd=2, lty=1)
dev.off()
cat("  Gráfico guardado: output/T05_CP03_eda.png\n")
pausa()

# --- ESTIMACIÓN ---
cat("\n--- ESTIMACIÓN: POOLED, EF, EA ---\n\n")
pdata <- pdata.frame(panel_sal, index=c("id_trabajador","anio"))
fml <- log_salario ~ experiencia + horas_formacion + antiguedad
mod_p  <- plm(fml, data=pdata, model="pooling")
mod_fe <- plm(fml, data=pdata, model="within")
mod_re <- plm(fml, data=pdata, model="random")

# EA con sexo (invariante en t)
fml2 <- log_salario ~ experiencia + horas_formacion + antiguedad + sexo
mod_re2 <- plm(fml2, data=pdata, model="random")

vv <- c("experiencia","horas_formacion","antiguedad")
ct_p <- coef(summary(mod_p)); ct_f <- coef(summary(mod_fe)); ct_r <- coef(summary(mod_re))
cat(sprintf("  %-16s | %9s %8s | %9s %8s | %9s %8s\n",
            "Variable","Pooled","EE","EF","EE","EA","EE"))
cat(sprintf("  %s\n", paste(rep("-",76), collapse="")))
for (v in vv) {
  cat(sprintf("  %-16s | %9.5f %8.5f | %9.5f %8.5f | %9.5f %8.5f\n",
              v, ct_p[v,1], ct_p[v,2], ct_f[v,1], ct_f[v,2], ct_r[v,1], ct_r[v,2]))
}
cat("\n  INTERPRETACIÓN (EF):\n")
cat(sprintf("  Un año más de experiencia → +%.2f%% en salario\n",
            100*coef(mod_fe)["experiencia"]))
cat(sprintf("  Una hora más de formación → +%.3f%% en salario\n",
            100*coef(mod_fe)["horas_formacion"]))
cat(sprintf("  Un año más de antigüedad  → +%.2f%% en salario\n",
            100*coef(mod_fe)["antiguedad"]))

# Efecto sexo solo en EA
ct2 <- coef(summary(mod_re2))
cat(sprintf("\n  Efecto sexo (EA): Coef. = %.4f | p = %.4f\n",
            ct2["sexo",1], ct2["sexo",4]))
cat("  'sexo' es invariante en t → solo estimable con EA.\n")
pausa()

# --- TESTS ---
cat("\n--- TEST F: EFECTOS INDIVIDUALES ---\n\n")
ft <- pFtest(mod_fe, mod_p)
cat(sprintf("  F = %.2f | p-valor = %.6f\n", ft$statistic, ft$p.value))
if (ft$p.value < 0.05)
  cat("  ✓ Efectos individuales (habilidad) altamente significativos.\n\n")
pausa()

cat("--- TEST DE HAUSMAN: EF vs EA ---\n\n")
ht <- phtest(mod_fe, mod_re)
cat(sprintf("  H = %.2f | g.l. = %d | p-valor = %.4f\n",
            ht$statistic, ht$parameter, ht$p.value))
if (ht$p.value < 0.05) {
  cat("  ✓ Se rechaza H0 → EFECTOS FIJOS.\n")
  cat("    La habilidad no observada correlaciona con formación y experiencia.\n")
  cat("    Esto sesga Pooled OLS y EA: sobreestiman el retorno de la formación.\n\n")
} else {
  cat("  ✗ No se rechaza H0 → EA adecuado.\n\n")
}
pausa()

# --- COMPARACIÓN SESGO POOLED vs EF ---
cat("--- SESGO DE VARIABLE OMITIDA: POOLED vs EF ---\n\n")
cat(sprintf("  horas_formacion  Pooled: %.5f | EF: %.5f\n",
            coef(mod_p)["horas_formacion"], coef(mod_fe)["horas_formacion"]))
dif_pct <- 100*(coef(mod_p)["horas_formacion"] - coef(mod_fe)["horas_formacion"])/
           coef(mod_fe)["horas_formacion"]
cat(sprintf("  Diferencia: %.1f%%\n\n", dif_pct))
cat("  Pooled OLS sobreestima el retorno de la formación porque confunde\n")
cat("  el efecto de la formación con el de la habilidad no observada.\n")
cat("  Los más hábiles se forman más Y ganan más → sesgo positivo.\n")
pausa()

# --- EF CON ERRORES CLUSTERED ---
cat("\n--- EFECTOS FIJOS CON ERRORES CLUSTERED ---\n\n")
rob <- coeftest(mod_fe, vcov=vcovHC(mod_fe, type="HC1", cluster="group"))
cat(sprintf("  %-16s | %9s %8s %8s %10s\n", "Variable","Coef.","EE rob.","t","p-valor"))
cat(sprintf("  %s\n", paste(rep("-",58), collapse="")))
for (v in vv) {
  sig <- ifelse(rob[v,4]<0.001,"***",ifelse(rob[v,4]<0.01,"**",
               ifelse(rob[v,4]<0.05,"*","   ")))
  cat(sprintf("  %-16s | %9.5f %8.5f %8.3f %10.6f %s\n",
              v, rob[v,1], rob[v,2], rob[v,3], rob[v,4], sig))
}
pausa()

# --- PRIMERAS DIFERENCIAS ---
cat("\n--- PRIMERAS DIFERENCIAS (FD) ---\n\n")
mod_fd <- plm(fml, data=pdata, model="fd")
ct_fd <- coef(summary(mod_fd))
cat(sprintf("  %-16s | %9s %8s %10s\n", "Variable","Coef.","EE","p-valor"))
cat(sprintf("  %s\n", paste(rep("-",48), collapse="")))
ct_fd_names <- rownames(ct_fd)
for (v in ct_fd_names[ct_fd_names != "(Intercept)"]) {
  cat(sprintf("  %-16s | %9.5f %8.5f %10.6f\n", v, ct_fd[v,1], ct_fd[v,2], ct_fd[v,4]))
}
cat("\n  Con T=5, EF (within) es más eficiente que FD si errores son iid.\n")
cat("  FD sería preferible si los errores siguen un paseo aleatorio.\n")
pausa()

# --- GRÁFICO: COMPARACIÓN COEFICIENTES ---
png(file.path(OUTPUT_DIR, "T05_CP03_comparacion.png"), width=1200, height=600, res=150)
par(mar=c(5,10,2.5,1))
modelos <- c("Pooled","EF","EA","FD")
coefs_form <- c(coef(mod_p)["horas_formacion"], coef(mod_fe)["horas_formacion"],
                coef(mod_re)["horas_formacion"], coef(mod_fd)["horas_formacion"])
bp <- barplot(rev(coefs_form), horiz=TRUE, names.arg=rev(modelos),
              col=gray(c(0.3,0.5,0.65,0.80)), border="white",
              xlab="Coeficiente de horas_formacion", las=1,
              main="Sesgo por variable omitida")
abline(v=coefs_form[2], lty=2, lwd=1.5, col=gray(0.3))
text(coefs_form[2], 5.2, "EF (referencia)", cex=0.7, pos=4)
dev.off()
cat("  Gráfico: output/T05_CP03_comparacion.png\n")
pausa()

cat("\n================================================================\n")
cat("  RESUMEN CP03 — Salarios y Formación\n")
cat("================================================================\n\n")
cat("  1. Panel de 500 trabajadores × 5 años (2500 obs).\n")
cat("  2. Habilidad no observada sesga Pooled OLS hacia arriba.\n")
cat("  3. Hausman rechaza EA: la habilidad correlaciona con formación.\n")
cat("  4. EF corrige el sesgo: el retorno real de la formación es menor.\n")
cat("  5. 'sexo' (invariante) solo estimable con EA (brecha: ~8%).\n\n")
cat("================================================================\n")
