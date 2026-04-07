# =============================================================================
# TEMA 08 — CP3: Situación Laboral (Multinomial)
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================
pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
if (!requireNamespace("nnet", quietly=TRUE)) install.packages("nnet", quiet=TRUE)
suppressPackageStartupMessages(library(nnet))
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
cat("  CP03 — Situación Laboral (Multinomial)\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR, "T08_CP03_situacion_laboral.RData"))
cat("Dataset:", nrow(sit_lab), "individuos\n")
cat(sprintf("  Situaciones: %s\n\n",
    paste(names(table(sit_lab$situacion)),
          table(sit_lab$situacion), sep="=", collapse=", ")))

# --- DESCRIPTIVOS ---
cat("--- ESTADÍSTICOS DESCRIPTIVOS ---\n\n")
vars <- c("edad","educacion","experiencia","hijos","sexo")
labs <- c("Edad","Educación","Experiencia","Hijos","Sexo (mujer)")
cat(sprintf("  %-14s %8s %8s %8s %8s\n", "Variable","Media","D.E.","Mín.","Máx."))
cat(sprintf("  %s\n", paste(rep("-",50), collapse="")))
for (i in seq_along(vars)) {
  x <- sit_lab[[vars[i]]]
  cat(sprintf("  %-14s %8.2f %8.2f %8.1f %8.1f\n", labs[i],
              mean(x), sd(x), min(x), max(x)))
}
pausa()

# --- EDA ---
png(file.path(OUTPUT_DIR, "T08_CP03_eda.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)
barplot(table(sit_lab$situacion), col=gray(c(0.3,0.55,0.80)),
        border="white", main="Distribución situación", ylab="Frecuencia")
boxplot(educacion ~ situacion, data=sit_lab, col=gray(c(0.3,0.55,0.80)),
        border=gray(0.2), main="Educación por situación",
        ylab="Años educación")
dev.off()
cat("\n  Gráfico: output/T08_CP03_eda.png\n")
cat("  Los empleados tienen mayor nivel educativo en promedio.\n")
pausa()

# --- MULTINOMIAL ---
cat("\n--- LOGIT MULTINOMIAL (base = empleado) ---\n\n")
ml <- multinom(situacion ~ edad + educacion + experiencia + hijos + sexo,
               data=sit_lab, trace=FALSE)
cf <- coef(ml)
cat(sprintf("  %-14s | %8s %8s %8s %8s %8s %8s\n", "",
            "Intcpt","edad","educ","exper","hijos","sexo"))
cat(sprintf("  %s\n", paste(rep("-",66), collapse="")))
for (alt in rownames(cf)) {
  cat(sprintf("  %-14s | %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n",
              alt, cf[alt,1], cf[alt,2], cf[alt,3],
              cf[alt,4], cf[alt,5], cf[alt,6]))
}

cat("\n  INTERPRETACIÓN:\n")
cat("  Educación negativa en ambas → más educación reduce la prob. de\n")
cat("  desempleo e inactividad (respecto a empleo).\n")
cat("  Hijos positivo en inactivo → tener hijos aumenta la inactividad,\n")
cat("  especialmente combinado con sexo femenino.\n")
cat("  Sexo positivo en inactivo → las mujeres tienen mayor prob. de\n")
cat("  inactividad (no de desempleo).\n")
pausa()

# --- CLASIFICACIÓN ---
cat("\n--- BONDAD DEL AJUSTE ---\n\n")
pred <- predict(ml)
tab <- table(Observado=sit_lab$situacion, Predicho=pred)
cat("  Tabla de clasificación:\n")
print(tab)
cat(sprintf("\n  %% correcto: %.1f%%\n", 100*sum(diag(tab))/sum(tab)))
cat("\n  El modelo predice bien 'empleado' pero tiene dificultades con\n")
cat("  'desempleado' (categoría minoritaria: solo el 9.4% de la muestra).\n")
pausa()

# --- GRÁFICO ---
png(file.path(OUTPUT_DIR, "T08_CP03_sexo.png"), width=1200, height=600, res=150)
par(mar=c(4.5, 4.5, 2.5, 6))
tab_sx <- prop.table(table(sit_lab$sexo, sit_lab$situacion), margin=1)
barplot(t(tab_sx), beside=TRUE, col=gray(c(0.3,0.55,0.80)),
        border="white", names.arg=c("Hombre","Mujer"),
        ylab="Proporción", main="Situación laboral por sexo")
legend("topright", c("Empleado","Desempleado","Inactivo"),
       fill=gray(c(0.3,0.55,0.80)), border="white", cex=0.8, bty="n",
       inset=c(-0.15,0))
dev.off()
cat("  Gráfico: output/T08_CP03_sexo.png\n")

cat("\n================================================================\n")
cat("  RESUMEN CP03\n")
cat("================================================================\n")
cat(sprintf("  %% correcto: %.1f%%\n", 100*sum(diag(tab))/sum(tab)))
cat("  Educación y experiencia protegen contra desempleo e inactividad.\n")
cat("  Hijos y sexo femenino aumentan especialmente la inactividad.\n")
cat("================================================================\n")
