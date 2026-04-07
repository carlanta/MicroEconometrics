# =============================================================================
# TEMA 08 — CP1: Elección de Medio de Transporte (Multinomial)
# =============================================================================
# Manual de Microeconometría — Carlos de Anta Puig
# https://github.com/carlanta/MicroEconometrics  Versión 1.0 — 2026
# INSTRUCCIONES: Session > Set Working Directory > To Source File Location
# =============================================================================
pausa <- function(msg="\n>>> Pulsa ENTER para continuar...") {
  if (interactive()) readline(msg) else Sys.sleep(0.5)
}
pkgs <- c("nnet","marginaleffects")
for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, quiet=TRUE)
suppressPackageStartupMessages({ library(nnet) })
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
cat("  CP01 — Elección de Medio de Transporte (Multinomial)\n")
cat("================================================================\n\n")

load(file.path(DATA_DIR, "T08_CP01_transporte.RData"))
cat("Dataset:", nrow(transporte), "individuos\n")
cat(sprintf("  Alternativas: %s\n\n",
    paste(names(table(transporte$eleccion)),
          table(transporte$eleccion), sep="=", collapse=", ")))

# --- DESCRIPTIVOS ---
cat("--- ESTADÍSTICOS DESCRIPTIVOS ---\n\n")
vars <- c("ingreso","distancia","edad","zona_urbana")
labs <- c("Ingreso (miles)","Distancia (km)","Edad","Zona urbana")
cat(sprintf("  %-16s %8s %8s %8s %8s\n", "Variable","Media","D.E.","Mín.","Máx."))
cat(sprintf("  %s\n", paste(rep("-",54), collapse="")))
for (i in seq_along(vars)) {
  x <- transporte[[vars[i]]]
  cat(sprintf("  %-16s %8.2f %8.2f %8.1f %8.1f\n", labs[i],
              mean(x), sd(x), min(x), max(x)))
}
pausa()

# --- EDA ---
png(file.path(OUTPUT_DIR, "T08_CP01_eda.png"), width=1400, height=700, res=150)
par(mfrow=c(1,2), mar=c(4.5,4.5,2.5,0.5), cex.main=0.88)
barplot(table(transporte$eleccion), col=gray(c(0.3,0.55,0.80)),
        border="white", main="Distribución elección", ylab="Frecuencia")
boxplot(ingreso ~ eleccion, data=transporte, col=gray(c(0.3,0.55,0.80)),
        border=gray(0.2), main="Ingreso por elección", ylab="Ingreso")
dev.off()
cat("\n  Gráfico: output/T08_CP01_eda.png\n")
cat("  Los usuarios de coche tienen ingresos más altos en promedio.\n")
pausa()

# --- MULTINOMIAL ---
cat("\n--- LOGIT MULTINOMIAL (base = coche) ---\n\n")
ml <- multinom(eleccion ~ ingreso + distancia + edad + zona_urbana,
               data=transporte, trace=FALSE)
cf <- coef(ml)
se <- summary(ml)$standard.errors
cat(sprintf("  %-12s | %9s %9s %9s %9s %9s\n", "",
            "Intercept","ingreso","distancia","edad","urbana"))
cat(sprintf("  %s\n", paste(rep("-",64), collapse="")))
for (alt in rownames(cf)) {
  cat(sprintf("  %-12s | %9.4f %9.4f %9.4f %9.4f %9.4f\n",
              alt, cf[alt,1], cf[alt,2], cf[alt,3], cf[alt,4], cf[alt,5]))
}
cat("\n  INTERPRETACIÓN:\n")
cat("  Un coef. positivo indica que la variable favorece esa alternativa\n")
cat("  respecto al coche. Ingreso negativo: los ricos prefieren coche.\n")
pausa()

# --- AME ---
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
  cat("\n  Los AME indican el cambio en PROBABILIDAD (no log-odds).\n")
}
pausa()

# --- CLASIFICACIÓN ---
cat("\n--- BONDAD DEL AJUSTE ---\n\n")
pred <- predict(ml)
tab <- table(Observado=transporte$eleccion, Predicho=pred)
cat("  Tabla de clasificación:\n")
print(tab)
cat(sprintf("\n  %% correcto: %.1f%%\n", 100*sum(diag(tab))/sum(tab)))
pausa()

# --- GRÁFICO ---
png(file.path(OUTPUT_DIR, "T08_CP01_probs.png"), width=1200, height=600, res=150)
par(mar=c(4.5, 4.5, 2.5, 6))
tab_zona <- prop.table(table(transporte$zona_urbana, transporte$eleccion), margin=1)
barplot(t(tab_zona), beside=TRUE, col=gray(c(0.3,0.55,0.80)),
        border="white", names.arg=c("No urbana","Urbana"),
        ylab="Proporción", main="Elección por zona")
legend("topright", c("Coche","Bus","Tren"), fill=gray(c(0.3,0.55,0.80)),
       border="white", cex=0.8, bty="n", inset=c(-0.15,0))
dev.off()
cat("  Gráfico: output/T08_CP01_probs.png\n")

cat("\n================================================================\n")
cat("  RESUMEN CP01\n")
cat("================================================================\n")
cat(sprintf("  %% correcto: %.1f%%\n", 100*sum(diag(tab))/sum(tab)))
cat("  Ingreso favorece coche; distancia favorece tren; zona urbana → bus.\n")
cat("================================================================\n")
