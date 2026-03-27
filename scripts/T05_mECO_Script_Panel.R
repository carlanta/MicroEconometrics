# =============================================================================
# TEMA 5 — DATOS DE PANEL: SCRIPT INTERACTIVO
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
#  Autor:   Carlos de Anta Puig
#           Economista · Perito Financiero
#           Miembro del Colegio de Economistas de Madrid
#           Miembro del Instituto Español de Analistas Financieros (IEAF)
#           Profesor de Econometría y Microeconometría
#  Email:   carlosmaria.deanta@unir.net
#
#  Control de version | Versión: 1.1 | Fecha: 2026-03-16
#
#  Descripción:
#    Script interactivo para aprender datos de panel paso a paso.
#    Usa readline() para pausar entre bloques y permite al alumno
#    reflexionar sobre los resultados antes de avanzar.
#
#  Dataset: Grunfeld (paquete plm) — 10 empresas, 20 años (1935-1954)
#
#  Bloques:
#    1. Carga de paquetes y datos
#    2. Exploración del panel (within/between)
#    3. Pooled OLS
#    4. Efectos Fijos (Within)
#    5. Primeras Diferencias
#    6. Efectos Aleatorios (GLS)
#    7. Test de Hausman y bateria de tests
#    8. Diagnóstico completo
#    9. Modelos con interacciónes
#   10. Panel dinámico y sesgo de Nickell
#   11. Resumen final
#
# =============================================================================

# ===== BLOQUE 1: CARGA DE PAQUETES Y DATOS ==================================

cat("\n")
cat("=============================================================\n")
cat("  TEMA 5 — DATOS DE PANEL: Script Interactivo\n")
cat("  Carlos de Anta Puig\n")
cat("=============================================================\n\n")
cat("Este script te guiará paso a paso por los modelos de datos\n")
cat("de panel: Pooled OLS, Efectos Fijos, Efectos Aleatorios,\n")
cat("tests de especificación, interacciónes y panel dinámico.\n\n")
cat("Usaremos el dataset clásico de Grunfeld (1958):\n")
cat("  - 10 empresas estadounidenses durante 20 años (1935-1954)\n")
cat("  - Variable dependiente: inversión bruta\n")
cat("  - Regresores: valor de mercado y stock de capital\n\n")

readline("Pulsa ENTER para cargar los paquetes...")

suppressPackageStartupMessages({
  library(plm)
  library(lmtest)
  library(sandwich)
  library(car)
  library(ggplot2)
  library(DT)
})

cat("\n[OK] Paquetes cargados: plm, lmtest, sandwich, car, ggplot2, DT\n\n")

# Funcion auxiliar: tabla interactiva con exportacion a Excel/CSV
tabla_dt <- function(datos, título = "", decimales = 2, filas = TRUE) {
  if (is.numeric(datos) && !is.null(names(datos))) {
    datos <- data.frame(Nombre = names(datos), Valor = round(datos, decimales))
    filas <- FALSE
  }
  DT::datatable(
    datos,
    caption = título,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel'),
      pageLength = 15,
      scrollX = TRUE
    ),
    rownames = filas
  )
}

# Cargar datos
data("Grunfeld", package = "plm")
pGrunfeld <- pdata.frame(Grunfeld, index = c("firm", "year"))

cat("Dataset Grunfeld:\n")
cat("  - Empresas (n):", pdim(pGrunfeld)$nT$n, "\n")
cat("  - Periodos (T):", pdim(pGrunfeld)$nT$T, "\n")
cat("  - Observaciones:", pdim(pGrunfeld)$nT$N, "\n")
cat("  - Panel balanceado:", pdim(pGrunfeld)$balanced, "\n\n")

cat("Variables:\n")
cat("  inv    = Inversión bruta (mill. $)\n")
cat("  value  = Valor de mercado (mill. $)\n")
cat("  capital = Stock de capital (mill. $)\n")
cat("  firm   = Identificador de empresa\n")
cat("  year   = Año\n\n")

print(summary(Grunfeld[, c("inv", "value", "capital")]))

cat("\n[Abriendo tabla interactiva de datos en el Viewer...]\n")
print(tabla_dt(Grunfeld, "Dataset Grunfeld completo (exportable a Excel/CSV)"))


# ===== BLOQUE 2: EXPLORACION DEL PANEL ======================================

readline("\n--- Pulsa ENTER para explorar la estructura del panel ---")

cat("\n=== DESCOMPOSICION DE LA VARIACION ===\n\n")
cat("Cualquier variable se puede descomponer en:\n")
cat("  x_it = media_i (between) + (x_it - media_i) (within)\n\n")

# Variación overall, between, within
sd_overall <- sd(Grunfeld$inv)
medias_inv <- tapply(Grunfeld$inv, Grunfeld$firm, mean)
sd_between <- sd(medias_inv)
Grunfeld$inv_within <- Grunfeld$inv - ave(Grunfeld$inv, Grunfeld$firm, FUN = mean)
sd_within <- sd(Grunfeld$inv_within)

cat("Variable: inv (Inversión)\n")
cat("  SD overall :", round(sd_overall, 2), "\n")
cat("  SD between :", round(sd_between, 2), "(entre empresas)\n")
cat("  SD within  :", round(sd_within, 2), "(dentro de cada empresa)\n\n")

ratio <- round(sd_between / sd_within, 2)
cat("  Ratio between/within:", ratio, "\n")
if (ratio > 1.5) {
  cat("  => Las diferencias ENTRE empresas dominan.\n")
  cat("     Esto sugiere heterogeneidad individual importante.\n")
} else {
  cat("  => La variación temporal DENTRO de cada empresa es relevante.\n")
}

readline("\n--- Pulsa ENTER para ver las trayectorias individuales ---")

cat("\n[Generando gráfico de trayectorias...]\n")
print(
  ggplot(Grunfeld, aes(x = year, y = inv, color = factor(firm), group = firm)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.2) +
    labs(x = "Año", y = "Inversión (mill. $)",
         title = "Trayectorias de inversión por empresa",
         color = "Empresa") +
    theme_minimal(base_size = 12)
)

cat("\nObserva:\n")
cat("  - Cada línea es una empresa.\n")
cat("  - Las diferencias de nivel entre líneas = variación between.\n")
cat("  - Las fluctuaciones de cada línea = variación within.\n")


# ===== BLOQUE 3: POOLED OLS =================================================

readline("\n--- Pulsa ENTER para estimar Pooled OLS ---")

cat("\n=== POOLED OLS ===\n")
cat("Ignora la estructura de panel: apila todas las observaciónes.\n")
cat("Modelo: inv ~ value + capital\n\n")

mod_pooled <- plm(inv ~ value + capital,
                  data = pGrunfeld,
                  model = "pooling")
print(summary(mod_pooled))

cat("\n--- Errores robustos agrupados (clustered by firm) ---\n\n")
robust_pooled <- coeftest(mod_pooled,
                          vcov = vcovHC(mod_pooled, type = "HC1", cluster = "group"))
print(robust_pooled)

cat("\nCompara los errores estándar clásicos vs robustos.\n")
cat("Si difieren mucho, hay correlación intra-grupo (efecto individual).\n")
cat("Pooled OLS sera INCONSISTENTE si alpha_i correlaciónado con X_it.\n")


# ===== BLOQUE 4: EFECTOS FIJOS (WITHIN) =====================================

readline("\n--- Pulsa ENTER para estimar Efectos Fijos ---")

cat("\n=== EFECTOS FIJOS (estimador Within) ===\n")
cat("Elimina alpha_i restando la media temporal de cada individuo.\n")
cat("Modelo: (y_it - y_bar_i) = (x_it - x_bar_i)' beta + (eps_it - eps_bar_i)\n\n")

mod_fe <- plm(inv ~ value + capital,
              data = pGrunfeld,
              model = "within")
print(summary(mod_fe))

cat("\n--- Efectos fijos individuales estimados ---\n")
ef <- fixef(mod_fe)
print(round(sort(ef, decreasing = TRUE), 2))

cat("\n[Abriendo tabla de efectos fijos en el Viewer...]\n")
ef_df <- data.frame(
  Empresa = names(ef),
  Alpha_i = round(as.numeric(ef), 2)
)
ef_df <- ef_df[order(-ef_df$Alpha_i), ]
print(tabla_dt(ef_df, "Efectos fijos individuales (exportable)", filas = FALSE))

cat("\nCada alpha_i es el nivel base de inversión de cada empresa,\n")
cat("una vez controlados value y capital.\n")

readline("\n--- Pulsa ENTER para ver la equivalencia con LSDV ---")

cat("\n=== EQUIVALENCIA LSDV (Least Squares Dummy Variables) ===\n")
cat("LSDV incluye una dummy por empresa. Produce el MISMO beta.\n\n")

mod_lsdv <- lm(inv ~ factor(firm) + value + capital, data = Grunfeld)
cat("Coeficientes FE (within):\n")
print(round(coef(mod_fe), 6))
cat("\nCoeficientes LSDV:\n")
print(round(coef(mod_lsdv)[c("value", "capital")], 6))
cat("\nDiferencia maxima:", max(abs(coef(mod_fe) - coef(mod_lsdv)[c("value", "capital")])), "\n")
cat("=> Son IDENTICOS (hasta precision numerica).\n")

readline("\n--- Pulsa ENTER para el test F: Pooled vs FE ---")

cat("\n=== TEST F: Pooled OLS vs Efectos Fijos ===\n")
cat("H0: alpha_1 = alpha_2 = ... = alpha_n (Pooled suficiente)\n")
cat("H1: Al menos un alpha_i difiere (FE necesario)\n\n")
ftest <- pFtest(mod_fe, mod_pooled)
print(ftest)

if (ftest$p.value < 0.05) {
  cat("\n=> RECHAZAMOS H0 (p =", format.pval(ftest$p.value, digits = 4), ")\n")
  cat("   Los efectos individuales son significativos.\n")
  cat("   Pooled OLS NO es adecuado. Necesitamos FE o RE.\n")
} else {
  cat("\n=> No rechazamos H0. Pooled OLS podria ser suficiente.\n")
}


# ===== BLOQUE 5: PRIMERAS DIFERENCIAS =======================================

readline("\n--- Pulsa ENTER para Primeras Diferencias ---")

cat("\n=== PRIMERAS DIFERENCIAS (FD) ===\n")
cat("Otra forma de eliminar alpha_i: Delta y_it = Delta x_it' beta + Delta eps_it\n\n")

mod_fd <- plm(inv ~ value + capital,
              data = pGrunfeld,
              model = "fd")
print(summary(mod_fd))

cat("\n--- Comparación FD vs FE ---\n")
cat("         value    capital\n")
cat("FE:  ", round(coef(mod_fe)["value"], 4), "  ", round(coef(mod_fe)["capital"], 4), "\n")
cat("FD:  ", round(coef(mod_fd)["value"], 4), "  ", round(coef(mod_fd)["capital"], 4), "\n\n")
cat("Regla:\n")
cat("  - Con T=2, FD = FE (identicos).\n")
cat("  - Con T>2 y errores iid, FE es más eficiente.\n")
cat("  - Con errores paseo aleatorio, FD es preferible.\n")


# ===== BLOQUE 6: EFECTOS ALEATORIOS =========================================

readline("\n--- Pulsa ENTER para Efectos Aleatorios ---")

cat("\n=== EFECTOS ALEATORIOS (GLS) ===\n")
cat("Supuesto: alpha_i NO correlaciónado con X_it.\n")
cat("Transforma restando theta * media (cuasi-demeaning).\n\n")

mod_re <- plm(inv ~ value + capital,
              data = pGrunfeld,
              model = "random")
print(summary(mod_re))

cat("\n--- Componentes de varianza ---\n")
ec <- ercomp(mod_re)
cat("sigma2_alpha (efecto individual):", round(ec$sigma2["id"], 2), "\n")
cat("sigma2_epsilon (idiosincratico) :", round(ec$sigma2["idios"], 2), "\n")
cat("theta:", round(ec$theta[1], 4), "\n\n")

if (ec$theta[1] > 0.7) {
  cat("theta cercano a 1 => RE se parece mucho a FE.\n")
} else if (ec$theta[1] < 0.3) {
  cat("theta cercano a 0 => RE se parece mucho a Pooled OLS.\n")
} else {
  cat("theta intermedio => RE pondera entre FE y Pooled.\n")
}

cat("\n--- Test LM de Breusch-Pagan: Pooled vs RE ---\n")
cat("H0: sigma2_alpha = 0 (Pooled suficiente)\n")
cat("H1: sigma2_alpha > 0 (hay efectos aleatorios)\n\n")
lm_bp <- plmtest(mod_pooled, type = "bp")
print(lm_bp)

if (lm_bp$p.value < 0.05) {
  cat("\n=> sigma2_alpha > 0: hay efectos individuales significativos.\n")
} else {
  cat("\n=> No se detectan efectos aleatorios significativos.\n")
}


# ===== BLOQUE 7: TEST DE HAUSMAN ============================================

readline("\n--- Pulsa ENTER para el Test de Hausman ---")

cat("\n=== TEST DE HAUSMAN: FE vs RE ===\n")
cat("H0: alpha_i NO correlaciónado con X_it (RE consistente + eficiente)\n")
cat("H1: alpha_i correlaciónado con X_it (solo FE es consistente)\n\n")

haus <- phtest(mod_fe, mod_re)
print(haus)

cat("\n")
if (haus$p.value < 0.05) {
  cat("=> RECHAZAMOS H0 (p =", round(haus$p.value, 4), ")\n")
  cat("   alpha_i esta correlaciónado con los regresores.\n")
  cat("   DECISION: usar EFECTOS FIJOS.\n")
} else {
  cat("=> No rechazamos H0 (p =", round(haus$p.value, 4), ")\n")
  cat("   No hay evidencia de correlación.\n")
  cat("   DECISION: usar EFECTOS ALEATORIOS (mas eficiente).\n")
}

cat("\n--- Comparación de coeficientes ---\n\n")
cat("          Pooled      FE         RE\n")
cat("value  ", sprintf("%9.4f", coef(mod_pooled)["value"]),
    sprintf("%9.4f", coef(mod_fe)["value"]),
    sprintf("%9.4f", coef(mod_re)["value"]), "\n")
cat("capital", sprintf("%9.4f", coef(mod_pooled)["capital"]),
    sprintf("%9.4f", coef(mod_fe)["capital"]),
    sprintf("%9.4f", coef(mod_re)["capital"]), "\n")

cat("\n[Abriendo tabla comparativa en el Viewer...]\n")
comp_df <- data.frame(
  Variable = c("(Intercept)", "value", "capital"),
  Pooled = round(coef(mod_pooled), 4),
  FE = round(c(NA, coef(mod_fe)), 4),
  RE = round(coef(mod_re), 4)
)
print(tabla_dt(comp_df, "Comparación Pooled vs FE vs RE (exportable)", filas = FALSE))


# ===== BLOQUE 8: DIAGNOSTICO COMPLETO =======================================

readline("\n--- Pulsa ENTER para la bateria completa de diagnóstico ---")

cat("\n=== BATERIA DE DIAGNOSTICO ===\n\n")

# 1. Test F
cat("1. Test F (Pooled vs FE):\n")
f_p <- pFtest(mod_fe, mod_pooled)$p.value
cat("   p-valor:", format.pval(f_p, digits = 4), "->",
    ifelse(f_p < 0.05, "Efectos individuales SI", "Pooled suficiente"), "\n\n")

# 2. LM
cat("2. Test LM Breusch-Pagan (Pooled vs RE):\n")
lm_p <- plmtest(mod_pooled, type = "bp")$p.value
cat("   p-valor:", format.pval(lm_p, digits = 4), "->",
    ifelse(lm_p < 0.05, "sigma2_alpha > 0", "sigma2_alpha = 0"), "\n\n")

# 3. Hausman
cat("3. Test de Hausman (FE vs RE):\n")
h_p <- haus$p.value
cat("   p-valor:", format.pval(h_p, digits = 4), "->",
    ifelse(h_p < 0.05, "Usar FE", "Usar RE"), "\n\n")

# 4. Wooldridge
cat("4. Test de Wooldridge (autocorrelación serial):\n")
w_p <- pwartest(mod_fe)$p.value
cat("   p-valor:", format.pval(w_p, digits = 4), "->",
    ifelse(w_p < 0.05, "HAY autocorrelación", "No hay autocorrelación"), "\n\n")

# 5. Pesaran CD
cat("5. Test de Pesaran CD (dependencia transversal):\n")
cd_p <- pcdtest(mod_fe, test = "cd")$p.value
cat("   p-valor:", format.pval(cd_p, digits = 4), "->",
    ifelse(cd_p < 0.05, "HAY dep. transversal", "No hay dep. transversal"), "\n\n")

# 6. BP heteroscedasticidad
cat("6. Test Breusch-Pagan (heteroscedasticidad):\n")
bp_p <- bptest(inv ~ factor(firm) + value + capital, data = Grunfeld,
               studentize = FALSE)$p.value
cat("   p-valor:", format.pval(bp_p, digits = 4), "->",
    ifelse(bp_p < 0.05, "HAY heteroscedasticidad", "Homoscedasticidad"), "\n\n")

cat("RECOMENDACION: Ante cualquier problema, usar errores robustos agrupados.\n\n")

cat("[Abriendo tabla resumen de diagnóstico en el Viewer...]\n")
diag_df <- data.frame(
  Test = c("F (Pooled vs FE)", "LM B-P (Pooled vs RE)",
           "Hausman (FE vs RE)", "Wooldridge (autocorr.)",
           "Pesaran CD (dep. transv.)", "B-P (heteroscedast.)"),
  p_valor = round(c(f_p, lm_p, h_p, w_p, cd_p, bp_p), 4),
  Decision = c(
    ifelse(f_p < 0.05, "Efectos SI", "Pooled OK"),
    ifelse(lm_p < 0.05, "sigma2 > 0", "sigma2 = 0"),
    ifelse(h_p < 0.05, "Usar FE", "Usar RE"),
    ifelse(w_p < 0.05, "Autocorrelación", "OK"),
    ifelse(cd_p < 0.05, "Dep. transversal", "OK"),
    ifelse(bp_p < 0.05, "Heteroscedast.", "OK"))
)
print(tabla_dt(diag_df, "Bateria de diagnóstico (exportable)", filas = FALSE))

cat("\n--- Modelo final con errores robustos ---\n\n")
print(coeftest(mod_fe, vcov = vcovHC(mod_fe, type = "HC1", cluster = "group")))


# ===== BLOQUE 9: MODELOS CON INTERACCIONES ==================================

readline("\n--- Pulsa ENTER para modelos con interacciónes ---")

cat("\n=== MODELOS CON INTERACCIONES ===\n\n")
cat("Creamos dos variables categoricas para ilustrar:\n")
cat("  tipo: grande/pequenia (segun inversión media)\n")
cat("  período: preguerra (<=1945) / postguerra (>1945)\n\n")

medias_empresa <- tapply(Grunfeld$inv, Grunfeld$firm, mean)
grandes <- names(medias_empresa[medias_empresa > median(medias_empresa)])
Grunfeld$tipo <- factor(ifelse(Grunfeld$firm %in% grandes, "grande", "pequenia"))
Grunfeld$periodo <- factor(ifelse(Grunfeld$year <= 1945, "preguerra", "postguerra"),
                           levels = c("preguerra", "postguerra"))

cat("Tabla de contingencia:\n")
print(table(Grunfeld$tipo, Grunfeld$periodo))

readline("\n--- Pulsa ENTER para la interacción simple ---")

cat("\n--- 9a. Interaccion simple: inv ~ value * tipo ---\n")
cat("Efecto de value puede diferir entre grandes y pequenias.\n\n")

mod_inter <- lm(inv ~ value * tipo, data = Grunfeld)
print(summary(mod_inter))

cat("\nInterpretación:\n")
cat("  Efecto value en pequenias :", round(coef(mod_inter)["value"], 4), "\n")
ef_grande <- coef(mod_inter)["value"] + coef(mod_inter)["value:tipogrande"]
cat("  Efecto value en grandes   :", round(ef_grande, 4), "\n")
cat("  Diferencia (interacción)  :", round(coef(mod_inter)["value:tipogrande"], 4), "\n")

readline("\n--- Pulsa ENTER para los modelos saturados ---")

cat("\n--- 9b. Modelo saturado CON intercepto ---\n")
cat("Formula: inv ~ tipo * período\n")
cat("(Coeficientes son diferencias respecto a la referencia)\n\n")

mod_sat1 <- lm(inv ~ tipo * período, data = Grunfeld)
print(round(coef(mod_sat1), 2))

cat("\nReferencia: pequenia-preguerra =", round(coef(mod_sat1)[1], 2), "\n")
cat("Cada coeficiente es una DIFERENCIA respecto a la referencia.\n")

readline("\n--- Pulsa ENTER para el modelo sin intercepto ---")

cat("\n--- 9c. Modelo saturado SIN intercepto ---\n")
cat("Formula: inv ~ 0 + tipo:periodo\n")
cat("(Cada coeficiente es la MEDIA DIRECTA del grupo)\n\n")

mod_sat0 <- lm(inv ~ 0 + tipo:periodo, data = Grunfeld)
print(round(coef(mod_sat0), 2))

cat("\n[Abriendo tabla del modelo saturado en el Viewer...]\n")
sat_df <- data.frame(
  Grupo = names(coef(mod_sat0)),
  Media = round(coef(mod_sat0), 2),
  IC_inf = round(confint(mod_sat0)[, 1], 2),
  IC_sup = round(confint(mod_sat0)[, 2], 2)
)
print(tabla_dt(sat_df, "Modelo saturado sin intercepto: medias e IC (exportable)", filas = FALSE))

cat("\nVentajas del modelo sin intercepto:\n")
cat("  1. Lectura directa: cada coef = media del grupo.\n")
cat("  2. IC directos por grupo (sin combinaciones líneales).\n")
cat("  3. Sin categoria de referencia arbitraria.\n")

cat("\nVerificacion: mismos residuos en ambos modelos:\n")
cat("  Max dif. residuos:", max(abs(residuals(mod_sat1) - residuals(mod_sat0))), "\n")
cat("  SCR modelo 1:", round(sum(residuals(mod_sat1)^2), 2), "\n")
cat("  SCR modelo 0:", round(sum(residuals(mod_sat0)^2), 2), "\n")

readline("\n--- Pulsa ENTER para Difference-in-Differences ---")

cat("\n--- 9d. Difference-in-Differences (DiD) ---\n\n")
cat("'Tratamiento': ser empresa grande.\n")
cat("'Post': período postguerra.\n\n")

gamma <- coef(mod_sat0)
cat("Medias por grupo (modelo saturado sin intercepto):\n")
for (i in seq_along(gamma)) {
  cat("  ", names(gamma)[i], ":", round(gamma[i], 2), "\n")
}

did <- coef(mod_sat1)["tipogrande:periodopostguerra"]
cat("\nCoeficiente DiD (modelo con intercepto):", round(did, 2), "\n")
cat("Interpretación: la diferencia entre grandes y pequenias CAMBIO en",
    round(did, 2), "mill. $ tras la guerra.\n")

cat("\n[Generando gráfico DiD...]\n")
medias_did <- aggregate(inv ~ tipo + período, data = Grunfeld, FUN = mean)
print(
  ggplot(medias_did, aes(x = período, y = inv, color = tipo, group = tipo)) +
    geom_point(size = 4) +
    geom_line(linewidth = 1.2) +
    labs(x = "Periodo", y = "Inversión media (mill. $)",
         title = "Difference-in-Differences",
         subtitle = "Efecto del 'tratamiento' (ser grande) en postguerra",
         color = "Tipo") +
    scale_color_manual(values = c("darkorange", "steelblue")) +
    theme_minimal(base_size = 12)
)


# ===== BLOQUE 10: PANEL DINAMICO Y SESGO DE NICKELL =========================

readline("\n--- Pulsa ENTER para panel dinámico y sesgo de Nickell ---")

cat("\n=== PANEL DINAMICO: SESGO DE NICKELL ===\n\n")
cat("Si incluimos y_{t-1} como regresor y usamos FE,\n")
cat("el estimador de rho esta SESGADO en paneles cortos.\n")
cat("El sesgo es O(1/T): grave con T pequenio.\n\n")
cat("Simulamos para verificar...\n")

set.seed(123)
n_sim <- 100
T_vals <- c(5, 10, 20, 50)
rho_true <- 0.5
n_rep <- 200

resultados <- data.frame(T = integer(), rho_hat = numeric())

for (Ti in T_vals) {
  for (rep in 1:n_rep) {
    alpha_i <- rnorm(n_sim, 0, 2)
    y <- matrix(NA, n_sim, Ti)
    y[, 1] <- alpha_i + rnorm(n_sim)
    for (tt in 2:Ti) {
      y[, tt] <- alpha_i + rho_true * y[, tt - 1] + rnorm(n_sim)
    }
    y_mean <- rowMeans(y)
    y_dm <- y - y_mean
    y_dep <- as.vector(y_dm[, 2:Ti])
    y_lag <- as.vector(y_dm[, 1:(Ti - 1)])
    rho_hat <- coef(lm(y_dep ~ y_lag - 1))
    resultados <- rbind(resultados, data.frame(T = Ti, rho_hat = rho_hat))
  }
}

cat("\n[Generando gráfico del sesgo de Nickell...]\n")
medias_rho <- aggregate(rho_hat ~ T, resultados, mean)

par(mar = c(4, 4, 3, 1))
boxplot(rho_hat ~ T, data = resultados, col = "lightblue",
        xlab = "Numero de períodos (T)", ylab = "rho estimado (FE)",
        main = paste("Sesgo de Nickell (rho verdadero =", rho_true, ")"))
abline(h = rho_true, col = "red", lwd = 2, lty = 2)
points(1:length(T_vals), medias_rho$rho_hat, pch = 16, col = "darkred", cex = 1.5)
legend("bottomright",
       c(paste("rho verdadero =", rho_true), "Media FE"),
       col = c("red", "darkred"), lty = c(2, NA), pch = c(NA, 16), lwd = c(2, NA))

cat("\nMedia del estimador FE por T:\n")
for (i in 1:nrow(medias_rho)) {
  sesgo <- medias_rho$rho_hat[i] - rho_true
  cat("  T =", sprintf("%2d", medias_rho$T[i]),
      ": rho_hat =", round(medias_rho$rho_hat[i], 4),
      " (sesgo =", round(sesgo, 4), ")\n")
}

cat("\n=> Con T=5, el sesgo es enorme. Con T=50, casi desaparece.\n")

readline("\n--- Pulsa ENTER para Arellano-Bond (GMM) ---")

cat("\n=== ARELLANO-BOND (Difference GMM) ===\n")
cat("Solucion al sesgo de Nickell:\n")
cat("  1. Primeras diferencias para eliminar alpha_i.\n")
cat("  2. Instrumentar Delta y_{t-1} con retardos de y en niveles.\n\n")

mod_ab <- pgmm(inv ~ lag(inv, 1) + value + capital | lag(inv, 2:4),
               data = pGrunfeld,
               effect = "individual",
               model = "onestep",
               collapse = TRUE)
print(suppressWarnings(summary(mod_ab)))

cat("\nClaves del diagnóstico GMM:\n")
cat("  - Test de Sargan: H0 = instrumentos válidos. NO rechazar es bueno.\n")
cat("  - AR(1): se espera significativo (por construccion de las diferencias).\n")
cat("  - AR(2): NO debe ser significativo. Si lo es, instrumentos invalidos.\n")


# ===== BLOQUE 11: RESUMEN FINAL =============================================

readline("\n--- Pulsa ENTER para el resumen final ---")

cat("\n")
cat("=============================================================\n")
cat("  RESUMEN FINAL — DATOS DE PANEL\n")
cat("=============================================================\n\n")

cat("ESTIMADORES:\n")
cat("  Pooled OLS  : Ignora panel. Valido solo si no hay efectos.\n")
cat("  FE (Within) : Elimina alpha_i. Robusto pero pierde vars fijas en t.\n")
cat("  FD           : Alternativa a FE. Mejor con errores paseo aleatorio.\n")
cat("  RE (GLS)    : Mas eficiente si alpha_i no correlaciónado con X.\n")
cat("  GMM (A-B)   : Para paneles dinámicos (y_{t-1} como regresor).\n\n")

cat("TESTS:\n")
cat("  F-test       : Pooled vs FE (hay efectos individuales?)\n")
cat("  LM (B-P)     : Pooled vs RE (sigma2_alpha > 0?)\n")
cat("  Hausman       : FE vs RE (correlación alpha_i con X_it?)\n")
cat("  Wooldridge    : Autocorrelación serial\n")
cat("  Pesaran CD    : Dependencia transversal\n")
cat("  Breusch-Pagan : Heterocedasticidad\n\n")

cat("INTERACCIONES:\n")
cat("  Con intercepto : lm(y ~ grupo * período) -> coefs son diferencias.\n")
cat("  Sin intercepto : lm(y ~ 0 + grupo:periodo) -> coefs son medias.\n")
cat("  DiD            : la interacción es el efecto causal (si tendencias paralelas).\n\n")

cat("REGLA DE ORO:\n")
cat("  Siempre reportar errores robustos agrupados por individuo.\n")
cat("  coeftest(mod, vcov = vcovHC(mod, type='HC1', cluster='group'))\n\n")

cat("=============================================================\n")
cat("  Fin del script | Tema 5 | Carlos de Anta Puig\n")
cat("=============================================================\n")
