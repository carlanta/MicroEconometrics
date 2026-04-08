# Manual de Microeconometría

## Microeconometría Aplicada a la Economía

**Autor:** Carlos de Anta Puig
Economista · Perito Financiero
Miembro del Colegio de Economistas de Madrid
Miembro del Instituto Español de Analistas Financieros (IEAF)
Profesor de Econometría y Microeconometría
carlos@cwconsultores.com

---

### Publicación

Este manual está publicado por **Digital Reasons** (www.digitalreasons.es).

---

### Prólogo

Este manual nació de una frustración y de una convicción.

La frustración es la que siente cualquier profesor que, tras años de enseñanza, constata que los materiales disponibles no terminan de funcionar. Los manuales internacionales de referencia — Cameron y Trivedi, Wooldridge, Greene — son obras maestras, pero están escritos para investigadores, no para alumnos que se enfrentan por primera vez a un modelo Logit o a un panel de datos. Los apuntes de las universidades, por su parte, suelen pecar de lo contrario: son telegráficos, dan por supuestos conceptos que el alumno no domina, y saltan de la fórmula al código sin detenerse a explicar el porqué de cada paso.

La convicción es que la microeconometría puede explicarse de otra manera. Con rigor, sí — las derivaciones formales están aquí, completas y sin atajos —, pero también con claridad pedagógica, con ejemplos numéricos resueltos paso a paso, con gráficos que iluminan la intuición detrás de las fórmulas, y con un hilo narrativo que conecta cada modelo con los problemas económicos reales que lo motivaron.

Si este libro consigue que algún alumno más disfrute aprendiendo microeconometría, habrá cumplido su propósito.

*Carlos de Anta Puig — Madrid, 2026*

---

### Índice

**Capítulo 0 — Fundamentos de Probabilidad, Inferencia y Econometría**

- Repaso de probabilidad: distribuciones, densidad y acumulada
- Estimación e inferencia: MLE, propiedades asintóticas
- Contrastes de hipótesis: Likelihood Ratio, Wald y Lagrange Multiplier
- Repaso del Modelo Lineal (MCO)
- Especificación y diagnóstico del modelo
- Conceptos avanzados: variable latente, función de enlace, modelo índice

**Capítulo 1 — Datos con Variable Dependiente Limitada**

- Taxonomía de variables dependientes limitadas (binarias, multinomiales, censuradas, recuento)
- Variables binarias: Bernoulli, Binomial, variable latente
- Variables multinomiales: modelos de elección múltiple
- Variables continuas limitadas: censura vs. truncamiento
- El análisis exploratorio de datos (EDA) como primer paso sistemático
- Casos prácticos: EDA de datos de mercado laboral, gasto en seguros y visitas al médico

**Capítulo 2 — Elección Discreta (MPL, Logit y Probit)**

- El Modelo Lineal de Probabilidad (MPL) y sus limitaciones
- El modelo Probit: formulación, estimación MLE e interpretación
- El modelo Logit: formulación, estimación e interpretación
- Efectos marginales: AME, MEM, MER
- Odds ratios (Logit)
- Bondad de ajuste: Pseudo-R² de McFadden, tabla de clasificación
- Contrastes: LR, Wald
- Comparación MPL vs Probit vs Logit
- Tres casos prácticos completos con scripts interactivos

**Capítulo 3 — Modelos para Datos Censurados (Tobit)**

- Datos censurados y truncados: conceptos y consecuencias
- El modelo Tobit tipo I: formulación, estimación MLE
- Por qué MCO falla con datos censurados: demostración del sesgo
- Interpretación de coeficientes y efectos marginales
- Pruebas de especificación: normalidad, homocedasticidad
- Extensiones: Tobit generalizado, modelo de selección de Heckman
- Tres casos prácticos completos con scripts interactivos

**Capítulo 4 — Modelos para Datos de Recuento (Poisson y Binomial Negativa)**

- La distribución de Poisson y el supuesto de equidispersión
- Modelo de regresión de Poisson: formulación, MLE, IRR
- Sobredispersión: diagnóstico y el modelo Binomial Negativa (NB2)
- Inferencia: significación individual, conjunta, AIC, BIC, test de Vuong
- Extensiones: ZIP, ZINB, modelos hurdle
- Tres casos prácticos completos con scripts interactivos

**Capítulo 5 — Datos de Panel**

- Estructura de los datos de panel: paneles cortos/largos, balanceados/no balanceados
- Descomposición within/between
- Pooled OLS, Efectos Fijos (estimador within) y Efectos Aleatorios (GLS)
- Primeras diferencias
- Test de Hausman: elección entre EF y EA
- Diagnósticos: test F, Breusch-Pagan, Wooldridge, errores clustered
- Extensiones: efectos bidireccionales, DiD, paneles dinámicos (Arellano-Bond)
- Tres casos prácticos: inversión empresarial, desempleo regional, salarios y formación

**Capítulo 6 — Estimación con Variables Instrumentales**

- El problema de la endogeneidad: variable omitida, simultaneidad, error de medida
- Sesgo de endogeneidad: demostración formal y ejemplo numérico
- La idea del instrumento: relevancia y exogeneidad
- Estimador IV simple: derivación y consistencia
- Mínimos Cuadrados en Dos Etapas (MC2E / 2SLS)
- Diagnósticos: F primera etapa (Staiger-Stock), Wu-Hausman, Sargan
- Tres casos prácticos: retorno de la educación, gasto público y crecimiento, curva de demanda

**Capítulo 7 — Modelos de Duración**

- Función de supervivencia S(t), función de riesgo h(t) y riesgo acumulado H(t)
- Tipos de censura: derecha, izquierda, intervalo
- Estimación no paramétrica: Kaplan-Meier y test de log-rank
- Modelos paramétricos: exponencial, Weibull, log-normal, log-logístico
- Modelo de Cox (riesgos proporcionales): verosimilitud parcial, hazard ratios
- Diagnósticos: residuos de Schoenfeld, test de proporcionalidad
- Tres casos prácticos: duración del desempleo, supervivencia de empresas, impago crediticio

**Capítulo 8 — Datos Cualitativos**

- Variables ficticias (dummies), interacciones y cambio estructural
- Modelos ANOVA y ANCOVA
- Logit Multinomial: utilidad aleatoria de McFadden, propiedad IIA
- Probit/Logit Ordenado: variable latente con umbrales
- Efectos marginales en modelos de elección múltiple
- Tres casos prácticos: elección de transporte, satisfacción laboral, situación laboral

---

### Estructura del repositorio

- `data/` — Datasets utilizados en los ejemplos prácticos (formato `.RData`, reproducibles con `set.seed` fijo)
- `scripts/` — Scripts R autocontenidos e interactivos por capítulo y caso práctico

---

### Scripts disponibles

Cada script es autocontenido: instala automáticamente los paquetes necesarios, detecta su propia ubicación para encontrar los datos (funciona tanto con `source()` como con `Rscript`), presenta los outputs con interpretaciones narrativas en la consola y guarda gráficos en la carpeta `output/` del proyecto. Las paradas interactivas (`readline`) permiten seguir el análisis paso a paso.

| Script | Descripción |
|--------|-------------|
| `T00_mECO_Script_Fundamentos.R` | **T0** — Fundamentos de probabilidad e inferencia |
| `T01_mECO_Script_VD_Limitada.R` | **T1** — Variable dependiente limitada: taxonomía |
| `T01_CP01_mECO_EDA_Empleo.R` | **T1 CP1** — EDA: mercado laboral (variable binaria) |
| `T01_CP02_mECO_EDA_Gasto.R` | **T1 CP2** — EDA: gasto en seguros (variable censurada) |
| `T01_CP03_mECO_EDA_Visitas.R` | **T1 CP3** — EDA: visitas al médico (variable de recuento) |
| `T02_mECO_Script_Eleccion_Discreta.R` | **T2** — Elección discreta: MPL, Logit, Probit |
| `T02_CP01_mECO_Logit_Credito.R` | **T2 CP1** — Logit: concesión de crédito bancario |
| `T02_CP02_mECO_Logit_Compra.R` | **T2 CP2** — Logit: compra online |
| `T02_CP03_mECO_Logit_Matricula.R` | **T2 CP3** — Logit: matrícula universitaria |
| `T03_mECO_Script_Tobit.R` | **T3** — Modelo Tobit: censura y variable latente |
| `T03_CP01_mECO_Tobit_Seguros.R` | **T3 CP1** — Tobit: gasto en seguros médicos |
| `T03_CP02_mECO_Tobit_Donaciones.R` | **T3 CP2** — Tobit: donaciones benéficas |
| `T03_CP03_mECO_Tobit_Formacion.R` | **T3 CP3** — Tobit: gasto en formación |
| `T04_mECO_Script_Recuento.R` | **T4** — Poisson y Binomial Negativa |
| `T04_CP01_mECO_Poisson_Criminalidad.R` | **T4 CP1** — Poisson/NB: delitos por municipio |
| `T04_CP02_mECO_NB_Accidentes.R` | **T4 CP2** — NB: accidentes de tráfico |
| `T04_CP03_mECO_NB_Publicaciones.R` | **T4 CP3** — NB: publicaciones académicas |
| `T05_mECO_Script_Panel.R` | **T5** — Panel: Pooled, EF, EA con datos de Grunfeld |
| `T05_CP01_mECO_Panel_Inversion.R` | **T5 CP1** — Panel: inversión empresarial (80 empresas × 10 años) |
| `T05_CP02_mECO_Panel_Desempleo.R` | **T5 CP2** — Panel: desempleo regional (50 provincias × 15 años) |
| `T05_CP03_mECO_Panel_Salarios.R` | **T5 CP3** — Panel: salarios y formación (500 trabajadores × 5 años) |
| `T06_mECO_Script_Instrumentales.R` | **T6** — IV/MC2E: simulación didáctica con parámetros conocidos |
| `T06_CP01_mECO_IV_Educacion.R` | **T6 CP1** — IV: retorno de la educación (2 instrumentos, Sargan) |
| `T06_CP02_mECO_IV_Crecimiento.R` | **T6 CP2** — IV: gasto público y crecimiento (id. exacta) |
| `T06_CP03_mECO_IV_Demanda.R` | **T6 CP3** — IV: curva de demanda (simultaneidad, Sargan) |
| `T07_mECO_Script_Duracion.R` | **T7** — Duración: KM, Cox, Weibull, log-normal |
| `T07_CP01_mECO_Duracion_Desempleo.R` | **T7 CP1** — Duración: desempleo (800 individuos, censura 30%) |
| `T07_CP02_mECO_Duracion_Empresas.R` | **T7 CP2** — Duración: supervivencia empresarial (600 empresas) |
| `T07_CP03_mECO_Duracion_Impago.R` | **T7 CP3** — Duración: impago crediticio (1000 créditos, censura 67%) |
| `T08_mECO_Script_Cualitativos.R` | **T8** — Dummies, ANOVA/ANCOVA, multinomial, ordenado |
| `T08_CP01_mECO_Multinomial_Transporte.R` | **T8 CP1** — Multinomial: elección de transporte (3 alternativas) |
| `T08_CP02_mECO_Ordenado_Satisfaccion.R` | **T8 CP2** — Probit/Logit ordenado: satisfacción laboral (5 niveles) |
| `T08_CP03_mECO_Multinomial_SitLaboral.R` | **T8 CP3** — Multinomial: situación laboral (empleado/desempleado/inactivo) |

---

### Datasets

| Dataset | Tema | Descripción |
|---------|------|-------------|
| `T01_CP01_empleo_basico.RData` | **T1** — Participación laboral (binaria) |
| `T01_CP02_gasto_hogar.RData` | **T1** — Gasto en seguros médicos (censurada) |
| `T01_CP03_visitas_medico.RData` | **T1** — Visitas al médico (recuento) |
| `T02_CP01_credito_bancario.RData` | **T2** — Concesión de crédito |
| `T02_CP02_compra_online.RData` | **T2** — Compra online |
| `T02_CP03_matricula_universidad.RData` | **T2** — Matrícula universitaria |
| `T03_CP01_gasto_seguros.RData` | **T3** — Gasto en seguros médicos |
| `T03_CP02_donaciones.RData` | **T3** — Donaciones benéficas |
| `T03_CP03_gasto_formacion.RData` | **T3** — Gasto en formación empresarial |
| `T04_CP01_criminalidad.RData` | **T4** — Delitos por municipio |
| `T04_CP02_accidentes.RData` | **T4** — Accidentes de tráfico |
| `T04_CP03_publicaciones.RData` | **T4** — Publicaciones académicas |
| `T05_CP01_inversion_empresas.RData` | **T5** — Panel: inversión empresarial (80×10) |
| `T05_CP02_desempleo_regional.RData` | **T5** — Panel: desempleo provincial (50×15) |
| `T05_CP03_salarios_formacion.RData` | **T5** — Panel: salarios y formación (500×5) |
| `T06_CP01_retorno_educacion.RData` | **T6** — Retorno educación con instrumentos |
| `T06_CP02_crecimiento_paises.RData` | **T6** — Gasto público y crecimiento (60×20) |
| `T06_CP03_oferta_demanda.RData` | **T6** — Mercado simulado (oferta-demanda) |
| `T07_CP01_duracion_desempleo.RData` | **T7** — Duración desempleo con censura |
| `T07_CP02_supervivencia_empresas.RData` | **T7** — Supervivencia empresarial |
| `T07_CP03_impago_crediticio.RData` | **T7** — Impago crediticio (credit scoring) |
| `T08_CP01_transporte.RData` | **T8** — Elección de transporte (3 alternativas) |
| `T08_CP02_satisfaccion_laboral.RData` | **T8** — Satisfacción laboral ordinal (1-5) |
| `T08_CP03_situacion_laboral.RData` | **T8** — Situación laboral (3 categorías) |

Todos los datasets se generan con `set.seed(2026)` para reproducibilidad completa.

---

### Requisitos

- R >= 4.0
- Paquetes principales: `kableExtra`, `lmtest`, `sandwich`, `AER`, `MASS`, `pscl`, `marginaleffects`, `plm`, `survival`, `nnet`, `censReg`, `VGAM`
- Los scripts instalan automáticamente los paquetes necesarios si no están disponibles.

---

### Cómo ejecutar los scripts

Los scripts detectan automáticamente su ubicación. Funcionan de tres formas:

1. **En RStudio** (recomendado): Abrir el script → `Source`
2. **Desde la consola de R**: `source("ruta/al/script.R")`
3. **Desde terminal**: `Rscript ruta/al/script.R`

Los gráficos se guardan en la carpeta `output/` del proyecto.

---

*© Carlos de Anta Puig, 2026. Publicado por Digital Reasons. Todos los derechos reservados.*
