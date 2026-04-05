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

Este manual está escrito pensando en mis alumnos. En los que llegan al curso con buena base de estadística pero nunca han visto una variable latente. En los que saben ejecutar `lm()` en R pero no entienden qué pasa cuando la variable dependiente es binaria. En los que necesitan no solo saber *qué* hace un modelo, sino *por qué* lo hace, *cuándo* usarlo y *cómo* interpretar sus resultados de forma que un economista — no un estadístico — pueda tomar decisiones informadas.

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

---

### Estructura del repositorio

- `data/` — Datasets utilizados en los ejemplos prácticos de cada capítulo (formato `.RData`, reproducibles con `set.seed` fijo)
- `scripts/` — Scripts R autocontenidos e interactivos por capítulo y caso práctico

---

### Scripts disponibles

Cada script en `scripts/` es autocontenido: instala automáticamente los paquetes necesarios, carga el dataset correspondiente desde `data/`, presenta los outputs con interpretaciones narrativas en la consola y guarda tablas y gráficos en `scripts/output/`. Las paradas interactivas (`readline`) permiten seguir el análisis paso a paso.

| Script | Tema | Caso | Contenido |
|--------|------|------|-----------|
| `T01_CP01_mECO_EDA_Empleo.R` | 1 | CP1 | EDA: mercado laboral (variable binaria) |
| `T01_CP02_mECO_EDA_Gasto.R` | 1 | CP2 | EDA: gasto en seguros (variable censurada) |
| `T01_CP03_mECO_EDA_Visitas.R` | 1 | CP3 | EDA: visitas al médico (variable de recuento) |

Para ejecutar cualquier script en RStudio:

1. Abre el script
2. `Session > Set Working Directory > To Source File Location`
3. Pulsa el botón `Source`

---

### Requisitos

- R >= 4.0
- Paquetes: `kableExtra`, `lmtest`, `sandwich`, `AER`, `MASS`, `pscl`, `marginaleffects`

---

*© Carlos de Anta Puig, 2026. Publicado por Digital Reasons. Todos los derechos reservados.*
