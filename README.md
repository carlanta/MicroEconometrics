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

*Carlos de Anta Puig — Madrid, marzo de 2026*

---

### Índice

**Capítulo 0 — Fundamentos de Probabilidad, Inferencia y Econometría**

**Capítulo 1 — Variable Dependiente Limitada**

**Capítulo 2 — Elección Discreta (Logit y Probit)**

**Capítulo 3 — Modelos para Datos Censurados (Tobit)**

**Capítulo 4 — Modelos para Datos de Recuento (Poisson y Binomial Negativa)**

**Capítulo 5 — Datos de Panel (Efectos Fijos y Aleatorios)**

**Anexos — Índice de Figuras · Índice de Cuadros**

**Bibliografía**

---

### Estructura del repositorio

- `data/` — Datasets utilizados en los ejemplos prácticos de cada capítulo
- `scripts/` — Scripts R autocontenidos por capítulo (generan análisis, tablas y gráficos)

### Scripts

Cada script en `scripts/` reproduce el análisis completo del capítulo correspondiente. Para ejecutar:

```r
setwd("ruta/al/proyecto")
source("scripts/T02_mECO_Script_Eleccion_Discreta.R")
```

### Requisitos

- R >= 4.0
- Paquetes: `tidyverse`, `kableExtra`, `plm`, `AER`, `MASS`, `pscl`, `lmtest`, `sandwich`

---

*© Carlos de Anta Puig, 2026. Publicado por Digital Reasons. Todos los derechos reservados.*
