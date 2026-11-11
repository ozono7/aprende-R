# Aprende R

Material académico para estudiantes, con problemas cotidianos de las áreas
químico-científicas y académicas, resueltos en R con tidyverse y gráficas claras.

**Autor:** Dr. José Juan Torres Martínez
**Docente de Ciencias Químicas**
**Universidad Autónoma de San Luis Potosí (UASLP), México**

## Cómo funciona

Cada miércoles se publica una lección nueva. Las lecciones son secuenciales:
cada una usa lo que se aprendió en las anteriores, como un experimento real
que avanza paso a paso, desde el contexto y la toma de datos hasta el
análisis estadístico.

Cada lección es un script de R completo, funcional y bien documentado, que
puedes correr tal cual en tu computadora. Solo necesitas R y el paquete
`tidyverse` instalado:

```r
install.packages("tidyverse")
```

## Lecciones

| # | Lección | Tema |
|---|---------|------|
| 01 | `01-contexto-y-toma-de-datos.R` | El experimento, cómo se toman los datos en el laboratorio, datos ordenados (tidy data) y primer análisis exploratorio. |
| 02 | `02-curva-crecimiento-chlorella.R` | Curva de crecimiento de *Chlorella vulgaris*, velocidad específica de crecimiento (mu) y gráficas claras. |
| 03 | `03-diseno-de-experimentos.R` | Diseño de experimentos: completamente al azar, bloques al azar y factorial; cómo elegir el diseño según la pregunta. |

## Biología celular

Lecciones de biología celular con datos, en el mismo formato de la serie
(datos primero, todo calculado a partir de ellos). No forman parte de la
numeración secuencial de las lecciones 01-08: son una sección aparte.

| Lección | Tema |
|---|---|
| `biologia-celular-estructura-y-funcion.R` | Estructura y función celular: mediciones del diámetro de *Chlorella vulgaris*, volumen de la célula a partir de los datos y tamaño típico de los organelos comparado con la célula. Datos: `datos_diametros_chlorella.csv`. |
| `eucariotas-y-procariotas.R` | Comparación cuantitativa eucariota contra procariota: volumen, superficie, relación superficie/volumen y tamaño del genoma de *E. coli*, *S. aureus*, *S. cerevisiae*, *Chlorella vulgaris* y una célula animal típica. Datos: `datos_celulas_comparacion.csv`. |

## Próximas lecciones

La serie continúa con visualización y análisis avanzados, siempre con
problemas reales del laboratorio y de la academia:

- **04 — ANOVA de una vía:** comparar el crecimiento entre condiciones de luz.
- **05 — Prueba de Dunnett:** comparar cada tratamiento contra un control.
- **06 — Curva de calibración y regresión lineal:** construir una curva de calibración, R², límite de detección y cuantificación.
- **07 — Prueba t:** comparar dos grupos (por ejemplo, dos métodos de extracción).
- **08 — Correlación y regresión:** relación entre absorbancia y número de células.
- **Modelos de crecimiento:** Gompertz, logístico, Baranyi y otros modelos; ajuste no lineal y comparación de modelos.
- **ANOVA de dos vías y factorial:** efectos principales e interacciones.
- **Diseños por bloques y por grupos:** bloques completos al azar, análisis por grupos.
- **HPLC y FPLC:** análisis de cromatogramas, integración de picos y curvas de calibración.
- **Análisis de proteínas:** cuantificación (Bradford, BCA), geles SDS-PAGE y densitometría.
- **Análisis de geles de ADN:** electroforesis, estimación de tamaños y cuantificación de bandas.
- **ggplot2 a fondo:** gráficas de publicación, facetas, temas y combinación de figuras.
- **gganimate:** animar curvas de crecimiento y series de tiempo.
- **Análisis bioinformáticos:** expresión génica, RNA-Seq y visualización de datos ómicos.
- **Bioseparaciones:** balances de masa, eficiencia de separación y curvas de ruptura.
- **Biotecnología ambiental:** cinéticas de degradación y tratamiento de aguas.
- **Bioquímica:** cinética enzimática (Michaelis-Menten) y curvas dosis-respuesta.
- **Estadística para posgrado:** ANOVA factorial, modelos mixtos, pruebas no paramétricas y análisis multivariado.

## Datos

Los datos de las lecciones son simulados con fines didácticos, con valores
realistas para el laboratorio. La lección 01 genera el archivo
`datos_chlorella.csv`, que usa la lección 02. Las lecciones de biología
celular usan sus propios archivos: `datos_diametros_chlorella.csv` (30
mediciones del diámetro de *Chlorella vulgaris*, dentro del rango publicado
de 2-10 µm) y `datos_celulas_comparacion.csv` (tamaños y genomas publicados
de cinco células).

La lección 03 usa tres archivos con las mediciones por unidad
 experimental: `datos_dca_nitrogeno.csv` (biomasa seca de *Chlorella vulgaris*
con tres fuentes de nitrógeno, diseño completamente al azar),
`datos_bloques_nitrogeno.csv` (el mismo experimento por estante del
incubador, diseño en bloques) y `datos_factorial_clorofila.csv`
(concentración de clorofila a según temperatura y tiempo de extracción,
diseño factorial 2 x 2). La asignación de tratamientos a las unidades se
genera en el script con aleatorización.

## Licencia

Material académico de uso libre para fines educativos.
