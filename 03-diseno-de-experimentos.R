# ==========================================================================
# 03. Diseño de experimentos: completamente al azar, bloques al azar
#   y factorial
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Serie "Aprende R" — Lección 03
#
# Descripción: Cómo planear un experimento antes de tocar un matraz.
#   Se comparan tres diseños con datos reales de laboratorio: el diseño
#   completamente al azar (DCA), el diseño en bloques completos al azar
#   (DBCA) y el diseño factorial 2 x 2. En cada diseño la asignación de
#   los tratamientos a las unidades experimentales se hace con
#   aleatorización en R (set.seed y sample), y todas las medias, los
#   valores p y los parámetros se calculan A PARTIR DE LOS DATOS con
#   tidyverse y aov(). El ejemplo principal es la producción de biomasa
#   de Chlorella vulgaris con tres fuentes de nitrógeno, y el factorial
#   es la extracción de clorofila a según la temperatura y el tiempo.
#
# Objetivos de aprendizaje:
#   - Explicar qué es una unidad experimental, un tratamiento, una
#     réplica y un bloque, y por qué se aleatoriza.
#   - Aleatorizar la asignación de tratamientos con set.seed() y
#     sample(), para que el resultado sea reproducible.
#   - Elegir el diseño según la pregunta: DCA, bloques o factorial.
#   - Comparar tratamientos con un ANOVA de una vía, un ANOVA con
#     bloques y un ANOVA factorial con interacción.
#   - Leer un gráfico de interacción y explicar qué significa que dos
#     factores interactúen.
#   - Graficar los resultados con ggplot2, con gráficas claras
#     y bien presentadas.
#
# Datos: archivos datos_dca_nitrogeno.csv, datos_bloques_nitrogeno.csv
#   y datos_factorial_clorofila.csv. Cada archivo trae solo las
#   mediciones por unidad experimental (matraz, posición o unidad); la
#   asignación de tratamientos se genera en este script con
#   aleatorización, tal como se haría antes de montar el experimento.
# ==========================================================================

# --------------------------------------------------------------------------
# 1. Cargar paquetes
# --------------------------------------------------------------------------
library(tidyverse)

# --------------------------------------------------------------------------
# 2. Cómo elegir el diseño según la pregunta
# --------------------------------------------------------------------------
# Antes de sembrar un solo cultivo hay que decidir cómo se reparten los
# tratamientos entre las unidades experimentales. Tres preguntas guían
# la elección:
#
#   - ¿Hay un solo factor y las unidades son homogéneas? Entonces el
#     diseño completamente al azar (DCA) basta: cada unidad recibe un
#     tratamiento elegido al azar, con el mismo número de réplicas.
#   - ¿Hay una fuente de variación conocida que no es lo que estudio,
#     pero puede contaminar el resultado (el estante del incubador, el
#     día de siembra, el lote de medio)? Entonces se agrupan las
#     unidades en bloques homogéneos y dentro de cada bloque se ponen
#     todos los tratamientos: diseño en bloques completos al azar (DBCA).
#     El bloque se incluye en el análisis y su variación deja de
#     confundirse con la de los tratamientos.
#   - ¿Quiero probar dos factores a la vez y saber si se combinan, es
#     decir, si el efecto de uno depende del nivel del otro? Entonces
#     el diseño factorial: se prueban todas las combinaciones de los
#     niveles de ambos factores y el análisis estima la interacción.
#
# Tres principios valen para los tres diseños: aleatorización (los
# tratamientos se asignan al azar, nunca por comodidad), repetición
# (cada tratamiento se repite en varias unidades independientes) y
# control (siempre hay una referencia con la que comparar, aquí el
# nitrato, que es la fuente de nitrógeno habitual del medio de cultivo).

# --------------------------------------------------------------------------
# 3. Diseño completamente al azar (DCA)
# --------------------------------------------------------------------------
# Pregunta: ¿la fuente de nitrógeno cambia la biomasa seca de Chlorella
# vulgaris al cabo de 10 días de cultivo?
# Tratamientos: Nitrato (control), Urea y Amonio.
# Unidades experimentales: 12 matraces con el mismo medio base, la misma
# luz y la misma temperatura. Cada tratamiento se repite 4 veces.

# 3.1 Aleatorización: se reparten los 12 matraces entre los tres
# tratamientos al azar. set.seed() fija la semilla para que cualquiera
# que corra el script obtenga exactamente la misma asignación.
set.seed(2026)
tratamientos_dca <- rep(c("Nitrato", "Urea", "Amonio"), each = 4)
asignacion_dca <- tibble(
  matraz = 1:12,
  tratamiento = sample(tratamientos_dca)
)
print(asignacion_dca)

# 3.2 Se leen las mediciones: biomasa seca en g/L, medida al día 10
# en cada matraz. El archivo trae solo matraz y medición; el
# tratamiento de cada matraz es el que generó la aleatorización.
mediciones_dca <- read_csv("datos_dca_nitrogeno.csv", show_col_types = FALSE)
datos_dca <- asignacion_dca %>%
  left_join(mediciones_dca, by = "matraz") %>%
  mutate(
    tratamiento = factor(
      tratamiento,
      levels = c("Nitrato", "Urea", "Amonio")
    )
  )
print(datos_dca)

# 3.3 Resumen por tratamiento: media, desviación estándar y número de
# réplicas, calculados a partir de las mediciones.
resumen_dca <- datos_dca %>%
  group_by(tratamiento) %>%
  summarise(
    media = mean(biomasa_seca_g_L),
    de = sd(biomasa_seca_g_L),
    n = n(),
    .groups = "drop"
  )
print(resumen_dca)

# 3.4 ANOVA de una vía: ¿las medias de biomasa difieren entre las tres
# fuentes de nitrógeno? La hipótesis nula es que las tres medias son
# iguales. El valor p que imprime summary() dice si los datos son
# compatibles con esa hipótesis: si es menor que 0.05, las diferencias
# entre tratamientos no se explican por azar.
anova_dca <- aov(biomasa_seca_g_L ~ tratamiento, data = datos_dca)
print(summary(anova_dca))

# 3.5 Gráfica 1: biomasa seca por fuente de nitrógeno. Cada punto es un
# matraz (una réplica) y el rombo rojo es la media del tratamiento.
grafica_dca <- ggplot(
  datos_dca,
  aes(x = tratamiento, y = biomasa_seca_g_L)
) +
  geom_jitter(width = 0.15, size = 3, color = "#2166ac") +
  stat_summary(
    fun = mean, geom = "point",
    size = 4, color = "#b2182b", shape = 18
  ) +
  labs(
    x = "Fuente de nitrógeno",
    y = "Biomasa seca (g/L)",
    title = "Biomasa seca según la fuente de nitrógeno",
    subtitle = "DCA: cada punto es un matraz; el rombo rojo es la media (n = 4)"
  ) +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(grafica_dca)

# --------------------------------------------------------------------------
# 4. Diseño en bloques completos al azar (DBCA)
# --------------------------------------------------------------------------
# El mismo experimento se repite, pero ahora se sabe que los cuatro
# estantes del incubador no reciben exactamente la misma luz: el estante
# de arriba recibe un poco más que el de abajo. Esa diferencia entre
# estantes no es lo que se estudia, pero puede contaminar el resultado.
# La solución es bloquear: cada estante es un bloque y dentro de cada
# bloque se pone un matraz de cada tratamiento, en posiciones
# aleatorizadas. Así, si un estante produce más biomasa, ese aumento
# afecta por igual a los tres tratamientos y no se confunde con ellos.
# Son 4 bloques x 3 tratamientos = 12 matraces.

# 4.1 Aleatorización dentro de cada bloque: en cada estante se sortean
# las tres posiciones entre los tres tratamientos.
set.seed(2026)
asignacion_bloques <- tibble(
  bloque = rep(c("Estante 1", "Estante 2", "Estante 3", "Estante 4"), each = 3),
  posicion = rep(1:3, times = 4)
) %>%
  group_by(bloque) %>%
  mutate(tratamiento = sample(c("Nitrato", "Urea", "Amonio"))) %>%
  ungroup()
print(asignacion_bloques)

# 4.2 Se leen las mediciones de biomasa seca (g/L) por bloque y posición,
# y se unen con la asignación aleatorizada.
mediciones_bloques <- read_csv("datos_bloques_nitrogeno.csv", show_col_types = FALSE)
datos_bloques <- asignacion_bloques %>%
  left_join(mediciones_bloques, by = c("bloque", "posicion")) %>%
  mutate(
    bloque = factor(
      bloque,
      levels = c("Estante 1", "Estante 2", "Estante 3", "Estante 4")
    ),
    tratamiento = factor(
      tratamiento,
      levels = c("Nitrato", "Urea", "Amonio")
    )
  )
print(datos_bloques)

# 4.3 Resumen por tratamiento y por bloque, calculado a partir de los datos.
resumen_bloques_tratamiento <- datos_bloques %>%
  group_by(tratamiento) %>%
  summarise(
    media = mean(biomasa_seca_g_L),
    de = sd(biomasa_seca_g_L),
    n = n(),
    .groups = "drop"
  )
print(resumen_bloques_tratamiento)

resumen_bloques_estante <- datos_bloques %>%
  group_by(bloque) %>%
  summarise(
    media = mean(biomasa_seca_g_L),
    de = sd(biomasa_seca_g_L),
    n = n(),
    .groups = "drop"
  )
print(resumen_bloques_estante)

# 4.4 ANOVA con bloques: la fórmula incluye el bloque y el tratamiento.
# El bloque se estima aparte, así que su variación ya no se mezcla con
# la de los tratamientos ni con el error. El valor p del tratamiento
# se lee igual que en el DCA.
anova_bloques <- aov(
  biomasa_seca_g_L ~ bloque + tratamiento,
  data = datos_bloques
)
print(summary(anova_bloques))

# 4.5 Gráfica 2: biomasa seca por tratamiento, con las líneas que unen
# los matraces del mismo estante. Si los bloques funcionan, las líneas
# suben y bajan juntas: cada estante mantiene su nivel respecto a los
# demás en los tres tratamientos.
grafica_bloques <- ggplot(
  datos_bloques,
  aes(x = tratamiento, y = biomasa_seca_g_L, color = bloque, group = bloque)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "Estante 1" = "#2166ac",
      "Estante 2" = "#4393c3",
      "Estante 3" = "#E69F00",
      "Estante 4" = "#b2182b"
    )
  ) +
  labs(
    x = "Fuente de nitrógeno",
    y = "Biomasa seca (g/L)",
    color = "Bloque",
    title = "Biomasa seca por bloque (estante del incubador)",
    subtitle = "DBCA: cada línea es un estante, con un matraz por tratamiento"
  ) +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(grafica_bloques)

# --------------------------------------------------------------------------
# 5. Diseño factorial 2 x 2
# --------------------------------------------------------------------------
# Pregunta: ¿la temperatura y el tiempo de extracción cambian la
# cantidad de clorofila a que se extrae de Chlorella vulgaris, y el
# efecto de uno depende del otro? Dos factores, cada uno con dos
# niveles: temperatura 25 o 45 grados Celsius, y tiempo 30 o 90 minutos.
# Las cuatro combinaciones se prueban con 3 réplicas cada una: 12
# unidades experimentales. La respuesta es la concentración de
# clorofila a en el extracto, en mg/L.

# 5.1 Aleatorización: se reparten las 12 unidades entre las cuatro
# combinaciones al azar.
set.seed(2026)
combinaciones_factorial <- rep(
  c("25 °C_30 min", "25 °C_90 min", "45 °C_30 min", "45 °C_90 min"),
  each = 3
)
asignacion_factorial <- tibble(
  unidad = 1:12,
  combinacion = sample(combinaciones_factorial)
)
print(asignacion_factorial)

# 5.2 Se leen las mediciones de clorofila a (mg/L) por unidad y se
# separan los dos factores de la combinación.
mediciones_factorial <- read_csv("datos_factorial_clorofila.csv", show_col_types = FALSE)
datos_factorial <- asignacion_factorial %>%
  left_join(mediciones_factorial, by = "unidad") %>%
  separate(
    combinacion,
    into = c("temperatura", "tiempo"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
    temperatura = factor(temperatura, levels = c("25 °C", "45 °C")),
    tiempo = factor(tiempo, levels = c("30 min", "90 min"))
  )
print(datos_factorial)

# 5.3 Resumen por combinación: media, desviación estándar y réplicas.
resumen_factorial <- datos_factorial %>%
  group_by(temperatura, tiempo) %>%
  summarise(
    media = mean(clorofila_a_mg_L),
    de = sd(clorofila_a_mg_L),
    n = n(),
    .groups = "drop"
  )
print(resumen_factorial)

# 5.4 ANOVA factorial con interacción: la fórmula temperatura * tiempo
# estima los dos efectos principales y la interacción. La interacción
# dice si el efecto del tiempo cambia según la temperatura (o al
# revés). Un valor p pequeño en la interacción significa que los dos
# factores no actúan de forma independiente: hay que leerlos juntos.
anova_factorial <- aov(
  clorofila_a_mg_L ~ temperatura * tiempo,
  data = datos_factorial
)
print(summary(anova_factorial))

# 5.5 Gráfica 3: gráfico de interacción. Cada línea es un tiempo de
# extracción y cada punto es la media de una combinación, con su
# desviación estándar. Si las líneas son paralelas, no hay interacción:
# el efecto de la temperatura es el mismo a los dos tiempos. Si se
# cruzan o se separan, hay interacción.
grafica_factorial <- ggplot(
  resumen_factorial,
  aes(x = temperatura, y = media, color = tiempo, group = tiempo)
) +
  geom_errorbar(
    aes(ymin = media - de, ymax = media + de),
    width = 0.1, linewidth = 0.7
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("30 min" = "#2166ac", "90 min" = "#b2182b")
  ) +
  labs(
    x = "Temperatura de extracción (°C)",
    y = "Clorofila a (mg/L)",
    color = "Tiempo de extracción",
    title = "Interacción temperatura x tiempo: clorofila a",
    subtitle = "Media ± desviación estándar (n = 3) por combinación"
  ) +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(grafica_factorial)

# --------------------------------------------------------------------------
# 6. Exportar las gráficas
# --------------------------------------------------------------------------
# Todas las gráficas de la lección se guardan como archivos PNG en alta
# resolución (300 dpi), listas para un informe o una presentación.
ggsave("03_dca_nitrogeno.png", grafica_dca,
       width = 18, height = 12, units = "cm", dpi = 300)
ggsave("03_bloques_nitrogeno.png", grafica_bloques,
       width = 18, height = 12, units = "cm", dpi = 300)
ggsave("03_factorial_clorofila.png", grafica_factorial,
       width = 18, height = 12, units = "cm", dpi = 300)

# --------------------------------------------------------------------------
# Interpretación
# --------------------------------------------------------------------------
# Gráfica 1: Biomasa seca según la fuente de nitrógeno (DCA)
# - Eje X: fuente de nitrógeno (Nitrato, Urea o Amonio). Es una variable
#   categórica, sin unidades.
# - Eje Y: biomasa seca, en gramos por litro (g/L), medida al día 10 de
#   cultivo. Cada punto es un matraz y el rombo rojo es la media de las
#   cuatro réplicas del tratamiento.
# - Lo que se aprecia: los puntos de cada tratamiento forman un grupo
#   compacto y los grupos están separados: la urea queda por arriba, el
#   nitrato (control) queda en medio y el amonio queda por abajo. Las
#   réplicas dentro de cada tratamiento varían poco entre sí.
# - Qué significa: la fuente de nitrógeno cambia la biomasa producida.
#   La urea produce más biomasa que el nitrato y el amonio produce menos
#   que el control. El ANOVA de una vía confirma si esas diferencias
#   entre medias son mayores de lo que el azar entre réplicas explicaría.
#
# Gráfica 2: Biomasa seca por bloque (DBCA)
# - Eje X: fuente de nitrógeno (Nitrato, Urea o Amonio).
# - Eje Y: biomasa seca, en gramos por litro (g/L).
# - El color y las líneas distinguen el bloque: cada línea une los tres
#   matraces de un mismo estante del incubador.
# - Lo que se aprecia: dentro de cada estante se repite el mismo orden
#   (urea arriba, nitrato en medio, amonio abajo), y las líneas suben
#   juntas de un estante a otro: el estante de arriba produce más
#   biomasa en los tres tratamientos y el de abajo produce menos.
# - Qué significa: el estante sí influye en la biomasa, pero como cada
#   estante contiene los tres tratamientos, ese efecto no se confunde
#   con el de la fuente de nitrógeno. El ANOVA con bloques separa las
#   dos fuentes de variación y el efecto del tratamiento se estima sin
#   la contaminación del estante.
#
# Gráfica 3: Interacción temperatura x tiempo (factorial)
# - Eje X: temperatura de extracción, en grados Celsius (25 o 45).
# - Eje Y: concentración de clorofila a en el extracto, en miligramos
#   por litro (mg/L). Cada punto es la media de tres réplicas y las
#   barras son la desviación estándar.
# - El color distingue el tiempo de extracción: la línea azul es 30
#   minutos y la línea roja es 90 minutos.
# - Lo que se aprecia: las líneas no son paralelas. A 25 grados, pasar
#   de 30 a 90 minutos sube mucho la clorofila extraída; a 45 grados,
#   pasar de 30 a 90 minutos apenas la mueve, porque a esa temperatura
#   la extracción ya está casi completa a los 30 minutos.
# - Qué significa: hay interacción entre temperatura y tiempo. El
#   efecto del tiempo depende de la temperatura, así que no tiene
#   sentido hablar del efecto del tiempo en general: hay que decir a
#   qué temperatura. En la práctica, a 45 grados basta con 30 minutos
#   y alargar la extracción no aporta casi nada.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - El diseño se decide antes del experimento, según la pregunta: un
#   solo factor en condiciones homogéneas se resuelve con un diseño
#   completamente al azar; una fuente de variación conocida se controla
#   con bloques; dos factores que pueden combinarse se prueban con un
#   factorial.
# - La aleatorización reparte los tratamientos al azar entre las
#   unidades experimentales, para que ninguna diferencia sistemática
#   entre matraces se confunda con el efecto del tratamiento. Con
#   set.seed() la asignación es reproducible.
# - La repetición (aquí, n = 4 en el DCA y los bloques, n = 3 en el
#   factorial) es lo que permite estimar la variación entre unidades
#   iguales y decidir si las diferencias entre tratamientos son reales.
# - En un diseño en bloques, cada bloque contiene todos los
#   tratamientos, y el bloque entra en el ANOVA para que su variación
#   no contamine la comparación entre tratamientos.
# - En un factorial, la interacción dice si el efecto de un factor
#   depende del nivel del otro. Cuando hay interacción, los efectos se
#   leen juntos, nunca por separado.
# - En el ejemplo, la urea produce más biomasa seca que el nitrato y
#   el amonio produce menos, y en la extracción de clorofila a la
#   temperatura y el tiempo interactúan: a 45 grados, alargar de 30 a
#   90 minutos apenas aumenta la clorofila extraída.
