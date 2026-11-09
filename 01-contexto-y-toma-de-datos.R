# ==========================================================================
# 01. Contexto y toma de datos: crecimiento de Chlorella vulgaris
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Serie "Aprende R" — Lección 01
#
# Descripción: Primera lección de la serie. Se presenta el experimento
#   (crecimiento de Chlorella vulgaris bajo luz LED azul, blanca
#   (control) y roja), se leen
#   los datos tal como se registraron en el laboratorio (archivo
#   datos_chlorella_ancho.csv), se ordenan en R (datos ordenados o
#   "tidy data") y se hace un primer análisis exploratorio: resumen por
#   grupo, valores faltantes y gráficas iniciales. Los datos ordenados se
#   guardan para la Lección 02.
#
# Objetivos de aprendizaje:
#   - Describir un experimento: variables, unidades y réplicas.
#   - Leer datos de un archivo con read_csv().
#   - Pasar los datos del formato ancho (libreta de laboratorio) al
#     formato largo (tidy data) con pivot_longer().
#   - Resumir los datos por grupo (media, desviación estándar).
#   - Explorar los datos con gráficas antes de analizarlos.
#
# Datos: archivo datos_chlorella_ancho.csv, con las mediciones de
#   absorbancia (OD680) registradas en el laboratorio. Los valores son
#   simulados con fines didácticos, con magnitudes realistas.
# ==========================================================================

# --------------------------------------------------------------------------
# 1. Cargar paquetes
# --------------------------------------------------------------------------
library(tidyverse)

# --------------------------------------------------------------------------
# 2. El experimento (contexto)
# --------------------------------------------------------------------------
# Pregunta: ¿La luz LED azul o la luz LED roja produce un mayor crecimiento
#   de la microalga Chlorella vulgaris, comparadas con el control con luz
#   blanca?
#
# Diseño:
#   - Organismo: Chlorella vulgaris (microalga verde).
#   - Tratamientos: luz LED azul, luz LED blanca (control) y luz LED
#     roja (3 condiciones).
#   - Réplicas: 3 cultivos independientes por condición (n = 3).
#   - Duración: 10 días, con una medición diaria.
#   - Variable medida: absorbancia a 680 nm (OD680), que es proporcional
#     a la densidad celular. En la práctica, las muestras con OD680 mayor
#     a 1.0 se diluyen antes de medir.
#
# Variables:
#   - dia: día del cultivo (0 a 10).
#   - luz: condición de luz (Azul, Blanco o Rojo).
#   - replica: número de réplica (1, 2 o 3).
#   - OD680: absorbancia a 680 nm (sin unidades).

# --------------------------------------------------------------------------
# 3. Leer los datos tal como se registraron en el laboratorio
# --------------------------------------------------------------------------
# El archivo viene en formato ANCHO, como en la libreta o la hoja de
# cálculo: una fila por día y una columna por réplica. Así NO se analizan
# bien en R; primero hay que ordenarlos (sección 4).

datos_ancho <- read_csv("datos_chlorella_ancho.csv", show_col_types = FALSE)

print(datos_ancho)

# --------------------------------------------------------------------------
# 4. Ordenar los datos (tidy data, formato largo)
# --------------------------------------------------------------------------
# Regla de los datos ordenados ("tidy data"):
#   - Cada variable es una columna.
#   - Cada observación es una fila.
#   - Cada valor es una celda.
# Con pivot_longer() pasamos del formato ancho al formato largo.

datos <- datos_ancho %>%
  pivot_longer(
    cols = -dia,
    names_to = c("luz", "replica"),
    names_sep = "_R",
    values_to = "OD680"
  ) %>%
  mutate(replica = as.integer(replica))

print(datos)

# --------------------------------------------------------------------------
# 5. Primer análisis: resumen por día y condición
# --------------------------------------------------------------------------
resumen <- datos %>%
  group_by(dia, luz) %>%
  summarise(
    n        = n(),
    media_OD = mean(OD680),
    de_OD    = sd(OD680),
    min_OD   = min(OD680),
    max_OD   = max(OD680),
    .groups = "drop"
  )

print(resumen)

# --------------------------------------------------------------------------
# 6. Revisión de calidad: valores faltantes y valores fuera de rango
# --------------------------------------------------------------------------
# ¿Hay valores faltantes? (debe ser 0)
print(sum(is.na(datos$OD680)))

# ¿Hay valores fuera de rango? (OD680 debe estar entre 0 y 3)
print(filter(datos, OD680 < 0 | OD680 > 3))

# --------------------------------------------------------------------------
# 7. Exploración gráfica (antes de analizar, hay que mirar los datos)
# --------------------------------------------------------------------------
# Gráfica 1: OD680 contra el tiempo; puntos = réplicas, línea = media.
grafica_puntos <- ggplot(datos, aes(x = dia, y = OD680, color = luz)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_line(
    data = resumen,
    aes(x = dia, y = media_OD, group = luz),
    linewidth = 1
  ) +
  scale_color_manual(
    values = c("Azul" = "#2166ac", "Blanco" = "#E69F00", "Rojo" = "#b2182b")
  ) +
  labs(
    x = "Tiempo (días)",
    y = "Absorbancia a 680 nm (OD680)",
    color = "Luz LED",
    title = "Crecimiento de Chlorella vulgaris (datos crudos)",
    subtitle = "Puntos: réplicas individuales; línea: media por día"
  ) +
  theme_classic(base_size = 13)

print(grafica_puntos)

# Gráfica 2: distribución de OD680 por condición de luz (caja y puntos).
grafica_caja <- ggplot(datos, aes(x = luz, y = OD680, fill = luz)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.6) +
  scale_fill_manual(
    values = c("Azul" = "#2166ac", "Blanco" = "#E69F00", "Rojo" = "#b2182b")
  ) +
  labs(
    x = "Luz LED",
    y = "Absorbancia a 680 nm (OD680)",
    title = "Distribución de OD680 por condición de luz"
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none")

print(grafica_caja)

# --------------------------------------------------------------------------
# 8. Guardar los datos ordenados para la Lección 02
# --------------------------------------------------------------------------
write_csv(datos, "datos_chlorella.csv")

# --------------------------------------------------------------------------
# 9. Exportar las gráficas
# --------------------------------------------------------------------------
# Todas las gráficas de la lección se guardan como archivos PNG en alta
# resolución (300 dpi), listas para un informe o una presentación.
ggsave("01_exploracion_puntos.png", grafica_puntos,
       width = 18, height = 12, units = "cm", dpi = 300)
ggsave("01_exploracion_caja.png", grafica_caja,
       width = 16, height = 12, units = "cm", dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: Gráfica 1 (OD680 contra el tiempo)
# --------------------------------------------------------------------------
# - Eje X: tiempo, en días (0 a 10). Es el tiempo desde que se inició el
#   cultivo.
# - Eje Y: absorbancia a 680 nm (OD680), adimensional (no tiene unidades:
#   es una medida de cuánta luz absorbe el cultivo, y crece cuando hay más
#   células). Un OD680 de 0 significa cultivo casi sin células; un OD680
#   de 2 significa un cultivo denso.
# - Lo que se aprecia: los puntos (cada réplica) suben con el tiempo y la
#   línea (la media por día) también sube. La curva azul sube más rápido
#   y llega más alto; la blanca (control, luz blanca) queda en medio y
#   la roja queda abajo. Al principio (días 0-1) las tres están casi en
#   cero; después se separan.
# - Qué significa: el cultivo crece con el tiempo. La luz azul produce
#   más crecimiento que el control con luz blanca, y la luz roja produce
#   menos que el control.
#
# Interpretación: Gráfica 2 (distribución de OD680 por luz)
# --------------------------------------------------------------------------
# - Eje X: condición de luz (Azul, Blanco o Rojo). No es una cantidad,
#   son tres grupos.
# - Eje Y: absorbancia a 680 nm (OD680), adimensional, igual que arriba.
# - Lo que se aprecia: cada caja resume todas las mediciones de un
#   tratamiento. La línea dentro de la caja es la mediana (el valor del
#   medio). La caja cubre el 50% central de los datos. Los puntos son las
#   mediciones. La caja azul está más arriba que la blanca (control) y
#   la roja está más abajo: la azul tiene los valores mayores, la blanca
#   queda en medio y la roja los menores.
# - Qué significa: en conjunto, la luz azul da valores de OD680 más altos
#   que el control con luz blanca, y la luz roja da valores más bajos que
#   el control. La dispersión (el ancho de la caja y de los puntos)
#   muestra que los valores varían entre días y réplicas, lo cual es
#   normal porque el cultivo crece con el tiempo.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - Los datos se toman en el laboratorio como mediciones reales (OD680 por
#   día y por réplica) y se ordenan en formato tidy para poder analizarlos.
# - Antes de analizar, hay que mirar los datos: un gráfico de puntos contra
#   el tiempo muestra el crecimiento, y un gráfico de caja compara los
#   tratamientos.
# - La OD680 es adimensional y crece cuando hay más células; por eso sirve
#   para seguir el crecimiento del cultivo.
# - La luz azul produce más crecimiento que el control con luz blanca, y
#   la luz roja produce menos que el control.

# --------------------------------------------------------------------------
# Qué sigue (Lección 02)
# --------------------------------------------------------------------------
# En la Lección 02 tomaremos este archivo (datos_chlorella.csv),
# convertiremos la OD680 a densidad celular (células/mL) con la
# calibración, calcularemos la velocidad específica de crecimiento (mu)
# A PARTIR DE LOS DATOS, y graficaremos la curva de crecimiento. Nada de
# lo que calcularemos está dado de
# antemano: todo sale de estas mediciones.
