# ==========================================================================
# 02. Curva de crecimiento de Chlorella vulgaris bajo luz LED azul,
#   blanca (control) y roja
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Serie "Aprende R" — Lección 02
#
# Descripción: Se toma el archivo de datos ordenados de la Lección 01
#   (datos_chlorella.csv), se convierte la absorbancia (OD680) a densidad
#   celular (células/mL) con la calibración, se calcula la velocidad
#   específica de crecimiento (mu) en la fase exponencial A PARTIR DE LOS
#   DATOS (regresión lineal de ln(densidad) contra tiempo) y se grafica la
#   curva de crecimiento usando
#   tidyverse (ggplot2). Nada está dado de antemano: mu sale de las
#   mediciones, no de un valor supuesto.
#
# Objetivos de aprendizaje:
#   - Leer datos ordenados con read_csv().
#   - Convertir unidades con una calibración (OD680 a células/mL).
#   - Calcular la velocidad específica de crecimiento (mu).
#   - Graficar curvas de crecimiento con ggplot2, con gráficas claras
#     y bien presentadas.
#
# Datos: archivo datos_chlorella.csv, generado en la Lección 01.
#   Si no lo tienes, corre primero la Lección 01.
# ==========================================================================

# --------------------------------------------------------------------------
# 1. Cargar paquetes
# --------------------------------------------------------------------------
library(tidyverse)

# --------------------------------------------------------------------------
# 2. Leer los datos ordenados de la Lección 01
# --------------------------------------------------------------------------
datos <- read_csv("datos_chlorella.csv", show_col_types = FALSE)

# --------------------------------------------------------------------------
# 3. Convertir OD680 a densidad celular (células/mL)
# --------------------------------------------------------------------------
# Calibración: 1 unidad de OD680 = 1 x 10^7 células/mL.
datos <- datos %>%
  mutate(densidad = OD680 * 1e7)

# --------------------------------------------------------------------------
# 4. Resumen por día y condición (media ± desviación estándar)
# --------------------------------------------------------------------------
resumen <- datos %>%
  group_by(dia, luz) %>%
  summarise(
    media = mean(densidad),
    de    = sd(densidad),
    .groups = "drop"
  )

# --------------------------------------------------------------------------
# 5. Velocidad específica de crecimiento (mu) en fase exponencial
# --------------------------------------------------------------------------
# En la fase exponencial, ln(N) crece linealmente con el tiempo:
#   ln(N) = ln(N0) + mu * t
# mu es la pendiente de esa recta (unidades: 1/día).
# La fase exponencial se ubica entre los días 2 y 6 (ver la gráfica).

ajuste_azul <- lm(
  log(densidad) ~ dia,
  data = filter(datos, luz == "Azul", dia >= 2, dia <= 6)
)
ajuste_blanco <- lm(
  log(densidad) ~ dia,
  data = filter(datos, luz == "Blanco", dia >= 2, dia <= 6)
)
ajuste_rojo <- lm(
  log(densidad) ~ dia,
  data = filter(datos, luz == "Rojo", dia >= 2, dia <= 6)
)

mu_azul <- coef(ajuste_azul)[["dia"]]
mu_blanco <- coef(ajuste_blanco)[["dia"]]
mu_rojo <- coef(ajuste_rojo)[["dia"]]

tabla_mu <- tibble(
  luz = c("Azul", "Blanco", "Rojo"),
  mu = c(mu_azul, mu_blanco, mu_rojo),
  tiempo_duplicacion_dias = log(2) / c(mu_azul, mu_blanco, mu_rojo)
)

print(tabla_mu)

# --------------------------------------------------------------------------
# 6. Gráfica 1: Curva de crecimiento (escala logarítmica)
# --------------------------------------------------------------------------
grafica_crecimiento <- ggplot(
  resumen,
  aes(x = dia, y = media, color = luz, group = luz)
) +
  geom_errorbar(
    aes(ymin = media - de, ymax = media + de),
    width = 0.2, linewidth = 0.6
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_log10() +
  scale_color_manual(
    values = c("Azul" = "#2166ac", "Blanco" = "#E69F00", "Rojo" = "#b2182b")
  ) +
  labs(
    x = "Tiempo (días)",
    y = "Densidad celular (células/mL)",
    color = "Luz LED",
    title = "Curva de crecimiento de Chlorella vulgaris",
    subtitle = "Media ± desviación estándar (n = 3)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = c(0.18, 0.82),
    plot.title = element_text(face = "bold")
  )

print(grafica_crecimiento)

# --------------------------------------------------------------------------
# 7. Gráfica 2: Velocidad específica de crecimiento (mu)
# --------------------------------------------------------------------------
grafica_mu <- ggplot(
  tabla_mu,
  aes(x = luz, y = mu, fill = luz)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = round(mu, 2)),
    vjust = -0.5, size = 4.5
  ) +
  scale_fill_manual(
    values = c("Azul" = "#2166ac", "Blanco" = "#E69F00", "Rojo" = "#b2182b")
  ) +
  labs(
    x = "Luz LED",
    y = "Velocidad específica de crecimiento, μ (1/día)",
    title = "Velocidad específica de crecimiento (fase exponencial, días 2–6)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

print(grafica_mu)

# --------------------------------------------------------------------------
# 8. Exportar las gráficas
# --------------------------------------------------------------------------
# Todas las gráficas de la lección se guardan como archivos PNG en alta
# resolución (300 dpi), listas para un informe o una presentación.
ggsave("02_curva_crecimiento_chlorella.png", grafica_crecimiento,
       width = 18, height = 12, units = "cm", dpi = 300)
ggsave("02_velocidad_crecimiento_chlorella.png", grafica_mu,
       width = 14, height = 12, units = "cm", dpi = 300)

# --------------------------------------------------------------------------
# Interpretación
# --------------------------------------------------------------------------
# Gráfica 1: Curva de crecimiento (escala logarítmica)
# - Eje X: tiempo, en días (0 a 10).
# - Eje Y: densidad celular, en células/mL. La escala es logarítmica
#   (cada marca multiplica por 10), por eso la fase exponencial se ve
#   como una línea recta: en escala logarítmica, un crecimiento
#   exponencial es una recta. Las barras verticales son la desviación
#   estándar (n = 3): muestran cuánto varían las réplicas.
# - Lo que se aprecia: las tres curvas suben con el tiempo, pero la azul
#   sube más rápido y llega más alto (cerca de 2 × 10^7 células/mL); la
#   blanca (control) queda en medio (cerca de 1.6 × 10^7) y la roja queda
#   abajo (cerca de 1.3 × 10^7). Al principio las tres están juntas; se
#   separan a partir del día 2.
# - Qué significa: la luz azul produce más células (mayor crecimiento)
#   que el control con luz blanca, y la luz roja produce menos que el
#   control.
#
# Gráfica 2: Velocidad específica de crecimiento (mu)
# - Eje X: condición de luz (Azul, Blanco o Rojo).
# - Eje Y: velocidad específica de crecimiento (mu), en 1/día. Es la
#   pendiente de la recta en escala logarítmica durante la fase
#   exponencial (días 2-6). Mu es una velocidad instantánea: en un día de
#   fase exponencial la población se multiplica por e^mu (con mu = 0.73
#   1/día, se multiplica por 2.08 en un día, es decir, se duplica en
#   0.95 días). Un mu mayor es un crecimiento más rápido.
# - Lo que se aprecia: la barra azul (mu = 0.73) es la más alta, la
#   blanca (mu = 0.65) queda en medio y la roja (mu = 0.55) es la más
#   baja. El número está escrito encima de cada barra.
# - Qué significa: bajo luz azul el cultivo crece más rápido que el
#   control con luz blanca (mu = 0.65), y bajo luz roja crece más lento
#   que el control. El tiempo de duplicación es menor bajo luz azul
#   (0.95 días), intermedio con luz blanca (1.06 días) y mayor con luz
#   roja (1.26 días).
#
# - En un experimento real, estas diferencias se confirmarían con una
#   prueba estadística (por ejemplo, prueba t o ANOVA). Eso es justo lo
#   que veremos en las próximas lecciones.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - La velocidad específica de crecimiento (mu) no se supone: se calcula
#   A PARTIR DE LOS DATOS, con una regresión lineal del logaritmo de la
#   densidad contra el tiempo en la fase exponencial.
# - En escala logarítmica, la fase exponencial se ve como una recta, y su
#   pendiente es mu (en 1/día). Un mu mayor significa crecimiento más rápido.
# - La luz azul da un mu mayor (0.73) y un tiempo de duplicación menor
#   (0.95 días) que el control con luz blanca (mu = 0.65, duplicación
#   1.06 días) y que la luz roja (mu = 0.55, duplicación 1.26 días).
