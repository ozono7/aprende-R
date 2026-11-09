# ==========================================================================
# 02. Curva de crecimiento de Chlorella vulgaris bajo luz LED azul y roja
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
#   curva de crecimiento con el estilo de una revista científica usando
#   tidyverse (ggplot2). Nada está dado de antemano: mu sale de las
#   mediciones, no de un valor supuesto.
#
# Objetivos de aprendizaje:
#   - Leer datos ordenados con read_csv().
#   - Convertir unidades con una calibración (OD680 a células/mL).
#   - Calcular la velocidad específica de crecimiento (mu).
#   - Graficar curvas de crecimiento con ggplot2 al estilo científico.
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
ajuste_rojo <- lm(
  log(densidad) ~ dia,
  data = filter(datos, luz == "Rojo", dia >= 2, dia <= 6)
)

mu_azul <- coef(ajuste_azul)[["dia"]]
mu_rojo <- coef(ajuste_rojo)[["dia"]]

tabla_mu <- tibble(
  luz = c("Azul", "Rojo"),
  mu = c(mu_azul, mu_rojo),
  tiempo_duplicacion_dias = log(2) / c(mu_azul, mu_rojo)
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
    values = c("Azul" = "#2166ac", "Rojo" = "#b2182b")
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
    values = c("Azul" = "#2166ac", "Rojo" = "#b2182b")
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
# - La luz azul produce una mayor velocidad de crecimiento (mu) y una mayor
#   densidad final que la luz roja, lo que se ve en la curva y en mu.
# - El tiempo de duplicación es menor bajo luz azul.
# - En un experimento real, estas diferencias se confirmarían con una
#   prueba estadística (por ejemplo, prueba t o ANOVA). Eso es justo lo
#   que veremos en las próximas lecciones.
