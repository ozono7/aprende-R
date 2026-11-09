# ==========================================================================
# Biología celular: estructura y función de la célula
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Serie Aprende R — Biología celular
#
# Descripción: Primera lección de biología celular de la serie. Se parte
#   de mediciones reales del diámetro de células de Chlorella vulgaris
#   (archivo datos_diametros_chlorella.csv), se resumen en R y se calcula
#   el volumen de la célula A PARTIR DE LOS DATOS. Después se comparan los
#   tamaños típicos de los organelos con el tamaño de la célula, para
#   entender qué cabe dentro de una célula y qué hace cada organelo.
#   Chlorella vulgaris es el ejemplo principal: es una microalga verde
#   eucariota, fotosintética, con un solo cloroplasto en forma de copa,
#   un núcleo pequeño, mitocondrias y aparato de Golgi.
#
# Objetivos de aprendizaje:
#   - Leer mediciones de microscopía con read_csv().
#   - Resumir mediciones: media, desviación estándar, mínimo y máximo.
#   - Calcular el volumen de una célula esférica A PARTIR DE LOS DATOS.
#   - Comparar tamaños en escala logarítmica (µm contra nm).
#   - Relacionar cada organelo con su función en la célula.
#
# Datos: archivo datos_diametros_chlorella.csv, con 30 mediciones del
#   diámetro de células de Chlorella vulgaris en µm, obtenidas por
#   microscopía óptica. Los valores están dentro del rango publicado
#   para la especie (2-10 µm; Safi et al., 2014) y la cepa de referencia
#   SAG 211-11b mide 2.3-5.3 µm. Los tamaños de los organelos son los
#   rangos típicos de los libros de texto de biología celular (Campbell,
#   Alberts, Molecular Biology of the Cell): en Chlorella, que es una
#   célula pequeña, los organelos están en el extremo bajo de esos
#   rangos, como se muestra en la lección.
# ==========================================================================

# --------------------------------------------------------------------------
# 1. Cargar paquetes
# --------------------------------------------------------------------------
library(tidyverse)

# --------------------------------------------------------------------------
# 2. La pregunta y el contexto
# --------------------------------------------------------------------------
# Pregunta: ¿qué tan grande es una célula de Chlorella vulgaris y qué
#   tan grandes son sus organelos comparados con ella?
#
# - Organismo: Chlorella vulgaris, microalga verde unicelular, eucariota
#   y fotosintética. Esférica, sin flagelo, con pared celular.
# - Medición: diámetro de la célula en µm, medido al microscopio.
# - Réplicas: 30 células medidas (n = 30).
#
# Variables:
#   - celula: número de célula medida (1 a 30).
#   - diametro_um: diámetro de la célula, en micrómetros (µm).
#     1 µm = 0.001 mm = 1000 nm.

# --------------------------------------------------------------------------
# 3. Leer las mediciones
# --------------------------------------------------------------------------
datos <- read_csv("datos_diametros_chlorella.csv", show_col_types = FALSE)

print(datos)

# --------------------------------------------------------------------------
# 4. Resumen de las mediciones
# --------------------------------------------------------------------------
resumen <- datos %>%
  summarise(
    n            = n(),
    media_um     = mean(diametro_um),
    de_um        = sd(diametro_um),
    mediana_um   = median(diametro_um),
    minimo_um    = min(diametro_um),
    maximo_um    = max(diametro_um)
  )

print(resumen)

# --------------------------------------------------------------------------
# 5. Volumen de la célula A PARTIR DE LOS DATOS
# --------------------------------------------------------------------------
# Chlorella vulgaris es esférica, así que su volumen se calcula como el
# volumen de una esfera con el diámetro medio medido:
#   V = (4/3) * pi * r^3, con r = diámetro / 2
# La superficie es:
#   A = 4 * pi * r^2
# Y la relación superficie/volumen es:
#   A/V = 3 / r
# El radio es el diámetro medio medido dividido entre 2.

diametro_medio <- resumen$media_um
radio_um <- diametro_medio / 2

volumen_um3 <- (4 / 3) * pi * radio_um^3
superficie_um2 <- 4 * pi * radio_um^2
relacion_sup_vol <- superficie_um2 / volumen_um3

# ¿Cuántas células caben en fila en 1 mm?
celulas_por_mm <- 1000 / diametro_medio

tabla_celula <- tibble(
  diametro_medio_um      = diametro_medio,
  radio_um               = radio_um,
  volumen_um3            = volumen_um3,
  superficie_um2         = superficie_um2,
  sup_vol_por_um         = relacion_sup_vol,
  celulas_en_1_mm        = celulas_por_mm
)

print(tabla_celula)

# --------------------------------------------------------------------------
# 6. Los organelos: tamaño típico y función
# --------------------------------------------------------------------------
# Tabla con los organelos de una célula eucariota: su función y su tamaño
# típico en µm. Los tamaños son rangos de libros de texto (Campbell,
# Alberts). La columna "fraccion_diametro" se calcula en R: es el tamaño
# típico del organelo dividido entre el diámetro medio medido de la
# célula de Chlorella (sección 5). Un valor mayor que 1 significa que
# el organelo típico es más grande que toda la célula de Chlorella.

organelos <- tribble(
  ~organelo,         ~funcion,                                             ~medida,   ~tamano_tipico_um, ~tamano_min_um, ~tamano_max_um,
  "Núcleo",          "Guarda el ADN y controla las actividades de la célula", "diámetro", 10.0,            5.0,            20.0,
  "Cloroplasto",     "Fotosíntesis: captura la luz y fija el CO2",           "largo",    5.0,             2.0,            10.0,
  "Mitocondria",     "Respiración: produce ATP (energía) para la célula",    "diámetro", 1.0,             0.5,            1.0,
  "Aparato de Golgi", "Modifica, clasifica y empaca proteínas",              "diámetro", 0.5,             0.1,            1.0,
  "Lisosoma",        "Digiere y recicla material dentro de la célula",       "diámetro", 1.0,             0.5,            3.0,
  "Ribosoma",        "Sintetiza proteínas",                                  "diámetro", 0.025,           0.020,          0.030,
  "Pared celular",   "Protege la célula y le da forma",                      "grosor",   0.020,           0.002,          0.021
) %>%
  mutate(
    fraccion_diametro = tamano_tipico_um / diametro_medio,
    veces_ribosoma = tamano_tipico_um / 0.025
  )

print(organelos)

# --------------------------------------------------------------------------
# 7. Gráfica 1: distribución de los diámetros medidos
# --------------------------------------------------------------------------
grafica_histograma <- ggplot(datos, aes(x = diametro_um)) +
  geom_histogram(binwidth = 1, boundary = 2, fill = "#1b9e77",
                 color = "white") +
  geom_vline(xintercept = diametro_medio, color = "#d95f02",
             linewidth = 1, linetype = "dashed") +
  labs(
    x = "Diámetro de la célula (µm)",
    y = "Número de células",
    title = "Diámetros medidos de Chlorella vulgaris (n = 30)",
    subtitle = "Línea punteada: diámetro medio"
  ) +
  theme_classic(base_size = 13)

print(grafica_histograma)

# --------------------------------------------------------------------------
# 8. Gráfica 2: tamaño típico de los organelos (escala logarítmica)
# --------------------------------------------------------------------------
grafica_organelos <- ggplot(
  organelos,
  aes(x = reorder(organelo, tamano_tipico_um), y = tamano_tipico_um)
) +
  geom_col(fill = "#7570b3", width = 0.6) +
  geom_errorbar(
    aes(ymin = tamano_min_um, ymax = tamano_max_um),
    width = 0.2, linewidth = 0.6
  ) +
  scale_y_log10(
    breaks = c(0.02, 0.1, 1, 10),
    labels = c("0.02", "0.1", "1", "10")
  ) +
  coord_flip() +
  labs(
    x = "Organelo",
    y = "Tamaño típico (µm, escala logarítmica)",
    title = "Tamaño típico de los organelos de una célula eucariota",
    subtitle = "Barras: valor típico; líneas: rango (mínimo a máximo)"
  ) +
  theme_classic(base_size = 13)

print(grafica_organelos)

# --------------------------------------------------------------------------
# 9. Exportar las gráficas
# --------------------------------------------------------------------------
# Todas las gráficas de la lección se guardan como archivos PNG en alta
# resolución (300 dpi), listas para un informe o una presentación.
ggsave("biologia-celular-histograma-diametros.png", grafica_histograma,
       width = 16, height = 12, units = "cm", dpi = 300)
ggsave("biologia-celular-tamanos-organelos.png", grafica_organelos,
       width = 18, height = 12, units = "cm", dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: Gráfica 1 (distribución de los diámetros)
# --------------------------------------------------------------------------
# - Eje X: diámetro de la célula, en micrómetros (µm). El rango va de 2
#   a 10 µm, que es el rango publicado para Chlorella vulgaris.
# - Eje Y: número de células, en células (conteo). Cada barra cuenta
#   cuántas de las 30 células medidas caen en ese intervalo de 1 µm.
# - Lo que se aprecia: las 30 mediciones van de 2.5 a 8.6 µm, con la
#   mayoría entre 3 y 7 µm. La línea punteada marca el diámetro medio,
#   5.29 µm. La mediana es 5.65 µm y la desviación estándar es 1.74 µm:
#   las células no miden todas lo mismo, varían porque están en
#   distintas etapas de su ciclo (una célula recién dividida es más
#   pequeña que una a punto de dividirse).
# - Qué significa: una célula típica de Chlorella vulgaris mide unos
#   5.29 µm de diámetro. En 1 mm caben unas 189 células en fila. Con
#   ese diámetro medio, el volumen de la célula es 77.51 µm³ y su
#   superficie es 87.91 µm², con una relación superficie/volumen de
#   1.13 1/µm. Todos estos valores salen del diámetro medio medido,
#   no de un valor supuesto.
#
# Interpretación: Gráfica 2 (tamaño típico de los organelos)
# --------------------------------------------------------------------------
# - Eje X: organelo (núcleo, cloroplasto, mitocondria, aparato de Golgi,
#   lisosoma, ribosoma, pared celular). No es una cantidad, son siete
#   estructuras de la célula.
# - Eje Y: tamaño típico, en micrómetros (µm), en escala logarítmica
#   (cada marca multiplica por 10: 0.02, 0.1, 1, 10). La escala es
#   logarítmica porque los tamaños van de 0.02 µm (ribosoma y pared
#   celular) a 10 µm (núcleo): 500 veces de diferencia, y en escala
#   lineal los organelos pequeños no se verían. Las líneas verticales
#   son el rango (mínimo a máximo) de cada organelo.
# - Lo que se aprecia: el núcleo (10 µm) es el organelo más grande y
#   el ribosoma (0.025 µm) el más pequeño: el núcleo típico es 400
#   veces más grande que un ribosoma. El cloroplasto (5 µm) es el
#   segundo más grande. La pared celular (0.020 µm de grosor) es tan
#   delgada como un ribosoma.
# - Qué significa: comparado con el diámetro medio medido de Chlorella
#   (5.29 µm), el núcleo típico de 10 µm es 1.89 veces más grande que
#   toda la célula, y el cloroplasto típico de 5 µm es el 94.5% del
#   diámetro de la célula. Eso no es un error: los tamaños típicos son
#   de células eucariotas grandes (de 10 a 100 µm). En Chlorella, que
#   es una célula pequeña, el núcleo y el cloroplasto están en el
#   extremo bajo de esos rangos: Chlorella tiene un solo cloroplasto
#   en forma de copa que ocupa gran parte de la célula, y un núcleo
#   pequeño. La mitocondria típica (1 µm) es el 18.9% del diámetro de
#   la célula. El ribosoma (0.025 µm) cabe unas 212 veces en el
#   diámetro de la célula.
#
# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - El tamaño de una célula se mide al microscopio y se resume con media,
#   desviación estándar, mínimo y máximo: las células de Chlorella
#   vulgaris medidas tienen un diámetro medio de 5.29 µm (DE 1.74 µm,
#   rango 2.5-8.6 µm, n = 30).
# - El volumen de una célula esférica se calcula A PARTIR DEL DIÁMETRO
#   MEDIDO: con 5.29 µm de diámetro, la célula tiene 77.51 µm³ de
#   volumen y 87.91 µm² de superficie.
# - Los organelos tienen tamaños muy distintos: del ribosoma (0.025 µm)
#   al núcleo (10 µm) hay 400 veces de diferencia, por eso se grafican
#   en escala logarítmica.
# - Cada organelo tiene una función: el núcleo guarda el ADN, el
#   cloroplasto hace la fotosíntesis, la mitocondria produce ATP, el
#   aparato de Golgi empaca proteínas, el lisosoma recicla material,
#   el ribosoma sintetiza proteínas y la pared celular protege.
# - Chlorella vulgaris es una eucariota fotosintética pequeña: su único
#   cloroplasto en forma de copa y su núcleo ocupan una parte grande
#   de la célula, a diferencia de una célula eucariota grande.
