# ==========================================================================
# Células eucariotas y procariotas: comparación cuantitativa
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Serie Aprende R — Biología celular
#
# Descripción: Segunda lección de biología celular. Se compara la célula
#   eucariota con la célula procariota con números, no solo con palabras:
#   a partir de los tamaños y los genomas de cinco células (archivo
#   datos_celulas_comparacion.csv) se calculan en R el volumen, la
#   superficie y la relación superficie/volumen de cada una. Los ejemplos
#   son Escherichia coli y Staphylococcus aureus (procariotas),
#   Saccharomyces cerevisiae (levadura) y Chlorella vulgaris (eucariotas
#   unicelulares), y una célula animal típica como referencia de eucariota
#   grande. También se comparan los tamaños de los genomas y la presencia
#   de organelos.
#
# Objetivos de aprendizaje:
#   - Leer una tabla de comparación con read_csv().
#   - Calcular volumen y superficie según la forma de la célula: esfera
#     para cocos y células eucariotas, cápsula para bacilos como E. coli.
#   - Calcular la relación superficie/volumen y explicar por qué las
#     células pequeñas la tienen mayor.
#   - Comparar tamaños de genomas en escala logarítmica (Mbp).
#   - Decir qué organelos tiene cada tipo de célula y cuáles no.
#
# Datos: archivo datos_celulas_comparacion.csv. Los tamaños son valores
#   típicos publicados: E. coli mide unos 2.0 µm de largo y 0.5 µm de
#   diámetro, con un genoma de 4.64 Mbp (cepa K-12); S. aureus es un coco
#   de 0.5-1.0 µm de diámetro, con un genoma de 2.80-2.90 Mbp;
#   Chlorella vulgaris mide 2-10 µm de diámetro y su genoma es de
#   37.34 Mbp (cepa UTEX 395, Guarnieri et al., 2018); S. cerevisiae
#   tiene un genoma de 12.1 Mbp; el genoma humano tiene 3200 Mbp.
#   Los diámetros típicos de la levadura (5 µm) y de la célula animal
#   (20 µm) son valores típicos de libros de texto. El número de genes
#   de Chlorella vulgaris no se incluye porque varía entre cepas y no
#   se verificó un valor único.
# ==========================================================================

# --------------------------------------------------------------------------
# 1. Cargar paquetes
# --------------------------------------------------------------------------
library(tidyverse)

# --------------------------------------------------------------------------
# 2. La pregunta y el contexto
# --------------------------------------------------------------------------
# Pregunta: ¿en qué se diferencia una célula eucariota de una procariota,
#   en números?
#
# - Eucariota: tiene núcleo definido (con membrana) y organelos con
#   membrana (mitocondrias, y cloroplastos en las fotosintéticas).
#   Ejemplos: Chlorella vulgaris, Saccharomyces cerevisiae, célula animal.
# - Procariota: no tiene núcleo definido ni organelos con membrana; su
#   ADN está en una región llamada nucleoide. Ejemplos: E. coli,
#   Staphylococcus aureus.
#
# Variables del archivo:
#   - organismo: nombre de la célula.
#   - tipo_celular: Eucariota o Procariota.
#   - forma: esfera (cocos y eucariotas), bastón (E. coli).
#   - diametro_um: diámetro típico de la célula, en µm.
#   - largo_um: largo de la célula, en µm (solo para el bastón).
#   - diametro_min_um, diametro_max_um: rango publicado, en µm.
#   - genoma_Mbp: tamaño del genoma, en millones de pares de bases (Mbp).
#   - genes_aprox: número aproximado de genes.
#   - nucleo, cloroplasto, mitocondria: Sí o No.
#   - ribosoma: 70S (procariota) u 80S (eucariota).

# --------------------------------------------------------------------------
# 3. Leer los datos
# --------------------------------------------------------------------------
datos <- read_csv("datos_celulas_comparacion.csv", show_col_types = FALSE)

print(datos)

# --------------------------------------------------------------------------
# 4. Volumen, superficie y relación superficie/volumen A PARTIR DE LOS DATOS
# --------------------------------------------------------------------------
# Esfera (cocos, levadura, Chlorella, célula animal):
#   V = (4/3) * pi * r^3 ; A = 4 * pi * r^2 ; A/V = 3 / r
# Cápsula (bastón de E. coli: cilindro con dos extremos semiesféricos):
#   V = pi * r^2 * (largo - 2*r) + (4/3) * pi * r^3
#   A = 2 * pi * r * (largo - 2*r) + 4 * pi * r^2
# con r = diámetro / 2. El volumen, la superficie y la relación
# superficie/volumen se calculan en R a partir del diámetro y del largo
# de la tabla; ningún valor está dado de antemano.

datos <- datos %>%
  mutate(
    radio_um = diametro_um / 2,
    volumen_um3 = if_else(
      is.na(largo_um),
      (4 / 3) * pi * radio_um^3,
      pi * radio_um^2 * (largo_um - 2 * radio_um) +
        (4 / 3) * pi * radio_um^3
    ),
    superficie_um2 = if_else(
      is.na(largo_um),
      4 * pi * radio_um^2,
      2 * pi * radio_um * (largo_um - 2 * radio_um) +
        4 * pi * radio_um^2
    ),
    sup_vol_por_um = superficie_um2 / volumen_um3
  )

print(select(datos, organismo, tipo_celular, volumen_um3,
             superficie_um2, sup_vol_por_um))

# --------------------------------------------------------------------------
# 5. Comparaciones entre células (cocientes calculados en R)
# --------------------------------------------------------------------------
# ¿Cuántas veces cabe una célula dentro de otra? Se divide el volumen
# de una entre el volumen de la otra. Lo mismo con los genomas.

volumen_ecoli <- filter(datos, organismo == "Escherichia coli")$volumen_um3
volumen_chlorella <- filter(datos, organismo == "Chlorella vulgaris")$volumen_um3
volumen_animal <- filter(datos, organismo == "Celula animal tipica")$volumen_um3
sup_vol_ecoli <- filter(datos, organismo == "Escherichia coli")$sup_vol_por_um
sup_vol_chlorella <- filter(datos, organismo == "Chlorella vulgaris")$sup_vol_por_um
genoma_ecoli <- filter(datos, organismo == "Escherichia coli")$genoma_Mbp
genoma_chlorella <- filter(datos, organismo == "Chlorella vulgaris")$genoma_Mbp
genoma_animal <- filter(datos, organismo == "Celula animal tipica")$genoma_Mbp

comparaciones <- tibble(
  comparacion = c(
    "Volumen de Chlorella / volumen de E. coli",
    "Volumen de célula animal / volumen de E. coli",
    "Volumen de célula animal / volumen de Chlorella",
    "Sup/vol de E. coli / sup/vol de Chlorella",
    "Genoma de Chlorella / genoma de E. coli",
    "Genoma humano / genoma de E. coli"
  ),
  veces = c(
    volumen_chlorella / volumen_ecoli,
    volumen_animal / volumen_ecoli,
    volumen_animal / volumen_chlorella,
    sup_vol_ecoli / sup_vol_chlorella,
    genoma_chlorella / genoma_ecoli,
    genoma_animal / genoma_ecoli
  )
)

print(comparaciones)

# --------------------------------------------------------------------------
# 6. ¿Qué organelos tiene cada célula?
# --------------------------------------------------------------------------
print(select(datos, organismo, tipo_celular, nucleo, cloroplasto,
             mitocondria, ribosoma))

# --------------------------------------------------------------------------
# 7. Gráfica 1: volumen celular (escala logarítmica)
# --------------------------------------------------------------------------
grafica_volumen <- ggplot(
  datos,
  aes(x = reorder(organismo, volumen_um3), y = volumen_um3,
      fill = tipo_celular)
) +
  geom_col(width = 0.6) +
  scale_y_log10() +
  scale_fill_manual(values = c("Eucariota" = "#1b9e77",
                               "Procariota" = "#d95f02")) +
  coord_flip() +
  labs(
    x = "Célula",
    y = "Volumen celular (µm³, escala logarítmica)",
    fill = "Tipo de célula",
    title = "Volumen celular: eucariotas contra procariotas"
  ) +
  theme_classic(base_size = 13)

print(grafica_volumen)

# --------------------------------------------------------------------------
# 8. Gráfica 2: relación superficie/volumen
# --------------------------------------------------------------------------
grafica_sup_vol <- ggplot(
  datos,
  aes(x = reorder(organismo, sup_vol_por_um), y = sup_vol_por_um,
      fill = tipo_celular)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = round(sup_vol_por_um, 2)),
    hjust = -0.2, size = 4
  ) +
  scale_fill_manual(values = c("Eucariota" = "#1b9e77",
                               "Procariota" = "#d95f02")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  coord_flip() +
  labs(
    x = "Célula",
    y = "Relación superficie/volumen (1/µm)",
    fill = "Tipo de célula",
    title = "Relación superficie/volumen por célula"
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none")

print(grafica_sup_vol)

# --------------------------------------------------------------------------
# 9. Gráfica 3: tamaño del genoma (escala logarítmica)
# --------------------------------------------------------------------------
grafica_genoma <- ggplot(
  datos,
  aes(x = reorder(organismo, genoma_Mbp), y = genoma_Mbp,
      fill = tipo_celular)
) +
  geom_col(width = 0.6) +
  scale_y_log10() +
  scale_fill_manual(values = c("Eucariota" = "#1b9e77",
                               "Procariota" = "#d95f02")) +
  coord_flip() +
  labs(
    x = "Célula",
    y = "Tamaño del genoma (Mbp, escala logarítmica)",
    fill = "Tipo de célula",
    title = "Tamaño del genoma: eucariotas contra procariotas",
    subtitle = "Mbp = millones de pares de bases"
  ) +
  theme_classic(base_size = 13)

print(grafica_genoma)

# --------------------------------------------------------------------------
# 10. Exportar las gráficas
# --------------------------------------------------------------------------
# Todas las gráficas de la lección se guardan como archivos PNG en alta
# resolución (300 dpi), listas para un informe o una presentación.
ggsave("eucariotas-procariotas-volumen.png", grafica_volumen,
       width = 18, height = 12, units = "cm", dpi = 300)
ggsave("eucariotas-procariotas-superficie-volumen.png", grafica_sup_vol,
       width = 18, height = 12, units = "cm", dpi = 300)
ggsave("eucariotas-procariotas-genoma.png", grafica_genoma,
       width = 18, height = 12, units = "cm", dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: Gráfica 1 (volumen celular)
# --------------------------------------------------------------------------
# - Eje X: célula (E. coli, S. aureus, S. cerevisiae, Chlorella vulgaris
#   y célula animal típica). No es una cantidad, son cinco células.
# - Eje Y: volumen celular, en micrómetros cúbicos (µm³), en escala
#   logarítmica (cada marca multiplica por 10). La escala es
#   logarítmica porque los volúmenes van de 0.36 µm³ (E. coli) a
#   4188.79 µm³ (célula animal): más de 10 000 veces de diferencia, y
#   en escala lineal las bacterias no se verían.
# - Lo que se aprecia: las dos procariotas (naranja) están abajo, con
#   volúmenes menores de 1 µm³: E. coli 0.36 µm³ y S. aureus 0.52 µm³.
#   Las eucariotas unicelulares (verde) están en medio, con 65.45 µm³
#   cada una (levadura y Chlorella miden lo mismo, 5 µm de diámetro).
#   La célula animal típica (20 µm de diámetro) está arriba, con
#   4188.79 µm³.
# - Qué significa: una célula de Chlorella vulgaris tiene un volumen
#   unas 182 veces mayor que una de E. coli, y una célula animal
#   típica tiene un volumen unas 11 636 veces mayor que E. coli y 64
#   veces mayor que Chlorella. El volumen crece con el cubo del
#   diámetro: una célula 4 veces más ancha (20 µm contra 5 µm) tiene
#   64 veces más volumen (4^3 = 64). Por eso las eucariotas pueden
#   tener organelos y las procariotas no: hay espacio para ellos.
# - Nota sobre E. coli: el volumen calculado con el modelo de cápsula
#   (0.36 µm³, con 0.5 µm de diámetro) es menor que el rango publicado
#   de 0.6-0.7 µm³. La diferencia es que las células publicadas se
#   midieron en medio rico, donde E. coli es más ancha (unos 0.7 µm
#   de diámetro). El modelo y la fórmula son los mismos; cambia el
#   diámetro que se pone en la tabla.
#
# Interpretación: Gráfica 2 (relación superficie/volumen)
# --------------------------------------------------------------------------
# - Eje X: célula (las mismas cinco). No es una cantidad, son cinco
#   células.
# - Eje Y: relación superficie/volumen, en 1/µm. Es la superficie de la
#   célula (µm²) dividida entre su volumen (µm³): las unidades µm²/µm³
#   se simplifican a 1/µm. El número está escrito al final de cada
#   barra.
# - Lo que se aprecia: E. coli tiene la relación más alta (8.73 1/µm),
#   seguida de S. aureus (6.00 1/µm). Las eucariotas unicelulares
#   tienen 1.20 1/µm y la célula animal típica solo 0.30 1/µm. La
#   relación baja cuando la célula crece: para una esfera es 3/r, así
#   que al duplicar el radio, la relación se reduce a la mitad.
# - Qué significa: las células pequeñas tienen mucha más superficie por
#   cada unidad de volumen, así que intercambian nutrientes y desechos
#   con el exterior más rápido en relación con su tamaño. La relación
#   superficie/volumen de E. coli es 7.3 veces mayor que la de
#   Chlorella. Por eso las bacterias crecen y se dividen rápido y las
#   células grandes necesitan organelos y sistemas internos de
#   transporte: su superficie ya no alcanza para abastecer todo su
#   volumen por difusión.
#
# Interpretación: Gráfica 3 (tamaño del genoma)
# --------------------------------------------------------------------------
# - Eje X: célula (las mismas cinco). No es una cantidad, son cinco
#   células.
# - Eje Y: tamaño del genoma, en millones de pares de bases (Mbp), en
#   escala logarítmica. 1 Mbp = 1 000 000 de pares de bases. La escala
#   es logarítmica porque los genomas van de 2.90 Mbp (S. aureus) a
#   3200 Mbp (humano): más de 1000 veces de diferencia.
# - Lo que se aprecia: las procariotas (naranja) tienen los genomas más
#   pequeños: S. aureus 2.90 Mbp y E. coli 4.64 Mbp. La levadura tiene
#   12.1 Mbp y Chlorella vulgaris 37.34 Mbp. El genoma humano (3200
#   Mbp) es mucho mayor que todos los demás.
# - Qué significa: el genoma de Chlorella vulgaris es 8.0 veces mayor
#   que el de E. coli y 3.1 veces mayor que el de la levadura, y el
#   genoma humano es 690 veces mayor que el de E. coli. Pero el tamaño
#   del genoma no crece igual que el número de genes: E. coli tiene
#   unos 4300 genes y el humano unos 21 000 (solo 4.9 veces más genes
#   para un genoma 690 veces mayor). Gran parte del genoma humano es
#   ADN que no codifica proteínas. Además, el tamaño del genoma no
#   dice qué tan compleja es la célula: Chlorella tiene un genoma
#   mayor que la levadura y ambas son eucariotas unicelulares.
#
# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - La diferencia entre eucariota y procariota se mide con números: una
#   célula de Chlorella vulgaris tiene un volumen unas 182 veces mayor
#   que una de E. coli, y una célula animal típica unas 11 636 veces
#   mayor que E. coli.
# - El volumen y la superficie se calculan A PARTIR DE LOS DATOS, según
#   la forma: esfera para cocos y eucariotas, cápsula para el bastón
#   de E. coli. El volumen crece con el cubo del diámetro.
# - La relación superficie/volumen es mayor en las células pequeñas:
#   E. coli 8.73 1/µm contra 1.20 1/µm de Chlorella y 0.30 1/µm de
#   una célula animal. Por eso las células grandes necesitan organelos
#   y transporte interno.
# - Los genomas también se comparan en escala logarítmica: Chlorella
#   vulgaris 37.34 Mbp, E. coli 4.64 Mbp, humano 3200 Mbp. El tamaño
#   del genoma no crece igual que el número de genes.
# - Las procariotas no tienen núcleo, ni cloroplastos, ni mitocondrias,
#   y sus ribosomas son 70S; las eucariotas tienen núcleo y
#   mitocondrias, ribosomas 80S, y cloroplastos solo si son
#   fotosintéticas como Chlorella vulgaris.
