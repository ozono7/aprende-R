# ==========================================================================
# 04. ANOVA de una vía: ¿cambia la remoción de un colorante al cambiar
#   la dosis de carbón activado?
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Serie "Aprende R" — Lección 04
#
# Descripción: Comparar las medias de más de dos grupos con el análisis
#   de varianza (ANOVA) de una vía. El ejemplo es de biotecnología
#   ambiental: se mide el porcentaje de remoción de azul de metileno,
#   un colorante que contamina el agua, al tratarla con carbón activado
#   en cuatro dosis (0.5, 1.0, 2.0 y 4.0 g/L), con cinco réplicas por
#   dosis. Todas las medias, el valor F, el valor p, el tamaño del
#   efecto y las comparaciones entre pares se calculan A PARTIR DE LOS
#   DATOS con tidyverse, aov() y TukeyHSD(). El script también revisa
#   los supuestos del ANOVA (normalidad de los residuales y varianzas
#   homogéneas) y explica qué hacer con el resultado de cada prueba.
#
# Objetivos de aprendizaje:
#   - Explicar cuándo se usa un ANOVA de una vía en lugar de varias
#     pruebas t: comparar las medias de tres o más grupos a la vez.
#   - Plantear la hipótesis nula y la alternativa de un ANOVA y leer
#     la tabla de ANOVA: suma de cuadrados, grados de libertad,
#     cuadrados medios, valor F y valor p.
#   - Calcular el tamaño del efecto (eta cuadrado) a partir de la
#     tabla de ANOVA y explicar qué proporción de la variación explica
#     el tratamiento.
#   - Revisar los supuestos del ANOVA: normalidad de los residuales
#     con la prueba de Shapiro-Wilk y varianzas homogéneas con la
#     prueba de Bartlett.
#   - Comparar los grupos por pares con la prueba de Tukey (HSD),
#     que corrige por hacer varias comparaciones a la vez.
#   - Graficar los resultados con ggplot2, con gráficas claras
#     y bien presentadas.
#
# Datos: archivo datos_remocion_azul_metileno.csv. Cada fila es una
#   medición: la dosis de carbón activado en g/L, el número de réplica
#   y el porcentaje de remoción de azul de metileno medido por
#   espectrofotometría en esa réplica.
# ==========================================================================

# --------------------------------------------------------------------------
# 1. Cargar paquetes
# --------------------------------------------------------------------------
library(tidyverse)

# La lección usa caracteres con acentos en los textos de las gráficas.
# Esta línea fija la configuración regional en UTF-8 para que los
# acentos se vean bien en cualquier equipo.
Sys.setlocale("LC_ALL", "C.UTF-8")

# --------------------------------------------------------------------------
# 2. La pregunta y los datos
# --------------------------------------------------------------------------
# Pregunta: ¿la dosis de carbón activado cambia el porcentaje de
# remoción de azul de metileno del agua?
# Tratamientos: cuatro dosis de carbón activado: 0.5, 1.0, 2.0 y 4.0
# gramos por litro (g/L). La dosis de 0.5 g/L es la más baja y sirve
# como referencia para ver cuánto mejora la remoción al subir la dosis.
# Unidades experimentales: 20 frascos con el mismo volumen de agua
# contaminada con azul de metileno (concentración inicial 50 mg/L),
# el mismo tiempo de contacto (60 minutos) y la misma agitación. Cada
# dosis se repite 5 veces, en frascos independientes.
#
# La respuesta es el porcentaje de remoción, que se calcula en el
# laboratorio a partir de la concentración inicial y la concentración
# final medida por espectrofotometría: remoción (%) = 100 x (C0 - C) /
# C0. El archivo de datos ya trae ese porcentaje por réplica, que es
# la medición que se analiza. El porcentaje es adimensional: es una
# proporción multiplicada por 100, por eso se expresa en % y no en
# unidades de concentración.

# 2.1 Se leen las mediciones: dosis, réplica y porcentaje de remoción.
datos <- read_csv("datos_remocion_azul_metileno.csv", show_col_types = FALSE) %>%
  mutate(
    dosis = factor(
      dosis_g_L,
      levels = c(0.5, 1.0, 2.0, 4.0),
      labels = c("0.5 g/L", "1.0 g/L", "2.0 g/L", "4.0 g/L")
    )
  )
print(datos)

# --------------------------------------------------------------------------
# 3. Resumen por dosis
# --------------------------------------------------------------------------
# Media, desviación estándar, error estándar e intervalo de confianza
# al 95% del porcentaje de remoción en cada dosis, calculados a partir
# de las cinco réplicas. El intervalo de confianza se construye con el
# valor t de Student para 4 grados de libertad (n - 1 = 5 - 1): media
# más o menos t por el error estándar. Si se repitiera el experimento
# muchas veces, el 95% de los intervalos así construidos contendría la
# media verdadera de esa dosis.
resumen <- datos %>%
  group_by(dosis) %>%
  summarise(
    media = mean(remocion_pct),
    de = sd(remocion_pct),
    n = n(),
    ee = de / sqrt(n),
    ic_inf = media - qt(0.975, df = n - 1) * ee,
    ic_sup = media + qt(0.975, df = n - 1) * ee,
    .groups = "drop"
  )
print(resumen)

# --------------------------------------------------------------------------
# 4. ANOVA de una vía
# --------------------------------------------------------------------------
# La pregunta tiene cuatro grupos y se quiere saber si sus medias son
# iguales. Hacer una prueba t por cada par (seis comparaciones) infla
# la probabilidad de encontrar una diferencia por azar: cada prueba
# tiene su propio 5% de error y los errores se acumulan. El ANOVA
# resuelve eso con una sola prueba para los cuatro grupos a la vez.
#
# Hipótesis nula: las cuatro medias poblacionales son iguales, es
# decir, la dosis de carbón activado no cambia el porcentaje de
# remoción. Hipótesis alternativa: al menos una media es distinta de
# las demás. El ANOVA compara dos fuentes de variación: la variación
# ENTRE grupos (qué tanto se separan las medias de las dosis) y la
# variación DENTRO de los grupos (qué tanto varían las réplicas de una
# misma dosis). El cociente de ambas, el valor F, dice si las medias
# están más separadas de lo que la variación entre réplicas explicaría
# por azar. Un valor p menor que 0.05 indica que las diferencias entre
# medias no se explican por azar.
anova_remocion <- aov(remocion_pct ~ dosis, data = datos)
tabla_anova <- summary(anova_remocion)
print(tabla_anova)

# 4.1 Tamaño del efecto (eta cuadrado): qué proporción de la variación
# total del porcentaje de remoción explica la dosis. Se calcula a
# partir de las sumas de cuadrados de la tabla de ANOVA: suma de
# cuadrados del tratamiento dividida entre la suma de cuadrados total.
# Un valor cercano a 1 significa que casi toda la variación entre
# frascos se debe a la dosis y no al azar entre réplicas.
sumas_cuadrados <- summary(anova_remocion)[[1]][["Sum Sq"]]
eta_cuadrado <- sumas_cuadrados[1] / sum(sumas_cuadrados)
print(paste("Eta cuadrado:", round(eta_cuadrado, 3)))

# --------------------------------------------------------------------------
# 5. Supuestos del ANOVA
# --------------------------------------------------------------------------
# El valor p del ANOVA es confiable cuando se cumplen dos supuestos
# sobre los residuales, que son la diferencia entre cada medición y la
# media de su dosis. Si un supuesto no se cumple, el valor p puede
# engañar y conviene usar una alternativa no paramétrica, como la
# prueba de Kruskal-Wallis. Aquí se revisan los dos supuestos con los
# residuales del modelo ajustado.
residuales <- residuals(anova_remocion)
valores_ajustados <- fitted(anova_remocion)

# 5.1 Normalidad de los residuales: prueba de Shapiro-Wilk. La
# hipótesis nula es que los residuales siguen una distribución normal.
# Un valor p mayor que 0.05 no rechaza la normalidad, así que el
# supuesto se considera cumplido.
prueba_shapiro <- shapiro.test(residuales)
print(prueba_shapiro)

# 5.2 Varianzas homogéneas: prueba de Bartlett. La hipótesis nula es
# que las cuatro dosis tienen la misma varianza en el porcentaje de
# remoción. Un valor p mayor que 0.05 no rechaza esa igualdad, así
# que el supuesto se considera cumplido. Con cinco réplicas por grupo
# y varianzas parecidas, el ANOVA es robusto.
prueba_bartlett <- bartlett.test(remocion_pct ~ dosis, data = datos)
print(prueba_bartlett)

# --------------------------------------------------------------------------
# 6. Comparaciones por pares: prueba de Tukey (HSD)
# --------------------------------------------------------------------------
# El ANOVA dice que al menos una media difiere, pero no dice cuáles.
# La prueba de Tukey compara los cuatro grupos por pares (seis
# comparaciones) y ajusta los valores p para que la probabilidad
# total de declarar una diferencia falsa se mantenga en 5%. La tabla
# muestra la diferencia de medias de cada par, su intervalo de
# confianza ajustado al 95% y el valor p ajustado. Un par difiere
# cuando su intervalo no incluye el cero, lo que equivale a un valor
# p ajustado menor que 0.05.
tukey <- TukeyHSD(anova_remocion)
print(tukey)

# Para graficar las comparaciones se convierte la tabla de Tukey en
# un data frame ordenado, con el nombre del par separado en sus dos
# dosis.
comparaciones <- as_tibble(tukey$dosis, rownames = "par") %>%
  separate(par, into = c("dosis_a", "dosis_b"), sep = "-", remove = FALSE) %>%
  mutate(
    significativa = `p adj` < 0.05,
    par = fct_reorder(par, diff)
  )
print(comparaciones)

# --------------------------------------------------------------------------
# 7. Gráficas
# --------------------------------------------------------------------------
# Gráfica 1: porcentaje de remoción por dosis. Cada punto es un
# frasco (una réplica) y el rombo rojo es la media de la dosis.
grafica_puntos <- ggplot(
  datos,
  aes(x = dosis, y = remocion_pct)
) +
  geom_jitter(width = 0.15, size = 3, color = "#2166ac") +
  stat_summary(
    fun = mean, geom = "point",
    size = 4, color = "#b2182b", shape = 18
  ) +
  labs(
    x = "Dosis de carbón activado",
    y = "Remoción de azul de metileno (%)",
    title = "Remoción de azul de metileno por dosis de carbón activado",
    subtitle = "Cada punto es un frasco; el rombo rojo es la media (n = 5)"
  ) +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(grafica_puntos)

# Gráfica 2: medias con intervalo de confianza al 95% por dosis. Cada
# punto es la media de la dosis y las barras son el intervalo de
# confianza calculado en la sección 3.
grafica_medias <- ggplot(
  resumen,
  aes(x = dosis, y = media)
) +
  geom_errorbar(
    aes(ymin = ic_inf, ymax = ic_sup),
    width = 0.15, linewidth = 0.8, color = "#2166ac"
  ) +
  geom_point(size = 4, color = "#2166ac") +
  labs(
    x = "Dosis de carbón activado",
    y = "Remoción de azul de metileno (%)",
    title = "Media de remoción con intervalo de confianza al 95%",
    subtitle = "El intervalo se calcula a partir de las 5 réplicas de cada dosis"
  ) +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(grafica_medias)

# Gráfica 3: diferencias entre pares de dosis según la prueba de
# Tukey. Cada punto es la diferencia de medias de un par y la barra es
# su intervalo de confianza ajustado al 95%. La línea vertical en cero
# marca la ausencia de diferencia: si la barra cruza el cero, el par
# no difiere de forma significativa.
grafica_tukey <- ggplot(
  comparaciones,
  aes(x = diff, y = par, color = significativa)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = lwr, xmax = upr),
    width = 0.2, linewidth = 0.8, orientation = "y"
  ) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("TRUE" = "#b2182b", "FALSE" = "#2166ac"),
    labels = c("TRUE" = "Diferencia significativa", "FALSE" = "Sin diferencia")
  ) +
  labs(
    x = "Diferencia de medias (puntos porcentuales)",
    y = "Par de dosis comparadas",
    color = "",
    title = "Comparaciones por pares: prueba de Tukey (HSD)",
    subtitle = "Intervalos de confianza ajustados al 95%"
  ) +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(grafica_tukey)

# Gráfica 4: diagnóstico de residuales. A la izquierda, residuales
# contra valores ajustados: los puntos deben repartirse al azar
# alrededor de la línea de cero, sin forma de embudo. A la derecha,
# gráfico cuantil-cuantil (Q-Q) de los residuales: si los puntos
# siguen la línea recta, los residuales se comportan como una
# distribución normal. Los dos paneles se construyen con los
# residuales del modelo de la sección 5.
diagnostico <- tibble(
  ajustado = valores_ajustados,
  residual = residuales
)

grafica_residuales <- ggplot(
  diagnostico,
  aes(x = ajustado, y = residual)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 3, color = "#2166ac") +
  labs(
    x = "Valor ajustado: media de la dosis (%)",
    y = "Residual: medición menos media de su dosis (%)",
    title = "Residuales contra valores ajustados",
    subtitle = "Los puntos se reparten al azar alrededor de cero"
  ) +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(grafica_residuales)

grafica_qq <- ggplot(
  diagnostico,
  aes(sample = residual)
) +
  geom_qq(size = 3, color = "#2166ac") +
  geom_qq_line(color = "#b2182b", linewidth = 0.8) +
  labs(
    x = "Cuantiles teóricos de la distribución normal",
    y = "Residuales ordenados (%)",
    title = "Gráfico cuantil-cuantil (Q-Q) de los residuales",
    subtitle = "Los puntos siguen la línea recta"
  ) +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(grafica_qq)

# --------------------------------------------------------------------------
# 8. Exportar las gráficas
# --------------------------------------------------------------------------
# Todas las gráficas de la lección se guardan como archivos PNG en alta
# resolución (300 dpi), listas para un informe o una presentación.
ggsave("04_remocion_por_dosis.png", grafica_puntos,
       width = 18, height = 12, units = "cm", dpi = 300)
ggsave("04_medias_ic95.png", grafica_medias,
       width = 18, height = 12, units = "cm", dpi = 300)
ggsave("04_comparaciones_tukey.png", grafica_tukey,
       width = 18, height = 12, units = "cm", dpi = 300)
ggsave("04_residuales_vs_ajustados.png", grafica_residuales,
       width = 18, height = 12, units = "cm", dpi = 300)
ggsave("04_qq_residuales.png", grafica_qq,
       width = 18, height = 12, units = "cm", dpi = 300)

# --------------------------------------------------------------------------
# Interpretación
# --------------------------------------------------------------------------
# Gráfica 1: Remoción de azul de metileno según la dosis (puntos)
# - Eje X: dosis de carbón activado (0.5, 1.0, 2.0 o 4.0 g/L). Es una
#   variable categórica en esta lección, con sus unidades en la etiqueta.
# - Eje Y: remoción de azul de metileno, en porcentaje (%). El
#   porcentaje es adimensional: mide qué parte del colorante inicial
#   se quitó del agua. Cada punto es un frasco y el rombo rojo es la
#   media de las cinco réplicas de la dosis.
# - Lo que se aprecia: los puntos de cada dosis forman un grupo
#   compacto y los grupos suben con la dosis: la remoción crece de
#   unos 47% a 0.5 g/L hasta unos 87% a 2.0 g/L, y de 2.0 a 4.0 g/L
#   apenas sube. Las réplicas dentro de cada dosis varían poco entre sí.
# - Qué significa: la dosis cambia la remoción, pero el aumento se
#   agota: a partir de 2.0 g/L el carbón extra ya no quita mucho más
#   colorante, porque casi todo el azul de metileno ya está adsorbido.
#   El ANOVA confirma si esas diferencias entre medias son mayores de
#   lo que la variación entre réplicas explicaría por azar.
#
# Gráfica 2: Media de remoción con intervalo de confianza al 95%
# - Eje X: dosis de carbón activado (0.5, 1.0, 2.0 o 4.0 g/L).
# - Eje Y: remoción de azul de metileno, en porcentaje (%). Cada punto
#   es la media de la dosis y las barras son su intervalo de confianza
#   al 95%, calculado a partir de las cinco réplicas.
# - Lo que se aprecia: los intervalos de 0.5 y 1.0 g/L no se tocan con
#   los de 2.0 y 4.0 g/L, mientras que los intervalos de 2.0 y 4.0 g/L
#   se solapan ampliamente. La media sube con la dosis y se estabiliza
#   alrededor de 87-90%.
# - Qué significa: la remoción a 0.5 y 1.0 g/L es claramente menor que
#   a 2.0 y 4.0 g/L, pero entre 2.0 y 4.0 g/L no se distingue una
#   diferencia. Los intervalos que se solapan apuntan en esa dirección,
#   y la prueba de Tukey de la gráfica 3 lo decide con el ajuste por
#   comparaciones múltiples.
#
# Gráfica 3: Comparaciones por pares (prueba de Tukey)
# - Eje X: diferencia de medias entre dos dosis, en puntos
#   porcentuales. Un valor positivo indica que la segunda dosis del par
#   remueve más que la primera.
# - Eje Y: el par de dosis comparadas (seis pares en total).
# - El color distingue si la diferencia es significativa: en rojo, los
#   pares cuya diferencia no se explica por azar; en azul, los pares
#   sin diferencia. La línea vertical punteada en cero marca la
#   ausencia de diferencia.
# - Lo que se aprecia: cinco de las seis barras quedan a la derecha
#   del cero, sin cruzarlo; solo la barra del par 4.0 g/L menos 2.0 g/L
#   cruza el cero.
# - Qué significa: todas las dosis difieren entre sí salvo 2.0 y 4.0
#   g/L. Duplicar la dosis de 2.0 a 4.0 g/L no aumenta la remoción de
#   forma significativa: 2.0 g/L es la dosis suficiente para este
#   tratamiento y usar el doble de carbón sería gastar material sin
#   ganar remoción.
#
# Gráfica 4: Residuales contra valores ajustados
# - Eje X: valor ajustado, es decir, la media de la dosis a la que
#   pertenece cada frasco, en porcentaje (%).
# - Eje Y: residual, la diferencia entre la medición del frasco y la
#   media de su dosis, en puntos porcentuales.
# - Lo que se aprecia: los puntos se reparten al azar alrededor de la
#   línea de cero, sin forma de embudo ni curva, y con una dispersión
#   parecida en las cuatro dosis.
# - Qué significa: no hay un patrón sistemático que el modelo haya
#   dejado sin explicar, y la variación entre réplicas es parecida en
#   todas las dosis, lo que apoya el supuesto de varianzas homogéneas
#   que también revisa la prueba de Bartlett.
#
# Gráfica 5: Gráfico cuantil-cuantil (Q-Q) de los residuales
# - Eje X: cuantiles teóricos de la distribución normal, adimensionales
#   porque son los valores que los residuales tendrían si fueran
#   perfectamente normales.
# - Eje Y: residuales ordenados de menor a mayor, en puntos
#   porcentuales.
# - Lo que se aprecia: los puntos siguen de cerca la línea recta, sin
#   alejarse de forma sistemática en los extremos.
# - Qué significa: los residuales se comportan como una distribución
#   normal, lo que apoya el supuesto de normalidad que también revisa
#   la prueba de Shapiro-Wilk. Con los dos supuestos cumplidos, el
#   valor p del ANOVA es confiable.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - Con tres o más grupos se usa un ANOVA de una vía, no una prueba t
#   por cada par: el ANOVA hace una sola prueba y no acumula el error
#   de declarar diferencias falsas.
# - La hipótesis nula del ANOVA es que todas las medias son iguales.
#   El valor F compara la variación entre las medias de los grupos
#   con la variación entre réplicas del mismo grupo, y el valor p dice
#   si las diferencias entre medias se explican por azar.
# - El tamaño del efecto (eta cuadrado) dice qué proporción de la
#   variación total explica el tratamiento, más allá de si el valor p
#   es significativo o no.
# - Antes de creer el valor p se revisan los supuestos sobre los
#   residuales: normalidad con Shapiro-Wilk y varianzas homogéneas
#   con Bartlett. Si un supuesto no se cumple, la alternativa es la
#   prueba no paramétrica de Kruskal-Wallis.
# - El ANOVA dice que al menos una media difiere, pero no cuáles: para
#   saberlo se usa la prueba de Tukey, que compara los grupos por
#   pares con los valores p ajustados por hacer varias comparaciones.
# - En el ejemplo, la dosis de carbón activado cambia la remoción de
#   azul de metileno: la remoción sube de unos 47% a 0.5 g/L hasta
#   unos 87% a 2.0 g/L, y de 2.0 a 4.0 g/L ya no mejora de forma
#   significativa. La dosis de 2.0 g/L es la suficiente para este
#   tratamiento.
