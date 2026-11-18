# Instalación de R y RStudio

## 1. Instalar R

R es el lenguaje; se instala antes que el editor. Descarga el instalador
desde CRAN, el repositorio oficial de R: https://cloud.r-project.org/

- **Windows:** "Download R for Windows" → base → "Download R-4.x.x for
  Windows". Ejecuta el instalador y acepta las opciones.
- **Mac:** "Download R for macOS". Descarga el archivo .pkg que corresponde
  a tu equipo (Apple silicon o Intel) y ejecútalo.
- **Linux (Ubuntu/Debian):** en la terminal:

  ```
  sudo apt update
  sudo apt install r-base
  ```

  Para otras distribuciones, CRAN tiene las instrucciones por sistema en la
  misma página.

## 2. Instalar un editor: RStudio o Positron

Con R ya instalado, instala uno de los dos editores (basta con uno):

- **RStudio Desktop** (gratuito): https://posit.co/download/rstudio-desktop/
  — descarga el instalador de tu sistema operativo y ejecútalo. Es el editor
  clásico de R: consola, editor de scripts, panel de gráficas y panel de
  paquetes en una sola ventana.
- **Positron** (gratuito): https://posit.co/download/positron/ — el editor
  nuevo de Posit, también para R y Python.

## 3. Instalar tidyverse

En la consola de R (o de RStudio o Positron):

```r
install.packages("tidyverse")
library(tidyverse)
```

La primera instalación descarga varios paquetes y tarda unos minutos. En
Linux, si la instalación pide librerías del sistema, instálalas antes; en
Ubuntu/Debian:

```
sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev
```

## 4. Abrir las lecciones

Descarga o clona este repositorio, abre el archivo `.R` de la lección en
RStudio o Positron y ejecútalo línea por línea o con el botón Source. Los
archivos `.csv` deben estar en la misma carpeta que el script.

---

## Sin instalar nada: opciones en la nube

Si no puedes instalar programas en tu equipo (equipo prestado, Chromebook,
tableta) o solo quieres empezar ya, estas opciones gratuitas corren R en el
navegador.

### Posit Cloud — posit.cloud

RStudio completo en el navegador, con R ya instalado. El plan gratuito da
25 horas de cómputo al mes, 1 GB de RAM y hasta 25 proyectos.

1. Entra a https://posit.cloud y crea una cuenta gratuita (puedes entrar
   con tu cuenta de Google).
2. New Project → New RStudio Project.
3. Sube los archivos de la lección (el `.R` y los `.csv`) con el botón
   Upload del panel Files.
4. En la consola, instala tidyverse una vez por proyecto:

   ```r
   install.packages("tidyverse")
   ```

5. Abre el script y ejecútalo.

### Google Colab — colab.research.google.com

Cuaderno gratuito en la nube con R como lenguaje.

1. Entra a https://colab.research.google.com y crea un cuaderno nuevo.
2. Runtime → Change runtime type → elige **R** → Save. (También puedes
   abrir directo https://colab.to/r, que ya abre un cuaderno en R.)
3. Sube el `.R` y los `.csv` de la lección con el botón de subir archivos
   del panel de archivos; quedan en `/content`.
4. En la primera celda instala tidyverse:

   ```r
   install.packages("tidyverse")
   ```

5. Copia el contenido del script en las celdas y ejecútalo, o ejecuta el
   archivo subido con `source("nombre-del-script.R")`.

La máquina virtual se reinicia al cerrar la sesión: los archivos subidos y
los paquetes instalados hay que volver a subirlos e instalarlos en la
siguiente sesión.

### Kaggle Notebooks — kaggle.com/code

Cuadernos gratuitos en la nube con R o Python, sin costo.

1. Crea una cuenta gratuita en https://www.kaggle.com y entra a Notebooks
   → New Notebook.
2. Elige **R** como lenguaje.
3. Sube los archivos de la lección y ejecuta el código en las celdas. El
   entorno de R ya trae instalados los paquetes habituales; si falta alguno,
   instálalo con `install.packages()`.
