# Palabras más usadas en los discursos presidenciales en el Perú entre 2020-2024 (Con R)

Las palabras más usadas entre el 2022 y 2024 son mil y millones. Mientras que entre el 2020 y 2021 en el contexto de la pandemia del Covid -19 fue salud. 

# Al final encontrará el análisis por cada uno de los discursos

¿Cómo contar las palabras en un discurso presidencial en el Perú? (2020-2024)

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tibble")
install.packages("tidyr")
install.packages("readr")
install.packages("purrr")
install.packages("dplyr")
install.packages("stringr")
install.packages("forcats")
install.packages("lubridate")
install.packages("pdftools")
install.packages("tidytext")
install.packages("ggthemes")
library(tidytext)
library(tidyverse)
library(ggplot2)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(forcats)
library(lubridate)

library(pdftools)
library(ggthemes)

#Lista de los URLs de los PDFs

urls2024 <- c(
  "https://cdn.www.gob.pe/uploads/document/file/6756015/415-mensaje-a-la-nacion-fiestas-patrias.pdf")
urls2023<- c(
    "https://cdn.www.gob.pe/uploads/document/file/4914521/Mensaje%20a%20la%20naci%C3%B3n%20Fiestas%20Patrias.pdf")

urls2022<- c(
    "https://cdn.www.gob.pe/uploads/document/file/3463089/2022%20-%20MENSAJE%20A%20LA%20NACI%C3%93N%20PRESIDENTE%20PEDRO%20CASTILLO.pdf.pdf")

urls2021<- c(
"https://cdn.www.gob.pe/uploads/document/file/2049663/Mensaje_a_la_nacion_presidente_Pedro_Castillo.pdf.pdf")

urls2020 <- c(
    "https://cdn.www.gob.pe/uploads/document/file/1057525/Mensaje_a_la_Nacion_28_de_Julio_2020.pdf")

#LEER EN LINEA SIN DESCARGARLOS
# Leer el texto de cada PDF en línea sin descargarlo
textos_pdf2024 <- lapply(urls2024, pdf_text)
textos_pdf2023 <- lapply(urls2023, pdf_text)
textos_pdf2022 <- lapply(urls2022, pdf_text)
textos_pdf2021 <- lapply(urls2021, pdf_text)
textos_pdf2020 <- lapply(urls2020, pdf_text)

###### Supongamos que textos_pdf2024 es una lista de vectores de caracteres
# Primero, convertimos la lista en un tibble
textos_pdf2024 <- tibble(texto2024 = textos_pdf2024)
textos_pdf2023 <- tibble(texto2023 = textos_pdf2023)
textos_pdf2022 <- tibble(texto2022 = textos_pdf2022)
textos_pdf2021 <- tibble(texto2021 = textos_pdf2021)
textos_pdf2020 <- tibble(texto2020 = textos_pdf2020)





# Unir todas las páginas de cada documento en un solo string
textos_pdf2024 <- textos_pdf2024 %>%
  mutate(texto2024 = map_chr(texto2024, paste, collapse = " "))
textos_pdf2023 <- textos_pdf2023 %>%
  mutate(texto2023 = map_chr(texto2023, paste, collapse = " "))
textos_pdf2022 <- textos_pdf2022 %>%
  mutate(texto2022 = map_chr(texto2022, paste, collapse = " "))
textos_pdf2021 <- textos_pdf2021 %>%
  mutate(texto2021 = map_chr(texto2021, paste, collapse = " "))
textos_pdf2020 <- textos_pdf2020 %>%
  mutate(texto2020 = map_chr(texto2020, paste, collapse = " "))






##################DIVIDIR EL TEXTO EN PALABRAS
#Creamos un data nuevo con las palabras divididas

textos_pdf2024_dividido <- textos_pdf2024 %>%
  unnest_tokens(palabras2024, texto2024)
textos_pdf2023_dividido <- textos_pdf2023 %>%
  unnest_tokens(palabras2023, texto2023)
textos_pdf2022_dividido <- textos_pdf2022 %>%
  unnest_tokens(palabras2022, texto2022)
textos_pdf2021_dividido <- textos_pdf2021 %>%
  unnest_tokens(palabras2021, texto2021)
textos_pdf2020_dividido <- textos_pdf2020 %>%
  unnest_tokens(palabras2020, texto2020)

#Visualizamos 
View(textos_pdf2024_dividido)
View(textos_pdf2023_dividido)
View(textos_pdf2022_dividido)
View(textos_pdf2021_dividido)
View(textos_pdf2020_dividido)

#####################Conteo de palabras (word counts)
count_palabras2024 <- textos_pdf2024_dividido %>%
  count(palabras2024, sort = TRUE)
count_palabras2023 <- textos_pdf2023_dividido %>%
  count(palabras2023, sort = TRUE)
count_palabras2022 <- textos_pdf2022_dividido %>%
  count(palabras2022, sort = TRUE)
count_palabras2021 <- textos_pdf2021_dividido %>%
  count(palabras2021, sort = TRUE)
count_palabras2020 <- textos_pdf2020_dividido %>%
  count(palabras2020, sort = TRUE)



########################eliminar los stopwords
stopwords_es <- get_stopwords("es")  # "es" para español

a2024_sin_stopwords <- count_palabras2024 %>%
  anti_join(stopwords_es, by = c("palabras2024" = "word"))
a2023_sin_stopwords <- count_palabras2023 %>%
  anti_join(stopwords_es, by = c("palabras2023" = "word"))
a2022_sin_stopwords <- count_palabras2022 %>%
  anti_join(stopwords_es, by = c("palabras2022" = "word"))
a2021_sin_stopwords <- count_palabras2021 %>%
  anti_join(stopwords_es, by = c("palabras2021" = "word"))
a2020_sin_stopwords <- count_palabras2020 %>%
  anti_join(stopwords_es, by = c("palabras2020" = "word"))





print(a2020_sin_stopwords )


##############################FILTRADO DE LAS TOP25
# Filtrar las 5 palabras más frecuentes
a2024_top5 <- a2024_sin_stopwords %>%
  slice_max(n, n = 5)  # Selecciona las 5 filas con los valores más altos en la columna n
a2023_top5 <- a2023_sin_stopwords %>%
  slice_max(n, n = 5)  # Selecciona las 5 filas con los valores más altos en la columna n
a2022_top5 <- a2022_sin_stopwords %>%
  slice_max(n, n = 5)  # Selecciona las 5 filas con los valores más altos en la columna n
a2021_top5 <- a2021_sin_stopwords %>%
  slice_max(n, n = 5)  # Selecciona las 5 filas con los valores más altos en la columna n
a2020_top5 <- a2020_sin_stopwords %>%
  slice_max(n, n = 5)  # Selecciona las 5 filas con los valores más altos en la columna n



########VISUALIZACIÓN DE PALABRAS MAS FRECUENTES

# Crear el gráfico de barras
grafico2024 <- ggplot(a2024_top5, aes(x = reorder(palabras2024, -n), y = n)) +
  geom_col(fill = "#1f77b4") +  # Color verde claro para las barras
  labs(x = "Palabra", y = "Frecuencia") +  # Etiquetas de los ejes
  ggtitle("Palabras más frecuentes en el discurso presidencial 2024") +  # Título del gráfico
  theme_economist() +  # Usar un tema minimalista
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.1),  # Rotar etiquetas del eje x
    plot.title = element_text(hjust = 1)  # Centrar el título
  )
grafico2023 <- ggplot(a2023_top5, aes(x = reorder(palabras2023, -n), y = n)) +
  geom_col(fill = "#1f77b4") +  # Color verde claro para las barras
  labs(x = "Palabra", y = "Frecuencia") +  # Etiquetas de los ejes
  ggtitle("Palabras más frecuentes en el discurso presidencial 2023") +  # Título del gráfico
  theme_economist() +  # Usar un tema minimalista
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.1),  # Rotar etiquetas del eje x
    plot.title = element_text(hjust = 1)  # Centrar el título
  )
grafico2022 <- ggplot(a2022_top5, aes(x = reorder(palabras2022, -n), y = n)) +
  geom_col(fill = "#1f77b4") +  # Color verde claro para las barras
  labs(x = "Palabra", y = "Frecuencia") +  # Etiquetas de los ejes
  ggtitle("Palabras más frecuentes en el discurso presidencial 2022") +  # Título del gráfico
  theme_economist() +  # Usar un tema minimalista
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.1),  # Rotar etiquetas del eje x
    plot.title = element_text(hjust = 1)  # Centrar el título
  )
grafico2021 <- ggplot(a2021_top5, aes(x = reorder(palabras2021, -n), y = n)) +
  geom_col(fill = "#1f77b4") +  # Color verde claro para las barras
  labs(x = "Palabra", y = "Frecuencia") +  # Etiquetas de los ejes
  ggtitle("Palabras más frecuentes en el discurso presidencial 2021") +  # Título del gráfico
  theme_economist() +  # Usar un tema minimalista
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.1),  # Rotar etiquetas del eje x
    plot.title = element_text(hjust = 1)  # Centrar el título
  )
grafico2020 <- ggplot(a2020_top5, aes(x = reorder(palabras2020, -n), y = n)) +
  geom_col(fill = "#1f77b4") +  # Color verde claro para las barras
  labs(x = "Palabra", y = "Frecuencia") +  # Etiquetas de los ejes
  ggtitle("Palabras más frecuentes en el discurso presidencial 2020") +  # Título del gráfico
  theme_economist() +  # Usar un tema minimalista
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.1),  # Rotar etiquetas del eje x
    plot.title = element_text(hjust = 1)  # Centrar el título
  )


# Mostrar el gráfico en una ventana gráfica
plot(grafico2024)
plot(grafico2023)
plot(grafico2022)
plot(grafico2021)
plot(grafico2020)

![image](https://github.com/user-attachments/assets/5e82fa4c-0fc5-4555-a6ee-16eb3335163d)
![image](https://github.com/user-attachments/assets/e5ebf02d-a989-4928-81bc-0260d3ae615d)
![image](https://github.com/user-attachments/assets/140fc6d7-f5ed-4330-9342-1c5d6d689de2)
![image](https://github.com/user-attachments/assets/0f33d14c-4d26-4909-9623-573f9bc6391e)
![image](https://github.com/user-attachments/assets/be564de9-a2dd-4c28-bd22-4e1d3bfab5f5)





