library(tm)
install.packages("httr")
install.packages("jsonlite")
install.packages("tm")  # Para análisis de texto si quieres usarlo

library(httr)
library(jsonlite)

# Define la URL base del API
base_url <- "http://data.riksdagen.se/dokumentlista/"


# Define los parámetros de la solicitud (ejemplo: búsqueda de "budget" y 10 resultados)
query <- "budget"  # La palabra clave que quieres buscar
limit <- 10        # Límite de resultados

# Construye la URL completa con los parámetros
url <- paste0(base_url, "?sok=", query, "&doktyp=&utformat=json&sort=datum&sortorder=desc&antal=", limit)

# Realiza la solicitud GET
response <- GET(url)



