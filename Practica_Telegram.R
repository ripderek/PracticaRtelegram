#install.packages("jsonlite")
#install.packages("dplyr")
#install.packages("ggplot2")

library(jsonlite)
library(dplyr)
library(ggplot2)

datos_json <- fromJSON("C:/INTELIGENCIA DE NEGOCIOS/PREPROCESAMIENTO/result1.json")

# Ver dataset
str(datos_json)

# filtrar message
mensajes_filtrados <- datos_json$messages %>%
  filter(type == "message")

show(mensajes_filtrados)

#filtrar person text
datos_filtrados <- mensajes_filtrados %>% select(from, text)

#Construir dataset
datos <- data.frame(
  Persona = datos_filtrados$from,
  Mensaje = datos_filtrados$text
)


show(datos)

# Contar la cantidad de mensajes por persona
datos_resumen <- datos %>%
  group_by(Persona) %>%
  summarise(Cantidad_Mensajes = n())



#Eliminar los usuarios que ya no existen 
#NA
datos_resumen <- datos_resumen %>%
  filter(!is.na(Persona))


# Crear un gráfico de barras
ggplot(datos_resumen, aes(x = Persona, y = Cantidad_Mensajes, fill = Persona)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de Mensajes por Persona",
       x = "Persona",
       y = "Cantidad de Mensajes") +
  scale_fill_manual(values = rainbow(nrow(datos_resumen))) +  #paleta de colores
  theme_minimal()

#Grafica de circular 
ggplot(datos_resumen, aes(x = "", y = Cantidad_Mensajes, fill = Persona)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  theme_minimal() +
  labs(title = "Cantidad de Mensajes por Persona")


