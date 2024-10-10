install.packages("tidyverse")
install.packages("ggplot2")
install.packages("skimr")
install.packages("lubridate")
install.packages("scales")
library(dplyr)
library(ggplot2)
library(janitor)
library(tidyverse)
library(skimr)
library(lubridate)
library(scales)


# Lista de archivos y nombres dinámicos
archivos <- list.files(pattern = "divvy-tripdata.csv")
nombres <- archivos %>%
  str_extract("[0-9]{6}") %>%
  str_replace("-", "_")

# Leer, limpiar y agregar columnas adicionales para cada data frame
for (i in seq_along(archivos)) {
  # Cargar y limpiar datos
  df <- read.csv(archivos[i]) %>%
    clean_names() %>%
    drop_na()
  
  # Convertir columnas 'started_at' y 'ended_at' a formato fecha-hora, calcular ride_length y day_of_week
  df <- df %>%
    mutate(
      # Convertir las columnas a formato fecha-hora
      started_at = ymd_hms(started_at),
      ended_at = ymd_hms(ended_at),
      
      # Calcular la duración del viaje (ride_length) en segundos
      ride_length = as.numeric(difftime(ended_at, started_at, units = "secs")),
      ride_length_formatted = as.POSIXct(ride_length, origin = "1970-01-01", tz = "UTC") - as.POSIXct(0, origin = "1970-01-01", tz = "UTC"),
      
      # Obtener el nombre completo del día de la semana (Monday, Tuesday, etc.)
      day_of_week = wday(started_at, label = TRUE, abbr = FALSE, week_start = 1)
    )
  
  # Asignar el data frame con el nombre correspondiente
  assign(paste0("datos_", nombres[i]), df)
}

head(datos_202309)
###################################################################################
duracion_media_dia <- datos_202408 %>%
  group_by(day_of_week, member_casual) %>%
  summarize(media_duracion = mean(ride_length, na.rm = TRUE))

# Gráfico de barras de duración media, diferenciado por tipo de usuario
ggplot(duracion_media_dia, aes(x = reorder(day_of_week, -media_duracion), y = media_duracion, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +  # Usa position = "dodge" para barras lado a lado
  labs(title = "Duración Media de los Viajes por Día de la Semana y Tipo de Usuario",
       x = "Día de la Semana",
       y = "Duración Media del Viaje (segundos)") +
  theme_minimal() +
  scale_fill_manual(values = c("orange", "blue"), name = "Tipo de Usuario", labels = c("Casual", "Miembro"))  # Cambia los colores según prefieras
#####################################################################################################

viajes_por_bicicleta <- as.data.frame(table(datos_202408$rideable_type, datos_202408$member_casual))

# Renombrar las columnas para mayor claridad
colnames(viajes_por_bicicleta) <- c("rideable_type", "member_casual", "cantidad_viajes")

# Gráfico de barras de la cantidad de viajes por tipo de bicicleta y tipo de usuario
ggplot(viajes_por_bicicleta, aes(x = rideable_type, y = cantidad_viajes, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +  # Usa position = "dodge" para barras lado a lado
  labs(title = "Cantidad de Viajes por Tipo de Bicicleta y Tipo de Usuario",
       x = "Tipo de Bicicleta",
       y = "Cantidad de Viajes") +
  theme_minimal() +
  scale_fill_manual(values = c("orange", "blue"), name = "Tipo de Usuario", labels = c("Casual", "Miembro")) 
##########################################################################################################################################################
# Convertir la columna de fecha en tipo Date
datos_202408$started_at <- as.Date(datos_202408$started_at)

# Crear un nuevo dataframe para la cantidad de viajes por fecha y tipo de usuario
viajes_por_fecha <- datos_202408 %>%
  group_by(started_at, member_casual) %>%
  summarize(cantidad_viajes = n(), .groups = "drop")

# Gráfico de línea
ggplot(viajes_por_fecha, aes(x = started_at, y = cantidad_viajes, color = member_casual)) +
  geom_line(size = 1.2) + # Línea más gruesa
  geom_point(size = 2.5, alpha = 0.7) + # Puntos en la línea
  labs(title = "Cantidad de Viajes a lo Largo del Tiempo por Tipo de Usuario",
       x = "Fecha",
       y = "Cantidad de Viajes") +
  theme_minimal(base_size = 15) + # Aumentar el tamaño de la base de texto
  scale_color_manual(values = c("member" = "blue", "casual" = "orange")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20), # Centrar y ajustar tamaño del título
    axis.title.x = element_text(face = "bold", size = 14), # Estilo del título del eje X
    axis.title.y = element_text(face = "bold", size = 14), # Estilo del título del eje Y
    legend.title = element_blank(), # Eliminar título de la leyenda
    legend.position = "top", # Cambiar la posición de la leyenda
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"), # Línea de cuadrícula mayor
    panel.grid.minor = element_blank() # Sin línea de cuadrícula menor
  )
##################################################################################################################################


# Duración media de los viajes por día de la semana y tipo de usuario
duracion_media_dia <- datos_202407 %>%
  group_by(day_of_week, member_casual) %>%
  summarize(media_duracion = mean(ride_length, na.rm = TRUE), .groups = "drop")

# Gráfico de barras de duración media
ggplot(duracion_media_dia, aes(x = reorder(day_of_week, -media_duracion), y = media_duracion, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Duración Media de los Viajes por Día de la Semana y Tipo de Usuario",
       x = "Día de la Semana",
       y = "Duración Media del Viaje (segundos)") +
  theme_minimal() +
  scale_fill_manual(values = c("orange", "blue"), name = "Tipo de Usuario", labels = c("Casual", "Miembro"))

ggsave("Duración Media de los Viajes por Día 202407.png", width = 10, height = 6, dpi = 300)

############################################################################



