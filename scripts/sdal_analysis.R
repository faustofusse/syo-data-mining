library(dplyr)

# Cargar sdal Dictionary
sdal <- read.csv("~/Desktop/syo-data-mining/datasets/sdal/meanAndStdev.csv", header = FALSE, sep = ";", col.names = 
  c("palabra", "media_agrado", "media_activacion", "media_imaginabilidad", "stdev_agrado", "stdev_activacion", "stdev_imaginabilidad"))

# Sacar caracteres despues del "_"
sdal$palabra <- sapply(strsplit(sdal$palabra, split = "_", fixed=TRUE), function(x) (x[1]))

# Cargar tweets adaptados
tweets <- read.csv("~/Desktop/syo-data-mining/datasets/ADAPTED_DATASET_26MAY-26JUN.csv") %>%
  select("screen_name", "created_at", "text", "is_retweet", "text_inf", "text_inf_sing")

# Pasar los "text" a vectores
tweets$text <- sapply(strsplit(tweets$text, split = ";", fixed=TRUE), function(x) (x))
tweets$text_inf <- sapply(strsplit(tweets$text_inf, split = ";", fixed=TRUE), function(x) (x))
tweets$text_inf_sing <- sapply(strsplit(tweets$text_inf_sing, split = ";", fixed=TRUE), function(x) (x))

# Match los "text" con las palabras del sdal
tweets$agrado <- sapply(tweets$text_inf_sing, function(x) na.omit(match(x, sdal$palabra)))
# Reemplazo los indices con el nivel de agrado de cada palabra
tweets$agrado <- sapply(tweets$agrado, function(x) sdal$media_agrado[x])
# Promedio entre todas las palabras
tweets$agrado <- sapply(tweets$agrado, function(x) (mean(x)))

# Repito el proceso para activacion e imaginabilidad
tweets$activacion <- sapply(tweets$text_inf_sing, function(x) na.omit(match(x, sdal$palabra)))
tweets$activacion <- sapply(tweets$activacion, function(x) sdal$media_activacion[x])
tweets$activacion <- sapply(tweets$activacion, function(x) (mean(x)))
tweets$imaginabilidad <- sapply(tweets$text_inf_sing, function(x) na.omit(match(x, sdal$palabra)))
tweets$imaginabilidad <- sapply(tweets$imaginabilidad, function(x) sdal$media_imaginabilidad[x])
tweets$imaginabilidad <- sapply(tweets$imaginabilidad, function(x) (mean(x)))

# Hago nuevo dataframe con las columnas importantes 
t <- tweets %>% select("created_at", "agrado", "activacion", "imaginabilidad")
# Le saco las horas, minutos y segundos a los "created_at"
t$created_at <- sapply(t$created_at, function(x) substr(x,1,nchar(x)-9))
# Agrupo las filas por fecha
t <- t %>% group_by(created_at) %>% summarise_all(funs(list(na.omit(.))))
# Hago nuevas columnas con los promedios de cada dia
t$mean_agrado <- sapply(t$agrado, function(x) (mean(x)))
t$mean_activacion <- sapply(t$activacion, function(x) (mean(x)))
t$mean_imaginabilidad <- sapply(t$imaginabilidad, function(x) (mean(x)))

# Hago el grafico (ir cambiando los parametros)
plot(t$mean_activacion,
     pch = 19,         # Solid circle
     cex = 1.5,        # Make 150% size
     col = "#43a047",  # Red
     main = "Activacion en el ultimo mes",
     xlab = "Dias (desde el 25/05 al 25/06)",
     ylab = "Activacion (1=pasivo, 3=activo)",
     type = "b")
