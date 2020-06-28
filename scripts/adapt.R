require(tidyr)
require(dplyr)

# ---- CARGAR Y LIMPIAR TWEETS -------------------------------------------------

# Cargar tweets
tweets <- read.csv("~/Desktop/syo-data-mining/datasets/CLEAN_DATASET_26MAY-26JUN.csv")
tweets <- tweets %>% select("screen_name", "created_at", "text", "is_retweet")

# Pasar "text" a vector
tweets$text <- sapply(strsplit(tweets$text, split = " ", fixed=TRUE), function(x) (x))

# ---- CONVERTIR VERBOS A INFINITIVO -------------------------------------------

# Cargar Verbos
verbos <- read.csv("~/Desktop/POSTA_R/datasets/verbs/verbos_clean.csv")
verbos <- verbos %>% select("infinitive", "forms")

# Pasar "forms" a vector
verbos$forms <- sapply(strsplit(verbos$forms, split = ";", fixed=TRUE), function(x) (x))

# Pasar verbo a infinitivo
verbo_a_infinitivo <- function(x){
  index <- buscar_verbo(x, verbos$forms)
  if (is.na(index)) return(x)
  else return(verbos$infinitive[index])
}

# Buscar verbo 
buscar_verbo <- function(verbo, forms){
  for (i in 1:length(forms)){
    f <- (unlist(forms[i]))
    for (j in 1:length(f)) {
      if(f[j] == verbo) return(i)
    }
  }
  return(NA)
}

# Pasar todos los verbos a infinitivo (tarda un rato)
counter <- 1
total <- length(tweets$text)
tweets$text_inf <- sapply(tweets$text, function(x){
  p <- paste("(", round(counter * 100 / total, digits = 2), "%)", sep = "")
  info <- paste(counter, total, sep = "/")
  print(paste(info, p, sep = " "))
  counter <<- counter + 1
  return(sapply(x, function(x) verbo_a_infinitivo(x), USE.NAMES = FALSE))
})

# ---- CONVERTIR TODO A SINGULAR -----------------------------------------------

# Cargar Spal Dictionary
spal <- read.csv("~/Desktop/S_ANALYSIS/SpanishDAL-v1.2/meanAndStdev.csv", header = FALSE, sep = ";", col.names = 
                   c("palabra", "media_agrado", "media_activacion", "media_imaginabilidad", "stdev_agrado", "stdev_activacion", "stdev_imaginabilidad"))

# Sacar caracteres despues del "_"
spal$palabra <- sapply(strsplit(spal$palabra, split = "_", fixed=TRUE), function(x) (x[1]))

# Sacarle el tilde a una palabra
sacar_tilde <- function(x) gsub("'", "", iconv(x, to = "ASCII//TRANSLIT"))

# Buscar palabra y si existe, devolver la que esta en el vector
get_palabra <- function(x, palabras) {
  for (p in palabras)
    if (sacar_tilde(p) == x)
      return(p)
  return(NULL)
}

# Pasar una palabra a singular
pasar_a_singular <- function(x, palabras){
  palabras <- unlist(palabras)
  if (endsWith(x, "ces")){
    new <- paste(substr(x,1,nchar(x)-3), "z", sep = "")
    q <- get_palabra(new, palabras)
    if(!is.null(q)) return(q)
  }
  if (endsWith(x, "es")){
    new <- substr(x,1,nchar(x)-2)
    q <- get_palabra(new, palabras)
    if(!is.null(q)) return(q)
  }
  if (endsWith(x, "s")){
    new <- substr(x,1,nchar(x)-1)
    q <- get_palabra(new, palabras)
    if(!is.null(q)) return(q)
  }
  return(x)
}

# Pasar todas las palabras a singular
counter <- 1
total <- length(tweets$text_inf)
tweets$text_inf_sing <- sapply(tweets$text_inf, function(x){
  p <- paste("(", round(counter * 100 / total, digits = 2), "%)", sep = "")
  info <- paste(counter, total, sep = "/")
  print(paste(info, p, sep = " "))
  counter <<- counter + 1
  return(sapply(x, function(x) pasar_a_singular(x, spal$palabra), USE.NAMES = FALSE))
})


# ---- GUARDAR CAMBIOS----------------------------------------------------------

# Guardar
tweets$text_inf_sing <- vapply(tweets$text_inf_sing, paste, collapse = ";", character(1L))
tweets$text_inf <- vapply(tweets$text_inf, paste, collapse = ";", character(1L))
tweets$text <- vapply(tweets$text, paste, collapse = ";", character(1L))
write.csv(tweets, "~/Desktop/syo-data-mining/datasets/ADAPTED_DATASET_26MAY-26JUN.csv")
