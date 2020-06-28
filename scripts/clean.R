require(stringr)
require(dplyr)
require(tm)

x <- read.csv('~/Desktop/syo-data-mining/datasets/DATASET_26MAY-26JUN.csv')
clean <- clean_tweets(x)
write.csv(clean, '~/Desktop/syo-data-mining/datasets/CLEAN_DATASET_26MAY-26JUN.csv')

clean_tweets <- function(x) {
  x$text <- enc2native(x$text)
  
  x$text <- gsub("\\p{So}|\\p{Cn}", "", x$text, perl = TRUE) # Remove emojis (falla en un tipo)
  x$text <- gsub('#\\S+', '', x$text) ## Remove Hashtags
  x$text <- gsub('@\\S+', '', x$text) ## Remove Mentions
  x$text <- gsub("<(.*)>", "", x$text) # Remove pesky Unicodes like <U+A>
  x$text <- gsub("  ", " ", x$text) # Replace double space with single space
  x$text <- gsub("https(.*)*$", "", x$text) # remove tweet URL
  x$text <- gsub("&amp;", "&", x$text) # fix ampersand &
  x$text <- gsub('[[:cntrl:]]', '', x$text) ## Remove Controls and special characters
  x$text <- gsub("\\d", '', x$text) ## Remove Controls and special characters
  x$text <- gsub('[[:punct:]]', '', x$text) ## Remove Punctuations
  x$text <- tolower(x$text) # to lower case
  
  clean <- x %>% select("screen_name", "created_at", "text", "is_retweet")
  
  tw_corpus <- Corpus(VectorSource(clean$text))
  tw_clean <- tm_map(tw_corpus, removeWords, stopwords(kind = "es"))
  tw_clean <- tm_map(tw_clean, removeWords, c("coronavirus", "covid", "covid19", "argentina"))
  tw_clean <- tm_map(tw_clean, removeWords, c("vía", "día", "días", "acá", "hoy", "dijo", "buenos aires", "mil", "dos", "dio")) # como
  
  clean$text <- sapply(tw_clean, identity) # corpus to vector, then replace the tweets' text
  clean$text <- gsub(' +',' ',clean$text) ## Remove extra whitespaces
  clean$text <- gsub("^[[:space:]]*","",clean$text) ## Remove leading whitespaces
  clean$text <- gsub("[[:space:]]*$","",clean$text) ## Remove trailing whitespaces
  
  return(clean)
  rm(x, tw_corpus, tw_clean, clean)
}

