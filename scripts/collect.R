library(rtweet)

# Credentials
api_key             <- "x"
api_secret          <- "x"
access_token        <- "x"
access_token_secret <- "x"
twitter_app         <- "x"

# Authentication
create_token(
  app             = twitter_app,
  consumer_key    = api_key,
  consumer_secret = api_secret,
  access_token    = access_token,
  access_secret   = access_token_secret)

# Default twitter search
covid_tweets <- search_tweets("coronavirus+argentina", include_rts = FALSE, lang = "es", n = 500)

# Get tweets from specific month (fullarchive) 
# NO TERMINADA
get_tweets_by_month <- function(query, tweets_per_day, year, month){
  days <- 31
  if (month == 2) days <- 28
  if (month == 4 || month == 6 || month == 9 || month == 11) days <- 30
  month_str <- month
  if (month < 10) month_str <- paste(0, month, sep = "")
  for (i in 1:days) {
    day_str <- i
    if (i < 10) day_str <- paste(0, i, sep = "")
    date <- paste(year, month_str, day_str, sep = "-")
    # aca ir pidiendo tweets
    # tweets <- filter(tweets, is_retweet == FALSE)
  }
  return(date)
}
get_tweets_by_month("coronavirus+argentina", 100, 2020, 2)

# Get tweets from specific date (fullarchive)
t <- search_fullarchive("coronavirus+argentina", n=100, 
                        fromDate = "2020-05-01", toDate = "2020-05-01", 
                        env_name = "devv")

# 30 day search 
toDate <- format(Sys.time() - 60 * 60 * 24 * 26, "%Y%m%d%H%M")
tw <- search_30day("coronavirus argentina lang:es", env_name = "dev", toDate = toDate)

# Collect from the last month (hace 6 y se corta, hay que ir cambiando el i inicial)
df <- data.frame()
for (i in 1:31){
  toDate <- format(Sys.time() - 60 * 60 * 24 * i, "%Y%m%d%H%M")
  temp <- search_30day("coronavirus argentina lang:es", n = 500, env_name = "dev", toDate = toDate)
  df <- rbind(df, temp)
}

# Write csv
tw <- df %>% select("screen_name", "created_at", "text", "is_retweet") # "retweet_text", "retweet_created_at"
write.csv(tw, "~/Desktop/DATASET.csv")

