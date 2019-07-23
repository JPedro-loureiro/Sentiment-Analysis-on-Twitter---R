# Sentiment Analysis on Twitter

#install.packages("twitteR")
library(twitteR)

#install.packages("rvest")
library(rvest)

#install.packages("tidyverse")
library(tidyverse)


#API's and Token's keys
API_key = "hhrTn58RfcBoxjpGdvQbLTf6v"
API_secret_key = "r6zON2bTvFv8qlaEDWWaIowRknxRxc97EGENdSBfdXZlFmkmrm"
Access_token = "4928132291-OFnrrwGy5FKHLPePkUsxLA5exJwrbJ7l51azMWV"
Access_token_secret = "q7inW05npl3S70yRihMkJdasF0roZyrlzZW46D1XTSbDd"

#OAuth
setup_twitter_oauth(API_key, API_secret_key, Access_token, Access_token_secret)

#Cleaning tweets
clean_tweets <- function(tweets){
  
  #Getting tweets texts
  tweets = sapply(tweets, function(x) x$text)
  
  #Remove retweet entities
  tweets = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', "", tweets)
  
  #Remove Hashtags
  tweets = gsub('#\\w+', "", tweets)
  
  #Remove links
  tweets = gsub('http\\w+', "", tweets)
  
  #Remove punctuations
  tweets = gsub('[[:punct:]]', "", tweets)
  
  #Remove numbers
  tweets = gsub('[[:digit:]]', "", tweets)
  
  #Remove line break
  tweets = gsub('\n', "", tweets)
  
  #lower case
  tweets = tolower(tweets)
}

#Search Twitters
#install.packages("rlist")
library(rlist)

search_twiter <- function(){
  #This function search for a specific number of tweets per day in a range of dates
  #Return a Tibble whith 3 columns: 1- TweetID; 2- Date; 3- Tweet;
  
  #Search string
  #search_string <- paste(readline(prompt = "A wordkey or a hashtag: "), " -filter:retweets") #An exemple: "ifood -filter:retweets"
  search_string <- "ifood -filter:retweets"
  
  #Number of tweets per day
  #n_tweets <- as.integer(readline(prompt = "The number of tweets per day: "))
  n_tweets <- 10
  
  #Language
  #language <- readline(prompt = "Language: (ex.: pt): ")
  language <- "pt"
  
  #Locale
  #loc <- readline(prompt = "Tweets locale: (ex.: br): ")
  loc <- "br"
  
  #Date range: yyyy-mm-dd  #Da erro quando as datas são iguais
  #ini_date <- as.Date(readline(prompt = "The inicial date (yyyy-mm-dd): "), format = "%Y-%m-%d") #Inicial date
  ini_date <- as.Date("2019-07-20", format = "%Y-%m-%d")
  #final_date <- as.Date(readline(prompt = "The final date (yyyy-mm-dd): "), format = "%Y-%m-%d") #Final date
  final_date <- as.Date("2019-07-22", format = "%Y-%m-%d")
  n_days <- as.integer(final_date - ini_date)
  
  #Searching
  tweets <- c()
  date <- c()
  for(day in 0:n_days-1){
    tweets <- c(tweets, clean_tweets(searchTwitter(searchString = search_string, n = n_tweets,
                                                   lang = language, locale = loc,
                                                   since = as.character(ini_date + day),
                                                   until = as.character(ini_date + day + 1))))
    
    date <- c(date, rep(ini_date + day, n_tweets))
  }
  
  tweets_df <- tibble(tweetID = 1:length(tweets),
                      date = format(as.Date(date, origin = "1970-01-01"), "%d-%m-%Y"),
                      tweets)
  
  return(tweets_df)
}

tweets_df <- search_twiter()
glimpse(tweets_df)
View(tweets_df)

#Tokenization  
library(tidytext)
tweets_tokens <- tweets_df %>%
  unnest_tokens(words, tweets)
View(tweets_tokens)

#Wordcloud
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

wc <- function(tweets_tokens){
  plot <- tweets_tokens %>%
    group_by(words) %>%
    summarise(freq = n()) %>%
    filter(words != "ifood")
  
  wordcloud(words = plot$words, freq = plot$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}

wc(tweets_tokens)

#Stopwords
#install.packages("stopwords")
library(stopwords)

stopwords <- stopwords(language = "pt", source = "stopwords-iso")
stopwords <- tibble(stopwords)
View(stopwords)

tweets_tokens <- tweets_tokens %>%
  anti_join(stopwords, by = c("words" = "stopwords"))

#LexiconPT
#install.packages("lexiconPT")
library(lexiconPT)

lex_pt <- oplexicon_v3.0
View(lex_pt)

tweets_tokens <- tweets_tokens %>%
  inner_join(lex_pt, by = c("words" = "term")) %>%
  select(tweetID, date, words, polarity)

View(tweets_tokens)

#Most negative tweets
top_10_neg <- tweets_tokens %>%
  group_by(tweetID) %>%
  summarise(polarity = sum(polarity)) %>%
  arrange(polarity) %>%
  head(10)

  #Ordering
  top_10_neg$tweetID <- factor(top_10_neg$tweetID,
                               levels = unique(top_10_neg$tweetID)[order(top_10_neg$polarity,
                                                                           decreasing = TRUE)])
  #PLOT
  ggplot(top_10_neg, aes(tweetID, polarity)) +
    geom_col(fill = "lightcoral") +
    coord_flip() +
    xlab("Tweet ID") + ylab("Polarity") + ggtitle("Top 10 negative Polarity x Tweet ID")

#Most positive tweets
top_10_pos <- tweets_tokens %>%
  group_by(tweetID) %>%
  summarise(polarity = sum(polarity)) %>%
  arrange(desc(polarity)) %>%
  head(10)

  #Ordering
  top_10_pos$tweetID <- factor(top_10_pos$tweetID,
                               levels = unique(top_10_pos$tweetID[order(top_10_pos$polarity,
                                                                        decreasing = FALSE)]))
  #Plot
  ggplot(top_10_pos, aes(tweetID, polarity)) +
    geom_col(fill = "cornflowerblue") +
    coord_flip() +
    xlab("Tweet ID") + ylab("Polarity") + ggtitle("Top 10 positive Polarity x Tweet ID")

#Sentiment Analysis Overview
str(tweets_tokens)

tweets_tokens %>%
  group_by(tweetID) %>%
  summarise(polarity = sum(polarity)) %>%
  mutate(class = ifelse(polarity > 0, "positive",
                       ifelse(polarity < 0, "negative", "neutral"))) %>%
  count(class) %>%
  ggplot(aes(factor(class), n, fill = class)) +
  geom_col() +
  xlab("Class") + ylab("Number of tweets") + ggtitle("Class x Number of tweets")

#Time series
tweets_tokens %>%
  group_by(date) %>%
  summarise(polarity = sum(polarity)) %>%
  ggplot(aes(date, polarity, group = 1)) +
  geom_line() +
  ggtitle("Polarity x Date")


