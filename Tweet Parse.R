library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(MASS)
library(tokenizers)
library(tm)
library(sparsesvd)
library(textmineR)
library(quanteda)
library(udpipe)

tweet <- data.frame(tweet = c("Vote for Hilary for President and not Trump"))

http_reg <- "http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;"
# remove punctuation (except # (for hashtags) and @ (for usernames))
punct_reg <- "[^a-zA-Z@#\\s]"

tweet.clean <- tweet %>%
  mutate(text = str_replace_all(tweet, http_reg, "")) %>%
  mutate(text = str_replace_all(tweet, punct_reg, "")) %>%
  mutate(doc_id = row_number()) 

tweet.clean$tweet <- NULL

tweet.corpus <- tweet.clean %>%
  DataframeSource %>%
  Corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords('english'))


tweet.matrix <- DocumentTermMatrix(tweet.corpus)
tweet.terms <- data.frame(as.matrix(tweet.matrix), check.names = FALSE)
tweet.merged <- merge(tweet.clean, tweet.terms, by.x = "doc_id", by.y = "row.names")

clinton.data <- read.csv('corpusData/clinton_merged.csv', check.names = FALSE) %>%
  mutate(timestamp = as.Date(timestamp, format = "%Y-%m-%d")) %>%
  mutate(days_to_election = as.numeric(as.Date("2016-11-02", format = "%Y-%m-%d") - timestamp))

clinton.full_lm_fav <- lm(favorite_count ~ . - (retweet_count + text + timestamp), data = clinton.data)


newtweet <- tweet.terms
predict(clinton.full_lm_fav, newtweet, interval="prediction", level=0.95) 
