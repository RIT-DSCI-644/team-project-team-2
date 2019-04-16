library(tidyverse)
library(lubridate)
library(dplyr)
library(tokenizers)
library(tm)
library(textmineR)
library(quanteda)


#load in models

load("~/team-project-team-2/models/clinton_fav.rda")
load("~/team-project-team-2/models/clinton_rt.rda")

# Replace this code with whatever input we get from the shiny app as appropriate.
# For Clinton tweets, the only variables coming in are the text and the data.

clinton.tweet <- data.frame(tweet_text = "Vote for Hillary for President and not Trump", date = as.Date("2016-11-01"))

http_reg <- "http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;"
# remove punctuation (except # (for hashtags) and @ (for usernames))
punct_reg <- "[^a-zA-Z@#\\s]"

clinton.cleanTweet <- clinton.tweet %>%
  mutate(tweet_text = str_replace_all(tweet_text, http_reg, "")) %>%
  mutate(tweet_text = str_replace_all(tweet_text, punct_reg, "")) %>%
  mutate(days_to_election = as.numeric(as.Date("2016-11-02", format = "%Y-%m-%d") - date)) %>%
  dplyr::select(tweet_text, days_to_election)

clinton.corpus <- VCorpus(VectorSource(clinton.cleanTweet$tweet_text)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords('english'))

clinton.matrix <- DocumentTermMatrix(clinton.corpus)
clinton.terms <- data.frame(as.matrix(clinton.matrix), check.names = FALSE)
clinton.merged <- cbind(clinton.cleanTweet, clinton.terms)

# for predicting, we need to have ALL variables in the model
# these steps create a vector of zeroes for each variable in the model not in our data
clinton.varList <- variable.names(clinton_fav.AIC)[-1]
clinton.varList <-sapply(clinton.varList, function(x) gsub("`", "", x))
  
clinton.diffList <- clinton.varList[!(clinton.varList %in% variable.names(clinton.merged))]
clinton.zeroMatrix <- data.frame(matrix(0, 1, length(clinton.diffList)))
colnames(clinton.zeroMatrix) = clinton.diffList

clinton.merged <- cbind(clinton.merged, clinton.zeroMatrix)


# out outputs
fav <- predict(clinton_fav.AIC, newdata=clinton.merged) 
rt <- predict(clinton_rt.AIC, newdata=clinton.merged) 