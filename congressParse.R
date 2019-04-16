library(tidyverse)
library(lubridate)
library(dplyr)
library(tokenizers)
library(tm)
library(textmineR)
library(quanteda)


#load in model

load("~/team-project-team-2/models/congress_rt.rda")

# Replace this code with whatever input we get from the shiny app as appropriate.
# For Congress tweets, the variables coming in are the tweet text, date,
# whether media was present, the race of the tweeter, the gender of the tweeter, and their political score.

congress.tweet <- data.frame(
  tweet_text = "Vote for Hillary for President and not Trump",
  date = as.Date("2016-11-01"),
  var_media = 1, var_gender = 0, var_race = 1,
  dw_score = 2.5
  )

http_reg <- "http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;"
# remove punctuation (except # (for hashtags) and @ (for usernames))
punct_reg <- "[^a-zA-Z@#\\s]"

congress.cleanTweet <- congress.tweet %>%
  mutate(tweet_text = str_replace_all(tweet_text, http_reg, "")) %>%
  mutate(tweet_text = str_replace_all(tweet_text, punct_reg, "")) %>%
  mutate(days_to_election = as.numeric(as.Date("2016-11-02", format = "%Y-%m-%d") - date)) %>%
  dplyr::select(tweet_text, days_to_election, var_media, var_gender, var_race, dw_score)

congress.corpus <- VCorpus(VectorSource(congress.cleanTweet$tweet_text)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords('english'))

congress.matrix <- DocumentTermMatrix(congress.corpus)
congress.terms <- data.frame(as.matrix(congress.matrix), check.names = FALSE)
congress.merged <- cbind(congress.cleanTweet, congress.terms)

# for predicting, we need to have ALL variables in the model
# these steps create a vector of zeroes for each variable in the model not in our data
congress.varList <- variable.names(congress_rt.AIC)[-1]
# get rid of the inverted apostrophes so the variable names match
congress.varList <-sapply(congress.varList, function(x) gsub("`", "", x))
  
congress.diffList <- congress.varList[!(congress.varList %in% variable.names(congress.merged))]
congress.zeroMatrix <- data.frame(matrix(0, 1, length(congress.diffList)))
colnames(congress.zeroMatrix) = congress.diffList

congress.merged <- cbind(congress.merged, congress.zeroMatrix)


# output
rt <- max(
  round(predict(congress_rt.AIC, newdata=congress.merged)), 0
)