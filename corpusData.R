library(tidyverse)
library(lubridate)
library(tokenizers)
library(tm)
library(sparsesvd)
library(textmineR)
library(quanteda)

clinton.clean <- read.csv('cleanData/clinton_clean.csv')
trump.clean <- read.csv('cleanData/trump_clean.csv')
congress.clean <- read.csv('cleanData/congress_clean.csv')

# convert the text to corpus for creating the Document Term Matrix

clinton.corpus <- clinton.clean %>%
  DataframeSource %>%
  Corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords('english'))

trump.corpus <- trump.clean %>%
  DataframeSource %>%
  Corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords('english'))

congress.corpus <- congress.clean %>%
  DataframeSource %>%
  Corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords('english'))

# create the DTMs

clinton.matrix <- DocumentTermMatrix(clinton.corpus)
trump.matrix <- DocumentTermMatrix(trump.corpus)
congress.matrix <- DocumentTermMatrix(congress.corpus)

# keep only hashtags and usernames

clinton.hashUser <- data.frame(as.matrix(clinton.matrix[, grepl("@|#" , clinton.matrix$dimnames$Terms )]),
                               check.names = FALSE)
trump.hashUser <- data.frame(as.matrix(trump.matrix[, grepl("@|#" , trump.matrix$dimnames$Terms )]),
                               check.names = FALSE)
#congress.hashUser <- data.frame(as.matrix(congress.matrix[, grepl("@" , congress.matrix$dimnames$Terms )]),
#                             check.names = FALSE)

clinton.merged <- merge(clinton.clean, clinton.hashUser, by.x = "doc_id", by.y = "row.names")
trump.merged <- merge(trump.clean, trump.hashUser, by.x = "doc_id", by.y = "row.names")