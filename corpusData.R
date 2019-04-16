library(tidyverse)
#library(tokenizers)
library(tm)
#library(filehash)
#library(textmineR)
#library(quanteda)

clinton.clean <- read.csv('cleanData/clinton_clean.csv')
trump.clean <- read.csv('cleanData/trump_clean.csv')
congress.clean <- read.csv('cleanData/congress_clean.csv')

# convert the text to corpus for creating the Document Term Matrix

clinton.corpus <- VCorpus(VectorSource(clinton.clean$text)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords('english'))

trump.corpus <- VCorpus(VectorSource(trump.clean$text)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords('english'))

# be warned, this takes a while to run
congress.corpus <- VCorpus(VectorSource(congress.clean$text)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords('english'))


# create the DTMs and only keep terms that appear in at least 1% of tweets

clinton.matrix <- DocumentTermMatrix(clinton.corpus) %>%
  removeSparseTerms(0.9975)
trump.matrix <- DocumentTermMatrix(trump.corpus) %>%
  removeSparseTerms(0.99)
congress.matrix <- DocumentTermMatrix(congress.corpus) %>%
  removeSparseTerms(0.99)


clinton.terms <- data.frame(as.matrix(clinton.matrix), check.names = FALSE)
trump.terms <- data.frame(as.matrix(trump.matrix), check.names = FALSE)
congress.terms <- data.frame(as.matrix(congress.matrix), check.names = FALSE)


clinton.merged <- merge(clinton.clean, clinton.terms, by.x = "doc_id", by.y = "row.names") %>%
  mutate(tweet_text = text.x) %>%
  dplyr::select(-one_of("doc_id", "text.x"))
trump.merged <- merge(trump.clean, trump.terms, by.x = "doc_id", by.y = "row.names") %>%
  mutate(tweet_text = text) %>%
  dplyr::select(-one_of("doc_id", "text"))
congress.merged <- merge(congress.clean, congress.terms, by.x = "doc_id", by.y = "row.names") %>%
  mutate(tweet_text = text) %>%
  dplyr::select(-one_of("doc_id", "text"))



write.csv(clinton.merged, file = "corpusData/clinton_merged.csv", row.names=FALSE)
write.csv(trump.merged, file = "corpusData/trump_merged.csv", row.names=FALSE)
write.csv(congress.merged, file = "corpusData/congress_merged.csv", row.names=FALSE)