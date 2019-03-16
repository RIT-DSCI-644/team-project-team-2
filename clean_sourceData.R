library(tidyverse)
library(lubridate)

# load in csv files
clinton.raw <- read.csv('sourceData/clinton_raw.csv')
trump.raw <- read.csv('sourceData/trump_raw.csv')
congress.raw <- read.csv('sourceData/congress_raw.csv')

# do some cleaning up of the datasets

# these are to clean up the tweet text
# remove links
http_reg <- "http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;"
# remove numbers
num_reg <- "[0-9]"
# remove punctuation (except # (for hashtags) and @ (for usernames))
punct_reg <- "[^a-zA-Z@#\\s]"


clinton.clean <- clinton.raw %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, http_reg, "")) %>%
  mutate(text = str_replace_all(text, num_reg, " ")) %>%
  mutate(text = str_replace_all(text, punct_reg, "")) %>%
  mutate(timestamp = mdy_hm(created_at)) %>%
  mutate(doc_id = row_number()) %>%
  select(doc_id, timestamp, favorite_count, retweet_count, text)

trump.clean <- trump.raw %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, http_reg, "")) %>%
  mutate(text = str_replace_all(text, num_reg, " ")) %>%
  mutate(text = str_replace_all(text, punct_reg, "")) %>%
  mutate(timestamp = mdy_hm(created_at)) %>%
  mutate(doc_id = row_number()) %>%
  select(doc_id, timestamp, favorite_count, retweet_count, text)

congress.clean <- congress.raw %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, http_reg, "")) %>%
  mutate(text = str_replace_all(text, num_reg, " ")) %>%
  mutate(text = str_replace_all(text, punct_reg, "")) %>%
  mutate(media = -(media + 0.5), gender = gender + 0.5, race = race + 0.5) %>%
  mutate(doc_id = row_number()) %>%
  select(doc_id, text, retweet_count, media, gender, dw_score, race, age, followers)

write.csv(clinton.clean, file = "cleanData/clinton_clean.csv")
write.csv(trump.clean, file = "cleanData/trump_clean.csv")
write.csv(congress.clean, file = "cleanData/congress_clean.csv")

