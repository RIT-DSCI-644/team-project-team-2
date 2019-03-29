library(MASS)
library(dplyr)
library(lubridate)

clinton.data <- read.csv('corpusData/clinton_merged.csv', check.names = FALSE) %>%
  mutate(timestamp = as.Date(timestamp, format = "%Y-%m-%d")) %>%
  mutate(days_to_election = as.numeric(as.Date("2016-11-02", format = "%Y-%m-%d") - timestamp))

clinton.full_lm_fav <- lm(favorite_count ~ . - (retweet_count + text + timestamp), data = clinton.data)

# limit to only variables with sngificance
res <- coef(summary(clinton.full_lm_fav))
Wtd <- row.names(res[res[,"Pr(>|t|)"] < 0.1,]) 
Wtd.cleaned <- Wtd[!grepl("\\(.*\\)",Wtd)] 

clinton.sig_fav <- lm(as.formula(paste("favorite_count ~",paste(Wtd.cleaned, collapse="+"))), data = clinton.data)

clinton_fav.AIC <- stepAIC(clinton.sig_fav, trace=FALSE)

clinton.full_lm_rt <- lm(retweet_count ~ . - (favorite_count + text + timestamp), data = clinton.data)

# limit to only variables with sngificance
res <- coef(summary(clinton.full_lm_rt))
Wtd <- row.names(res[res[,"Pr(>|t|)"] < 0.1,]) 
Wtd.cleaned <- Wtd[!grepl("\\(.*\\)",Wtd)] 

clinton.sig_rt <- lm(as.formula(paste("retweet_count ~",paste(Wtd.cleaned, collapse="+"))), data = clinton.data)

clinton_rt.AIC <- stepAIC(clinton.sig_rt, trace=FALSE)

trump.data <- read.csv('corpusData/trump_merged.csv', check.names = FALSE) %>%
  mutate(timestamp = as.Date(timestamp, format = "%Y-%m-%d")) %>%
  mutate(days_to_election = as.numeric(as.Date("2016-11-02", format = "%Y-%m-%d") - timestamp))

trump.full_lm_fav <- lm(favorite_count ~ . - (retweet_count + text + timestamp), data = trump.data)

# limit to only variables with sngificance
res <- coef(summary(trump.full_lm_fav))
Wtd <- row.names(res[res[,"Pr(>|t|)"] < 0.1,]) 
Wtd.cleaned <- Wtd[!grepl("\\(.*\\)",Wtd)] 

trump.sig_fav <- lm(as.formula(paste("favorite_count ~",paste(Wtd.cleaned, collapse="+"))), data = trump.data)

trump_fav.AIC <- stepAIC(trump.sig_fav, trace=FALSE)

trump.full_lm_rt <- lm(retweet_count ~ . - (favorite_count + text + timestamp), data = trump.data)

# limit to only variables with sngificance
res <- coef(summary(trump.full_lm_rt))
Wtd <- row.names(res[res[,"Pr(>|t|)"] < 0.1,]) 
Wtd.cleaned <- Wtd[!grepl("\\(.*\\)",Wtd)] 

trump.sig_rt <- lm(as.formula(paste("retweet_count ~",paste(Wtd.cleaned, collapse="+"))), data = trump.data)

trump_rt.AIC <- stepAIC(trump.sig_rt, trace=FALSE)