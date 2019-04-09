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

congress.data <- read.csv('corpusData/congress_merged.csv', check.names = FALSE)

congress.full_lm_rt <- lm(retweet_count ~ . -text, data = congress.data)

# limit to only variables with sngificance
res <- coef(summary(congress.full_lm_rt))
Wtd <- row.names(res[res[,"Pr(>|t|)"] < 0.05,]) 
Wtd.cleaned <- Wtd[!grepl("\\(.*\\)",Wtd)] 

congress.sig_rt <- lm(as.formula(paste("retweet_count ~",paste(Wtd.cleaned, collapse="+"))), data = congress.data)

congress_rt.AIC <- stepAIC(congress.sig_rt, trace=FALSE)

save(clinton_fav.AIC, file = "models/clinton_fav.rda")
save(clinton_rt.AIC, file = "models/clinton_rt.rda")
save(trump_fav.AIC, file = "models/trump_fav.rda")
save(trump_rt.AIC, file = "models/trump_rt.rda")
save(congress_rt.AIC, file = "models/congress_rt.rda")