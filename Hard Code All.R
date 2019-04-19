list.of.packages <- c("ggplot2", "dplyr", "shiny", "tidyverse", "lubridate", "tokenizers", "tm", "textmineR", "quanteda","shinydashboard","stats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

#update to git/EC2 directory
setwd("~/team-project-team-2-master/models")
load(file = "clinton_rt.rda")
load(file = "clinton_fav.rda")
load(file = "trump_rt.rda")
load(file = "trump_fav.rda")
load(file = "congress_rt.rda")


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Tweet Predictor")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Team 2 - DSCI644", tabName = "Team 2", icon = icon("dashboard"))
  )
)


frow1 <- fluidRow(
  box(
    title = "Tweet", solidHeader = TRUE,
    textInput("text", "Type Tweet:"),
    actionButton("submit", "Submit")),


  box(
    title = "Tweet Contents", solidHeader = TRUE,
    textOutput("content"))
)

frow2 <- fluidRow(
  box(
    width =10, title = "Clinton", solidHeader = TRUE,
    collapsible = TRUE,collapsed = TRUE, background = "light-blue",
    valueBoxOutput("cl.rt.v", width = 5),
    valueBoxOutput("cl.fv.v", width = 5))
)

frow3 <- fluidRow(
  box(
    width =10, title = "Trump", solidHeader = TRUE,
    collapsible = TRUE,collapsed = TRUE, background = "lime",
    valueBoxOutput("td.rt.v", width = 5),
    valueBoxOutput("td.fv.v", width = 5))
)

frow4 <- fluidRow(
  box(
    width =10, title = "Congress", solidHeader = TRUE,
    collapsible = TRUE,collapsed = TRUE, background = "maroon",
    valueBoxOutput("cg.rt.v", width = 5))
)

body <- dashboardBody(frow1, frow2, frow3, frow4)


ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='black')

server <- function(input, output){
 
  #Clinton 
  {text <- "Let's build a wall, and worry about emails"
   date <-  as.Date("2016-11-01")
    tweet <- data.frame(tweet_text = text, date = date)
    http_reg <- "http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;"
    # remove punctuation (except # (for hashtags) and @ (for usernames))
    punct_reg <- "[^a-zA-Z@#\\s]"
    
    clinton.cleanTweet <- tweet %>%
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
    
    # for predicting, we need to have ALL variables in BOTH models
    # these steps create a vector of zeroes for each variable in the models not in our data
    clinton.varList <- unique(c(variable.names(clinton_fav.AIC)[-1], 
                                variable.names(clinton_rt.AIC)[-1]))
    # get rid of the inverted apostrophes so the variable names match
    clinton.varList <- sapply(clinton.varList, function(x) gsub("`", "", x))
    
    clinton.diffList <- clinton.varList[!(clinton.varList %in% variable.names(clinton.merged))]
    clinton.zeroMatrix <- data.frame(matrix(0, 1, length(clinton.diffList)))
    colnames(clinton.zeroMatrix) = clinton.diffList
    
    clinton.merged <- cbind(clinton.merged, clinton.zeroMatrix)
    cl.fav <- predict(clinton_fav.AIC, newdata=clinton.merged) 
    cl.new.fav <- round(cl.fav[[1]],0)
    cl.rt <- predict(clinton_rt.AIC, newdata=clinton.merged)
    cl.new.rt <- round(cl.rt[[1]],0)
  }

  #Trump
  {
    trump.cleanTweet <- tweet %>%
      mutate(tweet_text = str_replace_all(tweet_text, http_reg, "")) %>%
      mutate(tweet_text = str_replace_all(tweet_text, punct_reg, "")) %>%
      mutate(days_to_election = as.numeric(as.Date("2016-11-02", format = "%Y-%m-%d") - date)) %>%
      dplyr::select(tweet_text, days_to_election)
    
    trump.corpus <- VCorpus(VectorSource(trump.cleanTweet$tweet_text)) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeWords, stopwords('english'))
    
    trump.matrix <- DocumentTermMatrix(trump.corpus)
    trump.terms <- data.frame(as.matrix(trump.matrix), check.names = FALSE)
    trump.merged <- cbind(trump.cleanTweet, trump.terms)
    
    # for predicting, we need to have ALL variables in BOTH models
    # these steps create a vector of zeroes for each variable in the models not in our data
    trump.varList <- unique(
      c(
        variable.names(trump_fav.AIC)[-1], variable.names(trump_rt.AIC)[-1]
      )
    )
    # get rid of the inverted apostrophes so the variable names match
    trump.varList <-sapply(trump.varList, function(x) gsub("`", "", x))
    
    trump.diffList <- trump.varList[!(trump.varList %in% variable.names(trump.merged))]
    trump.zeroMatrix <- data.frame(matrix(0, 1, length(trump.diffList)))
    colnames(trump.zeroMatrix) = trump.diffList
    
    trump.merged <- cbind(trump.merged, trump.zeroMatrix)
    
    
    # outputs
    trump.merged <- cbind(trump.merged, trump.zeroMatrix)
    td.fav <- predict(trump_fav.AIC, newdata=trump.merged) 
    td.new.fav <- round(td.fav[[1]],0)
    td.rt <- predict(trump_rt.AIC, newdata=trump.merged)
    td.new.rt <- round(td.rt[[1]],0)
  }
  
  #Congress
  {
  # Replace this code with whatever input we get from the shiny app as appropriate.
  # For Congress tweets, the variables coming in are the tweet text, date,
  # whether media was present, the race of the tweeter, the gender of the tweeter, and their political score.
  
  congress.tweet <- data.frame(
    tweet_text = text,
    date = as.Date("2016-11-01"),
    var_media = 1, var_gender = 0, var_race = 1,
    dw_score = 2.5
  )
  
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
  cg.rt <- max(round(predict(congress_rt.AIC, newdata=congress.merged)), 0)
  cg.new.rt <- round(cg.rt[[1]],0)
  }
  
  #Clinton Output
  {
  output$cl.rt.v <- renderValueBox({
    valueBox(
      formatC(cl.new.rt,format = "d", big.mark=',')
      ,'Predicted Retweets'
      ,icon = icon("retweet",lib='glyphicon')
      ,color = "teal")
  })
  
  output$cl.fv.v <- renderValueBox({
    
    valueBox(
      formatC(cl.new.fav, format="d", big.mark=',')
      ,'Predicted Favorites'
      ,icon = icon("heart",lib='glyphicon')
      ,color = "fuchsia")
    
  })
  }
  
  #Trump Output
  {
    output$td.rt.v <- renderValueBox({
      valueBox(
        formatC(td.new.rt,format = "d", big.mark=',')
        ,'Predicted Retweets'
        ,icon = icon("retweet",lib='glyphicon')
        ,color = "teal")
    })
    
    output$td.fv.v <- renderValueBox({
      
      valueBox(
        formatC(td.new.fav, format="d", big.mark=',')
        ,'Predicted Favorites'
        ,icon = icon("heart",lib='glyphicon')
        ,color = "fuchsia")
      
    })
  }
  
  #Congress Output
  {
    output$cg.rt.v <- renderValueBox({
      valueBox(
        formatC(td.new.rt,format = "d", big.mark=',')
        ,'Predicted Retweets'
        ,icon = icon("retweet",lib='glyphicon')
        ,color = "teal")
    })
  }
  
  output$content <- renderText(text)
}



shinyApp(ui, server)
