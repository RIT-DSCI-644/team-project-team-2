list.of.packages <- c("ggplot2", "dplyr", "shiny", "tidyverse", "lubridate", "tokenizers", "tm", "textmineR", "quanteda","shinydashboard","stats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

#update to git/EC2 directory
setwd("~/team-project-team-2/models")
load(file = "clinton_rt.rda")
load(file = "clinton_fav.rda")
load(file = "trump_rt.rda")
load(file = "trump_fav.rda")
load(file = "congress_rt.rda")

setwd("~/team-project-team-2-master/App")
outputDir <- "responses"

# Define the fields we want to save from the form
fields <- c("text", "date", "var_media","var_gender","var_race","dw_score")

saveData <- function(input) {
  # put variables in a data frame
  data <- data.frame(matrix(nrow=1,ncol=0))
  for (x in fields) {
    var <- input[[x]]
    if (length(var) > 1 ) {
      # handles lists from checkboxGroup and multiple Select
      data[[x]] <- list(var)
    } else {
      # all other data types
      data[[x]] <- var
    }
  }
  data$submit_time <- date()
  
  # Create a unique file name
  fileName <- sprintf(
    "data.rds", 
    as.integer(Sys.time()), 
    digest::digest(data)
  )
  
  # Write the file to the local system
  saveRDS(
    object = data,
    file = file.path(outputDir, fileName)
  )
}

loadData <- function() {
  # read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  
  if (length(files) == 0) {
    # create empty data frame with correct columns
    field_list <- c(fields, "submit_time")
    data <- data.frame(matrix(ncol = length(field_list), nrow = 0))
    names(data) <- field_list
  } else {
    data <- lapply(files, function(x) readRDS(x)) 
    
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
  }
  
  data
}

clintonRT <- function(input){
  tweet <- loadData()
  tweet <- tweet[c(1,2)]
  http_reg <- "http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;"
  # remove punctuation (except # (for hashtags) and @ (for usernames))
  punct_reg <- "[^a-zA-Z@#\\s]"
  
  clinton.cleanTweet <- tweet %>%
    mutate(tweet_text = str_replace_all(text, http_reg, "")) %>%
    mutate(tweet_text = str_replace_all(text, punct_reg, "")) %>%
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
  cl.rt <- predict(clinton_rt.AIC, newdata=clinton.merged)
  cl.new.rt <- round(cl.rt[[1]],0)
  cl.new.rt
}

clintonFav <- function(input){
  tweet <- loadData()
  tweet <- tweet[c(1,2)]
  http_reg <- "http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;"
  # remove punctuation (except # (for hashtags) and @ (for usernames))
  punct_reg <- "[^a-zA-Z@#\\s]"
  
  clinton.cleanTweet <- tweet %>%
    mutate(tweet_text = str_replace_all(text, http_reg, "")) %>%
    mutate(tweet_text = str_replace_all(text, punct_reg, "")) %>%
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
  cl.new.fav
}

trumpRT <- function(input){
  tweet <- loadData()
  tweet <- tweet[c(1,2)]
  http_reg <- "http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;"
  # remove punctuation (except # (for hashtags) and @ (for usernames))
  punct_reg <- "[^a-zA-Z@#\\s]"
  
  trump.cleanTweet <- tweet %>%
    mutate(tweet_text = str_replace_all(text, http_reg, "")) %>%
    mutate(tweet_text = str_replace_all(text, punct_reg, "")) %>%
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
  td.rt <- predict(trump_rt.AIC, newdata=trump.merged)
  td.new.rt <- round(td.rt[[1]],0)
  td.new.rt
}


trumpFAV <- function(input){
  tweet <- loadData()
  tweet <- tweet[c(1,2)]
  http_reg <- "http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;"
  # remove punctuation (except # (for hashtags) and @ (for usernames))
  punct_reg <- "[^a-zA-Z@#\\s]"
    
    trump.cleanTweet <- tweet %>%
      mutate(tweet_text = str_replace_all(text, http_reg, "")) %>%
      mutate(tweet_text = str_replace_all(text, punct_reg, "")) %>%
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
}

congressRT <- function(input){
    congress.tweet <- loadData()
    congress.tweet <- congress.tweet[c(1:6)]
    http_reg <- "http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;"
    # remove punctuation (except # (for hashtags) and @ (for usernames))
    punct_reg <- "[^a-zA-Z@#\\s]"
    congress.cleanTweet <- congress.tweet %>%
      mutate(tweet_text = str_replace_all(text, http_reg, "")) %>%
      mutate(tweet_text = str_replace_all(text, punct_reg, "")) %>%
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
    cg.rt <- max(round(predict(congress_rt.AIC, newdata=congress.merged)))
    cg.rt.v <- abs(round(cg.rt[[1]],0))
    cg.rt.v
}

deleteData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  
  lapply(files, file.remove)
}

resetForm <- function(session) {
  # reset values
  updateTextInput(session, "text", value = "")
  updateSliderInput(session, "date", value = as.Date("2015-11-07"))
  updateSliderInput(session, "var_media", value = 0)
  updateSliderInput(session, "var_gender", value = 0)
  updateSliderInput(session, "var_race", value = 0)
  updateSliderInput(session, "dw_score", value = 0)
}


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Tweet Predictor")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Team 2", tabName = "Team 2", icon = icon("dashboard")),
      title = "Tweet", solidHeader = TRUE,
      textInput("text", "Type Tweet:",width = 700,placeholder = "Tweet"),
      sliderInput("date",
                  "Dates:",
                  min = as.Date("2015-11-07","%Y-%m-%d"),
                  max = as.Date("2016-11-07","%Y-%m-%d"),
                  value=as.Date("2015-11-07"),
                  timeFormat="%Y-%m-%d"),
      sliderInput("var_media", "Does tweet contain media? (-1 = Yes, 0 = No)",
                  min = -1, max = 0, value = 0, step = 1, ticks = TRUE),
      sliderInput("var_gender", "What is the gender of the tweeter? (0 = Male, 1 = Female)",
                  min = 0, max = 1, value = 0, step = 1, ticks = TRUE),
      sliderInput("var_race", "What is the race of the tweeter? (0 = White, 1 = Non-White)",
                  min = 0, max = 1, value = 0, step = 1, ticks = TRUE),
      sliderInput("dw_score", "What is the dw score of the tweeter? (<0 = Liberal, >0 = Conservative)",
                  min = -.9, max = .9, value = 0, step =.1, ticks = TRUE),
      actionButton("submit", "Submit"))
  )


frow1 <- fluidRow(

  box(
    title = "Tweet Contents", solidHeader = TRUE,
    dataTableOutput("responses"))
)

frow2 <- fluidRow(
  box(
    width =3, title = "Clinton", solidHeader = TRUE,
    collapsible = TRUE, background = "light-blue",
    valueBoxOutput("cl.rt.v", width = 7),
    valueBoxOutput("cl.fv.v", width = 7)),

  box(
    width =3, title = "Trump", solidHeader = TRUE,
    collapsible = TRUE, background = "lime",
    valueBoxOutput("td.rt.v", width = 7),
    valueBoxOutput("td.fv.v", width = 7)),


  box(
    width =3, title = "Congress", solidHeader = TRUE,
    collapsible = TRUE, background = "maroon",
    valueBoxOutput("cg.rt.v", width = 7))
)

body <- dashboardBody(frow1, frow2)


ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='black')

server <- function(input, output, session){
  
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(input)
    clintonFav()
    clintonRT()
    trumpFAV()
    trumpRT()
    congressRT()
    resetForm(session)
  })
  
  observeEvent(input$clear, {
    resetForm(session)
  })
  
  # When the Delete button is clicked, delete all of the saved data files
  observeEvent(input$delete, {
    deleteData()
  })
  
  output$responses <- renderDataTable({
    # update with current response when Submit or Delete are clicked
    input$submit 
    input$delete
    
    loadData()
  })
  
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE, quote= TRUE)
    }
  )
  
  #Clinton Output
  {
  output$cl.rt.v <- renderValueBox({
    input$submit
    valueBox(
      formatC(clintonRT(),format = "d", big.mark=',')
      ,'Predicted Retweets'
      ,icon = icon("retweet",lib='glyphicon')
      ,color = "teal")
  })
  
  output$cl.fv.v <- renderValueBox({
    input$submit
    valueBox(
      formatC(clintonFav(), format="d", big.mark=',')
      ,'Predicted Favorites'
      ,icon = icon("heart",lib='glyphicon')
      ,color = "fuchsia")
    
  })
  }
  
  #Trump Output
  {
    output$td.rt.v <- renderValueBox({
      input$submit
      valueBox(
        formatC(trumpRT(),format = "d", big.mark=',')
        ,'Predicted Retweets'
        ,icon = icon("retweet",lib='glyphicon')
        ,color = "teal")
    })
    
    output$td.fv.v <- renderValueBox({
      input$submit
      valueBox(
        formatC(trumpFAV(), format="d", big.mark=',')
        ,'Predicted Favorites'
        ,icon = icon("heart",lib='glyphicon')
        ,color = "fuchsia")
      
    })
  }
  
  #Congress Output
  {
    output$cg.rt.v <- renderValueBox({
      input$submit
      valueBox(
        formatC(congressRT(),format = "d", big.mark=',')
        ,'Predicted Retweets'
        ,icon = icon("retweet",lib='glyphicon')
        ,color = "teal")
    })
  }
  
}




shinyApp(ui, server)


