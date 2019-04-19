list.of.packages <- c("ggplot2", "dplyr", "shiny", "tidyverse", "lubridate", "tokenizers", "tm", "textmineR", "quanteda","shinydashboard","stats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

#update to git/EC2 directory
setwd("~/team-project-team-2-master/models")
load(file = "clinton_rt.rda")
load(file = "clinton_fav.rda")



#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Tweet Predictor")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Team 2", tabName = "Team 2 644", icon = icon("dashboard"))
  )
)


frow1 <- fluidRow(
  box(
    title = "Tweet", solidHeader = TRUE,
    textInput("text", "Type Tweet:"),
    sliderInput("Date",
                "Dates:",
                min = as.Date("2015-11-07","%Y-%m-%d"),
                max = as.Date("2016-11-07","%Y-%m-%d"),
                value=as.Date("2015-11-07"),
                timeFormat="%Y-%m-%d"),
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

body <- dashboardBody(frow1, frow2)


ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')

server <- function(input, output){
  
  get.text = reactive({
    if (input$submit > 0) {
      input$text
      return()
    }
  })
  
  get.date = reactive({
    if (input$submit > 0) {
      input$Date
      return()
    }
  })

    text <- get.text
    date <-  get.date
    tweet <- data.frame(tweet_text = text, date = date)
    
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
  
    output$content <- renderText(text)
  
  }

shinyApp(ui, server)
