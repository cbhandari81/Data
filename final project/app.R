#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(twitteR)
library(caret)
library(lubridate)
library(MASS)
library(e1071)
library(readr)
library(randomForest)

dat <- read_csv("https://raw.githubusercontent.com/cbhandari81/data/master/twitter_s.csv")
#"https://raw.githubusercontent.com/cbhandari81/data/master/Twitterdata_s.csv")
consumer_key <- "aNTRmMXvPzA86nuKgnW8iAdha"
consumer_secret <- "WZgtz4LD8aO5ysFp5G7YmNmAb8LJstANlQw88H76rxzzxbrIBy"
access_token <- "1375379682-kabi92NOLPjVaZnFQPyGIlNcJWjfXtoVFeKoPAc"
access_secret <- "QCxMASRiUWL2PtgXoENxIsMeH2do2QKSslXGTxkgZzFx0"
options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions.
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)




calc_knn3 <- function(uname, cc){
  
  udetails <- getUser(uname)  
  test_set <- data.frame(udetails$followersCount, udetails$statusesCount, udetails$favoritesCount, udetails$friendsCount, udetails$created , udetails$lang)
  
  tmpk <- data.frame(dat$followersCount, dat$statusesCount, dat$favoritesCount, dat$friendsCount, dat$created , dat$lang)
  colnames(tmpk) <- c("followersCount", "statusesCount", "favoritesCount", "friendsCount", "created", "lang")
  
  tmpk$cc <- ifelse(tmpk$followersCount > 20*tmpk$friendsCount | tmpk$followersCount > 5000, 1, 0)
  tmpk <- na.omit(tmpk)
  
  tmpk$followersCount <- as.factor(tmpk$followersCount)
  #tmpk$lang <- substr(tmpk$lang, 1,2)
  tmpk$lang <- as.factor(tmpk$lang)
  tmpk$created <- as.Date(tmpk$created)
  tmpk$created <- year(tmpk$created)
  
  tmpk$statusesCount <- as.double(tmpk$statusesCount)
  tmpk$favoritesCount <- as.double(tmpk$favoritesCount)
  tmpk$friendsCount <- as.double(tmpk$friendsCount)
  test_set$cc <- cc
  colnames(test_set) <- colnames(tmpk)
  
  #test_set$followersCount <- round(test_set$followersCount/2000)
  #test_set$followersCount <- as.factor(test_set$followersCount)
  #test_set$lang <- substr(test_set$lang, 1,2)
  test_set$lang <- as.factor(test_set$lang)
  test_set$created <- as.Date(test_set$created)
  test_set$created <- year(test_set$created)
  test_set$followersCount <- NULL
  
  fit <- knn3(followersCount~., data=tmpk, k=5)
  ret <- as.double(predict(fit, newdata = test_set, type = "class")) 
  return(ret)
}

calc_random_forest <- function(uname, cc){
  
  udetails <- getUser(uname)  
  test_set <- data.frame(udetails$followersCount, udetails$statusesCount, udetails$favoritesCount, udetails$friendsCount, udetails$created)# , udetails$lang)
  
  tmpk <- data.frame(dat$followersCount, dat$statusesCount, dat$favoritesCount, dat$friendsCount, dat$created)# , dat$lang)
  colnames(tmpk) <- c("followersCount", "statusesCount", "favoritesCount", "friendsCount", "created")#, "lang")
  tmpk <- sample_n(tmpk, 1000)
  tmpk$cc <- ifelse(tmpk$followersCount > 20*tmpk$friendsCount | tmpk$followersCount > 5000, 1, 0)
  tmpk <- na.omit(tmpk)
  tmpk$created <- as.Date(tmpk$created)
  tmpk$created <- year(tmpk$created)
  
  test_set$cc <- cc
  
  colnames(test_set) <- colnames(tmpk)
  fitn <- randomForest(followersCount~., data = tmpk)
  ret <- round(predict(fitn, newdata = test_set)) 
  return(ret)
}


calc_lda <- function(uname, ccn){
  
  udetails <- getUser(uname)  
  test_set <- data.frame(udetails$followersCount, udetails$statusesCount, udetails$favoritesCount, udetails$friendsCount, udetails$created , udetails$lang)
  
  tmpk <- data.frame(dat$followersCount, dat$statusesCount, dat$favoritesCount, dat$friendsCount, dat$created , dat$lang)
  colnames(tmpk) <- c("followersCount", "statusesCount", "favoritesCount", "friendsCount", "created", "lang")
  tmpk$cc <- ifelse(tmpk$followersCount > 20*tmpk$friendsCount | tmpk$followersCount > 5000, 1, 0)
  tmpk <- na.omit(tmpk)
  tmpk <- filter(tmpk, cc == ccn)
  tmpk$lang <- as.factor(tmpk$lang)
  tmpk$created <- as.Date(tmpk$created)
  tmpk$created <- year(tmpk$created)
  
  test_set$cc <- ccn
  colnames(test_set) <- colnames(tmpk)
  
  test_set$lang <- as.factor(test_set$lang)
  test_set$created <- as.Date(test_set$created)
  test_set$created <- year(test_set$created)
  test_set$followersCount <- NULL
  
  tmpk$followersCount <- as.double(tmpk$followersCount)
  tmpk$statusesCount <- as.double(tmpk$statusesCount)
  tmpk$favoritesCount <- as.double(tmpk$favoritesCount)
  tmpk$friendsCount <- as.double(tmpk$friendsCount)
  
  fitlm <- lm(formula = followersCount~., data = tmpk)
  ret <- round(as.double(predict(fitlm, newdata = test_set)))
  ret <- ifelse(ret<0, 0 , ret)
  return(ret)
}

calc_qda <- function(uname, cc){
  
  udetails <- getUser(uname)  
  test_set <- data.frame(udetails$followersCount, udetails$statusesCount, udetails$favoritesCount, udetails$friendsCount, udetails$created)# , udetails$lang)
  
  tmpk <- data.frame(dat$followersCount, dat$statusesCount, dat$favoritesCount, dat$friendsCount, dat$created)#, dat$lang)
  colnames(tmpk) <- c("followersCount", "statusesCount", "favoritesCount", "friendsCount", "created")#, "lang")
  #tmpk$cc <- ifelse(tmpk$followersCount > 20*tmpk$friendsCount | tmpk$followersCount > 5000, 1, 0)
  tmpk <- na.omit(tmpk)
  #tmpk$lang <- as.factor(tmpk$lang)
  tmpk$created <- as.Date(tmpk$created)
  tmpk$created <- year(tmpk$created)
  
  #test_set$cc <- 1
  colnames(test_set) <- colnames(tmpk)
  
  #test_set$lang <- as.factor(test_set$lang)
  test_set$created <- as.Date(test_set$created)
  test_set$created <- year(test_set$created)
  test_set$followersCount <- NULL
  
  tmpk$followersCount <- as.double(tmpk$followersCount)
  tmpk$statusesCount <- as.double(tmpk$statusesCount)
  tmpk$favoritesCount <- as.double(tmpk$favoritesCount)
  tmpk$friendsCount <- as.double(tmpk$friendsCount)
  #tmpk$followersCount <- as.factor(tmpk$followersCount)
  
  #fitqda <- train(followersCount~., method= "qda",  preProcess = c('scale', 'center'), data=tmpk)
  
  tmpk$followersCount <- round(tmpk$followersCount/1000)
  tmpk$followersCount <- ifelse(tmpk$followersCount >= 50, 50, tmpk$followersCount)
  
  fitqda <- qda(formula = followersCount~., data = tmpk)
  ret <- as.double(predict(fitqda, newdata = test_set)$class)
  ret <- (ret*1000) - 500
  return(ret)
}

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict Twitter Followers"), 
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      textInput("uname", "Enter Twitter User Name"),
      checkboxInput("cc", "Are you a company or Celebrity ?"),
      actionButton("predictgo", label = "Predict !!"),
      br(),
      h4("Actual value"),
      verbatimTextOutput("actv")
    ), 
    
    mainPanel(
      h3("KNN Prediction"),
      verbatimTextOutput("knn"),
      br(),
      h3("Random Forest Prediction"),
      verbatimTextOutput("nbp"),
      br(),
      h3("Linear Regression Prediction"),
      verbatimTextOutput("ldap"),
      br(),
      h3("Quadratic Discriminant Prediction (sd=1000)"),
      verbatimTextOutput("qdap")
    )
  )
  
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  actv <- eventReactive(input$predictgo, {
    getUser(input$uname)$followersCount
  })
  
  knn <- eventReactive(input$predictgo, {
    numcc <- ifelse(input$cc == TRUE, 1, 0)
    round(mean(replicate(10,calc_knn3(input$uname, numcc))))
  })
  
  nbp <- eventReactive(input$predictgo, {
    numcc <- ifelse(input$cc == TRUE, 1, 0)
    round(mean(replicate(10, calc_random_forest(input$uname, numcc))))
  })
  
  ldap <- eventReactive(input$predictgo, {
    numcc <- ifelse(input$cc == TRUE, 1, 0)
    round(mean(replicate(10, calc_lda(input$uname, numcc))))
  })
  
  qdap <- eventReactive(input$predictgo, {
    numcc <- ifelse(input$cc == TRUE, 1, 0)
    round(mean(replicate(10, calc_qda(input$uname, numcc))))
  })
  #str <- paste0("you have selected user ", input$uname)
  output$knn  <- renderText({ 
    #input$predictgo 
    progress <- shiny::Progress$new()
    progress$set(message = "Computing KNN data. Please wait...", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    knn()
  })
  
  output$nbp  <- renderText({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Computing Random Forest data. Please wait...", value = 0)
    nbp()
  })
  
  output$ldap  <- renderText({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Computing LM regression data. Please wait...", value = 0)
    ldap()
  })
  
  output$qdap  <- renderText({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Computing QDA data. Please wait...", value = 0)
    qdap()
    
  }) 
  
  output$actv <- renderText({ actv() })
})

# Run the application 
shinyApp(ui = ui, server = server)

