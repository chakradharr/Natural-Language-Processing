# NLP Assignment Chakradhar Reddy Kothakapa

library(shiny)
library(rvest)
library(tm)
library(stringr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(caret)
library(tidyr)
library(DT)

shinyServer(function(input,output)
{
  
  Global_Values <- reactiveValues( train_df_global ="" , test_df_global ="",trainTag = "", testTag="", 
                               trainScore ="", testScore ="", testtf="", testMatrix="")
   ######## Web Scrapping #######
   scrape_reviews <- function(url){
    
    # Read web page
    webpage <- read_html(url)
    # Extract records info
    results <- webpage %>% html_nodes(".mmy-reviews__review")
    # Building the dataset
    records <- vector("list", length = length(results))
    
    for (j in seq_along(results)) {
      
      review_description <- str_sub(results[j] %>% html_nodes(".mmy-reviews__blurb") %>% html_text())
      review_year <- str_sub(results[j] %>% html_nodes(".mmy-reviews__date") %>% html_text(trim = TRUE), -4)
      review_star_rating <- str_sub(xml_contents(results[j])[4] %>% html_text(trim = TRUE), end=1 )
      records[[j]] <- data_frame(review_description = review_description, review_year= review_year, review_star_rating= review_star_rating)
      
    }
    df <- bind_rows(records)
    # Normalization of Reviews
    # Removing the punctuations on reviews
    df$Normalized_reviews <- tm::removePunctuation(df$review_description,preserve_intra_word_contractions = TRUE)
    # Coverting to lower cases
    df$Normalized_reviews <- tolower(df$Normalized_reviews)
    df$Normalized_reviews <- tm::removeNumbers(df$review_description)
    df
  } 

  #####Train Dataset
   
   traindf <- reactive({
    list_of_URLs <-  str_replace(input$URL, "year",input$YearsOftrain)
    
    traindf <- data.frame()
    n <- length(list_of_URLs)
    withProgress(message = 'Scrapping Reviews', value = 0.2, {
    for (i in list_of_URLs) {
      
      df<- as.data.frame(scrape_reviews(i))
      print(paste("No.of.reviews web scrapped:" ,nrow(df),"for the year",substr(i,44,47)))
      traindf <- rbind(df,traindf)
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste("Retrieved :" ,nrow(df)," reviews for year",substr(i,44,47)))
      
    }
    })
    Global_Values$train_df_global <- traindf
    traindf
  })
  
  
  ##########Test Dataset
  
  testdf <- reactive({
    
    testurl <- str_replace(input$URL, "year",input$YearsOftest)
    print(testurl)
    testdf<- as.data.frame(scrape_reviews(testurl))
    Global_Values$test_df_global <- testdf
    testdf
  })

 
  # This output contains the train dataset and used to display the dataset in table format
  output$trainDataset <- DT::renderDataTable({
    train_df <- traindf()
    DT::datatable(head(train_df,n=50), options = list(lengthMenu = c(15, 30, 50), pageLength = 5 ), rownames= FALSE , width = 6 )
    
  })
  
  # This output contains the test  dataset and used to display the dataset in table format
  output$testDataset<- DT::renderDataTable({
    test_df <- testdf()
    DT::datatable(head(test_df,n=50), options = list(lengthMenu = c(15, 30, 50), pageLength = 5  ), rownames= FALSE , width = 6)
    
    })
  
  
  ## Tag the Reviews based on our interest area
  
  TagReviews <- function(data){
    Tags <- c("service","interior","price","handling")
    
    data$Tags <- ""
    
    for(i in Tags)
    {
      index <- grep(i , data$Normalized_reviews)
      data$Tags[index] <- paste(data$Tags[index], i , sep = " ")
    }
    data
  }
  
 
  
#### computing sentiment analysis using NRC lexicons 
  
  SentimentAnalysis_Score <- function(df){
      df$id <- row.names(df)
      traindf_tokens <-  df %>% unnest_tokens(word, Normalized_reviews) %>%  ungroup ()
      afinn <- get_sentiments("afinn")
      train_WithOutStopWords<- traindf_tokens %>% anti_join(stop_words, by = c("word" = "word"))

      Affinscore <- train_WithOutStopWords %>%
      inner_join(afinn) %>%
      group_by(id) %>%
      summarise(Sentiment_Score = round(mean(score))) %>% mutate()

      data <-(inner_join(df,Affinscore))
      data
  }
  

  
#### Compute Operations on Training Data
  OperationsOnTrain <- function(){
    
    df1 <- isolate(Global_Values$train_df_global)
    TagReviews <- TagReviews(df1)
    Global_Values$trainTags <- TagReviews
    Global_Values$trainScore <- SentimentAnalysis_Score(TagReviews)
    isolate(Global_Values$trainScore)
  }
  
  #### Compute Operations on Test Data
  OperationsOnTest <- function(){
   
    
    df1 <- isolate(Global_Values$test_df_global)
    Global_Values$testTags <- TagReviews(df1)
    Global_Values$testScore <- SentimentAnalysis_Score(TagReviews(df1))
    test <- isolate(Global_Values$testScore)
    train <- isolate(Global_Values$trainScore)
    model <- lm(as.numeric(as.character(review_star_rating))~as.numeric(as.character(Sentiment_Score)), data = train)
    ypred <- predict(model, newdata = test)
    test <- cbind(test,round(ypred))
    colnames(test) <- c("review_description","review_year","review_star_rating","Normalized_Reviews","Tags","id","Sentiment_Scores","Predicted_Rating")
    test[is.na(test)] <- 0
    Global_Values$testScore <- test
    test
  }
 
  
  
  ########## Computed Train ###########
  
  output$TrainReviews_SentimentScores <- renderDataTable({
    OperationsOnTrain()
  })
  
  ##################################### 
  
  ######### Computed Test #############
  output$TestReviews_SentimentScores  <- renderDataTable({
    OperationsOnTest()
  })
  
  
  # ########### Accuracy ###############
  
  output$confusionMatrix <- renderPrint({
    data <- isolate(Global_Values$testScore)
    #data[is.na(data)] <- 0
    Pred_Rating <- matrix(data$Predicted_Rating,ncol = 1)
    YRating <- matrix(as.numeric(data$Ratings),ncol=1)
    Accuracy_Table <- table(factor(Pred_Rating, levels=min(YRating):max(YRating)), factor(YRating, levels=min(YRating):max(YRating)))
    Global_Values$testMatrix <- Accuracy_Table 
    Accuracy_Table 
  })
  
  ####################################
  
  ########## Accuracy ###############
  output$accuracy <- renderPrint({
    Acc_matrix <- isolate(Global_Values$testMatrix)
    testsc <- isolate(Global_Values$testScore)
    for(i in 1:5){
      diag_s = sum(as.numeric(Acc_matrix[i,i]))
    }
    accuracy = diag_s /nrow(testsc)
    accuracy * 100 
  })
  ###################################
  
  
  ########## Tagged Train ############
  output$tfidf <- renderDataTable({
    data <- isolate(Global_Values$trainData)
    tagData <- TagReviews(data)
    Global_Values$trainTag <- tagData
    
    separatedData <- separate_rows(tagData, Tags, convert = TRUE)
    
    custom_stop_words <- bind_rows(data_frame(word = c("a4","2012s","nor","e","8th","9th"), 
                                              lexicon = c("custom")), 
                                   stop_words)
    
    
    tf_words <- separatedData %>%
      unnest_tokens(word, Normalized_Reviews) %>%
      anti_join(custom_stop_words) %>%
      dplyr::count(word, Tags,sort=TRUE) %>%
      ungroup() %>%
      bind_tf_idf(word, Tags, n) %>% arrange(-tf_idf) 
    
    Global_Values$testtf <- tf_words
    colnames(tf_words)[1] <- c("Word")
    colnames(tf_words)[3] <- c("Count")
    tf_words
    
  })

  
  ############# Plot #################
  output$tfPlot <- renderPlot({
    tf_words <- isolate(Global_Values$testtf)
    Service <- head(tf_words[grep("Service",tf_words$Tags),],10,tf_words$tf_idf)
    Price <- head(tf_words[grep("Price",tf_words$Tags),],10,tf_words$tf_idf)
    Handling <- head(tf_words[grep("Handling",tf_words$Tags),],10,tf_words$tf_idf)
    Interior <- head(tf_words[grep("Interior",tf_words$Tags),],10,tf_words$tf_idf)
    
    tf_words <- rbind(Service,Price,Handling,Interior)
    
    
    tf_words %>%  mutate(word = factor(word, levels = rev(unique(word)))) %>%
      ggplot(aes(word, tf_idf, fill = Tags)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~Tags, ncol = 2, scales = "free") +
      coord_flip() +
      labs(title = "Top 10 most popular words in Camry Reviews in 2017",
           x = NULL, y = "tf-idf")
  })
  ####################################
  
  
  ############## Average Sentiment Score #############
  # output$sentiment <- renderDataTable({
  #   
  #   
  #   #Average Ratings 
  #   
  #   df <- isolate(Global_Values$testScore)
  #   avgSentimentScore <- mean(as.numeric(as.character(df $Sentiment_Scores)),na.rm = TRUE)
  #   avgRatingByUser <- mean(as.numeric(as.character(df $Predicted_Ratings)),na.rm = TRUE) 
  #   
  #   
  #   # max <- max(Affinscore$sentiment)
  #   # min <- min(Affinscore$sentiment)
  #   # 
  #   # Affinscore$stdscore <- round(5*((Affinscore$sentiment - min) / (max-min)),2)
  #   # avgscore <- mean(Affinscore$stdscore)
  #   # avgrating <- mean(as.integer(traindf$Rating))
  #   
  #   avgratingtags <- c()
  #   avgratingbytag <- list()
  #   tagWords <- c("service","interior","price","handling")
  #   traindf <- isolate(Global_Values$train_df_global)
  #   for(i in tagWords)
  #   {
  #     index <- grep(i, traindf$Tags)
  #     avgratingbytag[i] <- mean(as.integer(traindf$Ratings[index]))
  #   }
  #   scoreVector <- c(avgSentimentScore, avgRatingByUser, avgratingbytag[i], avgratingbytag[i] ,avgratingbytag[i],avgratingbytag[i])
  #   scoreVector <- data.frame(scoreVector)
  #   
  #   scoreTitle <- c("Avg Sentiment Score","Avg User Rating","Avg Service Rating",
  #                   "Avg Price Rating","Avg Handling Rating", "Avg Interior Rating")
  #   scoreTitle <- data.frame(scoreTitle)
  #   
  #   data <- cbind(scoreTitle,scoreVector)
  #   colnames(data) <- c("Average Of ","Calculated Rating")
  #   #data[,-1] <-round(data[,-1],2)
  #   data
  #   
  # })
  # 
  
  ############## Average Sentiment Score #############
  output$sentiment <- renderDataTable({
    data <- isolate(Global_Values$testScore)
    rowCount <- nrow(data)
    avgScore <- sum(as.numeric(as.character(data$Sentiment_Score))) / rowCount
    avgRating <- sum(as.numeric(as.character(data$Ratings))) / rowCount
    
    serviceData <- data[grep("Service",data$Tag_Reviews),]
    priceData <- data[grep("Price",data$Tag_Reviews),]
    handlingData <- data[grep("Handling",data$Tag_Reviews),]
    interiorData <- data[grep("Interior",data$Tag_Reviews),]
    
    avgServiceScore <- (sum(as.numeric(as.character(serviceData$Sentiment_Score))) / nrow(serviceData))
    avgPriceScore <- (sum(as.numeric(as.character(priceData$Sentiment_Score))) / nrow(priceData))
    avgHandlingScore <- (sum(as.numeric(as.character(handlingData$Sentiment_Score))) / nrow(handlingData))
    avgInteriorScore <- (sum(as.numeric(as.character(interiorData$Sentiment_Score))) / nrow(interiorData))
    
    scoreVec <- c(avgScore, avgRating, avgServiceScore, avgPriceScore ,avgHandlingScore,avgInteriorScore)
    scoreVec <- data.frame(scoreVec)
    
    scoreTitle <- c("Avg Sentiment Score ","Avg User Rating","Avg Service Rating",
                    "Avg Price Rating","Avg Handling Rating", "Avg Interior Rating")
    scoreTitle <- data.frame(scoreTitle)
    
    data <- cbind(scoreTitle,scoreVec)
    colnames(data) <- c("Entity","Calculated Rating")
    data
    
  })
  
  
  


})
