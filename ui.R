# Install the required packages 

list.of.packages <- c("shiny", "rvest","tm", "stringr","tidytext","dplyr","ggplot2","caret","DT","shinyjs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# remove all the objects from the environment

# rm(list=ls(all=TRUE))

# including the libraries

library(shiny)
library(shinythemes)
library(tidytext)
library(dplyr)
library(rvest)
library(tm)
library(stringr)
library(ggplot2)
library(caret)
library(DT)
library(shinyjs)
shinyjs::useShinyjs()
shinyUI <- fluidPage(theme = shinytheme("flatly"),
                     
   navbarPage(title = " Natural Language Processing - Chakradhar Reddy Kothakapa ", inverse = T, position="static-top" ,
         sidebarLayout(
               sidebarPanel (
                             
                 tags$b("Web Scraping : Toyota Camry Reviews from Cars.com"), 
                 br(),
                 br(),br(),
                 textInput(inputId ="URL",label = "Enter the URL : ", value = "https://www.cars.com/research/toyota-camry-year/consumer-reviews/?pg=1&nr=500"),
                 br(),
                 br(),
                 selectizeInput(
                   'YearsOftrain', 'Select the year of Toyota Camry Model for traning ', choices = c('2012','2013','2014','2015','2016','2017','2018'),
                   multiple = TRUE),
                 br(),
                 br(),
                 selectizeInput(
                   'YearsOftest', 'Select the year of Toyota Camry Model for test  ', choices = c('2017','2018'),
                   multiple = TRUE),
                 br(), br()
                
                  ),
              mainPanel(
                  tabsetPanel(type="tab",
                              
               # tabPanel("Train Dataset",mainPanel(uiOutput("OperationsOnTrain"))),
               # tabPanel("Test Dataset",mainPanel(uiOutput("OperationsOnTest"))),
                tabPanel("About",
                      tags$html(
                      h3(strong('Project Description :')),
                      p(br(),'In this project , will',br() )
                      )),
               tabPanel("Training Dataset",
                        fluidRow(
                          column(12, h3("Training Data "), dataTableOutput("trainDataset"))
                        )),
                        
               tabPanel("Testing Dataset",
                        fluidRow(
                          column(12, h3("Testing Data "), dataTableOutput("testDataset"))
                        )),
               
               tabPanel( "Sentiment Analysis" ,
                         tabsetPanel(type="tab",
                                     tabPanel("Training Data",
                                              fluidRow(
                                                column(12, h3("Training Data with Tags & Sentiment scores"), dataTableOutput("TrainReviews_SentimentScores"))
                                              )
                                     ),
                                     tabPanel("Testing Data",
                                              fluidRow(column(12,h3("Testing Data with Predicted star rating"), dataTableOutput("TestReviews_SentimentScores")))
                                     ),
                                     tabPanel("Performance Measures",
                                              fluidRow(column(4, h3("Confusion Matrix"), verbatimTextOutput("confusionMatrix"))),
                                              fluidRow(column(4, h3("Accuracy of Test Data"), verbatimTextOutput("accuracy")))
                                     )
                         )),
               tabPanel("Comparisions",
                        fluidRow
                        (
                          column(12, h3("Average Rating Comparison"), dataTableOutput("sentiment"))
                        )),
               tabPanel( "TF-IDF" ,
                         tabsetPanel(type="tab",
                                     tabPanel("Computation",
                                              fluidRow(
                                                column(12, h3(" TF-IDF scores for each tag in Training data"), dataTableOutput("tfidf"))
                                              )
                                     ),
                                     tabPanel("Visualization",
                                              fluidRow(
                                                (column(12,h3("Visualization of TF_IDF scores"), plotOutput("tfPlot")))
                                              )
                                     ))),                  
               tabPanel("Comments",
                        h3("Insights & Comments"),
                        p("1. The packages used are shiny, rvest, tm, stringr, tidytext, dplyr, ggplot2, caret and DT."),
                        p("2.The model is trained using linear modelling with Year 2012-2016 camry data."),
                        p("3.There are a few comments that are very short and is either very positive or 
                          very negative and hence have less sentiment score but the user rating is very high or very less."),
                        p("4.The accuracy of the model is approximately 67 %."))

                                         
                     ))
               )))



# Version 2.0 Changes 25/05/2017

# Read web page ( URL Changed)
# webpage <- read_html("https://www.cars.com/research/toyota-camry-2017/consumer-reviews//?pg=1&nr=50")
# # Extract records info ( object html class changed)
# results <- webpage %>% html_nodes(".review-listing-card")
# html nodes for description , year and star rating changed
# review_description <- str_sub(results[1] %>% html_nodes(".review-card-text") %>% html_text())
# review_year <- str_sub(results[1] %>% html_nodes(".review-card-review-by") %>% html_text(trim = TRUE), -4)
# review_star_rating <- str_sub(xml_contents(results[j])[4] %>% html_text(trim = TRUE), end=1 )