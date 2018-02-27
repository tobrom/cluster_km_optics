library(shiny)
library(xlsx)
library(RODBC)
library(plotly)
library(ggplot2)
rm(list = ls())

shinyUI(fluidPage(
  
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #000000}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #000000}")),
  
  
  
  titlePanel("Analytics"),
 # titlePanel("Clustering & Trends"),
  hr(),
  sidebarLayout(
    sidebarPanel(helpText("Information."),
                 hr(), 
    selectInput("tickers", "Index", choices = c("a", 
                                                  "b",
                                                  "c")),
    hr(),
    sliderInput("daylag",
                "Feature History",
                min = 1,
                max = 30,
                value = 20), 
    hr(),
    radioButtons("cluster_algo", "Clustering Algorithm:",
                 choices = c("K-means",
                             "OPTICS"),
                 selected = "K-means"),
    hr(),
    
    uiOutput("ui"),
    textOutput("text22"),
    hr(),
    selectInput("receive", "Select Cluster", choices = "Cluster 1"),
    hr(),
  
    textOutput("text")
    ),
   
    mainPanel(
      h3("Index Instrument Clustering"),
      h6("The clusters are created based on the first three Principal Components"),
      plotlyOutput("plot11"),
      plotlyOutput("plot1"),
      
      h3("Long Term Trend"),
      h6("The long term trend is evaluated by comparing the 20 days moving average with the 50 days"),
      
      plotOutput("plot2"),
      
      h3("Short Term Trend"),
      h6("The short term trend is evaluated by comparing the 15 days moving average with the 5 days"),
      plotOutput("plot3")
    

   )
  )
 )
)



