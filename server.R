library(shiny)
library(ggplot2)
library(forecast)
library(dplyr)
library(smooth)
library(reshape2)
require(scales)
library(TTR)
library(plotly)
library(tree)
library(dbscan)
Sys.setlocale(category = "LC_TIME", locale="us")

shinyServer(function(input, output, session) {
   
  dailyData <- reactive({

    getData()
    
})
  

  aggData <- reactive({
    
    dailyData() %>% 
      as.data.frame() 

  }) 
  
  
  prCompData <- reactive({
    
      aggData() %>%
        select(c(2:7)) %>%
          prcomp(scale = TRUE)
 })
  
  
  elbowData <- reactive({
  
    kData <- data.frame()
    
    for (i in 1:10) {
      
      temp <- data.frame(k = i, 
                         sumOfSquares = kmeans(prCompData()$x[,1:3], i)$tot.withinss)
      kData <- rbind(kData, temp)
    
    }
    
    kData
    
 }) 
  
  
  opticsData <- reactive({
    
    op <- optics(prCompData()$x[,1:3], eps = Inf, minPts = 2)
    
    data.frame(order = op$order, distance = op$reachdist)
    
  }) 
  
  
  output$ui <- renderUI({
    
    switch(input$cluster_algo,
          
            "OPTICS" = sliderInput("dynamic_op", "Epsilon",
                                  min = 0, 
                                  max = round(max(opticsData()$distance[opticsData()$distance < Inf]), 1), 
                                  value = round(max(opticsData()$distance[opticsData()$distance < Inf])/2, 1), 
                                  step= 0.1),
           
           "K-means" =  numericInput("dynamic_cl", "Number of Clusters",
                                     1, min = 1, max = 10))
  }) 
  
  
  
  clusterData <- reactive({
    
    if (input$cluster_algo == "K-means") {
      
      cData <- kmeans(prCompData()$x[,1:3], input$dynamic_cl)
      
    }  else {
      
       op <- optics(prCompData()$x[,1:3], eps = Inf, minPts = 2)  
       cData <- extractDBSCAN(op, eps_cl = input$dynamic_op)
       
         
    } 
    
    
    
    data.frame(aggData(), prCompData()$x[,1:3], Cluster = cData$cluster)
    
  })
  
  
  
  output$text22 <- renderText({
    
    if (input$cluster_algo == "K-means") {
    
      req(input$dynamic_cl)
  
      paste("The k-means clustering algorithm creates k clusters based 
            on the euclidean distance. Look for the elbow in the Total Within Sum of Squares plot
            to find a suitable value for the k parameter.") 
           
    } else {
      
      req(input$dynamic_op)
      
      paste("The OPTICS (Ordering points to identify the clustering structure) 
            algorithmn finds density-based clusters. Use the Reachability Distance plot
            to spot outliers and find a suitable value for the epsilon parameter.") 
      
    }    
          
    
  }) 
  
  
  

  observe({
    
    if (input$cluster_algo == "K-means") {

    req(input$dynamic_cl)
    
    updateSelectInput(session, "receive",
                      label = "Plot Cluster",
                      choices = paste0("Cluster ",1:max(clusterData()$Cluster)))
    
    }  else {
      
      req(input$dynamic_op)
      
      updateSelectInput(session, "receive",
                        label = "Plot Cluster",
                        choices = paste0("Cluster ",0:max(clusterData()$Cluster)))
    }
      
  })
  
  
    trendData <- reactive({
  
      
        dailyData() %>%         
      as.data.frame() 
  
    })   
    

  output$plot1 <- renderPlotly({
  
    if (input$cluster_algo == "K-means") {
      
      req(input$dynamic_cl)
      
      plot_ly(clusterData(), x = ~ PC1, 
              y = ~ PC2, 
              z = ~ PC3, 
              color = ~ as.factor(Cluster), 
              text = clusterData()$CODE_CUR,
              type = "scatter3d",
              mode = "markers") 
      
    }  else {
      
      req(input$dynamic_op)
      
      plot_ly(clusterData(), x = ~ PC1, 
              y = ~ PC2, 
              z = ~ PC3, 
              color = ~ as.factor(Cluster), 
              text = clusterData()$CODE_CUR,
              type = "scatter3d",
              mode = "markers") 
      
      }
    

    
  })
  
  
  output$plot11 <- renderPlotly({
    
    if (input$cluster_algo == "K-means") {
      
      req(input$dynamic_cl)
      
      g <- ggplot(elbowData(), aes(x = k, y = sumOfSquares)) + 
        geom_line(size = 1) +
        geom_point(size = 2) +
        geom_vline(xintercept = input$dynamic_cl, size = 1) +
      labs(x = "Number of Clusters", y = "Total Within Sum of Squares") +
        scale_x_continuous(breaks = 1:10) +
        
        
        theme(panel.background = element_blank(),
              plot.background = element_blank(),
              panel.grid.major.y = element_line(colour = "grey", size = 0.1),
             panel.grid.major.x = element_line(colour = "grey", size = 0.1))
      
      ggplotly(g)
      
    }  else {
      
      req(input$dynamic_op)
      
      gg <- ggplot(opticsData(), aes(x = order, y = distance)) + 
        geom_bar(stat = "identity") +
        geom_hline(yintercept = input$dynamic_op, size = 1) +
        labs(x = "Order", y = "Reachability Distance") +
        theme(panel.background = element_blank(),
              plot.background = element_blank(),
              panel.grid.major.y = element_line(colour = "grey", size = 0.1),
              panel.grid.major.x = element_line(colour = "grey", size = 0.1))
      
      ggplotly(gg) %>% layout(bargap=0.15)
      
    }
    
  })  
  

output$plot2 <- renderPlot({
  
  if (input$cluster_algo == "K-means") {
    
    req(input$dynamic_cl)  } else {
  
  req(input$dynamic_op)
  
  }
  
  data <- trendData()
  ggplot()...
 
     
})



output$plot3 <- renderPlot({
  
  if (input$cluster_algo == "K-means") {
    
    req(input$dynamic_cl)  } else {
      
      req(input$dynamic_op)
      
    }
  
  data <- trendData()
  ggplot()...
  
})


output$text <- renderText({
  
  if (input$cluster_algo == "K-means") {
    
    req(input$dynamic_cl)
    
  }  else {
    
    req(input$dynamic_op)
    
  }
  
    if (length(unique(clusterData()$Cluster)) == 1) {
      
      paste("")
 
     } else {
    
    df <- clusterData()
    
    df$Response <- ifelse(df$Cluster == as.numeric(substr(input$receive, 9, 9)), 1, 0)

    fit <- tree(...,data = df, split = "deviance")
    
    res.df <- fit$frame[(rownames(fit$frame) %in% c(1:3)),]
    
    splitVariable <- paste(res.df[1,1])
    splitValue <- as.numeric(gsub("<", "", res.df$splits[1,1]))
    
    if (res.df$yval[2] == 1) {
      
      direction <- "less"
      prob <- paste0(round(res.df$yprob[2,2]*100, 1), " %")
      
    } else  {
      
      direction <- "greater"
      prob <- paste0(round(res.df$yprob[3,2]*100, 1), " %")
      
    }
    
    paste("Text")  
    
  }
  
}) 


})

