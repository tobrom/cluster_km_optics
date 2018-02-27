library(RODBC)
library(plotly)

convertClusterInteger <- function(clusterInteger) {
  
  switch(clusterInteger, 
         "1" = "first",
         "2" = "second",
         "3" = "third",
         "4" = "forth",
         "5" = "fifth",
         "6" = "sixth",
         "7" = "seventh",
         "8" = "eight",
         "9" = "ninth",
         "10" = "tenth") }
}





getData <- function(x, y) {

  ch <- odbcConnect()  

  rawData <- sqlQuery(ch, paste0("....")) 
  
close(ch)
  
  return(rawData)  
}  
  




