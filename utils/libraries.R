libraries<- c("readr", "dplyr", "reshape2", "quantreg", "dygraphs", "xts", "lubridate", "quantmod", "stats",
              "PerformanceAnalytics", "scales", "ggthemes", "igraph", "shiny", "shinydashboard","lmtest",
              "plotly", "ggplot2", "tidyr", "stringr", "plm", "zoo","readr", "data.table", "DT", "shinyjs")

#install packages if not already installed 
packages_to_install <- libraries[!(libraries %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)
#load libraries
lapply(libraries, library, character.only = TRUE)



