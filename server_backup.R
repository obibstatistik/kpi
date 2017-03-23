library(markdown)
library(RPostgreSQL)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

source("global.R")

shinyServer(function(input, output) {

  source("~/.postpass")
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  
  #kpivisits <- dbGetQuery(con, "SELECT * FROM datamart.kpi_visits")
  #kpivisitsgraph <- dbGetQuery(con, "SELECT location, sum(visits2017) as v2017, sum(visits2016) as v2016, sum(visits2015) as v2015 FROM datamart.kpi_visits group by location")
  kpiloan <- dbGetQuery(con, "SELECT * FROM datamart.kpi_loan")
  
  dbDisconnect(con)
  
  # visits
  
  # fysiske materialer
  
  output$mytable = renderDataTable({
    kpiloan
  })
  
  # elektroniske materialer
   
  
})