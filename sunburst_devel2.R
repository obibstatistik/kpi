library(RPostgreSQL)
library(shiny)
library(tidyr)
library(dplyr)
library(sunburstR)
#source("global.R")
source("~/.postpass")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
beholdning <- dbGetQuery(con, "SELECT
  coalesce(department,'ingen afdeling') ||'-'||
  coalesce(locationname,'ingen opstilling') ||'-'||
  coalesce(sublocation,'ingen delopstilling') 
  AS sequence,
  material_dim_count antal
  from cicero.beholdning 
  where branch = 'Odense Hovedbibliotek'
  ")
dbDisconnect(con)

ui <- fluidPage(
  sunburstOutput("sunburst")
)

server = function(input, output) {
    sunburst(beholdning) 
}

shinyApp(ui = ui, server = server)