library(RPostgreSQL)
library(shiny)
library(treemap)
library(d3treeR)
library(tidyr)
library(dplyr)
library(formattable)
#source("global.R")
source("~/.postpass")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
beholdning_alt <- dbGetQuery(con, "SELECT * from cicero.beholdning where branch != 'Odense Arrest'")
dbDisconnect(con)

ui <- fluidPage(
  #selectInput(ns("niveau"),unique(circ_join$branch)),
  selectInput("niveau","Beholdningsniveau eller materialetype:",c("afdeling"="afdeling","opstilling"="opstilling")),
  formattableOutput("checkouts_table")
)

server <- function(input, output) {
  
  beholdning_alt <- plyr::rename(beholdning_alt, c("branch"="bibliotek","department"="afdeling","locationname"="opstilling","sublocation"="delopstilling","materialtypename"="materialetype","material_dim_count"="antal"))
  
  output$checkouts_table <- renderFormattable({
    beholdning_alt_tbl <- beholdning_alt %>%
      #select(branch,department,locationname,sublocation,materialtypename) %>%
      #select(department,branch,material_dim_count) %>%
      group_by_(input$niveau) %>%
      group_by(bibliotek) %>%
      #group_by(opstilling,bibliotek) %>%
      summarise(antal = sum(antal)) %>%
      spread(key = bibliotek, value = antal, fill = ' ')
      #distinct(beholdning_alt, branch, .keep_all = TRUE)
    formattable(beholdning_alt_tbl)
  })
}

shinyApp(ui = ui, server = server)