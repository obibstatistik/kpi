source("global.R")
source("modules.R")
source("~/.postpass")

# UI

materialsTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "physicalmat",
          
          box(width = 12, solidHeader = TRUE, id="materialsheader1",
              h3("Udlån"),
              img(src='icons/materialer_negativ_45x45.png', align = "right", height="46px")
          ),
          
          fluidRow(
            column(12,
                   
                   tabBox(width = 12,
                          id = "tabset2",
                          tabPanel("Generelt", 
                                   fluidRow(
                                     column(2, h4("Afgræns")),
                                     column(10,
                                            plotlyOutput(ns("heat")),
                                            tableOutput(ns('table'))
                                     ),
                                     column(12,
                                            formattableOutput(ns("loantableall")))
                                   )
                          )
                   ))))
  
}

# Server

materialsTabPanel <- function(input, output, session, data, tablename) {
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  udlaan <- dbGetQuery(con, "SELECT name, hour, circulation_fact_count FROM cicero.udlaan_per_klokkeslaet")
  dbDisconnect(con)
  
  udlaan_heat <- udlaan %>%
    mutate(branch = ifelse(is.na(name), "Andet", name)) %>%
    group_by(branch, hour) %>%
    summarise(sum = sum(circulation_fact_count))
  
  output$table <- renderTable(udlaan_heat)
  
  output$heat <- renderPlotly({
    plot_ly(x=udlaan_heat$hour ,y=udlaan_heat$branch ,z = udlaan_heat$sum, type = "heatmap")
  })
  
}