source("global.R")
source("modules.R")
source("~/.postpass")

# UI

datasourcesTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "datasources",
          box(width = 12, solidHeader = TRUE, id = "datasourcesheader",
              h3("Datakilder"),
              img(src='icons/datakilder_negativ_45x45', align = "right", height="46px")
          ),
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset3",
                          tabPanel("Kildediagram",
                                   fluidRow(width = 12,
                                            column(width = 12,
                                                   p("Diagrammet viser alle OBBs datakilder, hvilken type det er og sammenhængen mellem de enkelte kilder."),
                                                   img(src='DatakildeOversigt.svg', width="1440px", height="100%" )
                                            )
                                   )
                          ),
                          tabPanel("Kildetabel",
                                   fluidRow(width = 12,
                                            column(width = 12,
                                                   dataTableOutput(ns('datasources_table'))
                                            )
                                   )       
                          )
                   )
            )
          )
  )
  
  
  
}

# SERVER

datasourcesTabPanel <- function(input, output, session, data, tablename) {
  # module_data <- reactive({
  #   data %>% filter(name == tablename)
  # })
  
  ### DB QUERIES ###
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  datasources <- dbGetQuery(con, "SELECT * FROM dokumentation.datakilder")
  dbDisconnect(con)
  
  datasources <- datasources %>%
    select(-id, -undertitel) %>%
    arrange(kildetype) %>%
    mutate(aktiv2 = (ifelse(aktiv, 'Ja', 'Nej'))) %>%
    select(-aktiv, -tabeller) 
  output$datasources_table <- renderDataTable(datasources, options = list(paging = FALSE)) 
  
}