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
                                            formattableOutput(ns("checkout_all_plot"))
                                     ),
                                     column(12,
                                            formattableOutput(ns("checkout_all_table")))
                                   )
                          ),
                          tabPanel("Cirkulation", 
                                   fluidRow(
                                     column(2, h4("Afgræns")),
                                     column(10,
                                            plotlyOutput(ns("cirkulation")),
                                            tableOutput(ns('cirktable'))
                                     ),
                                     column(12,
                                            formattableOutput(ns("cirkulation_table_all"))
                                     )
                                   )
                          ),
                          tabPanel("Pr. bibliotek", 
                                   fluidRow(
                                     column(2, h4("Afgræns")),
                                     column(10,
                                            plotlyOutput(ns("bibliotasdfek")),
                                            tableOutput(ns('bibtableasdf'))
                                     ),
                                     column(12,
                                            formattableOutput(ns("bibtableall"))
                                     )
                                   )
                          ),
                          tabPanel("Data og dokumentation",
                                   fluidRow(
                                     column(12,
                                            p("Dokumentation")
                                     )
                                   )  
                          )
                   ))))
}

# Server

materialsTabPanel <- function(input, output, session, data, tablename) {
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  udlaan <- dbGetQuery(con, "SELECT name, hour, circulation_fact_count FROM cicero.udlaan_per_klokkeslaet")
  checkout_all <- dbGetQuery(con, "SELECT * FROM public.people_counter")
  dbDisconnect(con)
  
  
  # visitors total plot#
  output$checkouts_plot_all <- renderPlotly({
    visitsoverview <- visitors %>%
      select(date, count, location) %>%
      mutate(year = year(date)) %>%
      filter(year != as.integer(format(Sys.Date(), "%Y"))) %>%
      filter(if(input$mainlibrary == 'Uden Hovedbiblioteket')  (location != 'hb') else TRUE) %>%
      group_by(year) %>%
      summarise(sum = sum(count)) 
    plot_ly(visitsoverview, x = visitsoverview$year, y = visitsoverview$sum, type = 'bar', marker = list(color = color1)) %>%
      layout(yaxis = list(title = 'Antal'), xaxis = list(title = 'År', dtick = 1, autotick = FALSE))
  })
  
  udlaan_heat <- udlaan %>%
    mutate(branch = ifelse(is.na(name), "Andet", name)) %>%
    group_by(branch, hour) %>%
    summarise(sum = sum(circulation_fact_count))
  
  output$table <- renderTable(udlaan_heat)
  
  output$heat <- renderPlotly({
    plot_ly(x=udlaan_heat$hour ,y=udlaan_heat$branch ,z = udlaan_heat$sum, type = "heatmap")
  })
  
}