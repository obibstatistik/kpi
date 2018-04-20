# UI

metaTabPanelUI <- function(id) {
  
  ns <- NS(id)
  tabPanel("Meta", 
  fluidRow(
    column(12,
           tableOutput(ns("datasources_schema"))
    )
  )
  )
}

# SERVER

metaTabPanel <- function(input, output, session, data, tablename) {
  module_data <- reactive({
    data %>% filter(name == tablename)
  })
  output$datasources_schema <- renderTable(
    module_data()
  )
}