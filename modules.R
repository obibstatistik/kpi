# UI

metaTabPanelUI <- function(id) {
  
  ns <- NS(id)
  fluidRow(
    column(12,
           tableOutput(ns("datasources_schema"))
    )
  )
}

# SERVER

metaTabPanel <- function(input, output, session, data, tablename) {
  
  module_data <- reactive({
    data %>% 
      filter(name == tablename) %>%
      select(-name)
  })
  output$datasources_schema <- renderTable(
    module_data()
  )
}