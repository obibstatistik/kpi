source("global.R")

### POSTGRES METADATA MODUL ###

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

### DOWNLOAD MODUL ###

# UI

csvDownloadUI <- function(id, label = "Download CSV") {
  ns <- NS(id)
  
  downloadButton(ns("download"), label)
}

# SERVER

csvDownload <- function(input, output, session, data, name = NULL) {

  output$download <- downloadHandler(
    filename = function() {
      if(is.na(name)){"test.csv"} else {paste0(name,".csv")}
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
}