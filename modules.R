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

downloadUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput("filetype", "Vælg filtype:", choices = c("excel", "csv", "pdf")),
    downloadButton(ns('downloadData'), 'Download')
  )
}

# SERVER

download <- function(input, output, session, dataset) {
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$dataset, input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(datasetInput(), file, sep = sep,
                  row.names = FALSE)
    }
  )
  
  
}