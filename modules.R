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

# ### DOWNLOAD MODUL ###
# 
# # UI
# 
# downloadUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     downloadButton(ns('downloadData'), 'Download')
#   )
# }
# 
# # SERVER
# 
# download <- function(input, output, session, dataset) {
#   
#   data <- starwars
#   
#   output$downloadData <- downloadHandler(
#     filename = function() {
#       paste("data-", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#       write.csv(data, file)
#     }
#   )
#   
#   
# }