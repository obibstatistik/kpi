source("global.R")

# konverter integer til danske måneder

danskemåneder <- function(x){
  måned <- case_when(
    as.character(x) == "1" ~ "Januar",
    as.character(x) == "2" ~ "Februar",
    as.character(x) == "3" ~ "Marts",
    as.character(x) == "4" ~ "April",
    as.character(x) == "5" ~ "Maj",
    as.character(x) == "6" ~ "Juni",
    as.character(x) == "7" ~ "Juli",
    as.character(x) == "8" ~ "August",
    as.character(x) == "9" ~ "September",
    as.character(x) == "10" ~ "Oktober",
    as.character(x) == "11" ~ "November",
    as.character(x) == "12" ~ "December"
      ) 
  return(måned)
}


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