source("global.R")
source("modules.R")
source("~/.postpass")

# UI

frontpageTabPanelUI <- function(id) {
  
  ns <- NS(id)

  tabItem(tabName = "frontpage",
          box(
             h4("Feedback"),
             p("Vi vil meget gerne have feedback på Whitebook og I er meget velkomne til skrive til Thomas Bojsen, hvis I støder på problemer eller hvis I har udviklingsønsker til grafer eller visninger I er nysgerrige på.")
             )
          ) 
  
}

# SERVER

frontpageTabPanel <- function(input, output, session) {
  
}