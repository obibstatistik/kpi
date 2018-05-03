source("global.R")
source("modules.R")
source("~/.postpass")

# UI

frontpageTabPanelUI <- function(id) {
  
  ns <- NS(id)

  tabItem(tabName = "frontpage")
  
}

# SERVER

frontpageTabPanel <- function(input, output, session) {
  
}