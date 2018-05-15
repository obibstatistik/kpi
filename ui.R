# LIBRARIES
source("global.R")

# MODULES
source("modules.R")

# SECTIONS (MODULES)
source("./sections/datasources.R")
source("./sections/eressources.R")
source("./sections/events.R")
source("./sections/eventareas.R")
source("./sections/frontpage.R")
source("./sections/materials.R")
source("./sections/meetingrooms.R")
source("./sections/online_odensebib.R")
source("./sections/staff.R")
source("./sections/users.R")
source("./sections/visitors.R")

dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Hvidbog"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Forside", tabName = "frontpage", icon = icon("home", lib="font-awesome")),
      menuItem("Arrangementer", tabName = "events", icon = icon("calendar", lib="font-awesome")),
      menuItem("Det fysiske rum", tabName = "space", icon = icon("building-o", lib="font-awesome"),
               menuItem("Besøgende", tabName = "visits"),
               menuItem("Mødelokaler", tabName = "meetingrooms"),
               menuItem("Event områder", tabName = "eventareas")#,
               #menuItem("Smart City", tabName = "smartcity")
      ),
      menuItem("Online", tabName = "online", icon = icon("laptop", lib="font-awesome"), 
               #menuItem("Sites", tabName = "weboverview"),
               menuItem("Odensebib.dk", tabName = "odensebib")#,
               #menuItem("Biblioteket App", tabName = "app")
      ),
      menuItem("Materialer", tabName = "emat", icon = icon("book", lib="font-awesome"),
               menuItem("Udlån", tabName = "fysmat"),
               menuItem("Materialeindkøb", tabName = "acquisition")
      ),
      menuItem("E-Ressourcer", tabName = "emat", icon = icon("database", lib="font-awesome"),
               menuItem("E-Bøger", tabName = "ebooks")#,
               # menuItem("E-Film", tabName = "emovies"),
               # menuItem("E-Baser", tabName = "edatabases")
      ),
      menuItem("Brugere", tabName = "users", icon = icon("users", lib="font-awesome")),
      menuItem("Personale", tabName = "personal", icon = icon("users", lib="font-awesome")),
      menuItem("Datakilder", tabName = "datasources", icon = icon("database", lib="font-awesome"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    tabItems(
      
      ### FRONTPAGE ###  
      frontpageTabPanelUI(id = "frontpage"),  
      
      ### ARRANGEMENTER ###  
      eventsTabPanelUI(id = "events"),  
      
      ### DET FYSISKE RUM ###
      
      # Besøgende #
      visitorsTabPanelUI(id = "visitors"),    
      
      # Mødelokaler #
      meetingroomsTabPanelUI(id = "meetingrooms"), 
      
      # Event områder #
      eventareasTabPanelUI(id = "eventareas"), 
      
      ### ONLINE ###
      online_odensebibTabPanelUI(id = "online_odensebib"),  
      
      ### MATERIALER ###
      materialsTabPanelUI(id = "materials"),
      
      ### E-RESSOURCER ###
      eressourcesTabPanelUI(id = "eressources"),
      
      ### BRUGERE ###
      usersTabPanelUI(id = "users"),
      
      ### PERSONALE ###
      staffTabPanelUI(id = "staff"),
      
      ### DATAKILDER ###
      datasourcesTabPanelUI(id = "datasources")
    )
  )
)