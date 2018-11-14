function(request) {
  
# Libraries
source("global.R")

# Moduler
source("modules.R")

# Functioner
source("functions.R")

# Sections (Modules)
source("./sections/acquisition.R")
source("./sections/datasources.R")
source("./sections/eressources.R")
source("./sections/edatabases.R")
source("./sections/events.R")
source("./sections/eventareas.R")
source("./sections/frontpage.R")
source("./sections/materials.R")
source("./sections/indoor_climate.R")
source("./sections/inventory.R")
source("./sections/meetingrooms.R")
source("./sections/online_odensebib.R")
source("./sections/staff.R")
source("./sections/users.R")
source("./sections/visitors.R")

#  Dashboard Layout
dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Whitebook",
    tags$li(class = "dropdown",
            tags$li(bookmarkButton(label = "Bogmærk", icon = shiny::icon("link", lib = "glyphicon"), title = "Bogmærk siden med nuværende indstillinger"))
            )
  ),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
      menuItem("Forside", tabName = "frontpage", icon = icon("home", lib="font-awesome")),
      menuItem("Arrangementer", tabName = "events", icon = icon("calendar", lib="font-awesome")),
      menuItem("Det fysiske rum", tabName = "space", icon = icon("building", lib="font-awesome"),
               menuItem("Besøgende", tabName = "visits"),
               menuItem("Mødelokaler", tabName = "meetingrooms"),
               menuItem("Event områder", tabName = "eventareas"),
               menuItem("Indeklima", tabName = "indoor_climate") 
      ),
      menuItem("Online", tabName = "online", icon = icon("laptop", lib="font-awesome"), 
               menuItem("Odensebib.dk", tabName = "odensebib")#,
               #menuItem("Biblioteket App", tabName = "app")
      ),
      menuItem("Materialer & udlån", tabName = "pmat", icon = icon("book", lib="font-awesome"),
               menuItem("Udlån", tabName = "physicalmat"),
               #menuItem("Materialeindkøb", tabName = "acquisition"),
               menuItem("Beholdning", tabName = "inventory")
      ),
      menuItem("E-Ressourcer", tabName = "emat", icon = icon("database", lib="font-awesome"),
               #menuItem("E-Bøger", tabName = "ebooks"),
               #menuItem("E-Film", tabName = "emovies"),
               menuItem("E-Baser", tabName = "edatabases")
               #menuItem("Licenser", tabName = "licenses")
      ),
      menuItem("Brugere", tabName = "users", icon = icon("users", lib="font-awesome")),
      menuItem("Personale", tabName = "personal", icon = icon("users", lib="font-awesome")),
      menuItem("Datakilder", tabName = "datasources", icon = icon("database", lib="font-awesome"))#,
      #menuItem("Test", tabName = "test")
    )
  ),
  
#  Dashboard Content 
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      #tags$link(rel = "stylesheet", type = "text/css", href = "plotprint.css", media="print"),
      tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
      #includeCSS("www/plotprint.css"),
      tags$head(tags$script(src="hacks.js")),
      tags$head(tags$style(HTML('@media print { .plotteren { width: 500px; } }')))
    ),
    tabItems(
      # Frontpage  
      frontpageTabPanelUI(id = "frontpage"),  
      # Events  
      eventsTabPanelUI(id = "events"),  
      # Space Visitors
      visitorsTabPanelUI(id = "visitors"),    
      # Space Meetings Rooms
      meetingroomsTabPanelUI(id = "meetingrooms"), 
      # Space Event Areas #
      eventareasTabPanelUI(id = "eventareas"), 
      # Online Odensebib.dk
      online_odensebibTabPanelUI(id = "online_odensebib"),  
      # Materials Circulation 
      materialsTabPanelUI(id = "materials"),
      # Materials Acquisition
      acquisitionTabPanelUI(id = "acquisition"),
      # Materials Inventory
      inventoryTabPanelUI(id = "inventory"),
      # Indoor Climate
      indoor_climateTabPanelUI(id = "indoor_climate"),
      # E-Ressources
      eressourcesTabPanelUI(id = "eressources"),
      # Users
      edatabasesTabPanelUI(id = "edatabases"),
      # Users
      usersTabPanelUI(id = "users"),
      # Staff
      staffTabPanelUI(id = "staff"),
      # Datasources
      datasourcesTabPanelUI(id = "datasources")#,
      # tabItem(tabName = "test",
      # )
    )
  )
)

}