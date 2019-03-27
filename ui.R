function(request) {
  
# Global sourced
source("global.R")
source("modules.R")
source("functions.R")
source("~/.postpass")
source("~/.postpass_dwh")
  
# Sections (Modules) 
source("./sections/datasources.R", local = TRUE)
source("./sections/edatabases.R", local = TRUE)
source("./sections/events.R", local = TRUE)
source("./sections/eventareas.R", local = TRUE)
source("./sections/frontpage.R", local = TRUE)
source("./sections/materials.R", local = TRUE)
source("./sections/indoor_climate.R", local = TRUE)
source("./sections/inventory.R", local = TRUE)
source("./sections/meetingrooms.R", local = TRUE)
source("./sections/online_odensebib.R", local = TRUE)
source("./sections/staff.R", local = TRUE)
source("./sections/users.R", local = TRUE)
source("./sections/citizenservice.R", local = TRUE)
source("./sections/visitors.R", local = TRUE)

# Spinner options  
options(spinner.size=0.75)  
options(spinner.color="#555555")
options(spinner.type=1)
    
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
               menuItem("Licenser", tabName = "edatabases")
               #menuItem("Licenser", tabName = "licenses")
      ),
      menuItem("Brugere", tabName = "users", icon = icon("users", lib="font-awesome")),
      menuItem("Personale", tabName = "personal", icon = icon("users", lib="font-awesome")),
      menuItem("Borgerservice", tabName = "citizenservice", icon = icon("ban", lib="font-awesome")),
      menuItem("Datakilder", tabName = "datasources", icon = icon("database", lib="font-awesome"))#,
      #menuItem("Test", tabName = "test")
    )
  ),
  
#  Dashboard Content 
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "https://use.fontawesome.com/releases/v5.6.3/css/all.css", integrity="sha384-UHRtZLI+pbxtHCWp1t77Bi1L4ZtiqrqD80Kn4Z8NTSRyMA2Fd33n5dQ8lWUE00s/", crossorigin="anonymous"),
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
      #acquisitionTabPanelUI(id = "acquisition"),
      # Materials Inventory
      inventoryTabPanelUI(id = "inventory"),
      # Indoor Climate
      indoor_climateTabPanelUI(id = "indoor_climate"),
      # E-Ressources
      #eressourcesTabPanelUI(id = "eressources"),
      # Users
      edatabasesTabPanelUI(id = "edatabases"),
      # Users
      usersTabPanelUI(id = "users"),
      # Staff
      staffTabPanelUI(id = "staff"),
      # Citizen Service
      citizenserviceTabPanelUI(id = "citizenservice"),
      # Datasources
      datasourcesTabPanelUI(id = "datasources")#,
      # tabItem(tabName = "test",
      # )
    )
  )
)

}