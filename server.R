# Libraries
source("global.R")

shinyServer(function(input, output) {
  
  # Modules
  source("modules.R")
  
  # Sections (Modules)
  source("./sections/acquisition.R")
  source("./sections/datasources.R")
  source("./sections/eressources.R")
  source("./sections/edatabases.R")
  source("./sections/events.R")
  source("./sections/eventareas.R")
  source("./sections/frontpage.R")
  source("./sections/materials.R")
  source("./sections/inventory.R")
  source("./sections/meetingrooms.R")
  source("./sections/indoor_climate.R")
  source("./sections/online_odensebib.R")
  source("./sections/staff.R")
  source("./sections/users.R")
  source("./sections/visitors.R")
  
  source("~/.postpass")
  
  ### DB QUERIES ###
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  #datasources_schema <- (dbGetQuery(con, "SELECT columns.table_name as name, columns.column_name, columns.data_type,columns.column_default, columns.is_nullable FROM information_schema.columns;"))
  dbDisconnect(con)
  

  
  ### DATES ###
  year <- as.integer(format(Sys.Date(), "%Y"))
  month <- as.integer(format(Sys.Date(), "%M"))
  day <- as.integer(format(Sys.Date(), "%Y"))
  
  ### MODULES ###
  #callModule(metaTabPanel, id = "people_counter", data = datasources_schema, tablename = "people_counter")
  #callModule(metaTabPanel, id = "visitor_counter", data = datasources_schema, tablename = "visitor_counter")
  
  # Frontpage
  callModule(frontpageTabPanel, id = "frontpage")
  # Events 
  callModule(eventsTabPanel, id = "events")
  # Space Visitors
  callModule(visitorsTabPanel, id = "visitors")
  # Space Meetings Rooms
  callModule(meetingroomsTabPanel, id = "meetingrooms")
  # Space Event Areas #
  callModule(eventareasTabPanel, id = "eventareas")
  # Online Odensebib.dk
  callModule(online_odensebibTabPanel, id = "online_odensebib")
  # Materials Circulation
  callModule(materialsTabPanel, id = "materials")
  # Materials Acquisition
  callModule(acquisitionTabPanel, id = "acquisition")
  # Materials Inventory
  callModule(inventoryTabPanel, id = "inventory")
  # Indoor climate 
  callModule(indoor_climateTabPanel, id = "indoor_climate")
  # E-Ressources 
  callModule(eressourcesTabPanel, id = "eressources")
  # E-Databases 
  callModule(edatabasesTabPanel, id = "edatabases")
  # Users
  callModule(usersTabPanel, id = "users")
  # Staff
  callModule(staffTabPanel, id = "staff")
  # Datasources
  callModule(datasourcesTabPanel, id = "datasources")
  
})