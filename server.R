shinyServer(function(input, output) {
  
  # Sections (Modules)
  #source("./sections/acquisition.R")
  source("./sections/datasources.R", local = TRUE)
  #source("./sections/eressources.R")
  source("./sections/edatabases.R", local = TRUE)
  source("./sections/events.R", local = TRUE)
  source("./sections/eventareas.R", local = TRUE)
  source("./sections/frontpage.R", local = TRUE)
  source("./sections/materials.R", local = TRUE)
  source("./sections/inventory.R", local = TRUE)
  source("./sections/meetingrooms.R", local = TRUE)
  source("./sections/indoor_climate.R", local = TRUE)
  source("./sections/online_odensebib.R", local = TRUE)
  source("./sections/staff.R", local = TRUE)
  source("./sections/users.R", local = TRUE)
  source("./sections/visitors.R", local = TRUE)
  source("./sections/citizenservice.R", local = TRUE)

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
  #callModule(acquisitionTabPanel, id = "acquisition")
  # Materials Inventory
  callModule(inventoryTabPanel, id = "inventory")
  # Indoor climate 
  callModule(indoor_climateTabPanel, id = "indoor_climate")
  # E-Ressources 
  #callModule(eressourcesTabPanel, id = "eressources")
  # E-Databases 
  callModule(edatabasesTabPanel, id = "edatabases")
  # Users
  callModule(usersTabPanel, id = "users")
  # Staff
  callModule(staffTabPanel, id = "staff")
  # Citizenservice
  callModule(citizenserviceTabPanel, id = "citizenservice")
  # Datasources
  callModule(datasourcesTabPanel, id = "datasources")
  
})