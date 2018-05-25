# Libraries
source("global.R")

# Modules
source("modules.R")

# Sections (Modules)
source("./sections/acquisition.R")
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

shinyServer(function(input, output) {

  source("~/.postpass")
  
  ### DB QUERIES ###
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  datasources_schema <- (dbGetQuery(con, "SELECT columns.table_name as name, columns.column_name, columns.data_type,columns.column_default, columns.is_nullable FROM information_schema.columns;"))
  dbDisconnect(con)
  
  ### COLORS ###
  colors <- c('rgb(70,140,140)', 'rgb(174,176,81)', 'rgb(59,54,74)', 'rgb(192,57,83)', 'rgb(29,114,170)', 'rgb(225,123,81)', 'rgb(219,181,61)')
  color1 = c('rgb(70,140,140)')
  color2 = c('rgb(174,176,81)')
  color3 = c('rgb(59,54,74)')
  color4 = c('rgb(192,57,83)')
  color5 = c('rgb(29,114,170)')
  color6 = c('rgb(225,123,81)')
  color7 = c('rgb(219,181,61)')
  
  ### DATES ###
  year <- as.integer(format(Sys.Date(), "%Y"))
  month <- as.integer(format(Sys.Date(), "%M"))
  day <- as.integer(format(Sys.Date(), "%Y"))
  
  ### MODULES ###
  callModule(metaTabPanel, id = "arrangementer", data = datasources_schema, tablename = "arrangementer")
  callModule(metaTabPanel, id = "people_counter", data = datasources_schema, tablename = "people_counter")
  callModule(metaTabPanel, id = "visitor_counter", data = datasources_schema, tablename = "visitor_counter")
  
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
  # E-Ressources 
  callModule(eressourcesTabPanel, id = "eressources")
  # Users
  callModule(usersTabPanel, id = "users")
  # Staff
  callModule(staffTabPanel, id = "staff")
  # Datasources
  callModule(datasourcesTabPanel, id = "datasources")
  
})