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
source("./sections/users.R", local = TRUE)
source("./sections/visitors.R")

shinyServer(function(input, output) {

  source("~/.postpass")
  
  ### DB QUERIES ###
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)

  #sqlloan <- dbGetQuery(con, "SELECT * FROM datamart.kpi_loan")
  
  ereolentype <- dbGetQuery(con, "SELECT type, count(type) FROM public.ereolen group by type")
  ereolenhist <- dbGetQuery(con, "select to_char(dato, 'iyyy-iw') as date, count(type = 'Lydbog') as lydbog, count(type = 'E-bog') as ebog from public.ereolen group by date;")
  #ereolenalder <- dbGetQuery(con, "select extract(year from date_trunc('year',age(birth))) as alder, count(extract(year from date_trunc('year',age(birth)))) as antal, (case when mod((substring(laanernummer from 10 for 1))::integer,2) = 1 then 'mand' else 'kvinde' end) as sex from public.ereolen join public.patron on public.patron.patronno = laanernummer group by alder, sex;")
  
  datasources_schema <- (dbGetQuery(con, "SELECT columns.table_name as name, columns.column_name, columns.data_type,columns.column_default, columns.is_nullable FROM information_schema.columns;"))
  testdata <- dbGetQuery(con, "select * from datamart.whitebook_test_1 where substr(date::text,1,4) in ('2018','2017','2016','2015','2014');")
  
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
  
  ### LOCATIONS ###
  
  
  ### MODULES ###
  callModule(metaTabPanel, id = "arrangementer", data = datasources_schema, tablename = "arrangementer")
  callModule(metaTabPanel, id = "people_counter", data = datasources_schema, tablename = "people_counter")
  
  ### FRONTPAGE ### 
  callModule(frontpageTabPanel, id = "frontpage")
  
  ### EVENTS ### 
  callModule(eventsTabPanel, id = "events")
  
  ### FYSISKE RUM ###
  callModule(visitorsTabPanel, id = "visitors")
  
  #meetingrooms
  callModule(meetingroomsTabPanel, id = "meetingrooms")

  #bhus_events
  callModule(eventareasTabPanel, id = "eventareas")
  
  ### ONLINE ###
  callModule(online_odensebibTabPanel, id = "online_odensebib")
  
  ### MATERIALS ###
  callModule(materialsTabPanel, id = "materials")

  ### E-RESSOURCES ### 
  callModule(eressourcesTabPanel, id = "eressources")
  
  ### USERS ###
  callModule(usersTabPanel, id = "users")
  
  ### STAFF ###
  callModule(staffTabPanel, id = "staff")
  
  ### DATASOURCES ### 
  callModule(datasourcesTabPanel, id = "datasources")
  
})