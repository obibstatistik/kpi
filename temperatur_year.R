source("global.R")
source("functions.R")
source("modules.R")
source("~/.postpass") 





# Hent data fra DB:
fromdate <- '2018-01-01' #paste0(Sys.Date()-8)
todate <- paste0(Sys.Date()-1)
select_stmt <- paste0("select * from smartcity.icmeter where substr(realtime::text,1,10) between '",fromdate,"' and '",todate,"';")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
sensors_year <- dbGetQuery(con, select_stmt)
dbDisconnect(con)

sensors_year <- sensors_year %>%
  select(room,device_id,realtime,temperature,humidity,co2,noise_avg,noise_peak) %>%
  # Gotta input timezone since dates have datatype 'timestamp with timezone'
  mutate(dato = as.Date(realtime,tz="Europe/Copenhagen")) %>%
  mutate(hour = format(realtime,'%H',tz="Europe/Copenhagen"))

# create small df for looping through when calling the module's function and ui
device_ids = sensors %>% distinct(device_id,room)