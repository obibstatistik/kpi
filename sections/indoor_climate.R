source("global.R")
source("functions.R")
source("modules.R")
source("~/.postpass") 

### Deviceplot MODUL ###

# Modul UI
device_plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(3,
           htmlOutput(ns("sensors_room_title")),
           plotlyOutput(ns("sensors_plot"))
    )
  )
}

# Modul SERVER
sensors_plot <- function(input, output, session, data, device, room, sensor, limits, ticksuffix) {
  
  # Af en eller anden grund skal device kaldes eksplicit her, ellers vil der kun blive outputtet
  # grafer fra een enkelt device (den sidste) uanset at der inputtes forskellige device id'er
  # når denne funktion kaldes længere nede. Dvs. det er ikke nok at device er tilstede i funktionens parameter-variable ovenfor.
  # Det må være en sær scoping/caching/optimerings/reactive issue...
  device
  room
  
  # Titlen på chartet (har brugt html output, da chart-titlen i plot_ly() er svær at styre)
  output$sensors_room_title <- renderUI({
    x <- paste0("<h4>",room,"<h4>")
    HTML(x)
  })
  
  # Selve grafen
  output$sensors_plot <- renderPlotly({
    data <- data %>%
      select_("device_id","realtime","dato","hour",.dots = sensor) %>%
      filter(device_id == device) %>%
      group_by(device_id,dato,hour) %>%
      summarise_at(sensor,mean) %>%
      spread_("dato",sensor) %>%
      mutate(avg=rowMeans(.[-1:-2])) %>%    # .[-1,-2] betyder, at man tager alle columns bortset fra nr. 1 til nr. 2 (dvs. en range)
      select(avg,everything()) # move averages to first column, so we can have an open-ended/dynamic number of measured data columns for later use
    
    # Tilføj måledagenes graflinjer:
    p <- plot_ly(data, x = ~hour, y = as.formula(paste0("~`", names(data)[4], "`")), type = 'scatter', line = list(color = 'rgb(210,210,210)', width = 3), mode = 'lines')
    for(colname in names(data)[-1:-4]){
      p <- p %>% 
        add_trace(y = as.formula(paste0("~`", colname, "`")), name = colname, line = list(color = 'rgb(220,220,220)', width = 3), mode = 'lines')   
    }
    # Tilføj gennemsnittets graflinje:
    p <- p %>% 
      add_trace(y = ~avg, line = list(color = 'rgb(0,0,0)', width = 4), mode = 'lines')
    # Tilføj snorhøjder (limits) hvis der er nogen:
    if ( length(limits) != 0) {
      for(limit in limits){
        p <- p %>% 
          add_trace(y = limit, line = list(color = 'rgb(100,100,100)', width = 1, dash = 'dash'), mode = 'lines')   
      }
    }
    # Switch/case to distinguish between sensors to determine which y-axis range to use
    yaxis_range <- function(sensor) {
      switch(sensor,
             temperature = c(15,35),
             co2 = c(300,1100),
             humidity = c(20,70),
             noise_avg = c(20,65))
    }
    yaxis_step <- function(sensor) {
      switch(sensor,
             temperature = 2,
             co2 = 100,
             humidity = 5,
             noise_avg = 10)
      }
      p %>% layout(title = '', xaxis = list(title = 'timer på dagen'), yaxis = list (range = yaxis_range(sensor), title = '', dtick = yaxis_step(sensor), ticksuffix = ticksuffix), showlegend = FALSE)
  })
  
}

# Hent data fra DB:
fromdate <- paste0(Sys.Date()-8)
todate <- paste0(Sys.Date()-1)
select_stmt <- paste0("select * from smartcity.icmeter where substr(realtime::text,1,10) between '",fromdate,"' and '",todate,"';")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
sensors <- dbGetQuery(con, select_stmt)
dbDisconnect(con)

sensors_pr_device <- sensors %>%
  select(room,device_id,realtime,temperature,humidity,co2,noise_avg,noise_peak) %>%
  # Gotta input timezone since dates have datatype 'timestamp with timezone'
  mutate(dato = as.Date(realtime,tz="Europe/Copenhagen")) %>%
  mutate(hour = format(realtime,'%H',tz="Europe/Copenhagen"))

# create small df for looping through when calling the module's function and ui
device_ids = sensors %>% distinct(device_id,room)



indoor_climateTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "indoor_climate",
          
          box(width = 12, solidHeader = TRUE, id="spaceheader4",
              h3("Indeklima"),
              img(src='icons/detfysiskerrum_negativ_45x45.png', align = "right", height="46px")
          ),
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset22",
                          tabPanel("Temperatur", 
                                   fluidRow(
                                     column(12,
                                            h3("Temperatur i Borgernes Hus"),
                                            h4(paste0("Gennemsnit af målinger mellem ", fromdate, " og ", todate)),
                                            tags$br(),
                                            p("Nedenstående visualiseringer viser temperaturmålinger fra IC-meter enheder i Borgernes Hus over de senste otte dage, fordelt over timer på dagen"),
                                            p("Den kraftige sorte linje viser temperaturgennemsnittet for de otte dage og de grå linjer er de egentlige målinger, således at maximum og minimumværdier kan aflæses"),
                                            p("De stiplede snorhøjder på 21°C og 25°C angiver det temperaturvindue, som målingerne ifølge SDU bør ligge indenfor"),
                                            tags$br(),tags$br(),tags$br(),

                                            device_plotUI(ns(id = "temp-1BE1CA2D")), # 1BE1CA2D var forbyttet navnemæssigt frem til 13-08-2018 kl. ca. 15, dvs. at den var Oplevelse og ikke Viden/Info
                                            device_plotUI(ns(id = "temp-446AA82F")), # 446AA82F var "Magasinet" (kunne dog ikke levere data) frem til 13-08-2018 kl. ca. 15, derfra er den Viden og Info.
                                            #device_plotUI(ns(id = "temp-879D5B2C")), # 879D5B2C var forbyttet navnemæssigt frem til samme datotid. dvs. at den var Viden/Info og ikke Oplevelse
                                            device_plotUI(ns(id = "temp-8CAE312E")),
                                            device_plotUI(ns(id = "temp-20F2A02F")),
                                            device_plotUI(ns(id = "temp-F358EC2B")),
                                            device_plotUI(ns(id = "temp-DDBFED32"))
                                     )
                                   )
                          ),
                          tabPanel("Luftkvalitet (CO2)", 
                                   fluidRow(
                                     column(12,
                                            h3("Luftkvalitet i Borgernes Hus"),
                                            h4(paste0("Gennemsnit af målinger mellem ", fromdate, " og ", todate)),
                                            tags$br(),
                                            p("Nedenstående visualiseringer viser CO2-målinger fra IC-meter enheder i Borgernes Hus over de senste otte dage, fordelt over timer på dagen"),
                                            p("Målingerne er foretaget i ppm (parts per million)"),
                                            p("Den kraftige sorte linje viser temperaturgennemsnittet for de otte dage og de grå linjer er de egentlige målinger, således at maximum og minimumværdier kan aflæses"),
                                            p("Den stiplede snorhøjde på 1.000ppm angiver den mængde CO2, som ifølge SDU maximalt bør være til stede"),
                                            tags$br(),tags$br(),tags$br(),

                                            device_plotUI(ns(id = "co2-1BE1CA2D")),
                                            device_plotUI(ns(id = "co2-446AA82F")),
                                            #device_plotUI(ns(id = "co2-879D5B2C")),
                                            device_plotUI(ns(id = "co2-8CAE312E")),
                                            device_plotUI(ns(id = "co2-20F2A02F")),
                                            device_plotUI(ns(id = "co2-F358EC2B")),
                                            device_plotUI(ns(id = "co2-DDBFED32"))
                                     )
                                   )
                          ),
                          tabPanel("Luftfugtighed", 
                                   fluidRow(
                                     column(12,
                                            h3("Luftfugtighed i Borgernes Hus"),
                                            h4(paste0("Gennemsnit af målinger mellem ", fromdate, " og ", todate)),
                                            tags$br(),
                                            p("Nedenstående visualiseringer viser målinger af luftfugtighed fra IC-meter enheder i Borgernes Hus over de senste otte dage, fordelt over timer på dagen"),
                                            p("Den kraftige sorte linje viser gennemsnittet for de otte dage og de grå linjer er de egentlige målinger, således at maximum og minimumværdier kan aflæses"),
                                            tags$br(),tags$br(),tags$br(),

                                            device_plotUI(ns(id = "fugt-1BE1CA2D")),
                                            device_plotUI(ns(id = "fugt-446AA82F")),
                                            #device_plotUI(ns(id = "fugt-879D5B2C")),
                                            device_plotUI(ns(id = "fugt-8CAE312E")),
                                            device_plotUI(ns(id = "fugt-20F2A02F")),
                                            device_plotUI(ns(id = "fugt-F358EC2B")),
                                            device_plotUI(ns(id = "fugt-DDBFED32"))
                                     )
                                   )
                          ),
                          tabPanel("Lydniveau", 
                                   fluidRow(
                                     column(12,
                                            h3("Lydniveau i Borgernes Hus"),
                                            h4(paste0("Gennemsnit af målinger mellem ", fromdate, " og ", todate)),
                                            tags$br(),
                                            p("Nedenstående visualiseringer viser målinger af gennemsnitligt lydniveau fra IC-meter enheder i Borgernes Hus over de senste otte dage, fordelt over timer på dagen"),
                                            p("Den kraftige sorte linje viser gennemsnittet for de otte dage og de grå linjer er de egentlige målinger, således at maximum og minimumværdier kan aflæses"),
                                            tags$br(),tags$br(),tags$br(),

                                            device_plotUI(ns(id = "lyd-1BE1CA2D")),
                                            device_plotUI(ns(id = "lyd-446AA82F")),
                                            #device_plotUI(ns(id = "lyd-879D5B2C")),
                                            device_plotUI(ns(id = "lyd-8CAE312E")),
                                            device_plotUI(ns(id = "lyd-20F2A02F")),
                                            device_plotUI(ns(id = "lyd-F358EC2B")),
                                            device_plotUI(ns(id = "lyd-DDBFED32"))
                                     )
                                   )
                          ),
                          tabPanel("Data og dokumentation",
                                   fluidRow(
                                     column(12,
                                            p("Dokumentation")
                                     )
                                   )  
                          )
                   ))))
}

indoor_climateTabPanel <- function(input, output, session, data, tablename) {
  
  # Run the plot module for all devices and each of their four sensors:
  for(row in 1:nrow(device_ids)){
    callModule(sensors_plot, id = paste0("temp-",device_ids[row,"device_id"]), data = sensors_pr_device, device = device_ids[row,"device_id"], room = device_ids[row,"room"], sensor = 'temperature', ticksuffix = '°C ', limits = c(21,25))
    callModule(sensors_plot, id = paste0("co2-",device_ids[row,"device_id"]), data = sensors_pr_device, device =  device_ids[row,"device_id"], room = device_ids[row,"room"], sensor = 'co2', ticksuffix = 'ppm ', limits = 1000)
    callModule(sensors_plot, id = paste0("fugt-",device_ids[row,"device_id"]), data = sensors_pr_device, device = device_ids[row,"device_id"], room = device_ids[row,"room"], sensor = 'humidity', ticksuffix = '% ', limits = '')
    callModule(sensors_plot, id = paste0("lyd-",device_ids[row,"device_id"]), data = sensors_pr_device, device =  device_ids[row,"device_id"], room = device_ids[row,"room"], sensor = 'noise_avg', ticksuffix = 'dBa ', limits = 60)
  }
  
}
