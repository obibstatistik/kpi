source("global.R")
source("functions.R")
source("modules.R")
source("~/.postpass") 

### Deviceplot MODUL ###

# UI
device_plotUI <- function(id,device) {
  ns <- NS(id)
  tagList(
    column(3,
        p(device),
        plotlyOutput(ns("sensors_plot"))
    )
  )
}

# SERVER
sensors_plot <- function(input, output, session, data, device, sensor, limits, ticksuffix) {
  
  output$sensors_plot <- renderPlotly({
    data <- data %>%
      select_("device_id","realtime","dato","hour",.dots = sensor) %>%
      filter(device_id == device) %>%
      group_by(device_id,dato,hour) %>%
      summarise_at(sensor,mean) %>%
      spread_("dato",sensor) %>%
      mutate(avg=rowMeans(.[-1:-2]))
    
    # Tilføj måledagenes graflinjer:
    p <- plot_ly(data, x = ~hour, y = as.formula(paste0("~`", names(data)[3], "`")), type = 'scatter', line = list(color = 'rgb(210,210,210)', width = 3), mode = 'lines')
    for(colname in names(data)[4:10]){
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
    p %>% layout(xaxis = list(title = 'timer på dagen'), yaxis = list (title = '', ticksuffix = ticksuffix), showlegend = FALSE)
  })
  
}

# Hent data fra DB:
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
sensors <- dbGetQuery(con, "select *
                      from smartcity.icmeter
                      where substr(realtime::text,1,10) between '2018-07-19' and '2018-07-26'
                      ;")
dbDisconnect(con)


indoor_climateTabPanelUI <- function(id) {

ns <- NS(id)

tabItem(tabName = "indoor_climate",
        
        box(width = 12, solidHeader = TRUE, id="indoor_climate",
            h3("Indeklima"),
            img(src='icons/materialer_negativ_45x45.png', align = "right", height="46px")
        ),
        fluidRow(
          column(12,
                 tabBox(width = 12,
                        id = "tabset22",
                        tabPanel("Temperatur", 
                                 fluidRow(
                                   column(12,
                                      h4("Temperatur i Borgernes Hus"),
                                      p("Nedenstående visualiseringer viser temperaturmålinger fra IC-meter enheder i Borgernes Hus over de senste otte dage, fordelt over timer på dagen"),
                                      p("Den kraftige sorte linje viser temperaturgennemsnittet for de otte dage og de grå linjer er de egentlige målinger, således at maximum og minimumværdier kan aflæses"),
                                      p("De stiplede snorhøjder på 21°C og 25°C angiver det temperaturvindue, som målingerne ifølge SDU bør ligge indenfor"),
                                      tags$br(),tags$br(),tags$br(),
                                      device_plotUI(ns(id = "temp-1BE1CA2D"),device = "1BE1CA2D"),
                                      device_plotUI(ns(id = "temp-879D5B2C"),device = "879D5B2C"),
                                      device_plotUI(ns(id = "temp-8CAE312E"),device = "8CAE312E"),
                                      device_plotUI(ns(id = "temp-20F2A02F"),device = "20F2A02F"),
                                      device_plotUI(ns(id = "temp-F358EC2B"),device = "F358EC2B"),
                                      device_plotUI(ns(id = "temp-DDBFED32"),device = "DDBFED32")
                                   )
                                 )
                        ),
                        tabPanel("Luftkvalitet (CO2)", 
                                 fluidRow(
                                   column(12,
                                      h4("Luftkvalitet i Borgernes Hus"),
                                      p("Nedenstående visualiseringer viser CO2-målinger fra IC-meter enheder i Borgernes Hus over de senste syv dage, fordelt over timer på dagen"),
                                      p("Målingerne er foretaget i ppm (parts per million)"),
                                      p("Den kraftige sorte linje viser temperaturgennemsnittet for de 7 dage og de grå linjer er de egentlige målinger, således at maximum og minimumværdier kan aflæses"),
                                      p("Den stiplede snorhøjde på 1.000ppm angiver den mængde CO2, som ifølge SDU maximalt bør være til stede"),
                                      tags$br(),tags$br(),tags$br(),
                                      device_plotUI(ns(id = "co2-1BE1CA2D"),device = "1BE1CA2D"),
                                      device_plotUI(ns(id = "co2-879D5B2C"),device = "879D5B2C"),
                                      device_plotUI(ns(id = "co2-8CAE312E"),device = "8CAE312E"),
                                      device_plotUI(ns(id = "co2-20F2A02F"),device = "20F2A02F"),
                                      device_plotUI(ns(id = "co2-F358EC2B"),device = "F358EC2B"),
                                      device_plotUI(ns(id = "co2-DDBFED32"),device = "DDBFED32")
                                   )
                                 )
                        ),
                        tabPanel("Luftfugtighed", 
                                 fluidRow(
                                   column(12,
                                      h4("Luftfugtighed i Borgernes Hus"),
                                      p("Nedenstående visualiseringer viser målinger af luftfugtighed fra IC-meter enheder i Borgernes Hus over de senste syv dage, fordelt over timer på dagen"),
                                      p("Den kraftige sorte linje viser gennemsnittet for de 7 dage og de grå linjer er de egentlige målinger, således at maximum og minimumværdier kan aflæses"),
                                      tags$br(),tags$br(),tags$br(),
                                      device_plotUI(ns(id = "fugt-1BE1CA2D"),device = "1BE1CA2D"),
                                      device_plotUI(ns(id = "fugt-879D5B2C"),device = "879D5B2C"),
                                      device_plotUI(ns(id = "fugt-8CAE312E"),device = "8CAE312E"),
                                      device_plotUI(ns(id = "fugt-20F2A02F"),device = "20F2A02F"),
                                      device_plotUI(ns(id = "fugt-F358EC2B"),device = "F358EC2B"),
                                      device_plotUI(ns(id = "fugt-DDBFED32"),device = "DDBFED32")
                                   )
                                 )
                        ),
                        tabPanel("Lydniveau", 
                                 fluidRow(
                                   column(12,
                                      h4("Lydniveau i Borgernes Hus"),
                                      p("Nedenstående visualiseringer viser målinger af gennemsnitligt lydniveau fra IC-meter enheder i Borgernes Hus over de senste syv dage, fordelt over timer på dagen"),
                                      p("Den kraftige sorte linje viser gennemsnittet for de 7 dage og de grå linjer er de egentlige målinger, således at maximum og minimumværdier kan aflæses"),
                                      tags$br(),tags$br(),tags$br(),
                                      device_plotUI(ns(id = "lyd-1BE1CA2D"),device = "1BE1CA2D"),
                                      device_plotUI(ns(id = "lyd-879D5B2C"),device = "879D5B2C"),
                                      device_plotUI(ns(id = "lyd-8CAE312E"),device = "8CAE312E"),
                                      device_plotUI(ns(id = "lyd-20F2A02F"),device = "20F2A02F"),
                                      device_plotUI(ns(id = "lyd-F358EC2B"),device = "F358EC2B"),
                                      device_plotUI(ns(id = "lyd-DDBFED32"),device = "DDBFED32")
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
  
  sensors_pr_device <- sensors %>%
    # Gotta input timezone since dates have datatype 'timestamp with timezone'
    mutate(dato = as.Date(realtime,tz="Europe/Copenhagen")) %>%
    mutate(hour = format(realtime,'%H',tz="Europe/Copenhagen"))

  callModule(sensors_plot, id = 'temp-1BE1CA2D', data = sensors_pr_device, device = '1BE1CA2D', sensor = 'temperature', ticksuffix = '°C ', limits = c(21,25))
  callModule(sensors_plot, id = 'temp-879D5B2C', data = sensors_pr_device, device = '879D5B2C', sensor = 'temperature', ticksuffix = '°C ', limits = c(21,25))
  callModule(sensors_plot, id = 'temp-8CAE312E', data = sensors_pr_device, device = '8CAE312E', sensor = 'temperature', ticksuffix = '°C ', limits = c(21,25))
  callModule(sensors_plot, id = 'temp-20F2A02F', data = sensors_pr_device, device = '20F2A02F', sensor = 'temperature', ticksuffix = '°C ', limits = c(21,25))
  callModule(sensors_plot, id = 'temp-F358EC2B', data = sensors_pr_device, device = 'F358EC2B', sensor = 'temperature', ticksuffix = '°C ', limits = c(21,25))
  callModule(sensors_plot, id = 'temp-DDBFED32', data = sensors_pr_device, device = 'DDBFED32', sensor = 'temperature', ticksuffix = '°C ', limits = c(21,25))

  callModule(sensors_plot, id = 'co2-1BE1CA2D', data = sensors_pr_device, device = '1BE1CA2D', sensor = 'co2', ticksuffix = 'ppm ', limits = 1000)
  callModule(sensors_plot, id = 'co2-879D5B2C', data = sensors_pr_device, device = '879D5B2C', sensor = 'co2', ticksuffix = 'ppm ', limits = 1000)
  callModule(sensors_plot, id = 'co2-8CAE312E', data = sensors_pr_device, device = '8CAE312E', sensor = 'co2', ticksuffix = 'ppm ', limits = 1000)
  callModule(sensors_plot, id = 'co2-20F2A02F', data = sensors_pr_device, device = '20F2A02F', sensor = 'co2', ticksuffix = 'ppm ', limits = 1000)
  callModule(sensors_plot, id = 'co2-F358EC2B', data = sensors_pr_device, device = 'F358EC2B', sensor = 'co2', ticksuffix = 'ppm ', limits = 1000)
  callModule(sensors_plot, id = 'co2-DDBFED32', data = sensors_pr_device, device = 'DDBFED32', sensor = 'co2', ticksuffix = 'ppm ', limits = 1000)
  
  callModule(sensors_plot, id = 'fugt-1BE1CA2D', data = sensors_pr_device, device = '1BE1CA2D', sensor = 'humidity', ticksuffix = '% ', limits = '')
  callModule(sensors_plot, id = 'fugt-879D5B2C', data = sensors_pr_device, device = '879D5B2C', sensor = 'humidity', ticksuffix = '% ', limits = '')
  callModule(sensors_plot, id = 'fugt-8CAE312E', data = sensors_pr_device, device = '8CAE312E', sensor = 'humidity', ticksuffix = '% ', limits = '')
  callModule(sensors_plot, id = 'fugt-20F2A02F', data = sensors_pr_device, device = '20F2A02F', sensor = 'humidity', ticksuffix = '% ', limits = '')
  callModule(sensors_plot, id = 'fugt-F358EC2B', data = sensors_pr_device, device = 'F358EC2B', sensor = 'humidity', ticksuffix = '% ', limits = '')
  callModule(sensors_plot, id = 'fugt-DDBFED32', data = sensors_pr_device, device = 'DDBFED32', sensor = 'humidity', ticksuffix = '% ', limits = '')
  
  callModule(sensors_plot, id = 'lyd-1BE1CA2D', data = sensors_pr_device, device = '1BE1CA2D', sensor = 'noise_avg', ticksuffix = 'dBa ', limits = 60)
  callModule(sensors_plot, id = 'lyd-879D5B2C', data = sensors_pr_device, device = '879D5B2C', sensor = 'noise_avg', ticksuffix = 'dBa ', limits = 60)
  callModule(sensors_plot, id = 'lyd-8CAE312E', data = sensors_pr_device, device = '8CAE312E', sensor = 'noise_avg', ticksuffix = 'dBa ', limits = 60)
  callModule(sensors_plot, id = 'lyd-20F2A02F', data = sensors_pr_device, device = '20F2A02F', sensor = 'noise_avg', ticksuffix = 'dBa ', limits = 60)
  callModule(sensors_plot, id = 'lyd-F358EC2B', data = sensors_pr_device, device = 'F358EC2B', sensor = 'noise_avg', ticksuffix = 'dBa ', limits = 60)
  callModule(sensors_plot, id = 'lyd-DDBFED32', data = sensors_pr_device, device = 'DDBFED32', sensor = 'noise_avg', ticksuffix = 'dBa ', limits = 60)
}
