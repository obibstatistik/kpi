# drilldown_stmt <- paste0('select unit_id,box_id,building,"location",date(realdate) realdate,
#                              avg(temperature) temperature,avg(humidity) humidity,avg(co2) co2,
#                              avg(noise_avg) noise_avg,avg(noise_peak) noise_peak
#                              from smartcity.icmeter 
#                              GROUP BY unit_id,box_id,building,"location",realdate
#                            ;')

# checkouts_visitors_stmt <- paste0('SELECT transact_date dato, antal udlaan FROM (
#                             	(SELECT transact_date,sum(antal) antal
#                             	        from cicero.udlaan_per_opstillingsprofil
#                             			WHERE transact_date >= \'2017-11-01\'
#                             	        AND branch = \'Odense Hovedbibliotek\'
#                             	        group by transact_date,branch
#                             	        order by transact_date,branch) a
#                             	FULL OUTER JOIN
#                             	(SELECT date, visitor_count
#                             	FROM visitors.visitors_per_day
#                             	WHERE LOCATION = \'hb\'
#                                 AND date >= \'2017-11-01\') b
#                             	ON transact_date = date
# );')

drilldown_stmt <- paste0("SELECT building,location,realdate,avg_temp,avg_humid,avg_co2,avg_noise,avg_noise_peak,antal udlaan,visitor_count besoeg FROM (
  (select unit_id,box_id,building,location,date(realtime) realdate,
   avg(temperature) avg_temp,avg(humidity) avg_humid,avg(co2) avg_co2,
   avg(noise_avg) avg_noise,avg(noise_peak) avg_noise_peak
   from smartcity.icmeter
   GROUP BY unit_id,box_id,building,location,realdate) a
  
  LEFT JOIN
  
  (SELECT transact_date,sum(antal) antal
    from cicero.udlaan_per_opstillingsprofil
    WHERE transact_date >= '2017-11-01'
    AND branch = 'Odense Hovedbibliotek'
    group by transact_date,branch
    order by transact_date,branch) b
  ON transact_date = realdate
  
  FULL OUTER JOIN
  
  (SELECT date dato,visitor_count
    FROM visitors.visitors_per_day
    WHERE LOCATION = 'hb'
    AND date >= '2017-11-01') c
  ON transact_date = dato);
")


drv <- dbDriver("PostgreSQL")
con_dwh <- dbConnect(drv, dbname = dbname_dwh, host = host_dwh, port = port_dwh, user = user_dwh, password = password_dwh)
drilldown_df <- dbGetQuery(con_dwh, drilldown_stmt)
# checkouts_visitors_df <- dbGetQuery(con_dwh, checkouts_visitors_stmt)
dbDisconnect(con_dwh)


# # Necessary for testing outside shinyproxy env:
Sys.setenv('SHINYPROXY_USERGROUPS' = 'TGHFGHDFGH')
# Sys.setenv('SHINYPROXY_USERGROUPS' = 'WHITEBOOKREDAKTØRER,TESTGROUP,MATERIALEFORUM,DIGITALFORMIDLING')
# 
# # Get the user name and user groups of the current user for authorization
# ldap_username <- Sys.getenv('SHINYPROXY_USERNAME')
ldap_usergroups <- as.list(strsplit(Sys.getenv('SHINYPROXY_USERGROUPS'), ",")[[1]]) # converts comma separated string from env var into an R list


### Deviceplot MODUL ###

# Modul UI
device_plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(4,
           htmlOutput(ns("sensors_room_title")),
           withSpinner(plotlyOutput(ns("sensors_plot")))
    )
  )
}

# Modul SERVER
sensors_plot <- function(input, output, session, data, device, room, sensor, limits, ticksuffix) {
  
  # Af en eller anden grund skal device (og room tror jeg) kaldes eksplicit her, ellers vil der kun blive outputtet
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
             temperature = c(16,30),
             co2 = c(300,1100),
             humidity = c(20,70),
             noise_avg = c(20,65))
    }
    # Switch/case to distinguish between sensors to determine which y-axis step to use
    yaxis_step <- function(sensor) {
      switch(sensor,
             temperature = 1,
             co2 = 100,
             humidity = 5,
             noise_avg = 10)
      }
      p %>% layout(autosize = T, title = '', xaxis = list(title = 'timer på dagen'), yaxis = list(range = yaxis_range(sensor), title = '', dtick = yaxis_step(sensor), ticksuffix = ticksuffix), showlegend = FALSE)
  })
  
}

#sets dates for selects
fromdate <- paste0(Sys.Date()-8)
todate <- paste0(Sys.Date()-1)

indoor_climateTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "indoor_climate",
          
          box(width = 12, solidHeader = TRUE, id="spaceheader4",
              h3("Indeklima"),
              img(src='icons/detfysiskerum_negativ_45x45.png', align = "right", height="46px")
          ),
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset22",
                          tabPanel("I dag",
                              if ('DIGITALFORMIDLING' %in% ldap_usergroups) {
                                   fluidRow(
                                     column(12,
                                            column(2,
                                                   h4("Afgrænsning"),
                                                   tags$br(),
                                                   selectInput(ns("qt3q4t34tq34t"),"Lokation:",  sort(unique(licenser_overblik$eressource))),
                                                   selectInput(ns("dfgsd5g4we5gh4e"),"År:",  sort(unique(licenser_overblik$aar))),
                                                   checkboxGroupInput(ns("34t34g34"),
                                                                      'Sensor:',
                                                                      choices = list('Temperatur','Luftfugtighed','CO2 niveau','Støj'),
                                                                      selected = ('Temperatur'),
                                                                      inline = F),
                                                   radioButtons(ns("awtgfaw3g"),
                                                                      'Gitterlinjer for:',
                                                                      choices = list('Temperatur','Luftfugtighed','CO2 niveau','Støj'),
                                                                      selected = ('Temperatur'),
                                                                      inline = F),
                                                   checkboxGroupInput(ns("gse5rgs4e5"),
                                                                      'Snorhøjde:',
                                                                      inline = F),
                                                   tags$br(),tags$br(),
                                                   xlsxDownloadUI(ns("3g34gw3e4g4")),
                                                   tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                            ),
                                            column(8,
                                                   h3(paste("Indeklimamålinger i dag")),
                                                   h4(paste0("Gennemsnit af seneste målinger fra husets IC-meter sensorer ", date(now()) )),
                                                   tags$br(),
                                                   p("Nedenstående tal er gennemsnittet af de seneste målinger for hhv. temperatur fra IC-meter enheder i Borgernes Hus"),
                                                   p("De stiplede snorhøjder på 21°C og 25°C angiver det temperaturvindue, som målingerne ifølge SDU bør ligge indenfor"),
                                                   tags$br(),
                                                   tags$br(),
                                                   tags$br(),
                                                   tags$head(
                                                     tags$style(HTML(".rounded-box { padding: 10px; background: #eee; border-radius: 15px; height: 175px; width: auto; vertical-align: top; }")),
                                                     tags$style(HTML(".icmeter-large { color: #777; font-size: 100px; font-weight: bold; line-height: .7;}")),
                                                     tags$style(HTML(".icmeter-suffix { color: #999; font-size: 25px; line-height: 1; vertical-align: top; }")),
                                                     tags$style(HTML(".icmeter-updated { color: #999; font-size: 20px; line-height: 1.5; vertical-align: top; clear: left;}")),
                                                     tags$style(HTML(".icmeter-co2 { font-size: 50px; }")),
                                                     tags$style(HTML(".icmeter-updated { color: #999; font-size: 20px; line-height: 1.5; vertical-align: bottom; clear: left;}")),
                                                     tags$style(HTML(".icmeter-top { vertical-align: top; }"))
                                                   ),
                                                   column(2,
                                                          div(class = "rounded-box", 
                                                              div(class = "icmeter-top",
                                                                  span(class = "icmeter-large","25"), 
                                                                  span(class = "icmeter-suffix","°C")
                                                              ),
                                                              div(class = "icmeter-updated",
                                                                  span("Målt kl. 12:34")
                                                              )
                                                          )
                                                   ),
                                                   column(2,
                                                          div(class = "rounded-box",
                                                              span(class = "icmeter-large","40"), 
                                                              span(class = "icmeter-suffix","%"),
                                                              span(class = "icmeter-updated","Målt kl. 12:34")
                                                          )
                                                   ),
                                                   column(2,
                                                          div(class = "rounded-box", 
                                                              span(class = "icmeter-large icmeter-co2","1666"), 
                                                              span(class = "icmeter-suffix","ppm"),
                                                              span(class = "icmeter-updated","Målt kl. 12:34")
                                                          )
                                                   ),
                                                   column(2,
                                                          div(class = "rounded-box", 
                                                              span(class = "icmeter-large","32"), 
                                                              span(class = "icmeter-suffix","DBa"),
                                                              span(class = "icmeter-updated","Målt kl. 12:34")
                                                          )
                                                   )
                                            )
                                     )
                                   )
                              }
                          ),
                          tabPanel("Drill down",
                              if ('DIGITALFORMIDLING' %in% ldap_usergroups) {
                                   fluidRow(
                                     column(12,height = "900px",
                                            column(2,height = "900px",
                                                   h4("Afgrænsning"),
                                                   tags$br(),
                                                   selectInput(ns("drilldown_location"),"Område:", sort(unique(c(drilldown_df$location,"Borgernes Hus gennemsnit")))), # tilføj også valgmuligheden "BH gennemsnit"
                                                   selectInput(ns("drilldown_year"),"År:", sort(unique(year(drilldown_df$realdate)))),
                                                   checkboxGroupInput(ns("indoor_climate_sensor"),
                                                                      'Sensorer:',
                                                                      choices = list('Temperatur' = 'avg_temp','Luftfugtighed' = "avg_humid",'CO2 niveau' = "avg_co2",'Støj' = "avg_noise"),
                                                                      selected = list('Temperatur','Luftfugtighed'),
                                                                      inline = F),
                                                   checkboxGroupInput(ns("indoor_climate_weather"),
                                                                      "Vejret:",
                                                                      choices = list('Udetemperatur','Luftfugtighed udv.','Vindstyrke'),
                                                                      inline = F),
                                                   checkboxGroupInput(ns("indoor_climate_other_kpi"),
                                                                      "Andre KPI'er:",
                                                                      choices = list('Udlån på Borgernes Hus' = 'udlaan','Besøg på Borgernes Hus' = 'besoeg'), # hvad med sidevisninger og elektroniske udlån (evt. splittet på typer) og brug af mødelokaler?
                                                                      inline = F),
                                                   radioButtons(ns("indoor_climate_lattice"),
                                                                'Gitterlinjer:',
                                                                choices = list('Temperatur' = 'temperature','Luftfugtighed' = "humidity",'CO2 niveau' = "co2",'Støj' = "noise_avg","Ingen gitterlinjer" = "ingen"),
                                                                selected = ('Temperatur'),
                                                                inline = F),
                                                   p("Vælg hvilken sensor, der skal vises gitterlinjer for (når der er flere aktiverede sensorer)"),
                                                   checkboxInput(ns("indoor_climate_limits"),
                                                                      value = FALSE,
                                                                      'Grænseværdier:'),
                                                   p("Aktiverer snorhøjder, der viser den øvre og evt. nedre anbefalede grænseværdi for den sensor, der er valgt gitterlinjer for"),
                                                   radioButtons(ns("drthe45hr45"),
                                                                'Værditype (kun sensorer og vejr):',
                                                                choices = list('Værdi kl.12 middag','Dagens gennemsnit'),
                                                                selected = ('Dagens gennemsnit'),
                                                                inline = F),
                                                   p("Vælg om det er gennemsnittet for den enkelte dag eller værdien kl. 12 middag, der skal udgøre datapunkterne i grafen"),
                                                   tags$br(),tags$br(),
                                                   xlsxDownloadUI(ns("3g34ddgw3e4g4")),
                                                   tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                            ),
                                            column(8,height = "900px",
                                                  h3(paste("Drill down, indeklima")),
                                                h4("IC-Meter data pr. område/lokation i Borgernes Hus"),
                                                  tags$br(),
                                                  p("Andre biblioteksrelevante KPI'er, så som udlån og besøgende kan vælges fra venstremenuen."),
                                                  p("Vejrdata stammer fra IC-Meter"),
                                                  p("Andre KPI'er, dvs. udlån og besøg er her afgrænset til Borgernes Hus, men gælder hele huset. Afgrænsning af \"Område\" gælder således ikke for disse KPI'er."),
                                                  tags$br(),
                                                  tags$br(),
                                                  tags$br(),
                                                  withSpinner(plotlyOutput(ns("climate_drill_plot")))
                                            )
                                     )
                                )
                            }
                          ),
                          tabPanel("Temperatur", 
                                   fluidRow(
                                       column(12,
                                              h3("Temperatur i Borgernes Hus"),
                                              h4(paste0("Gennemsnit af målinger mellem ", fromdate, " og ", todate)),
                                              tags$br(),
                                              p("Nedenstående visualiseringer viser temperaturmålinger fra IC-meter enheder i Borgernes Hus over de senste otte dage, fordelt over timer på dagen"),
                                              p("Den kraftige sorte linje viser temperaturgennemsnittet for de otte dage og de grå linjer er de egentlige målinger, således at maximum og minimumværdier kan aflæses"),
                                              p("De stiplede snorhøjder på 21°C og 25°C angiver det temperaturvindue, som målingerne ifølge SDU bør ligge indenfor"),
                                              tags$br(),
                                              tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'330px\',\'290px\')"><i class="fa fa-print"></i> Print denne sektion</a>')),
                                              tags$br(),tags$br(),
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
                                              tags$br(),
                                              tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'330px\',\'290px\')"><i class="fa fa-print"></i> Print denne sektion</a>')),
                                              tags$br(),tags$br(),
  
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
                                            tags$br(),
                                            tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'330px\',\'290px\')"><i class="fa fa-print"></i> Print denne sektion</a>')),
                                            tags$br(),tags$br(),

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
                                            tags$br(),
                                            tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'330px\',\'290px\')"><i class="fa fa-print"></i> Print denne sektion</a>')),
                                            tags$br(),tags$br(),

                                            device_plotUI(ns(id = "lyd-1BE1CA2D")),
                                            device_plotUI(ns(id = "lyd-446AA82F")),
                                            #device_plotUI(ns(id = "lyd-879D5B2C")),
                                            device_plotUI(ns(id = "lyd-8CAE312E")),
                                            device_plotUI(ns(id = "lyd-20F2A02F")),
                                            device_plotUI(ns(id = "lyd-F358EC2B")),
                                            device_plotUI(ns(id = "lyd-DDBFED32"))
                                     )
                                   )
                          )
                          #,tabPanel("Data og dokumentation",
                          #         fluidRow(
                          #           column(12,
                          #                  p("Dokumentation")
                          #           )
                          #         )  
                          #)
                   ))))
}

indoor_climateTabPanel <- function(input, output, session, data, tablename) {
  
  # Hent data fra DB:
  fromdate <- paste0(Sys.Date()-8)
  todate <- paste0(Sys.Date()-1)
  select_stmt <- paste0("select * from smartcity.icmeter where substr(realtime::text,1,10) between '",fromdate,"' and '",todate,"';")
  # drilldown_reac_stmt <- paste0("select * from smartcity.icmeter where substr(realdate::text,1,10) between '2019-07-01' and '2019-09-30';")
  
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
  
  # Run the plot module for all devices and each of their four sensors:
  for(row in 1:nrow(device_ids)){
    callModule(sensors_plot, id = paste0("temp-",device_ids[row,"device_id"]), data = sensors_pr_device, device = device_ids[row,"device_id"], room = device_ids[row,"room"], sensor = 'temperature', ticksuffix = '°C ', limits = c(21,25))
    callModule(sensors_plot, id = paste0("co2-",device_ids[row,"device_id"]), data = sensors_pr_device, device =  device_ids[row,"device_id"], room = device_ids[row,"room"], sensor = 'co2', ticksuffix = 'ppm ', limits = 1000)
    callModule(sensors_plot, id = paste0("fugt-",device_ids[row,"device_id"]), data = sensors_pr_device, device = device_ids[row,"device_id"], room = device_ids[row,"room"], sensor = 'humidity', ticksuffix = '% ', limits = '')
    callModule(sensors_plot, id = paste0("lyd-",device_ids[row,"device_id"]), data = sensors_pr_device, device =  device_ids[row,"device_id"], room = device_ids[row,"room"], sensor = 'noise_avg', ticksuffix = 'dBa ', limits = 60)
  }
  
  drilldown_reac <- reactive({
    drilldown_df <- drilldown_df %>%
    # Gotta input timezone since dates have datatype 'timestamp with timezone'
    # mutate(uge = week(realdate),tz="Europe/Copenhagen")
    # mutate(dag = date(realdate)) %>%
    # group_by(dag) %>%
    # summarize(
    #   avg_temp = mean(temperature, na.rm = TRUE),
    #   avg_humid = mean(humidity, na.rm = TRUE),
    #   avg_co2 = mean(co2, na.rm = TRUE),
    #   avg_noise = mean(noise_avg, na.rm = TRUE)
    # ) %>%
    filter(location == input$drilldown_location) %>%
    filter(year(realdate) == input$drilldown_year) %>%
    mutate(uge = week(realdate)) %>%
    rename(dag = realdate) %>%
    # select(dag,uge,avg_temp,avg_humid,avg_co2,avg_noise)
    select(dag,uge,input$indoor_climate_sensor,input$indoor_climate_other_kpi)
    #mutate(hour = format(realdate,'%H',tz="Europe/Copenhagen"))
    # select(unit_id,box_id,dato,realdate,temperature,humidity,co2,noise_avg,noise_peak)
  })
  
  sensor <- "Temperatur"
  
  # Switch/case to distinguish between sensors to determine which y-axis range to use
  limits <- function(sensor) {
    switch(sensor,
           temperature = list(16,30,"y1"),
           humidity = list(20,70,"y2"),
           co2 = list(300,1100,"y3"),
           noise_avg = list(30,55,"y4"))
  }

  # Render the plot
  output$climate_drill_plot <- renderPlotly({
    colNames <- names(drilldown_reac())[-1:-2]                              # ie. get all colnames except the first which is year or month or whatever
    p <- plot_ly(drilldown_reac(), type = 'scatter', height = 600)
    p <- p %>% plotly::config(displayModeBar = F, showLink = F)       # plotly:: namespace needed here. Presumably plotlys config() is masked by something otherwise...

    # Grænseværdier / snorhøjder
    if(input$indoor_climate_limits == TRUE && input$indoor_climate_lattice != 'ingen') {  
      p <- p %>% add_trace(x = ~dag, 
                           y = limits(input$indoor_climate_lattice)[[1]], 
                           line = list(color = 'rgb(200,200,200)', 
                           width = 2, 
                           dash = 'dash'), 
                           showlegend = FALSE, 
                           mode = 'lines', 
                           zeroline = F,
                           yaxis = limits(input$indoor_climate_lattice)[[3]]) 
      
      p <- p %>% add_trace(x = ~dag,
                           y = limits(input$indoor_climate_lattice)[[2]], 
                           line = list(color = 'rgb(200,200,200)', 
                           width = 2, dash = 'dash'), 
                           name = 'grænseværdi', 
                           mode = 'lines', 
                           zeroline = F,
                           yaxis = limits(input$indoor_climate_lattice)[[3]]) 
    }
    
    # Udlån (fysisk)
    if("udlaan" %in% colnames(drilldown_reac())) {  
      p <- p %>% add_trace(x = ~dag,
                           y = ~udlaan,
                           name = 'udlån',
                           mode = 'lines',
                           yaxis = "y5",
                           line = list(color = 'rgb(200,200,200)', width = 1))
      
      p <- p %>% layout(yaxis5 = list( autotick = FALSE,
                                       #range = c(20,65), 
                                       title = "",
                                       overlaying = "y",
                                       side = 'right', 
                                       showline = F,
                                       zeroline = F,
                                       linewidth = 1,
                                       linecolor = 'rgb(200,200,200)',
                                       ticklen = 10,
                                       dtick = 500,
                                       tickwidth = 2,
                                       tickcolor = 'rgb(200,200,200)',
                                       tickprefix = '               ',
                                       # ticksuffix = 'dBa ',
                                       tickfont = list(color = 'rgb(200,200,200)'),
                                       autotick = F,
                                       showgrid = if(input$indoor_climate_lattice == "udlaan"){TRUE}else{FALSE},
                                       ticks = "inside"))
    }
    
    # Besøg (fysisk)
    if("besoeg" %in% colnames(drilldown_reac())) {  
      p <- p %>% add_trace(x = ~dag,
                           y = ~besoeg,
                           name = 'Besøg',
                           mode = 'lines',
                           yaxis = "y2",
                           line = list(color = 'rgb(120,120,120)', width = 1))
      
      p <- p %>% layout(yaxis2 = list( autotick = FALSE,
                                       range = c(0,3300), 
                                       title = "",
                                       overlaying = "y",
                                       side = 'right', 
                                       # showline = F,
                                       zeroline = F,
                                       linewidth = 1,
                                       linecolor = 'rgb(100,100,100)',
                                       ticklen = 10,
                                       dtick = 200,
                                       tickwidth = 2,
                                       tickcolor = 'rgb(100,100,100)',
                                       tickfont = list(color = 'rgb(100,100,100)'),
                                       autotick = F,
                                       showgrid = if(input$indoor_climate_lattice == "udlaan"){TRUE}else{FALSE},
                                       ticks = "outside"))
    }
    
    if("avg_temp" %in% colnames(drilldown_reac())) {
      
      # Temperatur
      p <- p %>% add_trace(x = ~dag, 
                           y = ~avg_temp, 
                           name = 'temperatur', 
                           mode = 'lines',
                           yaxis = "y1", 
                           line = list(color = color1))
      
      p <- p %>% layout(yaxis = list(dtick = 1, 
                                     tickwidth = 2,
                                     tickcolor = color1, 
                                     ticksuffix = '°C ', 
                                     ticklen = 10,
                                     zeroline = F,
                                     range = c(15,32),
                                     # title = 'temperatur', 
                                     title = '', 
                                     tickfont = list(color = color1), 
                                     side = 'left', 
                                     showline = T,
                                     # showgrid = if("temperature" %in% input$indoor_climate_lattice){TRUE}else{FALSE},
                                     showgrid = if(input$indoor_climate_lattice == "temperature"){TRUE}else{FALSE},
                                     ticks = "outside"))
    }
    
    # Luftfugtighed
    if("avg_humid" %in% colnames(drilldown_reac())) {  
      p <- p %>% add_trace(x = ~dag, 
                           y = ~avg_humid, 
                           name = 'luftfugtighed', 
                           mode = 'lines', 
                           yaxis = "y7", 
                           line = list(color = color2))
      
      p <- p %>% layout(yaxis7 = list(ticksuffix = '%              ',
                                     tickwidth = 2,
                                     ticklen = 10, 
                                     tickcolor = color2, 
                                     range = c(0,100),
                                     zeroline = F,
                                     # rangemode = "tozero",
                                     title = '', 
                                     # title = 'luftfugtighed', 
                                     tickfont = list(color = color2), 
                                     overlaying = "y", 
                                     side = 'left', 
                                     showline = T,
                                     # showgrid = if("humidity" %in% input$indoor_climate_lattice){TRUE}else{FALSE},
                                     showgrid = if(input$indoor_climate_lattice == "humidity"){TRUE}else{FALSE},
                                     ticks = "inside"))
    }
    
    # CO2
    if("avg_co2" %in% colnames(drilldown_reac())) {  
       p <- p %>% add_trace( x = ~dag,
                             y = ~avg_co2,
                             name = 'co2 niveau',
                             mode = 'lines',
                             yaxis = "y3",
                             line = list(color = color3))
       
       p <- p %>% layout(yaxis3 = list( ticksuffix = 'ppm ',
                                        tickwidth = 2,
                                        rangemode = "tozero",
                                        # title = 'co2 niveau', 
                                        title = '', 
                                        zeroline = F,
                                        tickfont = list(color = color3),
                                        ticklen = 10,
                                        overlaying = "y", 
                                        side = 'right', 
                                        showline = T, 
                                        # showgrid = if("co2" %in% input$indoor_climate_lattice){TRUE}else{FALSE},
                                        showgrid = if(input$indoor_climate_lattice == "co2"){TRUE}else{FALSE},
                                        ticks = "outside"))
     }
    
    # Støj (avg)
    if("avg_noise" %in% colnames(drilldown_reac())) {  
       p <- p %>% add_trace(x = ~dag, 
                            y = ~avg_noise, 
                            name = 'lydniveau', 
                            mode = 'lines', 
                            yaxis = "y4", 
                            line = list(color = color6))
       
       p <- p %>% layout(yaxis4 = list( autotick = FALSE,
                                    range = c(20,65), 
                                    # title = 'gnm.snit støj', 
                                    title = "",
                                    overlaying = "y", 
                                    side = 'right', 
                                    showline = F, 
                                    zeroline = F,
                                    linewidth = 2,
                                    linecolor = 'rgb(200,200,200)',
                                    ticklen = 10,
                                    dtick = 5,
                                    tickwidth = 2,
                                    tickcolor = color6, 
                                    tickprefix = '                      ',
                                    ticksuffix = 'dBa ', 
                                    tickfont = list(color = color6), 
                                    autotick = F,
                                    # showgrid = if("noise_avg" %in% input$indoor_climate_lattice){TRUE}else{FALSE},
                                    showgrid = if(input$indoor_climate_lattice == "noise_avg"){TRUE}else{FALSE},
                                    ticks = "inside"))
    }

    
    p <- p %>% layout(margin = list(l = 100, r = 150, t = 10, b = 30, pad = 15),
                      legend = list(x = 1.2, y = 1.038))
    
    # To get sparse tick labels (every 7th day) we need to use tickvals and ticktext to manually
    # supply the tick values and tick labels via two separate lists.
    ttxt <- rep("",length(drilldown_reac()$uge))
    ttxt[seq(1,length(drilldown_reac()$uge),7)] <- as.character(drilldown_reac()$uge)[seq(1,length(drilldown_reac()$uge),7)]
    
    # p <- p %>% layout(xaxis = list(showline = FALSE, 
    #                                autotick = FALSE,
    #                                # dtick = 7,
    #                                tickvals=drilldown_reac()$dag,
    #                                ticktext=ttxt,
    #                                # ticktext=drilldown_reac()$uge,
    #                                tickfont  = list(size = 14, color = 'rgb(0,0,0)'), 
    #                                title = ''))
    
    weektick <- 86400000.0 * 7
    
    p <- p %>% layout(xaxis = list(tickvals=drilldown_reac()$dag,
                                   ticktext=ttxt,
                                   tick0 = drilldown_reac()$dag[1], 
                                   zeroline = F,
                                   dtick = weektick,
                                   showgrid = if(input$indoor_climate_lattice == "ingen"){FALSE}else{TRUE},
                                   ticklen = 10,
                                   title = '\nUge',
                                   ticks = "outside"))
  })
}
