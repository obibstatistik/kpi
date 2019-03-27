# UI

citizenserviceTabPanelUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "citizenservice",
    box(
      width = 12,
      solidHeader = TRUE,
      id = "citizenheader",
      h3("Borgerservice"),
      img(src='icons/borgerservice_positiv_45x45.png', align = "right", height="46px")
    ),
    fluidRow(column(
      12,
      tabBox(
        width = 12,
        id = "tabset12",
        tabPanel("Generelt",
                 fluidRow(
                   column(width = 12,
                          h4("Betjeninger pr. år"),      
                          column(width = 2),
                          column(width = 10, withSpinner(plotlyOutput(ns("betjeninger_count_year_plot"))))
                   ),
                   column(12,tags$hr()),
                   column(width = 12,
                          h4("Betjeninger pr. måned"),
                          column(width = 2),
                          column(width = 10, withSpinner(plotlyOutput(ns("betjeninger_count_month_plot"))))
                   ),
                   column(12,tags$hr()),
                   column(width = 12,
                          h4("Gennemsnitlig ventetid / år"),
                          column(width = 2),
                          column(width = 10, withSpinner(plotlyOutput(ns("betjeninger_avgwait_year_plot"))))
                   ),
                   column(12,tags$hr()),
                   column(width = 12,
                          h4("Gennemsnitlig ventetid / måned"),
                          column(width = 2),
                          column(width = 10, withSpinner(plotlyOutput(ns("betjeninger_avgwait_month_plot"))))
                   ),
                   column(12,tags$hr()),
                   column(width = 12,
                          h4("Gennemsnitlig betjeningstid / år"),
                          column(width = 2),
                          column(width = 10, withSpinner(plotlyOutput(ns("betjeninger_service_year_plot"))))
                   ),
                   column(12,tags$hr()),
                   column(width = 12,
                          h4("Gennemsnitlig betjeningstid / måned"),
                          column(width = 2),
                          column(width = 10, withSpinner(plotlyOutput(ns("betjeninger_service_month_plot"))))
                   )
                 ))
        ,
        tabPanel("Borgerservice / Bibliotek",
                 fluidRow(width = 12,
                          column(
                            width = 12,
                            h4("Besøgende Borgernes Hus / Betjeninger Borgerservice"),
                            column(width = 12, 
                                   withSpinner(plotlyOutput(ns("borgvsbib_plot")))
                            )
                          )
                 )),
        tabPanel("Dag / tid heatmaps",
                 fluidRow(width = 12,
                          column(
                            width = 6,
                            heatmapWeekTableUI(ns(id = "heat1"), title = "Antal Betjeninger")
                          ),
                          column(
                            width = 6,          
                            heatmapWeekTableUI(ns(id = "heat2"), title = "Antal Unikke Betjeninger")
                          ),
                          column(
                            width = 6,          
                            heatmapWeekTableUI(ns(id = "heat3"), title = "Gennemsnitlig ventetid")
                          ),
                          column(
                            width = 6,
                            heatmapWeekTableUI(ns(id = "heat4"), title = "Gennemsnitlig betjeningstid")
                          )
                 )
        ),
        tabPanel("Køer",
                 fluidRow(width = 12,
                          column(
                            width = 12,
                            column(width = 12, 
                                   dateRangeInput(ns("queue_date_range"),
                                                  label = 'Vælg periode',
                                                  start = Sys.Date() - 90, end = Sys.Date(),
                                                  separator = " - "
                                   ),withSpinner(tableOutput(ns("betjeninger"))))
                          )))
        
      )
    ))
  )
  
}

# SERVER

citizenserviceTabPanel <-
  function(input, output, session, data, tablename) {
    
    # GENERELT #
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
    con_dwh <- dbConnect(drv, dbname = dbname_dwh, host = host_dwh, port = port_dwh, use = user_dwh, password = password_dwh)
    
    # Betjeninger
    betjeninger <- dbGetQuery(con_dwh, "select * from borgerservice.betjeninger")

    visitors_per_day <- dbGetQuery(con, "SELECT date, sum as bibcount FROM datamart.visitors_per_day WHERE location='hb' and date > '2017-11-24'") 
    betjeninger_per_day <- dbGetQuery(con_dwh, "SELECT distinct(\"Tid\"::date) as date, count(*) as borgcount FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' AND \"Tid\"::date > '2017-11-24' GROUP BY date")
    
    betjeninger_heat_1 <- dbGetQuery(con_dwh, "SELECT date_trunc('day', \"Tid\") as date, extract(isodow from \"Tid\") as weekday, extract(hour from \"Tid\") as hour, count(*) as count FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' AND \"Kø\" NOT IN ('Tilskud/Enkeltydelse','Tilskud/enkeltydelse - har reserveret tid','Pension','Tilskud/Enkeltydelser','Studievalg Fyn','Borgerkontakten') GROUP BY weekday, hour, date")
    betjeninger_heat_2 <- dbGetQuery(con_dwh, "SELECT * FROM borgerservice.betjeninger_per_hour_unique")
    betjeninger_heat_3 <- dbGetQuery(con_dwh, "SELECT date_trunc('day', \"Tid\") as date, extract(isodow from \"Tid\") as weekday, extract(hour from \"Tid\") as hour, avg(\"Ventetid\") as avgvente FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' AND \"Kø\" NOT IN ('Tilskud/Enkeltydelse','Tilskud/enkeltydelse - har reserveret tid','Pension','Tilskud/Enkeltydelser','Studievalg Fyn','Borgerkontakten') GROUP BY weekday, hour, date")
    betjeninger_heat_4 <- dbGetQuery(con_dwh, "SELECT date_trunc('day', \"Tid\") as date, extract(isodow from \"Tid\") as weekday, extract(hour from \"Tid\") as hour, avg(\"Behandlingstid\") as avgserve FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' AND \"Kø\" NOT IN ('Tilskud/Enkeltydelse','Tilskud/enkeltydelse - har reserveret tid','Pension','Tilskud/Enkeltydelser','Studievalg Fyn','Borgerkontakten')  GROUP BY weekday, hour, date")
    
    #kategorier <- dbGetQuery(con, "select \"Kø\", count(*) FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY \"Kø\" ORDER BY count(*) desc")
    
    dbDisconnect(con)
    dbDisconnect(con_dwh)
    
    # modules
    callModule(checkboxFromData, id = "citizen", data = betjeninger)
    
    # betjeninger year / count 
    output$betjeninger_count_year_plot <- renderPlotly({
      data <- betjeninger %>%
        mutate(year = year(dato)) %>%
        group_by(year) %>%
        summarise(count = sum(antal))
      plot_ly(data, x = ~year, y = ~count, type = 'bar', marker = list(color = color5)) %>%
        layout(xaxis = list(title = 'År', dtick = 1, autotick = FALSE,ticks = "outside",tick0 = 2016), yaxis = list(title = 'Antal'))
    }) 

    # betjeninger month / count
    
    output$betjeninger_count_month_plot <- renderPlotly({
      data <- betjeninger %>%
        select(dato, antal) %>%
        mutate(month = month(dato), year = year(dato)) %>%
        select(-dato) %>%
        group_by(month, year) %>%
        summarise(count = sum(antal)) %>%
        spread(key = year, value = count)
      colNames <- names(data)[-1] 
      p <- plot_ly(data)
      i<-0
      for(trace in colNames){
        i<-i+1
        p <- p %>% add_trace(x = ~month, y = as.formula(paste0("~`", trace, "`")), marker = list(color = colors[i]), name = trace, type = 'bar')
      }
      p %>% layout(xaxis = list(title = 'Måneder'), yaxis = list (title = 'Antal'), barmode = 'group')
    })
    
    # betjeninger ventetid / år
    
    output$betjeninger_avgwait_year_plot <- renderPlotly({
      data <- betjeninger %>%
        mutate(year = year(dato)) %>%
        mutate(gen_ventetid_min = ifelse(is.na(gen_ventetid_min),0,gen_ventetid_min)) %>%
        group_by(year) %>%
        summarise(avgwait = mean(gen_ventetid_min))
      plot_ly(data, x = ~year, y = ~avgwait, type = 'bar', marker = list(color = color5)) %>%
        layout(xaxis = list(title = 'År', dtick = 1, autotick = FALSE,ticks = "outside",tick0 = 2016), yaxis = list(title = 'Minutter'))
    })

    # betjeninger måned / ventetid
    
    output$betjeninger_avgwait_month_plot <- renderPlotly({
      data <- betjeninger %>%
        select(dato, gen_ventetid_min) %>%
        mutate(gen_ventetid_min = ifelse(is.na(gen_ventetid_min),0,gen_ventetid_min)) %>%
        mutate(month = month(dato), year = year(dato)) %>%
        select(-dato) %>%
        group_by(month, year) %>%
        summarise(count = mean(gen_ventetid_min)) %>%
        spread(key = year, value = count)
      colNames <- names(data)[-1] 
      p <- plot_ly(data)
      i<-0
      for(trace in colNames){
        i<-i+1
        p <- p %>% add_trace(x = ~month, y = as.formula(paste0("~`", trace, "`")), marker = list(color = colors[i]), name = trace, type = 'bar')
      }
      p %>% layout(xaxis = list(title = 'Måneder'), yaxis = list (title = 'Minutter'), barmode = 'group')
    })
    
    # betjeninger år / betjeningstid
    
    output$betjeninger_service_year_plot <- renderPlotly({
      data <- betjeninger %>%
        mutate(year = year(dato)) %>%
        mutate(gen_behandlingstid_min = ifelse(is.na(gen_behandlingstid_min),0,gen_behandlingstid_min)) %>%
        group_by(year) %>%
        summarise(avgservice = mean(gen_behandlingstid_min))
      plot_ly(data, x = ~year, y = ~avgservice, type = 'bar', marker = list(color = color5)) %>%
        layout(xaxis = list(title = 'År', dtick = 1, autotick = FALSE,ticks = "outside",tick0 = 2016), yaxis = list(title = 'Minutter'))
    })
    
    # betjeninger måned / betjeningstid
    
    output$betjeninger_service_month_plot <- renderPlotly({
      data <- betjeninger %>%
        select(dato, gen_behandlingstid_min) %>%
        mutate(gen_behandlingstid_min = ifelse(is.na(gen_behandlingstid_min),0,gen_behandlingstid_min)) %>%
        mutate(month = month(dato), year = year(dato)) %>%
        select(-dato) %>%
        group_by(month, year) %>%
        summarise(count = mean(gen_behandlingstid_min)) %>%
        spread(key = year, value = count)
      colNames <- names(data)[-1] 
      p <- plot_ly(data)
      i<-0
      for(trace in colNames){
        i<-i+1
        p <- p %>% add_trace(x = ~month, y = as.formula(paste0("~`", trace, "`")), marker = list(color = colors[i]), name = trace, type = 'bar')
      }
      p %>% layout(xaxis = list(title = 'Måneder'), yaxis = list (title = 'Minutter'), barmode = 'group')
    })
    
    # BORGERNES HUS / BORGERSERVICE #
    
    visitors_per_day <- visitors_per_day %>% 
      group_by(date = round_date(date, unit="month")) %>%
      summarise(bibcount = as.integer(sum(bibcount)))
    
    betjeninger_per_day <- betjeninger_per_day %>% 
      group_by(date = round_date(date, unit="month")) %>%
      summarise(borgcount = as.integer(sum(borgcount)))            
    
    datadata <- visitors_per_day %>%
      full_join(betjeninger_per_day, by = c("date" = "date")) %>%
      mutate_all(funs(ifelse(is.na(.), 0,.)))  %>%
      mutate(date = format(as.Date(date, origin="1970-01-01"), format="%Y-%m")) %>%
      slice(1:(n()-2)) 
    
    output$borgvsbib_plot <- renderPlotly({
      plot_ly(datadata) %>%
        add_lines(x = ~date, y = ~bibcount, name = 'Indgang Borgernes Hus', line = list(color = color3)) %>%
        add_lines(x = ~date, y = ~borgcount, name = 'Betjeninger Borgerservice', yaxis = "y2", line = list(color = color4)) %>%
        layout(
          xaxis = list(title="Dato", showgrid = FALSE),
          yaxis = list(
            tickfont = list(color = color3),
            title = "Indgang Borgernes Hus"),
          yaxis2 = list(
            tickfont = list(color = color4),
            overlaying = "y",
            side = "right",
            title = "Betjeninger Borgerservice",
            showgrid = FALSE
          )
        )
    })
    
    output$borghus <- renderTable(visitors_per_day)
    output$borgserv <- renderTable(betjeninger_per_day)
    output$joined <- renderTable(datadata)
    
    # HEATMAPS #
    
    betjeninger_heat_1 <- betjeninger_heat_1 %>%
      filter(hour > 7 & hour < 19)
    
    betjeninger_heat_3 <- betjeninger_heat_3 %>%
      filter(hour > 7 & hour < 19) %>%
      mutate(tid = paste0(substr(avgvente, 4, 5),".",substr(avgvente, 7, 8))) %>%
      select(-avgvente) %>%
      mutate_at(vars(3), funs(as.numeric(.)))
    
    betjeninger_heat_4 <- betjeninger_heat_4 %>%
      filter(hour > 7 & hour < 19) %>%
      mutate(tid = paste0(substr(avgserve, 4, 5),".",substr(avgserve, 7, 8))) %>%
      select(-avgserve) %>%
      mutate_at(vars(3), funs(as.numeric(.))) 
    
    callModule(heatmapWeekTable, id = "heat1", data = betjeninger_heat_1, type = "count")
    callModule(heatmapWeekTable, id = "heat2", data = betjeninger_heat_2, type = "count")
    callModule(heatmapWeekTable, id = "heat3", data = betjeninger_heat_3, type = "mean")
    callModule(heatmapWeekTable, id = "heat4", data = betjeninger_heat_4, type = "mean")
    
    # KATEGORIER
    
    output$betjeninger <- renderTable(
      betjeninger <- betjeninger %>% 
        filter(dato > input$queue_date_range[1] & dato < input$queue_date_range[2]) %>%
        mutate(gen_ventetid_min = ifelse(is.na(gen_ventetid_min), 0, gen_ventetid_min), gen_behandlingstid_min = ifelse(is.na(gen_behandlingstid_min), 0, gen_behandlingstid_min)) %>%
        group_by(kø) %>%
        summarise('Antal betjeninger' = sum(antal), 'Antal unikke betjeninger' = sum(unik_antal), 'Gennemsnitlig ventetid (min.)' = mean(gen_ventetid_min), 'Gennemsnitlig betjeningstid (min.)' = mean(gen_behandlingstid_min))
      )
    
  }