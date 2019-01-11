source("global.R")
source("modules.R")
source("~/.postpass")
source("functions.R")

# Queries

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)

  # OVERSIGT OVER TYPE & KATEGORI: select "Type", "Kategori", count(*) FROM borgerservice.x_betjeninger WHERE "Lokation" = 'Borgerservice Odense' GROUP BY "Type", "Kategori" ORDER BY "Type", count(*) desc
  betjeninger_years <- dbGetQuery(con, "SELECT distinct(extract(year from \"Tid\")) as year FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense'")
  betjeninger_categories <- dbGetQuery(con, "SELECT distinct(\"Kategori\") as category FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense'")
  
  betjeninger_count_years <- dbGetQuery(con, "SELECT extract(year from (\"Tid\")::date) as year, count(*) as count FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY year")
  betjeninger_count_date_category <- dbGetQuery(con, "SELECT date_trunc('month', \"Tid\") as date, count('ID') as count, \"Kategori\" as category FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY date, category")
  
  betjeninger_wait_year <- dbGetQuery(con, "SELECT date_trunc('year', \"Tid\") as year, round((EXTRACT(epoch FROM avg(\"Ventetid\"))/60)::numeric, 2) as avgwait FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY year")
  #betjeninger_wait_year_category <- dbGetQuery(con, "SELECT date_trunc('year', \"Tid\") as year, \"Kategori\" as category, avg(\"Ventetid\") as avgwait FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY year, category")
  betjeninger_wait_month <- dbGetQuery(con, "SELECT extract(month from \"Tid\") as month, round((EXTRACT(epoch FROM avg(\"Ventetid\"))/60)::numeric, 2) as avgwait FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY month ORDER BY month")
  #betjeninger_wait_month_category <- dbGetQuery(con, "SELECT extract(month from \"Tid\") as month, \"Kategori\" as category, avg(\"Behandlingstid\") as avgserve FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY month, category")
  
  betjeninger_service_year <- dbGetQuery(con, "SELECT date_trunc('year', \"Tid\") as year, round((EXTRACT(epoch FROM avg(\"Behandlingstid\"))/60)::numeric, 2) as avgservice FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY year")
  #betjeninger_service_year_category <- dbGetQuery(con, "SELECT date_trunc('year', \"Tid\") as year, \"Kategori\" as category, avg(\"Ventetid\") as avgwait FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY year, category")
  betjeninger_service_month <- dbGetQuery(con, "SELECT extract(month from \"Tid\") as month, round((EXTRACT(epoch FROM avg(\"Behandlingstid\"))/60)::numeric, 2) as avgservice FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY month ORDER BY month")
  #betjeninger_service_month_category <- dbGetQuery(con, "SELECT extract(month from \"Tid\") as month, \"Kategori\" as category, avg(\"Behandlingstid\") as avgserve FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY month, category")
  
  visitors_per_day <- dbGetQuery(con, "SELECT date, sum as bibcount FROM datamart.visitors_per_day WHERE location='hb' and date > '2016-08-11'") 
  betjeninger_per_day <- dbGetQuery(con, "SELECT distinct(\"Tid\"::date) as date, count(*) as borgcount FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY date")
  
  betjeninger_heat_1 <- dbGetQuery(con, "SELECT date_trunc('day', \"Tid\") as date, extract(isodow from \"Tid\") as weekday, extract(hour from \"Tid\") as hour, count(*) as count FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY weekday, hour, date")
  betjeninger_heat_2 <- dbGetQuery(con, "SELECT date_trunc('day', \"Tid\") as date, extract(isodow from \"Tid\") as weekday, extract(hour from \"Tid\") as hour, count(*) as count FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY weekday, hour, date")
  betjeninger_heat_3 <- dbGetQuery(con, "SELECT date_trunc('day', \"Tid\") as date, extract(isodow from \"Tid\") as weekday, extract(hour from \"Tid\") as hour, avg(\"Ventetid\") as avgvente FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY weekday, hour, date")
  betjeninger_heat_4 <- dbGetQuery(con, "SELECT date_trunc('day', \"Tid\") as date, extract(isodow from \"Tid\") as weekday, extract(hour from \"Tid\") as hour, avg(\"Behandlingstid\") as avgserve FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY weekday, hour, date")

  kategorier <- dbGetQuery(con, "select \"Type\", \"Kategori\", count(*) FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' GROUP BY \"Type\", \"Kategori\" ORDER BY \"Type\", count(*) desc")
  
  dbDisconnect(con)

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
                       column(width = 2#, 
                              #checkboxFromDataUI(ns(id = "citizen"), data = betjeninger_years$year, text = "Vælg årstal"),
                              #checkboxFromDataUI(ns(id = "citizen11"), data = betjeninger_categories$category, text = "Vælg kategori")
                       ),
                       column(width = 10, plotlyOutput(ns("betjeninger_count_year_plot")))
                     ),
                     column(12,tags$hr()),
                     column(width = 12,
                       h4("Betjeninger pr. måned"),
                       column(width = 2#, årstalfilter er skjult i første omgang
                              #checkboxFromDataUI(ns(id = "citizen2"), data = betjeninger_years$year, text = "Vælg årstal")
                       ),
                       column(width = 10, plotlyOutput(ns("betjeninger_count_month_plot")))
                     ),
                     column(12,tags$hr()),
                     column(width = 12,
                       h4("Gennemsnitlig ventetid / år"),
                       column(width = 2#, årstalfilter er skjult i første omgang
                              #checkboxFromDataUI(ns(id = "citizen3"), data = betjeninger_years$year, text = "Vælg årstal")
                       ),
                       column(width = 10, plotlyOutput(ns("betjeninger_avgwait_category_plot")))
                     ),
                     column(12,tags$hr()),
                     column(width = 12,
                       h4("Gennemsnitlig ventetid / måned"),
                       column(width = 2#, 
                         #checkboxFromDataUI(ns(id = "citizen3"), data = betjeninger_categories$category, text = "Vælg kategori")
                       ),
                       column(width = 10, plotlyOutput(ns("betjeninger_avgwait_month_plot")))
                     ),
                     column(12,tags$hr()),
                     column(width = 12,
                       h4("Gennemsnitlig betjeningstid / år"),
                       column(width = 2#, 
                         #checkboxFromDataUI(ns(id = "citizen4"), data = betjeninger_years$year, text = "Vælg årstal")
                       ),
                       column(width = 10, plotlyOutput(ns("betjeninger_service_year_plot")))
                     ),
                     column(12,tags$hr()),
                     column(width = 12,
                       h4("Gennemsnitlig betjeningstid / måned"),
                       column(width = 2#, 
                         #checkboxFromDataUI(ns(id = "citizen4"), data = betjeninger_years$year, text = "Vælg årstal")
                       ),
                       column(width = 10, plotlyOutput(ns("betjeninger_service_month_plot")))
                     )
                ))
                 ,
        tabPanel("Borgerservice / Bibliotek",
                 fluidRow(width = 12,
                    column(
                      width = 12,
                      h4("Besøgende Borgernes Hus / Betjeninger Borgerservice"),
                      column(width = 12, 
                        plotlyOutput(ns("borgvsbib_plot"))
                        )
                      )#,
                      #column(width = 4, tableOutput(ns("joined"))),
                      #column(width = 4, tableOutput(ns("borghus"))),
                      #column(width = 4, tableOutput(ns("borgserv")))
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
         tabPanel("Kategorier",
                  fluidRow(width = 12,
                   column(
                     width = 12,
                     h4("Kategorioverblik"),
                     column(width = 12, tableOutput(ns("kategorier")))
                   )))
        
      )
    ))
  )
  
}

# SERVER

citizenserviceTabPanel <-
  function(input, output, session, data, tablename) {
    
    # GENERELT #
    
    # modules
    callModule(checkboxFromData, id = "citizen", data = betjeninger)
    
    # betjeninger year / count 

    output$betjeninger_count_year_plot <- renderPlotly({
      data <- betjeninger_count_years %>%
        mutate_at(vars(1), funs(as.character(.)))
      plot_ly(data, x = ~year, y = ~count, type = 'bar', marker = list(color = color5)) %>%
        layout(xaxis = list(title = 'År'), yaxis = list(title = 'Antal'))
    }) 
    
    # betjeninger year / count med kategorier
    
    # output$betjeninger_count_year_plot <- renderPlotly({
    #   data <- betjeninger_count_date_category %>%
    #     mutate(year = year(date)) %>%
    #     select(-date) %>%
    #     group_by(category, year) %>%
    #     summarise(count = sum(count)) %>%
    #     spread(key = category, value = count)
    #   colNames <- names(data)[-1] 
    #   p <- plot_ly(data) 
    #   for(trace in colNames){
    #     p <- p %>% add_trace(x = ~year, y = as.formula(paste0("~`", trace, "`")), name = trace, type = 'bar')
    #   }
    #   p %>% layout(xaxis = list(title = 'Årstal', nticks = 3), yaxis = list (title = 'Antal betjeninger'), barmode = 'stack')
    # })
    
    # betjeninger month / count
    
    output$betjeninger_count_month_plot <- renderPlotly({
      data <- betjeninger_count_date_category %>%
        select(date, count) %>%
        mutate(month = month(date), year = year(date)) %>%
        #filter(year %in% input$INPUTNAVN) hvis der sættes års filter på 
        select(-date) %>%
        group_by(month, year) %>%
        summarise(count = sum(count)) %>%
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
    
    output$betjeninger_avgwait_category_plot <- renderPlotly({
      data <- betjeninger_wait_year %>%
        mutate_at(vars(1), funs(as.character(.))) %>%
        mutate_at(vars(1), funs(substr(., 1, 4)))
      plot_ly(data, x = ~year, y = ~avgwait, type = 'bar', marker = list(color = color5)) %>%
        layout(xaxis = list(title = 'År'), yaxis = list(title = 'Minutter'))
    })
    
    # betjeninger kategori / ventetid / år 
    
    # output$betjeninger_avgwait_category_plot <- renderPlotly({
    #   data <- betjeninger_wait_year_category %>%
    #     spread(key = year, value = avgwait)
    #   colNames <- names(data)[-1] 
    #   p <- plot_ly(data) 
    #   for(trace in colNames){
    #     p <- p %>% add_trace(x = ~category, y = as.formula(paste0("~`", trace, "`")), name = trace, type = 'bar')
    #   }
    #   p %>% layout(xaxis = list(title = 'Kategorier'), yaxis = list (title = 'Gennemsnitlig ventetid'), barmode = 'group')
    # })
    
    # betjeninger måned / ventetid
    
    output$betjeninger_avgwait_month_plot <- renderPlotly({
      data <- betjeninger_wait_month
      plot_ly(data, x = ~month, y = ~avgwait, type = 'bar', marker = list(color = color5)) %>%
        layout(xaxis = list(title = 'Måned'), yaxis = list(title = 'Minutter'))
    })
    
    # output$betjeninger_avgwait_month_plot <- renderPlotly({
    #   data <- betjeninger_wait_month_category %>%
    #     spread(key = category, value = avgserve)
    #   colNames <- names(data)[-1] 
    #   p <- plot_ly(data) 
    #   for(trace in colNames){
    #     p <- p %>% add_trace(x = ~month, y = as.formula(paste0("~`", trace, "`")), name = trace, type = 'scatter', mode =  'lines')
    #   }
    #   p %>% layout(xaxis = list(title = 'Måneder'), yaxis = list (title = 'Gennemsnitlig ventetid'), barmode = 'group')
    # })
    
    # betjeninger kategori / betjeningstid
    
    output$betjeninger_service_year_plot <- renderPlotly({
      data <- betjeninger_service_year %>%
        mutate_at(vars(1), funs(as.character(.))) %>%
        mutate_at(vars(1), funs(substr(., 1, 4)))
      plot_ly(data, x = ~year, y = ~avgservice, type = 'bar', marker = list(color = color5)) %>%
        layout(xaxis = list(title = 'År'), yaxis = list(title = 'Minutter'))
    })
    
    # betjeninger måned / betjeningstid
    
    output$betjeninger_service_month_plot <- renderPlotly({
      data <- betjeninger_service_month %>%
        arrange(month)
      plot_ly(data, x = ~month, y = ~avgservice, type = 'bar', marker = list(color = color5)) %>%
        layout(xaxis = list(title = 'Måned'), yaxis = list(title = 'Minutter'))
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
      mutate_all(funs(ifelse(is.na(.), 0,.)))
                      
    output$borgvsbib_plot <- renderPlotly({
      plot_ly(datadata) %>%
        add_lines(x = ~date, y = ~bibcount, name = 'Indgang Borgernes Hus', line = list(color = color3)) %>%
        add_lines(x = ~date, y = ~borgcount, name = 'Betjeninger Borgerservice', yaxis = "y2", line = list(color = color4)) %>%
        layout(
          xaxis = list(title="Dato"),
          yaxis = list(
            tickfont = list(color = color3),
            title = "Indgang Borgernes Hus"),
          yaxis2 = list(
            tickfont = list(color = color4),
            overlaying = "y",
            side = "right",
            title = "Betjeninger Borgerservice"
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
    callModule(heatmapWeekTable, id = "heat2", data = betjeninger_heat_1, type = "count")
    callModule(heatmapWeekTable, id = "heat3", data = betjeninger_heat_3, type = "mean")
    callModule(heatmapWeekTable, id = "heat4", data = betjeninger_heat_4, type = "mean")
    
    # KATEGORIER
    
    output$kategorier <- renderTable(kategorier)
    
  }