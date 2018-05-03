source("global.R")
source("modules.R")
source("~/.postpass")

# UI

eventareasTabPanelUI <- function(id) {
  
  ns <- NS(id)
 
  tabItem(tabName = "eventareas",
          box(width = 12, solidHeader = TRUE, id="spaceheader3",
              h3("Eventområder"),
              img(src='detfysiskerum.png', align = "right", height="46px")
          ),
          box(width = 12,
              column(2,
                     h4("Periode"),
                     dateRangeInput(ns('dateRangeBhus_events'),
                                    label = 'Vælg periode',
                                    start = Sys.Date() - 90, end = Sys.Date(),
                                    separator = " - "
                     )
              ),
              column(width = 10,
                     column(width = 6,
                            h4("Oversigtstabel"),
                            tableOutput(ns("tablebhus_events_overview"))
                     ),
                     column(width = 6,
                            h4("Vist på agendaskærm"), 
                            plotlyOutput(ns("bhus_events_agendascreen_plot"))
                     ),
                     column(width = 4,
                            h4("Booker top 10"),
                            tableOutput(ns("table_bhus_events_booker"))
                     ),
                     column(width = 8,
                            h4("Oversigtstabel"),
                            formattableOutput(ns("tablebhus_events"))
                     )
              )
          )
  )
   
}

# SERVER

eventareasTabPanel <- function(input, output, session, data, tablename) {
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  bhus_events <- dbGetQuery(con, "SELECT * FROM datamart.bhus_events")
  dbDisconnect(con)
  
  output$tablebhus_events_overview <- renderTable(
    bhus_events_overview <- bhus_events %>%
      filter(startdate > input$dateRangeBhus_events[1] & startdate < input$dateRangeBhus_events[2]) %>%
      mutate(tid = 	as.integer((slut - startdate))) %>%
      select(location, tid) %>%
      group_by(location) %>%
      summarise(count = n(), mean = mean(tid), median = median(tid), sum = sum(tid)/60 ) %>%
      mutate(timediff = percent(count/(as.integer(input$dateRangeBhus_events[2] - input$dateRangeBhus_events[1])*14)*100)) %>%
      rename(Lokation = location, Antal = count, Gennemsnit =	mean, Middelværdi =	median, Total =	sum, Belægninsprocent = timediff )  
  )
  
  output$bhus_events_agendascreen_plot <- renderPlotly({
    bhus_events_agendascreen <- bhus_events %>%
      filter(startdate > input$dateRangeBhus_events[1] & startdate < input$dateRangeBhus_events[2]) %>%
      select(show_on_screen) %>%
      group_by(show_on_screen) %>%
      summarise(count = n())
    plot_ly(bhus_events_agendascreen, labels = c("Ikke vist på skærm","Vist på skærm"), values = ~count, marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$table_bhus_events_booker <- renderTable(
    bhus_events_booker <- bhus_events %>%
      filter(startdate > input$dateRangeBhus_events[1] & startdate < input$dateRangeBhus_events[2]) %>%
      select(forfatter_mail) %>%
      group_by(forfatter_mail) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(10) %>%
      rename(Booker = forfatter_mail, Antal = count)
  )
  
  output$tablebhus_events <- renderFormattable({
    bhus_events_timeslots <- bhus_events %>%
      filter(startdate > input$dateRangeBhus_events[1] & startdate < input$dateRangeBhus_events[2]) %>%
      select(location, startdate ) %>%
      mutate(Tidspunkt = hour(format(as.POSIXct(startdate)))) %>%
      group_by(location, Tidspunkt) %>%
      summarise(count = n()) %>%
      spread(key = location, value = count) %>%
      replace(., is.na(.), "0")
    formattable(bhus_events_timeslots, list('Lokale 1.1' = color_tile("grey", '#468c8c')))}
  )
  
}