source("global.R")
source("modules.R")
source("~/.postpass")
source("functions.R")

# UI

eventareasTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "eventareas",
          box(width = 12, solidHeader = TRUE, id="spaceheader3",
              h3("Eventområder"),
              img(src='icons/detfysiskerum_negativ_45x45.png', align = "right", height="46px")
          ),
          
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset1",
                          tabPanel("Generelt", 
                                   fluidRow(
                                     column(width = 12,
                                            fluidRow(
                                              column(6,
                                                     h4("Eventområde oversigt"),
                                                     p("Oversigtstabellen viser aktiviteten i de enkelte event områder i Borgernes Hus for den valgte periode. Tabellen viser ”Antal møder” ”Median” – som er et udtryk for hvor langt møderne i snit varer i de enkelte lokaler, ”Total” – som er den totale tid mødelokalet har været optaget i perioden, samt ”Belægningsprocent” – der viser hvor meget af den totale tid mødelokalet har været booket."),
                                                     p("Grafen viser hvor mange events, der er blevet vist på agendaskærmen i perioden.")
                                              )
                                            ),
                                            column(2,
                                                   h4("Periode"),
                                                   dateRangeInput(ns('dateRangeBhus_events'),
                                                                  label = 'Vælg periode',
                                                                  start = Sys.Date() - 90, end = Sys.Date(),
                                                                  separator = " - "
                                                   )
                                            ),
                                            column(width = 10,
                                                   column(width = 12, class = "col-lg-7",
                                                          h4("Oversigtstabel"),
                                                          tableOutput(ns("tablebhus_events_overview")
                                                         )),
                                                   column(width = 12, class = "col-lg-5",
                                                           h4("Vist på agendaskærm"), 
                                                           plotlyOutput(ns("bhus_events_agendascreen_plot"))
                                                          )
                                                   )
                                                  
                                           ),
                                     column(12,tags$hr()),
                                     column(6,
                                            h4("Bookingoversigt"),
                                            p("Bookingoversigten viser top 10 over bookinger fordelt på afdelinger i en valgt periode."),
                                            p("”Andet” dækker over bookinger fra Odense Frivillighedscenter, bookinger fra Borgere, bookinger fra odense.dk brugere udenfor OBB. Fremover vil Odense Frivillighedscenter få særskilt kategori, og disse bookinger fremstå individuelt."),
                                            p("Det er muligt at fravælge kategorien ”andet” for bedre at kunne se intern OBB brug af mødelokalerne.")
                                     ),
                                     column(width = 12,
                                            column(width = 2),
                                            column(width = 10,
                                                   column(width = 12, class = "col-lg-6",
                                                          h4("Booker top 10"),
                                                          tableOutput(ns("table_bhus_events_booker"))
                                                   ),
                                                   column(width = 12, class = "col-lg-6",
                                                          h4("Booker top 10"),
                                                          plotlyOutput(ns("plot_pie_eventarea_booker"))
                                                   )
                                            )
                                     )
                                     
                                   )),
                          tabPanel("Timer",
                                   fluidRow(
                                     column(width = 12,
                                            column(2,
                                                   h4("Periode"),
                                                   dateRangeInput(ns('dateRangebhus_events2'),
                                                                  label = 'Vælg periode',
                                                                  start = Sys.Date() - 90, end = Sys.Date(),
                                                                  separator = " - "
                                                   )
                                            ),
                                            column(width = 10,   
                                                   column(width = 12,
                                                          h4("Timetabel"),
                                                          p("Graduering pr. kolonner"),
                                                          formattableOutput(ns("tablebhus_events_timeslots")), #%>% withSpinner(color="#0dc5c1"),
                                                          h4("Heatmap"),
                                                          p("Graduering i hele figuren"),
                                                          plotlyOutput(ns("bhus_events_time_heatmap"))
                                                   )
                                            )
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

# SERVER

eventareasTabPanel <- function(input, output, session, data, tablename) {
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  bhus_events <- dbGetQuery(con, "SELECT forfatter_navn, forfatter_mail, location, slut, location as sted, show_on_screen, slut as enddate, startdate, subject  FROM datamart.bhus_events")
  bhus_events2 <- dbGetQuery(con, "SELECT * FROM datamart.bhus_events")
  employees <- dbGetQuery(con, "SELECT navn, email, enhedsnavnniv5, enhedsnavnniv6 FROM web.ansatte")
  dbDisconnect(con)
  
  # Oversigtstabel
  output$tablebhus_events_overview <- renderTable(
    bhus_events_overview <- bhus_events %>%
      filter(startdate > input$dateRangeBhus_events[1] & startdate < input$dateRangeBhus_events[2]) %>%
      mutate(tid = as.integer((slut - startdate))) %>%
      select(location, tid) %>%
      group_by(location) %>%
      summarise(count = n(), median = median(tid), sum = sum(tid) ) %>%
      mutate(timediff = procenten(sum/((Nweekdays(input$dateRangeBhus_events[1], input$dateRangeBhus_events[2])*13)))) %>%
      rename(Lokation = location, Antal = count, Median =	median, 'Total(t)' =	sum, Belægningsprocent = timediff )  
  )
  
  # Vist på agendaskærm
  output$bhus_events_agendascreen_plot <- renderPlotly({
    bhus_events_agendascreen <- bhus_events %>%
      filter(startdate > input$dateRangeBhus_events[1] & startdate < input$dateRangeBhus_events[2]) %>%
      select(show_on_screen) %>%
      group_by(show_on_screen) %>%
      summarise(count = n())
    plot_ly(bhus_events_agendascreen, labels = c("Ikke vist på skærm","Vist på skærm"), values = ~count, textfont = list(color = '#FFFFFF'), marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # Booker top 10
  
  eventarea_booker <- reactive({
    bhus_events_booker <- bhus_events %>%
        filter(startdate > input$dateRangeBhus_events[1] & startdate < input$dateRangeBhus_events[2]) %>%
        select(forfatter_mail) %>%
        mutate(forfatter_mail = tolower(forfatter_mail)) %>%
        group_by(forfatter_mail) %>%
        summarise(count = n()) %>%
        arrange(desc(count)) %>%
        rename(Booker = forfatter_mail, Antal = count) %>%
        left_join(employees, by = c("Booker" = "email")) %>%
        mutate(enhed = ifelse(is.na(enhedsnavnniv6) & is.na(enhedsnavnniv5), "Andet", ifelse(is.na(enhedsnavnniv6), enhedsnavnniv5, enhedsnavnniv6))) %>%
        select(enhed, Antal) %>%
        group_by(enhed) %>%
        summarise(sum = sum(Antal)) %>%
        arrange(desc(sum)) %>%
        mutate(totalsum = sum(sum)) %>%
        head(10)
    })
    
  output$table_bhus_events_booker <- renderTable(
    bhus_events_booker <- eventarea_booker() %>%
      mutate(bookingprocent = procenten(sum/totalsum)) %>%
      select(-totalsum), rownames = TRUE
  )
  
  output$plot_pie_eventarea_booker <- renderPlotly({
    eventarea_booker <- eventarea_booker() %>%
      mutate(bookingprocent = (sum/totalsum))
    plot_ly(eventarea_booker, labels = ~enhed, values = ~bookingprocent, textfont = list(color = '#FFFFFF'), marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie() %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  # Timetabel

  rækker <- function(bhus_events){

    bhus_events_time <- bhus_events %>%
      mutate(startTidspunkt = hour(format(as.POSIXct(startdate)))) %>%
      mutate(slutTidspunkt = hour(format(as.POSIXct(enddate)))) %>%
      select(sted, startTidspunkt, slutTidspunkt, startdate ) %>%
      mutate(timer = (slutTidspunkt-startTidspunkt))

    for (i in 1:nrow(bhus_events_time)){
      for(x in 1:bhus_events_time$timer[i]) {
        sted <- bhus_events_time$sted[i]
        startTidspunkt <- bhus_events_time$startTidspunkt[i]+x
        slutTidspunkt <- bhus_events_time$slutTidspunkt[i]
        startdate <- bhus_events_time$startdate[i]
        timer <- bhus_events_time$timer[i]
        index <- i
        dfen <- data.frame(sted, startTidspunkt, slutTidspunkt, startdate, timer)
        bhus_events_time <- rbind(bhus_events_time, dfen)
      }
    }
    return(bhus_events_time)
  }
  
  output$test <- renderTable({rækker(bhus_events) %>% arrange(startdate, sted, startTidspunkt) })
  
  output$tablebhus_events_timeslots <- renderFormattable({
    bhus_events_timeslots <- rækker(bhus_events) %>%
      filter(startdate > input$dateRangebhus_events2[1] & startdate < input$dateRangebhus_events2[2]) %>%
      select(sted, startTidspunkt ) %>%
      group_by(sted, startTidspunkt) %>%
      summarise(count = n()) %>%
      spread(key = sted, value = count) %>%
      replace(., is.na(.), "0") 
    formattable(bhus_events_timeslots, 
      list(
        'Borgernes Torv (1. sal)' = color_tile("white", "CadetBlue"),
        'Borgernes værksted (2. sal)' = color_tile("white", "CadetBlue"),
        'Kantine området (3. sal)' = color_tile("white", "CadetBlue"),
        'Multimøbel / Borgernes Torv (1. sal)' = color_tile("white", "CadetBlue"),
        'Multimøbel / Borgernes Værksted (2. sal)' = color_tile("white", "CadetBlue"),
        'Musikscenen (2. sal)' = color_tile("white", "CadetBlue"),
        'Scene (stuen)' = color_tile("white", "CadetBlue"),
        'Store scene (café 2. sal)' = color_tile("white", "CadetBlue"),
        'Udstillingsområde (stuen)' = color_tile("white", "CadetBlue")
      )
    )
  })
  
  output$bhus_events_time_heatmap <- renderPlotly({
    bhus_events_timeslots <- rækker(bhus_events) %>%
      filter(startdate > input$dateRangebhus_events2[1] & startdate < input$dateRangebhus_events2[2]) %>%
      select(sted, startTidspunkt ) %>%
      group_by(sted, startTidspunkt) %>%
      summarise(count = n()) %>%
      replace(., is.na(.), "0")
    
    plot_ly(x=bhus_events_timeslots$sted, y=bhus_events_timeslots$startTidspunkt, z = bhus_events_timeslots$count, 
            colors = colorRamp(c("white", "CadetBlue")), type = "heatmap", showscale = FALSE) %>%
      layout(xaxis = list(showgrid = FALSE, dtick = 1, side = 'top'), yaxis = list(showgrid = FALSE, dtick = 1, autorange = 'reversed'), margin = list(l = 50, r = 50, b = 50, t = 150))
  })
  
}