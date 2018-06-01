source("global.R")
source("modules.R")
source("functions.R")
source("~/.postpass")

# UI

meetingroomsTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "meetingrooms",
          box(width = 12, solidHeader = TRUE, id="spaceheader2",
              h3("Mødelokaler"),
              img(src='detfysiskerum.png', align = "right", height="46px")
          ),
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset1",
                          tabPanel("Generelt", 
                                   fluidRow(
                                     column(width = 12,
                                       column(2,
                                              h4("Periode"),
                                              dateRangeInput(ns('dateRangeMeetingrooms'),
                                                             label = 'Vælg periode',
                                                             start = Sys.Date() - 90, end = Sys.Date(),
                                                             separator = " - "
                                              ),
                                              selectInput(ns("timeslot"), "Vælg tidspunkt på dagen",c('Indenfor arbejdstid, indtil kl. 16' = "1",'Udenfor arbejdstid, efter kl. 16' = "2",'Hele åbningstiden' = "3")) 
                                       ),
                                       column(width = 10,   
                                            column(width = 6,
                                                   h4("Oversigtstabel"),
                                                   tableOutput(ns("tablemeetingrooms_overview"))
                                            ),
                                            column(width = 6,
                                                   h4("Vist på agendaskærm"), 
                                                   plotlyOutput(ns("meetingrooms_agendascreen_plot"))
                                            )
                                       )
                                     ),
                                     column(12,tags$hr()),
                                     column(width = 12,
                                            column(width = 2,
                                                   h4("Periode"),
                                                   dateRangeInput(ns('dateRangeMeetingrooms_booker'),
                                                                  label = 'Vælg periode',
                                                                  start = Sys.Date() - 90, end = Sys.Date(),
                                                                  separator = " - "
                                                   ),
                                                   selectInput(ns("timeslot_booker"), "Med/uden andet",c('Med andet' = "1",'Uden andet' = "2")) 
                                                   
                                                   ),
                                      column(width = 10,
                                            column(width = 6,
                                                   h4("Booker top 10"),
                                                   tableOutput(ns("table_meetingrooms_booker"))
                                            ),
                                            column(width = 6,
                                                   h4("Booker top 10"),
                                                   plotlyOutput(ns("plot_pie_meetingrooms_booker"))
                                            )
                                      )
                                     
                                )
                          )),
                          tabPanel("Timer",
                                   fluidRow(
                                     column(width = 12,
                                            column(2,
                                                   h4("Periode"),
                                                   dateRangeInput(ns('dateRangeMeetingrooms2'),
                                                                  label = 'Vælg periode',
                                                                  start = Sys.Date() - 90, end = Sys.Date(),
                                                                  separator = " - "
                                                   )
                                            ),
                                            column(width = 10,   
                                                   column(width = 12,
                                                          h4("Timetabel"),
                                                          p("Graduering pr. kolonner"),
                                                          formattableOutput(ns("tablemeetingrooms_timeslots")), #%>% withSpinner(color="#0dc5c1"),
                                                          h4("Heatmap"),
                                                          p("Graduering i hele figuren"),
                                                          plotlyOutput(ns("meetingrooms_time_heatmap"))
                                                   )
                                            )
                                     )
                                   )  
                          ),
                          tabPanel("Data og dokumentation",
                                   fluidRow(
                                     column(12,
                                        h4("Forklaringer"),
                                        p("Belægningsprocent er beregnet som 'Sum af belægning' / 'Total antal timer i perioden & tidsperioden'")
                                     )
                                   )  
                          )
                          
                )
            )
        )
  )
  
}

# SERVER

meetingroomsTabPanel <- function(input, output, session, data, tablename) {
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  meetingrooms <- dbGetQuery(con, "SELECT * FROM datamart.meetingrooms")
  employees <- dbGetQuery(con, "SELECT navn,  email, enhedsnavnniv5, enhedsnavnniv6 FROM web.ansatte")
  dbDisconnect(con)
  
  employees <- rbind(employees, c("Team Musik", "teammusik@odense.dk", "Team Musik", "Team Musik"))
  employees <- rbind(employees, c("Team Børn", "teamboern@odense.dk", "Team Børn", "Team Børn"))
  employees <- rbind(employees, c("Team Oplevelse", "teamoplevelse@odense.dk", "Team Oplevelse", "Team Oplevelse"))
  employees <- rbind(employees, c("Team Viden", "teamviden@odense.dk ", "Team Viden", "Team Viden"))
  
  meetingrooms <- meetingrooms %>%
    mutate(
      sted = case_when(
        meetingrooms$roomnumber == "lok11_borghus@odense.dk" ~ "Lokale 1.1",
        meetingrooms$roomnumber == "lok12_borghus@odense.dk" ~ "Lokale 1.2",
        meetingrooms$roomnumber == "lok21_borghus@odense.dk" ~ "Lokale 2.1",
        meetingrooms$roomnumber == "lok22_borghus@odense.dk" ~ "Lokale 2.2",
        meetingrooms$roomnumber == "lok31_borghus@odense.dk" ~ "Lokale 3.1",
        meetingrooms$roomnumber == "lok32_borghus@odense.dk" ~ "Lokale 3.2",
        meetingrooms$roomnumber == "lok33_borghus@odense.dk" ~ "Lokale 3.3",
        meetingrooms$roomnumber == "lok34_borghus@odense.dk" ~ "Lokale 3.4",
        meetingrooms$roomnumber == "lok35_borghus@odense.dk" ~ "Lokale 3.5",
        meetingrooms$roomnumber == "lok36_borghus@odense.dk" ~ "Lokale 3.6"
      ) 
    )
  
  # Oversigtstabel
  
  output$tablemeetingrooms_overview <- renderTable(
    meetingrooms_overview <- meetingrooms %>%
      filter(startdate > input$dateRangeMeetingrooms[1] & startdate < input$dateRangeMeetingrooms[2]) %>%
      filter(if(input$timeslot == "1") hour(startdate) < 16 else if(input$timeslot == "2") hour(startdate) >= 16 else TRUE) %>%
      mutate(tid = 	as.integer((enddate - startdate))) %>%
      select(sted, tid) %>%
      group_by(sted) %>%
      summarise(count = n(), sum = sum(tid)/60, median = median(tid)/60) %>%
      mutate(Median2 = sprintf("%02d:%02d",(median*60)%/%60,(median*60)%%60)) %>%
      select(-median) %>%
      mutate(timediff = 
               if(input$timeslot == "1") procenten(sum/((Nweekdays(input$dateRangeMeetingrooms[1], input$dateRangeMeetingrooms[2])*8)))
             else if (input$timeslot == "2") procenten(sum/((Nweekdays(input$dateRangeMeetingrooms[1], input$dateRangeMeetingrooms[2])*5)))
             else procenten(sum/((Nweekdays(input$dateRangeMeetingrooms[1], input$dateRangeMeetingrooms[2])*13)))
      ) %>%
      
      rename(Lokalenummer = sted, Antal = count, Median =	Median2, "Total(t)" =	sum, Belægningsprocent = timediff ) %>%
      select(Lokalenummer, Antal, Median, "Total(t)", Belægningsprocent)
  )
  
  # Vist på agendaskærm
  
  output$meetingrooms_agendascreen_plot <- renderPlotly({
    meetingrooms_agendascreen <- meetingrooms %>%
      filter(startdate > input$dateRangeMeetingrooms[1] & startdate < input$dateRangeMeetingrooms[2]) %>%
      select(show_on_screen) %>%
      group_by(show_on_screen) %>%
      summarise(count = n())
    plot_ly(meetingrooms_agendascreen, labels = c("Ikke vist på skærm","Vist på skærm"), values = ~count, marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # booker
  meetingrooms_booker <- reactive({
    meetingrooms_booker <- meetingrooms %>%
      filter(startdate > input$dateRangeMeetingrooms_booker[1] & startdate < input$dateRangeMeetingrooms_booker[2]) %>%
      select(forfatter_mail) %>%
      mutate(forfatter_mail = tolower(forfatter_mail)) %>%
      group_by(forfatter_mail) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      rename(Booker = forfatter_mail, Antal = count) %>%
      left_join(employees, by = c("Booker" = "email")) %>%
      mutate(enhed = ifelse(is.na(enhedsnavnniv6) & is.na(enhedsnavnniv5), "Andet", ifelse(is.na(enhedsnavnniv6), enhedsnavnniv5, enhedsnavnniv6))) %>%
      filter(if(input$timeslot_booker != "1") enhed != "Andet" else TRUE) %>%
      select(enhed, Antal) %>%
      group_by(enhed) %>%
      summarise(sum = sum(Antal)) %>%
      arrange(desc(sum)) %>%
      mutate(totalsum = sum(sum)) %>%
      head(10) 
  })
  
  output$table_meetingrooms_booker <- renderTable({
    meetingrooms_booker <- meetingrooms_booker() %>% 
      mutate(bookingprocent = procenten(sum/totalsum)) %>%
      select(-totalsum)
  })
  
  output$plot_pie_meetingrooms_booker <- renderPlotly({
    meetingrooms_booker <- meetingrooms_booker() %>%
      mutate(bookingprocent = (sum/totalsum))
    plot_ly(meetingrooms_booker, labels = ~enhed, values = ~bookingprocent, marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie() %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # Timetabel
  
  rækker <- function(meetingrooms){
    
    meetingrooms_time <- meetingrooms %>%
      mutate(startTidspunkt = hour(format(as.POSIXct(startdate)))) %>%
      mutate(slutTidspunkt = hour(format(as.POSIXct(enddate)))) %>%
      select(sted, startTidspunkt, slutTidspunkt, startdate ) %>%
      mutate(timer = (slutTidspunkt-startTidspunkt))
    
    for (i in 1:nrow(meetingrooms_time)){
      for(x in 1:meetingrooms_time$timer[i]) {
        sted <- meetingrooms_time$sted[i]
        startTidspunkt <- meetingrooms_time$startTidspunkt[i]+x
        slutTidspunkt <- meetingrooms_time$slutTidspunkt[i]
        startdate <- meetingrooms_time$startdate[i]
        timer <- meetingrooms_time$timer[i]
        index <- i
        dfen <- data.frame(sted, startTidspunkt, slutTidspunkt, startdate, timer)
        meetingrooms_time <- rbind(meetingrooms_time, dfen)
      }
    }
    return(meetingrooms_time)
  }
  
  output$tablemeetingrooms_timeslots <- renderFormattable({
    meetingrooms_timeslots <- rækker(meetingrooms) %>%
      filter(startdate > input$dateRangeMeetingrooms2[1] & startdate < input$dateRangeMeetingrooms2[2]) %>%
      select(sted, startTidspunkt ) %>%
      group_by(sted, startTidspunkt) %>%
      summarise(count = n()) %>%
      spread(key = sted, value = count) %>%
      replace(., is.na(.), "0") 
    formattable(meetingrooms_timeslots, list(
      'Lokale 1.1' = color_tile("white", "CadetBlue"),
      'Lokale 1.2' = color_tile("white", "CadetBlue"),
      'Lokale 2.1' = color_tile("white", "CadetBlue"),
      'Lokale 2.2' = color_tile("white", "CadetBlue"),
      'Lokale 3.1' = color_tile("white", "CadetBlue"),
      'Lokale 3.2' = color_tile("white", "CadetBlue"),
      'Lokale 3.3' = color_tile("white", "CadetBlue"),
      'Lokale 3.4' = color_tile("white", "CadetBlue"),
      'Lokale 3.5' = color_tile("white", "CadetBlue"),
      'Lokale 3.6' = color_tile("white", "CadetBlue")
    ))
  })

  output$meetingrooms_time_heatmap <- renderPlotly({
    meetingrooms_timeslots <- rækker(meetingrooms) %>%
      filter(startdate > input$dateRangeMeetingrooms2[1] & startdate < input$dateRangeMeetingrooms2[2]) %>%
      select(sted, startTidspunkt ) %>%
      group_by(sted, startTidspunkt) %>%
      summarise(count = n()) %>%
      replace(., is.na(.), "0")
    
    plot_ly(x=meetingrooms_timeslots$sted, y=meetingrooms_timeslots$startTidspunkt, z = meetingrooms_timeslots$count, 
            colors = colorRamp(c("white", "CadetBlue")), type = "heatmap", showscale = FALSE) %>%
            layout(xaxis = list(showgrid = FALSE, dtick = 1, side = 'top'), yaxis = list(showgrid = FALSE, dtick = 1, autorange = 'reversed'))
  })
  
}