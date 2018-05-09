source("global.R")
source("modules.R")
source("~/.postpass")

# UI

meetingroomsTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "meetingrooms",
          box(width = 12, solidHeader = TRUE, id="spaceheader2",
              h3("Mødelokaler"),
              img(src='detfysiskerum.png', align = "right", height="46px")
          ),
          box(width = 12,
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
                     ),
                     column(width = 12,
                            h4("Oversigtstabel"),
                            formattableOutput(ns("tablemeetingrooms_timeslots"))
                     ),
                     column(width = 6,
                            h4("Booker top 10"),
                            tableOutput(ns("table_meetingrooms_booker"))
                     ),
                     column(width = 6,
                            h4("Emnefelt"),
                            tableOutput(ns("table_meetingrooms_title"))
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

  Nweekdays <- Vectorize(function(a, b) 
    sum(!weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday")))
  
  output$tablemeetingrooms_overview <- renderTable(
    meetingrooms_overview <- meetingrooms %>%
      filter(startdate > input$dateRangeMeetingrooms[1] & startdate < input$dateRangeMeetingrooms[2]) %>%
      filter(if(input$timeslot == "1") hour(startdate) < 16 else if(input$timeslot == "2") hour(startdate) >= 16 else TRUE) %>%
      mutate(tid = 	as.integer((enddate - startdate))) %>%
      select(sted, tid) %>%
      group_by(sted) %>%
      summarise(count = n(), median = median(tid)/60, sum = sum(tid)/60 ) %>%
      mutate(timediff = 
               if(input$timeslot == "1") sum/((Nweekdays(input$dateRangeMeetingrooms[1], input$dateRangeMeetingrooms[2])*8))*100
               else if (input$timeslot == "2") sum/((Nweekdays(input$dateRangeMeetingrooms[1], input$dateRangeMeetingrooms[2])*5))*100
               else sum/((Nweekdays(input$dateRangeMeetingrooms[1], input$dateRangeMeetingrooms[2])*13))*100 
             ) %>%
      rename(Lokalenummer = sted, Antal = count, "Median" =	median, "Total(t)" =	sum, Belægningsprocent = timediff )  
  )

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
  
  output$tablemeetingrooms_timeslots <- renderFormattable({
    meetingrooms_timeslots <- meetingrooms %>%
      filter(startdate > input$dateRangeMeetingrooms[1] & startdate < input$dateRangeMeetingrooms[2]) %>%
      select(sted, startdate ) %>%
      mutate(Tidspunkt = hour(format(as.POSIXct(startdate)))) %>%
      group_by(sted, Tidspunkt) %>%
      summarise(count = n()) %>%
      spread(key = sted, value = count) %>%
      replace(., is.na(.), "0")
    formattable(meetingrooms_timeslots, list('Lokale 1.1' = color_tile("grey", '#468c8c')))}
  )
  
  output$table_meetingrooms_booker <- renderTable(
    meetingrooms_booker <- meetingrooms %>%
      filter(startdate > input$dateRangeMeetingrooms[1] & startdate < input$dateRangeMeetingrooms[2]) %>%
      select(forfatter_mail) %>%
      group_by(forfatter_mail) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(10) %>%
      rename(Booker = forfatter_mail, Antal = count) %>%
      left_join(employees, by = c("Booker" = "email"))
  )
  
  output$table_meetingrooms_title <- renderTable(
    meetingrooms_title <- meetingrooms %>%
      filter(startdate > input$dateRangeMeetingrooms[1] & startdate < input$dateRangeMeetingrooms[2]) %>%
      select(subject) %>%
      group_by(subject) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
  )
  
}