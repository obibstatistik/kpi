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
                     )
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
                     column(width = 4,
                            h4("Booker top 10"),
                            tableOutput(ns("table_meetingrooms_booker"))
                     ),
                     column(width = 8,
                            h4("Oversigtstabel"),
                            formattableOutput(ns("tablemeetingrooms_timeslots"))
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
  
  output$tablemeetingrooms_overview <- renderTable(
    meetingrooms_overview <- meetingrooms %>%
      filter(startdate > input$dateRangeMeetingrooms[1] & startdate < input$dateRangeMeetingrooms[2]) %>%
      mutate(tid = 	as.integer((enddate - startdate))) %>%
      select(sted, tid) %>%
      group_by(sted) %>%
      summarise(count = n(), mean = mean(tid), median = median(tid), sum = sum(tid)/60 ) %>%
      mutate(timediff = percent(count/(as.integer(input$dateRangeMeetingrooms[2] - input$dateRangeMeetingrooms[1])*14)*100)) %>%
      rename(Lokalenummer = sted, Antal = count, Gennemsnit =	mean, Middelværdi =	median, Total =	sum, Belægningsprocent = timediff )  
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
  
  output$table_meetingrooms_booker <- renderTable(
    meetingrooms_booker <- meetingrooms %>%
      filter(startdate > input$dateRangeMeetingrooms[1] & startdate < input$dateRangeMeetingrooms[2]) %>%
      select(forfatter_mail) %>%
      group_by(forfatter_mail) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(10) %>%
      rename(Booker = forfatter_mail, Antal = count)
  )
  
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
  
}