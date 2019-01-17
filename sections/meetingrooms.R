# UI

meetingroomsTabPanelUI <- function(id) {
  
  ns <- NS(id)

  tabItem(tabName = "meetingrooms",
          box(width = 12, solidHeader = TRUE, id="spaceheader2",
              h3("Mødelokaler"),
              img(src='icons/detfysiskerum_negativ_45x45.png', align = "right", height="46px")
          ),
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset1",
                          tabPanel("Generelt", 
                                   fluidRow(
                                     column(12,
                                            fluidRow(
                                              column(6,
                                            h4("Mødelokaleoversigt"),
                                            p("Oversigtstabellen viser aktiviteten i de enkelte mødelokaler i Borgernes Hus for den valgte periode."),
                                            p("Tabellen viser ”Antal møder”, ”Gennemsnitlig mødevarighed” – som er et udtryk for hvor langt møderne i snit varer i de enkelte lokaler, ”Total” – som er den totale tid mødelokalet har været optaget i perioden, samt ”Belægningsprocent” – der viser hvor meget af den totale tid mødelokalet har været booket."),
                                            p("Det er muligt at få vist oversigten ”Indenfor arbejdstid (8-16)”, ”udenfor arbejdstid (16-21)” eller ”hele åbningstiden”"),
                                            p("Den totale tid er defineret som ”hverdage mellem 8 og 21”, hvorfor weekender ikke er medtaget."),
                                            p("Diagrammet viser hvor mange møder, der er blevet vist på agendaskærmen i perioden.")
                                            )
                                   ),
                                       column(2,
                                              h4("Periode"),
                                              dateRangeInput(ns('dateRangeMeetingrooms'),
                                                             label = 'Vælg periode',
                                                             start = Sys.Date() - 90, end = Sys.Date(),
                                                             separator = " - "
                                              ),
                                              selectInput(ns("timeslot"), "Vælg tidspunkt på dagen",c('Indenfor arbejdstid, indtil kl. 16' = "1",'Udenfor arbejdstid, efter kl. 16' = "2",'Hele åbningstiden' = "3")),
                                              xlsxDownloadUI(ns("Mødelokaleoversigt")),
                                              tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                       ),
                                       column(width = 10,   
                                            column(width = 12, class = "col-lg-6",
                                                   h4("Oversigtstabel"),
                                                   withSpinner(tableOutput(ns("tablemeetingrooms_overview")))
                                            ),
                                            column(width = 12, class = "col-lg-6",
                                                   h4("Vist på agendaskærm"), 
                                                   withSpinner(plotlyOutput(ns("meetingrooms_agendascreen_plot")))
                                            )
                                            #,
                                            #column(width = 12, class = "col-lg-6",
                                            #       h4("Fordelingen af mødelængde"),
                                            #       plotlyOutput(ns("plot_meetingrooms_overview"))
                                            #       
                                            #)
                                       )
                                     ),
                                     column(12,tags$hr()),
                                     column(12,
                                        fluidRow(
                                           column(6,
                                                  h4("Bookingoversigt"),
                                                  p("Bookingoversigten viser top 10 over bookinger fordelt på afdelinger i en valgt periode."),
                                                  p("Andet” dækker over bookinger fra Odense Frivillighedscenter, bookinger fra Borgere, bookinger fra odense.dk brugere udenfor OBB. Fremover vil Odense Frivillighedscenter få særskilt kategori, og disse bookinger fremstå individuelt."),
                                                  p("Det er muligt at fravælge kategorien ”andet” for bedre at kunne se intern OBB brug af mødelokalerne.")
                                              )
                                           ),
                                            column(width = 2,
                                                   h4("Periode"),
                                                   dateRangeInput(ns('dateRangeMeetingrooms_booker'),
                                                                  label = 'Vælg periode',
                                                                  start = Sys.Date() - 90, end = Sys.Date(),
                                                                  separator = " - "
                                                   ),
                                                   selectInput(ns("timeslot_booker"), "Med/uden andet",c('Med andet' = "1",'Uden andet' = "2")),
                                                   xlsxDownloadUI(ns("Bookingoversigt")),
                                                   tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                                   ),
                                            column(width = 10,
                                                  column(width = 12, class = "col-lg-6",
                                                         h4("Booker top 10"),
                                                         withSpinner(tableOutput(ns("table_meetingrooms_booker")))),
                                                  column(width = 12, class = "col-lg-6",
                                                         h4("Booker top 10"),
                                                         withSpinner(plotlyOutput(ns("plot_pie_meetingrooms_booker")))
                                                  )
                                                  
                                            ),
                                            column(width = 12, class = "col-lg-6",
                                               p("Mødelokalebelægningen er udelukkende et udtryk for bookede lokaler. Det vil sige, at belægningsgraden ikke viser interessen for at booke et lokale i et givent tidsrum. Det betyder at det ikke er muligt at se om der er mange som har haft interesse for at booke lokale, hvor det allerede har været booket. Dette kan skævvride billedet over mod en lavere belægningsgrad, da der ikke differentieres mellem reel belægning og ønsket belægning.
                                                         Hvis en medarbejder eksempelvis har behov for at booke lokale 3.5 kl. 11, men det er booket til en konference, er det ikke sikkert at medarbejderen har mulighed for at flytte sin aktivitet til kl. 15, hvor der er ledige lokaler.
                                                         Dette vil også gøre sig gældende i det tilfælde, hvor en medarbejder har behov for at have flere deltagende til sit møde end ledige lokaler kan tilbyde. I det tilfælde er medarbejderen nødt til at finde en alternativ placering til sit møde.")
                                               
                                        )
                                     )
                                )
                          ),
                          tabPanel("Timeoversigt",
                                   fluidRow(
                                     column(width = 12,
                                            column(2,
                                                   h4("Periode"),
                                                   dateRangeInput(ns('dateRangeMeetingrooms2'),
                                                                  label = 'Vælg periode',
                                                                  start = Sys.Date() - 90, end = Sys.Date(),
                                                                  separator = " - "
                                                   ),
                                                   tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                            ),
                                            column(width = 10, style = "page-break-after: always;",  
                                                   column(width = 12,
                                                          h4("Timeoversigt"),
                                                          p("Grafen viser brugen af de enkelte mødelokaler i Borgerens Hus fordelt på timer. Der kan kun sammenlignes i den enkelte kolonne."),
                                                          p("Jo mørkere markering jo højere brug af lokalet. Hvis et møde strækker sig over mere end én klokketime tæller mødet i begge time intervaller."),
                                                          withSpinner(formattableOutput(ns("tablemeetingrooms_timeslots")), proxy.height="150px;"), #%>% withSpinner(color="#0dc5c1"),
                                                          h4("Heatmap"),
                                                          p("Grafen viser brugen af mødelokaler i Borgernes Hus fordelt på døgnet. Heatmappet er dermed en indikation af hvordan brugen af huset er i løbet af en dag. Der kan sammenlignes på tværs af kolonner og rækker."),
                                                          withSpinner(plotlyOutput(ns("meetingrooms_time_heatmap")))
                                                   )
                                            )
                                     )
                                   )  
                          )
                          #,tabPanel("Data og dokumentation",
                          #         fluidRow(
                          #           column(12,
                          #              h4("Forklaringer"),
                          #              p("Belægningsprocent er beregnet som 'Sum af belægning' / 'Total antal timer i perioden & tidsperioden'")
                          #           )
                          #         )  
                          #)
                          
                )
            )
        )
  )
  
}

# SERVER

meetingroomsTabPanel <- function(input, output, session, data, tablename) {
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  con_dwh <- dbConnect(drv, dbname = dbname_dwh, host = host_dwh, port = port_dwh, user = user_dwh, password = password_dwh)
  meetingrooms <- dbGetQuery(con_dwh, "SELECT * FROM lokaler.meetingrooms")
  employees <- dbGetQuery(con, "SELECT navn,  email, enhedsnavnniv5, enhedsnavnniv6 FROM web.ansatte")
  dbDisconnect(con)
  dbDisconnect(con_dwh)
  
  employees <- rbind(employees, c("Team Musik", "teammusik@odense.dk", "Team Musik", "Team Musik"))
  employees <- rbind(employees, c("Team Børn", "teamboern@odense.dk", "Team Børn", "Team Børn"))
  employees <- rbind(employees, c("Team Oplevelse", "teamoplevelse@odense.dk", "Team Oplevelse", "Team Oplevelse"))
  employees <- rbind(employees, c("Team Viden", "teamviden@odense.dk ", "Team Viden", "Team Viden"))
  
  meetingrooms <- meetingrooms %>%
    mutate(
      sted = case_when(
        meetingrooms$roomnumber == "lok11_borghus@odense.dk" ~ "Lokale 1.1",
        meetingrooms$roomnumber == "lok12_borghus@odense.dk" ~ "Lokale 1.2",
        meetingrooms$roomnumber == "lok13_borghus@odense.dk" ~ "Lokale 1.3",
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
  
  meetingrooms_overview <- reactive({
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
      rename(Lokalenummer = sted, Antal = count, "Gennemsnit" =	Median2, "Total(t)" =	sum, Belægning = timediff ) %>%
      select(Lokalenummer, Antal, "Gennemsnit", Belægning)
  })
  
  output$tablemeetingrooms_overview <- renderTable(
    meetingrooms_overview()
  )
  
  callModule(xlsxDownload, "Mødelokaleoversigt", data = reactive(meetingrooms_overview()), name = "Mødelokaleoversigt")
  
  meetingrooms_overview_box <- reactive({
    meetingrooms_overview <- meetingrooms %>%
      filter(startdate > input$dateRangeMeetingrooms[1] & startdate < input$dateRangeMeetingrooms[2]) %>%
      filter(if(input$timeslot == "1") hour(startdate) < 16 else if(input$timeslot == "2") hour(startdate) >= 16 else TRUE) %>%
      mutate(tid = 	as.integer((enddate - startdate))) %>%
      select(startdate, sted, tid) #%>%
      #spread(sted, tid) 
  })
  
  output$plot_meetingrooms_overview <- renderPlotly({
    meetingrooms_overview_box_11 <- meetingrooms_overview_box() %>% filter(sted == 'Lokale 1.1')
    meetingrooms_overview_box_12 <- meetingrooms_overview_box() %>% filter(sted == 'Lokale 1.2')
    meetingrooms_overview_box_13 <- meetingrooms_overview_box() %>% filter(sted == 'Lokale 1.3')
    meetingrooms_overview_box_21 <- meetingrooms_overview_box() %>% filter(sted == 'Lokale 2.1')
    meetingrooms_overview_box_22 <- meetingrooms_overview_box() %>% filter(sted == 'Lokale 2.2')
    meetingrooms_overview_box_31 <- meetingrooms_overview_box() %>% filter(sted == 'Lokale 3.1')
    meetingrooms_overview_box_32 <- meetingrooms_overview_box() %>% filter(sted == 'Lokale 3.2')
    meetingrooms_overview_box_33 <- meetingrooms_overview_box() %>% filter(sted == 'Lokale 3.3')
    meetingrooms_overview_box_34 <- meetingrooms_overview_box() %>% filter(sted == 'Lokale 3.4')
    meetingrooms_overview_box_35 <- meetingrooms_overview_box() %>% filter(sted == 'Lokale 3.5')
    meetingrooms_overview_box_36 <- meetingrooms_overview_box() %>% filter(sted == 'Lokale 3.6')
    plot_ly(y = meetingrooms_overview_box_11$tid, type = "box", name = meetingrooms_overview_box_11$sted) %>%
      add_trace(y = meetingrooms_overview_box_12$tid, name = meetingrooms_overview_box_12$sted) %>%
      add_trace(y = meetingrooms_overview_box_13$tid, name = meetingrooms_overview_box_13$sted) %>%
      add_trace(y = meetingrooms_overview_box_21$tid, name = meetingrooms_overview_box_21$sted) %>%
      add_trace(y = meetingrooms_overview_box_22$tid, name = meetingrooms_overview_box_22$sted) %>%
      add_trace(y = meetingrooms_overview_box_31$tid, name = meetingrooms_overview_box_31$sted) %>%
      add_trace(y = meetingrooms_overview_box_32$tid, name = meetingrooms_overview_box_32$sted) %>%
      add_trace(y = meetingrooms_overview_box_33$tid, name = meetingrooms_overview_box_33$sted) %>%
      add_trace(y = meetingrooms_overview_box_34$tid, name = meetingrooms_overview_box_34$sted) %>%
      add_trace(y = meetingrooms_overview_box_35$tid, name = meetingrooms_overview_box_35$sted) %>%
      add_trace(y = meetingrooms_overview_box_36$tid, name = meetingrooms_overview_box_36$sted) %>%
      layout(yaxis = list(title = "Minutter"))
  })
  
  # Vist på agendaskærm
  output$meetingrooms_agendascreen_plot <- renderPlotly({
    meetingrooms_agendascreen <- meetingrooms %>%
      filter(startdate > input$dateRangeMeetingrooms[1] & startdate < input$dateRangeMeetingrooms[2]) %>%
      select(show_on_screen) %>%
      group_by(show_on_screen) %>%
      summarise(count = n())
    plot_ly(meetingrooms_agendascreen, 
            labels = c("Ikke vist på skærm","Vist på skærm"), 
            values = ~count,
            text = ~paste(round((count / sum(count))*100, 0),"%",sep=""), # denne og følgende linje runder procenterne af, så de er uden decimaler
            textinfo='text',
            textfont = list(color = '#FFFFFF'), 
            marker = list(colors = colors, 
            line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = T, autosize = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  #mutate(akk = percent((akku2-akku1)/akku1, digits = 0))
  
  # Booker top 10
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
  
  callModule(xlsxDownload, "Bookingoversigt", data = reactive(meetingrooms_booker()), name = "Bookingoversigt")
  
  output$table_meetingrooms_booker <- renderTable({
    meetingrooms_booker <- meetingrooms_booker() %>% 
      mutate(bookingprocent = procenten(sum/totalsum)) %>%
      select(-totalsum)
  }, rownames = TRUE)
  
  output$plot_pie_meetingrooms_booker <- renderPlotly({
    meetingrooms_booker <- meetingrooms_booker() %>%
      mutate(bookingprocent = (sum/totalsum))
    plot_ly(meetingrooms_booker,
            labels = ~enhed,
            values = ~bookingprocent,
            text = ~paste(round((bookingprocent / sum(bookingprocent))*100, 0),"%",sep=""), # denne og følgende linje runder procenterne af, så de er uden decimaler
            textinfo='text',
            textfont = list(color = '#FFFFFF'),
            marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie() %>%
      layout(showlegend = T, autosize = T,
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

  meetingrooms_timeslots <- reactive ({
    meetingrooms_timeslots <- rækker(meetingrooms) %>%
      filter(startdate > input$dateRangeMeetingrooms2[1] & startdate < input$dateRangeMeetingrooms2[2]) %>%
      select(sted, startTidspunkt ) %>%
      group_by(sted, startTidspunkt) %>%
      summarise(count = n()) %>%
      spread(key = sted, value = count) %>%
      replace(., is.na(.), "0") 
  })
  
  output$tablemeetingrooms_timeslots <- renderFormattable({
    formattable(meetingrooms_timeslots(), list(
      'Lokale 1.1' = color_tile("white", "#aeb051"),
      'Lokale 1.2' = color_tile("white", "#aeb051"),
      'Lokale 1.3' = color_tile("white", "#aeb051"),
      'Lokale 2.1' = color_tile("white", "#aeb051"),
      'Lokale 2.2' = color_tile("white", "#aeb051"),
      'Lokale 3.1' = color_tile("white", "#aeb051"),
      'Lokale 3.2' = color_tile("white", "#aeb051"),
      'Lokale 3.3' = color_tile("white", "#aeb051"),
      'Lokale 3.4' = color_tile("white", "#aeb051"),
      'Lokale 3.5' = color_tile("white", "#aeb051"),
      'Lokale 3.6' = color_tile("white", "#aeb051")
    ))
  })

  #callModule(csvDownload, "csv_timeoversigt", data = meetingrooms_timeslots(), name = "timeoversigt")
  callModule(xlsxDownload, "xlsx_timeoversigt", data = reactive(meetingrooms_timeslots()), name = "timeoversigt")
  
  output$meetingrooms_time_heatmap <- renderPlotly({
    meetingrooms_timeslots <- rækker(meetingrooms) %>%
      filter(startdate > input$dateRangeMeetingrooms2[1] & startdate < input$dateRangeMeetingrooms2[2]) %>%
      select(sted, startTidspunkt ) %>%
      group_by(sted, startTidspunkt) %>%
      summarise(count = n()) %>%
      replace(., is.na(.), "0")
    
    plot_ly(x=meetingrooms_timeslots$sted, y=meetingrooms_timeslots$startTidspunkt, z = meetingrooms_timeslots$count, 
            colors = colorRamp(c("white", "#aeb051")), type = "heatmap", showscale = FALSE) %>%
            layout(xaxis = list(showgrid = FALSE, dtick = 1, side = 'top'), yaxis = list(showgrid = FALSE, dtick = 1, autorange = 'reversed', autosize = T))
  })
  
}