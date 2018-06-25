source("global.R")
source("modules.R")
source("~/.postpass")

# UI

eventsTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "events",
          box(width = 12, solidHeader = TRUE, id="eventsheader",
              h3("Arrangementer"),
              img(src='icons/arrangementer_negativ_45x45.png', align = "right", height="45px")
          ),
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset3",
                          tabPanel("Antal",
                                   fluidRow(width = 12,
                                            column(width = 12,
                                                   column(width = 6,
                                                   h4("Arrangementer pr. år"),
                                                   p("Graferne viser det samlede antal arrangementer og undervisning/læringsseancer afholdt på OBB, samt antal deltagere i arrangementer pr. år i de sidste 5 år."),
                                                   plotlyOutput(ns("eventsyearplot"))),
                                                   column(width = 6,
                                                   h4("Deltagere pr. år"),
                                                   p("Antal deltagere pr. år de sidste 5 år"),
                                                   plotlyOutput(ns("eventsparticipantyearplot")))
                                            ),
                                            column(12,tags$hr()),
                                            column(width = 12,
                                                   #column(width = 2,
                                                          # h4("Afgrænser"),
                                                          # checkboxGroupInput(ns("event_year"), label = 'Vælg år', 
                                                          #   selected = list("2017"),
                                                          #   choices = list("2018","2017","2016","2015","2014"))
                                                   #),
                                                   column(width = 6,
                                                          h4("Arrangementer pr. måned"),
                                                          p("Graferne viser antal afholdte arrangementer, samt antal deltagere fordelt pr. måned. Det er muligt at vælge år via ”vælgeren” i venstre side – Det er også muligt at sammenligne to år via valg i drop down menuer"),
                                                          plotlyOutput(ns("eventsmonthplot"))),
                                                   column(width = 6,
                                                          h4("Deltagere pr. måned"),
                                                          p("Antal deltagere pr. måned de sidste 5 år"),
                                                          plotlyOutput(ns("eventsparticipantmonthplot")))
                                                   ),
                                            column(12,tags$hr()),
                                            column(12,
                                                   h4("Deltagere pr. år fordelt på arrangementstype"),
                                                   plotlyOutput(ns("test2")),
                                                   plotlyOutput(ns("test3"))
                                                   )
                                   )       
                          ),
                          tabPanel("Type",
                                   fluidRow(
                                     column(6,
                                            h4("Info"),
                                            p("Den første graf viser fordelingen pr. år mellem børne og voksen arrangementer. Den anden graf fordeling i arrangementskategorier. Det tredje diagram viser afholdte arrangementer på hvert bibliotek. Det er muligt at skifte mellem forskellige år via ”vælgeren” i venstre side.")
                                            )
                                   ),
                                   fluidRow(
                                     column(2,
                                            h4("Afgræns pr. år"),
                                            selectInput(ns("year"), "",c('Alle','2013','2014','2015','2016','2017'))
                                     ),
                                     column(10,
                                            column(width = 4,
                                                   h4("Arrangementer børn/voksen"),
                                                   p("Antal arrangementer målrettet hhv. børn og voksne i perioden"),
                                                   plotlyOutput(ns("eventsmaalgruppeplot"))
                                            ),
                                            column(width = 4,
                                                   h4("Kategori"),
                                                   p("Antal arrangementer målrettet hhv. arrangementer og læring/undervisning i perioden"),
                                                   plotlyOutput(ns("eventskategoriplot"))
                                            ),
                                            column(width = 4,
                                                   h4("Sted"),
                                                   p("Antal arrangementer på de enkelte biblioteker i perioden"),
                                                   plotlyOutput(ns("eventsstedplot"))
                                            ),
                                            column(width = 12,
                                                   h4("Type"),
                                                   p("Antal deltager pr. arrangementstype i perioden"),
                                                   plotlyOutput(ns("eventstypeplot"))
                                            )
                                     )
                                   )       
                          ),
                          tabPanel("Effekt",
                                   fluidRow(
                                     column(12,
                                           h4("Forhold imellem forberedelse og deltagere"),
                                            p("Grafen viser forholdet mellem forberedelsestid og antal deltagere fordelt på arrangementskategorier. Holdes musen henover det enkelte arrangement vises titel og specifikke data."),
                                            p("OBS. Maks mulige forberedelsestid i indberetning er 999 minutter."),
                                            p("Ved at klikke én gang på en arrangementstype fravælges denne i visningen. Ved at dobbeltklikke på en arrangementstype vises kun denne."), 
                                       column(2,      
                                            #selectInput(ns("effectyear"), "",c('Alle','2013','2014','2015','2016','2017','2018')),
                                            h4("Afgræns"),
                                            checkboxGroupInput(ns("effectyear"), label = 'Vælg år:', 
                                                               selected = list('Alle','2013','2014','2015','2016','2017','2018'),
                                                               choices = list('Alle','2013','2014','2015','2016','2017','2018')),
                                            radioButtons(ns("yakse"), "Vælg Y akse:",
                                                         c("Forberedelsestid" = "forberedelsestid",
                                                           "ID" = "id")
                                            )
                                       ),  
                                       column(width = 10,
                                              plotlyOutput(ns("eventsratioplot"))
                                     ) )
                                   )
                          ),
                          tabPanel("Dokumentation og data",
                            metaTabPanelUI(ns("events"))
                          )  
                    )
            )
          )
  )
  
}

# SERVER

eventsTabPanel <- function(input, output, session, data, tablename) {

  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
    events <- dbGetQuery(con, "SELECT * FROM datamart.arrangementer")
    eventsmaalgruppe <- dbGetQuery(con, "select extract(year from dato) as year, maalgruppe, count(*) from datamart.arrangementer group by maalgruppe, year")
    eventsyear <- dbGetQuery(con, "select extract(year from dato) as year, count(*) from datamart.arrangementer where extract(year from dato) > 2012 group by year order by year")
    #eventsmonth <- dbGetQuery(con, "select extract(month from dato) as month, count(*) from datamart.arrangementer where extract(year from dato) > 2012 group by month order by month")
    eventsdeltagere <- dbGetQuery(con, "select sum(deltagere), extract(year from dato) as year from datamart.arrangementer where extract(year from dato) > 2012 group by year order by year")
    #eventsparticipantmonth <- dbGetQuery(con, "select sum(deltagere), extract(month from dato) as month from datamart.arrangementer where extract(year from dato) > 2012 group by month order by month")
    eventssted <- dbGetQuery(con, "select lokation, extract(year from dato) as year, count(*) from datamart.arrangementer group by lokation, year")
    eventskategori <- dbGetQuery(con, "select kategori, extract(year from dato) as year, count(*) from datamart.arrangementer group by kategori, year")
    eventsratio <- dbGetQuery(con, "select titel, arrangementstype, deltagere, forberedelsestid from datamart.arrangementer where arrangementstype != ''")
  dbDisconnect(con)
  
  # arrangementer pr aar #
  output$eventsyearplot <- renderPlotly({
    plot_ly(eventsyear, x = eventsyear$year, y = eventsyear$count, type = 'bar', text = text, marker = list(color = color1)) 
  })
  
  # deltagere pr aar #
  output$eventsparticipantyearplot <- renderPlotly({
    plot_ly(eventsdeltagere, x = eventsdeltagere$year, y = eventsdeltagere$sum, type = 'bar', text = text, marker = list(color = color1)) 
  })
  
  eventspermonth <- reactive({
    eventspermonth <- events %>%
      select(deltagere, dato) %>%
      mutate(aar = year(dato), maaned = month(dato)) %>%
      select(-dato)
  }) 
  
  # arrangementer pr maaned #
  output$eventsmonthplot <- renderPlotly({
    eventsmonth2 <- eventspermonth() %>%
      select(-deltagere) %>%
      group_by(aar, maaned) %>%
      summarise(count = n()) %>% 
      spread(aar, count)
    plot_ly(eventsmonth2, x = factor(month.abb[eventsmonth2$maaned],levels=month.abb), y = ~`2013`, name = '2013', type = 'bar', text = "Antal", marker = list(color = color1)) %>%
      add_trace(y = ~`2014`, name = '2014', marker = list(color = color2)) %>%
      add_trace(y = ~`2015`, name = '2015', marker = list(color = color3)) %>%
      add_trace(y = ~`2016`, name = '2016', marker = list(color = color4)) %>%
      add_trace(y = ~`2017`, name = '2017', marker = list(color = color5)) %>%
      #add_trace(y = ~`2018`, name = '2018', marker = list(color = color6))
      layout(yaxis = list(title = 'Antal'), xaxis = list(title = 'Måned'), barmode = 'group')
  })
  
  # deltagere pr maaned #
  output$eventsparticipantmonthplot <- renderPlotly({
    participantmonth <- eventspermonth() %>%
      group_by(aar, maaned) %>%
      summarise(count = sum(deltagere)) %>% 
      spread(aar, count)
    plot_ly(participantmonth, x = factor(month.abb[participantmonth$maaned],levels=month.abb), y = ~`2013`, name = '2013', type = 'bar', text = text, marker = list(color = color1)) %>%
      add_trace(y = ~`2014`, name = '2014', marker = list(color = color2)) %>%
      add_trace(y = ~`2015`, name = '2015', marker = list(color = color3)) %>%
      add_trace(y = ~`2016`, name = '2016', marker = list(color = color4)) %>%
      add_trace(y = ~`2017`, name = '2017', marker = list(color = color5)) %>%
      #add_trace(y = ~`2018`, name = '2018', marker = list(color = color6)) %>%
      layout(yaxis = list(title = 'Antal'), xaxis = list(title = 'Måned'), barmode = 'group')
  })

  #deltagere pr aar fordelt på arrangementstype
  output$test2 <- renderPlotly({
    events <- events %>%
      mutate(aar = year(dato)) %>%
      group_by(aar, arrangementstype) %>%
      summarise(count = sum(deltagere)) %>%
      spread(aar, count)
    plot_ly(events, x = events$arrangementstype, y = ~`2013`, name = '2013', type = 'bar', text = text, marker = list(color = color1)) %>%
      add_trace(y = ~`2014`, name = '2014', marker = list(color = color2)) %>%
      add_trace(y = ~`2015`, name = '2015', marker = list(color = color3)) %>%
      add_trace(y = ~`2016`, name = '2016', marker = list(color = color4)) %>%
      add_trace(y = ~`2017`, name = '2017', marker = list(color = color5)) %>%
      #add_trace(y = ~`2018`, name = '2018', marker = list(color = color6)) %>%
      layout(separators=",.", barmode = 'group', xaxis = list(tickmode="linear", title = "Arrangementstype"), yaxis = list(title = "Antal deltagere", separatethousands = TRUE, exponentformat='none')) 
  })
  
  # #deltagere pr maaned fordelt på arrangementstype
  # output$test3 <- renderPlotly({
  #   events <- events %>%
  #     mutate(maaned = month(dato)) %>%
  #     group_by(maaned, arrangementstype) %>%
  #     summarise(count = sum(deltagere)) %>%
  #     spread(maaned, count)
  #   plot_ly(events, x = events$arrangementstype, y = ~`Teater`, name = 'Teater', type = 'bar', text = text, marker = list(color = color1)) %>%
  #     add_trace(y = ~`Musik`, name = 'Musik', marker = list(color = color2)) %>%
  #     #add_trace(y = ~`2015`, name = '2015', marker = list(color = color3)) %>%
  #     #add_trace(y = ~`2016`, name = '2016', marker = list(color = color4)) %>%
  #     #add_trace(y = ~`2017`, name = '2017', marker = list(color = color5)) %>%
  #     #add_trace(y = ~`2018`, name = '2018', marker = list(color = color6)) %>%
  #     layout(separators=",.", barmode = 'group', xaxis = list(tickmode="linear", title = "Arrangementstype"), yaxis = list(title = "Antal deltagere", separatethousands = TRUE, exponentformat='none')) 
  # })
  
  
  # målgruppe #
  output$eventsmaalgruppeplot <- renderPlotly({
    if (input$year != "Alle") {eventsmaalgruppe <- eventsmaalgruppe %>% filter(year == input$year) %>% mutate(colors = if_else(maalgruppe=="Voksne", color1, color2))}
    if (input$year == "Alle") {eventsmaalgruppe <- eventsmaalgruppe %>% filter(year %in% c("2013","2014","2015","2016","2017")) %>% mutate(colors = if_else(maalgruppe=="Voksne", color1, color2))}
    plot_ly(eventsmaalgruppe, labels = ~maalgruppe, values = ~count, marker = list(colors = ~colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.0) %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # sted #
  eventssted <- eventssted %>%
    mutate(
      sted = case_when(
        eventssted$lokation == "Tarup bibliotek" ~ "Tarup Bibliotek",
        eventssted$lokation == "Dalum bibliotek" ~ "Dalum Bibliotek",
        eventssted$lokation == "Hovedbiblioteket - Voksen" ~ "Hovedbiblioteket",
        eventssted$lokation == "Hovedbiblioteket - Børn" ~ "Hovedbiblioteket",
        eventssted$lokation == "Hovedbiblioteket - Opsøgende" ~ "Hovedbiblioteket",
        eventssted$lokation == "Holluf Pile bibliotek" ~ "Holluf Pile Bibliotek",
        eventssted$lokation == "Korup bibliotek" ~ "Korup Bibliotek",
        eventssted$lokation == "Højby bibliotek" ~ "Højby Bibliotek",
        eventssted$lokation == "Bolbro bibliotek" ~ "Bolbro Bibliotek",
        eventssted$lokation == "Vollsmose bibliotek" ~ "Vollsmose Bibliotek",
        eventssted$lokation == "Musikbiblioteket" ~ "Musikbiblioteket",
        eventssted$lokation == "lokalhistorisk" ~ "Historiens Hus",
        eventssted$lokation == "Næsby bibliotek" ~ "Næsby Bibliotek",
        eventssted$lokation == "Andet..." ~ "Andet"
      ) 
    ) 
  
  output$eventsstedplot <- renderPlotly({
    if (input$year != "Alle") {eventssted <- eventssted %>% filter(year == input$year)}
    plot_ly(eventssted, x = eventssted$sted, y = eventssted$count, type = 'bar', text = text, marker = list(color = color1)) %>%
      layout(margin = list(b = 125), xaxis = list(title = ""), yaxis = list(title =""))
  })
  
  # kategori #
  output$eventskategoriplot <- renderPlotly({
    if (input$year != "Alle") {eventskategori <- eventskategori %>% filter(year == input$year)}
    plot_ly(eventskategori, x = eventskategori$kategori, y = eventskategori$count, type = 'bar', text = text, marker = list(color = color1)) 
  })
  
  # arrangementstype
  output$eventstypeplot <- renderPlotly({
    if (input$year != "Alle") {events <- events %>% filter(year(dato) == input$year)}
    events <- events %>%
      group_by(arrangementstype) %>%
      summarise(count = sum(deltagere))
    plot_ly(events, x = events$arrangementstype, y = events$count, type = 'bar', text = text, marker = list(color = color1)) %>%
      layout(separators=",.", xaxis = list(tickmode="linear", title = "Arrangementstype"), yaxis = list(title = "Antal deltagere", separatethousands = TRUE, exponentformat='none')) 
  })
  
  # ratio #
  output$eventsratioplot <- renderPlotly({
    if (input$effectyear != "Alle") {events<- events %>% filter(year(dato) %in% input$effectyear)}
    plot_ly(events, x = events$deltagere, y = switch(input$yakse, forberedelsestid = events$forberedelsestid, id = events$id, events$forberedelsestid), text = text, color = events$arrangementstype) %>%
      layout(xaxis = list(title = "deltagere", range = c(0, 500)), yaxis = list(title = switch(input$yakse, forberedelsestid = "forberedelsestid", id = "id", "forberedelsestid")))
  })
  
  callModule(metaTabPanel, id = "events", schema = "datamart",  table = "arrangementer", description = "Arrangementsdatabase i brug indtil 3. kvartal 2018")
  
}