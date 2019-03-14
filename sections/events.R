### Radarplot MODUL ###

# UI
radarplotUI <- function(id, branch) {
  ns <- NS(id)
  tagList(
    column(3,
      p(branch),
      withSpinner(plotlyOutput(ns("radarplot")))
    )
  )
}

# SERVER
radarplot <- function(input, output, session, data, branch) {

  output$radarplot <- renderPlotly({
  
  print(branch)  
    
  data <- data %>% 
    #filter(lokation %in% c('Bolbro bibliotek','Dalum bibliotek','Højby bibliotek','Korup bibliotek','Tarup bibliotek','Musikbiblioteket','Vollsmose bibliotek')) %>%
    group_by(lokation) %>%
    summarise(antal = n(), deltagere = mean(antal_deltagere), forberedelsestid = mean(forberedelsestid)) %>%
    mutate(max_antal = max(antal), max_deltagere = max(deltagere), max_forberedelsestid = max(forberedelsestid)) %>%
    mutate(norm_antal = antal/max_antal,norm_deltagere = deltagere/max_deltagere,norm_forberedelsestid = forberedelsestid/max_forberedelsestid) %>%
    filter(startsWith(lokation, branch)) %>%
    select(norm_antal, norm_deltagere, forberedelsestid)
    
  print(data)

  plot_ly(
    type = 'scatterpolar',
    r = as.numeric(data[1,]),
    theta = c('antal','deltagere','forberedelsestid'),
    mode= "lines",
    fill = 'toself'
  ) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,1)
        )
      ),
      showlegend = F
    )
  })
}

# UI

eventsTabPanelUI <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "events",
    box(
      width = 12, solidHeader = TRUE, id = "eventsheader",
      h3("Arrangementer"),
      img(src = "icons/arrangementer_negativ_45x45.png", align = "right", height = "45px")
    ),
    fluidRow(
      column(
        12,
        tabBox(
          width = 12,
          id = "tabset3",
          tabPanel(
            "Antal",
            fluidRow(
              column(
                12,
                tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>')),
                column(
                  width = 6,
                  h4("Arrangementer pr. år"),
                  p("Graferne viser det samlede antal arrangementer og undervisning/læringsseancer afholdt på OBB, samt antal deltagere i arrangementer pr. år i de sidste 5 år."),
                  withSpinner(plotlyOutput(ns("eventsyearplot")))
                ),
                column(
                  width = 6,
                  h4("Deltagere pr. år"),
                  p("Antal deltagere pr. år de sidste 5 år"),
                  withSpinner(plotlyOutput(ns("eventsparticipantyearplot")))
                )
              ),
              column(12, tags$hr()),
              column(
                12,
                tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>')),
                # column(width = 2,
                # h4("Afgrænser"),
                # checkboxGroupInput(ns("event_year"), label = 'Vælg år',
                #   selected = list("2017"),
                #   choices = list("2018","2017","2016","2015","2014"))
                # ),
                column(12,
                  class = "col-lg-6",
                  h4("Arrangementer pr. måned"),
                  p("Graferne viser antal afholdte arrangementer, samt antal deltagere fordelt pr. måned. Det er muligt at vælge år via ”vælgeren” i venstre side – Det er også muligt at sammenligne to år via valg i drop down menuer"),
                  withSpinner(plotlyOutput(ns("eventsmonthplot")))
                ),
                column(12,
                  class = "col-lg-6",
                  h4("Deltagere pr. måned"),
                  p("Antal deltagere pr. måned de sidste 5 år"),
                  withSpinner(plotlyOutput(ns("eventsparticipantmonthplot")))
                )
              ),
              column(12, tags$hr()),
              column(
                12,
                tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>')),
                h4("Deltagere pr. år fordelt på arrangementstype"),
                withSpinner(plotlyOutput(ns("test2")))
                # ,plotlyOutput(ns("test3"))
              )
            )
          ),
          tabPanel(
            "Type",
            fluidRow(
              column(
                12,
                fluidRow(
                  column(
                    6,
                    h4("Info"),
                    p("Den første graf viser fordelingen pr. år mellem børne og voksen arrangementer. Den anden graf fordeling i arrangementskategorier. Det tredje diagram viser afholdte arrangementer på hvert bibliotek. Det er muligt at skifte mellem forskellige år via ”vælgeren” i venstre side.")
                  )
                ),
                fluidRow(
                  column(
                    2,
                    h4("Afgræns pr. år"),
                    selectInput(ns("year"), "", c("2018", "2017", "2016", "2015", "2014", "2013", "Alle")),
                    tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                  ),
                  column(
                    10,
                    column(
                      width = 6, class = "col-lg-4",
                      h4("Arrangementer børn/voksen"),
                      p("Antal arrangementer målrettet hhv. børn og voksne i perioden"),
                      withSpinner(plotlyOutput(ns("eventsmaalgruppeplot"))),
                      tags$div("", style = "page-break-after: always;")
                    ),
                    column(
                      width = 6, class = "col-lg-4",
                      h4("Kategori"),
                      p("Antal arrangementer målrettet hhv. arrangementer og læring/undervisning i perioden"),
                      withSpinner(plotlyOutput(ns("eventskategoriplot")))
                    ),
                    column(
                      width = 12, class = "col-lg-4",
                      h4("Sted"),
                      p("Antal arrangementer på de enkelte biblioteker i perioden"),
                      withSpinner(plotlyOutput(ns("eventsstedplot"))),
                      tags$div("", style = "page-break-after: always;")
                    ),
                    column(
                      width = 12,
                      h4("Type"),
                      p("Antal deltager pr. arrangementstype i perioden"),
                      withSpinner(plotlyOutput(ns("eventstypeplot")))
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            "Effekt",
            fluidRow(
              column(
                12,
                p(ldap_usergroups),
                h4("Top 5 - forhold deltagere / forberedelsestid"),
                tableOutput(ns('top5')),
                h4("Bund 5 - forhold deltagere / forberedelsestid"),
                tableOutput(ns('bottom5')),
                h4("Forhold imellem forberedelse og deltagere"),
                p("Grafen viser forholdet mellem forberedelsestid og antal deltagere fordelt på arrangementskategorier. Holdes musen henover det enkelte arrangement vises titel og specifikke data."),
                p("OBS. Maks mulige forberedelsestid i indberetning er 999 minutter."),
                p("Ved at klikke én gang på en arrangementstype fravælges denne i visningen. Ved at dobbeltklikke på en arrangementstype vises kun denne."),
                column(
                  2,
                  # selectInput(ns("effectyear"), "",c('Alle','2013','2014','2015','2016','2017','2018')),
                  h4("Afgræns"),
                  checkboxGroupInput(ns("effectyear"),
                    label = "Vælg år:",
                    selected = list("Alle", "2013", "2014", "2015", "2016", "2017", "2018"),
                    choices = list("Alle", "2013", "2014", "2015", "2016", "2017", "2018")
                  ),
                  # ,radioButtons(ns("yakse"), "Vælg Y akse:",
                  #             c("Forberedelsestid" = "forberedelsestid",
                  #               "ID" = "id")
                  # ),
                  tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                ),
                column(
                  width = 10,
                  plotlyOutput(ns("eventsratioplot"))
                ),
                h4("Radar plots"),
                radarplotUI(ns(id = "mus"), branch = "Musikbiblioteket"),
                radarplotUI(ns(id = "kor"), branch = "Korup"),
                radarplotUI(ns(id = "ta"), branch = "Tarup"),
                radarplotUI(ns(id = "da"), branch = "Dalum"),
                radarplotUI(ns(id = "hb"), branch = "Hovedbiblioteket"),
                radarplotUI(ns(id = "vo"), branch = "Vollsmose"),
                radarplotUI(ns(id = "hoj"), branch = "Højby"),
                radarplotUI(ns(id = "bo"), branch = "Bolbro"),
                radarplotUI(ns(id = "ho"), branch = "Holluf Pile")
              )
            )
          ), 
          tabPanel(
            "Dokumentation og data",
            xlsxDownloadUI(ns("events")),
            p("Opdateret: "), p(htmlOutput(ns("updateText")))
          )
        )
      )
    )
  )
}

# SERVER

eventsTabPanel <- function(input, output, session, data, tablename) {
  drv <- dbDriver("PostgreSQL")
  con_dwh <- dbConnect(drv, dbname = dbname_dwh, host = host_dwh, port = port_dwh, user = user_dwh, password = password_dwh)
  events <- dbGetQuery(con_dwh, "SELECT * FROM arrangementer.obib_arrangementer")
  eventsmaalgruppe <- dbGetQuery(con_dwh, "select extract(year from start_dato) as year, maalgruppe, count(*) from arrangementer.obib_arrangementer group by maalgruppe, year")
  eventsyear <- dbGetQuery(con_dwh, "select extract(year from start_dato) as year, count(*) from arrangementer.obib_arrangementer where extract(year from start_dato) > 2012 group by year order by year")
  eventsdeltagere <- dbGetQuery(con_dwh, "select sum(antal_deltagere), extract(year from start_dato) as year from arrangementer.obib_arrangementer where extract(year from start_dato) > 2012 group by year order by year")
  eventssted <- dbGetQuery(con_dwh, "select lokation, extract(year from start_dato) as year, count(*) from arrangementer.obib_arrangementer group by lokation, year")
  eventskategori <- dbGetQuery(con_dwh, "select kategori, extract(year from start_dato) as year, count(*) from arrangementer.obib_arrangementer group by kategori, year")
  eventsratio <- dbGetQuery(con_dwh, "select titel, arrangementstype, antal_deltagere as deltagere, forberedelsestid from arrangementer.obib_arrangementer where arrangementstype != ''")
  events_update <- dbGetQuery(con_dwh, "select obj_description('arrangementer.obib_arrangementer'::regclass)")
  dbDisconnect(con_dwh)

  # test
  output$table <- renderTable(
    events %>% 
      select(titel, start_dato) %>% 
      mutate(tid = format(start_dato, "%Y-%m-%d %H:%M:%OS")) %>%
      mutate(zone = as.POSIXct(start_dato, tz = "Europe/Berlin"))
  )
  
  # arrangementer pr aar #
  output$eventsyearplot <- renderPlotly({
    plot_ly(eventsyear, x = eventsyear$year, y = eventsyear$count, type = "bar", text = text, marker = list(color = color5))
  })

  # deltagere pr aar #
  output$eventsparticipantyearplot <- renderPlotly({
    plot_ly(eventsdeltagere, x = eventsdeltagere$year, y = eventsdeltagere$sum, type = "bar", text = text, marker = list(color = color5))
  })

  eventspermonth <- reactive({
    eventspermonth <- events %>%
      select(antal_deltagere, start_dato) %>%
      mutate(aar = year(start_dato), maaned = month(start_dato)) %>%
      select(-start_dato)
  })

  # arrangementer pr maaned #
  output$eventsmonthplot <- renderPlotly({
    eventsmonth2 <- eventspermonth() %>%
      select(-antal_deltagere) %>%
      group_by(aar, maaned) %>%
      summarise(count = n()) %>%
      spread(aar, count)
    plot_ly(eventsmonth2, x = factor(month.abb[eventsmonth2$maaned], levels = month.abb), y = ~ `2013`, name = "2013", type = "bar", text = "Antal", marker = list(color = color1)) %>%
      add_trace(y = ~ `2014`, name = "2014", marker = list(color = color2)) %>%
      add_trace(y = ~ `2015`, name = "2015", marker = list(color = color3)) %>%
      add_trace(y = ~ `2016`, name = "2016", marker = list(color = color4)) %>%
      add_trace(y = ~ `2017`, name = "2017", marker = list(color = color5)) %>%
      add_trace(y = ~ `2018`, name = "2018", marker = list(color = color6)) %>%
      layout(yaxis = list(title = "Antal"), xaxis = list(title = "Måned"), autosize = T, barmode = "group")
  })

  # deltagere pr maaned #
  output$eventsparticipantmonthplot <- renderPlotly({
    participantmonth <- eventspermonth() %>%
      group_by(aar, maaned) %>%
      summarise(count = sum(antal_deltagere)) %>%
      spread(aar, count)
    plot_ly(participantmonth, x = factor(month.abb[participantmonth$maaned], levels = month.abb), y = ~ `2013`, name = "2013", type = "bar", text = text, marker = list(color = color1)) %>%
      add_trace(y = ~ `2014`, name = "2014", marker = list(color = color2)) %>%
      add_trace(y = ~ `2015`, name = "2015", marker = list(color = color3)) %>%
      add_trace(y = ~ `2016`, name = "2016", marker = list(color = color4)) %>%
      add_trace(y = ~ `2017`, name = "2017", marker = list(color = color5)) %>%
      add_trace(y = ~ `2018`, name = "2018", marker = list(color = color6)) %>%
      layout(yaxis = list(title = "Antal"), xaxis = list(title = "Måned"), autosize = T, barmode = "group")
  })

  # deltagere pr aar fordelt på arrangementstype
  output$test2 <- renderPlotly({
    events <- events %>%
      mutate(aar = year(start_dato)) %>%
      group_by(aar, arrangementstype) %>%
      summarise(count = sum(antal_deltagere)) %>%
      spread(aar, count)
    plot_ly(events, x = events$arrangementstype, y = ~ `2013`, name = "2013", type = "bar", text = text, marker = list(color = color1)) %>%
      add_trace(y = ~ `2014`, name = "2014", marker = list(color = color2)) %>%
      add_trace(y = ~ `2015`, name = "2015", marker = list(color = color3)) %>%
      add_trace(y = ~ `2016`, name = "2016", marker = list(color = color4)) %>%
      add_trace(y = ~ `2017`, name = "2017", marker = list(color = color5)) %>%
      add_trace(y = ~ `2018`, name = "2018", marker = list(color = color6)) %>%
      layout(autosize = T, separators = ",.", barmode = "group", xaxis = list(tickmode = "linear", title = "Arrangementstype"), yaxis = list(title = "Antal deltagere", separatethousands = TRUE, exponentformat = "none"))
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
    if (input$year != "Alle") {
      eventsmaalgruppe <- eventsmaalgruppe %>%
        filter(year == input$year) %>%
        mutate(colors = if_else(maalgruppe == "Voksne", color1, color2))
    }

    if (input$year == "Alle") {
      eventsmaalgruppe <- eventsmaalgruppe %>%
        filter(year %in% c("2013", "2014", "2015", "2016", "2017")) %>%
        group_by(maalgruppe) %>%
        summarise(count = sum(count)) %>%
        mutate(colors = if_else(maalgruppe == "Voksne", color1, color2))
    }
    plot_ly(eventsmaalgruppe,
      labels = ~ maalgruppe,
      values = ~ count,
      # text = percent, # denne og følgende linje runder procenterne af, så de er uden decimaler
      text = ~ paste(round((count / sum(count)) * 100, 0), "%", sep = ""), # denne og følgende linje runder procenterne af, så de er uden decimaler
      # text = ~paste(sum(which(year)),"%",sep=""), # denne og følgende linje runder procenterne af, så de er uden decimaler. Kunne dog ikke bare bruge sum(count) som andre steder, da Børn og Voksen ikke udgør 100% til sammen
      textinfo = "text",
      textfont = list(color = "#FFFFFF"),
      marker = list(colors = colors, line = list(color = "#FFFFFF", width = 1))
    ) %>%
      add_pie(hole = 0.0) %>%
      layout(
        showlegend = T, autosize = T,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
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
    if (input$year != "Alle") {
      eventssted <- eventssted %>% filter(year == input$year)
    }
    plot_ly(eventssted, x = eventssted$sted, y = eventssted$count, type = "bar", text = text, marker = list(color = color1)) %>%
      layout(autosize = T, margin = list(b = 125), xaxis = list(title = ""), yaxis = list(title = ""))
  })

  # kategori #
  output$eventskategoriplot <- renderPlotly({
    if (input$year != "Alle") {
      eventskategori <- eventskategori %>% filter(year == input$year)
    }
    plot_ly(eventskategori, x = eventskategori$kategori, y = eventskategori$count, type = "bar", text = text, marker = list(color = color1)) %>%
      layout(autosize = T, xaxis = list(title = ""), yaxis = list(title = ""))
  })

  # arrangementstype
  output$eventstypeplot <- renderPlotly({
    if (input$year != "Alle") {
      events <- events %>% filter(year(start_dato) == input$year)
    }
    events <- events %>%
      group_by(arrangementstype) %>%
      summarise(count = sum(antal_deltagere))
    plot_ly(events, x = events$arrangementstype, y = events$count, type = "bar", text = text, marker = list(color = color1)) %>%
      layout(autosize = T, separators = ",.", xaxis = list(tickmode = "linear", title = "Arrangementstype"), yaxis = list(title = "Antal deltagere", separatethousands = TRUE, exponentformat = "none"))
  })

  # ratio #
  output$eventsratioplot <- renderPlotly({
    if (input$effectyear != "Alle") {
      events <- events %>% filter(year(dato) %in% input$effectyear)
    }
    # plot_ly(events, x = events$deltagere, y = switch(input$yakse, forberedelsestid = events$forberedelsestid, id = events$id, events$forberedelsestid), text = text, color = events$arrangementstype) %>%
    plot_ly(events, x = events$deltagere, y = events$forberedelsestid, text = text, color = events$arrangementstype) %>%
      layout(xaxis = list(title = "deltagere", range = c(0, 500)), yaxis = list(title = "forberedelsestid"), autosize = T)
    # layout(xaxis = list(title = "deltagere", range = c(0, 500)), yaxis = list(title = switch(input$yakse, forberedelsestid = "forberedelsestid", id = "id", "forberedelsestid")))
  })

  # data og dokumentation
  events_xlsx <- events %>% mutate(start_dato = as.POSIXct(start_dato, tz = "Europe/Berlin"), slut_dato = as.POSIXct(slut_dato, tz = "Europe/Berlin"))
  callModule(xlsxDownload, "events", data = reactive(events), name = "Arrangementsdata")
  event_update_time <- substr(events_update[[1]], 98, 1000)
  output$updateText <- renderText(event_update_time)
  
  # top 5 & last 5
    
  toplast <- events %>%
    filter(year(start_dato) > 2018) %>%
    filter(!is.na(antal_deltagere),!is.na(forberedelsestid)) %>%
    mutate(ratio = antal_deltagere/forberedelsestid) %>%
    mutate(start_dato = format(start_dato, "%Y-%m-%d"), slut_dato = format(slut_dato, "%Y-%m-%d"))
    
  top5 <- toplast %>%  
    top_n(5, ratio) %>%
    arrange(desc(ratio))
  
  output$top5 <- renderTable(top5 %>% select(titel, ratio, start_dato, slut_dato, antal_deltagere, forberedelsestid, type, kategori, arrangementstype))
  
  
  bottom5 <- toplast %>%
    top_n(-5, ratio) %>%
    arrange(desc(ratio))

  output$bottom5 <- renderTable(bottom5 %>% select(titel, ratio, start_dato, slut_dato, antal_deltagere, forberedelsestid, type, kategori, arrangementstype))
  
  # radar
  
  output$radar <- renderPlotly({
    plot_ly(
      type = 'scatterpolar',
      r = c(39, 28, 8, 7, 28, 39),
      theta = c('A','B','C', 'D', 'E', 'A'),
      fill = 'toself'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,50)
          )
        ),
        showlegend = F
      )
  })
  
  
  #events <- events %>% 
    #mutate(lokation = ifelse(startsWith(lokation, "Hovedbiblioteket"),"Hovedbiblioteket", .)) #%>%
    #filter(lokation != c("Andet...", "odense city, , 5000","Kulturmaskinen, Farvergården 7, 5000 Odense C", "HF & VUC FYN Odense City Campus, 3. sal, U310, Kottesgade 6-8, 5000 Odense C"))
  
  callModule(radarplot, id = "mus", data = events, branch = 'Musikbiblioteket')
  callModule(radarplot, id = "kor", data = events, branch = 'Korup bibliotek')
  callModule(radarplot, id = "ta", data = events, branch = 'Tarup bibliotek')
  callModule(radarplot, id = "da", data = events, branch = 'Dalum bibliotek')
  callModule(radarplot, id = "hb", data = events, branch = 'Hovedbibliotek')
  callModule(radarplot, id = "vo", data = events, branch = 'Vollsmose bibliotek')
  callModule(radarplot, id = "hoj", data = events, branch = 'Højby bibliotek')
  callModule(radarplot, id = "bo", data = events, branch = 'Bolbro bibliotek')
  callModule(radarplot, id = "ho", data = events, branch = "Holluf Pile bibliotek")
  
}
