source("global.R")
source("modules.R")
source("~/.postpass")

# UI

eventsTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "events",
          box(width = 12, solidHeader = TRUE, id="eventsheader",
              h3("Arrangementer"),
              img(src='/icons/arrangementer_negativ_45x45.png', align = "right", height="45px")
          ),
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset3",
                          tabPanel("Antal",
                                   fluidRow(width = 12,
                                            column(width = 6,
                                                   h4("Arrangementer pr. år"),
                                                   p("Antal arrangementer pr. år de sidste 5 år"),
                                                   plotlyOutput(ns("eventsyearplot")),
                                                   h4("Arrangementer pr. måned"),
                                                   p("Antal arrangementer pr. måned de sidste 5 år"),
                                                   plotlyOutput(ns("eventsmonthplot"))
                                            ),
                                            column(width = 6,
                                                   h4("Deltagere pr. år"),
                                                   p("Antal deltagere pr. år de sidste 5 år"),
                                                   plotlyOutput(ns("eventsparticipantyearplot")),
                                                   h4("Deltagere pr. måned"),
                                                   p("Antal deltagere pr. måned de sidste 5 år"),
                                                   plotlyOutput(ns("eventsparticipantmonthplot"))
                                            )
                                   )       
                          ),
                          tabPanel("Type",
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
                                                   h4("Sted"),
                                                   p("Antal arrangementer på de enkelte biblioteker i perioden"),
                                                   plotlyOutput(ns("eventsstedplot"))
                                            ),
                                            column(width = 4,
                                                   h4("Kategori"),
                                                   p("Antal arrangementer målrettet hhv. arrangementer og læring/undervisning i perioden"),
                                                   plotlyOutput(ns("eventskategoriplot"))
                                            )
                                     )
                                   )       
                          ),
                          tabPanel("Effekt",
                                   fluidRow(
                                     column(12,
                                            h4("Forhold imellem forberedelse og deltagere"),
                                            p("Arrangementer med max 500 deltagere"),
                                            plotlyOutput(ns("eventsratioplot"))
                                     )
                                   )
                          ),
                          tabPanel("Data",
                            metaTabPanelUI(id = "arrangementer")
                          )  
                    )
            )
          )
  )
  
}

# SERVER

eventsTabPanel <- function(input, output, session, data, tablename) {
  # module_data <- reactive({
  #   data %>% filter(name == tablename)
  # })
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  events <- dbGetQuery(con, "SELECT * FROM datamart.arrangementer")
  eventsmaalgruppe <- dbGetQuery(con, "select extract(year from dato) as year, maalgruppe, count(*) from datamart.arrangementer group by maalgruppe, year")
  
  eventsyear <- dbGetQuery(con, "select extract(year from dato) as year, count(*) from datamart.arrangementer where extract(year from dato) > 2012 group by year order by year")
  
  eventsmonth <- dbGetQuery(con, "select extract(month from dato) as month, count(*) from datamart.arrangementer where extract(year from dato) > 2012 group by month order by month")
  eventsdeltagere <- dbGetQuery(con, "select sum(deltagere), extract(year from dato) as year from datamart.arrangementer where extract(year from dato) > 2012 group by year order by year")
  eventsparticipantmonth <- dbGetQuery(con, "select sum(deltagere), extract(month from dato) as month from datamart.arrangementer where extract(year from dato) > 2012 group by month order by month")
  eventssted <- dbGetQuery(con, "select lokation, extract(year from dato) as year, count(*) from datamart.arrangementer group by lokation, year")
  eventskategori <- dbGetQuery(con, "select kategori, extract(year from dato) as year, count(*) from datamart.arrangementer group by kategori, year")
  eventsratio <- dbGetQuery(con, "select titel, arrangementstype, deltagere, forberedelsestid from datamart.arrangementer")
  dbDisconnect(con)
  
  # arrangementer pr aar #
  output$eventsyearplot <- renderPlotly({
    plot_ly(eventsyear, x = eventsyear$year, y = eventsyear$count, type = 'bar', text = text, marker = list(color = color1)) 
  })
  # arrangementer pr maaned #
  output$eventsmonthplot <- renderPlotly({
    plot_ly(eventsmonth, x = factor(month.abb[eventsmonth$month],levels=month.abb), y = eventsmonth$count, type = 'bar', text = text, marker = list(color = color1)) 
  })
  
  # deltagere pr aar #
  output$eventsparticipantyearplot <- renderPlotly({
    plot_ly(eventsdeltagere, x = eventsdeltagere$year, y = eventsdeltagere$sum, type = 'bar', text = text, marker = list(color = color1)) 
  })
  
  # deltagere pr maaned #
  output$eventsparticipantmonthplot <- renderPlotly({
    plot_ly(eventsparticipantmonth, x = factor(month.abb[eventsparticipantmonth$month],levels=month.abb), y = eventsparticipantmonth$sum, type = 'bar', text = text, marker = list(color = color1)) 
  })
  
  # målgruppe #
  output$eventsmaalgruppeplot <- renderPlotly({
    if (input$year != "Alle") {eventsmaalgruppe <- eventsmaalgruppe %>% filter(year == input$year)}
    if (input$year == "Alle") {eventsmaalgruppe <- eventsmaalgruppe %>% filter(year %in% c("2013","2014","2015","2016","2017"))}
    plot_ly(eventsmaalgruppe, labels = ~maalgruppe, values = ~count, marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
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
        eventssted$lokation == "lokalhistorisk" ~ "Lokalhistorisk",
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
  
  # ratio #
  output$eventsratioplot <- renderPlotly({
    #if (input$year != "Alle") {eventskategori <- eventskategori %>% filter(year == input$year)}
    plot_ly(eventsratio, x = eventsratio$deltagere, y = eventsratio$forberedelsestid, text = eventsratio$titel, color = eventsratio$arrangementstype) %>%
      layout(xaxis = list(title = "deltagere", range = c(0, 500)), yaxis = list(title ="forberedelsestid"))
  })
  
}