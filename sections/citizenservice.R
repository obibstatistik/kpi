source("global.R")
source("modules.R")
source("~/.postpass")

# UI

citizenserviceTabPanelUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "citizenservice",
    box(
      width = 12,
      solidHeader = TRUE,
      id = "userheader",
      h3("Borgerservice")
    ),
    fluidRow(column(
      12,
      tabBox(
        width = 12,
        id = "tabset12",
        tabPanel("Generelt",
                 fluidRow(
                   width = 12,
                   column(
                     width = 12,
                     h4("Betjeninger"),
                     column(width = 6, plotlyOutput(ns(
                       "betjeninger_plot"
                     ))),
                     column(width = 6, tableOutput(ns('betjeninger'))),
                     h4("aftalte_møder"),
                     tableOutput(ns('aftalte_møder')),
                     h4("reservationer"),
                     tableOutput(ns('reservationer')),
                     h4("skranke_check_ins"),
                     tableOutput(ns('skranke_check_ins')),
                     h4("stander_events"),
                     tableOutput(ns('stander_events'))
                   )
                 )),
        tabPanel("Borgerservice vs Bib",
                 fluidRow(width = 12,
                    column(
                      width = 12,
                      h4("Besøgende - Bibliotek vs Borgerservice"),
                      column(width = 6, plotlyOutput(ns("borgvsbib_plot"))),
                      column(width = 6, tableOutput(ns('borgvsbib')))
                    )))
      )
    ))
  )
  
}

# SERVER

citizenserviceTabPanel <-
  function(input, output, session, data, tablename) {
    drv <- dbDriver("PostgreSQL")
    con <-
      dbConnect(
        drv,
        dbname = dbname,
        host = host,
        port = port,
        user = user,
        password = password
      )
    aftalte_møder <-
      dbGetQuery(con, "SELECT * FROM borgerservice.x_aftalte_møder LIMIT 10")
    betjeninger <-
      dbGetQuery(con, "SELECT date_trunc('month', \"Tid\") as date, count('ID') as borg FROM borgerservice.x_betjeninger group by date")
    reservationer <-
      dbGetQuery(con, "SELECT * FROM borgerservice.x_reservationer LIMIT 10")
    skranke_check_ins <-
      dbGetQuery(con, "SELECT * FROM borgerservice.x_skranke_check_ins LIMIT 10")
    stander_events <-
      dbGetQuery(con, "SELECT * FROM borgerservice.x_stander_events LIMIT 10")
    visitors <-
      dbGetQuery(con, "SELECT date_trunc('month', registertime) as date, count(delta) as bib FROM visitor_counter where location='hb' and direction='In' group by date")
    dbDisconnect(con)
    
    #betjeninger
    #betjeninger_plot <- reactive({
    #  betjeninger <- betjeninger %>%
    #    select(ID, Tid) %>%
    #    mutate(date = floor_date(Tid, "month")) %>%
    #    group_by(date) %>%
    #    summarise(borg = n())
    #})
    
    output$betjeninger_plot <- renderPlotly({
      plot_ly(
        betjeninger,
        x = ~ date,
        y = ~ borg,
        type = 'scatter',
        mode = 'lines'
      )
    })
    output$betjeninger <- renderTable(betjeninger)
    
    output$visitors <- renderTable(visitors)
    
    borgvsbib  <- reactive({
      borgvsbib <- betjeninger %>%
        right_join(visitors, by = c("date" = "date"))
    })
    
    output$borgvsbib <- renderTable(
      borgvsbib()
    )
    
    output$borgvsbib_plot <- renderPlotly({
      plot_ly(
        borgvsbib(), x = ~date, y = ~`borg`, type = 'scatter', mode = 'lines', name = 'Borgerservice', line = list(color = color1)) %>%
          add_trace(y = ~`bib`, name = 'Bibliotek', line = list(color = color2)) %>%
        layout(yaxis = list(title = 'Antal'), xaxis = list(title = 'Dato'))
    })
    
    # test
    output$aftalte_møder <- renderTable(aftalte_møder)
    
    # test
    output$reservationer <- renderTable(reservationer)
    
    # test
    output$skranke_check_ins <- renderTable(skranke_check_ins)
    
    # test
    output$stander_events <- renderTable(stander_events)
    
  }