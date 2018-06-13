source("global.R")
source("modules.R")
source("~/.postpass")


### Branchplot MODUL ###

# UI
branchplotUI <- function(id, branch) {
  ns <- NS(id)
    tagList(
      column(3,
        p(branch),
        plotlyOutput(ns("branchplot"))
      )
  )
}

# SERVER
branchplot <- function(input, output, session, data, branch) {
  output$branchplot <- renderPlotly({
    data <- data %>% filter(name1 == branch)
    plot_ly(data, x = data$full_date1, y = data$loaner_stat_count1 , type = 'bar', name = 'Lånere', marker = list(color = color1)) %>%
      layout(yaxis = list(title = 'Antal'), xaxis = list(title = 'Alder'))
  })
}


# UI

usersTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "users",
          box(width = 12, solidHeader = TRUE, id="userheader",
              h3("Brugere")
          ),
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset12",
                          tabPanel("Generelt",
                                   fluidRow(width = 12,
                                            column(width = 12,     
                                                  h4("Bruger alder vs. borger alder"),
                                                  plotlyOutput(ns("agebranch_plot")),
                                                  h4("Aldersfordling pr. filial"),
                                                  branchplotUI(ns(id = "kor"), branch = "Korup"),
                                                  branchplotUI(ns(id = "ta"), branch = "Tarup"),
                                                  branchplotUI(ns(id = "da"), branch = "Dalum"),
                                                  branchplotUI(ns(id = "hb"), branch = "Hovedbiblioteket"),
                                                  branchplotUI(ns(id = "vo"), branch = "Vollsmose"),
                                                  branchplotUI(ns(id = "hoj"), branch = "Højby"),
                                                  branchplotUI(ns(id = "bo"), branch = "Bolbro"),
                                                  branchplotUI(ns(id = "ho"), branch = "Holluf Pile"),
                                                  h4("Aktive/Inaktive lånere"),
                                                  tableOutput(ns('tableloaners')),
                                                  csvDownloadUI(ns("inner2"))))),
                          tabPanel("Kort",
                                   fluidRow(width = 12,
                                            column(width = 12,
                                                   leafletOutput("mymap")
                                                   ))
                          ),
                          tabPanel("Dokumentation og Data",
                                   fluidRow(width = 12,
                                            column(width = 12,
                                                   h4("Dokumention"),
                                                     p("Bruger statistik og visualiseringer er dannet på baggrund af:"),
                                                     tags$ul(
                                                       tags$li("Antal lånere fordelt på alder og tilhørsfilial. Data udtrukket fra Cicero Reporting Services 12. juni 2018"), 
                                                       tags$li("Antal aktive lånere fordelt på lånergruppe. Data udtrukket fra Cicero Reporting Services 12. juni 2018"), 
                                                       tags$li("Odense Borgernes alder. Udtrukket fra Danmarks statistiks statistikbank")
                                                     ),
                                                     p("Aktive lånere: lånere som har lånt på biblioteket indenfor det seneste år"),
                                                     p("Inaktive lånere: lånere som har lånt på biblioteket for mere end et år siden og seneste for 5 år siden"),
                                                   h4("Data")  
                                            ))
                          )
                          
                    ))    
          )        
  )
  
}

# SERVER

usersTabPanel <- function(input, output, session, data, tablename) {
  # module_data <- reactive({
  #   data %>% filter(name == tablename)
  # })
  
  ### DB QUERIES ###
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  
  loaners <- dbGetQuery(con, "select case 
    when cicero.aktive_laanere.Name like '0%'::text OR cicero.aktive_laanere.Name like '0%'::text 
    OR cicero.aktive_laanere.Name like '1%'::text OR cicero.aktive_laanere.Name like '2%'::text 
    OR cicero.aktive_laanere.Name like '3%'::text OR cicero.aktive_laanere.Name like '4%'::text 
    OR cicero.aktive_laanere.Name like '5%'::text OR cicero.aktive_laanere.Name like '6%'::text 
    OR cicero.aktive_laanere.Name like '7%'::text then 'Skole'::text
    when cicero.aktive_laanere.Name = 'Voksen, Odense kommune' then 'Voksen, Odense kommune'
    when cicero.aktive_laanere.Name = 'Barn, Odense kommune' then 'Barn, Odense kommune'
    when cicero.aktive_laanere.Name = 'Voksen, udenfor Odense Kommune' then 'Voksen, udenfor Odense kommune'
    when cicero.aktive_laanere.Name = 'Barn, udenfor Odense Kommune' then 'Barn, udenfor Odense kommune'  
    else 'Andre' end as Kategori, 
    sum(cicero.aktive_laanere.loaner_stat_count)::text as Aktive, 
    (sum(cicero.inaktive_laanere.loaner_stat_count)-sum(cicero.aktive_laanere.loaner_stat_count))::text as Inaktive, 
    (sum(cicero.inaktive_laanere.loaner_stat_count))::text as Alle
    from cicero.aktive_laanere 
    join cicero.inaktive_laanere on cicero.aktive_laanere.Name = cicero.inaktive_laanere.Name
    group by kategori")
  agebranch <- dbGetQuery(con, "select * from cicero.brugere_alder_filial")
  ageodense <- dbGetQuery(con, "select * from datamart.odenseborgere")
  dbDisconnect(con)
  
  colnames(loaners) <- c("Kategori", "Aktive", "Inaktive","Alle")
  
  loaners <- loaners %>%
    mutate(
      Aktive = format(round(as.numeric(Aktive), 0), nsmall=0, big.mark="."),
      Inaktive = format(round(as.numeric(Inaktive), 0), nsmall=0, big.mark="."),     
      Alle = format(round(as.numeric(Alle), 0), nsmall=0, big.mark=".")     
    )
  
  output$tableloaners <- renderTable(loaners)
  
  innerResult <- callModule(csvDownload, "inner2", data = loaners, name = "users")
  
  
  
  # Alderfordeling
  agecitizenloaner <- agebranch %>%
    group_by(full_date1) %>%
    summarise(sum = sum(loaner_stat_count1)) %>%
    full_join(ageodense, by = c("full_date1" = "alder")) #%>%
    #rename("Alder" = full_date1, "Antal Lånere" = sum, "Antal Borgere" = antal)
  
  output$agebranch <- renderTable(agebranch) 
  
  output$agebranch_plot <- renderPlotly({
    plot_ly(agecitizenloaner, x = agecitizenloaner$full_date1, y = agecitizenloaner$sum, type = 'bar', name = 'Lånere', marker = list(color = color1)) %>%
      add_trace(y = agecitizenloaner$antal, name = 'Borgere i Odense', marker = list(color = color2) ) %>%
      layout(yaxis = list(title = 'Antal'), xaxis = list(title = 'Alder', dtick = 1, autotick = FALSE))
  })
  
  callModule(branchplot, id = "kor", data = agebranch, branch = 'Korup Bibliotek')
  callModule(branchplot, id = "ta", data = agebranch, branch = 'Tarup Bibliotek')
  callModule(branchplot, id = "da", data = agebranch, branch = 'Dalum Bibliotek')
  callModule(branchplot, id = "hb", data = agebranch, branch = 'Odense Hovedbibliotek')
  callModule(branchplot, id = "vo", data = agebranch, branch = 'Vollsmose Bibliotek')
  callModule(branchplot, id = "hoj", data = agebranch, branch = 'Højby Bibliotek')
  callModule(branchplot, id = "bo", data = agebranch, branch = 'Bolbro Bibliotek')
  callModule(branchplot, id = "ho", data = agebranch, branch = "Holluf Pile Bibliotek")
  
  # kort
    output$mymap <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng=-73.935242, lat=40.730610 , zoom=10)
    m
  })
  
}