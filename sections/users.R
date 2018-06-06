source("global.R")
source("modules.R")
source("~/.postpass")

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
                                  h3("Brugere"),
                                  p("test2"),
                                  p("Oversigt over antallet af:"), 
                                  p("- Aktive lånere: lånere som har lånt på biblioteket indenfor det seneste år"), 
                                  p("- Inaktive lånere: lånere som har lånt på biblioteket for mere end et år siden og seneste for 5 år siden"),
                                  p("-Fordelt på kategorier, som er sammentrækninger af en større mængde lånekategorier"),
                                  tableOutput(ns('tableloaners')),
                                  csvDownloadUI(ns("inner2"))))),
                          tabPanel("Kort",
                                   fluidRow(width = 12,
                                            column(width = 12,
                                                   leafletOutput("mymap")
                                                   )))
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
  
  # loaners <- dbGetQuery(con, "select sum(loaner_stat_count)::text as Antal, case 
  #   when Name like '0%'::text OR Name like '0%'::text OR Name like '1%'::text OR Name like '2%'::text OR Name like '3%'::text OR Name like '4%'::text OR Name like '5%'::text OR Name like '6%'::text OR Name like '7%'::text then 'Skole'::text
  #   when Name = 'Voksen, Odense kommune' then 'Voksen, Odense kommune'
  #   when Name = 'Barn, Odense kommune' then 'Barn, Odense kommune'
  #   when Name = 'Voksen, udenfor Odense Kommune' then 'Voksen, udenfor Odense kommune'
  #   when Name = 'Barn, udenfor Odense Kommune' then 'Barn, udenfor Odense kommune'  
  #   else 'Andre' end as Kategori  
  #   from cicero.aktive_laanere group by Kategori")
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
  
  # kort
  
  output$mymap <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng=-73.935242, lat=40.730610 , zoom=10)
    m
  })
  
}