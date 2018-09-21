source("global.R")
source("modules.R")
source("~/.postpass")

# UI

frontpageTabPanelUI <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "frontpage",
    fluidRow(
      column(6,
        box(width=12,
          h1("Velkommen til Odense Biblioteker og Borgerservices digitale Whitebook"),
          p("Her kan du finde nøgletal og tendenser indenfor forskellige områder af OBBs virksomhed."),
          p("Du får adgang til de enkelte områder ved at klikke dig rundt i menuerne i venstre side af skærmen."),
          p("Da Whitebooken er et nyt tiltag og er i konstant udvikling vil vi meget gerne have feedback på hvad du synes om den."),
          p("Du er meget velkomen til skrive til Thomas Bojsen (tsboj@odense.dk), hvis du støder på problemer eller hvis du har udviklingsønsker til grafer eller visninger du er nysgerrige på.")
        )
      ),
      column(6,
        #kpitileUI(ns(id = "visitors"), image = "icons/detfysiskerum_negativ_45x45.png", text = "Samlet besøg på OBB år til dato.", color = color2, width = 12),
        kpitileUI(ns(id = "loans"), image = "icons/materialer_negativ_45x45.png", text = "Samlet udlån på OBB år til dato", color = color3, width = 12),
        kpitileUI(ns(id = "events"), image = "icons/arrangementer_negativ_45x45.png", text = "Samlet antal afholdte arrangementer på OBB år til dato", color = color5, width = 12)
      )
    )
  )
}

# SERVER

frontpageTabPanel <- function(input, output, session) {
  
  current_year = year(Sys.Date())
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  visitors_current_year <- dbGetQuery(con, "SELECT sum(count) FROM people_counter WHERE direction = 'in' and extract(year from date) = 2018")
  loans_current_year <- dbGetQuery(con, "SELECT sum(antal) FROM cicero.udlaan_per_opstillingsprofil WHERE extract(year from (transact_date)) = 2018")
  events_current_year <- dbGetQuery(con, "SELECT count(*) FROM datamart.arrangementer_old WHERE extract(year from dato) = 2018")
  dbDisconnect(con)
  
  #callModule(kpitile, id = "visitors", data = visitors_current_year)
  callModule(kpitile, id = "loans", data = loans_current_year)
  callModule(kpitile, id = "events", data = events_current_year)
  
  
  output$visitorsbox <- renderInfoBox({
    infoBox(
      "Samlet besøg på OBB år til dato.", format(visitors_current_year, big.mark="."),
      icon = icon("list"),
      color = "red"
    )
  })
  
  output$visitorsbox2 <- renderInfoBox({
    infoBox(
      "Samlet udlån på OBB år til dato", format(loans_current_year, big.mark="."),
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$visitorsbox3 <- renderInfoBox({
    infoBox(
      "Samlet antal afholdte arrangementer på OBB år til dato", format(events_current_year, big.mark="."),
      icon = icon("list"),
      color = "lime"
    )
  })
  
}