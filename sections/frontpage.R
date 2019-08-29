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
        kpitileUI(ns(id = "visitors"), image = "icons/detfysiskerum_negativ_45x45.png", text = "Samlet besøg på OBB år til dato.", color = color2, width = 12),
        kpitileUI(ns(id = "loans"), image = "icons/materialer_negativ_45x45.png", text = "Samlet udlån på OBB år til dato", color = color3, width = 12),
        kpitileUI(ns(id = "events"), image = "icons/arrangementer_negativ_45x45.png", text = "Samlet antal afholdte arrangementer på OBB år til dato", color = color5, width = 12),
        kpitileUI(ns(id = "citizenservice"), image = "icons/detfysiskerum_negativ_45x45.png", text = "Samlet antal betjeninger i Borgerservice år til dato", color = color4, width = 12)
      )
    )
  )
}

# SERVER

frontpageTabPanel <- function(input, output, session) {
  
  current_year = year(Sys.Date())
  
  drv <- dbDriver("PostgreSQL")
  con_dwh <- dbConnect(drv, dbname = dbname_dwh, host = host_dwh, port = port_dwh, user = user_dwh, password = password_dwh)
  visitors_current_year <- dbGetQuery(con_dwh, paste0("SELECT sum(visitor_count) FROM visitors.visitors_per_day where extract(year from date) = ",current_year))
  loans_current_year <- dbGetQuery(con_dwh, paste0("SELECT sum(antal) FROM cicero.udlaan_per_opstillingsprofil WHERE extract(year from (transact_date)) = ",current_year))
  events_current_year <- dbGetQuery(con_dwh, paste0("SELECT count(*) FROM arrangementer.obib_arrangementer WHERE extract(year from start_dato) = ",current_year))
  citizenservice_current_year <- dbGetQuery(con_dwh, paste0("SELECT count(*) as count FROM borgerservice.x_betjeninger WHERE \"Lokation\" = 'Borgerservice Odense' and extract(year from (\"Tid\")) = ",current_year))
  dbDisconnect(con_dwh)
  
  callModule(kpitile, id = "visitors", data = visitors_current_year)
  callModule(kpitile, id = "loans", data = loans_current_year)
  callModule(kpitile, id = "events", data = events_current_year)
  callModule(kpitile, id = "citizenservice", data = citizenservice_current_year)
  
}