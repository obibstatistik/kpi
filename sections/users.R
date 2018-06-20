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
                                                  p("Grafen viser fordelingen af OBBs lånere på alder. Dette sammenlignes med borgere generelt i Odense Kommune. "),
                                                  p("Lånergrafen udgøres af data fra Cicero."),
                                                  p("Borgergrafen udgøres af data fra seneste kvartal fra Danmarks Statistik."),
                                                  plotlyOutput(ns("agebranch_plot")),
                                                  h4("Aldersfordeling pr. filial"),
                                                  p("Graferne viser aldersfordelingen blandt lånerne, fordelt efter tilhørsfilial."),
                                                  branchplotUI(ns(id = "kor"), branch = "Korup"),
                                                  branchplotUI(ns(id = "ta"), branch = "Tarup"),
                                                  branchplotUI(ns(id = "da"), branch = "Dalum"),
                                                  branchplotUI(ns(id = "hb"), branch = "Hovedbiblioteket"),
                                                  branchplotUI(ns(id = "vo"), branch = "Vollsmose"),
                                                  branchplotUI(ns(id = "hoj"), branch = "Højby"),
                                                  branchplotUI(ns(id = "bo"), branch = "Bolbro"),
                                                  branchplotUI(ns(id = "ho"), branch = "Holluf Pile"),
                                                  h4("Aktive/Inaktive lånere"),
                                                  p("Aktive lånere er lånere som har lånt et materiale indenfor det seneste år. Inaktive lånere har haft et lån mellem 1 til 5 år tilbage i tiden."),
                                                  p("OBS. Lånere kan optræde i flere kategorier"),
                                                  tableOutput(ns('tableloaners')),
                                                  csvDownloadUI(ns("inner2"))))),
                          # tabPanel("Kort",
                          #          fluidRow(width = 12,
                          #                   column(width = 12,
                          #                          leafletOutput("mymap")
                          #                          ))
                          # ),
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

  ### DB QUERIES ###
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  
  loaners_active <- dbGetQuery(con, "select * from cicero.aktive_laanere")
  loaners_inactive <- dbGetQuery(con, "select * from cicero.inaktive_laanere")
  agebranch <- dbGetQuery(con, "select * from cicero.brugere_alder_filial")
  ageodense <- dbGetQuery(con, "select * from datamart.odenseborgere")
  dbDisconnect(con)
  
  # Alderfordeling
  agecitizenloaner <- agebranch %>%
    group_by(full_date1) %>%
    summarise(sum = sum(loaner_stat_count1)) %>%
    full_join(ageodense, by = c("full_date1" = "alder"))

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
  
  loaners_active <- loaners_active %>%
    mutate(
      location = case_when(
        loaners_active$name == "Voksen, Odense kommune" ~ "Voksen, Odense kommune",
        loaners_active$name == "Barn, Odense kommune" ~ "Barn, Odense kommune",
        loaners_active$name == "Voksen, udenfor Odense Kommune" ~ "Voksen, udenfor Odense Kommune",
        loaners_active$name == "Barn, udenfor Odense Kommune" ~ "Barn, udenfor Odense Kommune",
        grepl("^[0-9]", loaners_active$name) ~ "Skole",
        TRUE ~ "Andet"
      ) 
    )
  loaners_active <- loaners_active %>%
    select(-name) %>% mutate(type = "Aktive")
  
  loaners_inactive <- loaners_inactive %>%
    mutate(
      location = case_when(
        loaners_inactive$name == "Voksen, Odense kommune" ~ "Voksen, Odense kommune",
        loaners_inactive$name == "Barn, Odense kommune" ~ "Barn, Odense kommune",
        loaners_inactive$name == "Voksen, udenfor Odense Kommune" ~ "Voksen, udenfor Odense Kommune",
        loaners_inactive$name == "Barn, udenfor Odense Kommune" ~ "Barn, udenfor Odense Kommune",
        grepl("^[0-9]", loaners_inactive$name) ~ "Skole",
        TRUE ~ "Andet"
      ) 
    )
  loaners_inactive <- loaners_inactive %>%
    select(-name) %>% mutate(type = "Inaktive")
  
  loaners <- rbind(loaners_active, loaners_inactive) 
  
  loaners <- loaners %>% 
    group_by(lånertype = location, dato = format(date, format="%Y-%m-%d")) %>%
    summarise(aktive = sum(ifelse(type == "Aktive", loaner_stat_count, 0)), inaktive = sum(loaner_stat_count)-aktive, total = sum(loaner_stat_count))
  
  output$tableloaners <- renderTable(loaners)
  
  innerResult <- callModule(csvDownload, "inner2", data = loaners, name = "users")
  
  # # kort
  #   output$mymap <- renderLeaflet({
  #   m <- leaflet() %>%
  #     addTiles() %>%
  #     setView(lng=-73.935242, lat=40.730610 , zoom=10)
  #   m
  # })
  
}