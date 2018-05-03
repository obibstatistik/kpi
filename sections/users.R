source("global.R")
source("modules.R")
source("~/.postpass")

# UI

usersTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "users",
          box(width = 12, solidHeader = TRUE,
              h3("Brugere"),
              p("test2"),
              p("Oversigt over antallet af:"), 
              p("- Aktive lånere: lånere som har lånt på biblioteket indenfor det seneste år"), 
              p("- Inaktive lånere: lånere som har lånt på biblioteket for mere end et år siden og seneste for 5 år siden"),
              p("-Fordelt på kategorier, som er sammentrækninger af en større mængde lånekategorier"),
              tableOutput(ns('tableloaners'))
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
  output$tableloaners <- renderTable(loaners)
  
}