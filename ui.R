source("global.R")
source("~/.postpass")
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)

kpi_visits_location <- dbGetQuery(con, "select distinct location from datamart.kpi_visits")

dbDisconnect(con)

dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "KPI"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Besøgstal", tabName = "visits", icon = icon("database", lib="font-awesome")),
      menuItem("Fysiske Materialer", tabName = "fysmat", icon = icon("database", lib="font-awesome")),
      menuItem("Elektroniske Materialer", tabName = "emat", icon = icon("database", lib="font-awesome")),
      menuItem("Dokumentation", tabName = "dokumentation", icon = icon("file-text-o", lib="font-awesome")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "visits",
        
        box(width = 12,
          h3("Besøgstal"),
          "Besøgstal indsamlet via gatetrackers"
          ),
        
        fluidRow(
          column(3,
            box(width = 12,
              column(12,
                h4("Filtre")
              ),
              column(12,
                selectInput("filial", "Filial:", c(
                  "Alle" = "All", 
                  "Bolbro" = "bo",
                  "Dalum" = "da",
                  "Holluf Pile" = "ho",
                  "Hovedbiblioteket" = "hb",
                  "Højby" = "hoj",
                  "Korup" = "kor",
                  "Musikbiblioteket" = "mus",
                  "Tarup" = "ta",
                  "Vollsmose" = "vo")
                )
              ),
              column(12,
                dateRangeInput('periode', label = 'Periode', start = Sys.Date() - 365, end = Sys.Date() + 0)
              )     
            )
          ),
          column(9,
            box(width = 12,
              h4("Besøgsgrafer"),
              plotlyOutput("plot"),
              plotlyOutput("lineplot")
            ),
            box(width = 12,
              DT::dataTableOutput("tablekpivisits")
            )   
          )
        )
      ),
      
      tabItem(tabName = "fysmat",
              
        box(width = 12,
          h3("Fysiske materialer"),
          "Målet er, at der max er 3 reserveringer pr. eksemplar. Dette gælder dog ikke bestsellere, film og lydbøger." 
        ),
              
        fluidRow(
          column(3,
            box(width = 12,
              column(12,
                h4("Filtre")
              ),
              column(12,
                selectInput("sprog", "Sprog:", c("All" = "All", "Dansk" = "dan","Norsk" = "nor"))
              )
            )
          ),
          column(9,
            box(width = 6,
              h4("Fordeling"),
              plotOutput("plotres")
            ),
            box(width = 12,
              DT::dataTableOutput("table")
            )
          )
        )
      ),
      
      tabItem(tabName = "emat",
              
        box(width = 12,
          h3("Elektroniske Materialer"),
          "Der må max gå 24 timer fra et materiale er bestilt til det er afsendt" 
        ),
        
        fluidRow(
          column(3,
            box(width = 12,     
              column(12,
                h4("Filtre")
              ),
              column(12,
                selectInput("year", "År:", c("Alle" = "All", "2017" = "2017","2016" = "2016","2015" = "2015" ))
              )
            )
          ),
          column(9,
            box(width = 6,
              plotOutput("plotbes")
            ),
            box(width = 6,
              plotOutput("plotbes2")
            ),
            box(width = 12,
              DT::dataTableOutput("tablebes")
            )
          )
        )
      ),
      
      tabItem(tabName = "dokumentation",
          
        box(width = 12,
          includeMarkdown("www/doc.md")
        )
      )
      
    )
  )
)