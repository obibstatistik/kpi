source("global.R")

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
      menuItem("Web", tabName = "web", icon = icon("database", lib="font-awesome")),
      menuItem("Arrangementer", tabName = "arrangementer", icon = icon("database", lib="font-awesome")),
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
              plotlyOutput("plot")
            ),
            box(width = 12,
              h4("Samlet"),  
              dataTableOutput('table')
            )   
          )
        )
      ),
      
      tabItem(tabName = "fysmat",
              
        box(width = 12,
          h3("Fysiske materialer"),
          "Udlån, reserveringer og fornyelser" 
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
            box(width = 12,
                "placeholder"
            ),
            box(width = 12,
              tableOutput('tableloan')
            )
          )
        )
      ),
      
      tabItem(tabName = "emat",
              
        box(width = 12,
          h3("Elektroniske Materialer"),
          "Elektroniske materialer" 
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
                "placeholder"
            ),
            box(width = 6,
                "placeholder"
            ),
            box(width = 12,
                "placeholder"
            )
          )
        )
      ),
      
      
      tabItem(tabName = "web",
              
              box(width = 12,
                  h3("Web"),
                  "Web" 
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
                           "placeholder"
                       ),
                       box(width = 6,
                           "placeholder"
                       ),
                       box(width = 12,
                           "placeholder"
                       )
                )
              )
      ),
      
      
      tabItem(tabName = "arrangementer",
              
              box(width = 12,
                  h3("Arrangementer"),
                  "Arrangementer" 
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
                           "placeholder"
                       ),
                       box(width = 6,
                           "placeholder"
                       ),
                       box(width = 12,
                           "placeholder"
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