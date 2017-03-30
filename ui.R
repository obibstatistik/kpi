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
          column(12,
            box(width = 12, h4("Besøgsgrafer"), plotlyOutput("plot")),
            box(width = 12, h4("Samlet"), formattableOutput("tableformat")),
            box(width = 6, h4("Bolbro"), formattableOutput('tablebo')),
            box(width = 6, h4("Dalum"), formattableOutput('tableda')),
            box(width = 6, h4("Hovedbibliotek"), formattableOutput('tablehb')),
            box(width = 6, h4("Holluf Pile"), formattableOutput('tableho')),
            box(width = 6, h4("Højby"), formattableOutput('tablehoj')),
            box(width = 6, h4("Korup"), formattableOutput('tablekor')),
            box(width = 6, h4("Musikbiblioteket"), formattableOutput('tablemus')),
            box(width = 6, h4("Tarup"), formattableOutput('tableta')),
            box(width = 6, h4("Vollsmose"), formattableOutput('tablevo'))
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
              "placeholder"
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
          column(12,
            box(width = 12,
                "ereolen og ereolen go"
            ),
            box(width = 12,
                "filmstriben"
            ),
            box(width = 12,
                "netbaser"
            )
          )
        )
      ),
      
      
      tabItem(tabName = "web",
              
        box(width = 12,
          h3("Web"),
          "Webstatistik for Odensebib.dk" 
        ),
        
        fluidRow(
          column(12,
            box(width = 6,
              tableOutput('table')
            ),
            box(width = 6,
              "Besøgende",
              #plotlyOutput("webplot")
              tableOutput('table2')
            )
          )
        )
      ),
      
      
      tabItem(tabName = "arrangementer",
        box(width = 12,
          h3("Arrangementer"),
          "Arrangementer på Odense Bibliotekerne" 
        ),
        fluidRow(
          column(12,
            box(width = 4,
              "Hvor tilfredse er borgerne med arrangementerne"
            ),
            box(width = 4,
              "Antal arrangementer"
            ),
            box(width = 4,
              "Besøgende til arrangementer"
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