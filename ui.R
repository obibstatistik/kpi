source("global.R")

dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Whitebook"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Forside", tabName = "frontpage", icon = icon("home", lib="font-awesome")),
      menuItem("Arrangementer", tabName = "events", icon = icon("calendar", lib="font-awesome")),
      menuItem("Det fysiske rum", tabName = "space", icon = icon("building-o", lib="font-awesome"),
               menuItem("Besøgstal", tabName = "visits"),
               menuItem("Smart City", tabName = "smartcity")
               ),
      menuItem("Online", tabName = "online", icon = icon("laptop", lib="font-awesome"), 
               menuItem("Oversigt", tabName = "weboverview"),
               menuItem("Odensebib.dk", tabName = "odensebib", badgeLabel = "New", badgeColor = "green"),
               menuItem("Biblioteket App", tabName = "app")
               ),
      menuItem("Fysiske Materialer", tabName = "emat", icon = icon("book", lib="font-awesome"),
               menuItem("Udlån", tabName = "fysmat"),
               menuItem("Materialeindkøb", tabName = "acquisition"),
               menuItem("Materialeomsætning", tabName = "flow")
               ),
      menuItem("Elektroniske Materialer", tabName = "emat", icon = icon("database", lib="font-awesome")),
      menuItem("Brugere", tabName = "users", icon = icon("users", lib="font-awesome")),
      menuItem("Økonomi", tabName = "economy", icon = icon("usd", lib="font-awesome")),
      menuItem("Datakilder", tabName = "sources", icon = icon("database", lib="font-awesome")),
      menuItem("Datasikkerhed", tabName = "security", icon = icon("shield", lib="font-awesome")),
      menuItem("Dokumentation", tabName = "documentation", icon = icon("file-text-o", lib="font-awesome")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    tabItems(
      
      tabItem(tabName = "frontpage",
        box(width = 12,
          h3("Whitebook")
        )
      ),
      
      tabItem(tabName = "visits",
        
        box(width = 12,
          h3("Besøgstal"),
          "Besøgstal indsamlet via gatetrackers"
          ),
        
        fluidRow(
          column(12,
            box(width = 12, h4("Besøgsgraf"), plotlyOutput("plot")),
            box(width = 12, h4("Samlet"), formattableOutput("tablevisits"), downloadButton("downloadData", "Download")),
            box(width = 6, h4("Bolbro"), formattableOutput('tablebo'), tableOutput("bo")),
            box(width = 6, h4("Dalum"), formattableOutput('tableda'), tableOutput("da")),
            box(width = 6, h4("Hovedbibliotek"), formattableOutput('tablehb'), tableOutput("hb")),
            box(width = 6, h4("Holluf Pile"), formattableOutput('tableho'), tableOutput("ho")),
            box(width = 6, h4("Højby"), formattableOutput('tablehoj'), tableOutput("hoj")),
            box(width = 6, h4("Korup"), formattableOutput('tablekor'), tableOutput("kor")),
            box(width = 6, h4("Musikbiblioteket"), formattableOutput('tablemus'), tableOutput("mus")),
            box(width = 6, h4("Tarup"), formattableOutput('tableta'), tableOutput("ta")),
            box(width = 6, h4("Vollsmose"), formattableOutput('tablevo'), tableOutput("vo"))
          )
        )
      ),
      
      tabItem(tabName = "fysmat",
              
        box(width = 12,
          h3("Udlån"),
          "Udlån, reserveringer og fornyelser" 
        ),
              
        fluidRow(
          column(12,
            box(width = 12,
              h4("Udlån"), 
              plotlyOutput("loanplot")
            ),
            box(width = 12, h4("Samlet"), formattableOutput("loantableall")), 
            box(width = 6, h4("Bolbro"), formattableOutput('loantableBol')),
            box(width = 6, h4("Dalum"), formattableOutput('loantableDal')),
            box(width = 6, h4("Hovedbibliotek"), formattableOutput('loantableHov')),
            box(width = 6, h4("Holluf Pile"), formattableOutput('loantableHol')),
            box(width = 6, h4("Højby"), formattableOutput('loantableHøj')),
            box(width = 6, h4("Korup"), formattableOutput('loantableKor')),
            box(width = 6, h4("Musikbiblioteket"), formattableOutput('loantableMus')),
            box(width = 6, h4("Tarup"), formattableOutput('loantableTar')),
            box(width = 6, h4("Vollsmose"), formattableOutput('loantableVol'))
          )
        )
      ),
      
      tabItem(tabName = "acquisition",
        box(width = 12,
          h3("Materialeindkøb"),
          p("Opgørelse over materialeindkøb fra Imusic")
        ),
        
        fluidRow(
          column(12,
            box(width = 12,
              tableOutput('acquisitionsumtable')
             ),
             box(width = 12,
                 "Test"
             ),
             box(width = 12,
                 "Test"
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
      
      ### Online
      
      tabItem(tabName = "weboverview",
              
              box(width = 12,
                  includeMarkdown("www/weboverview.md")
              )
      ),
      tabItem(tabName = "odensebib",
              
        box(width = 12,
          h3("Odensebib.dk")
        ),
        
        fluidRow(
          column(12,
            tabBox(width = 12,
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabset1", height = "250px",
              tabPanel("Generelt", 
                 fluidRow(
                   column(12,
                      column(width = 4,
                        h4("Sidevisninger"), 
                        plotlyOutput("plot1")
                      ),
                      column(width = 4,
                        h4("Enheder"),  
                        plotlyOutput("plot2")
                      ),
                      column(width = 4,
                        h4("Top 10 sider 2017"), 
                        tableOutput("tableplot3")
                      )
                    )
                  )   
                        ),
              tabPanel("Indholdsgrupper", "Data er der. Skal programmeres i Whitebook"),
              tabPanel("Kampagner", "Der skal sættes kampagne op på Drupal siden")
            )
          )
        )
      ),
      tabItem(tabName = "app",
        box(width = 12,
            h3("Biblioteket App"),
            p("KPI'er skal identificeres"),
            p("Det mangler data fra Redia")
        )
      ),
      
      ###
      
      tabItem(tabName = "arrangementer",
        box(width = 12,
          h3("Arrangementer"),
          "Arrangementer på Odense Bibliotekerne" 
        ),
        fluidRow(
          column(12,
            box (width = 12,
              #plotlyOutput("eventsplot"),
              formattableOutput('event2table')
            ),     
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
      tabItem(tabName = "events",
        box(width = 12,
            h3("Arrangementer"),
            p("Borgerfeedback"),
            p("Arrangementsdatabasen"),
            p("Place2book")
        )        
      ),
      tabItem(tabName = "libraryspace",
        box(width = 12,
            h3("Det fysiske rum"),
            "<ul></ul>"
        )        
      ),
      tabItem(tabName = "regionlibrary",
        box(width = 12,
            h3("Centralbibliotek"),
            "Hello World"
        )        
      ),
      tabItem(tabName = "users",
        box(width = 12,
            h3("Brugere"),
            p("Voksne i kommunen (29-09-2017): 72766"),
            p("Voksne udenfor kommunen (29-09-2017): 23169"),
            p("Børn i kommunen (29-09-2017): 5489"),
            p("Børn udenfor kommunen (29-09-2017): 493"),
            p("Metode: Der kan ikke trækkes noget automatisk. Man kan på langsommelig måde godt hente CSV lister ud. Navn, Lånernummer, Adresse, Postnummer & by, Telefon	E-mail")
        )        
      ),
      tabItem(tabName = "economy",
        box(width = 12,
          h3("Økonomi"),
          "6bytal"
        )
      ),
      tabItem(tabName = "sources",
          
        box(width = 12,
          includeMarkdown("www/sources.md")
        )
      ),
      tabItem(tabName = "security",
        box(width = 12,
            includeMarkdown("www/security.md")
        )
      ),
      tabItem(tabName = "documentation",
        box(width = 12,
            includeMarkdown("www/doc.md")
        )
      )
      
    )
  )
)