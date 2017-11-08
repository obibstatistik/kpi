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
        menuItem("Mødelokaler", tabName = "meetingrooms"),
        menuItem("Smart City", tabName = "smartcity")
      ),
      menuItem("Online", tabName = "online", icon = icon("laptop", lib="font-awesome"), 
        menuItem("Sites", tabName = "weboverview"),
        menuItem("Odensebib.dk", tabName = "odensebib", badgeLabel = "New", badgeColor = "green"),
        menuItem("Biblioteket App", tabName = "app")
      ),
      menuItem("Materialer", tabName = "emat", icon = icon("book", lib="font-awesome"),
        menuItem("Udlån", tabName = "fysmat"),
        menuItem("Materialeindkøb", tabName = "acquisition"),
        menuItem("Materialeomsætning", tabName = "flow")
      ),
      menuItem("E-Ressourcer", tabName = "emat", icon = icon("database", lib="font-awesome"),
        menuItem("E-Bøger", tabName = "ebooks"),
        menuItem("E-Film", tabName = "emovies"),
        menuItem("E-Baser", tabName = "edatabases")
      ),
      menuItem("Brugere", tabName = "users", icon = icon("users", lib="font-awesome")),
      menuItem("Økonomi", tabName = "economy", icon = icon("usd", lib="font-awesome")),
      menuItem("Personale", tabName = "personal", icon = icon("users", lib="font-awesome"))
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
    
      tabItem(tabName = "events",
        box(width = 12,
          h3("Arrangementer")
        ),

        fluidRow(
          column(2,
            box(width = 12,
              h4("Input"),
              selectInput("year", "År:", c('alle','2013','2014','2015','2016','2017'))
            )
          ),
          column(10,
             box(width = 12,
               column(width = 6,
                  h4("Arrangementer pr. år"),
                  p("Hvor mange arrangementer har der været pr. år de sidste 5 år"),
                  plotlyOutput("eventsyearplot")
               ),
               column(width = 6,
                  h4("Deltagere pr. år"),
                  p("Hvor mange deltagere har der været pr. år de sidste 5 år"),
                  plotlyOutput("eventsdeltagereplot")
               ),
               column(width = 6,
                  h4("Arrangementer børn/voksen"),
                  p("Hvor mange arrangementer er målrettet hhv. børn og voksne i perioden"),
                  plotlyOutput("eventsmaalgruppeplot")
               ),
               column(width = 6,
                  h4("Sted"),
                  p("Hvor mange arrangementer har der været på de enkelte biblioteker i en given periode"),
                  plotlyOutput("eventsstedplot")
               ),
               column(width = 6,
                  h4("Kategori"),
                  p("Hvor mange arrangementer er målrettet hhv. arrangementer og læring/undervisning i en given periode"),
                  plotlyOutput("eventskategoriplot")
               )
             )
          )
        )
        
        
        
        
        
        
        
        
      ),  
        
      tabItem(tabName = "visits",
        
        box(width = 12,
            h3("Besøgstal")
        ),      
              
        fluidRow(
          column(12,
             tabBox(width = 12,
                id = "tabset1",
                tabPanel("Generelt", 
                   fluidRow(
                     column(12,
                        p("Biblioteker med fremgang/tilbagegang")    
                     ),
                     column(12,
                        plotlyOutput("plot")    
                     ),
                     column(12,
                        formattableOutput("tablevisits"), downloadButton("downloadData", "Download"
                     )
                   )   
                )),
                tabPanel("Bolbro", formattableOutput('tablebo')),
                tabPanel("Dalum", formattableOutput('tableda')),
                tabPanel("Borgernes Hus", formattableOutput('tablehb')),
                tabPanel("Holluf Pile", formattableOutput('tableho')),
                tabPanel("Højby", formattableOutput('tablehoj')),
                tabPanel("Tarup", formattableOutput('tableta')),
                tabPanel("Vollsmose", formattableOutput('tablevo'))
             )
          )
        )
      ),
      
      tabItem(tabName = "meetingrooms",
        box(width = 12,
          h3("Mødelokaler")
        ),
        box(width = 12,
          p("Mangler kpi'er og data fra Consierge")
        )
      ),
      
      tabItem(tabName = "smartcity",
        box(width = 12,
          h3("Smartcity")
        ),
        box(width = 12,
          p("Mangler kpi'er og data")
        )
      ),
      
      ### Online
      
      tabItem(tabName = "weboverview",
              
              box(width = 12,
                  tableOutput("tablesites")
              )
      ),
      tabItem(tabName = "odensebib",
              
              box(width = 12,
                  h3("Odensebib.dk")
              ),
              
              fluidRow(
                column(12,
                       tabBox(width = 12,
                              id = "tabset1", height = "250px",
                              tabPanel("Generelt", 
                                       fluidRow(
                                         column(12,
                                                column(width = 4,
                                                       h4("Sidevisninger"), 
                                                       plotlyOutput("plot1"),
                                                       tableOutput("ga_pageviewstable")
                                                ),
                                                column(width = 4,
                                                       h4("Enheder")#,  
                                                       #plotlyOutput("plot2")
                                                ),
                                                column(width = 4,
                                                       h4("Top 10 sider 2017")#, 
                                                       #tableOutput("tableplot3")
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
      
      ### MATERIALER ###
      
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
      
      ### E-RESSOURCER ###
      
      tabItem(tabName = "ebooks",
              
        box(width = 12,
          h3("Elektroniske Ressourcer")
        ),
        
        fluidRow(
          column(12,
            box(width = 12,
                column(width = 6,
                  h4("Antal udlån over tid"),
                  plotlyOutput("p")
                ),
                column(width = 6,
                  h4("Type"), 
                  plotlyOutput("ereolentypeplot")
                )
            ),
            box(width = 12,
                h4("Alder"),
                column(width = 12,plotlyOutput("ereolenaldersubplot"))
            )
          )
        )
      ),
      tabItem(tabName = "emovies",
              box(width = 12,
                  h3("Film"),
                  "Filmstriben, kan man få data fra DBC eller er der noget som kan lægges manuelt ind?"
              )
      ),
      tabItem(tabName = "edatabases",
              box(width = 12,
                  h3("Databaser"),
                  "Netbaser, data fra proxy mangler"
              )
      ),
      
      ### BRUGERE ###
      
      tabItem(tabName = "users",
        box(width = 12,
          h3("Brugere"),
          p("Voksne i kommunen (29-09-2017): 72766"),
          p("Voksne udenfor kommunen (29-09-2017): 23169"),
          p("Børn i kommunen (29-09-2017): 5489"),
          p("Børn udenfor kommunen (29-09-2017): 493"),
          p("Brugere i bibliotekssystemet, e-ressourcer, hjemmesiden, i huset"),
          p("Datakilde: Der kan ikke trækkes noget automatisk. Man kan på langsommelig måde godt hente CSV lister ud. Navn, Lånernummer, Adresse, Postnummer & by, Telefon	E-mail")
          )        
      ),
      
      ### ØKONOMI ###
      
      tabItem(tabName = "economy",
        box(width = 12,
          h3("Økonomi"),
          p("Generelle økonomital fordelt på år"),
          p("6bytal"),
          p("Datakilde: ?")
        )
      ),
      
      ### PERSONALE ###
      
      tabItem(tabName = "personal",
        box(width = 12,
          h3("Personale"),
          p("Inspiration fra SDU:"),
          p("Årsværk fordelt efter årstal, arbejdssted og stillingskategori. Har desuden faneblade med alder og køn"),
          p("Datakilde: ?")
        )
      )
    )
  )
)