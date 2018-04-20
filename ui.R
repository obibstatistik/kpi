source("global.R")
source("modules.R")

dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Hvidbog"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Forside", tabName = "frontpage", icon = icon("home", lib="font-awesome")),
      menuItem("Arrangementer", tabName = "events", icon = icon("calendar", lib="font-awesome")),
      menuItem("Det fysiske rum", tabName = "space", icon = icon("building-o", lib="font-awesome"),
        menuItem("Besøgende", tabName = "visits"),
        menuItem("Mødelokaler", tabName = "meetingrooms"),
        menuItem("Event områder", tabName = "eventareas"),
        menuItem("Smart City", tabName = "smartcity")
      ),
      menuItem("Online", tabName = "online", icon = icon("laptop", lib="font-awesome"), 
        menuItem("Sites", tabName = "weboverview"),
        menuItem("Odensebib.dk", tabName = "odensebib"),
        menuItem("Biblioteket App", tabName = "app")
      ),
      menuItem("Materialer", tabName = "emat", icon = icon("book", lib="font-awesome"),
        menuItem("Udlån", tabName = "fysmat"),
        menuItem("Materialeindkøb", tabName = "acquisition")
      ),
      menuItem("E-Ressourcer", tabName = "emat", icon = icon("database", lib="font-awesome"),
        menuItem("E-Bøger", tabName = "ebooks"),
        menuItem("E-Film", tabName = "emovies"),
        menuItem("E-Baser", tabName = "edatabases")
      ),
      menuItem("Brugere", tabName = "users", icon = icon("users", lib="font-awesome")),
      menuItem("Personale", tabName = "personal", icon = icon("users", lib="font-awesome")),
      menuItem("Datakilder", tabName = "datasources", icon = icon("database", lib="font-awesome"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    ### FORSIDE ###
    
    tabItems(
      tabItem(tabName = "frontpage",
        box(width = 12,
          h3("Whitebook")
        )
      ),
      
    ### ARRANGEMENTER ###  
      
    tabItem(tabName = "events",
      box(width = 12,
        h3("Arrangementer")
      ),
      fluidRow(
        column(12,
           tabBox(width = 12,
              id = "tabset3",
              tabPanel("Antal",
                 fluidRow(width = 12,
                   column(width = 6,
                      h4("Arrangementer pr. år"),
                      p("Antal arrangementer pr. år de sidste 5 år"),
                      plotlyOutput("eventsyearplot"),
                      h4("Arrangementer pr. måned"),
                      p("Antal arrangementer pr. måned de sidste 5 år"),
                      plotlyOutput("eventsmonthplot")
                   ),
                   column(width = 6,
                      h4("Deltagere pr. år"),
                      p("Antal deltagere pr. år de sidste 5 år"),
                      plotlyOutput("eventsparticipantyearplot"),
                      h4("Deltagere pr. måned"),
                      p("Antal deltagere pr. måned de sidste 5 år"),
                      plotlyOutput("eventsparticipantmonthplot")
                   )
                 )       
              ),
              tabPanel("Type",
                 fluidRow(
                   column(2,
                      h4("Afgræns pr. år"),
                      selectInput("year", "",c('Alle','2013','2014','2015','2016','2017'))
                   ),
                   column(10,
                      column(width = 4,
                         h4("Arrangementer børn/voksen"),
                         p("Antal arrangementer målrettet hhv. børn og voksne i perioden"),
                         plotlyOutput("eventsmaalgruppeplot")
                      ),
                      column(width = 4,
                         h4("Sted"),
                         p("Antal arrangementer på de enkelte biblioteker i perioden"),
                         plotlyOutput("eventsstedplot")
                      ),
                      column(width = 4,
                         h4("Kategori"),
                         p("Antal arrangementer målrettet hhv. arrangementer og læring/undervisning i perioden"),
                         plotlyOutput("eventskategoriplot")
                      )
                   )
                 )       
              ),
              tabPanel("Effekt",
                fluidRow(
                  column(12,
                    h4("Forhold imellem forberedelse og deltagere"),
                    p("Arrangementer med max 500 deltagere"),
                    plotlyOutput("eventsratioplot")
                  )
                )
              ),
              metaTabPanelUI(id = "arrangementer")
            )
          )
        )
    ),  
     
    ### DET FYSISKE RUM ###
    
    # Besøgende #
       
    tabItem(tabName = "visits",
      box(width = 12,
          h3("Besøgende")
      ),      
      fluidRow(
        column(12,
           tabBox(width = 12,
              id = "tabset1",
              tabPanel("Generelt", 
                 fluidRow(
                   column(2,
                      h4("Afgræns"),
                      selectInput("mainlibrary", "",c('Med Hovedbiblioteket','Uden Hovedbiblioteket'))    
                   ),
                   column(10,
                      h4("Besøgende total"),
                      plotlyOutput("visitsplotall")
                   ),
                   column(2,
                      h4("Afgræns pr. år"),
                      uiOutput("visitorsfrom"),
                      uiOutput("visitorsto"),
                      h4("Afgræns pr. filial"),
                      selectInput("visitorslibrary", NULL, c("Alle" = "all","Bolbro" = "bo","Dalum" = "da","Højby" = "hoj","Historiens Hus" = "lok","Holluf Pile" = "ho","Borgernes Hus" = "hb","Korup" = "kor","Musikbiblioteket" = "mus","Tarup" = "ta","Vollsmose" = "vo")
                      )
                   ),
                   column(10,
                      h4("Besøgende detaljer"),
                      formattableOutput("visitors_table")
                   ),
                   column(2,
                      h4("Afgræns")   
                   ),
                   column(10,
                      h4("Besøgende total"),
                      plotlyOutput("visitsplotindividual"),
                      formattableOutput("visitorstest")
                   )
                 )   
              ),
              metaTabPanelUI(id = "people_counter")
           )
        )
      )
    ),
      
    # Mødelokaler #
    
    tabItem(tabName = "meetingrooms",
      box(width = 12,
        h3("Mødelokaler")
      ),
      box(width = 12,
        column(2,
          h4("Periode"),
            dateRangeInput('dateRangeMeetingrooms',
              label = 'Vælg periode',
              start = Sys.Date() - 90, end = Sys.Date(),
              separator = " - "
          )
        ),
        column(width = 10,
          column(width = 6,
            h4("Oversigtstabel"),
            tableOutput("tablemeetingrooms_overview")
          ),
          column(width = 6,
            h4("Vist på agendaskærm"), 
            plotlyOutput("meetingrooms_agendascreen_plot")
          ),
          column(width = 4,
            h4("Booker top 10"),
            tableOutput("table_meetingrooms_booker")
          ),
          column(width = 8,
            h4("Oversigtstabel"),
            formattableOutput("tablemeetingrooms_timeslots")
          )
        )
      )
    ),
    
    # Event områder #
    
    tabItem(tabName = "eventareas",
      box(width = 12,
        h3("Eventområder")
      ),
      box(width = 12,
        column(2,
          h4("Periode"),
          dateRangeInput('dateRangeBhus_events',
            label = 'Vælg periode',
            start = Sys.Date() - 90, end = Sys.Date(),
            separator = " - "
          )
            ),
            column(width = 10,
              column(width = 6,
                h4("Oversigtstabel"),
                tableOutput("tablebhus_events_overview")
              ),
              column(width = 6,
                h4("Vist på agendaskærm"), 
                plotlyOutput("bhus_events_agendascreen_plot")
              ),
              column(width = 4,
                h4("Booker top 10"),
                tableOutput("table_bhus_events_booker")
              ),
              column(width = 8,
                h4("Oversigtstabel"),
                formattableOutput("tablebhus_events")
              )
            )
          )
    ),
    
    # Smart City #
    
    tabItem(tabName = "smartcity",
      box(width = 12,
        h3("Smartcity")
      ),
      box(width = 12,
        p("")
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
                                                       h4("Enheder"),  
                                                       plotlyOutput("ga_device_plot")
                                                ),
                                                column(width = 4,
                                                       h4("Top 10 sider 2017"), 
                                                       formattableOutput("tableplot3")
                                                )
                                         )
                                       )   
                              ),
                              tabPanel("Indholdsgrupper", ""),
                              tabPanel("Kampagner", "")
                       )
                )
              )
      ),
      tabItem(tabName = "app",
              box(width = 12,
                  h3("Biblioteket App")
              )
      ),
      
      ### MATERIALER ###
      
      tabItem(tabName = "fysmat",
              
        box(width = 12,
          h3("Udlån"),
          "Udlån, reserveringer og fornyelser. Tal (fra DDE) frem til september 2017." 
        ),
              
        fluidRow(
          column(12,
                 
             tabBox(width = 12,
              id = "tabset2",
              tabPanel("Generelt", 
                     fluidRow(
                       column(2,
                         checkboxGroupInput("checkGroup", "Vælg",
                          choices = list(
                            "Bolbro Bibliotek" = "Bolbro Bibliotek",
                            "Dalum Bibliotek" = "Dalum Bibliotek",
                            "Fornyelser" = "Fornyelser",
                            "Højby Bibliotek" = "Højby Bibliotek",
                            "Historiens Hus" = "Historiens Hus",
                            "Holluf Pile Bibliotek" = "Holluf Pile Bibliotek",
                            "Hovedbiblioteket" = "Hovedbiblioteket",
                            "Korup Bibliotek" = "Korup Bibliotek",
                            "Musikbiblioteket" = "Musikbiblioteket",
                            "Opsøgende afdeling" = "Opsøgende afdeling",
                            "Tarup Bibliotek" = "Tarup Bibliotek",
                            "Vollsmose Bibliotek" = "Vollsmose Bibliotek"
                          ),selected = list("Bolbro Bibliotek","Dalum Bibliotek","Fornyelser","Højby Bibliotek","Historiens Hus","Holluf Pile Bibliotek","Hovedbiblioteket","Korup Bibliotek","Musikbiblioteket","Opsøgende afdeling","Tarup Bibliotek","Vollsmose Bibliotek"))
                       ),
                       column(10,
                         plotlyOutput("loanplot")    
                       ),
                       column(12,
                         formattableOutput("loantableall"))
                       )
                      ),
              tabPanel("Bolbro", formattableOutput('loantableBol')),
              tabPanel("Dalum", formattableOutput('loantableDal')),
              tabPanel("Borgernes Hus", formattableOutput('loantableHov')),
              tabPanel("Holluf Pile", formattableOutput('loantableHol')),
              tabPanel("Højby", formattableOutput('loantableHøj')),
              tabPanel("Tarup", formattableOutput('loantableTar')),
              tabPanel("Vollsmose", formattableOutput('loantableVol'))
        )))),
    
      ### MATERIALEINDKØB ###
        
      tabItem(tabName = "acquisition",
        box(width = 12,
          h3("Materialeindkøb - Imusic 2017")
        ),
        
        fluidRow(
          column(12,
            tabBox(width = 12,
              id = "tabset4",
              tabPanel("Generelt",      
                fluidRow(width = 12,
                  column(width = 12,
                    tableOutput('acquisitionsumtable')
                  )                
                )
              ),
              tabPanel("Klargøring",
                fluidRow(width = 12,
                  column(width = 12,       
                    plotlyOutput("acquisitionplotpreparation")   
                  )              
                )
              )
            )
          )
        )
      ),
      
      ### E-RESSOURCER ###
      
      tabItem(tabName = "ebooks",
              
        box(width = 12,
          h3("Elektroniske Ressourcer"),
          p("januar 2014 - februar 2016")
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
                  h3("Film")
              )
      ),
      tabItem(tabName = "edatabases",
              box(width = 12,
                  h3("Databaser")
              )
      ),
      
      ### BRUGERE ###
      
      tabItem(tabName = "users",
        box(width = 12,
          h3("Brugere"),
          p("Oversigt over antallet af:"), 
          p("- Aktive lånere: lånere som har lånt på biblioteket indenfor det seneste år"), 
          p("- Inaktive lånere: lånere som har lånt på biblioteket for mere end et år siden og seneste for 5 år siden"),
          p("-Fordelt på kategorier, som er sammentrækninger af en større mængde lånekategorier"),
          tableOutput('tableloaners')
        )        
      ),
      
      ### PERSONALE ###
      
      tabItem(tabName = "personal",
        
        box(width = 12,
            h3("Personale")
        ),      
              
        box(width = 12,
          column(6, 
            h4("Kønsfordeling"),
            plotlyOutput("peopleplot")),
          column(6, 
            h4("Aldersfordeling"),
            plotlyOutput("peopleplotage")
            ),
          column(6, 
            h4("Fordeling i øvre aldersinterval 2012-2016"),
            plotlyOutput("peopleplotageupper")
          ),
          column(6, 
            h4("Antal i øvre aldersinterval 2016"),
            plotlyOutput("peopleplotageupper2")
          ),
          column(6, 
            h4("Fordeling på faggrupper 2012-2016"),
            plotlyOutput("peopleplotfag")
          ),
          column(6, 
            h4("Antal i faggrupper 2016"),
            plotlyOutput("peopleplotfag2")
          ),
          column(6, 
            h4("Faggrupper gennemsnitsalder"),
            plotlyOutput("peopleplotfaggemall")
          ),
          column(6, 
            h4("Faggrupper gennemsnitsalder 2016"),
            plotlyOutput("peopleplotfaggem")
          )
        )
      ),
    
      ### DATAKILDER ###
    
      tabItem(tabName = "datasources",
        box(width = 12,
          h3("Datakilder")
        ),
        fluidRow(
          column(12,
            tabBox(width = 12,
              id = "tabset3",
              tabPanel("Kildetabel",
                fluidRow(width = 12,
                  column(width = 12,
                    tableOutput('datasources_table')
                  )
                )       
              ),
              tabPanel("Kildediagram",
                fluidRow(width = 12,
                  column(width = 12,       
                    img(src='DatakildeOversigt.svg', width="1440px", height="100%" ),
                    h4("moduletest"),
                    checkboxInput("display", "Show Value"),
                    sliderTextUI("module"),
                    h2(textOutput("value"))
                  )
                )
              )       
            )
          )
        )
      )
    )
  )
)