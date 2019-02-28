### DB QUERIES ###
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
dbc_eres_stats <- dbGetQuery(con, "SELECT * from dbc_eres_stats")
licenses_df <- dbGetQuery(con, "select navn,eressourcer.x_erms.endelig_pris_dkk endpris,product,downloads,soegninger,taelleaar,datamart.eressourcer_ddb_kategorier.pris,brug,statbank,erms
                          from eressourcer.x_erms left join datamart.eressourcer_ddb_kategorier on datamart.eressourcer_ddb_kategorier.name_match_erms ilike '%'||eressourcer.x_erms.navn||'%'")
dbDisconnect(con)

# UI
edatabasesTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "edatabases",
          box(width = 12, solidHeader = TRUE, id = "edatabasesheader",
              h3("eBaser"),
              img(src='icons/eressourcer_negativ_45x45.png', align = "right", height="46px")
          ),     
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset45",
                          tabPanel("Generelt", 
                                   fluidRow(
                                     column(12,
                                            column(2,
                                                   h4("Afgrænsning"),
                                                   tags$br(),
                                                   selectInput(ns("eres_vendor"),"eRessource:", unique(dbc_eres_stats$vendor)),
                                                   selectInput(ns("eres_stattype"),"Statistik:", c("Antal unikke besøgende" = "visits","Samlede antal handlinger" = "actions","Gennemsnitlig besøgstid i minutter" = "visit_length")),
                                                   selectInput(ns("eres_aar"),"År:", unique(dbc_eres_stats$aar)),
                                                   #selectInput(ns("eres_abs_vs_pro"),"Værditype", c("Absolutte tal","Procent af kommunens befolkning")),
                                                   tags$br(),tags$br(),
                                                   p("Vælg evt. andre CB'er eller København og Aarhus til sammenligning:"),
                                                   checkboxGroupInput(ns("eres_isil"),
                                                                      'Biblioteker:',
                                                                      unique(as.character(isil2name(dbc_eres_stats$isil))),
                                                                      selected = "Odense",
                                                                      inline = F),
                                                   xlsxDownloadUI(ns("edatabases")),
                                                   tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>')),
                                                   if ('WHITEBOOKREDAKTØRER' %in% ldap_usergroups) {
                                                     tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Gem til pdf</a>'))
                                                   }
                                            ),
                                            column(10,
                                                   h3("Faktalink og Forfatterweb"),
                                                   span("Følgende statistik stammer fra"),a("https://bibstats.dbc.dk", href = "https://bibstats.dbc.dk", target="_blank" ),
                                                   tags$br(),tags$br(),
                                                   tags$div( h4(htmlOutput(ns("edatabases_title1"))),style = "text-align: center;" ),
                                                   withSpinner(plotlyOutput(ns("dbc_eres_stats_plot"))),
                                                   tags$br(),tags$br(),
                                                   column(10,
                                                       tags$div(h4(htmlOutput(ns("edatabases_title2")))),
                                                       withSpinner(formattableOutput(ns("dbc_eres_faktalink_table")), proxy.height="150px"),
                                                       tags$br(),tags$br(),
                                                       tags$div(h4(htmlOutput(ns("edatabases_title3")))),
                                                       withSpinner(formattableOutput(ns("dbc_eres_forfatterweb_table")), proxy.height="150px")
                                                   )
                                            )
                                     )
                                )
                           ),
                           # Insert only the follow tab and contents if user belongs to the materialeforum group
                           if ('MATERIALEFORUM' %in% ldap_usergroups) {
                             tabPanel("Licenser", 
                                             fluidRow(
                                               column(12,
                                                      column(2,
                                                             h4("Afgrænsning"),
                                                             #selectInput(ns("lic_fromyear"), "År:", unique(as.numeric(licenses_df$year))),
                                                             #selectInput(ns("lic_statbank"), "Statistikbankens typer:", c("Seriepublikationer" = "serie","eBøger" = "ebooks","Multimedier" = "multimedia","Databaser" = "databaser")),
                                                             selectInput(ns("lic_statbank"), "Statistikbankens typer:", unique(as.character(licenses_df$statbank))),
                                                             selectInput(ns("lic_priskategori"), "Priskategori:", unique(as.character(licenses_df$pris))),
                                                             selectInput(ns("lic_brugskategori"), "Brugskategori:", unique(as.character(licenses_df$brug))),
                                                             radioButtons(ns("viztype"), "Graftype:", c("Linjer" = "lines", "Søjler" = "bar") ),
                                                             xlsxDownloadUI(ns("edatabases2")),
                                                             tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                                      ),
                                                      column(10,
                                                             h3("eRessource-licenser"),
                                                             span("Data for de fleste licenser stammer fra ERMS (Consortiamanager), men tal for Digital Artikelservice og Mediastream leveres af Statsbiblioteket ("),
                                                             a("www.statsbiblioteket.dk/digital-artikelservice/statistik",href = "https://www.statsbiblioteket.dk/digital-artikelservice/statistik", target="_blank" ),span(')'),
                                                             span("og tal for Faktalink og Forfatterweb leveres af DBC ("),
                                                             a("bibstats.dbc.dk", href = "https://bibstats.dbc.dk", target="_blank" ),span(')'),
                                                             p('De talte "visninger" dækker over forskellige brugstyper. Dvs. der kan være tale om downloads eller andre typer af handlinger.'),
                                                             p("Licenserne er forsøgt kategoriseret efter sammenlignelighed"),
                                                             # Only show this panel if 'lines', i.e. 'scatterplot' is selected
                                                             conditionalPanel(
                                                               paste0("input['", ns("viztype"), "'] == 'lines'"), plotlyOutput(ns("lic_scatterplot"))
                                                             ),
                                                             # Only show this panel if 'bar', i.e. 'barplot' is selected
                                                             conditionalPanel(
                                                               paste0("input['", ns("viztype"), "'] == 'bar'"), plotlyOutput(ns("lic_barplot"))
                                                             ),
                                                             tags$br(),tags$br(),
                                                             column(10,
                                                                    tags$div(h4(htmlOutput(ns("lic_title1"))))
                                                             )
                                                      ),
                                                      column(2,
                                                             checkboxGroupInput(ns("lic_productselector"),
                                                                                'Vælg eRessource:',
                                                                                unique(as.character(licenses_df$navn)),
                                                                                selected = unique(as.character(licenses_df$navn)),
                                                                                inline = F)
                                                      ),
                                                      column(8,
                                                             formattableOutput(ns("licenses_table"))
                                                      ),
                                                      column(12,tags$hr())
                                               )))
                           }
                )))
      )
}

# SERVER
edatabasesTabPanel <- function(input, output, session, data, tablename) {
    
  dbc_eres_stats_df <- reactive({
    dbc_eres_stats %>%
      mutate_at(vars(2), funs(isil2name(.))) %>%
      filter(vendor == input$eres_vendor) %>%
      filter(stattype == input$eres_stattype) %>%
      filter(aar == input$eres_aar) %>%
      filter(isil %in% input$eres_isil) %>%
      select(isil,vendor,stattype,aar,maaned,antal) %>%
      #mutate_at(vars(5), funs(danskemåneder(.))) %>%
      group_by(maaned, isil) %>%
      spread(key = isil, value = antal)
  })
  
  # Call Excel download function for tables 
  callModule(xlsxDownload, "edatabases", data = reactive(dbc_eres_stats_df()), name = "ebaser")
  
  # Render the plot
  output$dbc_eres_stats_plot <- renderPlotly({
    colNames <- names(dbc_eres_stats_df())[-1:-5]                             # ie. get all colnames except the first thru the fifth
    p <- plot_ly(dbc_eres_stats_df(), 
                 x = factor(month.abb[dbc_eres_stats_df()$maaned],levels=month.abb), 
                 y = as.formula(paste0("~`", names(dbc_eres_stats_df())[5],"`")), 
                 type = 'bar', name = names(dbc_eres_stats_df())[5], 
                 marker = list(color = color1)) 
    len <- length(colNames)
    for(i in 0:len){
      trace <- colNames[i+1]
      p <- p %>% add_trace(y = as.formula(paste0("~", trace)), 
                           type = 'bar', 
                           name = trace, 
                           marker = list(color = colors[i+2]))
    }
    if (input$eres_stattype == "visits") {titel <- "Besøgende"}
    else if (input$eres_stattype == "actions") {titel <- "Handlinger"}
    else if (input$eres_stattype == "visit_length") {titel <- "Minutter"}
    p %>% layout(autosize = TRUE, yaxis = list(title = titel), xaxis = list(title = 'Måned', dtick = 1, autotick = FALSE), barmode = 'group')
  })
  
  # Create dynamic titles based on the filter choices
  output$edatabases_title1 <- renderText(
    paste0(case_when(input$eres_stattype == "visits" ~ "Antal unikke besøgende",
                     input$eres_stattype == "actions" ~ "Samlede antal handlinger",
                     input$eres_stattype == "visit_length" ~ "Gennemsnitlig besøgstid i minutter")," ",
                     input$eres_aar," ", 
                     input$eres_vendor))

  output$edatabases_title2 <- renderText( paste0("Faktalink, Odense ",input$eres_aar) )
  output$edatabases_title3 <- renderText( paste0("Forfatterweb, Odense ",input$eres_aar) )
  
  
  dbc_eres_faktalink_table_df <- reactive({
    dbc_eres_stats %>%
      filter(aar == input$eres_aar) %>%
      filter(isil == 746100) %>%
      filter(vendor == 'Faktalink') %>%
      select(stattype,aar,maaned,antal) %>%
      mutate_at(vars(3), funs(danskemåneder(.))) %>%
      mutate_at(vars(1), funs(eresstattypedansk(.))) %>%
      group_by(stattype,maaned) %>%
      summarise(antal = sum(antal)) %>%
      spread(key = maaned, value = antal, fill = 0) %>%
      select(stattype,Januar,Februar,Marts,April,Maj,Juni,Juli,August,September,Oktober,November,December) %>% # select is needed to maintain column ordering...
      rename(statistik = stattype) %>%
      adorn_totals(c("row","col"))
  })
  
  output$dbc_eres_faktalink_table <- renderFormattable({ formattable(dbc_eres_faktalink_table_df()) })

  dbc_eres_forfatterweb_table_df <- reactive({
    dbc_eres_stats %>%
      filter(aar == input$eres_aar) %>%
      filter(isil == 746100) %>%
      filter(vendor == 'Forfatterweb') %>%
      select(stattype,aar,maaned,antal) %>%
      mutate_at(vars(3), funs(danskemåneder(.))) %>%
      mutate_at(vars(1), funs(eresstattypedansk(.))) %>%
      group_by(stattype,maaned) %>%
      summarise(antal = sum(antal)) %>%
      spread(key = maaned, value = antal, fill = 0) %>%
      select(stattype,Januar,Februar,Marts,April,Maj,Juni,Juli,August,September,Oktober,November,December) %>% # select is needed to maintain column ordering...
      rename(statistik = stattype) %>%
      adorn_totals(c("row","col"))
  })
    
  output$dbc_eres_forfatterweb_table <- renderFormattable({ formattable(dbc_eres_forfatterweb_table_df()) })
  
  lic_data <- reactive({
    licenses <- licenses_df %>%
      #filter(taelleaar == input$lic_fromyear) %>%
      filter(pris == input$lic_priskategori) %>%
      filter(brug == input$lic_brugskategori) %>%
      filter(statbank == input$lic_statbank) %>%
      select(navn,taelleaar,downloads) %>%
      # filter(produkt %in% input$lic_productselector) %>%
      group_by(navn,taelleaar) %>%
      summarise(downloads = sum(downloads)) #%>%
    #mutate_at(vars(-1), funs(replace(., is.na(.), 0)))
  })
  
  # Render the plot as a scatterplot
  output$lic_scatterplot <- renderPlotly({
    data <- lic_data() %>% spread(navn, downloads)   # the plot needs a spread (pivot) of produkt
    colNames <- names(data)[-1]                         # ie. get all colnames except the first which is year or month or whatever
    # cf. https://stackoverflow.com/questions/46583282/r-plotly-to-add-traces-conditionally-based-on-available-columns-in-dataframe                                
    p <- plot_ly(data, x = ~taelleaar, type = 'scatter', mode = 'lines') 
    for(trace in colNames){
      p <- p %>% add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace, mode = 'lines')   # add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace)
    }
    p %>% layout(xaxis = list(title = '', autorange = 'reversed'), yaxis = list (title = 'Visninger'))
  })
  
  # Render the plot as a barchart
  output$lic_barplot <- renderPlotly({
    data <- lic_data() %>% spread(navn, downloads) 
    colNames <- names(data)[-1]
    p <- plot_ly(data, x = ~taelleaar, type = 'bar') 
    for(trace in colNames){
      p <- p %>% add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace, mode = 'bar')
    }
    p %>% layout(xaxis = list(title = '', autorange = 'reversed'), yaxis = list (title = 'Visninger'))
  })
  
  # Create dynamic titles based on the filter choices
  output$lic_title1 <- renderText(
    paste0(input$lic_statbank," med ",
           input$lic_priskategori,", ", 
           input$lic_brugskategori, " brug")
  )
  
  # Render the table
  output$licenses_table <- renderFormattable({
    data <- lic_data() %>% 
      spread(taelleaar, downloads)  %>%     # the table needs a spread (pivot) of month
      #mutate_at(vars(-1), funs(replace(., is.na(.), '-')))
      mutate_at(vars(-1), funs(replace(., is.na(.), '0')))
    formattable(data[,c(1,ncol(data):2)])
  })
}

