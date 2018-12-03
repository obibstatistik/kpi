source("global.R")
source("modules.R")
source("~/.postpass")

### DB QUERIES ###
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
#licenses_df <- dbGetQuery(con, "select brug,pris,statbank,year,month,datamart.eressourcer_ddb.product produkt,sum(use) visninger 
#                          from datamart.eressourcer_ddb 
#                          left join datamart.eressourcer_ddb_kategorier on datamart.eressourcer_ddb_kategorier.name_match = datamart.eressourcer_ddb.product 
#                          where type = 'visninger' 
#                          group by brug,pris,statbank,year,month,datamart.eressourcer_ddb.product")
licenses_df <- dbGetQuery(con, "select navn,eressourcer.x_erms.endelig_pris_dkk endpris,product,downloads,soegninger,taelleaar,datamart.eressourcer_ddb_kategorier.pris,brug,statbank,erms
                      from eressourcer.x_erms left join datamart.eressourcer_ddb_kategorier on datamart.eressourcer_ddb_kategorier.name_match_erms ilike '%'||eressourcer.x_erms.navn||'%'")
dbDisconnect(con)
#licenses_df <- as.data.frame(licenses_df,stringsAsFactors = FALSE)

# UI
licensesTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "licenses",
          box(width = 12, solidHeader = TRUE, id = "eressourcesheader",
              h3("Elektroniske Ressourcer"),
              img(src='icons/eressourcer_negativ_45x45.png', align = "right", height="46px")
          ),     
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset1",
                          tabPanel("Generelt", 
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
                                                   xlsxDownloadUI(ns("edatabases")),
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
                                     )))))))
}

# SERVER
licensesTabPanel <- function(input, output, session, data, tablename) {
  
  # Manuel, kronologisk sortering af data fra ud fra måneder, så det ikke bliver alfabetisk
  #licenses_df$month <- factor(licenses_df$month, levels = c("jan","feb","mar","apr","maj","jun","jul","aug","sep","okt","nov","dec"))
  #licenses_df = licenses_df[order(licenses_df$month,decreasing=FALSE),]
  
  # Store data and its filters in reactive function (gives reusability)
  #lic_data <- reactive({
  #  licenses <- licenses_df %>%
  #    filter(year == input$lic_fromyear) %>%
  #    filter(pris == input$lic_priskategori) %>%
  #    filter(brug == input$lic_brugskategori) %>%
  #    filter(statbank == input$lic_statbank) %>%
  #    select(produkt,month,visninger) %>%
  #    # filter(produkt %in% input$lic_productselector) %>%
  #    group_by(produkt,month) %>%
  #    summarise(visninger = sum(visninger)) %>%
  #    mutate_at(vars(-1), funs(replace(., is.na(.), 0)))
  #})
  
  lic_data <- reactive({
    licenses <- erms_df %>%
      #filter(taelleaar == input$lic_fromyear) %>%
      filter(pris == input$lic_priskategori) %>%
      filter(brug == input$lic_brugskategori) %>%
      filter(statbank == input$lic_statbank) %>%
      select(navn,taelleaar,downloads) %>%
      # filter(produkt %in% input$lic_productselector) %>%
      group_by(navn,taelleaar) %>%
      summarise(downloads = sum(downloads)) %>%
      mutate_at(vars(-1), funs(replace(., is.na(.), 0)))
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
    data <- lic_data() %>% spread(taelleaar, downloads)     # the table needs a spread (pivot) of month
    formattable(data)
  })
}
