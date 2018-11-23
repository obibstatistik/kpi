source("global.R")
source("modules.R")
source("~/.postpass")

### DB QUERIES ###
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
licenses_df <- dbGetQuery(con, "select brug,pris,statbank,year,month,datamart.eressourcer_ddb.product produkt,sum(use) visninger 
from datamart.eressourcer_ddb 
left join datamart.eressourcer_ddb_kategorier on datamart.eressourcer_ddb_kategorier.name_match = datamart.eressourcer_ddb.product 
where type = 'visninger' 
group by brug,pris,statbank,year,month,datamart.eressourcer_ddb.product")
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
                                                   h4("Afgræns"),
                                                   selectInput(ns("eres_fromyear"), "År:", unique(as.numeric(licenses_df$year))),
                                                   #selectInput(ns("eres_statbank"), "Statistikbankens typer:", c("Seriepublikationer" = "serie","eBøger" = "ebooks","Multimedier" = "multimedia","Databaser" = "databaser")),
                                                   selectInput(ns("eres_statbank"), "Statistikbankens typer:", unique(as.character(licenses_df$statbank))),
                                                   selectInput(ns("eres_priskategori"), "Priskategori:", unique(as.character(licenses_df$pris))),
                                                   selectInput(ns("eres_brugskategori"), "Brugskategori:", unique(as.character(licenses_df$brug)))
                                            ),
                                            column(10,
                                                   h4("Licenser"),
                                                   p("Kategoriseret efter sammenlignelighed"),
                                                   plotlyOutput(ns("licenses_plot"))
                                            ),
                                            column(2
                                            #       checkboxGroupInput(ns("eres_productselector"),
                                            #       'Vælg eRessource:',
                                            #       unique(as.character(licenses_df$produkt)),
                                            #       selected = unique(as.character(licenses_df$produkt)),
                                            #       inline = F)
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
  licenses_df$month <- factor(licenses_df$month, levels = c("jan","feb","mar","apr","maj","jun","jul","aug","sep","okt","nov","dec"))
  licenses_df = licenses_df[order(licenses_df$month,decreasing=FALSE),]
  
  # Store data and its filters in reactive function (gives reusability)
  lic_data <- reactive({
    licenses <- licenses_df %>%
      filter(year == input$eres_fromyear) %>%
      filter(pris == input$eres_priskategori) %>%
      filter(brug == input$eres_brugskategori) %>%
      filter(statbank == input$eres_statbank) %>%
      select(produkt,month,visninger) %>%
      # filter(produkt %in% input$eres_productselector) %>%
      group_by(produkt,month) %>%
      summarise(visninger = sum(visninger)) %>%
      mutate_at(vars(-1), funs(replace(., is.na(.), 0)))
  })
 
  # Render the plot
  output$licenses_plot <- renderPlotly({
    data <- lic_data() %>% spread(produkt, visninger)   # the plot needs a spread (pivot) of produkt
    colNames <- names(data)[-1]                         # ie. get all colnames except the first which is year or month or whatever
    # cf. https://stackoverflow.com/questions/46583282/r-plotly-to-add-traces-conditionally-based-on-available-columns-in-dataframe                                
    p <- plot_ly(data, x = ~month, type = 'scatter', mode = 'lines') 
    for(trace in colNames){
      p <- p %>% add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace, mode = 'lines')   # add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace)
    }
    p %>% layout(xaxis = list(title = 'Måneder'), yaxis = list (title = 'Visninger'))
  })
  
  # Render the table
  output$licenses_table <- renderFormattable({
    data <- lic_data() %>% spread(month, visninger)     # the table needs a spread (pivot) of month
    formattable(data)
  })
}
