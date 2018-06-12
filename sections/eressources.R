source("global.R")
source("modules.R")
source("~/.postpass")

### COLORS ###

colors <- c('rgb(70,140,140)', 'rgb(174,176,81)', 'rgb(59,54,74)', 'rgb(192,57,83)', 'rgb(29,114,170)', 'rgb(225,123,81)', 'rgb(219,181,61)')
color1 = c('rgb(70,140,140)')
color2 = c('rgb(174,176,81)')
color3 = c('rgb(59,54,74)')
color4 = c('rgb(192,57,83)')
color5 = c('rgb(29,114,170)')
color6 = c('rgb(225,123,81)')
color7 = c('rgb(219,181,61)')

### DB QUERIES ###
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
licenses_df <- dbGetQuery(con, "select year,month,product produkt,sum(use) visninger from datamart.eressourcer_ddb where type = 'visninger' group by year,month,product")
dbDisconnect(con)
#licenses_df <- as.data.frame(licenses_df,stringsAsFactors = FALSE)

# UI
eressourcesTabPanelUI <- function(id) {
  
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
                                                   selectInput(ns("eres_statbank"), "Statistikbankens typer:", c("Seriepublikationer" = "serie","eBøger" = "ebooks","Multimedier" = "multimedia","Databaser" = "databaser")),
                                                   selectInput(ns("eres_priskategori"), "Priskategori:", c("Fastpris" = "fastpris", "Klikpris" = "klikpris")),
                                                   selectInput(ns("eres_brugskategori"), "Brugskategori:", c("Rekreativ brug" = "rekreativ", "Faglig brug" = "faglig"))
                                            ),
                                            column(10,
                                                   h4("Licenser"),
                                                   p("Kategoriseret efter sammenlignelighed"),
                                                   plotlyOutput(ns("licenses_plot"))
                                            ),
                                            column(2,
                                                   checkboxGroupInput(ns("eres_productselector"),
                                                                      'Vælg eRessource:',
                                                                      unique(as.character(licenses_df$produkt)),
                                                                      selected = unique(as.character(licenses_df$produkt)),
                                                                      inline = F)
                                            ),
                                            column(8,
                                                   formattableOutput(ns("licenses_table"))
                                            ),
                                            column(12,tags$hr())
                                     )))))))
}

# SERVER
eressourcesTabPanel <- function(input, output, session, data, tablename) {
  
  # Manuel, kronologisk sortering af data fra ud fra måneder, så det ikke bliver alfabetisk
  licenses_df$month <- factor(licenses_df$month, levels = c("jan","feb","mar","apr","maj","jun","jul","aug","sep","okt","nov","dec"))
  licenses_df = licenses_df[order(licenses_df$month,decreasing=FALSE),]
  
  output$licenses_table <- renderFormattable({
    licenses <- licenses_df %>%
      filter(year == input$eres_fromyear) %>%
      filter(produkt %in% input$eres_productselector) %>%
      #filter(year == '2016') %>%
      group_by(produkt,month) %>%
      summarise(visninger = sum(visninger)) %>%
      mutate_at(vars(-1), funs(replace(., is.na(.), 0))) %>%
      spread(month, visninger)
    
    formattable(licenses)
  })
  
  # Licenses plot
  output$licenses_plot <- renderPlotly({
    
    licenses_traces <- licenses_df %>%
      filter(year == input$eres_fromyear) %>%
      filter(produkt %in% input$eres_productselector) %>%
      select(produkt,month,visninger) %>%
      group_by(produkt,month) %>%
      summarise(visninger = sum(visninger)) %>%
      mutate_at(vars(-1), funs(replace(., is.na(.), 0))) %>%
      spread(produkt, visninger)
    
    colNames <- names(licenses_traces)[-1] # ie. get all colnames except the first which is year or month or whatever
    
    # tjek https://stackoverflow.com/questions/46583282/r-plotly-to-add-traces-conditionally-based-on-available-columns-in-dataframe                                
    p <- plot_ly(licenses_traces, x = ~month, y = ~`kompass`, name = 'kompass', type = 'scatter', mode = 'lines')
    for(trace in colNames){
      p <- p %>% add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace, mode = 'lines')   # add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace)
    }
    p %>% layout(xaxis = list(title = 'Måneder'), yaxis = list (title = 'Visninger'))
  })
}
