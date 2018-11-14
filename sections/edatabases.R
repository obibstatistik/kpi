source("global.R")
source("modules.R")
source("~/.postpass")

### DB QUERIES ###
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
dbc_eres_stats <- dbGetQuery(con, "SELECT * from dbc_eres_stats")
dbDisconnect(con)


# UI
edatabasesTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "edatabases",
          box(width = 12, solidHeader = TRUE, id = "edatabasesheader",
              h3("Elektroniske Ressourcer"),
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
                                                   selectInput(ns("eres_vendor"),"eRessource:", unique(dbc_eres_stats$vendor)),
                                                   selectInput(ns("eres_stattype"),"Statistik:", unique(dbc_eres_stats$stattype)),
                                                   selectInput(ns("eres_aar"),"År:", unique(dbc_eres_stats$aar)),
                                                   checkboxGroupInput(ns("isil_selector"),
                                                                      'Vælg biblioteker:',
                                                                      unique(as.character(dbc_eres_stats$isil)),
                                                                      selected = unique(as.character(dbc_eres_stats$isil)),
                                                                      inline = F)#,
                                                   #xlsxDownloadUI(ns("inventory")),
                                                   #tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                            ),
                                            column(10,
                                                   h4("eRessource statistik fra DBC"),
                                                   p("Følgende statistik stammer fra https://bibstats.dbc.dk"),
                                                   plotlyOutput(ns("dbc_eres_stats_plot"))#,
                                                   #formattableOutput(ns("dbc_eres_stats_table"))
                                            )
                                     ))))))
      )
}

# SERVER
edatabasesTabPanel <- function(input, output, session, data, tablename) {
  
  # Manuel, kronologisk sortering af data fra ud fra måneder, så det ikke bliver alfabetisk
  #licenses_df$month <- factor(licenses_df$month, levels = c("jan","feb","mar","apr","maj","jun","jul","aug","sep","okt","nov","dec"))
  
  # Call Excel download function for tables 
  #callModule(xlsxDownload, "edatabases", data = reactive(checkouts_all_tbl()), name = "eBaser")
  
  dbc_eres_stats_df <- reactive({
    dbc_eres_stats <- dbc_eres_stats %>%
      mutate_at(vars(2), funs(isil2name(.))) %>%
      filter(vendor == 'Forfatterweb') %>%
      filter(stattype == 'visits') %>%
      filter(aar == '2018') %>%
      select(isil,vendor,stattype,aar,maaned,antal) %>%
      mutate_at(vars(5), funs(danskemåneder(.))) %>%
      group_by(maaned, isil) %>%
      spread(key = isil, value = antal)
  })
  
  # Render the plot
  output$dbc_eres_stats_plot <- renderPlotly({   
    plot_ly(dbc_eres_stats_df(), x = ~maaned, y = ~Odense, type = 'bar', name = 'Odense', marker = list(color = color1)) %>%
      add_trace(y = ~Aalborg, name = 'Aalborg', marker = list(color = color3)) %>%
      add_trace(y = ~Herning, name = 'Herning', marker = list(color = color2)) %>%
      add_trace(y = ~Vejle, name = 'Vejle', marker = list(color = color4)) %>%
      add_trace(y = ~Aarhus, name = 'Aarhus', marker = list(color = color5)) %>%
      add_trace(y = ~København, name = 'København', marker = list(color = color6)) %>%
      add_trace(y = ~Roskilde, name = 'Roskilde', marker = list(color = color7)) %>%
      add_trace(y = ~Gentofte, name = 'Gentofte', marker = list(color = color8)) %>%
      layout(autosize = TRUE, yaxis = list(title = 'Antal'), xaxis = list(title = 'Måned', dtick = 1, autotick = FALSE), barmode = 'group')
  })
  
  #  # Render the plot
  #  output$licenses_plot <- renderPlotly({
  #    data <- lic_data() %>% spread(produkt, visninger)   # the plot needs a spread (pivot) of produkt
  #    colNames <- names(data)[-1]                         # ie. get all colnames except the first which is year or month or whatever
  #    # cf. https://stackoverflow.com/questions/46583282/r-plotly-to-add-traces-conditionally-based-on-available-columns-in-dataframe                                
  #    p <- plot_ly(data, x = ~month, type = 'scatter', mode = 'lines') 
  #    for(trace in colNames){
  #      p <- p %>% add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace, mode = 'lines')   # add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace)
  #    }
  #    p %>% layout(xaxis = list(title = 'Måneder'), yaxis = list (title = 'Visninger'))
  #  })
  
  # beholdning_alt_tbl <- reactive({
  #   beholdning_alt %>%
  #     group_by_at(vars(bibliotek,input$niveau)) %>%
  #     summarise(antal = sum(antal)) %>%
  #     spread(key = bibliotek, value = antal, fill = 0) %>%
  #     select(c(input$niveau,input$branch_selector)) %>%
  #     adorn_totals(c("row","col"))
  # })
}
