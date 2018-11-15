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
                                                   selectInput(ns("eres_vendor"),"eRessource:", unique(dbc_eres_stats$vendor)),
                                                   selectInput(ns("eres_stattype"),"Statistik:", c("Antal unikke besøgende" = "visits","Samlede antal handlinger" = "actions","Gennemsnitlig besøgstid i minutter" = "visit_length")),
                                                   selectInput(ns("eres_aar"),"År:", unique(dbc_eres_stats$aar)),
                                                   p("Vælg evt. andre CB'er eller København og Aarhus til sammenligning:"),
                                                   checkboxGroupInput(ns("eres_isil"),
                                                                      'Vælg biblioteker:',
                                                                      unique(as.character(isil2name(dbc_eres_stats$isil))),
                                                                      selected = "Odense",
                                                                      inline = F),
                                                   xlsxDownloadUI(ns("edatabases")),
                                                   tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                            ),
                                            column(10,
                                                   h3("Faktalink og Forfatterweb"),
                                                   span("Følgende statistik stammer fra"),a("https://bibstats.dbc.dk", href = "https://bibstats.dbc.dk"),
                                                   tags$br(),tags$br(),
                                                   tags$div(h4(htmlOutput(ns("edatabases_title1"))) ,style = "text-align: center;" ),
                                                   plotlyOutput(ns("dbc_eres_stats_plot")),
                                                   formattableOutput(ns("dbc_eres_stats_table"))
                                            )
                                     ))))))
      )
}

# SERVER
edatabasesTabPanel <- function(input, output, session, data, tablename) {
  
  dbc_eres_stats_df <- reactive({
    dbc_eres_stats <- dbc_eres_stats %>%
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
    colNames <- names(dbc_eres_stats_df())[-1:-5]                         # ie. get all colnames except the first thru the fifth
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
  
  # Create dynamic title for the page based on the filter choices
  output$edatabases_title1 <- renderText(
    paste0(case_when(input$eres_stattype == "visits" ~ "Antal unikke besøgende",
                     input$eres_stattype == "actions" ~ "Samlede antal handlinger",
                     input$eres_stattype == "visit_length" ~ "Gennemsnitlig besøgstid i minutter")," ",
                     input$eres_aar," ", 
                     input$eres_vendor))
  
  #dbc_eres_stats_table_df <- reactive({
  #  dbc_eres_stats %>%
  #    select(vendor,stattype,maaned,antal) %>%
  #    filter??????
  #    group_by_at(vars(bibliotek,input$niveau)) %>%
  #    #summarise(antal = sum(antal)) %>%
  #    spread(key = bibliotek, value = antal, fill = 0) %>%
  #    select(c(input$niveau,input$branch_selector)) %>%
  #    adorn_totals(c("row","col"))
  #})
  #
  #output$dbc_eres_stats_table <- renderFormattable({ formattable(dbc_eres_stats_table_df()) })
  
  }
