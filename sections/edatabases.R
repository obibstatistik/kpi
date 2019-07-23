### DB QUERIES ###
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
con_dwh <- dbConnect(drv, dbname = dbname_dwh, host = host_dwh, port = port_dwh, user = user_dwh, password = password_dwh)
dbc_eres_stats <- dbGetQuery(con, "SELECT * from dbc_eres_stats")
# licenser_overblik <- dbGetQuery(con_dwh, "SELECT 'faktor' datakilde,eressource,aar,maaned,antal,kilde,bibliotek,rapport_dato::date FROM eressourcer.x_licenser_overblik")
licenses_df <- dbGetQuery(con, "select navn,eressourcer.x_erms.endelig_pris_dkk endpris,product,downloads,soegninger,taelleaar,datamart.eressourcer_ddb_kategorier.pris,brug,statbank,erms
                          from eressourcer.x_erms left join datamart.eressourcer_ddb_kategorier on datamart.eressourcer_ddb_kategorier.name_match_erms ilike '%'||eressourcer.x_erms.navn||'%'")

licenser_overblik <- dbGetQuery(con_dwh, "
    SELECT 'DBC' datakilde,vendor eressource,aar,maaned,antal,vendor kilde,isil bibliotek,null::date rapport_dato
     FROM eressourcer.x_forfatterweb_faktalink
     WHERE stattype = 'visits'
     UNION ALL
    SELECT 'Faktor' datakilde,
           eressource,aar,
           case
    	when maaned = 'Januar' then 1
    	when maaned = 'Februar' then 2
    	when maaned = 'Marts' then 3
    	when maaned = 'April' then 4
    	when maaned = 'Maj' then 5
    	when maaned = 'Juni' then 6
    	when maaned = 'Juli' then 7
    	when maaned = 'August' then 8
    	when maaned = 'September' then 9
    	when maaned = 'Oktober' then 10
    	when maaned = 'November' then 11
    	when maaned = 'December' then 12
    end maaned,
    antal,
    kilde,
    bibliotek,
    rapport_dato::date
     FROM eressourcer.x_licenser_overblik
     UNION ALL
    SELECT 'DBC' datakilde, 'Filmstriben' eressource,
     extract(year from dato) aar,
     extract(month from dato) maaned,
     count(*) antal,
     'Filmstriben' kilde,
     'Odense' bibliotek,
     null::date rapport_dato
     FROM eressourcer.x_filmstriben_forbrug
     WHERE extract(year from dato) >= 2017
     group by datakilde,eressource,aar,maaned,kilde,bibliotek,rapport_dato
    UNION ALL
    SELECT 'DBC' datakilde,
     'Ereolen - ' || bogtype eressource,
     extract(year from oprettet) aar,
     extract(month from oprettet) maaned,
     count(*) antal,
     'eReolen' kilde,
     'Odense' bibliotek,
     null::date rapport_dato
     FROM eressourcer.x_pubhub_udlaan
     WHERE extract(year from oprettet) >= 2017
     group by datakilde,eressource,aar,maaned,kilde,bibliotek,rapport_dato")

dbDisconnect(con)
dbDisconnect(con_dwh)

# # Necessary for testing outside shinyproxy env:
Sys.setenv('SHINYPROXY_USERGROUPS' = 'WHITEBOOKREDAKTØRER,TESTGROUP,MATERIALEFORUM')
# 
# # Get the user name and user groups of the current user for authorization
# ldap_username <- Sys.getenv('SHINYPROXY_USERNAME')
ldap_usergroups <- as.list(strsplit(Sys.getenv('SHINYPROXY_USERGROUPS'), ",")[[1]]) # converts comma separated string from env var into an R list

# UI
edatabasesTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "edatabases",
          box(width = 12, solidHeader = TRUE, id = "edatabasesheader",
              h3("Licenser"),
              img(src='icons/eressourcer_negativ_45x45.png', align = "right", height="46px")
          ),     
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset45",
                          # Insert only the follow tab and contents if user belongs to the materialeforum group
                          if ('MATERIALEFORUM' %in% ldap_usergroups) {
                          tabPanel("Generelt", 
                                   fluidRow(
                                     column(12,
                                            column(2,
                                                   h4("Afgrænsning"),
                                                   tags$br(),
                                                   selectInput(ns("old_erms_produkt"),"eRessource:",  sort(unique(licenser_overblik$eressource))),
                                                   # selectInput(ns("old_erms_aar"),"År:", unique(licenser_overblik$aar)),
                                                   checkboxGroupInput(ns("old_erms_aar"),
                                                                      'År:',
                                                                      sort(unique(as.character(licenser_overblik$aar))),
                                                                      selected = as.character(max(licenser_overblik$aar)),
                                                                      inline = F),
                                                   tags$br(),tags$br(),
                                                   xlsxDownloadUI(ns("eres_produkt_xlsx")),
                                                   tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>')),
                                                   if ('MATERIALEFORUM' %in% ldap_usergroups) {
                                                     tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Gem til pdf</a>'))
                                                   }
                                            ),
                                            column(10,
                                                fluidRow(
                                                    column(10,
                                                           h3("Licenser, overblik"),
                                                           span("Følgende statistik stammer fra"),a("bibstats.dbc.dk", href = "https://bibstats.dbc.dk", target="_blank" ),
                                                           span(", fra"),a("Faktor", href = "https://faktor.nu/reports", target="_blank" ),
                                                           span("samt"),a("Pubhub", href = "https://admin.puhub.dk", target="_blank" ),
                                                           p('Y-aksens betegnelse "tilgange" dækker over eRessourcernes ret forskellige måder, at opgøre brug på. Begrebet dækker således både over downloads, visninger og lign.'),
                                                           p('N.B! Tal, der stammer fra Faktor (hovedparten), har pt. (sommer 2019) fortsat beta status')
                                                    )
                                                ),
                                                fluidRow(
                                                    column(10,
                                                           actionButton("button", "Uge", style='margin:10px;background-color:DeepSkyBlue;color:white;font-weight:bold;'),
                                                           actionButton("button", "Måned", style='margin:10px;background-color:DeepSkyBlue;color:white;font-weight:bold;'),
                                                           actionButton("button", "Kvartal", style='margin:10px;background-color:DeepSkyBlue;color:white;font-weight:bold;'),
                                                           actionButton("button", "År", style='margin:10px;background-color:DeepSkyBlue;color:white;font-weight:bold;')
                                                    )
                                                ),
                                                fluidRow(
                                                      column(10,
                                                             tags$br(),tags$br(),
                                                             tags$div( h4(htmlOutput(ns("old_erms_dyn_title_1"))),style = "text-align: center;" ),
                                                             tags$div( h5(htmlOutput(ns("old_erms_dyn_title_3"))),style = "text-align: center;" ),
                                                             withSpinner(plotlyOutput(ns("old_erms_plot"))),
                                                             tags$br(),tags$br(),
                                                             column(10,
                                                                    tags$div(h4(htmlOutput(ns("old_erms_dyn_title_2")))),
                                                                    tags$div(h5(htmlOutput(ns("old_erms_dyn_title_4")))),
                                                                    withSpinner(formattableOutput(ns("old_erms_table")), proxy.height="150px")
                                                             )
                                                      )
                                                )
                                            )
                                     )
                      ))
                          }
                            ))))
}

# SERVER
edatabasesTabPanel <- function(input, output, session, data, tablename) {
  
  # Ordn månederne efter deres normale rækkefølge, ikke alfabetet...
  licenser_overblik$maaned <- factor(danskemåneder(licenser_overblik$maaned), levels = c("Januar","Februar","Marts","April","Maj","Juni","Juli","August","September","Oktober","November","December"))
  
  old_erms_df <- reactive({
    licenser_overblik %>%
     filter(eressource == input$old_erms_produkt) %>%
    #mutate(aar = as.character(aar)) %>%
    spread(key = aar, value = antal) %>%
    #coalesce(as.data.frame(.),0) %>%
    mutate_at(vars(-1:-6), funs(replace(., is.na(.), 0))) %>%
    #select(aar %in% input$old_erms_aar)
    select(datakilde,eressource,maaned,kilde,bibliotek,rapport_dato,input$old_erms_aar)
    #select(datakilde,eressource,maaned,kilde,bibliotek,rapport_dato,c('2018','2019'))
  })
  
  # Call Excel download function for tables 
  callModule(xlsxDownload, "edatabases", data = reactive(dbc_eres_stats_df()), name = "eressourcer_overblik")
  
  # Render the plot
  output$old_erms_plot <- renderPlotly({
    
    colNames <- names(old_erms_df())[-1:-6]      # ie. get all colnames except the first thru the sixth
    len <- length(colNames)                      # the number of columns with years
    years <- unique(licenser_overblik$aar)       # get list of the years in the dataframe for assigning persistent colors to the bars
    aar1 = names(old_erms_df())[7]               # the columns having the data for each year start at column 7
    h = which(years==aar1)                       # also needed for persistent colors
    
    p <- plot_ly(old_erms_df(),
                 x = ~maaned, 
                 y = as.formula(paste0("~`", aar1,"`")),
                 type = 'bar',
                 name = aar1,
                 text = as.formula(paste0("~`", aar1,"`")),
                 textposition = 'outside',
                 marker = list(color = colors[h-1]),        # use the year's index in the year list to assign a persistent color
                 hoverinfo='none'
    )
    
    if( len > 1) {
      for(i in 2:len){
        trace <- colNames[i]
        j = which(years==trace)
        p <- p %>% add_trace(y = as.formula(paste0("~`", trace,"`")), 
                             type = 'bar', 
                             name = trace, 
                             text = as.formula(paste0("~`", trace,"`")),
                             textposition = 'outside',
                             marker = list(color = colors[j-1]),       # use the year's index in the year list to assign a persistent color
                             hoverinfo='none'
        )
      }
    }

    p %>% layout(separators = ',.',
                 autosize = TRUE,
                 barmode = 'group',
                 yaxis = list(title = 'Antal tilgange', exponentformat = 'none'),
                 xaxis = list(title = 'Måned', dtick = 1, autotick = FALSE)
                 )
    
  })
  
  year_df <- reactive({
    licenser_overblik %>%
      filter(eressource == input$old_erms_produkt) %>%
      filter(aar %in% input$old_erms_aar)
  })
    
  # Create dynamic titles based on the filter choices (two outputs with the same value)
  output$old_erms_dyn_title_1 <- output$old_erms_dyn_title_2 <- renderText( paste0(input$old_erms_produkt," ", toString(unique(year_df()$aar)) ) )
  output$old_erms_dyn_title_3 <- output$old_erms_dyn_title_4 <- renderText( paste0("Kilde: ", unique(year_df()$kilde)," - Datakilde: ", toString(unique(year_df()$datakilde)), " - Rapportdato: ", toString(unique(year_df()$rapport_dato)) ) )
  
  erms_table_df <- reactive({
    licenser_overblik %>%
      filter(eressource == input$old_erms_produkt) %>%
      select(aar,maaned,antal) %>%
      spread(key = maaned, value = antal) %>%
      filter(aar %in% input$old_erms_aar) %>%
      mutate(aar = as.character(aar)) %>%
      rename(.,År = aar) %>%
      adorn_totals(c("col")) %>%
      mutate_at(vars(c(-1)), funs(format(round(as.numeric(.), 0), nsmall=0, big.mark=".", decimal.mark=",")))
  })
  
  output$old_erms_table <- renderFormattable({ formattable(erms_table_df()) })
}

