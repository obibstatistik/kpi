### DB QUERIES ###
drv <- dbDriver("PostgreSQL")
con_dwh <- dbConnect(drv, dbname = dbname_dwh, host = host_dwh, port = port_dwh, user = user_dwh, password = password_dwh)

licenser_overblik <- dbGetQuery(con_dwh, "
SELECT 
      case
        when maaned in (1,2,3) then '1. kvartal'
    		when maaned in (4,5,6) then '2. kvartal'
    		when maaned in (7,8,9) then '3. kvartal'
    		when maaned in (10,11,12) then '4. kvartal'
    	end kvartal,       
    	case
        when maaned in (1,2,3,4,5,6) then '1. halvår'
    		when maaned in (7,8,9,10,11,12) then '2. halvår'
    	end halvaar,  	
    	* FROM (
      SELECT aar yr,'DBC' datakilde,vendor eressource,aar,maaned,antal,vendor kilde,isil bibliotek,null::date rapport_dato
             FROM eressourcer.x_forfatterweb_faktalink
             WHERE stattype = 'visits'
             UNION ALL
             SELECT aar yr,
                    'Faktor' datakilde,
                    eressource,
                    aar,
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
              SELECT extract(year from dato) yr,
               'DBC' datakilde, 
               'Filmstriben' eressource,
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
              SELECT extract(year from oprettet) yr,
               'DBC' datakilde,
               'Ereolen - ' || bogtype eressource,
               extract(year from oprettet) aar,
               extract(month from oprettet) maaned,
               count(*) antal,
               'eReolen' kilde,
               'Odense' bibliotek,
               null::date rapport_dato
               FROM eressourcer.x_pubhub_udlaan
               WHERE extract(year from oprettet) >= 2017
               group by datakilde,eressource,aar,maaned,kilde,bibliotek,rapport_dato) a")

dbDisconnect(con_dwh)

# # Necessary for testing outside shinyproxy env:
#Sys.setenv('SHINYPROXY_USERGROUPS' = 'WHITEBOOKREDAKTØRER,TESTGROUP,MATERIALEFORUM')
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
                          tabPanel("Generelt", 
                                   if ('MATERIALEFORUM' %in% ldap_usergroups) {
                                   fluidRow(
                                     column(12,
                                            column(2,
                                                   h4("Afgrænsning"),
                                                   tags$br(),
                                                   selectInput(ns("old_erms_produkt"),"eRessource:",  sort(unique(licenser_overblik$eressource))),
                                                   checkboxGroupInput(ns("old_erms_aar"),
                                                                      'År:',
                                                                      sort(unique(as.character(licenser_overblik$aar))),
                                                                      selected = as.character(max(licenser_overblik$aar)),
                                                                      inline = F),
                                                   tags$br(),tags$br(),
                                                   xlsxDownloadUI(ns("eres_produkt_xlsx")),
                                                   tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
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
                                                      radioButtons( ns("old_erms_radio_btns"), "", c("Årlig" = "yr", "Halvårlig" = "halvaar", "Kvartalsvis" = "kvartal", "Månedlig" = "maaned") )
                                                  )
                                                ),
                                                fluidRow(
                                                      column(10,
                                                             tags$br(),tags$br(),
                                                             tags$div( h4(htmlOutput(ns("old_erms_dyn_title_1"))),style = "text-align: center;" ),
                                                             tags$div( h5(htmlOutput(ns("old_erms_dyn_title_3"))),style = "text-align: center;" ),
                                                             withSpinner(plotlyOutput(ns("old_erms_plot"))),
                                                             tags$br(),tags$br(),
                                                             column(12,
                                                                    tags$div(h4(htmlOutput(ns("old_erms_dyn_title_2")))),
                                                                    tags$div(h5(htmlOutput(ns("old_erms_dyn_title_4")))),
                                                                    withSpinner(formattableOutput(ns("old_erms_table")), proxy.height="150px")
                                                ))))))})))))
}

# SERVER
edatabasesTabPanel <- function(input, output, session, data, tablename) {
  
  # Ordn månederne efter deres normale rækkefølge, ikke alfabetet...
  licenser_overblik$maaned <- factor(danskemåneder(licenser_overblik$maaned), levels = c("Januar","Februar","Marts","April","Maj","Juni","Juli","August","September","Oktober","November","December"))
  
  old_erms_df <- reactive({
    licenser_overblik %>%
    filter(eressource == input$old_erms_produkt) %>%
    group_by_(input$old_erms_radio_btns, 'datakilde','eressource','aar','kilde','bibliotek','rapport_dato') %>% 
    summarise(antal = sum(antal)) %>%
    spread(key = aar, value = antal) %>%
    mutate_at(vars(-1:-6), funs(replace(., is.na(.), 0))) %>%
    select(datakilde,eressource,input$old_erms_radio_btns,kilde,bibliotek,rapport_dato,input$old_erms_aar)
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
                 x = as.formula(paste0("~`", input$old_erms_radio_btns,"`")),
                 y = as.formula(paste0("~`", aar1,"`")),
                 type = 'bar',
                 name = aar1,
                 text = as.formula(paste0("~`", aar1,"`")),
                 textposition = 'outside',
                 marker = list(color = colors[h-1]),        # use the year's index in the year list to assign a persistent color to years
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
                             marker = list(color = colors[j-1]),
                             hoverinfo='none'
        )
      }
    }
    p %>% config(displayModeBar = F, showLink = F)
    p %>% layout(separators = ',.',
                 autosize = TRUE,
                 barmode = 'group',
                 yaxis = list(title = 'Antal tilgange', exponentformat = 'none'),
                 xaxis = list(title = "", dtick = 1, autotick = FALSE)
                 # xaxis = list(title = input$old_erms_radio_btns, dtick = 1, autotick = FALSE)
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
