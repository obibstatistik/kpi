### DB QUERIES ###
drv <- dbDriver("PostgreSQL")
con_dwh <- dbConnect(drv, dbname = dbname_dwh, host = host_dwh, port = port_dwh, user = user_dwh, password = password_dwh)
#tablecomment <- dbGetQuery(con, paste0("select obj_description('",schema,".", table, "'::regclass)"))
#(select regexp_matches(obj_description('eressourcer.x_filmstriben_forbrug'::regclass),'[0-9]{4}-[0-9]{2}-[0-9]{2}','g'))[1]::date rapport_dato

mainstream_eres <- dbGetQuery(con_dwh, "
SELECT 
				dato::date laan_dato,
		    count(*) antal,
		    sum(pris) omkostninger,
		    'film' mattype
FROM eressourcer.x_filmstriben_forbrug
WHERE extract(year from dato) >= 2012
GROUP BY laan_dato
UNION ALL
SELECT 
				oprettet::date laan_dato,
		    count(*) antal,
		    sum(pris) omkostninger,
		    bogtype mattype
FROM eressourcer.x_pubhub_udlaan
WHERE extract(year from oprettet) >= 2012
GROUP BY laan_dato,mattype
")

# mainstream_loan_phys_vs_eres_df <- dbGetQuery(con_dwh, "
# SELECT i.uge,fysiske,filmstriben
# FROM
# 	(SELECT to_char(dato, 'YYYY - uge IW') uge, sum(antal) fysiske FROM cicero.udlaan
# 	WHERE gmb = 'm' AND smb='th'
# GROUP BY uge) y
# FULL JOIN 
# 	(SELECT to_char(dato, 'YYYY - uge IW') uge,count(*) filmstriben FROM eressourcer.x_filmstriben_forbrug
# 	 WHERE dato >= '2017-01-01'
#      GROUP BY uge) i
# ON i.uge = y.uge
# ")

mainstream_loan_phys_vs_eres_df <- dbGetQuery(con_dwh, "
SELECT coalesce(dato,datoen) dato,coalesce(fysiske,0) fysiske,coalesce(filmstriben,0) filmstriben
FROM
(select dato, sum(antal) fysiske from cicero.udlaan
     WHERE gmb = 'm' AND smb='th'
     group by dato) y
full outer join
(select dato::date datoen, count(*) filmstriben from eressourcer.x_filmstriben_forbrug
        WHERE dato >= '2017-01-01'
        group by datoen) i
  ON datoen = dato
 order by dato
  ")

# SELECT i.aar,i.uge,fysiske,filmstriben
# FROM
# 	(SELECT date_part('year', dato) aar,date_part('week', dato) uge, sum(antal) fysiske FROM cicero.udlaan
# 	WHERE gmb = 'm' AND smb='th'
# GROUP BY aar,uge) y
# FULL JOIN 
# 	(SELECT date_part('year', dato) aar,date_part('week', dato) uge,count(*) filmstriben FROM eressourcer.x_filmstriben_forbrug
# 	 WHERE dato >= '2017-01-01'
#      GROUP BY aar,uge) i
# ON i.aar = y.aar AND i.uge = y.uge

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
               group by datakilde,eressource,aar,maaned,kilde,bibliotek,rapport_dato) a
")

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
                          tabPanel("Overblik", 
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
                                                   xlsxDownloadUI(ns("licenser_overblik")),
                                                   tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                            ),
                                            column(10,
                                                fluidRow(
                                                    column(10,
                                                           tags$head(
                                                             # Style the error message when no year is checked off
                                                             tags$style(HTML("#shiny-tab-edatabases .shiny-output-error-validation { color: black; font-size: 20px; }"))
                                                           ),
                                                           h3("Licenser, overblik"),
                                                           span("Følgende statistik stammer fra"),a("bibstats.dbc.dk", href = "https://bibstats.dbc.dk", target="_blank" ),
                                                           span(", fra"),a("Faktor", href = "https://faktor.nu/reports", target="_blank" ),
                                                           span("samt"),a("Pubhub", href = "https://admin.pubhub.dk", target="_blank" ),
                                                           p('Y-aksens betegnelse "tilgange" dækker over eRessourcernes ret forskellige måder, at opgøre brug på. Begrebet dækker således både over downloads, visninger og lign.'),
                                                           p('N.B! Tal, der stammer fra Faktor (hovedparten), har pt. (sommer 2019) fortsat beta status')
                                                    )
                                                ),
                                                fluidRow(
                                                  column(10,
                                                        # Duelling action buttons (only one active at a time, like radio buttons)
                                                        div(
                                                          # uiOutput(ns("yr-btn")),
                                                          # uiOutput(ns("halvaar-btn")),
                                                          # uiOutput(ns("halvaar-btn")),
                                                          # uiOutput(ns("maaned-btn"))
                                                          actionButton(ns("yr"), "Årlig"),
                                                          actionButton(ns("halvaar"), "Halvårlig"),
                                                          actionButton(ns("kvartal"), "Kvartalsvis"),
                                                          actionButton(ns("maaned"), "Månedlig")
                                                        , style="float:right")
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
                                                ))))))}
                                   ),
                          tabPanel("Mainstreamlicenser", 
                                   if ('MATERIALEFORUM' %in% ldap_usergroups) {
                                     fluidRow(
                                       column(12,
                                              h2("Mainstreamlicenser"),
                                              tags$br(),tags$br(),tags$br(),tags$br()
                                       ),
                                       column(12,
                                              column(1,
                                                     h4("Afgrænsning"),
                                                     tags$br(),tags$br()
                                                     # checkboxGroupInput(ns("old_erms_aar"),
                                                     #                    'År:',
                                                     #                    sort(unique(as.character(licenser_overblik$aar))),
                                                     #                    selected = as.character(max(licenser_overblik$aar)),
                                                     #                    inline = F)
                                              ),
                                              column(5,
                                                      h3("Udgifter, andele"),
                                                      tags$br(),tags$br(),
                                                      withSpinner(plotlyOutput(ns("mainstream_cost_pie"))),
                                                      tags$br(),tags$br()
                                              ),
                                              column(1,
                                                     h4("Afgrænsning"),
                                                     tags$br(),tags$br()
                                              ),
                                              column(5,
                                                     h3("Udlån, andele"),
                                                     span("Følgende statistik stammer fra"),a("bibstats.dbc.dk", href = "https://bibstats.dbc.dk", target="_blank" ),
                                                     withSpinner(plotlyOutput(ns("mainstream_loan_pie"))),
                                                     tags$br(),tags$br()
                                              )
                                       ),
                                       column(12,
                                              column(2,
                                                     h4("Afgrænsning"),
                                                     tags$br(),tags$br(),
                                                     selectInput(ns("phys_vs_eres_periode"),"Periode:", c('dato','uge','maaned','aar')),
                                                     tags$br(),tags$br(),
                                                     dateRangeInput(ns('dateRange_phys_vs_eres'),
                                                                    label = 'Datointerval',
                                                                    start = Sys.Date() - 182, end = Sys.Date(),
                                                                    separator = " - "
                                                     )
                                                     # ,selectInput(ns("oms_pukt"),"eRessource:",  sort(unique(licenser_overblik$eressource)))
                                              ),
                                              column(10,
                                                     h3("Film udlån, fysiske vs. Filmstriben"),
                                                     tags$br(),tags$br(),
                                                     withSpinner(plotlyOutput(ns("mainstream_loan_phys_vs_eres"))),
                                                     tags$br(),tags$br(),tags$br(),tags$br()
                                              )
                                       ),
                                       column(12,
                                              column(2,
                                                     h4("Afgrænsning"),
                                                     tags$br(),
                                                     selectInput(ns("mainstream_mattype"),"eRessource:", c('E-bog','Film' = 'film','Lydbog','Podcast','Alle')),
                                                     checkboxGroupInput(ns("mainstream_aar"),
                                                                        'År:',
                                                                        sort(unique(lubridate::year(mainstream_eres$laan_dato))),
                                                                        selected = as.character(max(lubridate::year(mainstream_eres$laan_dato))),
                                                                        inline = F),
                                                     tags$br(),tags$br(),
                                                     xlsxDownloadUI(ns("mainstream_udgifter_xlsx")),
                                                     tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                              ),
                                              column(10,
                                                     fluidRow(
                                                       column(10,
                                                              tags$head(
                                                                # Style the error message when no year is checked off
                                                                tags$style(HTML("#shiny-tab-edatabases .shiny-output-error-validation { color: black; font-size: 20px; }"))
                                                              ),
                                                              tags$div( h3(htmlOutput(ns("mainstream_dyn_title_1")))),
                                                              span("Følgende statistik stammer fra"),a("conman.dbc.dk/filmstriben", href = "https://conman.dbc.dk/filmstriben", target="_blank" ),
                                                              span("samt"),a("Pubhub", href = "https://admin.pubhub.dk", target="_blank" )
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(10,
                                                              # Duelling action buttons (only one active at a time, like radio buttons)
                                                              div(
                                                                actionButton(ns("aar_ms"), "Årlig"),
                                                                actionButton(ns("halvaar_ms"), "Halvårlig"),
                                                                actionButton(ns("kvartal_ms"), "Kvartalsvis"),
                                                                actionButton(ns("maaned_ms"), "Månedlig"),
                                                                actionButton(ns("uge_ms"), "Ugentlig")
                                                                , style="float:right")
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(10,
                                                              tags$br(),tags$br(),
                                                              withSpinner(plotlyOutput(ns("mainstream_plot"))),
                                                              tags$br(),tags$br(),
                                                              tags$div( h3(htmlOutput(ns("mainstream_dyn_title_2")))),
                                                              column(12,
                                                                     withSpinner(formattableOutput(ns("mainstream_table")), proxy.height="150px")
                                                              ))))
                                              )
                                       )
                              }
                          )
                  ))))
}

# SERVER
edatabasesTabPanel <- function(input, output, session, data, tablename) {
  
  # Ordn månederne efter deres normale rækkefølge, ikke alfabetet...
  licenser_overblik$maaned <- factor(danskemåneder(licenser_overblik$maaned), levels = c("Januar","Februar","Marts","April","Maj","Juni","Juli","August","September","Oktober","November","December"))

  # mechanics for the duelling buttons, cf. https://shiny.rstudio.com/articles/action-buttons.html
  # further styling stuff so the active button stays active despite falling out of focus: https://stackoverflow.com/questions/34574199/render-dueling-buttons-as-active-in-shiny
  v <- reactiveValues(data = 'maaned'
                      # ,yr_btn_class = "btn-default", 
                      # halvaar_btn_class = "btn-default", 
                      # kvartal_btn_class = "btn-default", 
                      # maaned_btn_class = "btn-primary"
                      )  # create reactive var with default value before the user clicks a button
  
  observeEvent(input$yr, {
    v$data <- 'yr'
    # v$yr_btn_class <- "btn-primary"
    # v$halvaar_btn_class <- "btn-default"
    # v$kvartal_btn_class <- "btn-default"
    # v$maaned_btn_class <- "btn-default"
  })
  
  observeEvent(input$halvaar, {
    v$data <- 'halvaar'
    # v$yr_btn_class <- "btn-default"
    # v$halvaar_btn_class <- "btn-primary"
    # v$kvartal_btn_class <- "btn-default"
    # v$maaned_btn_class <- "btn-default"
  })  
  
  observeEvent(input$kvartal, {
    v$data <- 'kvartal'
    # v$yr_btn_class <- "btn-default"
    # v$halvaar_btn_class <- "btn-default"
    # v$kvartal_btn_class <- "btn-primary"
    # v$maaned_btn_class <- "btn-default"
  })
  
  observeEvent(input$maaned, {
    v$data <- 'maaned'
    # v$yr_btn_class <- "btn-default"
    # v$halvaar_btn_class <- "btn-default"
    # v$kvartal_btn_class <- "btn-default"
    # v$maaned_btn_class <- "btn-primary"
  })  
  
  # output$yr <- renderUI({
  #   actionButton("yr", "yr-btn", class=v$yr_btn_class)
  # })
  # 
  # output$halvaar <- renderUI({
  #   actionButton("halvaar", "halvaar-btn", class=v$halvaar_btn_class)
  # })
  # 
  # output$kvartal <- renderUI({
  #   actionButton("kvartal", "kvartal-btn", class=v$kvartal_btn_class)
  # })
  # 
  # output$maaned <- renderUI({
  #   actionButton("maaned", "maaned-btn", class=v$maaned_btn_class)
  # })

  
  old_erms_df <- reactive({
    # Show error if a year is not checked off
    validate(
      need(input$old_erms_aar != "", "VÆLG MINDST ÈT ÅR I VENSTREMENUEN")
    )
    licenser_overblik %>%
    filter(eressource == input$old_erms_produkt) %>%
    #group_by_(input$old_erms_radio_btns, 'datakilde','eressource','aar','kilde','bibliotek','rapport_dato') %>% 
    group_by_(v$data, 'datakilde','eressource','aar','kilde','bibliotek','rapport_dato') %>% 
    summarise(antal = sum(antal)) %>%
    spread(key = aar, value = antal) %>%
    mutate_at(vars(-1:-6), funs(replace(., is.na(.), 0))) %>%
    select(datakilde,eressource,v$data,kilde,bibliotek,rapport_dato,input$old_erms_aar)
    # select(datakilde,eressource,input$old_erms_radio_btns,kilde,bibliotek,rapport_dato,input$old_erms_aar)
    # select(datakilde,eressource,input$last_btn,kilde,bibliotek,rapport_dato,input$old_erms_aar)
    # select(datakilde,eressource,ifelse(!is.character(input$last_btn),'yr',input$last_btn),kilde,bibliotek,rapport_dato,input$old_erms_aar)
    # select(datakilde,eressource,ifelse(is.character(input$last_btn),yr,yr),kilde,bibliotek,rapport_dato,input$old_erms_aar)
  })

      output$old_erms_plot <- renderPlotly({
        colNames <- names(old_erms_df())[-1:-6]      # ie. get all colnames except the first thru the sixth
        len <- length(colNames)                      # the number of columns with years
        years <- unique(licenser_overblik$aar)       # get list of the years in the dataframe for assigning persistent colors to the bars
        aar1 = names(old_erms_df())[7]               # the columns having the data for each year starting at column 7
        h = which(years==aar1)                       # also needed for persistent colors
        
        p <- plot_ly(old_erms_df(),
                     # x = as.formula(paste0("~`", input$old_erms_radio_btns,"`")),
                     x = as.formula(paste0("~`", v$data,"`")),
                     # x = as.formula(paste0("~`", input$last_btn,"`")),
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
    # Show error if a year is not checked off
    validate(
      need(input$old_erms_aar != "", "")
    )
    licenser_overblik %>%
    filter(eressource == input$old_erms_produkt) %>%
    filter(aar %in% input$old_erms_aar)
  })
  
  # Create dynamic titles based on the filter choices (two outputs with the same value)
  output$old_erms_dyn_title_1 <- output$old_erms_dyn_title_2 <- renderText( paste0(input$old_erms_produkt," ", toString(unique(year_df()$aar)) ) )
  output$old_erms_dyn_title_3 <- output$old_erms_dyn_title_4 <- renderText( paste0("Kilde: ", unique(year_df()$kilde)," - Datakilde: ", toString(unique(year_df()$datakilde)), " - Rapportdato: ", toString(unique(year_df()$rapport_dato)) ) )
  
  erms_table_df <- reactive({
    # Show error if a year is not checked off
    validate(
      need(input$old_erms_aar != "", "")
    )
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
  
  # Call Excel download function for tables 
  callModule(xlsxDownload, "licenser_overblik", data = reactive(erms_table_df()), name = "eRessourcer_licenser_overblik")
  
  output$old_erms_table <- renderFormattable({ formattable(erms_table_df()) })
  
  
  ############################################# MAINSTREAM RESSOURCERNE ############################################################
  
  mainstream_cost_mattype <- reactive({
    # Show error if an eRessource is not checked off
    # validate(
    #   need(input$old_erms_aar != "", "VÆLG MINDST ÈN RESSOURCE I VENSTREMENUEN")
    # )
    mainstream_eres %>%
    mutate(year = lubridate::year(laan_dato)) %>%
    group_by(mattype) %>% 
    summarise(omkostninger = sum(omkostninger)) 
  })
  
  output$mainstream_cost_pie <- renderPlotly({
    plot_ly(mainstream_cost_mattype(), 
            labels = ~mattype, 
            values = ~omkostninger, 
            text = ~paste(round((omkostninger / sum(omkostninger))*100, 0),"%",sep=""), # denne og følgende linje runder procenterne af, så de er uden decimaler
            textinfo = 'text',
            textfont = list(color = '#FFFFFF'), 
            marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  mainstream_loan_mattype <- reactive({
    # Show error if an eRessource is not checked off
    # validate(
    #   need(input$old_erms_aar != "", "VÆLG MINDST ÈN RESSOURCE I VENSTREMENUEN")
    # )
    mainstream_eres %>%
      mutate(year = lubridate::year(laan_dato)) %>%
      group_by(mattype) %>% 
      summarise(antal = sum(antal)) 
  })
  
  output$mainstream_loan_pie <- renderPlotly({
    plot_ly(mainstream_loan_mattype(),
            labels = ~mattype, 
            values = ~antal, 
            text = ~paste(round((antal / sum(antal))*100, 0),"%",sep=""), # denne og følgende linje runder procenterne af, så de er uden decimaler
            textinfo = 'text',
            textfont = list(color = '#FFFFFF'), 
            marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  mainstream_loan_phys_vs_eres_reac <- reactive({
    mainstream_loan_phys_vs_eres_df %>%
    mutate(filmstriben = replace_na(filmstriben, 0)) %>%
    mutate(fysiske = replace_na(fysiske, 0)) %>%
    filter( dato >= input$dateRange_phys_vs_eres[1] & dato <= input$dateRange_phys_vs_eres[2]) %>%
    mutate(aar = lubridate::year(dato)) %>%
    mutate(maaned = lubridate::month(dato)) %>%
    mutate(uge = lubridate::isoweek(dato)) %>%
    group_by_(input$phys_vs_eres_periode) %>%
    summarise(fysiske = sum(fysiske),filmstriben = sum(filmstriben))
    #spread(key = aar, value = antal) %>%
    #mutate_at(vars(-1:-6), funs(replace(., is.na(.), 0))) %>%
    #select(datakilde,eressource,v$data,kilde,bibliotek,rapport_dato,input$old_erms_aar)
  })
  
  output$mainstream_loan_phys_vs_eres <- renderPlotly({
    plot_ly(mainstream_loan_phys_vs_eres_reac()) %>%
            add_lines(x = as.formula(paste0("~`", input$phys_vs_eres_periode ,"`")),
                      y = ~fysiske,
                      name = 'DVD og Blu-ray',
                      line = list(color = color3)) %>%
            add_lines(x = as.formula(paste0("~`", input$phys_vs_eres_periode ,"`")),
                      y = ~filmstriben,
                      name = 'Filmstriben',
                      line = list(color = color4)) %>%
            layout(
              #xaxis = list(title="Uge nr.", showgrid = TRUE),
              xaxis = list(title="Uge nr.", showgrid = TRUE, tickmode='linear'),
              yaxis = list(
                tickfont = list(color = color3),
                title = "Udlån")
            )
  })
  
  mainstream_eres <- mainstream_eres %>% mutate(maaned = lubridate::month(laan_dato))
  mainstream_eres$maaned <- factor(danskemåneder(mainstream_eres$maaned), levels = c("Januar","Februar","Marts","April","Maj","Juni","Juli","August","September","Oktober","November","December"))

  p <- reactiveValues(data = 'maaned'
  )  # create reactive var with default value before the user clicks a button
  
  observeEvent(input$yr_ms, {
    p$data <- 'aar'
  })
  
  observeEvent(input$halvaar_ms, {
    p$data <- 'halvaar'
  })  
  
  observeEvent(input$kvartal_ms, {
    p$data <- 'kvartal'
  })
  
  observeEvent(input$maaned_ms, {
    p$data <- 'maaned'
  })  
  
  observeEvent(input$uge_ms, {
    p$data <- 'uge'
  }) 
  
  mainstream_cost_reac <- reactive({
   mainstream_eres %>%
      # validate(
      #   need(input$mainstream_aar != "", "VÆLG MINDST ÈT ÅR I VENSTREMENUEN")
      # )
      filter(mattype == input$mainstream_mattype) %>%
      mutate(uge = paste0("uge ",formatC(lubridate::isoweek(laan_dato),width=2,format="d",flag="0"))) %>%    # formatC() is used to leftpad weeknumbers so they sort properly
      mutate(kvartal = paste0(lubridate::quarter(laan_dato),".kvartal")) %>%
      mutate(halvaar = paste0(ifelse(lubridate::quarter(laan_dato) <= 2,1,2 ),".halvår")) %>%
      mutate(aar = lubridate::year(laan_dato)) %>%
      # group_by_('maaned','mattype','aar') %>%
      group_by_(p$data,'mattype','aar') %>%
      summarise(udgifter = sum(omkostninger)) %>%
      spread(key = aar, value = udgifter) %>%
      mutate_at(vars(-1:-2), funs(replace(., is.na(.), 0))) %>%
      select(p$data,mattype,as.character(input$mainstream_aar))
      # select('maaned',mattype,'2012')
  })
  
  output$mainstream_plot <- renderPlotly({
    colNames <- names(mainstream_cost_reac())[-1:-2]      # ie. get all colnames except the first and second
    len <- length(colNames)                               # the number of columns with years
    years <- unique(lubridate::year(mainstream_eres$laan_dato))           # get list of the years in the dataframe for assigning persistent colors to the bars
    aar1 = names(mainstream_cost_reac())[3]               # the columns having the data for each year starting at column 7
    h = which(years==aar1)                                # also needed for persistent colors
    
    p <- plot_ly(mainstream_cost_reac(),
                 x = as.formula(paste0("~`", p$data,"`")),
                 y = as.formula(paste0("~`", aar1,"`")),
                 type = 'bar',
                 name = aar1,
                 # text = as.formula(paste0("~`", aar1,"`")),
                 # textposition = 'outside',
                 marker = list(color = colors[h])        # use the year's index in the year list to assign a persistent color to years
                 #, hoverinfo='none'
    )
    
    if( len > 1) {
      for(i in 2:len){
        trace <- colNames[i]
        j = which(years==trace)
        p <- p %>% add_trace(y = as.formula(paste0("~`", trace,"`")), 
                             type = 'bar', 
                             name = trace, 
                             # text = as.formula(paste0("~`", trace,"`")),
                             # textposition = 'outside',
                             marker = list(color = colors[j])
                             # ,hoverinfo='none'
        )
      }
    }
    p %>% config(displayModeBar = F, showLink = F)
    p %>% layout(separators = ',.',
                 autosize = TRUE,
                 barmode = 'group',
                 yaxis = list(title = '', exponentformat = 'none', ticksuffix = 'kr.',hoverformat = '.2f'),
                 xaxis = list(title = "", dtick = 1, autotick = FALSE)
    )
  })
  
  ##### TEST-TABLE ######
  output$mainstream_table <- renderFormattable({ 
    formattable(mainstream_cost_reac() %>%
                  mutate_at(vars(c(-1:-2)), 
                            funs(paste0(format(round(as.numeric(.), 2), nsmall=2, big.mark=".", decimal.mark=","),' kr.'))
               ) %>% ungroup() %>% select(p$data,as.character(input$mainstream_aar))
    )
  })
  
  # Create dynamic titles based on the filter choices (two outputs with the same value)
  output$mainstream_dyn_title_1 <- renderText( 
                                      paste0( "Mainstream-licenser, ",
                                              switch(p$data, uge = 'ugentlige', maaned = 'månedlige', kvartal = 'kvartalsvise', halvaar = 'halvårlige', aar = 'årlige'),
                                              " udgifter ",
                                              toString(input$mainstream_aar) 
                                      ) 
                                   )
  
  output$mainstream_dyn_title_2 <- renderText( paste0("Udgifter til ", toString(input$mainstream_mattype)) )
  
  callModule(xlsxDownload, "mainstream_udgifter_xlsx", data = reactive(mainstream_cost_reac()), name = "mainstream_licenser_udgifter")
}
