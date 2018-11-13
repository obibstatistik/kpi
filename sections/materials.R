source("global.R")
source("functions.R")
source("modules.R")
source("~/.postpass") 

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
#udlaan <- dbGetQuery(con, "SELECT name, hour, circulation_fact_count FROM cicero.udlaan_per_klokkeslaet")
#max_date <- dbGetQuery(con, "select max(transact_date) max_date from cicero.udlaan_per_opstillingsprofil")
checkouts_all <- dbGetQuery(con, "SELECT extract(year from transact_date) aar,transact_date,branch,dep,sum(antal) antal
        from cicero.udlaan_per_opstillingsprofil
        where extract(year from (transact_date)) > extract(year from (current_date - interval '5 year'))
        group by aar,transact_date,branch,dep
        order by aar,transact_date,branch,dep")
beholdning <- dbGetQuery(con, "SELECT branch,department dep,sum(material_dim_count)
    from cicero.beholdning
    group by branch,dep
    order by branch,dep")
dbc_eres_stats <- dbGetQuery(con, "SELECT * from dbc_eres_stats")
dbDisconnect(con)

  
# UI

materialsTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "physicalmat",
          
          box(width = 12, solidHeader = TRUE, id="materialsheader1", class = "pageheader",
              h3("Udlån"),
              img(src='icons/materialer_negativ_45x45.png', align = "right", height="46px")
          ),
          fluidRow(
            column(12,
                   tabBox(width = 12, id = "tabset2", 
                          tabPanel("Generelt",
                                   fluidRow(
                                     column(12,
                                           column(2,
                                                  tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                           ),
                                           column(10,
                                                  h4("Samlet udlån på OBB"),
                                                  p("Grafen viser det samlede udlån på OBB fordelt pr. år. De grå søjler er hele året, men farvede søjler i forgrunden er år til dato. Det er dermed muligt at sammenligne indeværende års udlån med de forrige."),
                                                  tags$div( samedate_barchartOutput(ns('checkouts_samedate_plot')) )
                                           )
                                      ),
                                     column(12,tags$div( tags$hr(), class = "hidden-print" )),
                                     column(12,
                                            column(2,
                                                   h4("Afgræns"),
                                                   selectInput(ns("checkouts_mainlibrary_filter1"), "Total/Lokal:",c('Med Hovedbiblioteket','Uden Hovedbiblioteket')),
                                                   tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                            ),
                                            column(10,
                                                   h4("Samlet udlån på OBB, hele år"),
                                                   p("Denne graf viser samlet udlån på OBB fordelt pr. år med mulighed for at vælge hovedbibliotekets udlån fra via vælger i venstre side."),
                                                   # N.B! Hvis man indlejrer plotlyOutput i tags, så kan plottet åbenbart ikke skalere, selv ikke hvis man reloader siden!
                                                   plotlyOutput(ns("checkouts_plot_all"))
                                            )
                                     ),
                                     column(12,tags$div( tags$hr(), class = "hidden-print" )),
                                     column(12,
                                            column(2,
                                                   h4("Afgræns"),
                                                   # TODO hvad med Arresten (er det med??) og hvad med opsøgende (skal den ikke med ind under hb?? eller er den tom for udlån?)
                                                   selectInput(ns("checkouts_fromyear"), "Fra:", c("2018" = "2018", "2017" = "2017", "2016" = "2016", "2015" = "2015", "2014" = "2014"), as.integer(format(Sys.Date(), "%Y"))-1),
                                                   selectInput(ns("checkouts_toyear"), "Til:", c("2018" = "2018", "2017" = "2017", "2016" = "2016", "2015" = "2014", "2014" = "2013"), as.integer(format(Sys.Date(), "%Y"))),
                                                   selectInput(ns("checkouts_library"), "Bibliotek:", c("Alle" = "all",
                                                                                                     "Bolbro" = "Bolbro Bibliotek",
                                                                                                     "Dalum" = "Dalum Bibliotek",
                                                                                                     "Højby" = "Højby Bibliotek",
                                                                                                     "Historiens Hus" = "Lokalhistorisk Bibliotek",
                                                                                                     "Holluf Pile" = "Holluf Pile Bibliotek",
                                                                                                     "Borgernes Hus" = "Odense Hovedbibliotek",
                                                                                                     "Korup" = "Korup Bibliotek",
                                                                                                     "Musikbiblioteket" = "Musikbiblioteket",
                                                                                                     "Tarup" = "Tarup Bibliotek",
                                                                                                     "Vollsmose" = "Vollsmose Bibliotek")),
                                                   selectInput(ns("checkouts_mainlibrary_filter2"), "Total/Lokal:",c('Med Hovedbiblioteket','Uden Hovedbiblioteket')),
                                                   xlsxDownloadUI(ns("checkouts")),
                                                   tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                            ),
                                            column(10,
                                                   h4("Udlån på OBB"),
                                                   p("Tabellen viser det samlede udlån på OBB fordelt pr. måned."),
                                                   p("Visningen giver mulighed for at sammenligne mellem to forskellige år samt vælge hvilken lokation der ønskes vist. Det er desuden muligt at vælge Hovedbiblioteket til og fra."),
                                                   p("N.B! Der er en forsinkelse på 3 dage på modtagelsen af seneste statistik fra Cicero"),
                                                   formattableOutput(ns("checkouts_table"))
                                            )
                                     ),
                                     column(12,tags$div( tags$hr(), class = "hidden-print" )),                                     
                                     column(12,
                                            column(2,
                                                   selectInput(ns("vendor"),"eRessource:", unique(dbc_eres_stats$vendor)),
                                                   selectInput(ns("stattype"),"Statistik:", unique(dbc_eres_stats$stattype)),
                                                   selectInput(ns("aar"),"År:", unique(dbc_eres_stats$aar)),
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
                                                   p("diverse test test test"),
                                                   plotlyOutput(ns("dbc_eres_stats_plot")),
                                                   formattableOutput(ns("dbc_eres_stats_table"))
                                            )
                                      )
                                    
                                )
                          ),
                       #   tabPanel("Timer", 
                       #           fluidRow(
                       #             column(2, h4("Afgræns")),
                       #             column(10,
                       #                   plotlyOutput(ns("heat")),
                       #                  tableOutput(ns('table'))
                       #             )
                       #           )
                       #  ),
                          tabPanel("Cirkulation", 
                                   fluidRow(
                                     column(12,
                                       column(2,
                                              tags$br(),
                                              h4("Periode"),
                                              dateRangeInput(ns('dateRange_circ'),
                                                             label = 'Vælg periode',
                                                             start = Sys.Date() - 182, end = Sys.Date(),
                                                             separator = " - "
                                              ),
                                              tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                       ),
                                       column(10,height = "900px",
                                              h4("Cirkulationstal fordelt på biblioteker og afdelingerne Børn/Voksen"),
                                              p("Grafen viser cirkulationstallet, dvs. gennemsnitligt udlån pr. eksemplar over en given periode."),
                                              p("Perioden kan vælges i venstre side. Default er et halvt år tilbage (182 dage)"),
                                              plotlyOutput(ns("circ_join_plot"), height = "700px")
                                       )
                                     )
                                   )
                          )
                          #,tabPanel("Data og dokumentation",
                          #         fluidRow(
                          #           column(12,
                          #                  p("Dokumentation")
                          #           )
                          #         )  
                          #)
                   ))))
}

# Server
materialsTabPanel <- function(input, output, session, data, tablename) {

  # Calculate latest date with data
  max_date <- checkouts_all %>%
    summarize(max_date = max(transact_date))
  
  # Calculate sums for partial years, adapting max_date to each year
  checkouts_partial_years <- checkouts_all %>%
    select(aar,transact_date,antal) %>%
    group_by(aar) %>%       # Create group objects for each year. filter() and summarise() will run for each
    filter( transact_date >= as.Date( paste0(aar,"-01-01" )) & transact_date <= as.Date( paste0(aar,substr(max_date[[1]],5,11) ))) %>%
    summarise(sum = sum(antal)) %>%
    mutate(dato = paste0(aar,substr(max_date[[1]],5,11))) 
  
  # Ditto whole years
  checkouts_whole_years <- checkouts_all %>%
    select(aar,transact_date,antal) %>%
    group_by(aar) %>%
    summarise(sum = sum(antal)) %>%
    mutate(dato = paste0(aar,"-12-31")) 
  
  # Union the partial and whole years and order them by year and date (desc) 
  checkouts_samedate <- rbind(checkouts_whole_years,checkouts_partial_years) %>%
    arrange(aar, desc(dato))
  
  # Reordering and renaming columns
  checkouts_samedate <- checkouts_samedate[c(1,3,2)]      # does the column order matter?
  names(checkouts_samedate) <- c("year","date","count")   # unsure if the column names matter, but renaming just to be safe
  
  # Config for barchart comparing whole and partial years
  curDate <- as.Date(max_date[[1]], format="%Y-%m-%d") # the matching date you want data from, across all the years on the x-axis
  sortx <- "desc"         # controls direction of the sorting of the years on the x-axis
  frontColors <- colors # this vector turns into a javascript array
  backColor <- "Gainsboro"
  vizWidth <- ""
  labelx <- "År"
  labely <- "Antal"
  tickNumY <- 7
  showScaleY <- "false"
  fontSizeX <- ""
  fontSizeY <- ""
  barWidth <- 0.8    # This is a percentage. 1 means no gap between bars (i.e. 100%)
  barsOffset <- 10
  
  # Render barchart comparing whole and partial years
  output$checkouts_samedate_plot <- renderSamedate_barchart({
    samedate_barchart(checkouts_samedate,curDate,sortx,frontColors,backColor,labelx,labely,tickNumY,showScaleY,barWidth,barsOffset)
  })
  
  # Whole years dataframe with branches for filtering main library
  checkouts_whole_years_branches <- checkouts_all %>%
    group_by(aar,branch) %>%
    summarise(sum = sum(antal))
  
  # Whole years plot filtering main library
  output$checkouts_plot_all <- renderPlotly({
    checkouts_overview <- checkouts_whole_years_branches %>%
      filter(aar != as.integer(format(Sys.Date(), "%Y"))) %>%      # we only want whole years so filter out current year
      filter(if(input$checkouts_mainlibrary_filter1 == 'Uden Hovedbiblioteket') (branch != 'Odense Hovedbibliotek') else TRUE) %>%
      group_by(aar) %>%
      summarise(sum = sum(sum))
    plot_ly(checkouts_overview, x = checkouts_overview$aar, y = checkouts_overview$sum, type = 'bar', marker = list(color = color1)) %>%
      layout(autosize = TRUE, yaxis = list(title = 'Antal'), xaxis = list(title = 'År', dtick = 1, autotick = FALSE, autorange="reversed"))
  })
  
   # Reactive function wrapping dataframe for checkouts two-year comparison table incl. Excel download below
    checkouts_all_tbl <- reactive({
      checkouts_all %>%
      select(aar,transact_date,antal,branch) %>%
      filter(if(input$checkouts_mainlibrary_filter2 == 'Uden Hovedbiblioteket') (branch != 'Odense Hovedbibliotek') else TRUE) %>%
      mutate(month = month(transact_date)) %>%
      filter(aar == input$checkouts_fromyear | aar == input$checkouts_toyear ) %>%
      filter(if(input$checkouts_library != 'all') (branch == input$checkouts_library) else TRUE) %>%
      select(antal, month, aar) %>%
      group_by(month, aar) %>%
      summarise(sum = sum(antal)) %>%
      spread(key = aar, value = sum) %>%
      ungroup(.self) %>%
      mutate(akku1 = cumsum(.[[2]]), akku2 = cumsum(.[[3]]), mdr = percent(((.[[3]]-.[[2]])/(.[[2]])), digits = 0)) %>%
      mutate(akk = percent((akku2-akku1)/akku1, digits = 0)) %>%
      select(c(1,2,4,3,5,6,7)) %>%
      mutate_at(vars(-1), funs(replace(., is.na(.), 0))) %>%
      mutate_at(vars(c(-1,-6,-7)), funs(format(round(as.numeric(.), 0), nsmall=0, big.mark="."))) %>%
      mutate_at(vars(1), funs(danskemåneder(.))) %>%
      rename(Måned = month, Akkumuleret = akku1, "Akkumuleret " = akku2, 'Ændring pr. mdr.' = mdr, 'Ændring akkumuleret' = akk)
  })

  # Checkouts two-year comparison table
  output$checkouts_table <- renderFormattable({
    formattable(checkouts_all_tbl(), list(
      'Ændring pr. mdr.' = formatter("span", style = x ~ style(color = ifelse(x < 0 , color4, color1)), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
      'Ændring akkumuleret' = formatter("span", style = x ~ style(color = ifelse(x < 0 , color4, color1)), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
    ))
  })
  
  # Call Excel download function for tables 
  callModule(xlsxDownload, "checkouts", data = reactive(checkouts_all_tbl()), name = "Udlån_på_OBB")
  
  dbc_eres_stats_df <- reactive({
    dbc_eres_stats <- dbc_eres_stats %>%
      #filter(if(input$mainlibrary3 == 'Uden Hovedbiblioteket')  (location != 'Borgernes Hus') else TRUE) %>%
      mutate_at(vars(1), funs(danskemåneder(.))) %>%
      filter(vendor == 'Forfatterweb') %>%
      filter(stattype == 'visits') %>%
      filter(aar == '2018') %>%
      select(isil,vendor,stattype,aar,maaned,antal) %>%
      #mutate(year = year(date)) %>%
      mutate_at(vars(5), funs(danskemåneder(.))) %>%
      group_by(maaned, isil) %>%
      #summarise(sum = sum(count)) %>%
      spread(key = isil, value = antal)
      #ungroup(.self)}
  })

  # Render the plot
  output$dbc_eres_stats_plot <- renderPlotly({   
    plot_ly(dbc_eres_stats, x = ~maaned, y = ~`746100`, type = 'bar', name = 'Odense', marker = list(color = color1)) %>%
      add_trace(y = ~`785100`, name = 'Aalborg', marker = list(color = color3)) %>%
      add_trace(y = ~`765700`, name = 'Herning', marker = list(color = color2)) %>%
      add_trace(y = ~`763000`, name = 'Vejle', marker = list(color = color4)) %>%
      add_trace(y = ~`775100`, name = 'Aarhus', marker = list(color = color5)) %>%
      add_trace(y = ~`710100`, name = 'København', marker = list(color = color6)) %>%
      add_trace(y = ~`726500`, name = 'Roskilde', marker = list(color = color7)) %>%
      add_trace(y = ~`715700`, name = 'Gentofte', marker = list(color = color8)) %>%
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
    
  
  #udlaan_heat <- udlaan %>%
  #  mutate(branch = ifelse(is.na(name), "Andet", name)) %>%
  #  group_by(branch, hour) %>%
  #  summarise(sum = sum(circulation_fact_count))
  
  #output$table <- renderTable(udlaan_heat)
  
  #output$heat <- renderPlotly({
  #  plot_ly(x=udlaan_heat$hour ,y=udlaan_heat$branch ,z = udlaan_heat$sum, type = "heatmap")
  #})
  
  # Circulation numbers. Horizontal barchart
  # TODO tilføj søjlepar, med gennemsnit ligesom kbh. (gerne med to andre farver, så de fremhæves) + vælger til magasin vs. udlån
  output$circ_join_plot <- renderPlotly({
    circ_behold <- beholdning %>%
      group_by(branch,dep) %>%
      summarise(sum = sum(sum))
    
    circ_udlån <- checkouts_all %>%
      #filter( transact_date >= as.Date("2018-01-01") & transact_date <= as.Date("2018-06-30") ) %>%
      filter( transact_date >= input$dateRange_circ[1] & transact_date <= input$dateRange_circ[2]) %>%
      group_by(branch,dep) %>%
      summarise(sum = sum(antal))
    
    circ_join <- full_join(circ_udlån, circ_behold, by = c("branch","dep")) %>%
      filter(!branch %in% c('Fællessamlingen',
                            'Lokalhistorisk Bibliotek',
                            'Odense Arrest',
                            'Opsøgende afdeling Odense Bibliotekerne')) %>%
      filter(dep %in% c('Børn','Voksen','Musik')) %>%
      #mutate(cirkulationstal = format(round(sum.x / sum.y, 1), nsmall=0, big.mark=".", decimal.mark=",") ) %>%
      mutate(cirkulationstal = format(round(sum.x / sum.y, 1)) ) %>%
      select(branch,dep,cirkulationstal) %>%
      spread(key = dep, value = cirkulationstal)
    
    # Sorting Y-axis. cf. https://stackoverflow.com/questions/40224892/r-plotly-barplot-sort-by-value
    circ_join$branch <- factor(circ_join$branch,
                               levels = unique(circ_join$branch)[order(circ_join$Voksen, decreasing = FALSE)])
    
    plot_ly() %>%
      
      add_trace(y = ~circ_join$branch, 
                x = ~circ_join$Børn, 
                type = 'bar', 
                orientation = 'h', 
                name = 'Børn',
                marker = list(color = color2, 
                              width = 5)) %>%
      
      add_trace(y = ~circ_join$branch, 
                x = ~circ_join$Voksen, 
                type = 'bar', 
                orientation = 'h', 
                name = 'Voksen',
                marker = list(color = color1, 
                              width = 5)) %>%
      
      layout(autosize = TRUE,
             title = "",
             margin = list(l = 200, r = 10, b = 50, t = 50, pad = 10),
             barmode = 'group',
             bargap = 0.4,
             #height = 600,
             #width = 800,
             yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, title = "", type = "category"),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, domain = c(0,2), title = "", type = "line", showgrid = TRUE))
  })
}