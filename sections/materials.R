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
                                                  column(8,
                                                    tags$div( withSpinner(samedate_barchartOutput(ns('checkouts_samedate_plot'))) )
                                                  ),
                                                  column(10,
                                                    p("Udlånsstatistikken bygger på data fra Cicero. Der er ikke fuld gennemsigtighed i forhold til datakilden, hvorfor unøjagtigheder kan forekomme. Nyeste data har pt. en forsinkelse på tre døgn."),
                                                    p("* I 2017 blev Borgernes Hus bygget, hvorfor udlånsmønstret var mærkbart anderledes end øvrige år.")
                                                  )
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
                                                   withSpinner(plotlyOutput(ns("checkouts_plot_all")))
                                            )
                                     ),
                                     column(12,tags$div( tags$hr(), class = "hidden-print" )),
                                     column(12,
                                            column(2,
                                                   h4("Afgræns"),
                                                   # TODO hvad med Arresten (er det med??) og hvad med opsøgende (skal den ikke med ind under hb?? eller er den tom for udlån?)
                                                   selectInput(ns("checkouts_fromyear"), "Fra:", c("2019" = "2019", "2018" = "2018", "2017" = "2017", "2016" = "2016", "2015" = "2015", "2014" = "2014"), as.integer(format(Sys.Date(), "%Y"))-1),
                                                   selectInput(ns("checkouts_toyear"), "Til:", c("2019" = "2019", "2018" = "2018", "2017" = "2017", "2016" = "2016", "2015" = "2014", "2014" = "2013"), as.integer(format(Sys.Date(), "%Y"))),
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
                                                   withSpinner(formattableOutput(ns("checkouts_table")), proxy.height="400px")
                                            )
                                     ),
                                     column(12,tags$div( tags$hr(), class = "hidden-print" ))                                   
                                )
                          ),
                          tabPanel("Interurban", 
                                   fluidRow(
                                     column(12,
                                            column(2,
                                                   tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                            ),
                                            column(10,
                                                   h4("Interurban-udlån fra OBB"),
                                                   p("Grafen viser årets interurban-udlån fra OBB til andre biblioteksvæsner sammenlignet med sidste år. Farvede søjler i forgrunden viser antal pr. år til dato, mens den grå søjle i baggrunden er sidste års samlede interurban-udlån."),
                                                   column(8,
                                                       tags$div( withSpinner(samedate_barchartOutput(ns('interurban_samedate_plot'))) )
                                                   ),
                                                   column(10,
                                                       p("* Bemærk at udlånsstatistikker bygger på data fra Cicero. Der er ikke fuld gennemsigtighed i forhold til datakilden, hvorfor unøjagtigheder kan forekomme."), 
                                                       p("** Nyeste data har pt. en forsinkelse på tre døgn.")
                                                   )
                                             )
                                        ),
                                     column(12,tags$div( tags$hr(), class = "hidden-print" )),
                                     column(12,
                                            column(2,
                                                   tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>')),
                                                   xlsxDownloadUI(ns("interurban_month"))
                                            ),
                                            column(10,
                                                   h4("Interurban-udlån fra OBB pr. måned"),
                                                   p("Grafen viser årets interurban-udlån fra OBB til andre biblioteksvæsner, fordelt på måned og sammenlignet med sidste år."),
                                                   column(8,
                                                       tags$div( withSpinner(plotlyOutput(ns('interurban_months_plot'))) ),
                                                       tags$br(),
                                                       formattableOutput(ns("interurban_months_table"))
                                                   ),
                                                   column(10,
                                                       p("* Bemærk at udlånsstatistikker bygger på data fra Cicero. Der er ikke fuld gennemsigtighed i forhold til datakilden, hvorfor unøjagtigheder kan forekomme."), 
                                                       p("** Nyeste data har pt. en forsinkelse på tre døgn.")
                                                   )
                                             )
                                       )
                                )
                          ),
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
                                                withSpinner(plotlyOutput(ns("circ_join_plot"), height = "700px"))
                                         )
                                    )
                               )
                       ),
                       tabPanel("Lån pr. besøg", 
                                fluidRow(
                                  column(12,
                                         column(2,
                                                tags$br(),
                                                h4("Periode"),
                                                dateRangeInput(ns('dateRange_cpv'),
                                                               label = 'Vælg periode',
                                                               start = Sys.Date() - 21, end = Sys.Date(),
                                                               separator = " - "
                                                ),
                                                tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                         ),
                                         column(10,height = "900px",
                                                h4("Antal lån pr. besøg fordelt på biblioteker"),
                                                p("Grafen viser det gennemsnitlige udlån pr. eksemplar i bibliotekets beholdning over en given periode."),
                                                p("Perioden kan vælges i venstre side. Default er 21 dage"),
                                                p("* Der kan pt. kun anvendes tal fra 9. januar 2019 og frem, da tidligere datoer er forbundet med store usikkerheder."),
                                                p("** Interurbane udlån og andre autoudlån indgår pt. i beregningen, selvom det ikke er udlån til brugere, der fysisk besøger bibliotekerne."),
                                                withSpinner(plotlyOutput(ns("cpv_join_plot"), height = "700px")),
                                                p("Sammenlignet med filialerne vil Hovedbiblioteket have en større andel af udlånet, der ikke er udlån til besøgende, hvorfor udlån pr. besøg vil være relativit højt. Mange besøg af elever på kombibibliotekerne Holluf Pile og Højby bliver ikke talt i gaten og disse to lokationer vil ligeledes have et relativt højt udlån pr. besøg.")
                                         )
                                  )
                                )
                       )
                   ))))
}

# Server
materialsTabPanel <- function(input, output, session, data, tablename) {

  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  con_dwh <- dbConnect(drv, dbname = dbname_dwh, host = host_dwh, port = port_dwh, user = user_dwh, password = password_dwh)
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
  #dbc_eres_stats <- dbGetQuery(con, "SELECT * from dbc_eres_stats")
  comp_years_interurban <- dbGetQuery(con_dwh, "SELECT DISTINCT(EXTRACT(YEAR FROM transact_date)) aar,SUM(antal) sum,extract(YEAR FROM MAX(transact_date))::TEXT || '-12-31' dato
    FROM cicero.udlaan_per_laanersegment
    WHERE cat ILIKE '%ibliotek%'
    GROUP BY aar
    UNION ALL																   
    SELECT distinct(EXTRACT(YEAR FROM transact_date)) aar,SUM(antal) sum, extract(YEAR FROM transact_date)::text || to_char((SELECT max(transact_date) FROM cicero.udlaan_per_laanersegment), '-MM-DD') dato
    FROM cicero.udlaan_per_laanersegment
    WHERE cat ILIKE '%ibliotek%'
    AND to_char(transact_date,'MM-DD') <= to_char((SELECT max(transact_date) FROM cicero.udlaan_per_laanersegment), 'MM-DD')																					   
    GROUP BY aar
    ORDER BY aar,dato DESC")
  #comp_months_interurban <- dbGetQuery(con_dwh, "SELECT a.maaned,last_year,COALESCE(this_year,0) this_year
  comp_months_interurban <- dbGetQuery(con_dwh, "SELECT a.maaned,last_year, this_year
    FROM
    	(SELECT SUM(antal) last_year, EXTRACT(MONTH FROM transact_date) maaned 
    	 FROM cicero.udlaan_per_laanersegment 
    	 WHERE cat ILIKE '%ibliotek%' 
    	 AND EXTRACT(YEAR FROM transact_date) = EXTRACT(YEAR FROM (now() - INTERVAL '1 year')) 
    	 GROUP BY maaned) a
    LEFT JOIN 																																		
    	(SELECT SUM(antal) this_year, EXTRACT(MONTH FROM transact_date) maaned 
    	 FROM cicero.udlaan_per_laanersegment 
    	 WHERE cat ILIKE '%ibliotek%' 
    	 AND EXTRACT(YEAR FROM transact_date) = EXTRACT(YEAR FROM now()) 
    	 GROUP BY maaned) b
    ON a.maaned = b.maaned")
  cpv_df <- dbGetQuery(con, "select visits.branch,visits.dato,visits.sum visits,checkouts.sum checkouts from (
          select (
              case
                  when location = 'da' then 'Dalum Bibliotek'
                  when location = 'ta' then 'Tarup Bibliotek'
                  when location = 'vo' then 'Vollsmose Bibliotek'
                  when location = 'hoj' then 'Højby Bibliotek'
                  when location = 'ho' then 'Holluf Pile Bibliotek'
                  when location = 'bo' then 'Bolbro Bibliotek'
                  when location = 'kor' then 'Korup Bibliotek'
                  when location = 'hb' then 'Odense Hovedbibliotek'
              end) branch,
              to_char(registertime,'YYYY-MM-DD')::date dato,
              sum(delta)
              from public.visitor_counter
              where direction = 'In'
              and registertime > '2019-01-08'
              group by location, dato
      ) visits
      join (
          select branch, transact_date dato, sum(antal)
          from cicero.udlaan_per_opstillingsprofil
          where transact_date > '2019-01-08'
          group by branch, dato
      ) checkouts
      on checkouts.branch = visits.branch and checkouts.dato = visits.dato
      order by dato desc")
  dbDisconnect(con)
  dbDisconnect(con_dwh)
  
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
  
  # Reordering and renaming columns
  comp_years_interurban <- comp_years_interurban[c(1,3,2)]      # does the column order matter?
  names(comp_years_interurban) <- c("year","date","count")   # unsure if the column names matter, but renaming just to be safe
  
  # Render barchart comparing whole and partial years for interlibrary loans
  output$interurban_samedate_plot <- renderSamedate_barchart({
    samedate_barchart(comp_years_interurban,curDate,sortx,frontColors,backColor,labelx,labely,tickNumY,showScaleY,barWidth,barsOffset)
  })

  comp_months_interurban_shortnames <- comp_months_interurban %>% mutate_at(vars(1),funs(kortemåneder(.)))
  comp_months_interurban_shortnames$maaned <- factor(comp_months_interurban_shortnames$maaned, levels = c("jan","feb","mar","apr","maj","jun","jul","aug","sep","okt","nov","dec"))
  
  # Render barchart comparing current and last year month for month
  output$interurban_months_plot <- renderPlotly({
    comp_months_interurban_shortnames <- comp_months_interurban_shortnames %>%
    mutate_at(vars(-1), funs(replace(., is.na(.), 0)))
    plot_ly(comp_months_interurban_shortnames, x = ~maaned, y = ~`this_year`, type = 'bar', name = year(now()), marker = list(color = color1)) %>%
      add_trace(y = ~`last_year`, name = year(now()) -1 , marker = list(color = color2)) %>%
      layout(autosize = TRUE, yaxis = list(title = 'Antal'), xaxis = list(title = ''), barmode = 'group')
  })
  
  comp_months_interurban_longnames <- comp_months_interurban %>% mutate_at(vars(1),funs(danskemåneder(.)))
  comp_months_interurban_longnames$maaned <- factor(comp_months_interurban_longnames$maaned, levels = c("Januar","Februar","Marts","April","Maj","Juni","Juli","August","September","Oktober","November","December"))
  colnames(comp_months_interurban_longnames)[colnames(comp_months_interurban_longnames)=="this_year"] <- year(now())
  colnames(comp_months_interurban_longnames)[colnames(comp_months_interurban_longnames)=="last_year"] <- year(now()) -1 
  
  # Reactive function wrapping dataframe for interlibrary monthly comparison table incl. Excel download below
  interurban_month_tbl <- reactive({
    comp_months_interurban_longnames %>%
      mutate(akku1 = cumsum(.[[2]]), akku2 = cumsum(.[[3]]), mdr = percent(((.[[3]]-.[[2]])/(.[[2]])), digits = 0)) %>%
      mutate(akk = percent((akku2-akku1)/akku1, digits = 0)) %>%
      select(c(1,2,4,3,5,6,7)) %>%
      mutate_at(vars(-1), funs(replace(., is.na(.), 0))) %>%
      mutate_at(vars(c(-1,-6,-7)), funs(format(round(as.numeric(.), 0), nsmall=0, big.mark=".", decimal.mark=","))) %>%
      rename(Måned = maaned, Akkumuleret = akku1, "Akkumuleret " = akku2, 'Ændring pr. mdr.' = mdr, 'Ændring akkumuleret' = akk)
  })
  
  # Checkouts two-year comparison table
  output$interurban_months_table <- renderFormattable({
    formattable(interurban_month_tbl(), list(
      'Ændring pr. mdr.' = formatter("span", style = x ~ style(color = ifelse(x < 0 , color4, color1)), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
      'Ændring akkumuleret' = formatter("span", style = x ~ style(color = ifelse(x < 0 , color4, color1)), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
    ))
  })
  
  # Call Excel download function for tables 
  callModule(xlsxDownload, "interurban_month", data = reactive(interurban_month_tbl()), name = "Interurban_udlån_fra_OBB")
  
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
      mutate_at(vars(c(-1,-6,-7)), funs(format(round(as.numeric(.), 0), nsmall=0, big.mark=".", decimal.mark=","))) %>%
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

  # Circulation numbers. Horizontal barchart
  # TODO tilføj søjlepar, med gennemsnit ligesom kbh. (gerne med to andre farver, så de fremhæves) + vælger til magasin vs. udlån
  
  output$circ_join_plot <- renderPlotly({
    
    circ_behold <- beholdning %>%
       group_by(branch,dep) %>%
       summarise(sum = sum(sum))
    
    circ_behold_sum <- beholdning %>%
      select(sum) %>%
      summarise(sum = sum(sum))

    circ_behold_sum <- cbind(branch = 'OBB Samlet', dep = 'Alle', circ_behold_sum)
    
    circ_behold <- rbind(as.data.frame(circ_behold),as.data.frame(circ_behold_sum))

    circ_udlån <- checkouts_all %>%
      # filter( transact_date >= as.Date("2018-01-01") & transact_date <= as.Date("2018-06-30") ) %>%
      filter( transact_date >= input$dateRange_circ[1] & transact_date <= input$dateRange_circ[2]) %>%
      group_by(branch,dep) %>%
      summarise(sum = sum(antal))
    
    circ_checkouts_sum <- checkouts_all %>%
      # filter( transact_date >= '2018-01-01' & transact_date <= '2018-12-31') %>%
      filter( transact_date >= input$dateRange_circ[1] & transact_date <= input$dateRange_circ[2]) %>%
      select(antal) %>%
      summarise(sum = sum(antal))
    
    circ_checkouts_sum <- cbind(branch = 'OBB Samlet', dep = 'Alle', circ_checkouts_sum)
    
    circ_udlån <- rbind(as.data.frame(circ_udlån),as.data.frame(circ_checkouts_sum))
    
    circ_join <- full_join(circ_udlån, circ_behold, by = c("branch","dep")) %>%
      filter(!branch %in% c('Fællessamlingen',
                            'Lokalhistorisk Bibliotek',
                            'Odense Arrest',
                            'Opsøgende afdeling Odense Bibliotekerne')) %>%
      filter(dep %in% c('Børn','Voksen','Musik','Alle')) %>%
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
                hoverinfo = 'x',
                orientation = 'h', 
                name = 'Børn',
                marker = list(color = color2, 
                              width = 5)) %>%
      
      add_trace(y = ~circ_join$branch, 
                x = ~circ_join$Voksen, 
                type = 'bar', 
                hoverinfo = 'x',
                orientation = 'h', 
                name = 'Voksen',
                marker = list(color = color1, 
                              width = 5)) %>%
      
      add_trace(y = ~circ_join$branch, 
                x = ~circ_join$Alle, 
                type = 'bar', 
                hoverinfo = 'x',
                orientation = 'h', 
                name = 'Samlet',
                marker = list(color = color5, 
                              width = 15)) %>%

      layout(autosize = TRUE,
             title = "",
             hovermode = 'closest',
             margin = list(l = 200, r = 10, b = 50, t = 50, pad = 10),
             barmode = 'group',
             bargap = 0.4,
             #height = 600,
             #width = 800,
             yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, title = "", type = "category"),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, domain = c(0,2), title = "", type = "line", showgrid = TRUE))
  })
  
      
output$cpv_join_plot <- renderPlotly({

    cpv_plot_df <- cpv_df %>%
      filter( dato >= input$dateRange_cpv[1] & dato <= input$dateRange_cpv[2]) %>%
      # filter( dato >= '2019-01-09' & dato <= '2019-01-31' ) %>%
      group_by(branch) %>%
      summarise(visits = sum(visits), checkouts = sum(checkouts)) %>%
      mutate(cpv = format(round(checkouts / visits, 1)) ) %>%
      select(branch,cpv)
    
    cpv_sum <- cpv_df %>% 
      filter( dato >= input$dateRange_cpv[1] & dato <= input$dateRange_cpv[2]) %>%
      # filter( dato >= '2019-01-09' & dato <= '2019-01-31' ) %>%
      summarise(visits = sum(visits), checkouts = sum(checkouts)) %>%
      mutate(cpv = format(round(checkouts / visits, 1)) ) %>%
      select(cpv) %>%
      summarise(cpv = sum(as.numeric(cpv)))
    
    # extra, empty column called 'alle'
    cpv_plot_df$alle <- NA
    
    # create one-line dataframe with the collective cpv
    cpv_sum_line <- as.data.frame(c('OBB Samlet', NA, cpv_sum))
    colnames(cpv_sum_line) <- c('branch','cpv','alle')

    cpv_plot_df <- rbind(as.data.frame(cpv_plot_df),as.data.frame(cpv_sum_line))

    cpv_plot_df$branch <- factor(cpv_plot_df$branch, 
                               levels = unique(cpv_plot_df$branch)[order(cpv_plot_df$cpv, decreasing = FALSE)])
    
    plot_ly() %>%
      
      add_trace(y = ~cpv_plot_df$branch, 
                x = ~cpv_plot_df$alle, 
                type = 'bar', 
                hoverinfo = 'x',
                orientation = 'h', 
                name = 'Samlet udlån pr. besøg',
                marker = list(color = color5, width = 1)) %>%
      
      add_trace(y = ~cpv_plot_df$branch, 
                x = ~cpv_plot_df$cpv, 
                type = 'bar', 
                hoverinfo = 'x',
                orientation = 'h', 
                name = 'Udlån pr. besøg',
                marker = list(color = color2, width = 1)) %>%

      layout(autosize = TRUE,
             showlegend = FALSE,
             title = "",
             margin = list(l = 200, r = 10, b = 50, t = 50, pad = 10),
             barmode = 'group',
             bargap = 0.4,
             yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, title = "", type = "category"),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, domain = c(0,2), title = "", type = "line", showgrid = TRUE))
  })


  
}
