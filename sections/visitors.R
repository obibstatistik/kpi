# UI

visitorsTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "visits",
          box(width = 12, solidHeader = TRUE, id="spaceheader1",
              h3("Besøgende"),
              img(src='icons/detfysiskerum_negativ_45x45.png', align = "right", height="46px")
          ),      
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset1",
                          tabPanel("Generelt",
                                   fluidRow(
                                     column(6,
                                            # tags$div(
                                            #    HTML(paste(h3("N.B! Den ene af to gates i Borgernes Hus har ikke leveret data siden 9.maj. Besøgstal, der inkluderer denne, er derfor misvisende.",style="color:red")))
                                            # ),
                                            h4("Info"),
                                            p("Besøgstallet stemmer ikke fuldstændigt overens med de officielle besøgstal, da der fortages forskellige justeringer grundet specifikke forhold i OBB."),
                                            p("Der har været udfordringer med tælleren i Korup, hvilket bevirker, at der ikke er opsamlet besøgstal siden marts. Der arbejdes på at løse problemet."),
                                            p("* I 2017 blev Borgernes Hus bygget, hvorfor besøgsmønstret var mærkbart anderledes end øvrige år.")
                                     )
                                   ),
                                   fluidRow(
                                     column(12,
                                       column(2,
                                              tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                       ),
                                       column(10,
                                              h4("Samlet besøg på OBB"),
                                              p("Grafen viser det samlede besøg på OBB fordelt pr. år. De grå søjler er hele året, men farvede søjler i forgrunden er år til dato. Det er dermed muligt at sammenligne indeværende års besøg med de forrige."),
                                              column(10, tags$div( withSpinner(samedate_barchartOutput(ns('whity'))) ))
                                       )
                                     ),
                                     column(12,tags$hr()),
                                     column(12,
                                       column(2,
                                              h4("Afgræns"),
                                              selectInput(ns("mainlibrary"), "Total/Lokal:",c('Med Hovedbiblioteket','Uden Hovedbiblioteket')),
                                              tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                     ),
                                     column(10,
                                              h4("Samlet besøg på OBB"),
                                              p("Denne graf viser samlet besøg på OBB fordelt pr. år med mulighed for at vælge hovedbibliotekets besøg fra via vælger i venstre side."),
                                              withSpinner(plotlyOutput(ns("visitsplotall")))
                                       )
                                     ),
                                     column(12,tags$hr()),
                                     column(12,
                                       column(2,
                                              h4("Afgræns"),
                                              selectInput(ns("visitors_fromyear"), "År 1:", c("2019" = "2019", "2018" = "2018", "2017" = "2017", "2016" = "2016", "2015" = "2015", "2014" = "2014"), as.integer(format(Sys.Date(), "%Y"))-1),
                                              selectInput(ns("visitors_toyear"), "År 2:", c("2019" = "2019", "2018" = "2018", "2017" = "2017", "2016" = "2016", "2015" = "2014", "2014" = "2013"), as.integer(format(Sys.Date(), "%Y"))),
                                              selectInput(ns("visitorslibrary"), "Bibliotek:", c("Alle" = "all","Bolbro" = "bo","Dalum" = "da","Højby" = "hoj","Historiens Hus" = "lok","Holluf Pile" = "ho","Borgernes Hus" = "hb","Korup" = "kor","Musikbiblioteket" = "mus","Tarup" = "ta","Vollsmose" = "vo")),
                                              selectInput(ns("mainlibrary2"), "Total/Lokal:",c('Med Hovedbiblioteket','Uden Hovedbiblioteket')),
                                              xlsxDownloadUI(ns("visitors_table")),
                                              tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))  
                                       ),
                                       column(10,
                                              h4("Samlet besøg på OBB"),
                                              p("Tabellen viser det samlede besøg på OBB fordelt pr. måned."),
                                              p("Visningen giver mulighed for at sammenligne mellem to forskellige år samt vælge hvilken lokation der ønskes vist. Det er desuden muligt at vælge Hovedbiblioteket til og fra."),
                                              withSpinner(formattableOutput(ns("visitors_table")))
                                       )
                                     ),
                                     column(12,tags$hr()),
                                     column(12,
                                       column(2,
                                              h4("Afgræns"),
                                              selectInput(ns("norm"), "Indekstal/tal:",c('Ikke Normaliseret' = 'not_norm','Indeks 2016' = 'norm')),
                                              selectInput(ns("mainlibrary3"), "Total/Lokal:",c('Med Hovedbiblioteket','Uden Hovedbiblioteket')),
                                              xlsxDownloadUI(ns("csv_visitors_per_branch")),
                                              tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                       ),
                                       column(10,
                                              h4("Besøg fordelt på bibliotek"),
                                              p("Grafen viser besøget på de enkelte lokationer i OBB i de seneste 5 år. Der er to visninger: ”Ikke normaliseret, hvor antal besøg vises som det reelt er. Og ”Normaliseret” hvor 2016 er brugt som basisår og har værdien 1. Det gør det muligt lettere at sammenligne på tværs af lokationer."),
                                              p("Det er muligt at fravælge og vælge enkelte år via enkeltklik på året i diagrammets højre side. Ved dobbeltklik på et år vælges kun dette år."),
                                              plotlyOutput(ns("visitsplotindividual")),
                                              withSpinner(formattableOutput(ns("visitorstest")))
                                       )
                                     )
                                   )   
                          ),
                          tabPanel("Timer",
                                   fluidRow(
                                     column(6,
                                            # tags$div(
                                            #   HTML(paste(h3("N.B! Den ene af to gates i Borgernes Hus har ikke leveret data siden 9.maj. Besøgstal, der inkluderer denne, er derfor misvisende.",style="color:red")))
                                            # ),
                                            h4("Info"),
                                            p("Visningen giver overblik over besøget på de enkelte lokationer for en given periode fordelt på timer."),
                                            p("Det er muligt at vælge lokationer til og fra for at sammenligne på tværs."),
                                            p("Det er muligt at sammenligne 2 tidsperioder via valg i drop-down. Sørg for at perioderne er sammenlignelige."),
                                            p("Det er muligt at ændre på timeintervallerne, hvorved totaler og procenter ændres. Den valgte visning vil altid summe til 100%."),
                                            p("Det er muligt under outputmetode at vælge om besøg skal vises som ”Antal” eller ”Procent af total”."),
                                            p("OBS. Der tages ikke højde for forskellige åbningstider over tid og der skelnes ikke mellem ugedage.")
                                           )
                                   ),
                                   fluidRow(
                                     column(12,
                                       column(2,
                                              h4("Afgræns"),       
                                              checkboxGroupInput(ns("visitors_hours_library"), label = 'Vælg bibliotek', 
                                              selected = list("Bolbro","Dalum","Højby","Historiens Hus","Holluf Pile","Borgernes Hus","Korup","Musikbiblioteket","Tarup","Vollsmose"),
                                              choices = list("Bolbro","Dalum","Højby","Historiens Hus","Holluf Pile","Borgernes Hus","Korup","Musikbiblioteket","Tarup","Vollsmose")),
                                              xlsxDownloadUI(ns("csv_visitors_per_hour")),
                                              tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>')),
                                              tags$hr(),
                                              dateRangeInput(ns("daterange_visitors_hours_library"),
                                                             label = 'Vælg dato periode 1',
                                                             start = Sys.Date() - 90, end = Sys.Date(),
                                                             separator = " - "
                                              ),
                                              checkboxInput(ns("smooth"), "Sammenlign med anden periode"),
                                              conditionalPanel(
                                                paste0("input['", ns("smooth"), "']"),
                                                dateRangeInput(ns("daterange2_visitors_hours_library"),
                                                               label = 'Vælg dato periode 2',
                                                               start = 0, end = 0,
                                                               separator = " - "
                                                )
                                              ),
                                              tags$hr(),
                                              sliderInput(ns("range"), "Vælg tids periode:",
                                                          min = 0, max = 24,
                                                          value = c(8,16)),
                                              tags$hr(),
                                              selectInput(ns("numberpercent"), "Output metode:",c('Antal' = 'count','Procent af total' = 'percent')) 
                                       ),
                                       column(10,
                                              h4("Besøg fordelt på timer og periode"), 
                                              withSpinner(formattableOutput(ns("visitors_per_hours_table"))),
                                              conditionalPanel(
                                                paste0("input['", ns("smooth"), "']"),
                                                withSpinner(formattableOutput(ns("visitors_per_hours_table2")))
                                              )
                                         )
                                   )
                               )
                          )
                          #,tabPanel("Data",
                          #  h4("Generelt"),
                          #  p("Data fra 2014-01-01"),
                          #  metaTabPanelUI(id = "people_counter"),
                          #  column(12,tags$hr()),
                          #  h4("Timer"),
                          #  p("Data fra 2017-04-07"),
                          #  metaTabPanelUI(id = "visitor_counter")
                          #)
                   )
            )
          )
  )
}

# SERVER

visitorsTabPanel <- function(input, output, session, data, tablename) {

  drv <- dbDriver("PostgreSQL")
  con_dwh <- dbConnect(drv, dbname = dbname_dwh, host = host_dwh, port = port_dwh, user = user_dwh, password = password_dwh)
  
  visitors_per_year <- dbGetQuery(con_dwh, "SELECT * FROM visitors.visitors_per_year") 
  visitors_per_day <- dbGetQuery(con_dwh, "SELECT date, location, visitor_count as count FROM visitors.visitors_per_day")
  visitors_hours <- dbGetQuery(con_dwh, "SELECT * FROM visitors.visitors_per_hour")  

  dbDisconnect(con_dwh)
  ### ###
  
  # basic calculation
  visitors2 <- visitors_per_day %>%
    mutate(year = year(date)) %>%
    select(year, date, count) %>%
    group_by(year, date) %>%
    summarise(count = sum(count)) %>%
    arrange(date) %>%
    mutate(to_date=cumsum(count)) %>%
    select(-count) %>%
    rename(count = to_date)
  
  # get last day in year
  visitors3 <- visitors2 %>%
    group_by(year) %>% 
    filter(date == max(date) | date == Sys.Date()) %>%
    mutate(date = format(as.POSIXct(date, tz = "GMT", format, tryFormats = c("%Y-%m-%d %H:%M:%OS"), optional = FALSE)))
  
  # get current day
  visitors4 <- visitors2 %>%
    filter(if_else(date < '2015-11-17', month(date) == month(Sys.Date()), date == Sys.Date()-1 | date == Sys.Date() - years(1)-1 | date == Sys.Date() - years(2)-1 | date == Sys.Date() - years(3)-1 )) %>%
    mutate(date = if_else(date < '2015-11-17', date + (day(Sys.Date())-2), date, NULL)) %>% #sets dates of old data to current day of month 
    #mutate(count = if_else(date < '2015-11-17', count*(mday(date)/days_in_month(Sys.Date())), count)) %>% #calculates count of old data to fraction of month gone
    mutate(date = format(as.POSIXct(date, tz = "GMT", format, tryFormats = c("%Y-%m-%d %H:%M:%OS"), optional = FALSE)))  
  
  visitors6 <- rbind(visitors3, visitors4)
  visitors6 <- visitors6 %>%
   arrange(year, desc(date))

  curDate <- format(Sys.Date()-1, format="%Y-%m-%d") # the matching date you want data from, across all the years on the x-axis
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

  output$whity <- renderSamedate_barchart({
    samedate_barchart(visitors6,curDate,sortx,frontColors,backColor,labelx,labely,tickNumY,showScaleY,barWidth,barsOffset)
  })

  output$visitors_stack_table <- renderFormattable({formattable(visitors6)})

  # visitors total plot#
  output$visitsplotall <- renderPlotly({
    visitsoverview <- visitors_per_year %>%
      mutate(year = year(date)) %>%
      filter(year != as.integer(format(Sys.Date(), "%Y"))) %>%
      filter(if(input$mainlibrary == 'Uden Hovedbiblioteket')  (location != 'hb') else TRUE) %>%
      group_by(year) %>%
      summarise(sum = sum(visitor_count)) 
    plot_ly(visitsoverview, x = visitsoverview$year, y = visitsoverview$sum, type = 'bar', marker = list(color = color2)) %>%
      layout(autosize = TRUE, yaxis = list(title = 'Antal'), xaxis = list(title = 'År', dtick = 1, autotick = FALSE, autorange="reversed"))
  })
  
  # visitors table details
  
  visitors_table <-reactive({
    visitors <- visitors_per_day %>%
      select(date, count, location) %>%
      filter(if(input$mainlibrary2 == 'Uden Hovedbiblioteket')  (location != 'hb') else TRUE) %>%
      mutate(month = month(date)) %>%
      mutate(visitor_year = year(date)) %>%
      filter(visitor_year == input$visitors_fromyear | visitor_year == input$visitors_toyear ) %>%
      filter(if(input$visitorslibrary != 'all')  (location == input$visitorslibrary) else TRUE) %>%
      select(count, month, visitor_year) %>%
      group_by(month, visitor_year) %>%
      summarise(sum = sum(count)) %>%
      spread(key = visitor_year, value = sum) %>%
      ungroup(.self) %>%
      mutate(akku1 = cumsum(.[[2]]), akku2 = cumsum(.[[3]]), mdr = percent(((.[[3]]-.[[2]])/(.[[2]])), digits = 0)) %>%
      mutate(akk = percent((akku2-akku1)/akku1, digits = 0)) %>%
      select(c(1,2,4,3,5,6,7)) %>%
      mutate_at(vars(-1), funs(replace(., is.na(.), 0))) %>%
      mutate_at(vars(c(-1,-6,-7)), funs(format(round(as.numeric(.), 0), nsmall=0, big.mark=".", decimal.mark=","))) %>%
      mutate_at(vars(1), funs(danskemåneder(.))) %>%
      rename(Måned = month, Akkumuleret = akku1, "Akkumuleret " = akku2, 'Ændring pr. mdr.' = mdr, 'Ændring akkumuleret' = akk)
  })
  
  output$visitors_table <- renderFormattable({
    formattable(visitors_table(), list(
      'Ændring pr. mdr.' = formatter("span", style = x ~ style(color = ifelse(x < 0 , color9, color1)), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
      'Ændring akkumuleret' = formatter("span", style = x ~ style(color = ifelse(x < 0 , color9, color1)), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
    ))
  })
  
  #callModule(csvDownload, "visitors_table", data = visitors_table(), name = "visitors")
  callModule(xlsxDownload, "visitors_table", data = reactive(visitors_table()), name = "visitors")
  
  #visitors branch plot #
  visitors1 <- visitors_per_day %>%
    mutate(
      location = case_when(
        location == "bo" ~ "Bolbro",
        location == "vo" ~ "Vollsmose",
        location == "da" ~ "Dalum",
        location == "ta" ~ "Tarup",
        location == "hb" ~ "Borgernes Hus",
        location == "lok" ~ "Historiens Hus",
        location == "hoj" ~ "Højby",
        location == "ho" ~ "Holluf Pile",
        location == "kor" ~ "Korup",
        location == "mus" ~ "Musikbiblioteket",
        is.na(location) ~ "nope",
        TRUE ~ as.character(location)
      ) 
    )
  
  output$visitsplotindividual <- renderPlotly({
    if (input$norm == "norm") {visitorsbranch <- visitors1 %>%
      filter(if(input$mainlibrary3 == 'Uden Hovedbiblioteket')  (location != 'Borgernes Hus') else TRUE) %>%
      select(date, count, location) %>%
      mutate(year = year(date)) %>%
      group_by(location, year) %>%
      summarise(sum = sum(count)) %>%
      spread(key = year, value = sum) %>%
      ungroup(.self) %>%
      mutate(`2014` = `2014`/`2016`, `2015` = `2015`/`2016`, `2017` = `2017`/`2016`, `2018` = `2018`/`2016`, `2019` = `2019`/`2016`, `2016` = 1 ) 
    } 
    else {visitorsbranch <- visitors1 %>%
      filter(if(input$mainlibrary3 == 'Uden Hovedbiblioteket')  (location != 'Borgernes Hus') else TRUE) %>%
      select(date, count, location) %>%
      mutate(year = year(date)) %>%
      group_by(location, year) %>%
      summarise(sum = sum(count)) %>%
      spread(key = year, value = sum) %>%
      ungroup(.self)} 
    plot_ly(visitorsbranch, x = ~location, y = ~`2014`, type = 'bar', name = '2014', marker = list(color = color1)) %>%
      add_trace(y = ~`2015`, name = '2015', marker = list(color = color2)) %>%
      add_trace(y = ~`2016`, name = '2016', marker = list(color = color3)) %>%
      add_trace(y = ~`2017`, name = '2017', marker = list(color = color4)) %>%
      add_trace(y = ~`2018`, name = '2018', marker = list(color = color5)) %>%
      layout(autosize = TRUE, yaxis = list(title = 'Antal'), xaxis = list(title = 'Bibliotek'), barmode = 'group')
  })
  
  visitors_per_branch <- reactive({
    if (input$norm == "norm") {visitorsbranch2 <- visitors1 %>%
      filter(if(input$mainlibrary3 == 'Uden Hovedbiblioteket')  (location != 'Borgernes Hus') else TRUE) %>%
      select(date, count, location) %>%
      mutate(year = year(date)) %>%
      filter(year != as.integer(format(Sys.Date(), "%Y"))) %>%
      group_by(location, year) %>%
      summarise(sum = sum(count)) %>%
      spread(key = year, value = sum) %>%
      rename(Bibliotek = location) %>%
      mutate(`2014` = `2014`/`2016`, `2015` = `2015`/`2016`, `2017` = `2017`/`2016`, `2018` = `2018`/`2016`, `2019` = `2019`/`2016`, `2016` = 1) %>%
    mutate_at(vars(c(-1)), funs(format(round(as.numeric(.), 0), nsmall=0, big.mark=".", decimal.mark=","))) #%>%
      #mutate_at(vars(c(1,2,3,4,5)), funs(replace(., is.na(.), 0))) 
    } 
    else {visitorsbranch2 <- visitors1 %>%
      filter(if(input$mainlibrary3 == 'Uden Hovedbiblioteket')  (location != 'Borgernes Hus') else TRUE) %>%
      select(date, count, location) %>%
      mutate(year = year(date)) %>%
      filter(year != as.integer(format(Sys.Date(), "%Y"))) %>%
      group_by(location, year) %>%
      summarise(sum = sum(count)) %>%
      spread(key = year, value = sum) %>%
      rename(Bibliotek = location) %>%
      mutate_at(vars(c(-1)), funs(format(round(as.numeric(.), 0), nsmall=0, big.mark=".", decimal.mark=","))) #%>%
      #mutate_at(vars(c(1,2,3,4,5)), funs(format(round(as.numeric(.), 0), nsmall=0, big.mark=".")))
    }
  })
  
  output$visitorstest <- renderFormattable({
    formattable(visitors_per_branch(), align = (c('l','r','r','r','r','r'))
    )
  })
  
  #callModule(csvDownload, "csv_visitors_per_branch", data = visitors_per_branch(), name = "visitors_per_branch")
  callModule(xlsxDownload, "csv_visitors_per_branch", data = reactive(visitors_per_branch()), name = "visitors_per_branch")
  
  # visitors pr. hour
  
  visitors_hours <- visitors_hours %>%
    mutate(
      location = case_when(
        visitors_hours$location == "bo" ~ "Bolbro",
        visitors_hours$location == "vo" ~ "Vollsmose",
        visitors_hours$location == "da" ~ "Dalum",
        visitors_hours$location == "ta" ~ "Tarup",
        visitors_hours$location == "hb" ~ "Borgernes Hus",
        visitors_hours$location == "lok" ~ "Historiens Hus",
        visitors_hours$location == "hoj" ~ "Højby",
        visitors_hours$location == "ho" ~ "Holluf Pile",
        visitors_hours$location == "kor" ~ "Korup",
        visitors_hours$location == "mus" ~ "Musikbiblioteket"
      )
    )
  
  visitors_per_hour <- reactive({
    if (input$numberpercent == "percent")
      visitors_hours <- visitors_hours %>%
        filter(location %in% input$visitors_hours_library) %>%
        filter(visit_date_hour >= input$daterange_visitors_hours_library[1]) %>%
        filter(visit_date_hour < input$daterange_visitors_hours_library[2]) %>% 
        select(visit_date_hour, location, count) %>%
        mutate(tid = hour(visit_date_hour)) %>%
        filter(tid > input$range[1] & tid < input$range[2]) %>%
        select(-visit_date_hour) %>%
        group_by(location, tid) %>%
        summarise(sum = sum(count)) %>%
        spread(key = location, value = sum) %>%
        adorn_totals("row") %>%
        mutate_at(vars(-tid), funs(replace(., is.na(.), 0))) %>%
        mutate_at(vars(-tid), funs(ifelse( is.na(.), NA, procenten((. / sum(.)*2)))))
    else 
      visitors_hours <- visitors_hours %>%
        filter(location %in% input$visitors_hours_library) %>%
        filter(visit_date_hour >= input$daterange_visitors_hours_library[1]) %>%
        filter(visit_date_hour < input$daterange_visitors_hours_library[2]) %>% 
        select(visit_date_hour, location, count) %>%
        mutate(tid = hour(visit_date_hour)) %>%
        filter(tid > input$range[1] & tid < input$range[2]) %>%
        select(-visit_date_hour) %>%
        group_by(location, tid) %>%
        summarise(sum = sum(count)) %>%
        spread(key = location, value = sum) %>%
        adorn_totals("row") %>%
        mutate_at(vars(-tid), funs(replace(., is.na(.), 0))) %>%
        mutate_at(vars(-tid), funs(format(round(as.numeric(.), 0), nsmall=0, big.mark=".", decimal.mark=",")))
  })
  
  output$visitors_per_hours_table <- renderFormattable({
    formattable(visitors_per_hour())
  })
  
  #callModule(csvDownload, "csv_visitors_per_hour", data = visitors_per_hour(), name = "visitors_per_hour")
  callModule(xlsxDownload, "csv_visitors_per_hour", data = reactive(visitors_per_hour()), name = "visitors_per_hour")
    
  output$visitors_per_hours_table2<- renderFormattable({
    if (input$numberpercent == "percent")  
      visitors_hours <- visitors_hours %>%
        filter(location %in% input$visitors_hours_library) %>%
        filter(visit_date_hour > input$daterange2_visitors_hours_library[1] & visit_date_hour < input$daterange2_visitors_hours_library[2]) %>%
        select(visit_date_hour, location, count) %>%
        mutate(tid = hour(visit_date_hour)) %>%
        filter(tid > input$range[1] & tid < input$range[2]) %>%
        select(-visit_date_hour) %>%
        group_by(location, tid) %>%
        summarise(sum = sum(count)) %>%
        spread(key = location, value = sum) %>%
        mutate_at(vars(-tid), funs(replace(., is.na(.), 0))) %>%
        mutate_at(vars(-tid), funs(ifelse( is.na(.), NA, procenten((. / sum(.))))))
    else
      visitors_hours <- visitors_hours %>%
        filter(location %in% input$visitors_hours_library) %>%
        filter(visit_date_hour > input$daterange2_visitors_hours_library[1] & visit_date_hour < input$daterange2_visitors_hours_library[2]) %>%
        select(visit_date_hour, location, count) %>%
        mutate(tid = hour(visit_date_hour)) %>%
        filter(tid > input$range[1] & tid < input$range[2]) %>%
        select(-visit_date_hour) %>%
        group_by(location, tid) %>%
        summarise(sum = sum(count)) %>%
        spread(key = location, value = sum) %>%
        mutate_at(vars(-tid), funs(replace(., is.na(.), 0))) %>%
        mutate_at(vars(-tid), funs(format(round(as.numeric(.), 0), nsmall=0, big.mark=".", decimal.mark=",")))
    formattable(visitors_hours)
  })
  
}