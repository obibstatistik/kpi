source("global.R")

shinyServer(function(input, output) {

  source("~/.postpass")
    
  ### Google Analytics API
  
  token <- Auth(client.id,client.secret)
  save(token,file="./token_file")
  load("./token_file")
  
  ### DB QUERIES ###
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  
  visits <- dbGetQuery(con, "SELECT * FROM public.people_counter")
  sqlloan <- dbGetQuery(con, "SELECT * FROM datamart.kpi_loan")
  #events <- dbGetQuery(con, "SELECT * FROM datamart.arrangementer")
  acquisition <- dbGetQuery(con, "SELECT * FROM public.imusic")
  
  dbDisconnect(con)
  
  ### VISITORS ###
  
  # visits plot #
  visitsplot <- visits %>%
    mutate(year = format(date, "%y"), v2017 = ifelse(year == "17", count, 0), v2016 = ifelse(year == "16", count, 0), v2015 = ifelse(year == "15", count, 0)) %>%
    group_by(location) %>%
    summarise(v2017 = sum(v2017), v2016 = sum(v2016), v2015 = sum(v2015)) %>%
    select(location,v2017,v2016,v2015)
  
  output$plot <- renderPlotly({
    plot_ly(visitsplot, x = visitsplot$location, y = visitsplot$v2015, type = 'bar', name = '2015', text = text, marker = list(color = 'gold')) %>%
    add_trace(y = visitsplot$v2016, name = '2016', marker = list(color = 'rgb(63,168,123)')) %>%  
    add_trace(y = visitsplot$v2017, name = '2017', marker = list(color = 'rgb(72,35,115)')) %>% 
    layout(yaxis = list(title = 'Antal'), barmode = 'group')
  })
  
  # visitor tables all #
  visitsall <- visits %>%
    mutate(month = format(date, "%m"), year = format(date, "%y"), visits2017 = ifelse(year == "17", count, 0), visits2016 = ifelse(year == "16", count, 0), visits2015 = ifelse(year == "15", count, 0)) %>%
    arrange(month) %>%
    group_by(month) %>%
    summarise(count = sum(count), visits2017 = sum(visits2017), visits2016 = sum(visits2016), visits2015 = sum(visits2015)) %>%
    mutate(diff1716 = percent((visits2017-visits2016)/visits2016), diff1615 = percent((visits2016-visits2015)/visits2015), cumsum2017 = cumsum(visits2017), cumsum2016 = cumsum(visits2016), cumsum2015 = cumsum(visits2015)) %>%
    select(month, visits2017, cumsum2017, diff1716, visits2016, cumsum2016, diff1615, visits2015, cumsum2015, count)
  
  colnames(visitsall) <- c("Måned", "2017", "2017 akum","17><16", "2016", "2016 akum", "16><15", "2015", "2015 akum", "Total")
  
  output$tablevisits <- renderFormattable({formattable(visitsall, list(
    "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
    "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
  ))})
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
       write.xlsx(visitsall, file)
    }
  )
  
  # visitor tables branch #
  visitslocations <- visits %>% distinct(location) %>% filter(location != "lok")
  
  visitsbranch <- visits %>%
    mutate(month = format(date, "%m"), year = format(date, "%y"), visits2017 = ifelse(year == "17", count, 0), visits2016 = ifelse(year == "16", count, 0), visits2015 = ifelse(year == "15", count, 0)) %>%
    arrange(month, location) %>%
    group_by(month, location) %>%
    summarise(count = sum(count), visits2017 = sum(visits2017), visits2016 = sum(visits2016), visits2015 = sum(visits2015)) %>%
    mutate(diff1716 = percent((visits2017-visits2016)/visits2016), diff1615 = percent((visits2016-visits2015)/visits2015)) %>%
    select(month, location, visits2017, diff1716, visits2016, diff1615, visits2015, count)
    
  foreach(i = visitslocations$location) %do% {
    local ({
      my_i <- i
      plotname <- paste0("table",my_i)
      visitsbranch <- visitsbranch %>% 
        filter(location == my_i) %>%
        group_by(month) %>%
        summarize(visits2017, visits2016, visits2015, diff1716, diff1615) %>%
        mutate (cumsum2017 = cumsum(visits2017), cumsum2016 = cumsum(visits2016), cumsum2015 = cumsum(visits2015)) %>%
        select(month, visits2017, cumsum2017, diff1716, visits2016, cumsum2016, diff1615, visits2015, cumsum2015)
      colnames(visitsbranch) <- c("Måned", "2017", "2017 akum","17><16", "2016", "2016 akum", "16><15", "2015", "2015 akum")
      output[[plotname]] <- renderFormattable({formattable(visitsbranch, list(
        "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
        "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
      ))})
    })
  }
  
  ### LOAN & RENEWALS ###
  
  # loan initial pivot #
  loan <- sqlloan %>%
    mutate(loan2017 = ifelse(year == "2017", count, 0), loan2016 = ifelse(year == "2016", count, 0), loan2015 = ifelse(year == "2015", count, 0)) %>%
    select(month, library, loan2017, loan2016, loan2015) %>%
    group_by(month, library) %>%
    summarise(loan2017 = sum(loan2017), loan2016 = sum(loan2016), loan2015 = sum(loan2015))  
  
  # loan plot #
  loanplot <- loan %>%
    group_by(library) %>%
    summarise(loan2017 = sum(loan2017), loan2016 = sum(loan2016), loan2015 = sum(loan2015)) %>%
    select(library, loan2017, loan2016, loan2015)
  
  output$loanplot <- renderPlotly({
    plot_ly(loanplot, x = loanplot$library, y = loanplot$loan2015, type = 'bar', name = '2015', text = text, marker = list(color = 'gold')) %>%
    add_trace(y = loanplot$loan2016, name = '2016', marker = list(color = 'rgb(63,168,123)')) %>% #mediumseargb(63,168,123) 
    add_trace(y = loanplot$loan2017, name = '2017', marker = list(color = 'rgb(72,35,115)')) %>% #darkslateblue
    layout(yaxis = list(title = 'Antal'), barmode = 'group')
  })
  
  # loan all libraries #
  loanall <- loan %>%
    group_by(month) %>%
    summarise(loan2017 = sum(loan2017), loan2016 = sum(loan2016), loan2015 = sum(loan2015)) %>%
    mutate(diff1716 = percent((loan2017-loan2016)/loan2016), diff1615 = percent((loan2016-loan2015)/loan2015), cumsum2017 = cumsum(loan2017), cumsum2016 = cumsum(loan2016), cumsum2015 = cumsum(loan2015), cumkum1716 = percent((cumsum(loan2017)-cumsum(loan2016))/cumsum(loan2016))) %>%
    arrange(month) %>%
    select(month, loan2017,cumsum2017,diff1716,loan2016,cumsum2016,diff1615,loan2015,cumsum2015,cumkum1716)
  
  colnames(loanall) <- c("Måned", "2017", "2017 akum","17><16", "2016", "2016 akum", "16><15", "2015", "2015 akum","17><16 akum")
  
  output$loantableall <- renderFormattable({formattable(loanall, list(
    "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
    "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
  )
  )})
  
  # loan individual libraries #
  loanlibrary <- sqlloan %>% distinct(library)
  loanbranch <- loan
  
  foreach(i = loanlibrary$library) %do% {
    local ({
      my_i <- i
      plotname <- paste0("loantable",substr(my_i, 1, 3))
      loanbranch <- loanbranch %>% 
        filter(library == my_i) %>%
        group_by(month) %>%
        summarize(loan2017, loan2016, loan2015) %>%
        mutate (diff1716 = percent((loan2017-loan2016)/loan2016), diff1615 = percent((loan2016-loan2015)/loan2015), cumsum2017 = cumsum(loan2017), cumsum2016 = cumsum(loan2016), cumsum2015 = cumsum(loan2015)) %>%
        select(month, loan2017, cumsum2017, diff1716, loan2016, cumsum2016, diff1615, loan2015, cumsum2015)
      colnames(loanbranch) <- c("Måned", "2017", "2017 akum","17><16", "2016", "2016 akum", "16><15", "2015", "2015 akum")
      output[[plotname]] <- renderFormattable({formattable(loanbranch, list(
        "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
        "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
      ))})
    })
  }
  
  
  ### ACQUISITION ###
  
  acquisition2017 <- acquisition %>%
    filter(dateordered > '2017-01-01 00:00:00')
    
  is.not.null <- function(x) ! is.null(x) ## defines a is.not function
  
  sum2017 <- acquisition2017 %>%
    mutate(number = ifelse (is.not.null(orderid) , 1, 0), ordered_nbr = ifelse (is.null(dateinvoiced) , 1, 0), ordered = ifelse (is.null(dateinvoiced) , materialprice, 2), payd = ifelse (is.not.null(dateinvoiced) , materialprice, 0)) %>%
    select(kind, number, materialprice, ordered_nbr, ordered, payd) %>%
    group_by(kind) %>%
    summarize(format(sum(number), digits=1), sum(materialprice),sum(ordered_nbr), sum(ordered), sum(payd))
  colnames(sum2017) <- c("Materialetype","Antal", "Forbrug kr.","Antal bestilte","I bestilling","Faktureret")
  
  output$acquisitionsumtable <- renderTable(sum2017) 
    
  ### E-RESSOURCES ### 
  
  ### WEBSITES ###
  
  r <- GET("https://ws.webtrends.com/v3/Reporting/profiles/77605/reports/VSlaqtDP0P6/?totals=all&start_period=current_year-2&end_period=current_year&period_type=indv&format=json", authenticate(webtrendsusername, webtrendspassword, type = "basic"))
  json <- content(r, "text")
  json2 <- content(r, "text")
  
  jsontable <- json %>%
    enter_object("data") %>%
    gather_array %>%
    spread_values(Årstal = jstring("start_date")) %>%
    enter_object("measures") %>%
    spread_values(Besøg = jstring("ActiveVisits")) %>%
    spread_values(Sidevisninger = jstring("PageViews")) %>%
    spread_values(Klik = jstring("Clickthroughs")) %>%
    spread_values(Besøgende = jstring("DailyVisitors")) %>%
    spread_values(Ugentligebesøgende = jstring("WeeklyVisitors")) %>%
    spread_values(Månedligebesøgende = jstring("MonthlyVisitors")) %>%
    spread_values(Kvartalsvisbesøgende = jstring("QuarterlyVisitors")) %>%
    spread_values(Årligebesøgende = jstring("YearlyVisitors")) %>%
    spread_values(Enkeltsidebesøgende = jstring("SinglePageViewVisits")) %>%
    spread_values(Forsidevisning = jstring("EntryPageVisits")) %>%
    spread_values(Afvisningsrate = jstring("BounceRate")) %>%
    select(Årstal, Besøg, Sidevisninger, Besøgende)
  output$table <- renderTable(jsontable, width = "100%")
  
  jsontable2 <- json2 %>%
    enter_object("data") %>%
    gather_array %>%
    spread_values(start_date = jstring("start_date")) %>%
    enter_object("SubRows") %>%
    gather_array 
    #gather_array %>%
    #spread_values(y = jstring("measures","ActiveVisits")) 
  output$table2 <- renderTable(jsontable2)
  
  ### EVENTS ###
  
  output$eventtable <- renderFormattable({formattable(events, list(
  )
  )})
  
  #eventsplot <- events %>%
    #mutate(year = format(dato, "%y"), 
      #e2013 = ifelse ((year == "13") , 1, 0),
      #e2014 = ifelse ((year == "14") , 1, 0),
      #e2015 = ifelse ((year == "15") , 1, 0),
      #e2016 = ifelse ((year == "16") , 1, 0),
      #e2017 = ifelse ((year == "17") , 1, 0)) %>%
    #select (year,e2013,e2014,e2015)
    #group_by(year)
    #group_by(toString(year)) %>%
    #summarise(ec2017 = count(e2017), ec2016 = count(e2016), ec2015 = count(e2015)) %>%
    #select(year,ec2017, ec2016)
    
    #       , e2017 = ifelse(year == "17", count, 0), e2016 = ifelse(year == "16", count, 0), e2015 = ifelse(year == "15", count, 0)) #%>%
    #group_by(year) %>%
    #summarise(v2017 = count(v2017), v2016 = count(v2016), v2015 = count(v2015)) %>%
    #select(year,v2017,v2016,v2015)
  
  #output$event2table <- renderFormattable({formattable(eventsplot, list(  ))})
  
  #output$eventsplot <- renderPlotly({
  #  plot_ly(eventsplot, x = eventsplot$year, y = eventsplot$v2015, type = 'bar', name = '2015', text = text, marker = list(color = 'gold')) %>%
  #    add_trace(y = eventsplot$v2016, name = '2016', marker = list(color = 'rgb(63,168,123)')) %>%  
  #    add_trace(y = eventsplot$v2017, name = '2017', marker = list(color = 'rgb(72,35,115)')) %>% 
  #    layout(yaxis = list(title = 'Antal'), barmode = 'group')
  #})
  
  
  ### Web ###
  
  # pageviews
  query1.init <- Init(start.date = "2015-01-01",
                     end.date = "2017-12-31",
                     metrics = c("ga:pageviews"),
                     dimensions =c("ga:year, ga:month"),
                     max.results = 100,
                     table.id = "ga:6064370")
  query1 <- QueryBuilder(query1.init)
  data1 <- GetReportData(query1, token) %>%
    mutate(p2017 = ifelse(year == "2017", pageviews, 0), p2016 = ifelse(year == "2016", pageviews, 0), p2015 = ifelse(year == "2015", pageviews, 0)) %>%
    arrange(year,month) %>%
    select(year, month, p2015, p2016, p2017)
  
  output$plot1 <- renderPlotly({
    plot_ly(data1, x = ~month, y = ~p2015, type = "scatter", mode = 'lines', name = '2015', line = list(shape = "spline")) %>%
      add_trace(y = ~p2016, name = '2016', mode = 'lines', connectgaps = TRUE) %>%
      add_trace(y = ~p2017, name = '2017', mode = 'lines') %>%
      layout(showlegend = T)  
  })
  output$tableplot1 <- renderDataTable(data1)
  
  # device
  query2.init <- Init(start.date = "2017-01-01",
                      end.date = "2017-12-31",
                      metrics = c("ga:sessions"),
                      dimensions =c("ga:deviceCategory"),
                      table.id = "ga:6064370")
  query2 <- QueryBuilder(query2.init)
  data2 <- GetReportData(query2, token)
  
  output$plot2 <- renderPlotly({
    plot_ly(data2, labels = ~deviceCategory, values = ~sessions) %>%
    add_pie(hole = 0.6) %>%
    layout(showlegend = T,
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # top10 pages 2017
  query3.init <- Init(start.date = "2017-01-01",
                      end.date = "2017-12-31",
                      metrics = c("ga:pageviews"),
                      dimensions =c("ga:pagePath"),
                      sort = c("-ga:pageviews"),
                      max.results = 10,
                      table.id = "ga:6064370")
  query3 <- QueryBuilder(query3.init)
  data3 <- GetReportData(query3, token)
  
  output$tableplot3 <- renderTable(data3)
  
  
  
  
  
})