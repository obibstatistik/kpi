source("global.R")

shinyServer(function(input, output) {
  
  ### DB QUERIES ###
  
  source("~/.postpass")
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  
  visits <- dbGetQuery(con, "SELECT * FROM public.people_counter")
  sqlloan <- dbGetQuery(con, "SELECT * FROM datamart.kpi_loan")
  
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
  
  
  
})