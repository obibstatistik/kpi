source("global.R")
source("modules.R")

shinyServer(function(input, output) {

  display <- reactive({input$display})
  num <- callModule(sliderText, "module", display)
  output$value <- renderText({paste0("slider1+5: ", num())})
  
  source("~/.postpass")
  
  ### DB QUERIES ###
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  
  eventsmaalgruppe <- dbGetQuery(con, "select extract(year from dato) as year, maalgruppe, count(*) from datamart.arrangementer group by maalgruppe, year")
  
  eventsyear <- dbGetQuery(con, "select extract(year from dato) as year, count(*) from datamart.arrangementer where extract(year from dato) > 2012 group by year order by year")
  
  eventsmonth <- dbGetQuery(con, "select extract(month from dato) as month, count(*) from datamart.arrangementer where extract(year from dato) > 2012 group by month order by month")
  eventsdeltagere <- dbGetQuery(con, "select sum(deltagere), extract(year from dato) as year from datamart.arrangementer where extract(year from dato) > 2012 group by year order by year")
  eventsparticipantmonth <- dbGetQuery(con, "select sum(deltagere), extract(month from dato) as month from datamart.arrangementer where extract(year from dato) > 2012 group by month order by month")
  eventssted <- dbGetQuery(con, "select lokation, extract(year from dato) as year, count(*) from datamart.arrangementer group by lokation, year")
  eventskategori <- dbGetQuery(con, "select kategori, extract(year from dato) as year, count(*) from datamart.arrangementer group by kategori, year")
  eventsratio <- dbGetQuery(con, "select titel, arrangementstype, deltagere, forberedelsestid from datamart.arrangementer")
  
  visitors <- dbGetQuery(con, "SELECT * FROM public.people_counter")
  
  visits <- dbGetQuery(con, "SELECT * FROM public.people_counter")
  #visitsoverview <- dbGetQuery(con, "SELECT extract(year from date)::text as year, sum(count) FROM public.people_counter WHERE extract(year from date) in ('2015','2016','2017') group by year order by year")
  visitscompare <- dbGetQuery(con, "SELECT extract(year from date) as year, location, sum(count)n FROM public.people_counter WHERE extract(year from date) in ('2016','2017')  group by year, location")
  
  meetingrooms <- dbGetQuery(con, "SELECT * FROM datamart.meetingrooms")
  bhus_events <- dbGetQuery(con, "SELECT * FROM datamart.bhus_events")
  
  ga_pageviews <- dbGetQuery(con, "SELECT * FROM datamart.ga_pageviews where pageviews > 0")
  ga_device <- dbGetQuery(con, "select device, sum(users) as users from datamart.ga_device group by device")
  ga_top10 <- dbGetQuery(con, "SELECT title, pageviews FROM datamart.ga_top10 order by pageviews desc limit 11 offset 1")
  sqlloan <- dbGetQuery(con, "SELECT * FROM datamart.kpi_loan")
  events <- dbGetQuery(con, "SELECT * FROM datamart.arrangementer")
  acquisition <- dbGetQuery(con, "SELECT * FROM public.imusic")
  sites <- dbGetQuery(con, "SELECT * FROM datamart.sites")
  ereolentype <- dbGetQuery(con, "SELECT type, count(type) FROM public.ereolen group by type")
  ereolenhist <- dbGetQuery(con, "select to_char(dato, 'iyyy-iw') as date, count(type = 'Lydbog') as lydbog, count(type = 'E-bog') as ebog from public.ereolen group by date;")
  ereolenalder <- dbGetQuery(con, "select extract(year from date_trunc('year',age(birth))) as alder, count(extract(year from date_trunc('year',age(birth)))) as antal, (case when mod((substring(laanernummer from 10 for 1))::integer,2) = 1 then 'mand' else 'kvinde' end) as sex from public.ereolen join public.patron on public.patron.patronno = laanernummer group by alder, sex;")
  loaners <- dbGetQuery(con, "select sum(loaner_stat_count)::text as Antal, case 
    when Name like '0%'::text OR Name like '0%'::text OR Name like '1%'::text OR Name like '2%'::text OR Name like '3%'::text OR Name like '4%'::text OR Name like '5%'::text OR Name like '6%'::text OR Name like '7%'::text then 'Skole'::text
    when Name = 'Voksen, Odense kommune' then 'Voksen, Odense kommune'
    when Name = 'Barn, Odense kommune' then 'Barn, Odense kommune'
    when Name = 'Voksen, udenfor Odense Kommune' then 'Voksen, udenfor Odense kommune'
    when Name = 'Barn, udenfor Odense Kommune' then 'Barn, udenfor Odense kommune'  
    else 'Andre' end as Kategori  
    from cicero.aktive_laanere group by Kategori")
  loaners <- dbGetQuery(con, "select case 
    when cicero.aktive_laanere.Name like '0%'::text OR cicero.aktive_laanere.Name like '0%'::text 
    OR cicero.aktive_laanere.Name like '1%'::text OR cicero.aktive_laanere.Name like '2%'::text 
    OR cicero.aktive_laanere.Name like '3%'::text OR cicero.aktive_laanere.Name like '4%'::text 
    OR cicero.aktive_laanere.Name like '5%'::text OR cicero.aktive_laanere.Name like '6%'::text 
    OR cicero.aktive_laanere.Name like '7%'::text then 'Skole'::text
    when cicero.aktive_laanere.Name = 'Voksen, Odense kommune' then 'Voksen, Odense kommune'
    when cicero.aktive_laanere.Name = 'Barn, Odense kommune' then 'Barn, Odense kommune'
    when cicero.aktive_laanere.Name = 'Voksen, udenfor Odense Kommune' then 'Voksen, udenfor Odense kommune'
    when cicero.aktive_laanere.Name = 'Barn, udenfor Odense Kommune' then 'Barn, udenfor Odense kommune'  
    else 'Andre' end as Kategori, 
    sum(cicero.aktive_laanere.loaner_stat_count)::text as Aktive, 
    (sum(cicero.inaktive_laanere.loaner_stat_count)-sum(cicero.aktive_laanere.loaner_stat_count))::text as Inaktive, 
    (sum(cicero.inaktive_laanere.loaner_stat_count))::text as Alle
    from cicero.aktive_laanere 
    join cicero.inaktive_laanere on cicero.aktive_laanere.Name = cicero.inaktive_laanere.Name
    group by kategori")
  datasources <- dbGetQuery(con, "SELECT * FROM dokumentation.datakilder")
  
  dbDisconnect(con)
  
  ### COLORS ###
  
  colors <- c('rgb(70,140,140)', 'rgb(174,176,81)', 'rgb(59,54,74)', 'rgb(192,57,83)', 'rgb(29,114,170)', 'rgb(0,0,0)')
  color1 = c('rgb(70,140,140)')
  color2 = c('rgb(174,176,81)')
  color3 = c('rgb(59,54,74)')
  color4 = c('rgb(192,57,83)')
  color5 = c('rgb(29,114,170)')
  color6 = c('rgb(0,0,0)')
  
  ### DATES ###
  
  year <- as.integer(format(Sys.Date(), "%Y"))
  month <- as.integer(format(Sys.Date(), "%M"))
  day <- as.integer(format(Sys.Date(), "%Y"))
  
  ### LOCATIONS ###
  
  
  
  ### EVENTS ### 
  
  # arrangementer pr aar #
  output$eventsyearplot <- renderPlotly({
    plot_ly(eventsyear, x = eventsyear$year, y = eventsyear$count, type = 'bar', text = text, marker = list(color = color1)) 
  })
  # arrangementer pr maaned #
  output$eventsmonthplot <- renderPlotly({
    plot_ly(eventsmonth, x = factor(month.abb[eventsmonth$month],levels=month.abb), y = eventsmonth$count, type = 'bar', text = text, marker = list(color = color1)) 
  })
  
  # deltagere pr aar #
  output$eventsparticipantyearplot <- renderPlotly({
    plot_ly(eventsdeltagere, x = eventsdeltagere$year, y = eventsdeltagere$sum, type = 'bar', text = text, marker = list(color = color1)) 
  })
  
  # deltagere pr maaned #
  output$eventsparticipantmonthplot <- renderPlotly({
    plot_ly(eventsparticipantmonth, x = factor(month.abb[eventsparticipantmonth$month],levels=month.abb), y = eventsparticipantmonth$sum, type = 'bar', text = text, marker = list(color = color1)) 
  })
  
  # målgruppe #
  output$eventsmaalgruppeplot <- renderPlotly({
    if (input$year != "Alle") {eventsmaalgruppe <- eventsmaalgruppe %>% filter(year == input$year)}
    if (input$year == "Alle") {eventsmaalgruppe <- eventsmaalgruppe %>% filter(year %in% c("2013","2014","2015","2016","2017"))}
    plot_ly(eventsmaalgruppe, labels = ~maalgruppe, values = ~count, marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.0) %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })

  # sted #
  eventssted <- eventssted %>%
    mutate(
      sted = case_when(
        eventssted$lokation == "Tarup bibliotek" ~ "Tarup Bibliotek",
        eventssted$lokation == "Dalum bibliotek" ~ "Dalum Bibliotek",
        eventssted$lokation == "Hovedbiblioteket - Voksen" ~ "Hovedbiblioteket",
        eventssted$lokation == "Hovedbiblioteket - Børn" ~ "Hovedbiblioteket",
        eventssted$lokation == "Hovedbiblioteket - Opsøgende" ~ "Hovedbiblioteket",
        eventssted$lokation == "Holluf Pile bibliotek" ~ "Holluf Pile Bibliotek",
        eventssted$lokation == "Korup bibliotek" ~ "Korup Bibliotek",
        eventssted$lokation == "Højby bibliotek" ~ "Højby Bibliotek",
        eventssted$lokation == "Bolbro bibliotek" ~ "Bolbro Bibliotek",
        eventssted$lokation == "Vollsmose bibliotek" ~ "Vollsmose Bibliotek",
        eventssted$lokation == "Musikbiblioteket" ~ "Musikbiblioteket",
        eventssted$lokation == "lokalhistorisk" ~ "Lokalhistorisk",
        eventssted$lokation == "Næsby bibliotek" ~ "Næsby Bibliotek",
        eventssted$lokation == "Andet..." ~ "Andet"
      ) 
    ) 

  output$eventsstedplot <- renderPlotly({
    if (input$year != "Alle") {eventssted <- eventssted %>% filter(year == input$year)}
    plot_ly(eventssted, x = eventssted$sted, y = eventssted$count, type = 'bar', text = text, marker = list(color = color1)) %>%
      layout(margin = list(b = 125), xaxis = list(title = ""), yaxis = list(title =""))
  })
  
  # kategori #
  output$eventskategoriplot <- renderPlotly({
    if (input$year != "Alle") {eventskategori <- eventskategori %>% filter(year == input$year)}
    plot_ly(eventskategori, x = eventskategori$kategori, y = eventskategori$count, type = 'bar', text = text, marker = list(color = color1)) 
  })
  
  # ratio #
  output$eventsratioplot <- renderPlotly({
    if (input$year != "Alle") {eventskategori <- eventskategori %>% filter(year == input$year)}
    plot_ly(eventsratio, x = eventsratio$deltagere, y = eventsratio$forberedelsestid, text = eventsratio$titel, color = eventsratio$arrangementstype) %>%
      layout(xaxis = list(title = "deltagere", range = c(0, 500)), yaxis = list(title ="forberedelsestid"))
  })
  
  ### FYSISKE RUM ###
  
  output$visitorsfrom <- renderUI({
    selectInput("visitors_fromyear", "Fra:", c("2018" = "2018", "2017" = "2017", "2016" = "2016", "2015" = "2014", "2014" = "2013"), year-1)
  })
  output$visitorsto <- renderUI({
    selectInput("visitors_toyear", "Til:", c("2018" = "2018", "2017" = "2017", "2016" = "2016", "2015" = "2014", "2014" = "2013"), year)
  })
  
  # visitors table #
  output$visitors_table <- renderFormattable({
    visitors <- visitors %>%
      select(date, count, location) %>%
      mutate(month = month(date)) %>%
      mutate(year = year(date)) %>%
      filter(year == input$visitors_fromyear | year == input$visitors_toyear ) %>%
      filter(if(input$visitorslibrary != 'all')  (location == input$visitorslibrary) else TRUE) %>%
      select(count, month, year) %>%
      group_by(month, year) %>%
      summarise(sum = sum(count)) %>%
      spread(key = year, value = sum) %>%
      ungroup(.self) %>%
      mutate(akku1 = cumsum(.[[2]]), akku2 = cumsum(.[[3]]), mdr = percent(.[[3]]-.[[2]])/.[[2]]) %>%
      mutate(akk = percent((akku2-akku1)/akku1)) %>%
      select(c(1,2,4,3,5,6,7)) %>%
      rename(Måned = month)
    formattable(visitors, list(
      mdr = formatter("span", style = x ~ style(color = ifelse(x < 0 , color4, color1)), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
      akk = formatter("span", style = x ~ style(color = ifelse(x < 0 , color4, color1)), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
    ))
  })
  
  # visitors total plot#
  output$visitsplotall <- renderPlotly({
    visitsoverview <- visitors %>%
      select(date, count, location) %>%
      mutate(year = year(date)) %>%
      filter(year != as.integer(format(Sys.Date(), "%Y"))) %>%
      filter(if(input$mainlibrary == 'Uden Hovedbiblioteket')  (location != 'hb') else TRUE) %>%
      group_by(year) %>%
      summarise(sum = sum(count)) 
    plot_ly(visitsoverview, x = visitsoverview$year, y = visitsoverview$sum, type = 'bar') %>%
      layout(yaxis = list(title = 'Antal'), xaxis = list(title = 'År', dtick = 1, autotick = FALSE))
  })
  
  
  # visitors branch plot #

  output$visitsplotindividual <- renderPlotly({
    visitorsbranch <- visitors %>%
      select(date, count, location) %>%
      mutate(year = year(date)) %>%
      group_by(location, year) %>%
      summarise(sum = sum(count)) %>%
      spread(key = year, value = sum) %>%
      ungroup(.self) 
    plot_ly(visitorsbranch, x = ~location, y = ~`2014`, type = 'bar', name = '2014', text = text) %>%
      add_trace(y = ~`2015`, name = '2015') %>%
      add_trace(y = ~`2016`, name = '2016') %>%
      add_trace(y = ~`2017`, name = '2017') %>%
      add_trace(y = ~`2018`, name = '2018') %>%
      layout(yaxis = list(title = 'Antal'), barmode = 'group')
  })
  
  visitorsbranch2 <- visitors %>%
    select(date, count, location) %>%
    mutate(year = year(date)) %>%  
    group_by(location, year) %>%
    summarise(sum = sum(count)) %>%
    spread(key = year, value = sum) 
  output$visitorstest <- renderFormattable({formattable(visitorsbranch2)})
  
  #meetingrooms
  
  meetingrooms <- meetingrooms %>%
    mutate(
      sted = case_when(
        meetingrooms$roomnumber == "lok11_borghus@odense.dk" ~ "Lokale 1.1",
        meetingrooms$roomnumber == "lok12_borghus@odense.dk" ~ "Lokale 1.2",
        meetingrooms$roomnumber == "lok21_borghus@odense.dk" ~ "Lokale 2.1",
        meetingrooms$roomnumber == "lok22_borghus@odense.dk" ~ "Lokale 2.2",
        meetingrooms$roomnumber == "lok31_borghus@odense.dk" ~ "Lokale 3.1",
        meetingrooms$roomnumber == "lok32_borghus@odense.dk" ~ "Lokale 3.2",
        meetingrooms$roomnumber == "lok33_borghus@odense.dk" ~ "Lokale 3.3",
        meetingrooms$roomnumber == "lok34_borghus@odense.dk" ~ "Lokale 3.4",
        meetingrooms$roomnumber == "lok35_borghus@odense.dk" ~ "Lokale 3.5",
        meetingrooms$roomnumber == "lok36_borghus@odense.dk" ~ "Lokale 3.6"
      ) 
    )
  
  output$tablemeetingrooms_overview <- renderTable(
   meetingrooms_overview <- meetingrooms %>%
     filter(startdate > input$dateRangeMeetingrooms[1] & startdate < input$dateRangeMeetingrooms[2]) %>%
     mutate(tid = 	as.integer((enddate - startdate))) %>%
     select(sted, tid) %>%
     group_by(sted) %>%
     summarise(count = n(), mean = mean(tid), median = median(tid), sum = sum(tid)/60 ) %>%
     rename(Lokalenummer = sted, Antal = count, Gennemsnit =	mean, Middelværdi =	median, Total =	sum )  
  )
  
  output$meetingrooms_agendascreen_plot <- renderPlotly({
    meetingrooms_agendascreen <- meetingrooms %>%
      filter(startdate > input$dateRangeMeetingrooms[1] & startdate < input$dateRangeMeetingrooms[2]) %>%
      select(show_on_screen) %>%
      group_by(show_on_screen) %>%
      summarise(count = n())
    plot_ly(meetingrooms_agendascreen, labels = c("Ikke vist på skærm","Vist på skærm"), values = ~count, marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$table_meetingrooms_booker <- renderTable(
    meetingrooms_booker <- meetingrooms %>%
      filter(startdate > input$dateRangeMeetingrooms[1] & startdate < input$dateRangeMeetingrooms[2]) %>%
      select(forfatter_mail) %>%
      group_by(forfatter_mail) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(10) %>%
      rename(Booker = forfatter_mail, Antal = count)
  )
  
  output$tablemeetingrooms_timeslots <- renderFormattable({
    meetingrooms_timeslots <- meetingrooms %>%
      filter(startdate > input$dateRangeMeetingrooms[1] & startdate < input$dateRangeMeetingrooms[2]) %>%
      select(sted, startdate ) %>%
      mutate(Tidspunkt = hour(format(as.POSIXct(startdate)))) %>%
      group_by(sted, Tidspunkt) %>%
      summarise(count = n()) %>%
      spread(key = sted, value = count) %>%
      replace(., is.na(.), "0")
    formattable(meetingrooms_timeslots, list('Lokale 1.1' = color_tile("grey", '#468c8c')))}
  )

  #bhus_events
  
  output$tablebhus_events_overview <- renderTable(
    bhus_events_overview <- bhus_events %>%
      filter(startdate > input$dateRangeBhus_events[1] & startdate < input$dateRangeBhus_events[2]) %>%
      mutate(tid = 	as.integer((slut - startdate))) %>%
      select(location, tid) %>%
      group_by(location) %>%
      summarise(count = n(), mean = mean(tid), median = median(tid), sum = sum(tid)/60 ) %>%
      rename(Lokation = location, Antal = count, Gennemsnit =	mean, Middelværdi =	median, Total =	sum )  
  )
  
  output$bhus_events_agendascreen_plot <- renderPlotly({
    bhus_events_agendascreen <- bhus_events %>%
      filter(startdate > input$dateRangeBhus_events[1] & startdate < input$dateRangeBhus_events[2]) %>%
      select(show_on_screen) %>%
      group_by(show_on_screen) %>%
      summarise(count = n())
    plot_ly(bhus_events_agendascreen, labels = c("Ikke vist på skærm","Vist på skærm"), values = ~count, marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$table_bhus_events_booker <- renderTable(
    bhus_events_booker <- meetingrooms %>%
      filter(startdate > input$dateRangeBhus_events[1] & startdate < input$dateRangeBhus_events[2]) %>%
      select(forfatter_mail) %>%
      group_by(forfatter_mail) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(10) %>%
      rename(Booker = forfatter_mail, Antal = count)
  )
  
  output$tablebhus_events <- renderFormattable({
    bhus_events_timeslots <- bhus_events %>%
      filter(startdate > input$dateRangeBhus_events[1] & startdate < input$dateRangeBhus_events[2]) %>%
      select(location, startdate ) %>%
      mutate(Tidspunkt = hour(format(as.POSIXct(startdate)))) %>%
      group_by(location, Tidspunkt) %>%
      summarise(count = n()) %>%
      spread(key = location, value = count) %>%
      replace(., is.na(.), "0")
    formattable(bhus_events_timeslots, list('Lokale 1.1' = color_tile("grey", '#468c8c')))}
  )
  
  ### Web ###
  
  # sites
  
  sites <- sites %>% select("Organisation" = titel, "URL" = url)
  output$tablesites <- renderTable(sites)
  
  # pageviews
  
  ga_pageviews <- ga_pageviews %>%
    mutate(pv2018 = ifelse(aar == "2018", pageviews, 0), pv2017 = ifelse(aar == "2017", pageviews, 0), pv2016 = ifelse(aar == "2016", pageviews, 0), pv2015 = ifelse(aar == "2015", pageviews, 0)) %>%
    select(maaned,pv2015,pv2016,pv2017,pv2018) %>%
    group_by(maaned) %>%
    summarise(v2018 = sum(pv2018), v2017 = sum(pv2017), v2016 = sum(pv2016), v2015 = sum(pv2015))
  is.na(ga_pageviews) <- !ga_pageviews
  
  output$plot1 <- renderPlotly({
    plot_ly(ga_pageviews, x = ~maaned , y = ~v2015 , type = "bar", name = '2015', marker = list(color = color1)) %>%
      add_trace(y = ~v2016, name = '2016', marker = list(color = color2)) %>%
      add_trace(y = ~v2017, name = '2017', marker = list(color = color3)) %>%
      add_trace(y = ~v2018, name = '2018', marker = list(color = color4)) %>%
      layout(showlegend = T, xaxis = list(tickmode="linear", title = "Måned"), yaxis = list(title = "Antal"))  
  })
  
  # device
  
  output$ga_device_plot <- renderPlotly({
    plot_ly(ga_device, labels = ~device, values = ~users, marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # top10 pages 2017
  
  ga_top10 <- ga_top10 %>% 
    filter(title != "Adgang nægtet | Odense Bibliotekerne") %>%
    rename(Titel = title, Sidevisninger = pageviews )
  
  output$tableplot3 <- renderFormattable({formattable(ga_top10)})
  
  
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
    loanplot <- loanplot %>% filter(library %in% input$checkGroup)
    plot_ly(loanplot, x = loanplot$library, y = loanplot$loan2015, type = 'bar', name = '2015', text = text) %>%
    add_trace(y = loanplot$loan2016, name = '2016') %>%  
    add_trace(y = loanplot$loan2017, name = '2017') %>% 
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
    mutate(undervejs = ifelse (is.na(dateshipped), 1, 0), leveret = ifelse (is.na(dateshipped), 0, 1), number = ifelse (is.not.null(orderid), 1, 0) , disponeret = ifelse (is.na(dateinvoiced), materialprice, 0), payd = ifelse (is.na(dateinvoiced), 0, materialprice)) %>%
    select(kind, undervejs, leveret, number, disponeret, payd, materialprice) %>%
    group_by(kind) %>%
    summarize(format(sum(undervejs), digits=1), format(sum(leveret), digits=1), format(sum(number), digits=1), format(sum(disponeret), digits=2), format(sum(payd), digits=2), format(sum(materialprice), digits=2))
  colnames(sum2017) <- c("Materialetype", "Antal undervejs" , "Antal leveret", "Samlet bestilt", "Disponeret kr.", "Faktureret kr.", "Samlet Forbrug kr.")
  
  output$acquisitionsumtable <- renderTable(sum2017) 
  
  # 2017/2016 compare #
  
  acquisition2017prepared <- acquisition2017 %>%
    mutate (number = ifelse (prepared == TRUE, 1, 0)) %>%
    select (preparation, number) %>%
    group_by(preparation) %>%
    summarize(sum(number))
  colnames(acquisition2017prepared) <- c("preparation", "sum")
  acquisition2017prepared <- acquisition2017prepared %>%  filter (sum > 100)
  
  output$acquisitionpreparationtable <- renderTable(acquisition2017prepared) 
  
  output$acquisitionplotpreparation <- renderPlotly({
    plot_ly(acquisition2017prepared, labels = ~preparation, values = ~sum, type = 'pie', marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  

  ### E-RESSOURCES ### 
  
  ereolentype <- ereolentype
  output$ereolentable <- renderTable(ereolentype)
  
  output$ereolentypeplot <- renderPlotly({
    plot_ly(ereolentype, labels = ~type, values = ~count, marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  ereolenalderkvinde <- ereolenalder %>%
    filter(alder > 0) %>%
    filter(alder < 100) %>%
    filter(sex == 'kvinde')
  ereolenaldermand <- ereolenalder %>%
    filter(alder > 0) %>%
    filter(alder < 100) %>%
    filter(sex == 'mand')
  
  p1 <- plot_ly(ereolenalderkvinde, x = ~antal, y = ~alder, type = 'bar', orientation = 'h', name = 'kvinde', marker = list(color = color1)) %>%
    layout(yaxis = list(side = 'left', range = c(0, 100)), xaxis = list(range = c(0, 2000)))
  p2 <- plot_ly(ereolenaldermand, x = ~antal, y = ~alder, type = 'bar', orientation = 'h', name = 'mand', marker = list(color = color2)) %>%
    layout(yaxis = list(side = 'left', range = c(0, 100)), xaxis = list(range = c(0, 2000)))
  output$ereolenaldersubplot <- renderPlotly({subplot(p1, p2)})
  
  
  output$p <- renderPlotly({
    plot_ly(ereolenhist, x = ~date, y = ~lydbog, type = 'bar', name = 'Lydbog', marker = list(color = color1)) %>%
      add_trace(y = ~ebog, name = 'E-bog', marker = list(color = color2)) %>%
      layout(yaxis = list(title = 'antal udlån'), barmode = 'stack')
  })
  
  
  ### USERS ###
  
  colnames(loaners) <- c("Kategori", "Aktive", "Inaktive","Alle")
  output$tableloaners <- renderTable(loaners)
  
  ### STAFF ###
  
  # gender # 
  
  years <- c("2012", "2013", "2014", "2015", "2016")
  male <- c(38, 37, 35, 37, 38)
  female <- c(62, 63, 65, 63, 62)
  data <- data.frame(years, male, female)

  output$peopleplot <- renderPlotly({
    plot_ly(data, x = ~years, y = ~male, type = 'bar', name = 'Mænd', marker = list(color = color2)) %>%
      add_trace(y = ~female, name = 'Kvinder', marker = list(color = color3)) %>%
      layout(xaxis = list(title = 'Årstal'), yaxis = list(title = 'Procentfordeling'), barmode = 'stack')
  })
  
  # age # 
  
  years <- c("2012", "2013", "2014", "2015", "2016")
  first <- c(19,17,12,12,12)
  second <- c(18,21,27,27,26)
  third <- c(39,37,34,33,31)
  fourth <- c(60,56,54,56,56)
  data <- data.frame(years, first, second, third, fourth) %>%
    mutate(sum = (first + second + third + fourth)) %>%
    mutate(firstp = ((first/sum)*100), secondp = ((second/sum)*100), thirdp = ((third/sum)*100), fourthp = ((fourth/sum)*100))
  
  output$peopleplotage <- renderPlotly({
    plot_ly(data, x = ~years, y = ~firstp, type = 'bar', text = c(19,17,12,12,12), name = '25-34', marker = list(color = color1)) %>%
      add_trace(y = ~secondp, text = toString(second), name = '35-44', marker = list(color = color2)) %>%
      add_trace(y = ~thirdp, name = '45-54', marker = list(color = color3)) %>%
      add_trace(y = ~fourthp, name = '55+', marker = list(color = color4)) %>%
      layout(xaxis = list(title = 'Årstal'), yaxis = list(title = 'Procentfordeling'), barmode = 'stack')
  })
  
  # ageupper # 
  
  years <- c("2012", "2013", "2014", "2015", "2016")
  first <- c(36,35,33,27,28)
  second <- c(21,19,19,25,25)
  third <- c(3,2,2,4,3)
  data2 <- data.frame(years, first, second, third) %>%
    mutate(sum = (first + second + third)) %>%
    mutate(firstp = ((first/sum)*100), secondp = ((second/sum)*100), thirdp = ((third/sum)*100))
  
  output$peopleplotageupper <- renderPlotly({
    plot_ly(data2, x = ~years, y = ~firstp, type = 'bar', text = c(19,17,12,12,12), name = '55-59', marker = list(color = color1)) %>%
      add_trace(y = ~secondp, text = toString(second), name = '60-64', marker = list(color = color2)) %>%
      add_trace(y = ~thirdp, name = '65+', marker = list(color = color3)) %>%
      layout(xaxis = list(title = 'Årstal'), yaxis = list(title = 'Procentfordeling'), barmode = 'stack')
  })
  
  # ageupper2016 # 
  
  years <- c("2012", "2013", "2014", "2015", "2016")
  first <- c(36,35,33,27,28)
  second <- c(21,19,19,25,25)
  third <- c(3,2,2,4,3)
  data3 <- data.frame(years, first, second, third) %>%
    mutate(sum = (first + second + third)) %>%
    mutate(firstp = ((first/sum)*100), secondp = ((second/sum)*100), thirdp = ((third/sum)*100))
  
  output$peopleplotageupper <- renderPlotly({
    plot_ly(data3, x = ~years, y = ~firstp, type = 'bar', text = c(19,17,12,12,12), name = '55-59', marker = list(color = color1)) %>%
      add_trace(y = ~secondp, text = toString(second), name = '60-64', marker = list(color = color2)) %>%
      add_trace(y = ~thirdp, name = '65+', marker = list(color = color3)) %>%
      layout(xaxis = list(title = 'Årstal'), yaxis = list(title = 'Procentfordeling'), barmode = 'stack')
  })
  
  # ageupper2016 2 # 
  
  output$peopleplotageupper2 <- renderPlotly({
    plot_ly(
      x = c(55,56,57,58,59,60,61,62,63,64,65,66,67),
      y = c(9,2,4,3,10,8,8,5,2,2,2,0,1),
      name = "SF Zoo",
      type = "bar",
      marker = list(color = color1)
    )})
  
  # faggrupper # 
  
  years <- c("2012", "2013", "2014", "2015", "2016")
  first <- c(60,58,57,56,55)
  second <- c(65,60,59,60,58)
  third <- c(8,9,8,8,8)
  fourth <- c(3,4,3,4,4)
  data4 <- data.frame(years, first, second, third, fourth) %>%
    mutate(sum = (first + second + third + fourth)) %>%
    mutate(firstp = ((first/sum)*100), secondp = ((second/sum)*100), thirdp = ((third/sum)*100), fourthp = ((fourth/sum)*100))
  
  output$peopleplotfag <- renderPlotly({
    plot_ly(data4, x = ~years, y = ~firstp, type = 'bar', text = c(19,17,12,12,12), name = 'BF', marker = list(color = color1)) %>%
      add_trace(y = ~secondp, text = toString(second), name = 'HK', marker = list(color = color2)) %>%
      add_trace(y = ~thirdp, name = 'Øvrige AC', marker = list(color = color3)) %>%
      add_trace(y = ~fourthp, name = 'FOA_3F', marker = list(color = color4)) %>%
      layout(xaxis = list(title = 'Årstal'), yaxis = list(title = 'Procentfordeling'), barmode = 'stack')
  })
  
  # faggrupper gennemsnitsalder # 
  
  output$peopleplotfaggem <- renderPlotly({
    plot_ly(
      x = c("BF", "HK", "Øvrige ACere", "FOA/3F", "Tjenestemænd", "Overenskomstansatte", "Ledelse", "Samlet"),
      y = c(49.2, 45.6, 39.3, 58.5, 58.1, 46.3, 49.8, 50.1),
      name = "SF Zoo",
      type = "bar", marker = list(color = color1)
    )})
  
  # faggrupper gennemsnitsalder # 
  
  output$peopleplotfaggemall <- renderPlotly({
    plot_ly(
      x = c("BF", "HK", "Øvrige ACere", "FOA/3F", "Tjenestemænd", "Overenskomstansatte", "Ledelse", "Samlet"),
      y = c(49.2, 45.6, 39.3, 58.5, 58.1, 46.3, 49.8, 50.1), name = "2016", type = "bar", marker = list(color = color1)) %>%
      add_trace(y = c(50.6,51.8,40.1,57.5,59.1,45.6,48.8,50.7), name = '2015', marker = list(color = color2)) %>%
      add_trace(y = c(50.7,51.1,39.1,56.3,58.2,45.9,47.3,49.9), name = '2014', marker = list(color = color3)) %>%
      add_trace(y = c(50.4,50.2,39.4,56.8,57.5,45.2,47.4,49.7), name = '2013', marker = list(color = color4)) %>%
      add_trace(y = c(49.9,50.7,36.9,51.0,57.4,44.5,52.3,49.7), name = '2012', marker = list(color = color5))
    })
  
  # faggrupper gennemsnitsalder # 
  
  output$peopleplotfag2 <- renderPlotly({
    plot_ly(
      x = c("BF", "HK", "Øvrige ACere", "FOA/3F"),
      y = c(55,58,8,4), name = "2016", type = "bar", marker = list(color = color1)) %>%
      add_trace(y = c(56,60,8,4), name = '2015', marker = list(color = color2)) %>%
      add_trace(y = c(57,59,8,3), name = '2014', marker = list(color = color3)) %>%
      add_trace(y = c(58,60,9,4), name = '2013', marker = list(color = color4)) %>%
      add_trace(y = c(60,65,8,3), name = '2012', marker = list(color = color5))
  })
  
  
  ### Datasources ### 
  
  output$datasources_table <- renderTable(datasources) 
  
})