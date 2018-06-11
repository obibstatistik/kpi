source("global.R")
source("modules.R")
source("~/.postpass")

# UI

online_odensebibTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  
  tabItem(tabName = "odensebib",
          
          box(width = 12, solidHeader = TRUE, id="onlineheader2",
              h3("Odensebib.dk"),
              img(src='icons/online_negativ_45x45.png', align = "right", height="46px")
          ),
          
          fluidRow(          
            column(width = 12,

                   tabBox(width = 12,
                          id = "tabset1",
                          tabPanel("Generelt", 
                                   fluidRow(
                                     column(width = 12,
                                            h4("Sidevisninger"), 
                                            plotlyOutput(ns("plot1")),
                                            tableOutput(ns("ga_pageviewstable"))
                                     )),
                                   fluidRow(          
                                     column(width = 6,
                                            h4("Top 10 sider 2017"), 
                                            tableOutput(ns("tableplot3"))
                                     ),
                                     column(width = 6,
                                            h4("Enheder"),  
                                            plotlyOutput(ns("ga_device_plot"))
                                     )
                                   ),
                                   fluidRow(          
                                     column(width = 6,
                                            h4("Browser"), 
                                            plotlyOutput(ns("ga_browser_plot"))#,
                                            #tableOutput(ns("tablebrowser"))
                                     ),
                                     column(width = 6,
                                            h4("Sprog"), 
                                            plotlyOutput(ns("ga_language_plot"))#,
                                            #tableOutput(ns("tablelanguage"))
                                     )
                                   )
                                   
                          ),
                          tabPanel("Indholdsgrupper",
                                   p("Data fra 22-05-2018"),
                                   tableOutput(ns('content_groups'))        
                          ),
                          tabPanel("Fokus Netbiblioteket",
                                   h4("Antal ud links til e-ressourcer"),
                                   tableOutput(ns("table_events_category")),
                                   tableOutput(ns("table_events_action")),
                                   tableOutput(ns("table_events_clicks")),
                                   tableOutput(ns("table_events_clicks_na")),
                                   p("Eressourcer i søgeresulateter"),
                                   p("Netbiblioteket oversigt og undersider")
                          )
                   )
            ))
  )
}

# SERVER

online_odensebibTabPanel <- function(input, output, session) {
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  ga_pageviews <- dbGetQuery(con, "SELECT * FROM datamart.ga_pageviews where pageviews > 0")
  ga_device <- dbGetQuery(con, "select device, sum(users) as users from datamart.ga_device group by device")
  ga_top10 <- dbGetQuery(con, "SELECT title, pageviews FROM datamart.ga_top10 order by pageviews desc limit 11 offset 1")
  ga_browser <- dbGetQuery(con, "select browser, to_date(yearmonth::text, 'YYYYMM') as datoen, pageviews from datamart.ga_browser")
  ga_language <- dbGetQuery(con, "select language, to_date(yearmonth::text, 'YYYYMM') as datoen, pageviews from datamart.ga_language")
  ga_path <- dbGetQuery(con, "SELECT * FROM datamart.ga_path")
  ga_events <- dbGetQuery(con, "SELECT * FROM datamart.ga_events")
  sites <- dbGetQuery(con, "SELECT * FROM datamart.sites")
  dbDisconnect(con)
  
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
    mutate(pageviews = format(round(as.numeric(pageviews), 0), nsmall=0, big.mark=".")) %>%
    rename(Titel = title, Sidevisninger = pageviews )
  
  output$tableplot3 <- renderTable({formattable(ga_top10)}, rownames = TRUE)
  
  # browser
  
  ga_browser2 <- ga_browser %>%
    spread(browser, pageviews)
  ga_browser_table <- ga_browser %>%
    group_by(browser) %>%
    summarise(sum = sum(pageviews)) %>%
    arrange(desc(sum)) %>%
    head(10)
  
  output$ga_browser_plot <- renderPlotly({
    p <- plot_ly(ga_browser2, x = ~datoen, y = ~`Samsung Internet`, name = 'Samsung Internet', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~`Edge`, name = 'Edge', mode = 'lines') %>%
      add_trace(y = ~`Firefox`, name = 'Firefox', mode = 'lines') %>%
      add_trace(y = ~`Chrome`, name = 'Chrome', mode = 'lines') %>%
      add_trace(y = ~`Internet Explorer`, name = 'Internet Explorer', mode = 'lines') %>%
      add_trace(y = ~`Safari`, name = 'Safari', mode = 'lines') %>%
      add_trace(y = ~`Safari (in-app)`, name = 'Safari (in-app)', mode = 'lines') %>%
      add_trace(y = ~`Android Browser`, name = 'Android Browser ', mode = 'lines') %>%
      add_trace(y = ~`Mozilla Compatible Agent`, name = 'Mozilla Compatible Agent', mode = 'lines') %>%
      add_trace(y = ~`Android Webview`, name = 'Android Webview', mode = 'lines') %>%
      layout(xaxis = list(title = 'Dato'),yaxis = list (title = 'Sidevisninger'))
  })
  
  #output$tablebrowser <- renderTable(ga_browser_table)
  
  # language
  
  ga_language2 <- ga_language %>%
    spread(language, pageviews)
  ga_language_table <- ga_language %>%
    group_by(language) %>%
    summarise(sum = sum(pageviews)) %>%
    arrange(desc(sum)) %>%
    head(10)
  
  output$ga_language_plot <- renderPlotly({
    p <- plot_ly(ga_language2, x = ~datoen, y = ~`da-dk`, name = 'da-dk', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~`da`, name = 'da', mode = 'lines') %>%
      add_trace(y = ~`en-us`, name = 'en-us', mode = 'lines') %>%
      add_trace(y = ~`en-gb`, name = 'en-gb', mode = 'lines') %>%
      add_trace(y = ~`de-de`, name = 'de-de', mode = 'lines') %>%
      add_trace(y = ~`de`, name = 'de', mode = 'lines') %>%
      add_trace(y = ~`nb-no`, name = 'nb-no', mode = 'lines') %>%
      add_trace(y = ~`nb`, name = 'nb', mode = 'lines') %>%
      add_trace(y = ~`pl`, name = 'pl', mode = 'lines') %>%
      add_trace(y = ~`sv-se`, name = 'sv-se', mode = 'lines') %>%
      layout(xaxis = list(title = 'Dato'),yaxis = list (title = 'Sidevisninger'))
  })
  
  #output$tablelanguage <- renderTable(ga_language_table)
  
  #content groups
  
  ga_path <- ga_path %>%
    mutate(
      content_group = case_when(
        grepl("biblioteker/hovedbiblioteket", ga_path$path) ~ "Afdelingsside Hovedbiblioteket",
        grepl("biblioteker/bolbro", ga_path$path) ~ "Afdelingsside Bolbro",
        grepl("biblioteker/dalum", ga_path$path) ~ "Afdelingsside Dalum",
        grepl("biblioteker/slug-dalum", ga_path$path) ~ "Afdelingsside Dalum",
        grepl("biblioteker/node/73", ga_path$path) ~ "Afdelingsside Historiens Hus",
        grepl("biblioteker/slug-holluf", ga_path$path) ~ "Afdelingsside Holluf Pile",
        grepl("biblioteker/node/142", ga_path$path) ~ "Afdelingsside Højby",
        grepl("biblioteker/slug-korup", ga_path$path) ~ "Afdelingsside Korup",
        grepl("biblioteker/musikbiblioteket", ga_path$path) ~ "Afdelingsside Musikbiblioteket",
        grepl("biblioteker/slug-tarup", ga_path$path) ~ "Afdelingsside Tarup",
        grepl("biblioteker/slug-vollsmose", ga_path$path) ~ "Afdelingsside Vollsmose",
        grepl("^/biblioteker", ga_path$path) ~ "Biblioteker oversigtsiden",
        grepl("page/feedback", ga_path$path) ~ "Kontaktformularsiden",
        grepl("page/kontakt-personalet", ga_path$path) ~ "Personale oversigten",
        grepl("page/pas_paa_biblioteket", ga_path$path) ~ "Borgerservice siden",
        grepl("^/search/ting", ga_path$path) ~ "Søgning Brønd",
        grepl("^/search/node", ga_path$path) ~ "Søgning Hjemmeside",
        grepl("^/ting/collection/", ga_path$path) ~ "Visning Værker",
        grepl("^/ting/collection/", ga_path$path) ~ "Visning Værker",
        grepl("^/ting/object/", ga_path$path) ~ "Visning Objekt",
        grepl("^/ting/infomedia/", ga_path$path) ~ "Visning Infomedia",
        grepl("^/user/password", ga_path$path) ~ "Bruger Glemt kodeord",
        grepl("^/user/$", ga_path$path) ~ "Bruger Profil",
        grepl("^/user$", ga_path$path) ~ "Bruger Profil",
        grepl("^/payment/dibs", ga_path$path) ~ "Bruger Betaling Mellemværende",
        grepl("^/nyheder", ga_path$path) ~ "Nyheder Enkelte",
        grepl("^/news-category", ga_path$path) ~ "Nyheder Oversigtssider",
        grepl("^/arrangementer", ga_path$path) ~ "Arrangementer",
        grepl("^/403.html", ga_path$path) ~ "Adgang nægtet",
        grepl("^/404.html", ga_path$path) ~ "Ikke fundet",
        grepl("^/media/browser", ga_path$path) ~ "Medie browser!",
        grepl("^/ding_frontpage", ga_path$path) ~ "Bruger Log ind",
        grepl("^/page/netbiblioteket-oversigt", ga_path$path) ~ "Netbiblioteket Oversigt",
        grepl("^/$", ga_path$path) ~ "Forside"
        
      ) 
    ) %>%
    mutate(gruppe = ifelse(is.na(content_group), "Andet", content_group)) %>%
    select(gruppe, pageviews) %>%
    group_by(gruppe) %>%
    summarise(sum = sum(pageviews)) %>%
    mutate(sum = format(round(as.numeric(sum), 0), nsmall=0, big.mark=".")) %>%
    arrange(gruppe)
  
  output$content_groups <- renderTable(ga_path)
  
  #events - outlinks
  
  output$table_events_category <- renderTable (
    ga_events <- ga_events %>%
      select(eventcategory, yearmonth) %>%
      group_by(eventcategory, yearmonth) %>%
      summarise(sum = n()) %>%
      spread(eventcategory, sum)
  )
  
  output$table_events_action <- renderTable (
    ga_events <- ga_events %>%
      select(eventaction, yearmonth) %>%
      group_by(eventaction, yearmonth) %>%
      summarise(sum = n()) %>%
      spread(eventaction, sum)
  )

  events <- ga_events %>%
    filter(eventcategory == 'Outbound links') %>%
    select(eventlabel, yearmonth) %>%
    mutate(
      destination = case_when(
        grepl("^https://www.place2book.com", .$eventlabel) ~ "Place2book",
        grepl("^http://www.litteratursiden.dk", .$eventlabel) ~ "Litteratursiden",
        grepl("^https://www.litteratursiden.dk", .$eventlabel) ~ "Litteratursiden",
        grepl("^https://litteratursiden.dk/", .$eventlabel) ~ "Litteratursiden",
        grepl("^https://adm.biblioteksvagten.dk", .$eventlabel) ~ "Biblioteksvagten",
        grepl("^https://bibliotek.dk", .$eventlabel) ~ "Bibliotek.dk",
        grepl("^http://bibliotek.dk", .$eventlabel) ~ "Bibliotek.dk",
        grepl("^https://ereolen.dk/", .$eventlabel) ~ "Ereolen",
        grepl("^http://ereolen.dk/", .$eventlabel) ~ "Ereolen",
        grepl("^http://ereolenglobal", .$eventlabel) ~ "Ereolen",
        grepl("^https://biblioteksbaser.dk/linkme/", .$eventlabel) ~ "Proxy",
        grepl("^http://biblioteksbaser.dk/linkme/", .$eventlabel) ~ "Proxy",
        grepl("^http://bib461.bibbaser.dk/", .$eventlabel) ~ "Proxy",
        grepl("^http://ebookcentral.proquest.com/", .$eventlabel) ~ "Ebook Central",
        grepl("^http://danmarkshistorien.dk", .$eventlabel) ~ "Danmark.dk"
      )) %>%
    select(destination, eventlabel, yearmonth) %>%
    arrange(desc(destination), eventlabel) 
  
  output$table_events_clicks <- renderTable (
    events %>% 
      filter(!is.na(destination)) %>%
      group_by(destination, yearmonth) %>%
      summarize(sum = n()) %>%
      spread(destination, sum)
    , rownames = TRUE
  )
  
  output$table_events_clicks_na <- renderTable (
    events %>% filter(is.na(destination)), rownames = TRUE  
  )
  
  #
  areas <- c("eReolen","eReolen Go","eReolen Global","Fynsbibliografien","Historisk Atlas","Infomedia","Litteraturens Verden.dk","Matematikfessor")
  
}