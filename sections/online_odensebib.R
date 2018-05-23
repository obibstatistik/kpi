source("global.R")
source("modules.R")
source("~/.postpass")

# UI

online_odensebibTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  
  tabItem(tabName = "odensebib",
          
          box(width = 12, solidHeader = TRUE, id="onlineheader2",
              h3("Odensebib.dk"),
              img(src='online.png', align = "right", height="46px")
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
                                            formattableOutput(ns("tableplot3"))
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
                          tabPanel("Indholdsgrupper", "")
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
    rename(Titel = title, Sidevisninger = pageviews )
  
  output$tableplot3 <- renderFormattable({formattable(ga_top10)})
  
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
  
  
  
}