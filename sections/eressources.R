source("global.R")
source("modules.R")
source("~/.postpass")

### COLORS ###

colors <- c('rgb(70,140,140)', 'rgb(174,176,81)', 'rgb(59,54,74)', 'rgb(192,57,83)', 'rgb(29,114,170)', 'rgb(225,123,81)', 'rgb(219,181,61)')
color1 = c('rgb(70,140,140)')
color2 = c('rgb(174,176,81)')
color3 = c('rgb(59,54,74)')
color4 = c('rgb(192,57,83)')
color5 = c('rgb(29,114,170)')
color6 = c('rgb(225,123,81)')
color7 = c('rgb(219,181,61)')

# UI

eressourcesTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "ebooks",
          
          box(width = 12, solidHeader = TRUE, id = "eressourcesheader",
              h3("Elektroniske Ressourcer"),
              img(src='icons/eressourcer_negativ_45x45.png', align = "right", height="46px")
          ),
          
          fluidRow(
            column(12,
                   box(width = 12,
                       column(width = 6,
                              h4("Antal udlån over tid"),
                              plotlyOutput(ns("p"))
                       ),
                       column(width = 6,
                              h4("Type"), 
                              plotlyOutput(ns("ereolentypeplot"))
                       )
                   ),
                   box(width = 12,
                       h4("Alder")#,
                       #column(width = 12,plotlyOutput("ereolenaldersubplot"))
                   )
            )
          )
  )
  # tabItem(tabName = "emovies",
  #         box(width = 12,
  #             h3("Film")
  #         )
  # ),
  # tabItem(tabName = "edatabases",
  #         box(width = 12,
  #             h3("Databaser")
  #         )
  # ),
  
  
}

# SERVER

eressourcesTabPanel <- function(input, output, session, data, tablename) {
  # module_data <- reactive({
  #   data %>% filter(name == tablename)
  # })

  ### DB QUERIES ###
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  ereolentype <- dbGetQuery(con, "SELECT type, count(type) FROM public.ereolen group by type")
  ereolenhist <- dbGetQuery(con, "select to_char(dato, 'iyyy-iw') as date, count(type = 'Lydbog') as lydbog, count(type = 'E-bog') as ebog from public.ereolen group by date;")
  dbDisconnect(con)
  
  ereolentype <- ereolentype
  output$ereolentable <- renderTable(ereolentype)
  
  output$ereolentypeplot <- renderPlotly({
    plot_ly(ereolentype, labels = ~type, values = ~count, marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # ereolenalderkvinde <- ereolenalder %>%
  #   filter(alder > 0) %>%
  #   filter(alder < 100) %>%
  #   filter(sex == 'kvinde')
  # ereolenaldermand <- ereolenalder %>%
  #   filter(alder > 0) %>%
  #   filter(alder < 100) %>%
  #   filter(sex == 'mand')
  # 
  # p1 <- plot_ly(ereolenalderkvinde, x = ~antal, y = ~alder, type = 'bar', orientation = 'h', name = 'kvinde', marker = list(color = color1)) %>%
  #   layout(yaxis = list(side = 'left', range = c(0, 100)), xaxis = list(range = c(0, 2000)))
  # p2 <- plot_ly(ereolenaldermand, x = ~antal, y = ~alder, type = 'bar', orientation = 'h', name = 'mand', marker = list(color = color2)) %>%
  #   layout(yaxis = list(side = 'left', range = c(0, 100)), xaxis = list(range = c(0, 2000)))
  # output$ereolenaldersubplot <- renderPlotly({subplot(p1, p2)})
  
  
  output$p <- renderPlotly({
    plot_ly(ereolenhist, x = ~date, y = ~lydbog, type = 'bar', name = 'Lydbog', marker = list(color = color1)) %>%
      add_trace(y = ~ebog, name = 'E-bog', marker = list(color = color2)) %>%
      layout(yaxis = list(title = 'antal udlån'), barmode = 'stack')
  })
  
}