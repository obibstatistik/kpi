library(plotly)
library(dplyr)
library(shiny)

fromdate <- paste0(Sys.Date()-8)
todate <- paste0(Sys.Date()-1)
select_stmt <- paste0("select * from smartcity.icmeter where substr(realtime::text,1,10) between '",fromdate,"' and '",todate,"';")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
sensors <- dbGetQuery(con, select_stmt)
dbDisconnect(con)


ui <- fluidPage(
  fluidRow(
    column(width=9,
           plotlyOutput("loanplot")
    )))

server <- function(input, output, session) {
  
  output$loanplot <- renderPlotly({
    y <- c('Dagens bookinger')
    
    x1 <- 230
    x2 <- 400
    x3 <- 310
    x4 <- 280
    x5 <- 220
    
    z1 <- ""
    z2 <- "10:15 - 14:30"
    z3 <- "14:30 - 19:00"
    z4 <- ""
    z5 <- "20:00 - 23:59"
    
    data <- data.frame(y, x1,x2,x3,x4,x5, z1,z2,z3,z4,z5)
    
    
    
    

    data2 <- sensors %>%
      select_("device_id","realtime","dato","hour",.dots = 'noise_avg') %>%
      filter(device_id == '20F2A02F') %>%
      group_by(device_id,dato,hour) %>%
      summarise_at(sensor,mean) %>%
      spread_("dato",sensor) %>%
      mutate(avg=rowMeans(.[-1:-2])) %>%       # .[-1,-2] betyder, at man tager alle columns bortset fra nr. 1 til nr. 2 (dvs. en range)
      select(avg,everything())                 # move averages to first column, so we can have an open-ended/dynamic number of measured data columns for later use
    
    # Tilføj måledagenes graflinjer:
    p2 <- plot_ly(data, x = ~hour, y = as.formula(paste0("~`", names(data)[4], "`")), type = 'scatter', line = list(color = 'rgb(210,210,210)', width = 3), mode = 'lines')
    for(colname in names(data)[-1:-4]){
      p2 <- p2 %>% 
        add_trace(y = as.formula(paste0("~`", colname, "`")), name = colname, line = list(color = 'rgb(220,220,220)', width = 3), mode = 'lines')   
    }
    # Tilføj gennemsnittets graflinje:
    p2 <- p2 %>% 
      add_trace(y = ~avg, line = list(color = 'rgb(0,0,0)', width = 4), mode = 'lines')
    # Switch/case to distinguish between sensors to determine which y-axis range to use
    yaxis_range <- function(sensor) {
      switch(sensor,
             temperature = c(16,30),
             co2 = c(300,1100),
             humidity = c(20,70),
             noise_avg = c(20,65))
    }
    # Switch/case to distinguish between sensors to determine which y-axis step to use
    yaxis_step <- function(sensor) {
      switch(sensor,
             temperature = 1,
             co2 = 100,
             humidity = 5,
             noise_avg = 10)
    }
    p2 %>% layout(autosize = T, title = '', xaxis = list(title = 'timer på dagen'), yaxis = list (range = yaxis_range(sensor), title = '', dtick = yaxis_step(sensor), ticksuffix = ticksuffix), showlegend = FALSE)
    
    
    
    
    
    
    
    top_labels <- c('', 'Materialeforum', 'Anonyme hattemagere', '', 'Blabla møde')
    
    p1 <- plot_ly(data, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
                 marker = list(color = 'green', line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
      add_trace(x = ~x2, marker = list(color = 'red')) %>%
      add_trace(x = ~x3, marker = list(color = 'red')) %>%
      add_trace(x = ~x4, marker = list(color = 'green')) %>%
      add_trace(x = ~x5, marker = list(color = 'red')) %>%
      
      layout(xaxis = list(title = "",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          zeroline = FALSE,
                          domain = c(0.15, 1)),
             yaxis = list(title = "",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          zeroline = FALSE),
             barmode = 'stack',
             paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
             margin = list(l = 120, r = 10, t = 240, b = 80),
             showlegend = FALSE) %>%
      
      # labeling the y-axis
      add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                      xanchor = 'right',
                      text = y,
                      font = list(family = 'Arial', size = 15, color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE, align = 'right') %>%
      
      # labeling the percentages of each bar (x_axis)
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 / 2, y = y,
                      text = paste(data[,"z1"]),
                      font = list(family = 'Arial', size = 15, color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 + x2 / 2, y = y,
                      text = paste(data[,"z2"]),
                      font = list(family = 'Arial', size = 15, color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 + x2 + x3 / 2, y = y,
                      text = paste(data[,"z3"]),
                      font = list(family = 'Arial', size = 15, color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 + x2 + x3 + x4 / 2, y = y,
                      text = paste(data[,"z4"]),
                      font = list(family = 'Arial', size = 15, color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 + x2 + x3 + x4 + x5 / 2, y = y,
                      text = paste(data[,"z5"]),
                      font = list(family = 'Arial', size = 15, color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      
      # labeling the scale (on the top)
      add_annotations(xref = 'x', yref = 'paper',
                      x = c(x1 / 2,
                            x1 + x2 / 2,
                            x1 + x2 + x3 / 2,
                            x1 + x2 + x3 + x4 / 2,
                            x1 + x2 + x3 + x4 +x5 / 2),
                      y = 1.15,
                      text = top_labels,
                      font = list(family = 'Arial', size = 12, color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE)
    
      p <- subplot(p1, p) %>%
        layout(title = 'Indeklima-sensorer som indikatorer for mødeafholdelse',
               legend = list(x = 0.029, y = 1.038,
                             font = list(size = 10)),
               margin = list(l = 100, r = 20, t = 70, b = 70),
               paper_bgcolor = 'rgb(248, 248, 255)',
               plot_bgcolor = 'rgb(248, 248, 255)') %>%
        add_annotations(xref = 'paper', yref = 'paper',
                        x = -0.14, y = -0.15,
                        text = paste('OECD (2015), Household savings (indicator), Household net worth (indicator). doi: 10.1787/cfc6f499-en (Accessed on 05 June 2015)'),
                        font = list(family = 'Arial', size = 10, color = 'rgb(150,150,150)'),
                        showarrow = FALSE)
    
  })
}

shinyApp(ui = ui, server = server)                    