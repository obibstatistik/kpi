source("global.R")
source("modules.R")
source("~/.postpass")

# UI

staffTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "personal",
          
          box(width = 12, solidHeader = TRUE,
              h3("Personale")
          ),      
          
          box(width = 12,
              column(6, 
                     h4("Kønsfordeling"),
                     plotlyOutput(ns("peopleplot"))),
              column(6, 
                     h4("Aldersfordeling"),
                     plotlyOutput(ns("peopleplotage"))
              ),
              column(6, 
                     h4("Fordeling i øvre aldersinterval 2012-2017"),
                     plotlyOutput(ns("peopleplotageupper"))
              ),
              column(6, 
                     h4("Antal i øvre aldersinterval 2017"),
                     plotlyOutput(ns("peopleplotageupper2"))
              ),
              column(6, 
                     h4("Fordeling på faggrupper 2012-2017"),
                     plotlyOutput(ns("peopleplotfag"))
              ),
              column(6, 
                     h4("Antal i faggrupper 2017"),
                     plotlyOutput(ns("peopleplotfag2"))
              ),
              column(6, 
                     h4("Faggrupper gennemsnitsalder"),
                     plotlyOutput(ns("peopleplotfaggemall"))
              ),
              column(6, 
                     h4("Faggrupper gennemsnitsalder 2017"),
                     plotlyOutput(ns("peopleplotfaggem"))
              )
          )
  )
  
}

# SERVER

staffTabPanel <- function(input, output, session, data, tablename) {
  # module_data <- reactive({
  #   data %>% filter(name == tablename)
  # })
  
  # gender # 
  
  years <- c("2012", "2013", "2014", "2015", "2016","2017")
  male <- c(38, 37, 35, 37, 38, 39)
  female <- c(62, 63, 65, 63, 62, 61)
  data <- data.frame(years, male, female)
  
  output$peopleplot <- renderPlotly({
    plot_ly(data, x = ~years, y = ~male, type = 'bar', name = 'Mænd', marker = list(color = color2)) %>%
      add_trace(y = ~female, name = 'Kvinder', marker = list(color = color3)) %>%
      layout(xaxis = list(title = 'Årstal'), yaxis = list(title = 'Procentfordeling'), barmode = 'stack')
  })
  
  # age # 
  
  years <- c("2012", "2013", "2014", "2015", "2016", "2017")
  first <- c(19,17,12,12,12,14)
  second <- c(18,21,27,27,26,27)
  third <- c(39,37,34,33,31,31)
  fourth <- c(60,56,54,56,56,53)
  data <- data.frame(years, first, second, third, fourth) %>%
    mutate(sum = (first + second + third + fourth)) %>%
    mutate(firstp = ((first/sum)*100), secondp = ((second/sum)*100), thirdp = ((third/sum)*100), fourthp = ((fourth/sum)*100))
  
  output$peopleplotage <- renderPlotly({
    plot_ly(data, x = ~years, y = ~firstp, type = 'bar', text = text, name = '25-34', marker = list(color = color1)) %>%
      add_trace(y = ~secondp, text = toString(second), name = '35-44', marker = list(color = color2)) %>%
      add_trace(y = ~thirdp, name = '45-54', marker = list(color = color3)) %>%
      add_trace(y = ~fourthp, name = '55+', marker = list(color = color4)) %>%
      layout(xaxis = list(title = 'Årstal'), yaxis = list(title = 'Procentfordeling'), barmode = 'stack')
  })
  
  # ageupper # 
  
  years <- c("2012", "2013", "2014", "2015", "2016", "2017")
  first <- c(36,35,33,27,28,22)
  second <- c(21,19,19,25,25,32)
  third <- c(3,2,2,4,3,3)
  data2 <- data.frame(years, first, second, third) %>%
    mutate(sum = (first + second + third)) %>%
    mutate(firstp = ((first/sum)*100), secondp = ((second/sum)*100), thirdp = ((third/sum)*100))
  
  output$peopleplotageupper <- renderPlotly({
    plot_ly(data2, x = ~years, y = ~firstp, type = 'bar', text = text, name = '55-59', marker = list(color = color1)) %>%
      add_trace(y = ~secondp, text = toString(second), name = '60-64', marker = list(color = color2)) %>%
      add_trace(y = ~thirdp, name = '65+', marker = list(color = color3)) %>%
      layout(xaxis = list(title = 'Årstal'), yaxis = list(title = 'Procentfordeling'), barmode = 'stack')
  })
  
  # ageupper2016 # 
  
  years <- c("2012", "2013", "2014", "2015", "2016", "2017")
  first <- c(36,35,33,27,28,22)
  second <- c(21,19,19,25,25,32)
  third <- c(3,2,2,4,3,3)
  data3 <- data.frame(years, first, second, third) %>%
    mutate(sum = (first + second + third)) %>%
    mutate(firstp = ((first/sum)*100), secondp = ((second/sum)*100), thirdp = ((third/sum)*100))
  
  output$peopleplotageupper <- renderPlotly({
    plot_ly(data3, x = ~years, y = ~firstp, type = 'bar', text = text, name = '55-59', marker = list(color = color1)) %>%
      add_trace(y = ~secondp, text = toString(second), name = '60-64', marker = list(color = color2)) %>%
      add_trace(y = ~thirdp, name = '65+', marker = list(color = color3)) %>%
      layout(xaxis = list(title = 'Årstal'), yaxis = list(title = 'Procentfordeling'), barmode = 'stack')
  })
  
  # ageupper2016 2 # 
  
  output$peopleplotageupper2 <- renderPlotly({
    plot_ly(
      x = c(55,56,57,58,59,60,61,62,63,64,65,66,67),
      #y = c(9,2,4,3,10,8,8,5,2,2,2,0,1), 2016 tal
      y = c(9,2,4,3,9,9,7,5,2,1,1,0,1),
      name = "SF Zoo",
      type = "bar",
      marker = list(color = color1)
    )})
  
  # faggrupper # 
  
  years <- c("2012", "2013", "2014", "2015", "2016", "2017")
  first <- c(60,58,57,56,55,56)
  second <- c(65,60,59,60,58,56)
  third <- c(8,9,8,8,8,7)
  fourth <- c(3,4,3,4,4,6)
  data4 <- data.frame(years, first, second, third, fourth) %>%
    mutate(sum = (first + second + third + fourth)) %>%
    mutate(firstp = ((first/sum)*100), secondp = ((second/sum)*100), thirdp = ((third/sum)*100), fourthp = ((fourth/sum)*100))
  
  output$peopleplotfag <- renderPlotly({
    plot_ly(data4, x = ~years, y = ~firstp, type = 'bar', text = text, name = 'BF', marker = list(color = color1)) %>%
      add_trace(y = ~secondp, text = toString(second), name = 'HK', marker = list(color = color2)) %>%
      add_trace(y = ~thirdp, name = 'Øvrige AC', marker = list(color = color3)) %>%
      add_trace(y = ~fourthp, name = 'FOA_3F', marker = list(color = color4)) %>%
      layout(xaxis = list(title = 'Årstal'), yaxis = list(title = 'Procentfordeling'), barmode = 'stack')
  })
  
  # faggrupper gennemsnitsalder # 
  
  output$peopleplotfaggem <- renderPlotly({
    plot_ly(
      x = c("BF", "HK", "Øvrige ACere", "FOA/3F", "Tjenestemænd", "Overenskomstansatte", "Ledelse", "Samlet"),
      #y = c(49.2, 45.6, 39.3, 58.5, 58.1, 46.3, 49.8, 50.1), 2016 tal
      y = c(48.9, 52.2, 39.3, 54.8, 59.9, 46.6, 50.0, 49.2),
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
      add_trace(y = c(49.9,50.7,36.9,51.0,57.4,44.5,52.3,49.7), name = '2012', marker = list(color = color5)) %>%
      add_trace(y = c(48.9,52.2,39.3,54.8,59.9,46.6,50.0,50.2), name = '2017', marker = list(color = color6))
  })
  
  # faggrupper gennemsnitsalder # 
  
  output$peopleplotfag2 <- renderPlotly({
    plot_ly(
      x = c("BF", "HK", "Øvrige ACere", "FOA/3F"),
      y = c(55,58,8,4), name = "2016", type = "bar", marker = list(color = color1)) %>%
      add_trace(y = c(56,60,8,4), name = '2015', marker = list(color = color2)) %>%
      add_trace(y = c(57,59,8,3), name = '2014', marker = list(color = color3)) %>%
      add_trace(y = c(58,60,9,4), name = '2013', marker = list(color = color4)) %>%
      add_trace(y = c(60,65,8,3), name = '2012', marker = list(color = color5)) %>%
      add_trace(y = c(56,56,7,6), name = '2017', marker = list(color = color6))
  })
  
  
  
}