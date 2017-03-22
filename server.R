source("global.R")

shinyServer(function(input, output) {

  source("~/.postpass")
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  
  kpivisits <- dbGetQuery(con, "SELECT * FROM datamart.kpi_visits")
  kpivisitsgraph <- dbGetQuery(con, "SELECT location, sum(visits2017) as v2017, sum(visits2016) as v2016, sum(visits2015) as v2015 FROM datamart.kpi_visits group by location")
  
  dbDisconnect(con)
  
  # visits
  
  output$plot <- renderPlotly({
    plot_ly(kpivisitsgraph, x = kpivisitsgraph$location, 
      y = kpivisitsgraph$v2015, type = 'bar', name = '2015') %>%
      add_trace(y = kpivisitsgraph$v2016, name = '2016') %>%
      add_trace(y = kpivisitsgraph$v2017, name = '2017') %>%
    layout(yaxis = list(title = 'Antal'), barmode = 'group')
  })

  output$tablekpivisits <- DT::renderDataTable(DT::datatable({
    data <- kpivisits
    if (input$filial != "All") {
      data <- data[data$location == input$filial,]
    }
    #data <- data[data$month > input$periode[1] & data$month < input$periode[2],]
    data
  }, 
  class = 'cell-border stripe',
  rownames = FALSE,
  colnames = c('Måned', 'Filial', 'Antal2015', 'Antal2016', 'Antal2017')
  ))
  
  # fysiske materialer
  
  # elektroniske materialer
   
  
})