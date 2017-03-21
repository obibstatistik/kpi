source("global.R")

shinyServer(function(input, output) {

  source("~/.postpass")
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  
  levtidmat <- dbGetQuery(con, "SELECT * FROM datamart.accession_lev_tid_mat")
  levtidres <- dbGetQuery(con, "SELECT * FROM datamart.accession_lev_tid_res where r_pr_e > 3")
  rescount <- dbGetQuery(con, "SELECT (case when r_pr_e  > 3 then 'suppleres' else 'ok' end) as status, sprog FROM datamart.accession_lev_tid_res")
  levtidbes <- dbGetQuery(con, "SELECT * FROM datamart.accession_lev_tid_bes")
  kpivisits <- dbGetQuery(con, "SELECT * FROM datamart.kpi_visits")
  
  dbDisconnect(con)
  
  # visits
  
  output$plot <- renderPlotly({
    plot_ly(kpivisits, x = kpivisits$location, y = kpivisits$antal2015, type = 'bar', name = '2015') %>%
    add_trace(y = kpivisits$antal2016, name = '2016') %>%
    add_trace(y = kpivisits$antal2017, name = '2017') %>%
    layout(yaxis = list(title = 'Antal'), barmode = 'group')
  })

  output$tablekpivisits <- DT::renderDataTable(DT::datatable({
    data <- kpivisits
    if (input$filial != "All") {
      data <- data[data$location == input$filial,]
    }
    data <- data[data$month > input$periode[1] & data$month < input$periode[2],]
    data
  }, 
  class = 'cell-border stripe',
  rownames = FALSE,
  colnames = c('Måned', 'Filial', 'Antal2015', 'Antal2016', 'Antal2017')
  ))
  
  # reserveringer
  output$table <- DT::renderDataTable(DT::datatable({
    data <- levtidres
    if (input$sprog != "All") {
      data <- data[data$sprog == input$sprog,]
    }
    data
  }))
  
  output$plotres <- renderPlot({
    data <- rescount
    if (input$sprog != "All") {
      data <- data[data$sprog == input$sprog,]
    }
    slices <- c(sum(data$status == 'suppleres'), sum(data$status == 'ok'))
    lbls <- c("Suppleres", "Ok")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(slices, labels = lbls)
  })
  
  # bestillinger  
  
  
  output$plotbes <- renderPlot({
    data <- levtidbes
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    if (input$cat != "All") {
      data <- data[data$cat == input$cat,]
    }
    slices <- c(sum(data$time < '1 day'), sum(data$time < '1 day' & data$time < '2 day'), sum(data$time < '2 day'))
    lbls <- c("1 dag", "2 dage", "over 2 dage")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(slices, labels = lbls, main="Leveringsdage")
  })
  
  output$plotbes2 <- renderPlot({
    data <- levtidbes
    ggplot(data, aes(x= data$days)) + geom_freqpoly(bins = 365) + xlim(0, 365)
  })
  
  output$tablebes <- DT::renderDataTable(DT::datatable({
    data <- levtidbes
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    if (input$cat != "All") {
      data <- data[data$cat == input$cat,]
    }
    data
    
  },
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c('År', 'Resno', 'Cat', 'Bestil','Lev','Tid','Dage')))
  
})