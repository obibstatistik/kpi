source("global.R")

shinyServer(function(input, output) {
  
  source("~/.postpass")
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  
  
  kpivisitsgraph <- dbGetQuery(con, "SELECT location, sum(visits2017) as v2017, sum(visits2016) as v2016, sum(visits2015) as v2015 FROM datamart.kpi_visits group by location")
  
  kpivisits <- dbGetQuery(con, "SELECT month, sum(visits2017) as vis17, avg(diff1716) as diff1716, sum(visits2016) as vis17, avg(diff1615) as diff1615, sum(visits2015) as vis17 FROM datamart.kpi_visits group by month order by month")
  
  kpivisitsbo <- dbGetQuery(con, "SELECT month, visits2017, diff1716, visits2016, diff1615, visits2015 FROM datamart.kpi_visits where location = 'bo'")
  kpivisitsda <- dbGetQuery(con, "SELECT month, visits2017, diff1716, visits2016, diff1615, visits2015 FROM datamart.kpi_visits where location = 'da'")
  kpivisitsta <- dbGetQuery(con, "SELECT month, visits2017, diff1716, visits2016, diff1615, visits2015 FROM datamart.kpi_visits where location = 'ta'")
  kpivisitshb <- dbGetQuery(con, "SELECT month, visits2017, diff1716, visits2016, diff1615, visits2015 FROM datamart.kpi_visits where location = 'hb'")
  kpivisitsmus <- dbGetQuery(con, "SELECT month, visits2017, diff1716, visits2016, diff1615, visits2015 FROM datamart.kpi_visits where location = 'mus'")
  kpivisitsho <- dbGetQuery(con, "SELECT month, visits2017, diff1716, visits2016, diff1615, visits2015 FROM datamart.kpi_visits where location = 'ho'")
  kpivisitshoj <- dbGetQuery(con, "SELECT month, visits2017, diff1716, visits2016, diff1615, visits2015 FROM datamart.kpi_visits where location = 'hoj'")
  kpivisitskor <- dbGetQuery(con, "SELECT month, visits2017, diff1716, visits2016, diff1615, visits2015 FROM datamart.kpi_visits where location = 'kor'")
  kpivisitsvo <- dbGetQuery(con, "SELECT month, visits2017, diff1716, visits2016, diff1615, visits2015 FROM datamart.kpi_visits where location = 'vo'")
  
  kpiloan <- dbGetQuery(con, "SELECT * FROM datamart.kpi_loan")
  kpiloanbo <- dbGetQuery(con, "SELECT * FROM datamart.kpi_loan where bibliotek = 'Bolbro Bibliotek'")
  
  dbDisconnect(con)
  
  output$plot <- renderPlotly({
    plot_ly(kpivisitsgraph, x = kpivisitsgraph$location, 
            y = kpivisitsgraph$v2015, type = 'bar', name = '2015', text = text, #gold
            marker = list(color = 'gold')
            ) %>%
      add_trace(y = kpivisitsgraph$v2016, name = '2016', marker = list(color = 'rgb(63,168,123)')) %>% #mediumseargb(63,168,123) 
      add_trace(y = kpivisitsgraph$v2017, name = '2017', marker = list(color = 'rgb(72,35,115)')) %>% #darkslateblue
      layout(yaxis = list(title = 'Antal'), barmode = 'group')
  })
  
  kpivisits$diff1716 <- percent(kpivisits$diff1716)
  kpivisits$diff1615 <- percent(kpivisits$diff1615)
  kpivisitsbo$diff1716 <- percent(kpivisitsbo$diff1716)
  kpivisitsbo$diff1615 <- percent(kpivisitsbo$diff1615)
  kpivisitsda$diff1716 <- percent(kpivisitsda$diff1716)
  kpivisitsda$diff1615 <- percent(kpivisitsda$diff1615)
  kpivisitsta$diff1716 <- percent(kpivisitsta$diff1716)
  kpivisitsta$diff1615 <- percent(kpivisitsta$diff1615)
  kpivisitshb$diff1716 <- percent(kpivisitshb$diff1716)
  kpivisitshb$diff1615 <- percent(kpivisitshb$diff1615)
  kpivisitsmus$diff1716 <- percent(kpivisitsmus$diff1716)
  kpivisitsmus$diff1615 <- percent(kpivisitsmus$diff1615)
  kpivisitsho$diff1716 <- percent(kpivisitsho$diff1716)
  kpivisitsho$diff1615 <- percent(kpivisitsho$diff1615)
  kpivisitskor$diff1716 <- percent(kpivisitskor$diff1716)
  kpivisitskor$diff1615 <- percent(kpivisitskor$diff1615)
  kpivisitsvo$diff1716 <- percent(kpivisitsvo$diff1716)
  kpivisitsvo$diff1615 <- percent(kpivisitsvo$diff1615)
  
  colnames(kpivisits) <- c("Måned", "2017", "17><16", "2016", "16><15", "2015")
  colnames(kpivisitsbo) <- c("Måned", "2017", "17><16", "2016", "16><15", "2015")
  colnames(kpivisitsda) <- c("Måned", "2017", "17><16", "2016", "16><15", "2015")
  colnames(kpivisitsta) <- c("Måned", "2017", "17><16", "2016", "16><15", "2015")
  colnames(kpivisitshb) <- c("Måned", "2017", "17><16", "2016", "16><15", "2015")
  colnames(kpivisitsmus) <- c("Måned", "2017", "17><16", "2016", "16><15", "2015")
  colnames(kpivisitsho) <- c("Måned", "2017", "17><16", "2016", "16><15", "2015")
  colnames(kpivisitshoj) <- c("Måned", "2017", "17><16", "2016", "16><15", "2015")
  colnames(kpivisitskor) <- c("Måned", "2017", "17><16", "2016", "16><15", "2015")
  colnames(kpivisitsvo) <- c("Måned", "2017", "17><16", "2016", "16><15", "2015")
  
  output$tableformat <- renderFormattable({formattable(kpivisits, list(
    "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
    "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
  )
  )})
  
  output$tablebo <- renderFormattable({formattable(kpivisitsbo, list(
    "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
    "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
  ))})
  
  output$tableda <- renderFormattable({formattable(kpivisitsda, list(
    "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
    "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
  )
  )})
  
  output$tableta <- renderFormattable({formattable(kpivisitsta, list(
    "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
    "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
  )
  )})
  
  output$tablehb <- renderFormattable({formattable(kpivisitshb, list(
    "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
    "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
  )
  )})
  
  output$tablemus <- renderFormattable({formattable(kpivisitsmus, list(
    "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
    "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
  )
  )})
  
  output$tableho <- renderFormattable({formattable(kpivisitsho, list(
    "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
    "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
  )
  )})
  
  output$tablehoj <- renderFormattable({formattable(kpivisitshoj, list(
    "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
    "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
  )
  )})
  
  output$tablekor <- renderFormattable({formattable(kpivisitskor, list(
    "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
    "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
  )
  )})
  
  output$tablevo <- renderFormattable({formattable(kpivisitsvo, list(
    "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
    "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
  )
  )})
  

  
  
  
})