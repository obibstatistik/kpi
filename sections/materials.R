source("global.R")
source("modules.R")
source("~/.postpass")

# UI

materialsTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "fysmat",
          
          box(width = 12, solidHeader = TRUE, id="materialsheader1",
              h3("Udlån"),
              img(src='materialer.png', align = "right", height="46px")
          ),
          
          fluidRow(
            column(12,
                   
                   tabBox(width = 12,
                          id = "tabset2",
                          tabPanel("Generelt", 
                                   fluidRow(
                                     column(2,
                                            checkboxGroupInput("checkGroup", "Vælg",
                                                               choices = list(
                                                                 "Bolbro Bibliotek" = "Bolbro Bibliotek",
                                                                 "Dalum Bibliotek" = "Dalum Bibliotek",
                                                                 "Fornyelser" = "Fornyelser",
                                                                 "Højby Bibliotek" = "Højby Bibliotek",
                                                                 "Historiens Hus" = "Historiens Hus",
                                                                 "Holluf Pile Bibliotek" = "Holluf Pile Bibliotek",
                                                                 "Hovedbiblioteket" = "Hovedbiblioteket",
                                                                 "Korup Bibliotek" = "Korup Bibliotek",
                                                                 "Musikbiblioteket" = "Musikbiblioteket",
                                                                 "Opsøgende afdeling" = "Opsøgende afdeling",
                                                                 "Tarup Bibliotek" = "Tarup Bibliotek",
                                                                 "Vollsmose Bibliotek" = "Vollsmose Bibliotek"
                                                               ),selected = list("Bolbro Bibliotek","Dalum Bibliotek","Fornyelser","Højby Bibliotek","Historiens Hus","Holluf Pile Bibliotek","Hovedbiblioteket","Korup Bibliotek","Musikbiblioteket","Opsøgende afdeling","Tarup Bibliotek","Vollsmose Bibliotek"))
                                     ),
                                     column(10,
                                            plotlyOutput(ns("loanplot"))    
                                     ),
                                     column(12,
                                            formattableOutput(ns("loantableall")))
                                   )
                          ),
                          tabPanel("Bolbro", formattableOutput('loantableBol')),
                          tabPanel("Dalum", formattableOutput('loantableDal')),
                          tabPanel("Borgernes Hus", formattableOutput('loantableHov')),
                          tabPanel("Holluf Pile", formattableOutput('loantableHol')),
                          tabPanel("Højby", formattableOutput('loantableHøj')),
                          tabPanel("Tarup", formattableOutput('loantableTar')),
                          tabPanel("Vollsmose", formattableOutput('loantableVol'))
                   ))))
  
  ### MATERIALEINDKØB ###
  
  tabItem(tabName = "acquisition",
          box(width = 12, solidHeader = TRUE, id="materialsheader2",
              h3("Materialeindkøb - Imusic 2017"),
              img(src='materialer.png', align = "right", height="46px")
          ),
          
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset4",
                          tabPanel("Generelt",      
                                   fluidRow(width = 12,
                                            column(width = 12,
                                                   tableOutput(ns('acquisitionsumtable'))
                                            )                
                                   )
                          ),
                          tabPanel("Klargøring",
                                   fluidRow(width = 12,
                                            column(width = 12,       
                                                   plotlyOutput(ns("acquisitionplotpreparation"))   
                                            )              
                                   )
                          )
                   )
            )
          )
  )
  
}

# SERVER

materialsTabPanel <- function(input, output, session, data, tablename) {
  # module_data <- reactive({
  #   data %>% filter(name == tablename)
  # })
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  acquisition <- dbGetQuery(con, "SELECT * FROM public.imusic limit 1000")
  dbDisconnect(con)
  
  ### LOAN & RENEWALS ###
  
  # # loan initial pivot #
  # loan <- sqlloan %>%
  #   mutate(loan2017 = ifelse(year == "2017", count, 0), loan2016 = ifelse(year == "2016", count, 0), loan2015 = ifelse(year == "2015", count, 0)) %>%
  #   select(month, library, loan2017, loan2016, loan2015) %>%
  #   group_by(month, library) %>%
  #   summarise(loan2017 = sum(loan2017), loan2016 = sum(loan2016), loan2015 = sum(loan2015))  
  # 
  # # loan plot #
  # loanplot <- loan %>%
  #   group_by(library) %>%
  #   summarise(loan2017 = sum(loan2017), loan2016 = sum(loan2016), loan2015 = sum(loan2015)) %>%
  #   select(library, loan2017, loan2016, loan2015)
  # 
  # output$loanplot <- renderPlotly({
  #   loanplot <- loanplot %>% filter(library %in% input$checkGroup)
  #   plot_ly(loanplot, x = loanplot$library, y = loanplot$loan2015, type = 'bar', name = '2015', text = text) %>%
  #   add_trace(y = loanplot$loan2016, name = '2016') %>%  
  #   add_trace(y = loanplot$loan2017, name = '2017') %>% 
  #   layout(yaxis = list(title = 'Antal'), barmode = 'group')
  # })
  # 
  # # loan all libraries #
  # loanall <- loan %>%
  #   group_by(month) %>%
  #   summarise(loan2017 = sum(loan2017), loan2016 = sum(loan2016), loan2015 = sum(loan2015)) %>%
  #   mutate(diff1716 = percent((loan2017-loan2016)/loan2016), diff1615 = percent((loan2016-loan2015)/loan2015), cumsum2017 = cumsum(loan2017), cumsum2016 = cumsum(loan2016), cumsum2015 = cumsum(loan2015), cumkum1716 = percent((cumsum(loan2017)-cumsum(loan2016))/cumsum(loan2016))) %>%
  #   arrange(month) %>%
  #   select(month, loan2017,cumsum2017,diff1716,loan2016,cumsum2016,diff1615,loan2015,cumsum2015,cumkum1716)
  # 
  # colnames(loanall) <- c("Måned", "2017", "2017 akum","17><16", "2016", "2016 akum", "16><15", "2015", "2015 akum","17><16 akum")
  # 
  # output$loantableall <- renderFormattable({formattable(loanall, list(
  #   "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
  #   "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
  # )
  # )})
  # 
  # # loan individual libraries #
  # loanlibrary <- sqlloan %>% distinct(library)
  # loanbranch <- loan
  # 
  # foreach(i = loanlibrary$library) %do% {
  #   local ({
  #     my_i <- i
  #     plotname <- paste0("loantable",substr(my_i, 1, 3))
  #     loanbranch <- loanbranch %>% 
  #       filter(library == my_i) %>%
  #       group_by(month) %>%
  #       summarize(loan2017, loan2016, loan2015) %>%
  #       mutate (diff1716 = percent((loan2017-loan2016)/loan2016), diff1615 = percent((loan2016-loan2015)/loan2015), cumsum2017 = cumsum(loan2017), cumsum2016 = cumsum(loan2016), cumsum2015 = cumsum(loan2015)) %>%
  #       select(month, loan2017, cumsum2017, diff1716, loan2016, cumsum2016, diff1615, loan2015, cumsum2015)
  #     colnames(loanbranch) <- c("Måned", "2017", "2017 akum","17><16", "2016", "2016 akum", "16><15", "2015", "2015 akum")
  #     output[[plotname]] <- renderFormattable({formattable(loanbranch, list(
  #       "17><16" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)),
  #       "16><15" = formatter("span", style = x ~ style(color = ifelse(x < 0 , "rgb(213,57,57)", "rgb(63,168,123)")), x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
  #     ))})
  #   })
  # }
  
  
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
  
}