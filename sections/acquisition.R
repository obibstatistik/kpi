source("global.R")
source("modules.R")
source("~/.postpass")

# UI

acquisitionTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  ### MATERIALEINDKØB ###
  
  tabItem(tabName = "acquisition",
          box(width = 12, solidHeader = TRUE, id="materialsheader2",
              h3("Materialeindkøb - Imusic 2017"),
              img(src='icons/materialer_negativ_45x45.png', align = "right", height="46px")
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

acquisitionTabPanel <- function(input, output, session, data, tablename) {

  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  acquisition <- dbGetQuery(con, "SELECT * FROM public.imusic limit 1000")
  dbDisconnect(con)
  
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