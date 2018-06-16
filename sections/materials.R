source("global.R")
source("modules.R")
source("~/.postpass")

# UI

materialsTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "physicalmat",
          
          box(width = 12, solidHeader = TRUE, id="materialsheader1",
              h3("Udlån"),
              img(src='icons/materialer_negativ_45x45.png', align = "right", height="46px")
          ),
          
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset2",
                          tabPanel("Generelt", 
                                   fluidRow(
                                     column(2),
                                     column(10,
                                            h4("Samlet udlån på OBB"),
                                            p("Farvet: fra 1. januar til dags dato i pågældende år. Grå: Året total"),
                                            samedate_barchartOutput(ns('checkouts_samedate_plot'))
                                     ),
                                     column(12,tags$hr()),
                                     column(2,
                                            h4("Afgræns"),
                                            selectInput(ns("checkouts_mainlibrary_filter1"), "Total/Lokal:",c('Med Hovedbiblioteket','Uden Hovedbiblioteket'))    
                                     ),
                                     column(10,
                                            h4("Samlet udlån på OBB, hele år"),
                                            plotlyOutput(ns("checkouts_plot_all"))
                                     ),
                                     column(12,
                                            formattableOutput(ns("checkout_all_table")))
                                   )
                          ),
                          tabPanel("Timer", 
                                   fluidRow(
                                     column(2, h4("Afgræns")),
                                     column(10,
                                            plotlyOutput(ns("heat")),
                                            tableOutput(ns('table'))
                                     )
                                   )
                          ),
                          tabPanel("Cirkulation", 
                                   fluidRow(
                                     column(2, h4("Afgræns")),
                                     column(10,
                                            plotlyOutput(ns("cirkulation")),
                                            tableOutput(ns('cirktable'))
                                     ),
                                     column(12,
                                            formattableOutput(ns("cirkulation_table_all"))
                                     )
                                   )
                          ),
                          tabPanel("Pr. bibliotek", 
                                   fluidRow(
                                     column(2, h4("Afgræns")),
                                     column(10,
                                            plotlyOutput(ns("bibliotasdfek")),
                                            tableOutput(ns('bibtableasdf'))
                                     ),
                                     column(12,
                                            formattableOutput(ns("bibtableall"))
                                     )
                                   )
                          ),
                          tabPanel("Data og dokumentation",
                                   fluidRow(
                                     column(12,
                                            p("Dokumentation")
                                     )
                                   )  
                          )
                   ))))
}

# Server

materialsTabPanel <- function(input, output, session, data, tablename) {
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
  udlaan <- dbGetQuery(con, "SELECT name, hour, circulation_fact_count FROM cicero.udlaan_per_klokkeslaet")
  max_date <- dbGetQuery(con, "select max(transact_date) max_date from cicero.udlaan_per_opstillingsprofil")
  checkouts_all <- dbGetQuery(con, "SELECT extract(year from transact_date) aar,transact_date,branch,sum(antal) antal
    from cicero.udlaan_per_opstillingsprofil
    where extract(year from (transact_date)) > extract(year from (current_date - interval '5 year'))
    group by aar,transact_date,branch
    order by aar,transact_date,branch")
  dbDisconnect(con)
  
  # Calculate latest date with data
  max_date <- checkouts_all %>%
    summarize(max_date = max(transact_date))
  
  # Calculate sums for partial years, adapting max_date to each year
  checkouts_partial_years <- checkouts_all %>%
    select(aar,transact_date,antal) %>%
    group_by(aar) %>%       # Create group objects for each year. filter() and summarise() will run for each
    filter( transact_date >= as.Date( paste0(aar,"-01-01" )) & transact_date <= as.Date( paste0(aar,substr(max_date[[1]],5,11) ))) %>%
    summarise(sum = sum(antal)) %>%
    mutate(dato = paste0(aar,substr(max_date[[1]],5,11))) 
  
  # Ditto whole years
  checkouts_whole_years <- checkouts_all %>%
    select(aar,transact_date,antal) %>%
    group_by(aar) %>%
    summarise(sum = sum(antal)) %>%
    mutate(dato = paste0(aar,"-12-31")) 
  
  # Union the partial and whole years and order them by year and date (desc) 
  checkouts_samedate <- rbind(checkouts_whole_years,checkouts_partial_years) %>%
    arrange(aar, desc(dato))
  
  # Reordering and renaming columns
  checkouts_samedate <- checkouts_samedate[c(1,3,2)]      # does the column order matter?
  names(checkouts_samedate) <- c("year","date","count")   # unsure if the column names matter, but renaming just to be safe
  
  # Config for barchart comparing whole and partial years
  curDate <- as.Date(max_date[[1]], format="%Y-%m-%d") # the matching date you want data from, across all the years on the x-axis
  sortx <- "desc"         # controls direction of the sorting of the years on the x-axis
  frontColors <- colors # this vector turns into a javascript array
  backColor <- "Gainsboro"
  vizWidth <- ""
  labelx <- "År"
  labely <- "Antal"
  tickNumY <- 7
  showScaleY <- "false"
  fontSizeX <- ""
  fontSizeY <- ""
  barWidth <- 0.8    # This is a percentage. 1 means no gap between bars (i.e. 100%)
  barsOffset <- 10
  
  # Render barchart comparing whole and partial years
  output$checkouts_samedate_plot <- renderSamedate_barchart({
    samedate_barchart(checkouts_samedate,curDate,sortx,frontColors,backColor,labelx,labely,tickNumY,showScaleY,barWidth,barsOffset)
  })
  
  # Whole years dataframe with branches for filtering main library
  checkouts_whole_years_branches <- checkouts_all %>%
    group_by(aar,branch) %>%
    summarise(sum = sum(antal))
  
  # Whole years plot filtering main library
  output$checkouts_plot_all <- renderPlotly({
    checkouts_overview <- checkouts_whole_years_branches %>%
      filter(aar != as.integer(format(Sys.Date(), "%Y"))) %>%      # we only want whole years so filter out current year
      filter(if(input$checkouts_mainlibrary_filter1 == 'Uden Hovedbiblioteket') (branch != 'Odense Hovedbibliotek') else TRUE) %>%
      group_by(aar) %>%
      summarise(sum = sum(sum))
    plot_ly(checkouts_overview, x = checkouts_overview$aar, y = checkouts_overview$sum, type = 'bar', marker = list(color = color1)) %>%
      layout(yaxis = list(title = 'Antal'), xaxis = list(title = 'År', dtick = 1, autotick = FALSE, autorange="reversed"))
  })
  
  udlaan_heat <- udlaan %>%
    mutate(branch = ifelse(is.na(name), "Andet", name)) %>%
    group_by(branch, hour) %>%
    summarise(sum = sum(circulation_fact_count))
  
  output$table <- renderTable(udlaan_heat)
  
  output$heat <- renderPlotly({
    plot_ly(x=udlaan_heat$hour ,y=udlaan_heat$branch ,z = udlaan_heat$sum, type = "heatmap")
  })
  
}