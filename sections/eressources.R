source("global.R")
source("modules.R")
source("~/.postpass")

#rm(list = ls())
library(shiny)
library(ggplot2)
library(dplyr)
bcl <- read.csv("http://deanattali.com/files/bcl-data.csv", stringsAsFactors = F)



### DB QUERIES ###
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
licenses_df <- dbGetQuery(con, "select brug,pris,statbank,year,month,datamart.eressourcer_ddb.product produkt,sum(use) visninger 
from datamart.eressourcer_ddb 
left join datamart.eressourcer_ddb_kategorier on datamart.eressourcer_ddb_kategorier.name_match = datamart.eressourcer_ddb.product 
where type = 'visninger' 
group by brug,pris,statbank,year,month,datamart.eressourcer_ddb.product")
#produkter <- dbGetQuery(con, "select distinct product,statbank from datamart.eressourcer_ddb_kategorier")
dbDisconnect(con)
#licenses_df <- as.data.frame(licenses_df,stringsAsFactors = FALSE)

# UI
licensesTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "licenses",
          box(width = 12, solidHeader = TRUE, id = "eressourcesheader",
              h3("Elektroniske Ressourcer"),
              img(src='icons/eressourcer_negativ_45x45.png', align = "right", height="46px")
          ),     
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset1",
                          tabPanel("Generelt", 
                                   fluidRow(
                                     column(12,
                                            column(2,
                                                   h4("Afgræns"),
                                                   selectInput(ns("lic_fromyear"), "År:", unique(as.numeric(licenses_df$year))),
                                                   #selectInput(ns("lic_statbank"), "Statistikbankens typer:", c("Seriepublikationer" = "serie","eBøger" = "ebooks","Multimedier" = "multimedia","Databaser" = "databaser")),
                                                   #selectInput(ns("lic_statbank"), "Statistikbankens typer:", unique(as.character(licenses_df$statbank))),
                                                   #selectInput(ns("lic_priskategori"), "Priskategori:", unique(as.character(licenses_df$pris))),
                                                   #selectInput(ns("lic_brugskategori"), "Brugskategori:", unique(as.character(licenses_df$brug)))
                                                   #uiOutput(ns("lic_statbank_output")),
                                                   #uiOutput(ns("lic_priskategori_output")),
                                                   #uiOutput(ns("lic_brugskategori_output"))
                                                   selectInput(ns("countryInput"), "Country",sort(unique(bcl$Country))),
                                                   sliderInput(ns("priceInput"), "Price", min = 0, max = 100, value=c(0,50), pre="$"),
                                                   uiOutput(ns("typeOutput")),
                                                   uiOutput(ns("subtypeOutput"))    
                                            ),
                                            column(10,
                                                   h4("Licenser"),
                                                   p("Kategoriseret efter sammenlignelighed"),
                                                   #plotlyOutput(ns("licenses_plot")),
                                                   plotOutput(ns("coolplot")),
                                                   tableOutput(ns("results"))
                                            ),
                                            column(2,
                                                   #checkboxGroupInput(ns("lic_productselector"),
                                                   #                      'Vælg eRessource:',
                                                   #                      selected = unique(as.character(licenses_df$produkt)),
                                                   #                      inline = F),
                                                   #p("The first checkbox group controls the second"),
                                                   checkboxGroupInput(ns("inCheckboxGroup2"),        
                                                                      'Vælg eRessource:',
                                                                      inline = F)
                                            ),
                                            column(8,
                                                   formattableOutput(ns("licenses_table")),
                                                   tableOutput('test')
                                            ),
                                            column(12,tags$hr())
                                     )))))))
}

# SERVER
licensesTabPanel <- function(input, output, session, data, tablename) {
  # Manuel, kronologisk sortering af data fra ud fra måneder, så det ikke bliver alfabetisk
  licenses_df$month <- factor(licenses_df$month, levels = c("jan","feb","mar","apr","maj","jun","jul","aug","sep","okt","nov","dec"))
  licenses_df = licenses_df[order(licenses_df$month,decreasing=FALSE),]
  
  # Store data and its filters in reactive function (gives reusability)
  #lic_data_0 <- reactive({
  #  licenses <- licenses_df %>%
  #    filter(year == input$lic_fromyear) %>%
  #    filter(pris == input$lic_priskategori) %>%
  #    filter(brug == input$lic_brugskategori) %>%
  #    filter(statbank == input$lic_statbank)
  #})
  
  lic_data <- reactive({
    #lunk <- lic_data_0() %>%
    licenses <- licenses_df %>%
      filter(year == input$lic_fromyear) %>%
      filter(pris == input$lic_priskategori) %>%
      filter(brug == input$lic_brugskategori) %>%
      filter(statbank == input$lic_statbank)
      select(brug,statbank,produkt,month,visninger) %>%
      #filter(produkt %in% input$inCheckboxGroup2) %>%
      # filter(produkt %in% input$lic_productselector) %>%
      group_by(produkt,month) %>%
      summarise(visninger = sum(visninger)) %>%
      mutate_at(vars(-1), funs(replace(., is.na(.), 0)))
  })
  
  # Render the plot
  output$licenses_plot <- renderPlotly({
    data <- lic_data() %>%
    filter(produkt %in% input$inCheckboxGroup2) %>%
    spread(produkt, visninger)   # the plot needs a spread (pivot) of produkt
    colNames <- names(data)[-1]                         # ie. get all colnames except the first which is year or month or whatever
    # cf. https://stackoverflow.com/questions/46583282/r-plotly-to-add-traces-conditionally-based-on-available-columns-in-dataframe                                
    p <- plot_ly(data, x = ~month, type = 'scatter', mode = 'lines') 
    for(trace in colNames){
      p <- p %>% add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace, mode = 'lines')   # add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace)
    }
    p %>% layout(xaxis = list(title = 'Måneder'), yaxis = list (title = 'Visninger'))
  })

  
  
  
 # output$lic_statbank <- renderUI({
 #   selectInput(ns("lic_statbank"), "Statistikbankens typer:",choices = var_statbank())
 # })
 # 
 # output$lic_priskategori <- renderUI({
 #   selectInput(ns("lic_priskategori"), "Priskategori:",choices = var_priskategori())
 # })
 # 
 # output$lic_brugskategori <- renderUI({
 #   selectInput(ns("lic_brugskategori"), "Brugskategori:",choices = var_brugskategori())
 # })
  
  # if(is.null(lic_data())){return()}
  
  # returnerer en liste over statbankens kategorier
  # som eksisterer i lic_data() dataframen til brug i et filters choices
  
 #var_statbank <- reactive({
 #  file1 <- lic_data()
 #  as.list(unique(file1$statbank))
 #})
  
# # subsæt dataframen med det der vælges statbank inputtet
# 
# statbank_function <- reactive({
#   lic_data() %>% filter(statbank == input$lic_statbank)
# })
# 
# var_priskategori <- reactive({
#   file1 <- statbank_function()
#   as.list(unique(file1$lic_priskategori))
# })
# 
# priskategori_function <- reactive({
#   lic_data() %>% filter(pris == input$lic_priskategori)
# })
# 
# var_brugskategori <- reactive({
#   file1 <- priskategori_function()
#   as.list(unique(file1$lic_brugsskategori))
# })
# 
# brugskategori_function <- reactive({
#   lic_data() %>% filter(brug == input$lic_brugkategori)
# })
  
  
  df0 <- eventReactive(input$countryInput,{
    bcl %>% filter(Country %in% input$countryInput)
  })
  
  output$typeOutput <- renderUI({
    selectInput("typeInput", "Product type",sort(unique(df0()$Name)))
  })
  
  df1 <- eventReactive(input$typeInput,{
    df0() %>% filter(Country %in% input$countryInput)
  })
  
  output$subtypeOutput <- renderUI({
    selectInput("subtypeInput", "Product subtype",sort(unique(df1()$Subtype)))
  })
  
  df2 <- reactive({
    df1() %>% filter(Price >= input$priceInput[1], Price <= input$priceInput[2],Subtype %in% input$subtypeInput)
  })
  
  output$coolplot <- renderPlot({
    #ggplot(df2(), aes(Alcohol_Content)) + geom_histogram(binwidth = 1)
    ggplot(bcl, aes(Alcohol_Content)) + geom_histogram(binwidth = 1)
  })

  output$results <- renderTable({
    df2()
  })

  #state_function <- reactive({
  #  file1 <- continent_function()
  #  country <- input$Country
  #  file2 <- sqldf(sprintf("select * from file1 where Country = '%s' ", country))
  #  return (file2)
  #})
  
  #var_state <- reactive({
  #  file1 <- state_function()
  #  as.list(unique(file1$State))
  #})
  
  # Render the table
 #output$licenses_table <- renderFormattable({
 #  data <- lic_data() %>%
 #    filter(brug == input$lic_brugskategori) %>%
 #    spread(month, visninger)   # the table needs a spread (pivot) of month
 #  formattable(data)
 #})
 #
 #output$test <- renderTable({
 #  lic_data()
 #})
  
#  observe({
#    #x <- unique(lic_data()$statbank)
#    y <- unique(lic_data()$produkt)
#    z <- unique(lic_data()$pris)
#    w <- unique(lic_data()$brug)
#    
#    # Can use character(0) to remove all choices
#    #  if (is.null(z))
#    #    z <- character(0)
#    
#    #updateSelectInput(session, "lic_statbank",
#    #                  choices = x,
#    #                  selected = x
#    #)
#    
#    # Can also set the label and select items
#    updateCheckboxGroupInput(session, "inCheckboxGroup2",
#                             #label = paste("Checkboxgroup label", length(x)),
#                             choices = y,
#                             selected = y
#    )
#
#    updateSelectInput(session, "lic_priskategori",
#                      choices = z,
#                      selected = z
#    )   
#    
#    updateSelectInput(session, "lic_brugskategori",
#                      choices = w,
#                      selected = w
#    )   
#    
# })
    
  #lic_loc <- reactive({
  #  produkter <- produkter %>% filter(statbank == input$lic_statter)
  #})
}
