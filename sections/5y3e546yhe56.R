library(shiny)

library(ggplot2)

library(dplyr)


bcl <- read.csv("http://pub.data.gov.bc.ca/datasets/176284/BC_Liquor_Store_Product_Price_List.csv", stringsAsFactors = F)




ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countryInput", "Country",sort(unique(bcl$PRODUCT_COUNTRY_ORIGIN_NAME))),
      sliderInput("priceInput", "Price", min = 0, max = 100, value=c(0,50), pre="$"),
      uiOutput("typeOutput"),
      uiOutput("subtypeOutput")           
    ),               
    mainPanel(
      plotOutput("coolplot"),
      br(),
      dataTableOutput("results")
    )
  )
)



server <- function(input, output) {
  # create a reactive to filter the dataset
  
  df0 <- eventReactive(input$countryInput,{
    bcl %>% filter(PRODUCT_COUNTRY_ORIGIN_NAME %in% input$countryInput)
  })
  output$typeOutput <- renderUI({
    selectInput("typeInput", "Product type",sort(unique(df0()$PRODUCT_CLASS_NAME)))
  })
  
  df1 <- eventReactive(input$typeInput,{
    df0() %>% filter(PRODUCT_CLASS_NAME %in% input$typeInput)
  })
  output$subtypeOutput <- renderUI({
    selectInput("subtypeInput", "Product subtype",sort(unique(df1()$PRODUCT_MINOR_CLASS_NAME)))
  })
  
  df2 <- reactive({
    df1() %>% filter(CURRENT_DISPLAY_PRICE >= input$priceInput[1],
                     CURRENT_DISPLAY_PRICE <= input$priceInput[2],
                     PRODUCT_MINOR_CLASS_NAME %in% input$subtypeInput)
  })
  
  output$coolplot <- renderPlot({
    ggplot(df2(), aes(PRODUCT_ALCOHOL_PERCENT)) + geom_histogram(binwidth = 1)
  })
  output$results <- renderTable({
    df2()
  })
}




shinyApp(ui = ui, server = server)