# As you modify the rows of data being used from GNI (reactivity), 
# multiple objects appear on the web page, one treemap per update

library(d3treeR)
library(treemap)


server <- function(input, output) {
  
  # example 1 from ?treemap
  data(GNI2014)
  
  GNI2014$label <- paste(GNI2014$iso3, GNI2014$population, sep = "\n")
  
  GNIR <- reactive({ GNI2014[1:6] })
  
  output$tree1 <- renderD3tree3({
    
    #GNI2014$label <- paste(GNI2014$iso3, GNI2014$population, sep = "\n")
    
    tm <- treemap(GNIR()
                  ,index=c("continent", "label")
                  #,index=c("continent", "iso3")
                  ,vSize="population"
                  ,vColor="GNI"
                  ,type="value"
    )
    
    d3tree3(tm, rootname = "World")
    
  })
}

ui <- fluidPage(
  numericInput("rows", "Rows from GNI?", 20, min = 5, max = 208),
  d3tree3Output('tree1', height = "600px")
)

shinyApp(ui = ui, server = server)
