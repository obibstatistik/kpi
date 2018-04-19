## metadata module

sliderTextUI <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("slider"), "Slide me", 0, 100, 5),
    textOutput(ns("number"))
  )
}

sliderText <- function(input, output, session, show) {
  output$number <- renderText({
    if(show())
      input$slider
    else
      NULL
  })
  reactive({input$slider + 5})
}