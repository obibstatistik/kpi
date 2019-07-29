ui <- shinyUI(fluidPage(
  
  titlePanel("Track last clicked Action button"),
  tags$head(tags$script(HTML("$(document).on('click', '.needed', function () {
                                Shiny.onInputChange('last_btn',this.id);
                             });"))),

  sidebarLayout(
    sidebarPanel(
      HTML('<div class="btn-group btn-group-toggle" data-toggle="buttons">
              <label class="btn btn-secondary active needed"  id="fe">
                <input type="radio" name="options" id="first" autocomplete="off" checked>Årlig
              </label>
              <label class="btn btn-secondary needed"  id="fafas">
                <input type="radio" name="options" id="second" autocomplete="off">Halvårlig
              </label>
              <label class="btn btn-secondary needed" id="fafaswe">
                <input type="radio" name="options" id="third" autocomplete="off">Kvartalsvis
              </label>
              <label class="btn btn-secondary needed" id="fafaswdfe">
                <input type="radio" name="options" id="save" autocomplete="off">Månedlig
              </labe>
             </div>')
    ),
    
    mainPanel(
      
      textOutput("lastButtonCliked")
    )
  )
))


server <- shinyServer(function(input, output,session) {
  observeEvent(input$last_btn, {
    print(input$last_btn)
  })

})
# Run the application 
shinyApp(ui = ui, server = server)