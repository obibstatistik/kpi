source("global.R")
source("~/.postpass")
source("functions.R")

### POSTGRES METADATA MODUL ###

# UI

metaTabPanelUI <- function(id) {

  ns <- NS(id)
  fluidRow(
    column(12,
           tagList(
             h3(textOutput(ns("heading"))),
             p(textOutput(ns("description"))),
             tags$span("Schema: "), textOutput(ns("schemaname")),
             tags$span("Tabel: "), textOutput(ns("tablename")),
             tags$span("Opdatering: "), textOutput(ns("tablecomment")),
             h4("Eksempel"),
             tableOutput(ns("example")),
             h4("Kolonner"),
             tableOutput(ns("datasources_schema"))
           )
         )
       )
}

# SERVER

metaTabPanel <- function(input, output, session, schema, table, description) {

  drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
    tablecomment <- dbGetQuery(con, paste0("select obj_description('",schema,".", table, "'::regclass)"))
    datasources_schema <- (dbGetQuery(con, "SELECT columns.table_name as name, columns.column_name, columns.data_type,columns.column_default, columns.is_nullable FROM information_schema.columns;"))
    example <- dbGetQuery(con, "select * from datamart.arrangementer_old limit 1")
  dbDisconnect(con)

  output$heading <- renderText(table)
  output$description <- renderText(description)
  output$schemaname <- renderText(schema)
  output$tablename <- renderText(table)
  output$tablecomment <- renderText(tablecomment[[1]])
  output$example <- renderTable(example)
  output$datasources_schema <- renderTable(datasources_schema %>% filter(name == table) %>% select(-name))
}

### DOWNLOAD CSV MODUL ###

# UI

csvDownloadUI <- function(id, label = "Download CSV") {
  ns <- NS(id)
  
  downloadButton(ns("download"), label)
}

# SERVER

csvDownload <- function(input, output, session, data, name = NULL) {

  output$download <- downloadHandler(
    filename = function() {
      if(is.na(name)){"test.csv"} else {paste0(name,".csv")}
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
}

### DOWNLOAD XLSX MODUL ###

# UI
xlsxDownloadUI <- function(id, label = "Download tabel som Excelark") {
  ns <- NS(id)
  downloadButton(ns("download_xlsx"), label, class = "hidden-print")
}

# SERVER
xlsxDownload <- function(input, output, session, data, name = NULL) {

  # Formatting notes here: https://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
  output$download_xlsx <- downloadHandler(
    filename = function() {
      if(is.na(name)){"test.xlsx"} else {paste0(name,".xlsx")}
    },
    content = function(file) {
      tempFile <- tempfile(fileext = ".xlsx")
      wb <- createWorkbook()
      addWorksheet(wb, name)
      writeDataTable(wb, 1, data(), startRow = 3, startCol = 2, tableStyle = "TableStyleMedium2")
      setColWidths(wb, 1, cols = 2:15, widths = 22)
      #setColWidths(wb, 1, cols = 2:6, widths = 15)
      #setColWidths(wb, 1, cols = 7, widths = 17)
      #setColWidths(wb, 1, cols = 8:9, widths = 22)
      saveWorkbook(wb, file=tempFile, overwrite = TRUE)
      file.rename(tempFile, file)
    },
    contentType="application/xlsx"
  )
}

### KPI TILE MODULE ###

# UI
kpitileUI <- function(id, image, text, color, width) {
  ns <- NS(id)
  tagList(
    div(
      div(
        span(tags$img(src = image, width = "45px", height = "45px"), class="info-box-icon", style=paste0("background-color:",color)),
        div(
          span(text, class="info-box-text"),
          span(htmlOutput(ns("kpitile")), class="info-box-number"),
          class="info-box-content"
        ), 
        class = "info-box"
      ), 
      class = paste0("shiny-html-output shiny-bound-output col-md-", width)
    )
  )
}

# SERVER
kpitile <- function(input, output, session, data) {
  data_as_string <- paste( unlist(data), collapse='')
  data_as_string_with_thousand_seperator <- paste(substr(data_as_string, 1, 4-1), ".", substr(data_as_string, 4, nchar(data_as_string)), sep = "")
  output$kpitile <- renderText(ifelse(nchar(data) > 3, data_as_string_with_thousand_seperator, data_as_string))
}

### GENERISK CHECKBOX UDFRA DATA ###

checkboxFromDataUI <- function(id, data, text) {
  ns <- NS(id)
  checkboxGroupInput(ns(id),
     paste0(text,":"),
     unique(as.character(data)),
     selected = unique(as.character(data)), 
     inline = F)
}

checkboxFromData <- function(input, output, session, data) { 
}

### HEATMAP UGE TABEL ###

heatmapWeekTableUI <- function(id, title) {
  ns <- NS(id)
  tagList(
    h4(title),
    sliderInput(ns("timeslider"),
                NULL,
                min = as.Date("2016-08-11","%Y-%m-%d"),
                max = Sys.Date(),
                value= c(as.Date("2016-08-11","%Y-%m-%d"),Sys.Date()),
                step = 1,
                timeFormat="%Y-%m-%d"),
    #uiOutput(ns("timeslider")), # se kommentar i server funktion
    formattableOutput(ns("heatmapWeekTable"))
  )
}

heatmapWeekTable <- function(input, output, session, data, type) {
  
  # skal laves om til at bruge denne renderUI funktion, for at få dynamiske datoer alt efter datasæt
  
  # output$timeslider <- renderUI({
  #   sliderInput("timeslider",
  #               NULL,
  #               min = as.Date(min(data$date),"%Y-%m-%d"),
  #               max = Sys.Date(),
  #               value= c(as.Date(min(data$date),"%Y-%m-%d"),Sys.Date()),
  #               step = 1,
  #               timeFormat="%Y-%m-%d")
  # })
  
  output$heatmapWeekTable <- renderFormattable({
    if ( type== "count")
    data <- data %>%
      filter(date > input$timeslider[1] & date < input$timeslider[2]) %>%
      select(-date) %>%
      group_by(hour, weekday) %>%
      summarise(count = sum(as.numeric(count))) %>%
      mutate_at(vars(3), funs(as.integer(.))) %>%
      spread(key = hour, value = count) %>%
      rename(ugedag = weekday) %>%
      mutate_at(vars(1), funs(danskedage(.))) %>%
      adorn_totals("row")%>%
      mutate_all(funs(ifelse(is.na(.), "-",.))) 
    else  
    data <- data %>%
      filter(date > input$timeslider[1] & date < input$timeslider[2]) %>%
      select(-date) %>%
      group_by(hour, weekday) %>%
      summarise(tid = mean(as.numeric(tid))) %>%
      mutate_at(vars(3), funs(as.integer(.))) %>%
      spread(key = hour, value = tid) %>%
      rename(ugedag = weekday) %>%
      mutate_at(vars(1), funs(danskedage(.))) %>%  
      adorn_totals("row") %>%
      mutate_all(funs(ifelse(is.na(.), "-",.)))  
    formattable(data, list(area(col = -1, row = 1:5) ~ color_tile("transparent", "DarkSlateGray4")))
  })
}