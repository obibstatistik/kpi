source("global.R")
source("~/.postpass")

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

csvDownloadUI <- function(id, label = "Download Excelark") {
  ns <- NS(id)
  
  downloadButton(ns("download_xlsx"), label)
}

# SERVER

csvDownload <- function(input, output, session, data, name = NULL) {
  
  output$download <- downloadHandler(
    filename = function() {
      if(is.na(name)){"test.xlsx"} else {paste0(name,".xlsx")}
    },
    content = function(file) {
      write.csv(data, file)
    }
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
