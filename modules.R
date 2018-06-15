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
    example <- dbGetQuery(con, "select * from datamart.arrangementer limit 1")
  dbDisconnect(con)
  
  output$heading <- renderText(table)
  output$description <- renderText(description)
  output$schemaname <- renderText(schema)
  output$tablename <- renderText(table)
  output$tablecomment <- renderText(tablecomment[[1]])
  output$example <- renderTable(example)
  output$datasources_schema <- renderTable(datasources_schema %>% filter(name == table) %>% select(-name))
}

### DOWNLOAD MODUL ###

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