source("global.R")
source("functions.R")
source("modules.R")
source("~/.postpass") 

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
beholdning2 <- dbGetQuery(con, "SELECT branch bibliotek,department afdeling,locationname opstilling,sublocation delopstilling,materialtypename materialetype,sum(material_dim_count) antal
    from cicero.beholdning
    where branch != 'Odense Arrest'
    and branch != 'Opsøgende afdeling Odense Bibliotekerne'
    group by bibliotek,afdeling,opstilling,delopstilling,materialetype
    order by bibliotek,afdeling,opstilling,delopstilling,materialetype")
beholdning_alt <- dbGetQuery(con, "SELECT case 
      when branch = 'Bolbro Bibliotek' then 'Bolbro' 
      when branch = 'Dalum Bibliotek' then 'Dalum' 
      when branch = 'Tarup Bibliotek' then 'Tarup' 
      when branch = 'Odense Hovedbibliotek' then 'Hovedbiblioteket' 
      when branch = 'Korup Bibliotek' then 'Korup' 
      when branch = 'Opsøgende afdeling Odense Bibliotekerne' then 'Opsøgende afd.' 
      when branch = 'Vollsmose Bibliotek' then 'Vollsmose' 
      when branch = 'Højby Bibliotek' then 'Højby'
      when branch = 'Holluf Pile Bibliotek' then 'Holluf Pile'
      when branch = 'Lokalhistorisk Bibliotek' then 'Historiens Hus'
      else branch
    end bibliotek, 
    department afdeling,
    locationname opstilling,
    sublocation delopstilling,
    materialtypename materialetype,
    material_dim_count antal
    from cicero.beholdning 
    where branch != 'Odense Arrest'")
dbDisconnect(con)

# UI

inventoryTabPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "inventory",
          
          box(width = 12, solidHeader = TRUE, id="materialsheader1",
              h3("Beholdning"),
              img(src='icons/materialer_negativ_45x45.png', align = "right", height="46px")
          ),
          fluidRow(
            column(12,
                   tabBox(width = 12,
                          id = "tabset20",
                          tabPanel("Samlingens beskaffenhed", 
                                  fluidRow(
                                    column(12,
                                      column(2,
                                             #tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                                             #h4("Vælg bibliotek"),
                                             selectInput(ns("inv_compare1_bibfilter"),"Vælg bibliotek:",c(
                                                                                                 "Bolbro" = "Bolbro Bibliotek",
                                                                                                 "Dalum" = "Dalum Bibliotek",
                                                                                                 "Højby" = "Højby Bibliotek",
                                                                                                 "Historiens Hus" = "Lokalhistorisk Bibliotek",
                                                                                                 "Holluf Pile" = "Holluf Pile Bibliotek",
                                                                                                 "Borgernes Hus" = "Odense Hovedbibliotek",
                                                                                                 "Korup" = "Korup Bibliotek",
                                                                                                 "Tarup" = "Tarup Bibliotek",
                                                                                                 "Vollsmose" = "Vollsmose Bibliotek")),
                                             selectInput(ns("inv_compare_niveaufilter"),"Vælg beholdningsniveau:",c("afdeling","opstilling","delopstilling","materialetype")),
                                             #tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                                             selectInput(ns("inv_compare2_bibfilter"),"Vælg bibliotek til sammenligning:",c(
                                                                                                  "Bolbro" = "Bolbro Bibliotek",
                                                                                                  "Dalum" = "Dalum Bibliotek",
                                                                                                  "Højby" = "Højby Bibliotek",
                                                                                                  "Historiens Hus" = "Lokalhistorisk Bibliotek",
                                                                                                  "Holluf Pile" = "Holluf Pile Bibliotek",
                                                                                                  "Borgernes Hus" = "Odense Hovedbibliotek",
                                                                                                  "Korup" = "Korup Bibliotek",
                                                                                                  "Tarup" = "Tarup Bibliotek",
                                                                                                  "Vollsmose" = "Vollsmose Bibliotek")
                                             ),
                                             tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                             ),
                                      column(10,
                                             h4("Beholdning"),
                                             p("Brug denne visualisering til at få et overblik over sammensætningen af samlingen på et enkelt bibliotek eller til at sammenligne to biblioteker med hinanden"),
                                             p("Arealet af hver firkant er proportionelt med antallet af eksemplarer i den viste kategori. Antallet er desuden angivet i hver boks"),
                                             p("Materialegrupper (tidl. materialesamlinger/udlånsregler) er ikke medtaget, da de ikke forekommer i datagrundlaget"),
                                             plotOutput(ns("inv_treemap_compare1")),
                                             plotOutput(ns("inv_treemap_compare2"))
                                      )
                                    ),
                                    column(12,tags$hr()),
                                    column(12,
                                      column(2,
                                             selectInput(ns("niveau"),"Vælg beholdningsniveau:",c("afdeling","opstilling","delopstilling","materialetype")),
                                             checkboxGroupInput(ns("branch_selector"),
                                                                'Vælg biblioteker:',
                                                                unique(as.character(beholdning_alt$bibliotek)),
                                                                selected = unique(as.character(beholdning_alt$bibliotek)),
                                                                inline = F),
                                             xlsxDownloadUI(ns("inventory")),
                                             tags$div(HTML('<a id="print-checkouts" class="btn btn-default btn-print" onclick="printDiv.call(this,event,\'.col-sm-12\',\'700px\')"><i class="fa fa-print"></i> Print denne sektion</a>'))
                                      ),
                                      column(10,
                                             h4("Beholdning fordelt på biblioteker"),
                                             p("Denne tabel tilstræber at give et overblik over beholdningsniveauer og materialetyper i anvendelse samt det antal eksemplarer de indeholder, med mulighed for at sammenligne på tværs af biblioteker"),
                                             p("Materialegrupper (tidl. materialesamlinger/udlånsregler) er ikke medtaget, da de ikke forekommer i datagrundlaget"),
                                             p("Totalerne i bunden og i højre side angiver hhv. det samlede antal registrerede eksemplarer for hvert bibliotek og det samlede antal for hvert beholdningsniveau eller materialetype (for de valgte filialer)"),
                                             tags$br(),
                                             tags$br(),
                                             formattableOutput(ns("inventory_table"))
                                      )
                                    )
                                  )
                                  # TODO beholdning fordelt på opstillinger og afdelinger, så man kan sammenligne mellem børn,voksen,musik og på hvad der er i udlån,magasin,depot
                          )
                          #,tabPanel("Data og dokumentation",
                          #         fluidRow(
                          #           column(12,
                          #                  p("Dokumentation")
                          #           )
                          #         )  
                          #)
                   ))))
}

# Server

inventoryTabPanel <- function(input, output, session, data, tablename) {
  
  treemapdata1 <- reactive({
    behold_compare1 <- beholdning2 %>%
      filter(beholdning2$bibliotek == input$inv_compare1_bibfilter) %>%
      group_by_at(input$inv_compare_niveaufilter) %>%
      #filter(antal > 9) %>%
      summarise(antal = sum(antal)) %>%
      replace_na(list(afdeling="INGEN AFDELING",opstilling="INGEN OPSTILLING",delopstilling="INGEN DELOPSTILLING")) %>%
      rename("niveau" = names(.)[1]) %>%
      # tilføj en label til hver kasse med beholdningsniveauets navn og antal eksemplarer
      mutate(kasse_label = paste(niveau, format(round(as.numeric(antal), 0), nsmall=0, big.mark="."),sep = "\n"))
  })
  
  treemapdata2 <- reactive({
    behold_compare2 <- beholdning2 %>%
      filter(beholdning2$bibliotek == input$inv_compare2_bibfilter) %>%
      group_by_at(input$inv_compare_niveaufilter) %>%
      #filter(antal > 9) %>%
      summarise(antal = sum(antal)) %>%
      replace_na(list(afdeling="INGEN AFDELING",opstilling="INGEN OPSTILLING",delopstilling="INGEN DELOPSTILLING")) %>%
      rename("niveau" = names(.)[1]) %>%
      mutate(kasse_label = paste(niveau, format(round(as.numeric(antal), 0), nsmall=0, big.mark="."),sep = "\n"))
  })
  
  output$inv_treemap_compare1 <- renderPlot(
    treemap(treemapdata1(),
              #index=c("delopstilling"),
              index=c("kasse_label"),
              vSize="antal",
              type="index",
              title =""
      )
    )
  
  output$inv_treemap_compare2 <- renderPlot(
    treemap(treemapdata2(),
              index=c("kasse_label"),
              vSize="antal",
              type="index",
              title =""
    )   
  )
  
  # OVERSKRIFT: SAMLINGENS BESKAFFENHED + UNDEROVERSKRIFT VED SUNBURST: SAMLINGENS DIVERSITET/FRAGMENTERING
  
    beholdning_alt_tbl <- reactive({
      beholdning_alt %>%
        group_by_at(vars(bibliotek,input$niveau)) %>%
        summarise(antal = sum(antal)) %>%
        spread(key = bibliotek, value = antal, fill = 0) %>%
        select(c(input$niveau,input$branch_selector)) %>%
        adorn_totals(c("row","col"))
    })
    
    output$inventory_table <- renderFormattable({ formattable(beholdning_alt_tbl()) })
    
    # Call Excel download function for tables 
    callModule(xlsxDownload, "inventory", data = reactive(beholdning_alt_tbl()), name = "beholdning")
}