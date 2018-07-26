source("global.R")
source("functions.R")
source("modules.R")
source("~/.postpass") 
library(d3treeR)

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
          
          box(width = 12, solidHeader = TRUE, id="inventoryheader1",
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
                                             tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
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
                                             tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                                             #h4("Vælg bibliotek at sammenligne med"),
                                             selectInput(ns("inv_compare2_bibfilter"),"Vælg bibliotek til sammenligning:",c(
                                                                                                  "Bolbro" = "Bolbro Bibliotek",
                                                                                                  "Dalum" = "Dalum Bibliotek",
                                                                                                  "Højby" = "Højby Bibliotek",
                                                                                                  "Historiens Hus" = "Lokalhistorisk Bibliotek",
                                                                                                  "Holluf Pile" = "Holluf Pile Bibliotek",
                                                                                                  "Borgernes Hus" = "Odense Hovedbibliotek",
                                                                                                  "Korup" = "Korup Bibliotek",
                                                                                                  "Tarup" = "Tarup Bibliotek",
                                                                                                  "Vollsmose" = "Vollsmose Bibliotek"))
                                      ),
                                      column(10,
                                             h4("Beholdning"),
                                             p("Arealet af hver firkant er proportionelt med antallet af eksemplarer i den viste kategori."),
                                             p("Af performance-hensyn er kombinationer af afdeling, opstilling, delopstilling med under 10 eksemplarer ikke medtaget (dvs. for at danne visualiseringen inden for rimelig tid)."),
                                             p("Materialegrupper (tidl. materialesamlinger/udlånsregler) er ikke medtaget, da de ikke forekommer i datagrundlaget"),
                                             #d3tree3Output(ns('tree1'), height = "700px", width="100%")
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
                                                                inline = F)
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

inventoryTabPanel <- function(input, output, session, data, tablename) {
  
  treemapdata1 <- reactive({
    behold_compare1 <- beholdning2 %>%
      filter(beholdning2$bibliotek == input$inv_compare1_bibfilter) %>%
      group_by_at(input$inv_compare_niveaufilter) %>%
      #filter(antal > 9) %>%
      summarise(antal = sum(antal)) %>%
      rename("niveau" = names(.)[1]) %>%
      mutate(kasse_label = paste(niveau, antal,sep = "\n")) %>%
      replace_na(list(afdeling="INGEN AFDELING",opstilling="INGEN OPSTILLING",delopstilling="INGEN DELOPSTILLING"))
  })
  
  treemapdata2 <- reactive({
    behold_compare2 <- beholdning2 %>%
      filter(beholdning2$bibliotek == input$inv_compare2_bibfilter) %>%
      group_by_at(input$inv_compare_niveaufilter) %>%
      #group_by(delopstilling) %>%
      #filter(antal > 9) %>%
      summarise(antal = sum(antal)) %>%
      rename("niveau" = names(.)[1]) %>%
      #mutate(kasse_label = paste(input$inv_compare_niveaufilter, antal,sep = "\n")) %>%
      mutate(kasse_label = paste(niveau, antal,sep = "\n")) %>%
      #mutate(kasse_label = paste(delopstilling, antal,sep = "\n")) %>%
      replace_na(list(afdeling="INGEN AFDELING",opstilling="INGEN OPSTILLING",delopstilling="INGEN DELOPSTILLING"))
  })
  
  #beholdning2 <- beholdning2 %>%
  #  filter(beholdning2$branch == 'Dalum Bibliotek') %>%
  #  group_by(dep,loc,subloc) %>%
  #  #filter(antal > 9) %>%
  #  summarise(antal = sum(antal)) %>%
  #  replace_na(list(dep="INGEN AFDELING",loc="INGEN OPSTILLING",subloc="INGEN DELOPSTILLING"))
  
  #behold_compare1$subloc_label <- paste(behold_compare1$subloc, behold_compare1$antal,sep = "\n")
  #behold_compare2$subloc_label <- paste(behold_compare2$subloc, behold_compare2$antal,sep = "\n")
  
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
    # This makes the treemap interactive
    # rootname is the title of the plot
    #inter=d3tree2( p ,  rootname = "Beholdning" )
  
  #output$inv_treemap <- renderPlot(
  #  treemap(treemapdata(),
  #          index=c("overgruppe","gruppe"),
  #          vSize="sum",
  #          type="index",
  #          fontsize.title=14,
  #          title="Treemap"#,
  #          #vColor="sum",
  #          #palette=terrain.colors(10)
  #  )
  #)
  
  # OVERSKRIFT: SAMLINGENS BESKAFFENHED + UNDEROVERSKRIFT VED SUNBURST: SAMLINGENS DIVERSITET/FRAGMENTERING
  
    output$inventory_table <- renderFormattable({
      beholdning_alt_tbl <- beholdning_alt %>%
        group_by_at(vars(bibliotek,input$niveau)) %>%
        summarise(antal = sum(antal)) %>%
        spread(key = bibliotek, value = antal, fill = 0) %>%
        select(c(input$niveau,input$branch_selector)) %>%
        adorn_totals(c("row","col"))
      formattable(beholdning_alt_tbl)
    })
  
}