source("global.R")
source("functions.R")
source("modules.R")
source("~/.postpass") 
library(d3treeR)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
beholdning2 <- dbGetQuery(con, "SELECT branch,department dep,locationname loc,sublocation subloc,sum(material_dim_count) antal
    from cicero.beholdning
    where branch != 'Odense Arrest'
    and branch != 'Opsøgende afdeling Odense Bibliotekerne'
    group by branch,dep,loc,subloc
    order by branch,dep,loc,subloc")
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
                                             tags$br(),
                                             h4("Vælg bibliotek")
                                      ),
                                      column(10,
                                             h4("Beholdning"),
                                             p("Arealet af hver firkant er proportionelt med antallet af eksemplarer i den viste kategori."),
                                             p("Af performance-hensyn er kombinationer af afdeling, opstilling, delopstilling med under 10 eksemplarer ikke medtaget (dvs. for at danne visualiseringen inden for rimelig tid)."),
                                             p("Materialegrupper (tidl. materialesamlinger/udlånsregler) er ikke medtaget, da de ikke forekommer i datagrundlaget"),
                                             d3tree3Output(ns('tree1'), height = "700px", width="100%")
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
  
  beholdning2 <- beholdning2 %>%
    filter(beholdning2$branch == 'Dalum Bibliotek') %>%
    group_by(dep,loc,subloc) %>%
    filter(antal > 9) %>%
    summarise(antal = sum(antal)) %>%
    replace_na(list(dep="INGEN AFDELING",loc="INGEN OPSTILLING",subloc="INGEN DELOPSTILLING"))
  
  beholdning2$subloc_label <- paste(beholdning2$subloc, beholdning2$antal,sep = "\n")
  
  output$tree1 <- renderD3tree3({
    # basic treemap!"#
    p=treemap(beholdning2,
              #index=c("dep","loc","subloc"),
              index=c("dep","loc","subloc_label"),
              vSize="antal",
              type="index" 
    )    
    # This makes the treemap interactive
    # rootname is the title of the plot
    inter=d3tree2( p ,  rootname = "Beholdning" )
  })
  
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