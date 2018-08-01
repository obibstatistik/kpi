library(RPostgreSQL)
library(shiny)
library(treemap)
library(d3treeR)
library(tidyr)
library(dplyr)
#source("global.R")
source("~/.postpass")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = user, password = password)
beholdning <- dbGetQuery(con, "SELECT branch,department dep,locationname loc,sublocation subloc,sum(material_dim_count) antal
    from cicero.beholdning
    where branch != 'Odense Arrest'
    and branch != 'Opsøgende afdeling Odense Bibliotekerne'
    group by branch,dep,loc,subloc
    order by branch,dep,loc,subloc")
dbDisconnect(con)

#beholdning_1 <- beholdning %>%
#  select(dep,loc,subloc,antal) %>%
#  group_by(dep,loc,subloc) %>%
#  summarise(sum = sum(antal))

# Af performance-hensyn er kombinationer af afdeling, opstilling, delopstilling med under 10 eksemplarer ikke medtaget (dvs. for at danne visualiseringen inden for rimelig tid). 
# Materialegrupper er heller ikke medtaget af samme årsag.

ui <- fluidPage(
  d3tree3Output('tree1', height = "700px", width="100%")
)

server = function(input, output) {

  beholdning <- beholdning %>%
    filter(beholdning$branch == 'Odense Hovedbibliotek') %>%
    group_by(dep,loc,subloc) %>%
    filter(antal > 9) %>%
    summarise(antal = sum(antal)) %>%
    replace_na(list(dep="INGEN AFDELING",loc="INGEN OPSTILLING",subloc="INGEN DELOPSTILLING"))
  
  beholdning$subloc_label <- paste(beholdning$subloc," ", beholdning$antal,sep = "\n")
  
  output$tree1 <- renderD3tree3({
    # basic treemap!"#
    p=treemap(beholdning,
              #index=c("branch","dep","loc","subloc"),
              index=c("dep","loc","subloc_label"),
              vSize="antal",
              type="index",
              align.labels=list(c("center", "center"),c("right", "bottom")) # virker ikke med d3treeR
              #inflate.labels=TRUE # virker ikke med d3treeR
              #palette = "Reds",  #Select your color palette from the RColorBrewer presets or make your own.
              #title="Beholdning på OBB - Drilldown fra biblioteksniveau", #Customize your title
              #fontsize.title = 14 #Change the font size of the title
    )    
    # This makes the treemap interactive
    # rootname is the title of the plot
    inter=d3tree2( p ,  rootname = "Beholdning" )
  })
}

shinyApp(ui = ui, server = server)