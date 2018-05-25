# Whitebook
> Common stats for OBIB

Data visualization from a number of OBIB datasources.

## Install

``` shell
git clone https://github.com/obibstatistik/kpi
```

## Application Layout

### Files
Two file shiny application with helper files. 
- ui.R & server.R, containing core logic 
- global.R, central library loader 
- modules.R, common modules 
- functions.R, functions 
- /section/, modules with app sections.
- /www/, public static files, images and stylesheets

### Modules


ui.R sources common/section modules and call them
``` shell
# source module
source("./sections/acquisition.R")
...
# call module
acquisitionTabPanelUI(id = "acquisition") 
```

server.R common/section modules and call them
``` shell
# spource module
source("modules.R")
...
# call module
callModule(acquisitionTabPanel, id = "acquisition")
```

### Styling
Most of the styling comes from the ShinyDashboard Package which uses Boostrap & AdminLTE theme.   
Additional styles in /www/styles.css  
Icons in /www/ folder.


## Licensing

The code in this project is licensed under MIT license.
