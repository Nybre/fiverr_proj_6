options(shiny.trace = F)  


#---base libraries
library(shiny)
library(shinydashboard)  
library(rhandsontable)       
library(shinysky) 
library(shinyBS)
library(shinydashboardPlus)  
library(shinycssloaders) 
library(shinyEffects)   
library(rmarkdown)
 
  
#shadow boxes
setShadow = shinyEffects::setShadow 

#source scrips
source("modules/performance_rating.R",local=TRUE)   

#global helper functions
# add an asterisk to an input label
 labelMandatory <- function(label) {
        tagList(
                label,
                span("*", class = "mandatory_star")
        )
}

appCSS <-".mandatory_star { color: red; }"
#this can be made a working directory string

#important download guidelines for some libraries
#download version 0.7.5
#devtools::install_github('RinteRface/shinydashboardPlus@v0.7.5')
#install tex (if on windows this might be pre installed)