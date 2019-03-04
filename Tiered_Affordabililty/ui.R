
library(shiny)
library(shinydashboard)


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Rental Affordability"),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      tags$head(tags$style(HTML('
                                                  .well{
                                                      background-color: #FFFFFF;!important
                                                  }
                                                  /* boxes - solid and collapable */ 
                                                    .box.box-solid.box-primary>.box-header {
                                                      color:#fff;
                                                      background:#969696;
                                                      border-bottom-color:#616161;
                                                      border-left-color:#616161;
                                                      border-right-color:#616161;
                                                      border-top-color:#616161;
                                                  }
                                                  
                                                    .box.box-solid.box-primary{
                                                      border-bottom-color:#616161;
                                                      border-left-color:#616161;
                                                      border-right-color:#616161;
                                                      border-top-color:#616161;
                                                  }  
                      '))),
                      tabPanel("Tiered Affordability", afforadability.title.box, facet_affordability, affordability_selected)
                    )
)


