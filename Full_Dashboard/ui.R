

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Multifamily Dashboard", titleWidth = 450, dm, mm, tm),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                        tags$head(tags$style(HTML('
                                                  .well{
                                                      background-color: #FFFFFF;!important
                                                  }
                                                  
                                                  .container-fluid {
                                                      padding-right: 0px;
                                                      padding-left: 0px;
                                                  }

                                                  .navbar-header {
                                                      padding-left: 15px;
                                                  }

                                                  .row{
                                                      margin-right: 0px;
                                                      margin-left: 0px;
                                                  }

                                                  .irs-grid-text { font-size: 12px; color: black;}
                                                  .irs-max {font-size: 12px; color: black;}
                                                  .irs-min {font-size: 12px; color: black;}
                                                  .irs-single {color:black; font-size: 14px;}

                                               
                                                  /* logo */
                                                    .skin-blue .main-header .logo {
                                                      background-color: #67B3BA;
                                                      font-size: 30px;
                                                    }
                                                  
                                                  /* logo when hovered */
                                                    .skin-blue .main-header .logo:hover {
                                                      background-color: #67B3BA;
                                                    }
                                                  
                                                  /* navbar (rest of the header) */
                                                    .skin-blue .main-header .navbar {
                                                      background-color: #67B3BA;
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
                      navbarPage(property_name,
                                 tabPanel("Welcome Page", welcome.page, fr.welcome.page.infoboxes, b.data.credits
                                 ),
                                 navbarMenu("Market Analysis",
                                            tabPanel("Median Rent", meanrent.title.box, median_rent_reactive),
                                            tabPanel("Tiered Affordability", afforadability.title.box, facet_affordability, affordability_selected)
                                            ),
                                 navbarMenu("Marketing Audit",
                                            tabPanel("Audit Report Data",
                                                       titlePanel("Audit Report Data"),
                                                         box(width = 12, height = 700, column(width = 12, plotOutput("graphedaudit"))),
                                                         tabBox(width = 12,
                                                                tabPanel("ILS Audit", br(),br(), DT::dataTableOutput("ILStable", width = "100%")),
                                                                tabPanel("Reputation Audit",br(),br(), DT::dataTableOutput("reputationtable", width = "100%")),
                                                                tabPanel("Social Audit", br(),br(), DT::dataTableOutput("socialtable", width = "100%"))
                                                          )
                                                     ),
                                            tabPanel("Design and Branding Audit",  
                                                     titlePanel("Design and Branding Audit"),
                                                     box(width = 12, plotOutput("graphed_audit_design")),
                                                     tb.design.branding
                                                     )
                                            
                                             ),

                                 navbarMenu("Web Reputation",
                                            tabPanel("Google Report", greport),
                                            tabPanel("Social Media",
                                                     tabBox(
                                                       width = 12,
                                                       tabPanel("Twitter", twitterstats, succesfultweets, br(), row_plot_tweetsbyday, br(),  row_plot_tweetssentbyday),
                                                       tabPanel("Facebook"),
                                                       tabPanel("Instagram")))
                                            ),
                                 navbarMenu("Strategic Plan",
                                            tabPanel("Marketing ROI Calculator", 
                                                     ROIcalculator.title.box, 
                                                     ROIcalculator_page),
                                            tabPanel("Custom Plan", tb.strategic.plan),
                                            tabPanel("Credits", credits_page)
                                            
                                          )
                                 )
                    )
                    )



