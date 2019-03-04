server <- function(input, output, session) {
  
  
  output$facet_afford_output <- renderPlot({
    affordability_static  
  })
  
  #changes to this plots aesthetics must be changed in server because it is reactive
  
  output$afford_selected = renderPlot({
    selected_city() %>%
      ggplot(aes(x=date, y=PercentIncomeSpentOnRent)) +
      geom_point(aes(color= tier), size = 3) +
      xlab("Date") +
      ylab("Mean Percent of Income Spent on Rent") +
      labs (color = "Property Tier") +
      scale_y_continuous(labels = percent)+  #change the y-axis to percent
      theme(legend.text = element_text(face = "plain", size = 10),
            legend.title = element_text(face = "plain", size = 14),
            legend.title.align = .5,
            axis.title = element_text(face = "bold", size = 14),
            axis.title.y = element_text(margin = margin(r =20)),
            axis.title.x = element_text(margin = margin(t =20))
      )
  }) 
  
  output$pie_before <- renderPlot({
    MRes_Ratings_Before
  })
  
  output$pie_after <- renderPlot({
    MRes_Ratings_After
  })
  
  
  selected_city = reactive({
    Tiered %>%
      filter(RegionName == input$Region)})
  
  output$propertymap = renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = df_prop$Lon, lat = df_prop$Lat, zoom = 11) %>%
      addMarkers(data= df, lng = ~Lon, lat = ~Lat, popup = paste(df$Name, "<br>",
                                                                           df$Address, "<br>",
                                                                           "Rating:", df$Rating," / 5","<br>")) %>%
      addAwesomeMarkers(data= df_prop, lng = ~Lon, lat = ~Lat, icon = icon.fa, popup = paste( df_prop$Name, "<br>",
                                                                                              #df_prop$Address, "<br>",
                                                                                              "Rating:", df_prop$Rating," / 5","<br>"))
  })
  
  
  # Downloadable csv of selected dataset ----
  # Our dataset
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
  observe({
    input$State
    selected.state = as.character(input$State)
    median.rent = as.data.frame(median.rent)
    x = median.rent[median.rent$State == selected.state, 'City']
    xs = sort(unique(x))
    updateSelectInput(session, 'City', choices = xs, selected = '') # selected = '' makes default input no selection, prevents plot from rendering before selecting state and city
  })
  
  drawplots <- eventReactive(input$goButton, {
    input$City
  })
  
  selectedCityData = reactive({
    median.rent = filter(median.rent, State == input$State)
    median.rent[median.rent$City == input$City, ]
    
  })
  
  output$table = DT::renderDataTable({
    head(selectedCityData(), n= 2000)      
  })
  
  output$medianrentplot = renderPlot({
    drawplots()
    selectedCityData() %>%
      ggplot(aes(x=Date, y=Median_Rent)) +
      ggtitle(paste(paste0(input$City, ","), input$State),
              subtitle = "Median Rent by zipcode, 2011-2018") +
      geom_point(aes(color = Zip_Code), size = 2) +
      xlab("Date") +
      ylab("Median Rent") +
      labs (color = "Zip Code",
            caption = "Analytics Division, 2018") +
      theme_minimal() +
      theme(title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(face = "plain", size = 12),
            legend.title = element_text(face = "bold", hjust = 1),
            legend.text = element_text(face = "plain", size = 12),
            axis.line.x = element_line(size = .8, lineend = "square"),
            axis.line.y = element_line(size = .8, lineend = "butt"),
            panel.border = element_rect(fill = NA),
            plot.caption = element_text(face = "plain", size = 9),
            legend.position = "bottom",
            legend.direction = "horizontal"
      ) 
    
  }, height = 600) # size compenent here within renderplot({}) helps with plot size in tabBox
  
  output$medianrentfacet = renderPlot({
    drawplots()
    selectedCityData() %>%
      ggplot(aes(x=Date, y=Median_Rent)) + facet_wrap(vars(Zip_Code)) +
      ggtitle(paste(paste0(input$City, ","), input$State),
              subtitle = "Median Rent by zipcode, 2011-2018") +
      geom_point(aes(color = Zip_Code), size = 1) +
      xlab("Date") +
      ylab("Median Rent") +
      labs(caption = "Analytics Division, 2018") +
      theme_minimal() +
      theme(title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(face = "plain", size = 12),
            axis.line.x = element_line(size = .8, lineend = "square"),
            axis.line.y = element_line(size = .8, lineend = "butt"),
            panel.border = element_rect(fill = NA),
            plot.caption = element_text(size = 9),
            legend.position = "none"
      )
  }, height = 600
  )
  
  output$ilstable = DT::renderDataTable({portfolio})
  output$gtable = DT::renderDataTable({gmaptable})
  
  # Server Functions for the Marketing Audit tab
  output$ILStable = DT::renderDataTable({
    DT::datatable(ILS_audit[,2:6], ILS_audit, escape = FALSE, rownames = FALSE, colnames = c('Marketing Metric' = 'Marketing_Metric'))})
  output$reputationtable = DT::renderDataTable({
    DT::datatable(reputation_audit[,2:6], reputation_audit, escape = FALSE, rownames = FALSE, colnames = c('Marketing Metric' = 'Marketing_Metric'))})
  output$socialtable = DT::renderDataTable({
    DT::datatable(social_audit[,2:6], social_audit, escape = FALSE, rownames = FALSE, colnames = c('Marketing Metric' = 'Marketing_Metric'))})
  
  output$graphedaudit = renderPlot({audit_lolly}, height = 600)
  output$graphed_audit_design = renderPlot({audit_branding_lolly})
  
  output$table.branding.audit = DT::renderDataTable({
    DT::datatable(MFDC_branding_audit, rownames = FALSE)})
  
  
  # Reactive expression to create data frame of all input values for ROI ----

  sliderResults1 <- reactive({
    data.frame(
      Monthly = c("Rental Income from New Leases:",
                  "Net Revenue from New Leases:"
      ),
      Value = (c(paste("$", (input$prospective * (input$conversions/100)) * input$monthly_rent ),
                             paste("$", ((input$prospective * (input$conversions/100)) * input$monthly_rent )- input$cost_monthly)
                             )
                           ),
      stringsAsFactors = FALSE)
  })
  
  
  sliderResults2 <- reactive({
    data.frame(
      "Annual" = c( "Cost per Conversion:",
                    "Revenue per Conversion (12mo lease):",
                    "Net Revenue per Conversion:"
      ),
      Value = (c(
                paste("$",  round(((input$cost_monthly) / ((input$prospective * (input$conversions/100)))), 2)),
                paste("$",  input$monthly_rent * 12),
                paste("$",  round((input$monthly_rent * 12) - (input$cost_monthly / ((input$prospective * (input$conversions/100)) )), 2 )
                             )                      
      )
      )
      ,
      stringsAsFactors = FALSE)
  })
  
  
  output$return_month_rev <- renderTable({
    sliderResults1()
  })
  
  output$return_per_con <- renderTable({
    sliderResults2()
  })
  
  observeEvent(input$credits_dialog, {
    showModal(modalDialog(
      fluidRow(box(width = "100%")),
      fluidRow(box(width = "100%", 
                   h1("Interactive Dashboard and Custom Data Vizualizations"),
                 #  h3("Built using tools made for data science."),
                   br(),
                   br(),
    
                   h2("Coded from the ground up by:"),
                   h3("Alyssa Rolfe, PhD"),
                   h3("- - - With help from - - -"),
                   h3("Ali Darkazalli")))
      ,
      fluidRow(box(width = "100%")),
      
      size = "l",
      fade = TRUE,
      easyClose = TRUE,
      footer = NULL))
    }
    )
  
  output$tweetsbydayplot <- renderPlot({
    plot_tweetsbyday  
  })

  output$tweetsbydayplot2 <- renderPlot({
    plot_tweetssentbyday  
  })
  
  
  }
  

