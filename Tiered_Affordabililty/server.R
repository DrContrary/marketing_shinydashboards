
shinyServer(function(input, output) {
   
  
  output$facet_afford_output <- renderPlot({
    affordability_static  
  })
  
  
  output$afford_selected = renderPlot({
    selected_city() %>%
      ggplot(aes(x=date, y=PercentIncomeSpentOnRent)) +
      geom_point(aes(color= tier), size = 3) +
      xlab("Date") +
      ylab("Mean Percent of Income Spent on Rent") +
      labs (color = "Property Tier") +
      scale_y_continuous(labels = percent)+  #change the y-axis to percent
      theme_bw() + theme(legend.text = element_text(face = "plain", size = 10),
            legend.title = element_text(face = "plain", size = 14),
            legend.title.align = .5,
            axis.title = element_text(face = "bold", size = 14),
            axis.title.y = element_text(margin = margin(r =20)),
            axis.title.x = element_text(margin = margin(t =20))
      )
  }) 
  
  selected_city = reactive({
    Tiered %>%
      filter(RegionName == input$Region)})
  
  
})
