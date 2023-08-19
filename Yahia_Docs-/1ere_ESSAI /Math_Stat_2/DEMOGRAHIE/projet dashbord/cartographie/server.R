## Shiny Server component for dashboard

function(input, output, session){
  
  # Data table Output
  output$dataT <- renderDataTable(fdc@data)
  
  
  # Rendering the box header  
  output$head1 <- renderText(
    paste("5 regions with high rate of", input$var2)
  )
  
  # Rendering the box header 
  output$head2 <- renderText(
    paste("5 regions with low rate of", input$var2)
  )
  
  
  # Rendering table with 5 states with high arrests for specific crime type
  output$top5 <- renderTable({
    
    fdc@data %>% 
      select(del_nom, input$var2) %>% 
      arrange(desc(get(input$var2))) %>% 
      head(5)
    
  })
  
  # Rendering table with 5 states with low arrests for specific crime type
  output$low5 <- renderTable({
    
    fdc@data %>% 
      select(del_nom, input$var2) %>% 
      arrange(get(input$var2)) %>% 
      head(5)
    
    
  })
  
  
  # For Structure output
  output$structure <- renderPrint({
    fdc@data %>% 
      str()
  })
  
  
  # For Summary Output
  output$summary <- renderPrint({
    fdc@data %>% 
      summary()
  })
  
  # For histogram - distribution charts
  output$histplot <- renderPlotly({
    p1 = fdc@data %>% 
      plot_ly() %>% 
      add_histogram(x=~get(input$var1)) %>% 
      layout(xaxis = list(title = paste(input$var1)))
    
    
    p2 = fdc@data %>%
      plot_ly() %>%
      add_boxplot(x=~get(input$var1)) %>% 
      layout(yaxis = list(showticklabels = F))
    
    # stacking the plots on top of each other
    subplot(p2, p1, nrows = 2, shareX = TRUE) %>%
      hide_legend() %>% 
      layout(title = "Distribution chart - Histogram and Boxplot",
             yaxis = list(title="Frequency"))
  })
  
  
  ### Bar Charts - State wise trend
  output$bar <- renderPlotly({
    fdc@data %>% 
      plot_ly() %>% 
      add_bars(x=~del_nom, y=~get(input$var2)) %>% 
      layout(title = paste("Region by ", input$var2),
             xaxis = list(title = "Regions"),
             yaxis = list(title = paste(input$var2) ))
  })
  
  
  # Choropleth map
  output$map_plot <- renderPlot({
    fdc@data %>%
      plot(fdc, col = colMap, border = "black", lwd = 1)
    
    
  })
  
  
  
}