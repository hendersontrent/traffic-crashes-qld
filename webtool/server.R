# Define server function

shinyServer <- function(input, output, session) {
  
#------------------------TIME SERIES------------------------------
  
  #-------------------
  # RAW DATA
  #-------------------
  
  output$raw_ts <- renderPlotly({

    p <- d1 %>%
      filter(crash_severity == input$ts_severity) %>%
      group_by(crash_date, crash_severity) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      ggplot(aes(x = crash_date, y = value)) +
      geom_line(size = 1.25, colour = "steelblue2") +
      geom_point(size = 1.5, colour = "steelblue2") +
      labs(x = "Date",
           y = "Count") +
      scale_y_continuous(labels = comma) +
      theme_bw() +
      theme(panel.grid.minor = element_blank())
    
    ggplotly(p) %>%
      layout(plot_bgcolor  = "rgba(255, 255, 255, 0.2)",
             paper_bgcolor = "rgba(255, 255, 255, 0.2)") %>%
      config(displayModeBar = F)
    
  })
  
  #-------------------
  # DECOMPOSITION
  #-------------------
  
  # Run GAM model
  
  ts_gam <- reactive({
    
    tmp1 <- d1 %>%
      filter(crash_severity == input$ts_severity)
    
    m1 <- gam(counter ~ s(crash_month_int, k = 12, bs = "cc") + s(date_num),
              data = tmp1,
              family = poisson(link = "log"),
              method = "REML")
    
    return(m1)
    
  })
  
  # Extract deviance explained
  
  output$ts_gam_dev <- renderUI({
    
    the_text <- paste0("Model deviance explained = ", round(ts_gam()$dev.expl*100, digits = 2), "%")
    
    HTML(the_text)
    
  })
  
  # Extract trend component
  
  output$ts_gam_trend <- renderPlot({
    
    #INSERT PROJECT CODE
    
  })
  
  # Extract seasonality component
  
  output$ts_gam_seas <- renderPlot({
    
    #INSERT PROJECT CODE
    
  })
  
  #-------------------
  # FORECAST
  #-------------------
  
  output$forecast_mod <- renderPlot({
    
    
    
  })
  
#------------------------CROSS SECTIONAL--------------------------
  
  #-------------------
  # DENSITIES
  #-------------------
  
  
  
  #-------------------
  # MODEL OUTPUTS
  #-------------------
  
  
  
}