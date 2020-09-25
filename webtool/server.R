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
  
  # Extract deviance explained
  
  output$ts_gam_dev <- renderUI({
    
    tmp1 <- d1 %>%
      filter(crash_severity == input$ts_severity)
      
    # Final model
    
    m1 <- gam(value ~ s(nmonth, k = 12, bs = "cc") + s(nyear, k = 3),
              data = tmp1,
              family = poisson(link = "log"),
              method = "REML")
    
    the_text <- paste0("Model deviance explained = ", round(summary(m1)$dev.expl*100, digits = 2), "%")
    
    HTML(the_text)
    
  })
  
  # Extract trend component
  
  output$ts_gam_trend <- renderPlotly({
    
    tmp1 <- d1 %>%
      filter(crash_severity == input$ts_severity)
    
    # Final model
    
    m1 <- gam(value ~ s(nmonth, k = 12, bs = "cc") + s(nyear, k = 3),
              data = tmp1,
              family = poisson(link = "log"),
              method = "REML")
    
    outs <- plot_model(m1, type = "pred")
    tmp1 <- outs$nyear$data
    
    p <- tmp1 %>%
      ggplot() +
      geom_line(aes(x = x, y = predicted), colour = "steelblue2", size = 1.25) +
      geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill = "steelblue2", alpha = 0.4) +
      labs(title = "Predicted smooth effects",
           x = "Timeline",
           y = "Number of Crashes") +
      scale_y_continuous(labels = comma) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(fill = alpha("white", 0.2)),
            plot.background = element_rect(fill = alpha("white", 0.2)),
            legend.background = element_rect(fill = alpha("white", 0.2)))
    
    ggplotly(p) %>%
      layout(plot_bgcolor  = "rgba(255, 255, 255, 0.2)",
             paper_bgcolor = "rgba(255, 255, 255, 0.2)") %>%
      config(displayModeBar = F)
    
  })
  
  # Extract seasonality component
  
  output$ts_gam_seas <- renderPlotly({
    
    tmp1 <- d1 %>%
      filter(crash_severity == input$ts_severity)
    
    # Final model
    
    m1 <- gam(value ~ s(nmonth, k = 12, bs = "cc") + s(nyear, k = 3),
              data = tmp1,
              family = poisson(link = "log"),
              method = "REML")
    
    outs <- plot_model(m1, type = "pred")
    tmp1 <- outs$nmonth$data
    
    p <- tmp1 %>%
      ggplot() +
      geom_line(aes(x = x, y = predicted), colour = "steelblue2", size = 1.25) +
      geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill = "steelblue2", alpha = 0.4) +
      labs(title = "Predicted smooth effects",
           x = "Month",
           y = "Number of Crashes") +
      scale_y_continuous(labels = comma) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = alpha("white", 0.2)),
            plot.background = element_rect(fill = alpha("white", 0.2)),
            legend.background = element_rect(fill = alpha("white", 0.2)))
    
    ggplotly(p) %>%
      layout(plot_bgcolor  = "rgba(255, 255, 255, 0.2)",
             paper_bgcolor = "rgba(255, 255, 255, 0.2)") %>%
      config(displayModeBar = F)
    
  })
  
  #-------------------
  # FORECAST
  #-------------------
  
  output$forecast_mod <- renderPlot({
    
    #
    
  })
  
#------------------------CROSS SECTIONAL--------------------------

  #-------------------
  # MODEL OUTPUTS
  #-------------------
  
  # Extract deviance explained
  
  output$cs_gam_dev <- renderUI({
    
    tmp1 <- d2 %>%
      filter(year == input$cs_year) %>%
      filter(crash_severity == input$cs_severity)
    
    m1 <- gam(value ~ s(usual_resident_population) + s(educ_occ_score) + ra_name_2016, 
              data = tmp1,
              family = poisson(link = "log"),
              method = "REML")
    
    the_text <- paste0("Model deviance explained = ", round(summary(m1)$dev.expl*100, digits = 2), "%")
    
    HTML(the_text)
    
  })
  
  # Model plots
  
  output$pop_plot <- renderPlotly({
    
    tmp1 <- d2 %>%
      filter(year == input$cs_year) %>%
      filter(crash_severity == input$cs_severity)
    
    m1 <- gam(value ~ s(usual_resident_population) + s(educ_occ_score) + ra_name_2016, 
              data = tmp1,
              family = poisson(link = "log"),
              method = "REML")
    
    outs <- plot_model(m1, type = "pred")
    tmp1 <- outs$usual_resident_population$data
    
    p <- tmp1 %>%
      mutate(x = exp(x)) %>%
      ggplot() +
      geom_line(aes(x = x, y = predicted), colour = "steelblue2", size = 1.25) +
      geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill = "steelblue2", alpha = 0.4) +
      labs(title = "Predicted smooth effects",
           x = "Usual Resident Population",
           y = "Number of Crashes") +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = alpha("white", 0.2)),
            plot.background = element_rect(fill = alpha("white", 0.2)),
            legend.background = element_rect(fill = alpha("white", 0.2)))
    
    ggplotly(p) %>%
      layout(plot_bgcolor  = "rgba(255, 255, 255, 0.2)",
             paper_bgcolor = "rgba(255, 255, 255, 0.2)") %>%
      config(displayModeBar = F)
    
  })
  
  output$ses_plot <- renderPlotly({
    
    tmp1 <- d2 %>%
      filter(year == input$cs_year) %>%
      filter(crash_severity == input$cs_severity)
    
    m1 <- gam(value ~ s(usual_resident_population) + s(educ_occ_score) + ra_name_2016, 
              data = tmp1,
              family = poisson(link = "log"),
              method = "REML")
    
    outs <- plot_model(m1, type = "pred")
    tmp1 <- outs$educ_occ_score$data
    
    p <- tmp1 %>%
      ggplot() +
      geom_line(aes(x = x, y = predicted), colour = "steelblue2", size = 1.25) +
      geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill = "steelblue2", alpha = 0.4) +
      labs(title = "Predicted smooth effects",
           x = "Education Occupation Index",
           y = "Number of Crashes") +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = alpha("white", 0.2)),
            plot.background = element_rect(fill = alpha("white", 0.2)),
            legend.background = element_rect(fill = alpha("white", 0.2)))
    
    ggplotly(p) %>%
      layout(plot_bgcolor  = "rgba(255, 255, 255, 0.2)",
             paper_bgcolor = "rgba(255, 255, 255, 0.2)") %>%
      config(displayModeBar = F)
    
  })
  
  output$ra_plot <- renderPlotly({
    
    tmp1 <- d2 %>%
      filter(year == input$cs_year) %>%
      filter(crash_severity == input$cs_severity)
    
    m1 <- gam(value ~ s(usual_resident_population) + s(educ_occ_score) + ra_name_2016, 
              data = tmp1,
              family = poisson(link = "log"),
              method = "REML")
    
    outs <- plot_model(m1, type = "pred")
    tmp1 <- outs$ra_name_2016$data
    
    p <- tmp1 %>%
      mutate(ra_name_2016 = case_when(
        x == 1 ~ "Major Cities of Australia",
        x == 2 ~ "Inner Regional Australia",
        x == 3 ~ "Outer Regional Australia",
        x == 4 ~ "Remote Australia",
        x == 5 ~ "Very Remote Australia")) %>%
      ggplot() +
      geom_segment(aes(x = conf.low, xend = conf.high, y = ra_name_2016, yend = ra_name_2016), 
                   colour = "steelblue2", alpha = 0.6, size = 4) +
      geom_point(aes(x = predicted, y = ra_name_2016), size = 5, colour = "#05445E") +
      labs(title = "Predicted effects",
           x = "Crashes",
           y = "Remoteness Area") +
      scale_x_continuous(labels = comma) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = alpha("white", 0.2)),
            plot.background = element_rect(fill = alpha("white", 0.2)),
            legend.background = element_rect(fill = alpha("white", 0.2)))
    
    ggplotly(p) %>%
      layout(plot_bgcolor  = "rgba(255, 255, 255, 0.2)",
             paper_bgcolor = "rgba(255, 255, 255, 0.2)") %>%
      config(displayModeBar = F)
    
  })
  
  output$tidy_table <- renderTable({
    
    tmp1 <- d2 %>%
      filter(year == input$cs_year) %>%
      filter(crash_severity == input$cs_severity)
    
    m1 <- gam(value ~ s(usual_resident_population) + s(educ_occ_score) + ra_name_2016, 
              data = tmp1,
              family = poisson(link = "log"),
              method = "REML")
    
    outs <- tidy(m1)
    
    return(outs)
    
  })
  
  output$glance_table <- renderTable({
    
    tmp1 <- d2 %>%
      filter(year == input$cs_year) %>%
      filter(crash_severity == input$cs_severity)
    
    m1 <- gam(value ~ s(usual_resident_population) + s(educ_occ_score) + ra_name_2016, 
              data = tmp1,
              family = poisson(link = "log"),
              method = "REML")
    
    outs <- glance(m1)
    
    return(outs)
    
  })
  
}