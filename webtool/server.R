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
  
  #------------------------ MAP ----------------------------------
  
  # Define palette function for use with map 
  
  pal_fun <- colorNumeric("Spectral", NULL, na.color = "grey", reverse = TRUE)
  
  # Need the reverse of the palette function as the only way to do the legend in reverse order
  
  pal_fun_leg <- colorNumeric("Spectral", NULL, na.color = "grey", reverse = FALSE)
  
  # Prep map data
  
  the_map_data <- reactive({
    
    if(input$map_geog == "Postcode"){
      the_map_data <- post_1 %>%
        filter(crash_year == input$map_year) %>%
        filter(crash_severity == input$map_severity)
    } else if (input$map_geog == "SA2"){
      the_map_data <- sa2_2 %>%
        filter(crash_year == input$map_year) %>%
        filter(crash_severity == input$map_severity)
    } else if (input$map_geog == "SA3"){
      the_map_data <- sa3_3 %>%
        filter(crash_year == input$map_year) %>%
        filter(crash_severity == input$map_severity)
    } else{
      the_map_data <- sa4_4 %>%
        filter(crash_year == input$map_year) %>%
        filter(crash_severity == input$map_severity)
    }
    return(the_map_data)
  })
  
  # Render map
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      setView(lng = 142.702789, lat = -20.917574, zoom = 5) %>%
      addTiles()
  })
  
  observe({
    
    if(input$map_geog == "Postcode"){
      leafletProxy("map",
                   data = the_map_data()) %>%
        clearShapes() %>%
        addPolygons(fillColor = ~pal_fun(value),
                    fillOpacity = 0.8,
                    layerId = ~POA_NAME16,
                    stroke = TRUE,
                    color = "grey20",
                    label = paste0("Postcode: ",the_map_data()$POA_NAME16, " ", "Crash Count: ", 
                                   the_map_data()$value)) %>%
        clearControls() %>%
        addLegend(pal = pal_fun_leg,
                  values = ~value,
                  position = "bottomright",
                  opacity = 0.8,
                  title = "",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    } else if (input$map_geog == "SA2"){
      leafletProxy("map",
                   data = the_map_data()) %>%
        clearShapes() %>%
        addPolygons(fillColor = ~pal_fun(value),
                    fillOpacity = 0.8,
                    layerId = ~loc_abs_statistical_area_2,
                    stroke = TRUE,
                    color = "grey20",
                    label = paste0("SA2: ", the_map_data()$loc_abs_statistical_area_2, " ", "Crash Count: ", 
                                   the_map_data()$value)) %>%
        clearControls() %>%
        addLegend(pal = pal_fun_leg,
                  values = ~value,
                  position = "bottomright",
                  opacity = 0.8,
                  title = "",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    } else if (input$map_geog == "SA3"){
      leafletProxy("map",
                   data = the_map_data()) %>%
        clearShapes() %>%
        addPolygons(fillColor = ~pal_fun(value),
                    fillOpacity = 0.8,
                    layerId = ~loc_abs_statistical_area_3,
                    stroke = TRUE,
                    color = "grey20",
                    label = paste0("SA3: ", the_map_data()$loc_abs_statistical_area_3, " ", "Crash Count: ", 
                                   the_map_data()$value)) %>%
        clearControls() %>%
        addLegend(pal = pal_fun_leg,
                  values = ~value,
                  position = "bottomright",
                  opacity = 0.8,
                  title = "",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    } else{
      leafletProxy("map",
                   data = the_map_data()) %>%
        clearShapes() %>%
        addPolygons(fillColor = ~pal_fun(value),
                    fillOpacity = 0.8,
                    layerId = ~loc_abs_statistical_area_4,
                    stroke = TRUE,
                    color = "grey20",
                    label = paste0("SA4: ", the_map_data()$loc_abs_statistical_area_4, " ", "Crash Count: ", 
                                   the_map_data()$value)) %>%
        clearControls() %>%
        addLegend(pal = pal_fun_leg,
                  values = ~value,
                  position = "bottomright",
                  opacity = 0.8,
                  title = "",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    }
  })
  
 map_proxy <- leafletProxy("map")
  
}