#------------------------------------------
# This script sets out to build a Bayesian
# model on crash data
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 4 September 2020
#------------------------------------------

#------------------------- MODELLING -------------------------------

# Define GAM time series engine

gam_engine <- function(data,severity){

  tmp1 <- data %>%
    filter(crash_severity == severity)
  
  # Fit GAM
  
  model <- gam(counter ~ s(crash_month_int, year),
               data = tmp1,
               family = poisson)
  
  # Extract R squared for use in graph
  
  r_sq <- paste0("R squared: ",round((abs(summary(model)$r.sq))*100, digits = 2),"%")
  
  # Get fitted values
  
  tmp2 <- tmp1 %>%
    mutate(type = "Raw Data") %>%
    dplyr::select(c(counter, type, crash_date))
  
  tmp3 <- data.frame(counter = model$fitted.values) %>%
    mutate(type = "GAM") %>%
    mutate(crash_date = tmp1$crash_date)
  
  tmp4 <- bind_rows(tmp2, tmp3)
  
  p <- tmp4 %>%
    mutate(type = factor(type, levels = c("Raw Data", "GAM"))) %>%
    ggplot(aes(x = crash_date, y = counter, group = type, colour = type)) +
    geom_line(size = 0.8) +
    theme_bw() +
    labs(title = paste0("Crash severity: ", severity),
         subtitle = r_sq,
         x = "Date",
         y = "Crashes",
         colour = "Type") +
    scale_colour_manual(values = c("#189AB4", "#FD62AD")) +
    theme(legend.position = "bottom")
  
  return(p)
}

#------------------------- VISUALISATION ---------------------------

#----------------------
# Apply the function
#----------------------

p_f <- gam_engine(d2, "Fatal")
p_h <- gam_engine(d2, "Hospitalisation")
p_m <- gam_engine(d2, "Medical treatment")
p_i <- gam_engine(d2, "Minor injury")

#----------------------
# Wrap all into 1 image
#---------------------

footer <- "Model is a Poisson-distributed GAM with smoothed seasonal terms for year and month."
footer_format <- ggparagraph(text = footer, face = "italic", size = 11, color = "black")

main_plots <- ggarrange(p_f, p_h, p_m, p_i,
          nrow = 2, ncol = 2)

footer_text <- ggarrange(footer_format, nrow = 1, ncol = 1)

#------------------------- EXPORTS ---------------------------------

CairoPNG("output/gam-engine.png", 800, 600)
ggarrange(main_plots, footer_text, ncol = 1, nrow = 2,
          heights = c(1, 0.3))
dev.off()
