#------------------------------------------
# This script sets out to statistically
# model road crash data for minor injuries
# and dive into GAM specification
#------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 15 September 2020
#-------------------------------------------

#------------------------- MODELLING -------------------------------

# Define GAM time series engine

tmp1 <- d2 %>%
  filter(crash_severity == "Minor injury")

# Fit GAM

model <- gam(counter ~ s(crash_month_int, k = 12, bs = "cc") + s(date_num),
             data = tmp1,
             family = poisson,
             method = "REML")

print(summary(model))

# Extract deviance explained

r_sq <- paste0("Deviance explained: ", round((abs(summary(model)$dev.expl))*100, digits = 2), "%")

# Get fitted values

tmp2 <- tmp1 %>%
  mutate(type = "Raw Data") %>%
  dplyr::select(c(counter, type, crash_date, date_num))

tmp3 <- data.frame(counter = model$fitted.values) %>%
  mutate(type = "GAM") %>%
  mutate(crash_date = tmp1$crash_date,
         date_num = tmp1$date_num)

tmp4 <- bind_rows(tmp2, tmp3)

p <- tmp4 %>%
  mutate(type = factor(type, levels = c("Raw Data", "GAM"))) %>%
  ggplot(aes(x = crash_date, y = counter, group = type, colour = type)) +
  geom_line(size = 1.15) +
  labs(title = paste0("Crash severity: ", "Minor injury"),
       subtitle = r_sq,
       x = "Date",
       y = "Number of Crashes",
       colour = "Type",
       caption = "Model is a Poisson-distributed GAM with smoothed monthly seasonality and smoothed trend components.") +
  theme_bw() +
  scale_colour_manual(values = c("#189AB4", "#FD62AD")) +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
print(p)

#--------------------------------
# MANUAL SMOOTH FUNCTION PLOTTING
#--------------------------------

# Predict new values over the same range

ndata <- with(tmp1, tibble(date_num = seq(min(date_num), max(date_num),
                                          length = 96),
                           crash_month_int = as.integer(seq(min(crash_month_int), max(crash_month_int),
                                                            length = 96))))

ndata <- add_column(ndata, fit = predict(model, newdata = ndata, type = 'response'))

# Inverse link function for transformation

ilink <- family(model)$linkinv

# Add fit and se.fit on the **link** scale

ndata <- bind_cols(ndata, setNames(as_tibble(predict(model, ndata, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))

# Create the interval and backtransform

ndata <- mutate(ndata,
                fit_resp  = ilink(fit_link),
                right_upr = ilink(fit_link + (2 * se_link)),
                right_lwr = ilink(fit_link - (2 * se_link)),
                the_mean = (right_upr+right_lwr)/2) %>%
  mutate(type = "GAM")

# Re-plot

p1 <- tmp4 %>%
  filter(type != "GAM") %>%
  ggplot() +
  geom_ribbon(data = ndata,
              aes(x = date_num, ymin = right_lwr, ymax = right_upr, group = type, fill = type),
              alpha = 0.3, fill = "#FD62AD") +
  geom_line(aes(x = date_num, y = counter, colour = type), size = 1.15) +
  geom_line(data = ndata, aes(x = date_num, y = the_mean, colour = type), size = 1.15) +
  labs(title = paste0("Crash severity: ", "Minor injury"),
       subtitle = r_sq,
       x = "Timepoint",
       y = "Number of Crashes",
       colour = "Type") +
  theme_bw() +
  scale_colour_manual(name = NULL, values = c("#FD62AD", "#189AB4"), 
                      labels = c("GAM Model", "Real Data")) +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank()) # Hide this as it's not the real dates and might confuse readers
print(p1)

p1_alt <- tmp4 %>%
  filter(type != "GAM") %>%
  ggplot() +
  geom_ribbon(data = ndata,
              aes(x = date_num, ymin = right_lwr, ymax = right_upr, group = type, fill = type),
              alpha = 0.3, fill = "#FD62AD") +
  geom_point(aes(x = date_num, y = counter, colour = type), size = 3) +
  geom_line(data = ndata, aes(x = date_num, y = the_mean, colour = type), size = 1.15) +
  labs(title = paste0("Crash severity: ", "Minor injury"),
       subtitle = r_sq,
       x = "Timepoint",
       y = "Number of Crashes",
       colour = "Type",
       caption = "Model is a Poisson-distributed GAM with smoothed monthly seasonality and smoothed trend components.") +
  theme_bw() +
  scale_colour_manual(name = NULL, values = c("#FD62AD", "#189AB4"), 
                      labels = c("GAM Model", "Real Data")) +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank()) # Hide this as it's not the real dates and might confuse readers
print(p1_alt)

ggarrange(p1, p1_alt, nrow = 2, ncol = 1)

#--------------------------- CHECK ACF -----------------------------

layout(matrix(1:2, ncol = 2))
acf(resid(model), lag.max = 36, main = "Model residual ACF")
pacf(resid(model), lag.max = 36, main = "Model residual PACF")
layout(1)

# Try GAMM with order 4 autocorrelation included and compare to model without

model_gamm_reg <- gamm(counter ~ s(crash_month_int, k = 12, bs = "cc") + s(date_num),
                       data = tmp1,
                       family = poisson,
                       method = "REML")

model_gamm <- gamm(counter ~ s(crash_month_int, k = 12, bs = "cc") + s(date_num),
                   data = tmp1,
                   correlation = corARMA(form = ~ 1|year, p = 4),
                   family = poisson,
                   method = "REML")

# Compare AIC values

AIC(model_gamm_reg$lme)
AIC(model_gamm$lme)
