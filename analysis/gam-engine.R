#------------------------------------------
# This script sets out to statistically
# model road crash data for minor injuries
#
# NOTE: This script requires setup.R to
# have been run first
#------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 15 September 2020
#-------------------------------------------

#------------------------- MODELLING -------------------------------

# Define GAM time series engine

tmp1 <- d2 %>%
  filter(crash_severity == "Minor injury")

# Plot raw time series

p <- tmp1 %>%
  ggplot(aes(x = crash_date, y = counter)) +
  geom_line(size = 1.15, colour = "#189AB4") +
  labs(title = "Crash severity: Minor injury",
       x = "Date",
       y = "Number of Crashes") +
  theme_bw() +
  scale_y_continuous(labels = comma) +
  theme(panel.grid.minor = element_blank())
print(p)

# Fit GAM

model <- gam(counter ~ s(crash_month_int, k = 12, bs = "cc") + s(date_num),
             data = tmp1,
             family = poisson,
             method = "REML")

summary(model)

#--------------------------- CHECK ACF -----------------------------

layout(matrix(1:2, ncol = 2))
acf(resid(model), lag.max = 36, main = "Model residual ACF")
pacf(resid(model), lag.max = 36, main = "Model residual PACF")
layout(1)
