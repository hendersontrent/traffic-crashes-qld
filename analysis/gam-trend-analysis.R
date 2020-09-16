#------------------------------------------
# This script sets out to compute periods
# of significant change using derivatives
# for the minor injury GAM model
#
# NOTE: This script requires setup.R to
# have been run first
#------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 16 September 2020
#-------------------------------------------

#------------------------- MODELLING -------------------------------

# Define GAM time series engine

tmp1 <- d2 %>%
  filter(crash_severity == "Minor injury")

# Fit GAMs

model_gamm <- gamm(counter ~ s(crash_month_int, k = 12, bs = "cc") + s(date_num),
                   data = tmp1,
                   correlation = corARMA(form = ~ 1|year, p = 4),
                   family = poisson,
                   method = "REML")

model_gamm_6 <- gamm(counter ~ s(crash_month_int, k = 12, bs = "cc") + s(date_num),
                   data = tmp1,
                   correlation = corARMA(form = ~ 1|year, p = 6),
                   family = poisson,
                   method = "REML")

model_gamm_reg <- gamm(counter ~ s(crash_month_int, k = 12, bs = "cc") + s(date_num),
                       data = tmp1,
                       family = poisson,
                       method = "REML")

#------------------------- ANALYSIS --------------------------------

# Write a function to extract pointwise confidence intervals

grab_cis <- function(mod,the_length){
  want <- seq(1, nrow(tmp1), length.out = the_length)
  
  pdat <- with(tmp1,
               data.frame(crash_month_int = crash_month_int[want], date_num = date_num[want], year = year[want]))
  
  p2 <- predict(mod$gam, newdata = pdat, type = "terms", se.fit = TRUE)
  
  pdat <- transform(pdat, p2 = p2$fit[,2], se2 = p2$se.fit[,2])
  
  df.res <- df.residual(mod$gam)
  
  crit.t <- qt(0.025, df.res, lower.tail = FALSE)
  
  pdat <- pdat %>%
    mutate(upper = p2 + (crit.t * se2),
           lower = p2 - (crit.t * se2))
  
  return(pdat)
}

# Retrieve values

ci_ar_4 <- grab_cis(model_gamm, 96) %>%
  mutate(model = "GAMM: AR(4) Errors")

ci_ar_6 <- grab_cis(model_gamm_6, 96) %>%
  mutate(model = "GAMM: AR(6) Errors")

ci_unc <- grab_cis(model_gamm_reg, 96) %>%
  mutate(model = "GAMM: Uncorrelated Errors")

all_models <- bind_rows(ci_ar_4, ci_ar_6, ci_unc)

#------------------------- DATA VISUALISATION ----------------------

the_palette <- c("GAMM: Uncorrelated Errors" = "#189AB4",
                 "GAMM: AR(6) Errors" = "#FEB06A",
                 "GAMM: AR(4) Errors" = "#FD62AD")

comparison_plot <- all_models %>%
  ggplot() +
  geom_ribbon(data = all_models,
              aes(x = date_num, ymin = lower, ymax = upper, fill = model),
              alpha = 0.3) +
  geom_line(aes(x = date_num, y = p2, colour = model), size = 1) +
  labs(title = "Model fitted trend comparison",
       subtitle = "Shading indicates pointwise 95% confidence interval",
       x = "Timepoint",
       y = "Crashes (Centred)",
       colour = "Model",
       caption = "Source: Department of Transport and Main Roads. Analysis: Orbisant Analytics.") +
  theme_bw() +
  scale_colour_manual(values = the_palette) +
  scale_fill_manual(values = the_palette, guide = FALSE) +  
  scale_y_continuous(labels = function(x) round(x, digits = 2)) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank()) # Hide this as it's not the real dates and might confuse readers
print(comparison_plot)

#------------------------- DERIVATIVES -----------------------------

# Load function from Gavin Simpson

tmpf <- tempfile()
download.file("https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30/raw/82118ee30c9ef1254795d2ec6d356a664cc138ab/Deriv.R",
              tmpf)
source(tmpf)
ls()

# Compute derivative

Term <- "date_num"
m2.d <- Deriv(model_gamm, n = 96)
m2.dci <- confint(m2.d, term = Term)
m2.dsig <- signifD(ci_ar_4$p2, d = m2.d[[Term]]$deriv, m2.dci[[Term]]$upper, m2.dci[[Term]]$lower)

# Bring everything into a tidy format for plotting

sig_data <- data.frame(date_num = ci_ar_4$date_num,
                       incr = unlist(m2.dsig$incr),
                       decr = unlist(m2.dsig$decr))

CairoPNG("output/deriv-ar4.png", 800, 600)
deriv_plot <- ci_ar_4 %>%
  ggplot() +
  geom_ribbon(data = ci_ar_4,
              aes(x = date_num, ymin = lower, ymax = upper),
              alpha = 0.3, fill = "steelblue2") +
  geom_line(aes(x = date_num, y = p2), colour = "black", size = 0.8) +
  geom_line(data = sig_data, aes(x = date_num, y = incr), colour = "#75E6DA", size = 1.5) +
  geom_line(data = sig_data, aes(x = date_num, y = decr), colour = "#FD62AD", size = 1.5) +
  labs(title = "GAMM AR(4) trend component derivative",
       subtitle = "Thick coloured lines indicate statistically significant periods of change. Shading indicates pointwise 95% confidence interval",
       x = "Timepoint",
       y = "Crashes (Centred)",
       colour = "Model",
       caption = "Source: Department of Transport and Main Roads. Analysis: Orbisant Analytics.") +
  theme_bw() +
  scale_y_continuous(labels = function(x) round(x, digits = 2)) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank()) # Hide this as it's not the real dates and might confuse readers
print(deriv_plot)
dev.off()
