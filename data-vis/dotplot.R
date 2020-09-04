#------------------------------------------
# This script sets out to graph some
# crash data
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 4 September 2020
#------------------------------------------

#------------------------- PRE PROCESSING --------------------------

#--------
# Monthly
#--------

d2 %>%
  ggplot(aes(x = crash_date, y = counter)) +
  geom_point(colour = "#189AB4", alpha = 0.8) +
  labs(title = "Time series of crash count by severity",
       x = "Date",
       y = "Crashes") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "#05445E"),
        strip.text = element_text(colour = "white", face = "bold")) +
  facet_wrap(~crash_severity)
