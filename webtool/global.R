
library(shiny)
library(shinyjs)
library(tidyverse)
library(stringr)
library(data.table)
library(shinyWidgets)
library(scales)
library(readxl)
library(janitor)
library(shinycssloaders)
library(mgcv)
library(sjPlot)
library(plotly)

# Load HTML files

import_files <- list.files("imports", full.names = TRUE, pattern = "\\.html")
for(f in import_files){
  object_name <- gsub("imports/", "", f)
  object_name <- gsub("\\.html", "", object_name)
  assign(object_name, readLines(f, warn = FALSE))
}

# Load R helper functions

r_files <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]")
for(f in r_files){
  source(f)
}

# Load data

d <- read_csv("data/locations.csv") %>%
  clean_names() %>%
  filter(crash_year > 2010)

# Run cleaning functions

d1 <- ts_cleaner(d)
d2 <- model_cleaner(d)

# Define tab names

navtab0 <- "STATISTICAL MODELLING"
navtab1 <- "ABOUT"

# Define lists for user inputs

severities <- unique(d1$crash_severity)
years <- unique(d2$year)
