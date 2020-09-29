
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
library(broom)
library(sf)
library(leaflet)

# Turn off scientific notation

options(scipen = 999)

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

d_postcodes <- read_excel("data/postcodes.xlsx") %>%
  clean_names() %>%
  filter(ra_name_2016 != "NULL") %>%
  filter(usual_resident_population != "NULL") %>%
  filter(educ_occ_score != "NULL")  %>%
  mutate(usual_resident_population = as.numeric(usual_resident_population),
         educ_occ_score = as.numeric(educ_occ_score)) %>%
  mutate(usual_resident_population = log(usual_resident_population))

# Load geographies

load("data/postcode_geometries.Rda")

# Run cleaning functions

d1 <- ts_cleaner(d)
d2 <- model_cleaner(d, d_postcodes)
d3 <- map_cleaner(d)

post_1 <- postcode_cleaner(posts)

# Define tab names

navtab0 <- "STATISTICAL MODELLING"
navtab1 <- "ABOUT"
navtab2 <- "GEOSPATIAL MAPPING"

# Define lists for user inputs

severities <- unique(d1$crash_severity)
years <- unique(d2$year)
