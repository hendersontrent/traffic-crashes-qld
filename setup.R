#------------------------------------------
# This script sets out to load all 
# things required for the project
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 4 September 2020
#------------------------------------------

# Load packages

library(tidyverse)
library(readxl)
library(janitor)
library(scales)
library(Cairo)
library(rstan)
library(data.table)
library(ggpubr)
library(mgcv)

# Turn off scientific notation

options(scipen = 999)

# Create an output folder if none exists:

if(!dir.exists('output')) dir.create('output')

# Run processing script to prep data for Bayesian modelling

r_files <- list.files("processing", full.names = TRUE, pattern = "\\.[Rr]")
for(f in r_files){
  source(f)
}
