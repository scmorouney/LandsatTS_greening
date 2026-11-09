library(dplyr)
library(tidyverse)
library(ggplot)
library(devtools)
library(usethis)
library(rgee)
library(remotes)
library(reticulate)
library(googledrive)
library(sf)
library(purrr)
library(data.table)
library(stringr)
install.packages("terra", type = "binary")

# working python environment: /Users/spensermorouney/.virtualenvs/rgee/bin/python


##START OF LANDSAT TS CODE

##creating to try out the LandsatTS method for tracking greening

devtools::install_github("logan-berner/LandsatTS", build_vignettes = TRUE)

library(LandsatTS)

# Initialize Google Earth Engine
ee_Initialize(
  project = "fire-locations",
  auth_mode = "localhost"
)

# Check Earth Engine connection
ee_check()

