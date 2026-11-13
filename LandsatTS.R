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

fires <- st_read("/Users/spensermorouney/Desktop/data/LandsatTS/LandsatTS_greening/drive-download-20261112T223101Z-1-001/AllFireShapefile.shp")
names(fires)

pixel_list <- fires %>%
       split(.$Fire_ID) %>%
       map(
             lsat_get_pixel_centers,
            pixel_prefix_from = "Fire_ID"
         ) %>%
      bind_rows()

pixel_list

 pixel_list <- pixel_list %>%
       mutate(
             Fire_ID = sub("_.*$", "", sample_id)
        )
 
 pixel_list <- pixel_list %>%
     left_join(
             st_drop_geometry(fires),
             by = "Fire_ID"
         )
 
 chunk_size <- 500
 
 pixel_chunks <- split(
   pixel_list,
   ceiling(seq_len(nrow(pixel_list)) / chunk_size)
 )
 
 length(pixel_chunks)
 
 
 
 

