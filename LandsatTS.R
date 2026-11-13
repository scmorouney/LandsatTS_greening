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

#upload shapefile of all fires desired for landsat extraction 
fires <- st_read("/Users/spensermorouney/Desktop/data/LandsatTS/LandsatTS_greening/drive-download-20261112T223101Z-1-001/AllFireShapefile.shp")
#check variables 
names(fires)

#create pixel centre for each pixel within each polygon  
pixel_list <- fires %>%
       split(.$Fire_ID) %>%
       map(
             lsat_get_pixel_centers,
            pixel_prefix_from = "Fire_ID"
         ) %>%
      bind_rows()

#preview variables and geometry type 
pixel_list


#this code automatically created a sample ID based on fire ID - return to original format so can be joined
 pixel_list <- pixel_list %>%
       mutate(
             Fire_ID = sub("_.*$", "", sample_id)
        )
 
 #join the fire variables back to dataset by fire ID
 pixel_list <- pixel_list %>%
     left_join(
             st_drop_geometry(fires),
             by = "Fire_ID"
         )
 
 #specify how many pixels to run per chunk for export 
 chunk_size <- 500
 
 #divide dataset into desired chunk size for export 
 pixel_chunks <- split(
   pixel_list,
   ceiling(seq_len(nrow(pixel_list)) / chunk_size)
 )
 
 #number of pixel chunks total 
 length(pixel_chunks)
 
 
 #test export on one chunk (good to estimate total time before starting)
 
 test_chunk <- pixel_chunks[[1]]
 
nrow(test_chunk) #make sure this is the same as chunk_size


test_task <- lsat_export_ts(
    test_chunk,
    max_chunk_size = 500
    )
 
 
 ##### EE tools for managing exports #####
 
 #check the submitted tasks 
 ee_monitoring()
 #cancel all tasks currently running (good for if stuck in loops)
 ee_manage_cancel_all_running_task()
