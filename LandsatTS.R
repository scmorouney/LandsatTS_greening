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


######### START OF LANDSAT TS CODE  ############



####### SET UP #########

devtools::install_github("logan-berner/LandsatTS", build_vignettes = TRUE)

library(LandsatTS)

# Initialize Google Earth Engine
ee_Initialize(
  project = "fire-locations",
  auth_mode = "localhost"
)

# Check Earth Engine connection
ee_check()

#upload shapefile of all fires desired for landsat extraction - abandoned this because using pixel centres is too much data 
#fires <- st_read("/Users/spensermorouney/Desktop/data/LandsatTS/LandsatTS_greening/drive-download-20261112T223101Z-1-001/AllFireShapefile.shp")
#check variables 
#names(fires)


######### RUN CSV EXPORT FOR DESIRED IMAGERY + LOCATIONS #############

### uploading existing shapefile of random points generated from GEE ##
points <- st_read("/Users/spensermorouney/Desktop/data/LandsatTS/LandsatTS_greening/RandomPoints/Burned_RandomSample_Test2.shp")
points$sample_id <- paste0("S_", 1:nrow(points))
nrow(points)
head(points)

#using built in landsat TS function to export CSV with chunks of 1000 points each
#this exports csvs for entire specified time range over growing season with imagery metadata and band values across all sensors 
task <- lsat_export_ts(
  points,
  max_chunk_size = 1000,
  start_date = "2016-01-01",
  end_date = "2025-12-31", 
  start_doy = 152, # limit to just growing season to speed up export 
  end_doy= 243
)


### PROCESSING ####
chunk1<- read.csv("/Users/spensermorouney/Desktop/data/LandsatTS/LandsatTS_greening/lsatTS_export_chunk_1.csv")
lsat.dt_chunk1 <- lsat_format_data(chunk1)
head(chunk1)

lsat.dt_chunk1  <- lsat_clean_data(lsat.dt_chunk1, geom.max = 15, cloud.max = 80, sza.max = 60, filter.cfmask.snow = T, filter.cfmask.water = T, filter.jrc.water = T)
head(lsat.dt_chunk1 )

###attempted fix......
lsat_neighborhood_mean_fixed <- function(dt) {
  
  dt <- data.table::data.table(dt)
  
  dt <- dt[,list(
    latitude = mean(latitude, na.rm = TRUE),
    longitude = mean(longitude, na.rm = TRUE),
    blue = mean(blue, na.rm = TRUE),
    green = mean(green, na.rm = TRUE),
    red = mean(red, na.rm = TRUE),
    nir = mean(nir, na.rm = TRUE),
    swir1 = mean(swir1, na.rm = TRUE),
    swir2 = mean(swir2, na.rm = TRUE)
  ), by = c("sample.id", "year", "doy", "satellite")]
  
  return(dt)
}


lsat.dt_chunk1 <- lsat_neighborhood_mean_fixed(lsat.dt_chunk1) #### this line is the issue with losing unique sampleID##
head(lsat.dt_chunk1 )

data.summary.dt <- lsat_summarize_data(lsat.dt_chunk1)

lsat.dt_chunk1 <- lsat_calc_spectral_index(lsat.dt_chunk1, si = 'ndvi')

lsat.dt_chunk1 <- lsat_calibrate_rf(lsat.dt_chunk1, 
                             band.or.si = 'ndvi', 
                             doy.rng = 151:242, 
                             min.obs = 5, 
                             frac.train = 0.75, 
                             overwrite.col = T, 
                             write.output = F)

lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt_chunk1, si = 'ndvi', window.yrs = 5, window.min.obs = 10, spl.fit.outfile = F, progress = T)


lsat.gs.dt <- lsat_summarize_growing_seasons(lsat.pheno.dt, si = 'ndvi', min.frac.of.max = 0.75)

lsat.gs.eval.dt <- lsat_evaluate_phenological_max(lsat.pheno.dt, si = 'ndvi', min.obs = 10, reps = 5, min.frac.of.max = 0.75, outdir = NA)

lsat.trnds.dt <- lsat_calc_trend(lsat.gs.dt, si = 'ndvi.max', 2016:2023, sig = 0.05)



############ TEST CODE ########################


#create pixel centre for each pixel within each polygon as sampling point
pixel_list <- fires %>%
       split(.$Fire_ID) %>%
       map(
             lsat_get_pixel_centers,
            pixel_prefix_from = "Fire_ID"
         ) %>%
      bind_rows()

#preview variables and geometry type 
pixel_list <- points

head(pixel_list)

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
 
 

 
 # Check total sampled pixels
 nrow(sampled_pixels)
 
 # Check sample size by fire
 sampled_pixels %>%
   count(Fire_ID, name = "sampled_pixels")
 
 #specify how many pixels to run per chunk for export 
 chunk_size <- 50
 
 #divide dataset into desired chunk size for export 
 pixel_chunks <- split(
   pixel_list,
   ceiling(seq_len(nrow(pixel_list)) / chunk_size)
 )
 
 #number of pixel chunks total 
 length(pixel_chunks)
 
 
 #test export on one chunk (good to estimate total time before starting)
 
 test_chunk <- pixel_chunks[1:5]
 
nrow(test_chunk) #make sure this is the same as chunk_size

test_pixels <- bind_rows(test_chunk)

test_task <- lsat_export_ts(
  test_pixels,
  max_chunk_size = 50,
  start_date = "2016-01-01",
  end_date = "2025-12-31", 
  start_doy = 152, # limit to just growing season to speed up export 
  end_doy= 243
)
 
names(test_task)



####### Randomly sample 50% of pixels within each fire to reduce load#######
set.seed(0)

sampled_pixels <- pixel_list %>%
  group_by(Fire_ID) %>%
  slice_sample(prop = 0.25) %>%
  ungroup()

length(sampled_pixels)

#specify how many pixels to run per chunk for export 
chunk_size <- 500

#divide dataset into desired chunk size for export 
pixel_chunks <- split(
  sampled_pixels,
  ceiling(seq_len(nrow(sampled_pixels)) / chunk_size)
)

#number of pixel chunks total 
length(pixel_chunks)


#test export on one chunk (good to estimate total time before starting)

test_chunk <- pixel_chunks[[1]]

nrow(test_chunk) #make sure this is the same as chunk_size

test_pixels <- bind_rows(test_chunk)

test_task <- lsat_export_ts(
  test_pixels,
  max_chunk_size = 500,
  start_date = "2016-01-01",
  end_date = "2025-12-31", 
  start_doy = 152, # limit to just growing season to speed up export 
  end_doy= 243
)



## LOOKING AT OUTPUT CSVs ## 
Landsat<- read.csv("lsatTS_export_chunk_TEST500.csv")
head(Landsat)



 ##### EE tools for managing exports #####
 
 #check the submitted tasks 
 ee_monitoring()
 #cancel all tasks currently running (good for if stuck in loops)
 ee_manage_cancel_all_running_task()
