
library(envRaster)

# inputs
start_date <- "2013-01-01"
end_date <- "2023-12-31"

## raster defining area of interest
aoi <- terra::rast(xmin = 1450000
                   , ymin = 1710000
                   , xmax = 1455500
                   , ymax = 1715500
                   , resolution = c(50, 50)
                   , crs = "epsg:8059"
                   , vals = 1
                   )

get_sat_data(aoi
             , start_date = start_date
             , end_date = end_date
             , out_dir = tempdir()
             , collections = c("ga_ls9c_ard_3"
                               , "ga_ls8c_ard_3"
                               )
             , period = "P12M"
             , property_filter = function(x) {x[["eo:cloud_cover"]] < 50}
             , layers = c("red", "blue", "green")
             , indices = NULL
             , cores = 30
             ) %>%
  gdalcubes::animate(rgb = 3:1)


