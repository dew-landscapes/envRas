
library(magrittr)
library(terra)
library(gdalcubes)
library(rstac)
library(sf)
library(fs)

# inputs
start_date <- "2020-03-01"
end_date <- "2020-05-31"

## raster defining area of interest
aoi <- terra::rast(xmin = 1443500
                   , ymin = 1699000
                   , xmax = 1461500
                   , ymax = 1726990
                   , resolution = c(30, 30)
                   , crs = "epsg:8059"
                   )

# bbox from raster
bbox <- sf::st_bbox(aoi)

# decimal degress bbox for stac search
bbox_latlong <- bbox %>%
  sf::st_as_sfc() %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_bbox()



# find items in Digital Earth Australia collections
items <- rstac::stac("https://explorer.sandbox.dea.ga.gov.au/stac") %>%
  rstac::stac_search(collections = c("ga_s2am_ard_3", "ga_s2bm_ard_3")
                     , bbox = bbox_latlong
                     , datetime = paste0(as.character(start_date)
                                         , "/"
                                         , as.character(end_date)
                                         )
                     ) %>%
  rstac::get_request() %>%
  rstac::items_fetch()

# Create collection of blue and cloud mask
col <- gdalcubes::stac_image_collection(items$features
                                        , asset_names = c("nbart_blue", "oa_fmask")
                                        , property_filter = function(x) {x[["eo:cloud_cover"]] < 10}
                                        )
        
# Setup regular cube
  # only one 3-month time slice in 'cube'
use_extent <- c(as.list(bbox)
                , t0 = as.character(start_date)
                , t1 = as.character(end_date)
                )

names(use_extent)[1:4] <- c("left", "bottom", "right", "top")

v_num <- gdalcubes::cube_view(srs = "EPSG:8059"
                              , extent = use_extent
                              , dx = terra::res(aoi)[1]
                              , dy = terra::res(aoi)[2]
                              , dt = "P3M"
                              , aggregation = "median"
                              , resampling = "bilinear"
                              )

cloud_mask <- gdalcubes::image_mask(band = "oa_fmask"
                                    , values = c(2, 3)
                                    )

# Bring together collection and regular cube
r <- gdalcubes::raster_cube(col
                            , v_num
                            , mask = cloud_mask
                            ) %>%
  gdalcubes::select_bands("nbart_blue")

# Retrieve and save the same cube 5 times
purrr::walk(1:5
            , \(x) {
              
              gdalcubes::write_tif(r
                                   , dir = tempdir()
                                   , prefix = paste0("attempt_", x)
                                   , pack = gdalcubes::pack_minmax(type = "int16"
                                                                   , min = 0
                                                                   , max = 10000
                                                                   )
                                   , creation_options = list("COMPRESS" = "NONE")
                                   )
             
             
            }
            )

# Plot the results from each of the 5 saves
  # some will have a chunk or two missing IN DIFFERENT PLACES
fs::dir_ls(tempdir()
           , regexp = "attempt.*tif$"
           ) %>%
  terra::rast() %>%
  terra::plot()


