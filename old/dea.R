  
  
  # max_cores-----
  
  max_cores <- 8
  

  # packages------
  
  packages <- 
    sort(
      unique(
        c("base"
          
          # tidyverse
          , "dplyr"
          , "tidyr"
          , "purrr"
          , "ggplot2"
          , "tibble"
          , "readr"
          , "forcats"
          , "stringr"
          , "lubridate"
          
          # misc
          , "fs"
          , "ggridges"
          
          # gis
          , "rstac"
          , "gdalcubes"
          , "terra"
          , "sf"
          , "tmap"
          , "tidyterra"
          
          # env
          , "envRaster"
          , "envFunc"
        )
      )
    )
  
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(new_packages)) install.packages(new_packages)
  
  purrr::walk(packages
              , library
              , character.only = TRUE
              )
  
  
  # functions------
  
  purrr::walk(fs::dir_ls("function")
              , source
              )
  
  
  # options-------
  
  gdalcubes_options(parallel = max_cores) 
  
  tmap_mode("view")
  
  # Sys.setenv(AWS_NO_SIGN_REQUEST = "YES")
  
  # see https://gdalcubes.github.io/source/concepts/config.html
  gdalcubes_set_gdal_config("VSI_CACHE", "TRUE")
  gdalcubes_set_gdal_config("GDAL_CACHEMAX","30%")
  gdalcubes_set_gdal_config("VSI_CACHE_SIZE","100000000")
  gdalcubes_set_gdal_config("GDAL_HTTP_MULTIPLEX","YES")
  gdalcubes_set_gdal_config("GDAL_INGESTED_BYTES_AT_OPEN","32000")
  gdalcubes_set_gdal_config("GDAL_DISABLE_READDIR_ON_OPEN","EMPTY_DIR")
  gdalcubes_set_gdal_config("GDAL_HTTP_VERSION","2")
  gdalcubes_set_gdal_config("GDAL_HTTP_MERGE_CONSECUTIVE_RANGES","YES")
  
  
  # settings------
  
  settings <- list(use_epsg = 7845
                   , use_res = 30
                   , sample_n = 9999
                   )
  
  settings$use_source <- "DEA"
  
  settings$use_collection <- "ga_ls8c_ard_3"
  
  settings$use_period <- "P3M"
  
  settings$layer <- "lsa"
  
  settings$filt_col <- "LSA"
  
  settings$use_aoi <- "KI"
  
  settings$use_bbox <- TRUE
  
  settings$use_buffer <- 5000
  
  settings$start_year <- 2015
  settings$end_year <- 2022
  
  settings$seasons <- make_seasons(settings$start_year
                                   , settings$end_year
                                   )
  
  
  # directories------
  
  data_dir <- fs::path("D:"
                       , "env"
                       , "data"
                       )
  
  settings$ras_save_dir <- fs::path("out"
                                    , paste(settings$use_source
                                            , settings$use_collection
                                            , settings$use_aoi
                                            , settings$use_buffer
                                            , settings$use_res
                                            , settings$use_period
                                            , sep = "__"
                                            )
                                    )
  
  settings$munged_dir <- fs::path("D:"
                         , "env"
                         , "data"
                         , "raster"
                         , "aligned"
                         , paste(settings$use_aoi
                                 , settings$use_buffer
                                 , settings$use_res
                                 , sep = "__"
                                 )
                         )
  
  fs::dir_create(c(settings$ras_save_dir
                   , settings$munged_dir
                   )
                 )
  
  
  # boundary-------
  
  settings$boundary <- make_aoi(layer = sfarrow::st_read_parquet(fs::path(data_dir
                                                                          , "vector"
                                                                          , paste0(settings$layer
                                                                                   , ".parquet"
                                                                                   )
                                                                          )
                                                                 )
                                , filt_col = settings$filt_col
                                , level = settings$use_aoi
                                , buffer = settings$use_buffer
                                , bbox = TRUE
                                )
  
  if(FALSE) tm_shape(settings$boundary) + tm_polygons()
  
  
  # seasons-------
  
  stacks <- settings$seasons$months %>%
    dplyr::filter(season %in% c("summer", "autumn")) %>%
    dplyr::group_by(year_use, season) %>%
    dplyr::summarise(start_date = min(start_date)
                     , end_date = max(end_date)
                     ) %>%
    dplyr::ungroup() 
  
  
  # get data-------
    
  purrr::walk2(stacks$start_date
               , stacks$end_date
               , make_sat_data
               , settings = settings
               , force_new = F
               )
  
  
  # test satellite data-------
  
  results <- fs::dir_info(settings$ras_save_dir
                          , regexp = "tif$"
                          ) %>%
    dplyr::arrange(desc(modification_time)) %>%
    dplyr::mutate(name = gsub("\\.tif", "", basename(path))
                  , name2 = basename(dirname(path))
                  ) %>%
    dplyr::select(path, name, name2) %>%
    tidyr::separate(name, into = c("layer", "start_date"), sep = "__") %>%
    tidyr::separate(name2,into = c("source", "collection", "aoi", "buffer", "res", "period")
                    , sep = "__"
                    ) %>%
    dplyr::mutate(start_date = as.Date(start_date))
  
  temp <- results %>%
    #dplyr::filter(grepl("count|water", path)) %>%
    dplyr::slice(1:9) %>%
    dplyr::pull(path) %>%
    terra::rast()
  
  terra::plot(temp)
  
  
  if(FALSE) {
    
    plot(temp, key.pos = 1, col = viridis::viridis, nbreaks = 10)
    
    # animate-------
    
    library(colorspace)
    
    ndvi.col = function(n) {
      
      rev(sequential_hcl(n, "Green-Yellow"))
      
    }
    
    v <- v %>%
      cube_view(dt = "P1M"
                , aggregation = "median"
                , resampling = "bilinear"
                )
    
    gdalcubes::raster_cube(col
                           , v
                           ) %>%
      gdalcubes::apply_pixel("(nbart_nir-nbart_red)/(nbart_nir+nbart_red)", "NDVI") %>%
      gdalcubes::animate(col = ndvi.col
                         , zlim = c(-0.5, 1)
                         , key.pos = 1
                         , save_as = "anim.gif"
                         , fps = 4
                         )
  
  }
  
  
  
  
  