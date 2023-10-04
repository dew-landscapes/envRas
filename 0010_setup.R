  
  
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
  
  
  # seasons-------
  
  settings$seasons <- make_seasons(settings$start_year
                                   , settings$end_year
                                   )
  
  
  # directories------
  
  data_dir <- fs::path("D:"
                       , "env"
                       , "data"
                       )
  
  settings$sat_save_dir <- fs::path(data_dir
                                    , "raster"
                                    , paste0("cube"
                                             , "__"
                                             , settings$use_period
                                             )
                                    , paste(settings$sat_source
                                            , paste(settings$sat_collection, collapse = "--")
                                            , settings$use_aoi
                                            , settings$use_buffer
                                            , settings$use_res
                                            , sep = "__"
                                            )
                                    )
  
  settings$cli_save_dir <- fs::path(data_dir
                                    , "raster"
                                    , paste0("cube"
                                             , "__"
                                             , settings$use_period
                                             )
                                    , paste(settings$cli_source
                                            , paste(settings$cli_collection, collapse = "--")
                                            , settings$use_aoi
                                            , settings$use_buffer
                                            , settings$use_res
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
  
  
  # bboxes-------
  
  settings$bbox <- sf::st_bbox(settings$boundary)
    
  settings$bbox_use_epsg <- settings$boundary %>%
    sf::st_transform(crs = settings$use_epsg) %>%
    sf::st_bbox()

  settings$bbox_adj <- settings$use_res * ceiling(100 / settings$use_res)
  
  settings$use_extent <- list(left = round(floor(settings$bbox_use_epsg["xmin"][[1]]), -2) - settings$bbox_adj
                              , right = round(ceiling(settings$bbox_use_epsg["xmax"][[1]]), -2) + settings$bbox_adj
                              , top = round(ceiling(settings$bbox_use_epsg["ymax"][[1]]), -2) + settings$bbox_adj
                              , bottom = round(floor(settings$bbox_use_epsg["ymin"][[1]]), -2) - settings$bbox_adj
                              )  
  
  rio::export(settings
              , fs::path(settings$munged_dir
                         , "settings.rds"
                         )
              )
  
  
  # seasons-------
  
  stacks <- settings$seasons$months %>%
    dplyr::filter(season %in% c("summer", "autumn")) %>%
    dplyr::group_by(year_use, season) %>%
    dplyr::summarise(start_date = min(start_date)
                     , end_date = max(end_date)
                     ) %>%
    dplyr::ungroup() 
  