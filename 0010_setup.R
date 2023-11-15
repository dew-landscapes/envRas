  
  
  # max_cores-----
  
  max_cores <- 15
  

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
  
  settings$seasons <- make_seasons(settings[["start_year", exact = TRUE]]
                                   , settings[["end_year", exact = TRUE]]
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
                                             , settings[["use_period", exact = TRUE]]
                                             )
                                    , paste(settings[["sat_source", exact = TRUE]]
                                            , paste(settings[["sat_collection", exact = TRUE]], collapse = "--")
                                            , settings[["use_aoi", exact = TRUE]]
                                            , settings[["use_buffer", exact = TRUE]]
                                            , settings[["use_res", exact = TRUE]]
                                            , sep = "__"
                                            )
                                    )
  
  settings$cli_save_dir <- fs::path(data_dir
                                    , "raster"
                                    , paste0("cube"
                                             , "__"
                                             , settings[["use_period", exact = TRUE]]
                                             )
                                    , paste(settings[["cli_source", exact = TRUE]]
                                            , paste(settings[["cli_collection", exact = TRUE]], collapse = "--")
                                            , settings[["use_aoi", exact = TRUE]]
                                            , settings[["use_buffer", exact = TRUE]]
                                            , settings[["use_res", exact = TRUE]]
                                            , sep = "__"
                                            )
                                    )
  
  settings$munged_dir <- fs::path("D:"
                         , "env"
                         , "data"
                         , "raster"
                         , "aligned"
                         , paste(settings[["use_aoi", exact = TRUE]]
                                 , settings[["use_buffer", exact = TRUE]]
                                 , settings[["use_res", exact = TRUE]]
                                 , sep = "__"
                                 )
                         )
  
  fs::dir_create(settings[["munged_dir", exact = TRUE]])
  
  
  # maps-------
  
  # sa
  sa <- sfarrow::st_read_parquet(fs::path(data_dir
                                          , "vector"
                                          , "sa.parquet"
                                          )
                                 )
  
  # what aoi to use
  lay <- sfarrow::st_read_parquet(fs::path(data_dir
                                           , "vector"
                                           , paste0(settings[["layer", exact = TRUE]]
                                                    , ".parquet"
                                                    )
                                           )
                                  )
    
  settings$boundary <- make_aoi(layer = lay
                           , filt_col = settings[["filt_col", exact = TRUE]]
                           , level = settings[["use_aoi", exact = TRUE]]
                           , buffer = settings[["use_buffer", exact = TRUE]]
                           , bbox = settings[["use_bbox", exact = TRUE]]
                           , clip = settings[["use_clip", exact = TRUE]]
                           , clip_buf = settings[["use_clip_buffer", exact = TRUE]]
                           )
  
  if(FALSE) tmap::tm_shape(settings[["boundary", exact = TRUE]]) + tmap::tm_polygons()
  
  
  # bboxes-------
  
  # geographic
  settings$bbox <- sf::st_bbox(settings[["boundary", exact = TRUE]])
    
  # projected
  settings$bbox_use_epsg <- settings[["boundary", exact = TRUE]] %>%
    sf::st_transform(crs = settings[["epsg_proj", exact = TRUE]]) %>%
    sf::st_bbox()

  bbox_adj <- settings[["use_res", exact = TRUE]] * ceiling(100 / settings$use_res)
  
  settings$use_extent <- list(left = round(floor(settings[["bbox_use_epsg", exact = TRUE]]["xmin"][[1]]), -2) - bbox_adj
                              , right = round(ceiling(settings[["bbox_use_epsg", exact = TRUE]]["xmax"][[1]]), -2) + bbox_adj
                              , top = round(ceiling(settings[["bbox_use_epsg", exact = TRUE]]["ymax"][[1]]), -2) + bbox_adj
                              , bottom = round(floor(settings[["bbox_use_epsg", exact = TRUE]]["ymin"][[1]]), -2) - bbox_adj
                              )
  
  
  # save-------
  rio::export(settings
              , fs::path(settings[["munged_dir", exact = TRUE]]
                         , "settings.rds"
                         )
              )
                           
  # seasons-------
  
  stacks <- settings[["seasons", exact = TRUE]]$months %>%
    dplyr::filter(season %in% c("summer", "autumn")) %>%
    dplyr::group_by(year_use, season) %>%
    dplyr::summarise(start_date = min(start_date)
                     , end_date = max(end_date)
                     ) %>%
    dplyr::ungroup() 
  