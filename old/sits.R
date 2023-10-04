
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
          , "sits"
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
  
  tmap_mode("view")
  
  
  # settings------
  
  settings <- list(use_epsg = 7845
                   , target_res = 10
                   , sample_n = 9999
                   )
  
  settings$use_source <- "MPC"
  
  settings$use_collection <- "LANDSAT-C2-L2"
  
  settings$layer <- "lsa"
  
  settings$filt_col <- "LSA"
  
  settings$use_aoi <- "KI"
  
  settings$use_bbox <- TRUE
  
  settings$use_buffer <- 5000
  
  settings$use_period <- "P1M"
  
  settings$use_res <- 30
  
  settings$start_year <- 2015
  settings$end_year <- 2022
  
  settings$seasons <- make_seasons(settings$start_year
                                   , settings$end_year
                                   )
    
  settings$use_cores <- 4
  
  
  # directories------
  
  data_dir <- fs::path("D:"
                       , "env"
                       , "data"
                       )
  
  ras_save_dir <- fs::path("out"
                           , paste(settings$use_source
                                   , settings$use_collection
                                   , settings$use_aoi
                                   , settings$use_buffer
                                   , settings$use_period
                                   , settings$use_res
                                   , sep = "_"
                                   )
                           )
  
  fs::dir_create(ras_save_dir)
  
  
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
  
  
  # irregular cubes------
  
  cubes <- settings$seasons$months %>%
    dplyr::group_by(year_use) %>%
    dplyr::summarise(start_date = min(start_date)
                     , end_date = max(end_date)
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(out_dir = fs::path(ras_save_dir
                                     , year_use
                                     )
                  , cube = purrr::pmap(list(out_dir = out_dir
                                            , start = start_date
                                            , end = end_date
                                            )
                                       , get_sat_data
                                       , settings = settings
                                       , force_new = FALSE
                                       , fid_regex = "LC08"
                                       )
                  )
  
  
  # regular cubes-------
  
  reg_cubes <- cubes %>%
    dplyr::mutate(out_reg = fs::path(out_dir, "reg")
                  , reg_cube = purrr::map2(cube
                                           , out_reg
                                           , make_cube
                                           , settings = settings
                                           )
                  )
    
  
  if(FALSE){
    
    if(FALSE) {
      
      # clean up 1kb files (download errors?)
      small_tif <- fs::dir_info(ras_save_dir
                                , recurse = TRUE
                                , regexp = "tif$"
                                , type = "file"
                                ) %>%
        dplyr::filter(size < "10KB")
      
      fs::file_delete(small_tif$path)
    
    }
    
    # load data if cubes is still processing
    temp <- fs::dir_info(ras_save_dir
                         , recurse = TRUE
                         , regexp = "tif$"
                         , type = "file"
                         ) %>%
      dplyr::select(path, size) %>%
      dplyr::mutate(file = gsub("\\.tif", "", basename(path))) %>%
      tidyr::separate(file
                      , into = c("source", "collection", "scene", "band", "date")
                      , sep = "_"
                      , remove = FALSE
                      ) %>%
      dplyr::mutate(date = as.Date(date))
    
    
    plot(terra::rast(temp$path[[1]]))
    
  }
  
  
  # mung ----------
  # outside of sits framework
    
  epoch_seasons <- envFunc::make_epochs(start_year = settings$start_year
                                        , end_year = settings$end_year
                                        , epoch_step = 10
                                        , epoch_overlap = FALSE
                                        ) %>%
    tidyr::unnest(cols = c(years)) %>%
    dplyr::rename(year = years) %>%
    dplyr::select(year, epoch) %>%
    dplyr::left_join(settings$seasons$seasons) %>%
    dplyr::group_by(epoch, season) %>%
    dplyr::summarise(start_date = min(start_date)
                     , end_date = max(end_date)
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = purrr::map2(start_date, end_date, seq, "days")) %>%
    tidyr::unnest(cols = c(date)) %>%
    dplyr::left_join(temp) %>%
    dplyr::filter(!is.na(path)) %>%
    tidyr::nest(data = -c(source, collection, scene, band, epoch, season, start_date, end_date))
  
  qs <- c(0.05, 0.50, 0.95)
  
  raw <- epoch_seasons %>%
    tidyr::u
    tidyr::nest(data = -c(source, collection, satellite, sensor, band, season)) %>%
    dplyr::mutate(data = purrr::map(data
                                    , ~ terra::rast(.$path)
                                    )
                  ) %>%
    tidyr::unite("out_rast", !c(data), remove = FALSE) %>%
    dplyr::mutate(out_rast = fs::path(ras_aligned_dir
                                     , paste0(out_rast
                                              , ".tif"
                                              )
                                     )
                  , done = file.exists(out_rast)
                  )
  
  purrr::walk2(raw$data[!raw$done]
               , raw$out_rast[!raw$done]
               , ~terra::app(.x
                             , fun = stats::quantile
                             , na.rm = TRUE
                             , q = qs
                             , filename = .y
                             , overwrite = TRUE
                             , cores = settings$use_cores
                             )
               )
    
  
  if(FALSE) {
    
    library(tidyterra)
    library(ggplot2)
    
    r <- terra::rast(raw$out_rast[[1]])
    
    terra::plot(temp)
    
    # Plot a single image
    
    reg_cube %>%
      dplyr::filter(tile == reg_cube$tile[1]) %>%
      plot(red = "RED", green = "GREEN", blue = "BLUE")
    
    reg_cube %>%
      dplyr::filter(tile == reg_cube$tile[2]) %>%
      dplyr::pull(file_info) %>%
      `[[`(1) %>%
      dplyr::filter(band == "NDVI") %>%
      dplyr::slice(3) %>%
      dplyr::pull(path) %>%
      terra::rast() %>%
      terra::plot()
    
    # plot the cube
    plot(reg_cube
         , band = "NDMI"
         , date = sits_timeline(reg_cube)[2]
         )
    
  }
  