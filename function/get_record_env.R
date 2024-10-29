
  get_record_env <- function(base_path
                             , lat
                             , long
                             , crs_data = 4283
                             , dist_m = NULL # will use half resolution of base if NULL
                             , date
                             , log = NULL
                             , cores = 1
                             , use_base = FALSE
                             , obs_period = "P12M"
                             , extract_args = list(FUN = median
                                                   , merge = FALSE
                                                   , drop_geom = FALSE
                                                   , reduce_time = FALSE
                                                   )
                             , ... # get_sat_data args
                             ) {
    
    # setup ------
    dots <- list(...)
    attempts <- dots$attempts
    
    gdalcubes::gdalcubes_set_gdal_config("GDAL_NUM_THREADS", as.character(cores))
    
    if(!is.null(log)) {
      
      start_time <- Sys.time()
      
      this_cube <- paste0(dist_m
                          , " m__"
                          , date
                          , "__("
                          , round(lat, 3)
                          , ","
                          , round(long, 3)
                          , ")"
                          )
      
      log_text <- paste0(this_cube, ": ", format(start_time, "%a %b %d %X %Y\n"))
      
    }
    
    # base ------
    base <- terra::rast(base_path)
    
    if(!is.null(log)) {
      
      log_text <- paste0(log_text
                         , "load base raster took "
                         , round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 2)
                         , " minutes\n"
                         )
      
      start_time <- Sys.time()
      
    }
    
    # dist_m --------
    
    if(is.null(dist_m)) dist_m <- terra::res(base)[[1]] / 2
    
    # poly and x -------
    rec_row <- tibble::tibble(lat = lat, long = long, dist_m = dist_m, date = date)
    
    ## polygon -------
    poly <- sf::st_as_sf(rec_row
                         , coords = c("long", "lat")
                         , crs = crs_data
                         , remove = FALSE
                         ) %>%
      sf::st_transform(crs = sf::st_crs(base)) %>%
      sf::st_buffer(dist_m / 2)
    
    ## x ---------
    if(use_base) {
      
      x <- base %>%
        terra::mask(poly) %>%
        terra::trim()
      
    } else {
      
      x <- terra::rast(extent = sf::st_bbox(poly)
                  , resolution = terra::res(base)
                  , crs = terra::crs(base)
                  , vals = 1
                  ) %>%
        terra::mask(mask = terra::vect(poly))
      
    }
    
    if(!is.null(log)) {
      
      log_text <- paste0(log_text
                         , "make point poly and raster for virtual cube took "
                         , round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 2)
                         , " minutes\n"
                         )
      
      start_time <- Sys.time()
      
    }
    
    if(FALSE) {
      
      # plot poly and x
      tm_shape(poly) + tm_borders() +
        tm_shape(x) + tm_raster()
      
    }
    
    
    # cube -------
    
    safe_cube <- purrr::safely(get_sat_data)
    
    cube <- safe_cube(x
                      , start_date = lubridate::ceiling_date(date, "month") - round(lubridate::time_length(obs_period, unit = "days"), 0)
                      , end_date = lubridate::ceiling_date(date, "month") - 1
                      , cores = 1 # as parallel over points, not within points
                      , ...
                      )
    
    if(!is.null(log)) {
      
      if(is.null(cube$result)) {
        
        log_text <- paste0(log_text
                           , "cube took "
                           , round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 2)
                           , " minutes\n"
                           , cube$error
                           , "\n"
                           )
        
      }
      
    }
    
    if(!is.null(cube$result)) {
      
      cube <- cube$result
    
      if(!is.null(log)) {
        
        log_text <- paste0(log_text
                           , "cube took "
                           , round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 2)
                           , " minutes\n"
                           )
        
        start_time <- Sys.time()
        
      }
      
      # extract------
      
      safe_extract <- purrr::safely(gdalcubes::extract_geom)
      
      env_data <- tibble()
      counter <- 0
      
      while(all(nrow(env_data) < 1, counter < attempts)) {
        
        env_data <- do.call(safe_extract
                            , args = c(list(cube = cube
                                            , sf = poly
                                            )
                                       , extract_args
                                       )
                            )
        
        counter <- counter + 1
        
      }
      
      if(!is.null(env_data$result)) {
        
        env_data <- env_data$result
        
      } else env_data <- NULL
      
      if(!is.null(log)) {
        
        log_text <- paste0(log_text
                             , paste0("extract took "
                                      , round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 2)
                                      , " minutes\n"
                                      )
                             )
        
      }
      
    } else env_data <- NULL
    
    if(!is.null(log)) {
        
        readr::write_lines(log_text
                           , log
                           , append = TRUE
                           )
        
    }
    
    return(env_data)
    
  }
  