
  get_record_env <- function(base_path
                             , lat
                             , long
                             , crs_data = 4283
                             , dist_m
                             , date
                             , log = NULL
                             , cores = 1
                             , extract_args = list(FUN = median
                                                   , merge = FALSE
                                                   , drop_geom = FALSE
                                                   , reduce_time = FALSE
                                                   )
                             , ... # get_sat_data args
                             ) {
    
    dots <- list(...)
    period <- dots$period
    
    # base to determine crs and resolution
    base <- terra::rast(base_path)
    
    # check dist_m is > half the resolution of the base raster
    if(dist_m < 0.5 * terra::res(base)[[1]]) dist_m <- 0.5 * terra::res(base)[[1]]
    
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
    
    # tibble for making sf
    rec_row <- tibble::tibble(lat = lat, long = long, dist_m = dist_m, date = date)
    
    # polygon around point based on spatial reliability
    poly <- sf::st_as_sf(rec_row
                         , coords = c("long", "lat")
                         , crs = crs_data
                         , remove = FALSE
                         ) %>%
      sf::st_transform(crs = sf::st_crs(base)) %>%
      sf::st_buffer(dist_m)
    
    # raster to use in virtual cube
    x <- terra::rast(extent = sf::st_bbox(poly)
                , resolution = terra::res(base)
                , crs = terra::crs(base)
                , vals = 1
                ) %>%
      terra::mask(mask = terra::vect(poly))
    
    safe_cube <- purrr::safely(get_sat_data)
    
    cube <- safe_cube(x
                      , start_date = date - round(lubridate::time_length(period, unit = "days"), 0)
                      , end_date = date
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
      
      safe_extract <- purrr::safely(gdalcubes::extract_geom)
      
      env_data <- do.call(safe_extract
                          , args = c(list(cube = cube
                                          , sf = poly
                                          )
                                     , extract_args
                                     )
                          )
      
      if(!is.null(log)) {
        
        log_text <- paste0(log_text
                             , paste0("extract took "
                                      , round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 2)
                                      , " minutes\n"
                                      )
                             )
        
      }
      
    } else env_data <- NULL
    
    env_data <- if(!is.null(env_data$result)) env_data$result else NULL
    
    if(!is.null(log)) {
        
        readr::write_lines(log_text
                           , log
                           , append = TRUE
                           )
        
      }
    
    return(env_data)
    
  }
  