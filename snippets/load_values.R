if(FALSE) {
  
  # get_sat_data
  x = terra::sources(settings$base)
  start_date <- settings$months$start_date[[500]]
  end_date <- settings$months$end_date[[500]]
  
  period = settings$min_period
  
  out_dir = settings$sat_month_cube_dir
  collections = settings$sat_collection
  
  layers = settings$sat_layers
  indices = settings$sat_indices
  mask = list(band = "oa_fmask", mask = c(2, 3))
  sleep = 60
  attempts = 5
  max_image_cloud = 50
  
  force_new <- FALSE
 
  
  save_cube <- TRUE
  save_cube <- FALSE
  
  
  # get_record_env
  date <- points$date[[1]]
  lat <- points$lat[[1]]
  long <- points$long[[1]]
  dist_m <- terra::res(settings$base)[[1]]
  
  start_date = points$date[[1]] - round(lubridate::time_length(period, unit = "days"), 0)
  end_date = points$date[[1]]
  
  extract_args = list(FUN = NULL
                      , merge = FALSE
                      , drop_geom = TRUE
                      , reduce_time = FALSE
                      )
  
}