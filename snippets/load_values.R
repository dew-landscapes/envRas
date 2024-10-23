if(FALSE) {
  
  # get_sat_data
  x = terra::sources(settings$base)
  start_date = points$date[[1]] - round(lubridate::time_length(period, unit = "days"), 0)
  end_date = points$date[[1]]
  
  period = "P365D"
  out_dir = settings$sat_month_cube_dir
  collections = settings$sat_collection
  
  layers = settings$sat_layers
  indices = settings$sat_indices
  mask = list(band = "oa_fmask", mask = c(2, 3))
  sleep = 60
  attempts = 5
  max_image_cloud = 50
  
  force_new <- FALSE
 
  # get_record_env
  date <- test$date[[1]]
  lat <- test$lat[[1]]
  long <- test$long[[1]]
  rel <- test$rel_metres[[3]]
  
  extract_args = list(FUN = NULL
                      , merge = FALSE
                      , drop_geom = TRUE
                      , reduce_time = TRUE
                      )
  
}