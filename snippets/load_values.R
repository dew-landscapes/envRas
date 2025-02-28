if(FALSE) {
  
  # get_sat_data
  id <- which(sat_cube_todo$start_date == "2023-11-01")
  x = terra::sources(settings$base)
  source_url = "https://explorer.sandbox.dea.ga.gov.au/stac"
  collections = sat_cube_todo$collection[[id]]
  excludes = "nbar_" # no longer needed?
  start_date <- sat_cube_todo$start_date[[id]]
  end_date <- sat_cube_todo$end_date[[id]]
  collections <- sat_cube_todo$collection[[id]]
  out_dir <- sat_cube_todo$path[[id]]
  aggregation_func = "median"
  resampling_method = "bilinear"
  property_filter = function(x) {x[["eo:cloud_cover"]] < 20}
  period = settings$period
  
  layers = settings$sat_layers
  indices = settings$sat_indices
  mask = list(band = "oa_fmask", mask = c(2, 3))
  sleep = 60
  attempts = 5
  max_image_cloud = 50
  cores = settings$use_cores
  
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