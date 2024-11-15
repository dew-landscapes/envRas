
  wofs_cube <- settings$months %>%
    dplyr::cross_join(tibble::enframe(settings$wofs_month_cube, name = NULL, value = "path") %>%
                        tidyr::unnest(cols = c(path)) %>%
                        dplyr::mutate(collection = settings$wofs_collection)
                      ) %>%
    dplyr::mutate(todo = dplyr::case_when(grepl("wo", path) & start_date > "1986-08-01" ~ TRUE
                                          , TRUE ~ FALSE
                                          )
                  ) %>%
    dplyr::filter(todo)
  
  wofs_cube_todo <- wofs_cube %>%
    dplyr::mutate(file = fs::path(path, paste0("water__", start_date, ".tif"))
                  , done = file.exists(file)
                  ) %>%
    dplyr::filter(!done)
  
  fs::dir_create(unique(wofs_cube_todo$path))
  
  purrr::pwalk(list(wofs_cube_todo$start_date
                           , wofs_cube_todo$end_date
                           , wofs_cube_todo$collection
                           , wofs_cube_todo$path
                           )
               , \(a, b, c, d) {
                 
                  if(FALSE) {
                    
                    # testing
                    a <- wofs_cube_todo$start_date[[100]]
                    b <- wofs_cube_todo$end_date[[100]]
                    c <- wofs_cube_todo$collection[[100]]
                    d <- wofs_cube_todo$path[[100]]
                    
                  }
               
                 cube <- get_sat_data(x = ras_path
                                      , start_date = a
                                      , end_date = b
                                      , collections = c
                                      , period = min_period
                                      , property_filter = function(x) {x[["eo:cloud_cover"]] < settings$sat_max_cloud}
                                      , layers = "water"
                                      , indices = NULL
                                      , mask = NULL # already dealt with in wofs
                                      , sleep = 60
                                      , attempts = 5
                                      , save_cube = FALSE
                                      , cores = settings$use_cores # CHANGE TO 1 IF USING FURRR INSTEAD OF PURRR!!
                                      , creation_options = list("COMPRESS" = "NONE")
                                      , force_new = force_new_sat
                                      , resampling_method = "near"
                                      )
                 
                 r <- cube %>%
                   gdalcubes::st_as_stars.cube() %>%
                   terra::rast() %>%
                   terra::app(\(x) x == 128)
                 
                 message(a)
                 
                 terra::writeRaster(r
                                    , filename = fs::path(d, paste0("water__", a, ".tif"))
                                    , datatype = "INT1U"
                                    , gdal = c("COMPRESS=NONE")
                                    )
                 
                 
                 }
               
               )  
  