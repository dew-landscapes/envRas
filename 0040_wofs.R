
  wofs_cube_todo <- sat_cube_todo %>%
    dplyr::select(-todo, -left_to_do) %>%
    dplyr::mutate(path = dirname(path)
                  , collection = "ga_ls_wo_3"
                  , path = fs::path(path, paste0("DEA__", collection))
                  , file = fs::path(path, paste0("water__", start_date, ".tif"))
                  , done = file.exists(file)
                  ) %>%
    dplyr::filter(!done)
  
  purrr::pwalk(list(wofs_cube_todo$start_date
                           , wofs_cube_todo$end_date
                           , wofs_cube_todo$collection
                           , wofs_cube_todo$path
                           )
               , \(a, b, c, d) {
                 
                  if(FALSE) {
                    
                    # testing
                    a <- wofs_cube_todo$start_date[[10]]
                    b <- wofs_cube_todo$end_date[[10]]
                    c <- wofs_cube_todo$collection[[10]]
                    d <- wofs_cube_todo$path[[10]]
                    
                  }
               
                 cube <- get_sat_data(x = ras_path
                                      , start_date = a
                                      , end_date = b
                                      , collections = c
                                      , period = min_period
                                      , property_filter = function(x) {x[["eo:cloud_cover"]] < settings$sat_max_cloud}
                                      , layers = "water"
                                      , indices = NULL
                                      #, mask = list(band = "oa_fmask", mask = c(2, 3)) # already dealt with in wofs
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
                 
                 terra::writeRaster(r
                                    , filename = fs::path(d, paste0("water__", a, ".tif"))
                                    , datatype = "INT1U"
                                    , gdal = c("COMPRESS=NONE")
                                    )
                 
                 
                 }
               
               )  
  