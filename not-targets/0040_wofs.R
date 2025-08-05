
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
  
  
  ## cube results ------
  results <- name_env_tif(dirname(settings[["sat_month_cube", exact = TRUE]][[1]]), parse = TRUE) %>%
    dplyr::filter(grepl("water", path)) %>%
    dplyr::mutate(start_date = as.Date(start_date))
  
  
  # 'static' (predict) layers --------
   
  static <- results %>%
    dplyr::inner_join(settings$months %>%
                        dplyr::filter(epoch == max(epoch))
                      ) %>%
    dplyr::filter(!is.na(path)) %>%
    dplyr::mutate(scale = gdalcubes::pack_minmax(min = 0, max = 1)$scale
                  , offset = gdalcubes::pack_minmax(min = -1, max = 1)$offset
                  ) %>%
    dplyr::distinct(polygons, filt_col, level, buffer, period, res, source, collection, layer
                    , start_date, end_date, epoch, season
                    , year, year_use, path, scale, offset
                    ) %>%
    tidyr::nest(data = c(year, year_use, start_date, end_date, path)) %>%
    dplyr::mutate(start_date = purrr::map_chr(data, \(x) as.character(min(x$start_date)))) %>%
    dplyr::mutate(period = paste0(settings$epoch_period, "--P3M")) %>%
    name_env_tif() %>%
    dplyr::mutate(tif_paths = purrr::map(data, "path")
                  , out_file = fs::path("I:", out_file)
                  , done = file.exists(out_file)
                  )
  
  fs::dir_create(unique(dirname(static$out_file)))
  
  purrr::pwalk(list(static$tif_paths[!static$done]
                    , static$out_file[!static$done]
                    , static$scale[!static$done]
                    , static$offset[!static$done]
                    )
               , \(a, b, c, d) terra::app(terra::rast(a)
                                          , fun = \(x) sum(x, na.rm = TRUE) / sum(!is.na(x))
                                          , filename = b
                                          , overwrite = TRUE
                                          , wopt = list(datatype = "INT2S"
                                                        , scale = c
                                                        , offset = d
                                                        , gdal = c("COMPRESS=NONE")
                                                        )
                                          , cores = settings$use_cores
                                          )
               )
  