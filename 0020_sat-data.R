
  # cube settings ------
  
  sat_month_cube_dir <- settings$sat_month_cube_dir
  sat_collection <- settings$sat_collection
  layers <- settings$sat_layers
  indices <- settings$sat_indices
  cube_res <- terra::res(settings$base)[[1]]
  ras_path <- terra::sources(settings$base)

  # periods
  min_period <- settings$period
  epoch_period <- settings$epoch_period
  
  if(check_tifs) {
    
    # check month cube ---------
    # only need to run this if there is a problem
    
    safe_rast <- purrr::safely(terra::rast)
      
    test_files <- fs::dir_ls(unlist(settings$sat_month_cube)
                             , regexp = "tif$"
                             )
    
    tests <- furrr::future_map(test_files
                               , safe_rast
                               ) %>%
      purrr::map("error") %>%
      purrr::compact()
    
    if(length(tests)) {
      
      message("files: "
              , tests
              , " are being deleted"
              )
      
      purrr::map(names(tests)[file.exists(names(tests))]
                 , unlink
                 , force = TRUE
                 )
      
    }  
    
  }
    
  
  # month cube -------
  
  ## Best if settings$period is the same as the temporal extent of each time slice in the cube
  
  sat_cube <- settings$months %>%
    dplyr::cross_join(tibble::enframe(settings$sat_month_cube, name = NULL, value = "path") %>%
                        tidyr::unnest(cols = c(path)) %>%
                        dplyr::mutate(collection = settings$sat_collection)
                      ) %>%
    dplyr::mutate(todo = dplyr::case_when(grepl("ls9", path) & start_date > "2013-03-01" ~ TRUE
                                          , grepl("ls7", path) & start_date > "1986-05-01" & start_date < "2013-03-01" ~ TRUE
                                          , grepl("s2", path) & start_date > "2015-07-01" ~ TRUE
                                          , TRUE ~ FALSE
                                          )
                  ) %>%
    dplyr::filter(todo)
  
  sat_cube_todo <- sat_cube %>%
    dplyr::cross_join(tibble::tibble(layer = c(settings$sat_layers, names(settings$sat_indices)))) %>%
    dplyr::mutate(file = fs::path(path, paste0(layer, "__", start_date, ".tif"))
                  , done = file.exists(file)
                  ) %>%
    dplyr::filter(!done) %>%
    dplyr::select(names(sat_cube)) %>%
    dplyr::count(dplyr::across(tidyselect::any_of(names(sat_cube)))
                 , name = "left_to_do"
                 )
  
  purrr::pwalk(list(sat_cube_todo$start_date
                           , sat_cube_todo$end_date
                           , sat_cube_todo$collection
                           , sat_cube_todo$path
                           )
               , \(p, q, r, s) {
               
                 get_sat_data(x = ras_path
                              , start_date = p # sat_stacks$start_date[[1]]
                              , end_date = q # sat_stacks$end_date[[1]]
                              , out_dir = s
                              , collections = r
                              , period = min_period
                              , property_filter = function(x) {x[["eo:cloud_cover"]] < settings$sat_max_cloud}
                              , layers = layers
                              , indices = indices
                              , mask = list(band = "oa_fmask", mask = c(2, 3))
                              , sleep = 60
                              , attempts = 5
                              , save_cube = TRUE
                              , cores = settings$use_cores # CHANGE TO 1 IF USING FURRR INSTEAD OF PURRR!!
                              , creation_options = list("COMPRESS" = "NONE")
                              , force_new = force_new_sat
                              )
                 }
               # , .options = furrr::furrr_options(seed = TRUE # probably not neccessary?
               #                                   , scheduling = Inf # limit the 'tail' in use across cores
               #                                   )
               )
  
  ## cube results ------
  results <- name_env_tif(dirname(settings[["sat_month_cube", exact = TRUE]][[1]]), parse = TRUE) %>%
    dplyr::mutate(start_date = as.Date(start_date))
  
  
  # 'static' (predict) layers --------
   
  static <- results %>%
    dplyr::inner_join(settings$months %>%
                        dplyr::filter(epoch == max(epoch))
                      ) %>%
    dplyr::filter(!is.na(path)) %>%
    dplyr::mutate(scale = dplyr::case_when(layer %in% names(settings$sat_indices) ~ gdalcubes::pack_minmax(min = -1, max = 1)$scale
                                            , !layer %in% names(settings$sat_indices) ~ gdalcubes::pack_minmax(min = 0, max = 10000)$scale
                                            , TRUE ~ 1
                                            )
                  , offset = dplyr::case_when(layer %in% names(settings$sat_indices) ~ gdalcubes::pack_minmax(min = -1, max = 1)$offset
                                            , !layer %in% names(settings$sat_indices) ~ gdalcubes::pack_minmax(min = 0, max = 10000)$offset
                                            , TRUE ~ 0
                                            )
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
  
  if(check_tifs) {
    
    # Check predict cube ---------
    # only need to run this if there is a problem
    
    safe_rast <- purrr::safely(terra::rast)
      
    test_files <- fs::dir_ls(unique(dirname(static$out_file))
                             , regexp = "tif$"
                             )
    
    tests <- purrr::map(test_files
                               , safe_rast
                               ) %>%
      purrr::map("error") %>%
      purrr::compact()
    
    if(length(tests)) {
      
      message("files:\n"
              , paste0(names(tests), collapse = "\n")
              , "\nwill be deleted"
              )
      
      purrr::map(names(tests)[file.exists(names(tests))]
                 , unlink
                 , force = TRUE
                 )
      
    }  
    
  }
  
  fs::dir_create(unique(dirname(static$out_file)))
  
  purrr::pwalk(list(static$tif_paths[!static$done]
                    , static$out_file[!static$done]
                    , static$scale[!static$done]
                    , static$offset[!static$done]
                    )
               , \(a, b, c, d) terra::app(terra::rast(a)
                                          , fun = "median"
                                          , na.rm = TRUE
                                          , filename = b
                                          , overwrite = TRUE
                                          , wopt = list(datatype = "INT2S"
                                                        , scale = c
                                                        , offset = d
                                                        , gdal = c("COMPRESS=NONE")
                                                        )
                                          )
               )
  
  
  if(FALSE) {
    
    # This creates a cube from the saved monthly cube
    
    cube <- gdalcubes::create_image_collection(results$path
                                               , date_time = results$start_date
                                               , band_names = results$layer
                                               )
    
    
  }
  
  
  if(FALSE) {
    
    # Test results
    
    temp <- static %>%
      #dplyr::filter(grepl("count|water", path)) %>%
      dplyr::slice(1:9) %>%
      dplyr::mutate(r = purrr::map(out_file, \(x) terra::rast(x)))
    
    r <- terra::rast(temp$r)
    
    names(r) <- paste0(temp$layer)
  
    terra::plot(r)
    
  }
