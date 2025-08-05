
  # cube-------

  base_url <- "https://thredds.nci.org.au/thredds/dodsC/gh70/ANUClimate/v2-0/stable/month"
  
  
  cli_layers <- tibble::tribble(
    ~layer, ~func, ~min, ~max
    ,"rain", "mean", 0, 300 # from the data: min = 0 and max = 231
    , "evap", "mean", 0, 600 # from the data: min = 26.3 and max = 526
    #, "srad", "mean"
    , "tavg", "mean", -5, 40 # from the data: min = 3.32 and max = 35.2
    , "vpd", "mean", 0, 60 # from the data: min = 0.147 and max = 54.83
    , "tmin", "min", -5, 35 # from the data: min = 0.197 and max = 27.2
    , "tmax", "max", 0, 50 # from the data: min = 5.96 and max = 44.1
    )
  
  

  cli_files <- settings$months %>%
    dplyr::filter(epoch == max(epoch)) |>
    dplyr::cross_join(tibble::enframe(settings$cli_month_cube, name = NULL, value = "path") %>%
                        tidyr::unnest(cols = c(path)) %>%
                        dplyr::mutate(collection = settings$cli_collection)
                      ) %>%
    dplyr::cross_join(cli_layers) %>%
    dplyr::mutate(file_specific = format(start_date, "%Y%m")
                  , file = paste0(base_url
                                  , "/"
                                  , layer
                                  , "/"
                                  , year
                                  , "/"
                                  , paste0("ANUClimate_v2-0_"
                                           , layer
                                           , "_monthly_"
                                           , file_specific
                                           , ".nc"
                                           )
                                  )
                  , out_file = fs::path(path
                                        , paste0(layer, "__", start_date,".tif")
                                        )
                  , done = file.exists(out_file)
                  , scale = purrr::map2_dbl(min, max
                                        , \(x, y) gdalcubes::pack_minmax(min = x, max = y)$scale
                                        )
                  , offset = purrr::map2_dbl(min, max
                                         , \(x, y) gdalcubes::pack_minmax(min = x, max = y)$offset
                                         )
                  )
  
  if(check_tifs) {
    
    # check month cube ---------
    # only need to run this if there is a problem
    
    safe_rast <- purrr::safely(terra::rast)
      
    test_files <- fs::dir_ls(unlist(settings$cli_month_cube)
                             , regexp = "tif$"
                             )
    
    tests <- purrr::map(test_files
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
  
  # monthly cube ------
  purrr::pwalk(list(files = cli_files$file
                    , out_file = cli_files$out_file
                    , func = cli_files$func
                    , scale = cli_files$scale
                    , offset = cli_files$offset
                    )
               , get_thredds_data
               #, base = settings$base # don't use base here
               , bbox = settings$bbox_latlong
               , force_new = force_new_cli
               # , .options = furrr::furrr_options(seed = TRUE # probably not neccessary?
               #                                   , scheduling = Inf # limit the 'tail' in use across cores
               #                                   )
               )
  
  
  ## cube results ------
  results <- name_env_tif(dirname(settings[["cli_month_cube", exact = TRUE]][[1]]), parse = TRUE) %>%
    dplyr::mutate(start_date = as.Date(start_date))
  
  if(FALSE) {
    
    temp <- results %>%
      dplyr::sample_n(16) %>%
      dplyr::pull(path)
    
    terra::plot(temp %>%
                  terra::rast()
                )
    
  }
  

  # 'static' (predict) layers --------
   
  static <- results %>%
    dplyr::inner_join(settings$months %>%
                        dplyr::filter(epoch == max(epoch))
                      ) %>%
    dplyr::filter(!is.na(path)) %>%
    dplyr::left_join(cli_layers) %>%
    dplyr::mutate(scale = gdalcubes::pack_minmax(min = min, max = max)$scale
                  , offset = gdalcubes::pack_minmax(min = min, max = max)$offset
                  ) %>%
    dplyr::distinct(polygons, filt_col, level, buffer, period, res, source, collection, layer
                    , start_date, end_date, epoch, season
                    , year, year_use, path, scale, offset, func
                    ) %>%
    tidyr::nest(data = c(year, year_use, start_date, end_date, path)) %>%
    dplyr::mutate(start_date = purrr::map_chr(data, \(x) as.character(min(x$start_date)))) %>%
    dplyr::mutate(period = paste0(settings$epoch_period, "--P3M")
                  , res = settings$sat_res # needs to be the same here for predicting
                  ) %>%
    name_env_tif() %>%
    dplyr::mutate(tif_paths = purrr::map(data, "path")
                  , out_file = fs::path("I:", out_file)
                  , done = file.exists(out_file)
                  , coarse = gsub("90", "1000", out_file)
                  , coarse_done = file.exists(coarse)
                  )
  
  # fine scale -------
  fs::dir_create(dirname(static$out_file[1]))
  
  purrr::pwalk(list(static$tif_paths[!static$done]
                    , static$out_file[!static$done]
                    , static$func[!static$done]
                    , static$scale[!static$done]
                    , static$offset[!static$done]
                    )
               , \(a, b, c, d, e) align_cli(paths = a
                                            , out_file = b
                                            , func = c
                                            , scale = d
                                            , offset = e
                                            , base = settings$base # needs to be the same as satellite data for predicting
                                            )
               )
  
  # coarse scale ------
  fs::dir_create(dirname(static$coarse[[1]]))
  
  purrr::pwalk(list(static$tif_paths[! static$coarse_done]
                    , static$coarse[! static$coarse_done]
                    , static$func[! static$coarse_done]
                    , static$scale[! static$coarse_done]
                    , static$offset[! static$coarse_done]
                    )
               , \(a, b, c, d, e) terra::app(terra::rast(a)
                                             , fun = c
                                             , na.rm = TRUE
                                             ) |>
                 terra::project(y = paste0("epsg:", settings$epsg_proj)
                                , overwrite = TRUE
                                , filename = b
                                , wopt = list(datatype = "INT2S"
                                              , gdal = c("COMPRESS=NONE")
                                              , scale = d
                                              , offset = e
                                              )
                                )
               )
  
  if(FALSE) {
    
    temp <- static %>%
      dplyr::sample_n(6) %>%
      dplyr::pull(coarse)
    
    terra::plot(temp %>%
                  terra::rast()
                )
    
  }