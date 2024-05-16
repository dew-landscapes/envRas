
  # cube-------

  fs::dir_create(settings[["cli_seas_cube_dir", exact = TRUE]])
  
  base_url <- "https://dapds00.nci.org.au/thredds/dodsC/gh70/ANUClimate/v2-0/stable/month"
  
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
  
  safe_nc <- purrr::safely(stars::read_ncdf)

  files <- settings[["seasons", exact = TRUE]]$months %>%
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
                  ) %>%
    dplyr::select(year = year_use, season, layer, func, file, min , max) %>%
    tidyr::nest(data = c(file)) %>%
    dplyr::left_join(settings[["seasons", exact = TRUE]]$season) %>%
    #dplyr::sample_n(1) %>% # TESTING
    dplyr::mutate(out_file = fs::path(settings[["cli_seas_cube_dir", exact = TRUE]]
                                      , paste0(layer, "__", season, "__", start_date,".tif")
                                      )
                  , done = file.exists(out_file)
                  , scale = purrr::map2_dbl(min, max
                                        , \(x, y) gdalcubes::pack_minmax(min = x, max = y)$scale
                                        )
                  , offset = purrr::map2_dbl(min, max
                                         , \(x, y) gdalcubes::pack_minmax(min = x, max = y)$offset
                                         )
                  )
  
  
  purrr::pwalk(list(files$data[!files$done]
                    , files$out_file[!files$done]
                    , files$func[!files$done]
                    , files$scale[!files$done]
                    , files$offset[!files$done]
                    )
               , get_cli_data
               , base = settings$base
               )
  
  
  # cube results-------
  
  results_cli <- fs::dir_info(settings[["cli_seas_cube_dir", exact = TRUE]]
                          , regexp = "tif$"
                          ) %>%
    dplyr::select(path) %>%
    name_env_tif(parse = TRUE) %>%
    dplyr::mutate(start_date = as.Date(start_date))
  
  
  if(FALSE) {
    
    temp <- results %>%
      dplyr::sample_n(16) %>%
      dplyr::pull(path)
    
    plot(temp %>%
           terra::rast()
         )
    
  }
  

  # epochs_cli ----------
  
  epochs_cli <- settings$epochs %>%
    tidyr::unnest(cols = c(years)) %>%
    dplyr::rename(year = years) %>%
    dplyr::select(year, epoch) %>%
    dplyr::left_join(settings[["seasons", exact = TRUE]]$seasons) %>%
    dplyr::left_join(results_cli) %>%
    dplyr::filter(!is.na(path)) %>%
    dplyr::left_join(cli_layers) %>%
    tidyr::nest(data = c(year, start_date, end_date, path)) %>%
    dplyr::mutate(start_date = purrr::map_chr(data, \(x) as.character(min(x$start_date)))) %>%
    name_env_tif() %>%
    dplyr::mutate(out_file = fs::path("I:"
                                      , gsub("P3M", "P10Y", out_file)
                                      )
                  , out_file = gsub("1000", settings$sat_res, out_file)
                  ) %>%
    dplyr::mutate(stack = purrr::map(data
                                     , ~ terra::rast(.$path)
                                     )
                  , done = file.exists(out_file)
                  , scale = purrr::map2_dbl(min, max
                                        , \(x, y) gdalcubes::pack_minmax(min = x, max = y)$scale
                                        )
                  , offset = purrr::map2_dbl(min, max
                                         , \(x, y) gdalcubes::pack_minmax(min = x, max = y)$offset
                                         )
                  )
  
  fs::dir_create(dirname(epochs_cli$out_file[1]))
  
  purrr::pwalk(list(epochs_cli$stack[!epochs_cli$done]
                    , epochs_cli$out_file[!epochs_cli$done]
                    , epochs_cli$func[!epochs_cli$done]
                    , epochs_cli$scale[!epochs_cli$done]
                    , epochs_cli$offset[!epochs_cli$done]
                    )
               , function(a, b, c, d, e) align_cli(s = a
                                                   , out_file = b
                                                   , func = c
                                                   , scale = d
                                                   , offset = e
                                                   , base = settings$base
                                                   )
               )
  
  