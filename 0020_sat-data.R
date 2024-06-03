

  # stacks ------
  
  sat_stacks <- stacks %>%
    dplyr::filter(season %in% c("summer", "autumn"))

  # check tifs ------
  
  safe_rast <- purrr::safely(terra::rast)
    
  test_files <- fs::dir_ls(settings$sat_seas_cube_dir
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
  
  
  # make cube ------
  
  n_files <- nrow(sat_stacks) * (length(settings$sat_layers) + length(settings$sat_indices))
  done_files <- length(fs::dir_ls(settings$sat_seas_cube_dir, regexp = "\\.tif$"))
  run <- done_files < n_files
  
  
  message("expect ", n_files, " files in ", settings$sat_seas_cube_dir)
  
  ## run cube -----
  capture.output(
      
    # Warning (from ?sink (linked from capture.output))
      # Do not sink the messages stream unless you understand the source code implementing it and hence the pitfalls.
      
    {
      message("cube log: ", Sys.time())
    
      purrr::pwalk(list(sat_stacks$start_date
                        , sat_stacks$end_date
                        , sat_stacks$season
                        )
                   , \(a, b, c) {
                   
                   make_sat_cube(settings$base
                                 , start_date = a #sat_stacks$start_date[[1]]
                                 , end_date = b #sat_stacks$end_date[[1]]
                                 , season = c #sat_stacks$season[[1]]
                                 , out_dir = settings$sat_seas_cube_dir
                                 , collections = settings$sat_collection
                                 , period = settings$period
                                 , layers = settings$sat_layers
                                 , indices = settings$sat_indices
                                 , mask = list(band = "oa_fmask", mask = c(2, 3))
                                 , chunks = settings$chunks
                                 , chunk_dir = fs::path(data_dir, "raster", "sat_cube_temp")
                                 , chunk_prefix = "tile_"
                                 , sleep = 60
                                 , attempts = 1
                                 # dots
                                 , property_filter = \(x) {x[["eo:cloud_cover"]] < 10}
                                 )
        }
      )
    }
     
    , file = fs::path(settings$sat_seas_cube_dir, "cube.log")
    , type = "message"
    , append = TRUE
  )
  
  
  # cube results ------
  results <- name_env_tif(settings[["sat_seas_cube_dir", exact = TRUE]], parse = TRUE) %>%
    dplyr::mutate(start_date = as.Date(start_date))
  
  if(FALSE) {
    
    # Test results
    
    temp <- results %>%
      #dplyr::filter(grepl("count|water", path)) %>%
      dplyr::slice(1:9) %>%
      dplyr::pull(path) %>%
      terra::rast()
  
    terra::plot(temp)
    
  }
  
  # seasons --------
   
  indices <- names(settings$sat_indices)
  
  epochs <- settings$epochs %>%
    tidyr::unnest(cols = c(years)) %>%
    dplyr::rename(year = years) %>%
    dplyr::select(year, epoch) %>%
    dplyr::left_join(settings[["seasons", exact = TRUE]]$seasons) %>%
    dplyr::left_join(results) %>%
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
    tidyr::nest(data = c(year, start_date, end_date, path)) %>%
    dplyr::mutate(start_date = purrr::map_chr(data, \(x) as.character(min(x$start_date)))) %>%
    name_env_tif() %>%
    dplyr::mutate(out_file = fs::path("I:"
                                      , gsub("P3M", "P10Y", out_file)
                                      )
                  ) %>%
    dplyr::mutate(stack = purrr::map(data
                                     , ~ terra::rast(.$path)
                                     )
                  , done = file.exists(out_file)
                  )
  
  
  # epochs------
  
  fs::dir_create(dirname(epochs$out_file))
  
  purrr::pwalk(list(epochs$stack[!epochs$done]
                    , epochs$out_file[!epochs$done]
                    , epochs$scale[!epochs$done]
                    , epochs$offset[!epochs$done]
                    )
               , \(a, b, c, d) terra::app(a
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
    
    # Test results
    
    temp <- epochs %>%
      #dplyr::filter(grepl("count|water", path)) %>%
      dplyr::slice(1:9) %>%
      dplyr::mutate(r = purrr::map(out_file, \(x) terra::rast(x)))
    
    r <- terra::rast(temp$r)
    
    names(r) <- paste0(temp$layer)
  
    terra::plot(r)
    
  }
