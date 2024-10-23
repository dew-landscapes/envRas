
  # check tifs ------
  
  if(FALSE) {
    
    # only need to run this if there is a problem
    
    safe_rast <- purrr::safely(terra::rast)
      
    test_files <- fs::dir_ls(settings$sat_month_cube_dir
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
    
  
  # cube settings ------
  
  sat_month_cube_dir <- settings$sat_month_cube_dir
  sat_collection <- settings$sat_collection
  # period <- settings$period
  layers <- settings$sat_layers
  indices <- settings$sat_indices
  ras_path <- terra::sources(settings$base)
  
  if(FALSE) {
    
    #make physical cube -------
  
  furrr::future_pwalk(list(settings$months$start_date
                           , settings$months$end_date
                           )
               , \(a, b) {
               
                 get_sat_data(x = ras_path
                              , start_date = a # sat_stacks$start_date[[1]]
                              , end_date = b # sat_stacks$end_date[[1]]
                              , out_dir = sat_month_cube_dir
                              , collections = sat_collection
                              , period = period
                              , layers = layers
                              , indices = indices
                              , mask = list(band = "oa_fmask", mask = c(2, 3))
                              , sleep = 60
                              , attempts = 5
                              , max_image_cloud = 50
                              , save_cube = TRUE
                              )
                 }
               )
  
  ## cube results ------
  results <- name_env_tif(settings[["sat_month_cube_dir", exact = TRUE]], parse = TRUE) %>%
    dplyr::mutate(start_date = as.Date(start_date))
  
  
  }
  
  if(FALSE) {
    
    # Test results
    
    temp <- results %>%
      #dplyr::filter(grepl("count|water", path)) %>%
      dplyr::slice(1:9) %>%
      dplyr::pull(path) %>%
      terra::rast()
  
    terra::plot(temp)
    
  }
  
  
  if(FALSE) {
    
    # This makes epochal seasonal cubes
    # try gdalcubes instead
  ## seasons --------
   
  indices <- names(settings$sat_indices)
  
  epochs <- settings$cube %>%
    dplyr::inner_join(results) %>%
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
    dplyr::distinct(epoch, season
                    , polygons, filt_col, level, buffer, period, res, source, collection
                    , start_date, end_date, year, path
                    , layer, scale, offset
                    ) %>%
    tidyr::nest(data = c(year, start_date, end_date, path)) %>%
    dplyr::mutate(start_date = purrr::map_chr(data, \(x) as.character(min(x$start_date)))) %>%
    name_env_tif() %>%
    dplyr::mutate(out_file = fs::path("I:"
                                      , gsub("P1M", "P5Y--P3M", out_file)
                                      )
                  ) %>%
    dplyr::mutate(tif_paths = purrr::map(data, "path")
                  , done = file.exists(out_file)
                  )
  
  
  ## epochs------
  
  fs::dir_create(unique(dirname(epochs$out_file)))
  
  purrr::pwalk(list(epochs$tif_paths[!epochs$done]
                    , epochs$out_file[!epochs$done]
                    , epochs$scale[!epochs$done]
                    , epochs$offset[!epochs$done]
                    )
               , \(a, b, c, d) make_epoch_layer(a
                                                , func = "median"
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
  
  }
  
  if(FALSE) {
    
    # This creates a virtual cube
  # gdalcubes  ---------
    ## single cube -------
  
  cube <- get_sat_data(x = ras_path
                       , start_date = min(settings$months$start_date)
                       , end_date = max(settings$months$end_date)
                       , out_dir = sat_month_cube_dir
                       , collections = sat_collection
                       , period = period
                       , layers = layers
                       , indices = indices
                       , mask = list(band = "oa_fmask", mask = c(2, 3))
                       , sleep = 60
                       , attempts = 5
                       , max_image_cloud = 50
                       )
  
  temp <- fs::dir_ls("H:/data/occ/sa_ibrasub_xn__0/bio_all"
                     , regexp = "parquet$"
                     ) %>%
    purrr::map(\(x) rio::import(x, setclass = "tibble")) %>%
    dplyr::bind_rows() %>%
    envClean::filter_geo_range(settings$bbox %>%
                                 sf::st_as_sfc() %>%
                                 sf::st_sf()
                               ) %>%
    envRaster::add_raster_cell(settings$base, ., add_xy = TRUE)
  
  points <- temp %>%
    dplyr::distinct(lat, long, date, rel_metres) %>%
    envRaster::add_raster_cell(settings$base, ., add_xy = TRUE) %>%
    dplyr::filter(!is.na(cell)) %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = settings$epsg_latlong, remove = FALSE) %>%
    sf::st_transform(crs = sf::st_crs(settings$base)) %>%
    dplyr::filter(!is.na(rel_metres))
  
  gdalcubes::gdalcubes_set_gdal_config("GDAL_NUM_THREADS", settings$use_cores) # only one core as parallel over periods
  
  points_env <- gdalcubes::extract_geom(cube
                                        , points
                                        , time_column = "date"
                                        , merge = TRUE
                                        )
  
  }
  
  if(FALSE) {
    
    ## cube per record --------
    temp <- fs::dir_ls("H:/data/occ/sa_ibrasub_xn__0/bio_all"
                       , regexp = "parquet$"
                       ) %>%
      purrr::map(\(x) rio::import(x, setclass = "tibble")) %>%
      dplyr::bind_rows() %>%
      envClean::filter_geo_range(settings$bbox %>%
                                   sf::st_as_sfc() %>%
                                   sf::st_sf()
                                 ) %>%
      envRaster::add_raster_cell(settings$base, ., add_xy = TRUE)
  
    points <- temp %>%
      dplyr::distinct(lat, long, date, rel_metres) %>%
      envRaster::add_raster_cell(settings$base, ., add_xy = TRUE) %>%
      dplyr::filter(!is.na(cell)) %>%
      sf::st_as_sf(coords = c("long", "lat"), crs = settings$epsg_latlong, remove = FALSE) %>%
      sf::st_transform(crs = sf::st_crs(settings$base)) %>%
      dplyr::filter(rel_metres <= 250)
     
    test <- dplyr::sample_n(points, 2) %>%
      dplyr::select(-rel_metres) %>%
      dplyr::cross_join(tibble::tibble(rel_metres = c(5, 50, 500)))
    
    tictoc::tic()
    
    points_env <- test %>% # TESTING
      dplyr::mutate(env = furrr::future_pmap(list(long
                                           , lat
                                           , rel_metres
                                           , date
                                           )
                                      , \(a, b, c, d) get_record_env(base_path = ras_path
                                                                     , long = a
                                                                     , lat = b
                                                                     , dist_m = c
                                                                     , date = d
                                                                     , extract_args = list(FUN = NULL
                                                                                           , merge = FALSE
                                                                                           , drop_geom = TRUE
                                                                                           , reduce_time = FALSE
                                                                                           )
                                                                     , log = fs::path(settings$out_dir, "env_data", "logs", "log.log")
                                                                     , cores = 1 # parallel over points not within points
                                                                     # dots
                                                                     , collections = sat_collection
                                                                     , period = "P365D"
                                                                     , layers = layers
                                                                     , indices = indices
                                                                     , mask = list(band = "oa_fmask", mask = c(2, 3))
                                                                     , sleep = 5
                                                                     , attempts = 5
                                                                     , max_image_cloud = 50
                                                                     )
                                      )
                    )
    
    tictoc::toc()
    
    
  }
  
  
  
  if(FALSE) {
    
    # This creates a cube from the saved monthly cube
    
    cube <- gdalcubes::create_image_collection(results$path
                                               , date_time = results$start_date
                                               , band_names = results$layer
                                               )
    
    
  }
  
  L8_files <- list.files(system.file("L8NY18", package = "gdalcubes"),
                       ".TIF", recursive = TRUE, full.names = TRUE)
d = as.Date(substr(basename(L8_files), 18, 25), "%Y%m%d")
fname = basename(tools::file_path_sans_ext(L8_files))
b = substr(fname, 27, nchar(fname)) # extract band names
y = create_image_collection(L8_files, date_time = d, band_names = b)
  
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
