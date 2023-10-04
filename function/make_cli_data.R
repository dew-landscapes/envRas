  

  # helps-------
  # https://blog.djnavarro.net/posts/2022-03-17_using-aws-s3-in-r/
  # https://registry.opendata.aws/ecmwf-era5/
  # https://github.com/planet-os/notebooks/blob/master/aws/era5-s3-via-boto.ipynb
  
  if(FALSE) {
  
  make_cli_data <- function(start_date = "2019-12-01"
                            , end_date = "2020-02-29"
                            , settings = list(use_epsg = 7845
                                              , use_res = 30
                                              , cli_source = "DEA"
                                              , cli_collection = "era5-pds"
                                              , use_period = "P3M"
                                              , cli_save_dir = "temp"
                                              , boundary = c(xmin = 138.75
                                                             , ymin = -35.45
                                                             , xmax = 138.85
                                                             , ymax = -35.55
                                                             )
                                              )
                            , force_new = FALSE
                            , get_layers = c("air_temperature_at_2_metres.nc", "rainfall")
                            , return_stack = FALSE
                            ) {
    
    message(paste0(start_date, " to ", end_date))
    
    use_extent <- c(settings$use_extent
                    , t0 = as.character(start_date)
                    , t1 = as.character(end_date)
                    )
    
    # setup -------
    
    Sys.setenv(AWS_NO_SIGN_REQUEST = "YES"
               , AWS_REGION = "us-east-1"
               )
    
    prefixes <- function(start_date, end_date) {
      
      start <- paste0(year(start_date)
                      , "/"
                      , stringr::str_pad(month(start_date), 2, pad = "0")
                      )
      
      mid <- paste0(year(end_date)
                    , "/"
                    , stringr::str_pad(month(end_date) - 1, 2, pad = "0")
                    )
      
      end <- paste0(year(end_date)
                    , "/"
                    , stringr::str_pad(month(end_date), 2, pad = "0")
                    )
      
      c(start, mid, end)
      
    }
    
    use_prefix <- prefixes(start_date, end_date)
    
    # get data------
    
    purrr::map(get_layers
               , function(x) {
                 
                 out_file <- fs::path(settings$cli_save_dir
                                      , paste0(gsub(".nc", "", x), "__", start_date, ".tif")
                                      )
                 
                 run <- if(!file.exists(out_file)) TRUE else force_new
                 
                 if(run) {
                   
                   keys <- purrr::map_df(use_prefix
                                         , ~ aws.s3::get_bucket_df(bucket = "era5-pds"
                                                                   , prefix = .
                                                                   , max = 1000
                                                                   ) %>% 
                                           tibble::as_tibble() %>%
                                           dplyr::filter(grepl(x, Key))
                                         ) %>%
                     dplyr::mutate(out_file = purrr::map(Key
                                                         , ~ fs::path(tempdir(), .)
                                                         )
                                   )
                   
                   
                   purrr::walk2(keys$Key
                                , keys$out_file
                                , ~ aws.s3::save_object(.x
                                                        , bucket = "era5-pds"
                                                        , file = .y
                                                        )
                                )
                   
                   
                   v_num <- gdalcubes::cube_view(srs = paste0("EPSG:"
                                                              , settings$use_epsg
                                                              )
                                                 , extent = use_extent
                                                 , dx = settings$use_res #ceiling(abs(use_extent$left - use_extent$right) / 30)
                                                 , dy = settings$use_res #ceiling(abs(use_extent$top - use_extent$bottom) / 30)
                                                 , dt = settings$use_period
                                                 , aggregation = "median"
                                                 , resampling = "near"
                                                 )
                   
                   
                   
                   cube <- terra::rast(unlist(keys$out_file)) %>%
                     terra::crop(y = settings$bbox) %>%
                     terra::project(y = paste0("epsg:", settings$use_epsg))
                   
                 }
                 
                 
                 
               }
                 
                 
    )
    
    
    
    
    
    col <- gdalcubes::stack_cube(items$path
                                 , datetime = items$datetime
                                 , band_names = month.name
                                 )
    
    # cube setup------
    
    v_num <- gdalcubes::cube_view(srs = paste0("EPSG:"
                                               , settings$use_epsg
                                               )
                                  , extent = use_extent
                                  , dx = settings$use_res #ceiling(abs(use_extent$left - use_extent$right) / 30)
                                  , dy = settings$use_res #ceiling(abs(use_extent$top - use_extent$bottom) / 30)
                                  , dt = settings$use_period
                                  , aggregation = "median"
                                  , resampling = "bilinear"
                                  )
    
    
    # process bands------
    
    purrr::walk(names(col)
                , function(x) {
                  
                  message(x)
                  
                  out_file <- fs::path(settings$cli_save_dir
                                       , paste0(gsub("nbart_", "", x), "__", start_date, ".tif")
                                       )
                  
                  run <- if(!file.exists(out_file)) TRUE else force_new
                  
                  if(run) {
                    
                    r <- gdalcubes::raster_cube(col
                                                , v_num
                                                ) %>%
                      gdalcubes::select_bands(x) %>%
                      gdalcubes::reduce_time(names = x
                                             , FUN = function(a) {
                                               
                                               median(a, na.rm = TRUE)
                                               
                                               }
                                             )
                    
                    gdalcubes::write_tif(r
                                         , dir = settings$cli_save_dir
                                         , prefix = paste0(gsub("nbart_", "", x), "__")
                                         )
                    
                    }
                  
                }
                
    )
    
    
    # process indices-------
    
    purrr::iwalk(indices
                 , ~ {
                   
                   message(.y)
                   
                   out_file <- fs::path(settings$cli_save_dir
                                        , paste0(.y, "__", start_date, ".tif")
                                        )
                   
                   
                   run <- if(!file.exists(out_file)) TRUE else force_new
                   
                   if(run) {
                     
                     .x[[1]] <- grep(.x[[1]], get_bands, value = TRUE)
                     .x[[2]] <- grep(.x[[2]], get_bands, value = TRUE)
                     
                     r <- gdalcubes::raster_cube(col
                                                 , v_num
                                                 , mask = cloud_mask
                                                 ) %>%
                       gdalcubes::select_bands(c(.x[[1]], .x[[2]])) %>%
                       apply_pixel(paste0("("
                                          , .x[[1]]
                                          , "-"
                                          , .x[[2]]
                                          , ")/("
                                          , .x[[1]]
                                          , "+"
                                          , .x[[2]]
                                          , ")"
                                          )
                                   , .y
                                   ) %>%
                       gdalcubes::reduce_time(names = .y
                                              , FUN = function(a) {
                                                
                                                median(a, na.rm = TRUE)
                                                
                                              }
                                              
                                              )
                     
                     gdalcubes::write_tif(r
                                          , dir = settings$cli_save_dir
                                          , prefix = paste0(.y, "__")
                                          )
                     
                   }
                   
                 }
                 
    )
    
    
    # process categorical---------
    
    if(!is.null(categorical)) {
      
      purrr::iwalk(categorical[-1]
                   , ~ {
                                    
                     message(.y)
                     
                     out_file <- fs::path(settings$cli_save_dir
                                          , paste0(.y, "__", start_date, ".tif")
                                          )
                                            
                                    
                     run <- if(!file.exists(out_file)) TRUE else force_new
                     
                     if(run) {
                       
                       use_mask <- gdalcubes::image_mask("oa_fmask"
                                                         , values = .x[[1]]
                                                         , invert = TRUE
                                                         )
                       
                       r <- gdalcubes::raster_cube(col
                                                   , v_cat
                                                   , mask = use_mask
                                                   ) %>%
                         gdalcubes::select_bands(categorical[1][[1]]) %>%
                         gdalcubes::reduce_time(paste0("count("
                                                       , categorical[1][[1]]
                                                       , ")"
                                                       )
                                                )
                       
                       temp_file <- fs::path(settings$cli_save_dir
                                             , "temp"
                                             , paste0(.y, "__", start_date, ".tif")
                                             )
                                      
                       gdalcubes::write_tif(r
                                            , dir = dirname(temp_file)
                                            , prefix = paste0(.y, "__")
                                            )
                       
                       s <- terra::rast(c(temp_file
                                          , pixel_file
                                          )
                                        )
                       
                       func <- function(x, y) {y / x}
                       
                       r <- terra::lapp(s[[1:2]]
                                        , fun = func
                                        , filename = out_file
                                        , overwrite = TRUE
                                        )
                       
                       names(r) <- .y
                       
                       terra::update(r, names = TRUE)
                       
                       fs::dir_delete(dirname(temp_file))
                       
                     }
                     
                   }
                   
      )
  
    }
    
    if(return_stack) {
      
      # stack------
      
      stack <- fs::dir_info(settings$cli_save_dir
                            , regexp = "tif$"
                            ) %>%
        dplyr::filter(grepl(paste0(c(gsub("nbart_", "", get_bands)
                                     , names(indices)
                                     , if(!is.null(categorical)) names(tail(categorical, -1))
                                     )
                                   , collapse = "|"
                                   )
                            , path
                            )
                      ) %>%
        dplyr::mutate(name = gsub("\\.tif", "", basename(path))) %>%
        tidyr::separate(name, into = c("band", "date"), sep = "__") %>%
        dplyr::select(path, band, date) %>%
        dplyr::mutate(date = as.Date(date)) %>%
        dplyr::filter(date == start_date) %>%
        dplyr::pull(path) %>%
        terra::rast()
      
      return(stack)
      
    }
    
  }
  
  }
  