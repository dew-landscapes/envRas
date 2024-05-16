
get_static_data <- function(static_sets
                            , collections = c("ga_ls_wo_fq_myear_3")
                            , layers = c("frequency")
                            , force_new = FALSE
                            , sleep = 60 # seconds to wait between failed get_request calls
                            , start_date <- "1970-01-01"
                            , end_date <- "2019-12-01"
                            , season <- "all"
                            , ... # passed to gdalcubes::stac_image_collection
                            ) {
    
  # find images--------
    
  items <- NULL
  
  counter <- 0
  
  while(!any(!is.null(items), counter > 10)) {
    
    if(counter > 0) Sys.sleep(sleep)
    
    stac_request <- function(start_date, end_date, static_sets) {
      
      res <- rstac::stac("https://explorer.sandbox.dea.ga.gov.au/stac") %>%
        rstac::stac_search(collections = collections
                           , bbox = static_sets[["bbox", exact = TRUE]]
                           , datetime = paste0(start_date
                                               , "/"
                                               , end_date
                                               )
                           ) %>%
        rstac::get_request()
      
      return(res)
      
    }
      
    safe_request <- purrr::safely(stac_request)
    
    items <- safe_request(start_date, end_date, static_sets)
    
    if(!is.null(items$error)) {
      
      message(items$error)
      
    }
        
    items <- items$result
    
    counter <- counter + 1
    
    message("counter: "
            , counter
            , ". features: "
            , length(items$features)
            )
    
  }
  
  if(length(items)) {
    
    layers <- items %>%
      rstac::items_assets() %>%
      grep(paste0(unique(c(layers)), collapse = "$|"), ., value = TRUE)
    
    col <- gdalcubes::stac_image_collection(items$features
                                            , asset_names = c(layers)
                                            , ...
                                            )
      
    if(FALSE) {
        
      # interactive
      col <- gdalcubes::stac_image_collection(items$features
                                              , asset_names = c(layers)
                                              )
        
    }
    
    use_extent <- c(static_sets[["use_extent", exact = TRUE]]
                    , t0 = as.character(start_date)
                    , t1 = as.character(end_date)
                    )
    
    # cube setup------
    
    v_num <- gdalcubes::cube_view(srs = paste0("EPSG:"
                                               , static_sets[["epsg_proj", exact = TRUE]]
                                               )
                                  , extent = use_extent
                                  , dx = static_sets[["static_res", exact = TRUE]] #ceiling(abs(use_extent$left - use_extent$right) / 30)
                                  , dy = static_sets[["static_res", exact = TRUE]] #ceiling(abs(use_extent$top - use_extent$bottom) / 30)
                                  , dt = "P50Y"
                                  , aggregation = "median"
                                  , resampling = "bilinear"
                                  )
      
    
    # process layers------
    
    purrr::walk(layers
                , \(x) {
                  
                  message(x)
                  
                  out_file <- fs::path(static_sets[["static_dir", exact = TRUE]]
                                       , paste0(x, "__"
                                                , season, "__"
                                                , start_date, ".tif"
                                                )
                                       )
                  
                  run <- if(!file.exists(out_file)) TRUE else force_new
                  
                  if(run) {
                    
                    r <- gdalcubes::raster_cube(col
                                                , v_num
                                                ) %>%
                      gdalcubes::select_bands(x)
                    
                    gdalcubes::write_tif(r
                                         , dir = static_sets[["static_dir", exact = TRUE]]
                                         , prefix = paste0(x, "__", season, "__")
                                         # , pack = list(type = "uint16"
                                         #               , scale = 1
                                         #               , offset = 0
                                         #               , nodata = 65535
                                         #               )
                                         # , creation_options = list("COMPRESS" = "NONE")
                                         )
                    
                    
                  }
                  
                }
                
    )
      
      
      # process indices-------
      
      purrr::iwalk(indices
                   , ~ {
                     
                     message(.y)
                     
                     out_file <- fs::path(static_sets[["sat_cube_dir", exact = TRUE]]
                                          , paste0(.y, "__", start_date, ".tif")
                                          )
                     
                     
                     run <- if(!file.exists(out_file)) TRUE else force_new
                     
                     if(run) {
                       
                       .x[[1]] <- grep(.x[[1]], layers, value = TRUE)
                       .x[[2]] <- grep(.x[[2]], layers, value = TRUE)
                       
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
                                                  
                                                  10000 * median(a, na.rm = TRUE)
                                                  
                                                }
                                                
                                                )
                       
                       gdalcubes::write_tif(r
                                            , dir = static_sets[["sat_cube_dir", exact = TRUE]]
                                            , prefix = paste0(.y, "__")
                                            , pack = list(type = "int16"
                                                          , scale = 1
                                                          , offset = 0
                                                          , nodata = -32768
                                                          )
                                            , creation_options = list("COMPRESS" = "NONE")
                                            )
                       
                     }
                     
                   }
                   
      )
      
      if(return_stack) {
        
        # stack------
        
        stack <- fs::dir_info(static_sets[["sat_cube_dir", exact = TRUE]]
                              , regexp = "tif$"
                              ) %>%
          dplyr::filter(grepl(paste0(c(gsub("nbart_", "", layers)
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
        
      }
      
    } else {
      
      warning("No stac items retrieved")
      
    }
    
    
    # return------
    
    if(exists("stack")) return(stack) else return(invisible(NULL))
    
  }
  