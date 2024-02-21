  make_lc_data <- function(start_date = "2014-01-01"
                            , end_date = "2023-12-31"
                            , settings = list(use_epsg = 7845
                                              , use_res = 30
                                              , lc_source = "https://explorer.sandbox.dea.ga.gov.au/stac"
                                              , lc_collection = "ga_ls_landcover_class_cyear_2"
                                              , use_period = "P1Y"
                                              , lc_cube_dir = "temp"
                                              , bbox = c(xmin = 138.75
                                                         , ymin = -35.45
                                                         , xmax = 138.85
                                                         , ymax = -35.55
                                                         )
                                              )
                            , force_new = FALSE
                            , bands = c("level4")
                            , return_stack = FALSE
                            , ... # passed to gdalcubes::stac_image_collection
                            ) {
    
    
    message(paste0(start_date, " to ", end_date))
    
    # find images--------
    
    items <- list()
    
    counter <- 1
    
    while(!any(length(items), counter > 10)) {
      
      items <- rstac::stac("https://explorer.sandbox.dea.ga.gov.au/stac") %>%
        rstac::stac_search(collections = settings[["lc_collection", exact = TRUE]]
                           , bbox = settings[["bbox", exact = TRUE]]
                           , datetime = paste0(as.character(start_date)
                                               , "/"
                                               , as.character(end_date)
                                               )
                           , limit = 1000
                           ) %>%
        rstac::get_request()
      
      counter <- counter + 1
      
    }
    
    message(paste0("counter: ", counter, ". length(items):", length(items)))
    
    if(length(items)) {
    
      bands <- items %>%
        rstac::items_assets() %>%
        grep(paste0(unique(bands), collapse = "$|"), ., value = TRUE) %>%
        grep("nbar_", ., value = TRUE, invert = TRUE)
      
      col <- gdalcubes::stac_image_collection(items$features
                                              , asset_names = c(bands, mask$band)
                                              , ...
                                              )
      
      use_extent <- c(settings[["use_extent", exact = TRUE]]
                      , t0 = as.character(start_date)
                      , t1 = as.character(end_date)
                      )
      
      # cube setup------
      
      v_num <- gdalcubes::cube_view(srs = paste0("EPSG:"
                                                 , settings[["epsg_proj", exact = TRUE]]
                                                 )
                                    , extent = use_extent
                                    , dx = settings[["use_res", exact = TRUE]] #ceiling(abs(use_extent$left - use_extent$right) / 30)
                                    , dy = settings[["use_res", exact = TRUE]] #ceiling(abs(use_extent$top - use_extent$bottom) / 30)
                                    , dt = settings[["use_period", exact = TRUE]]
                                    , aggregation = "median"
                                    , resampling = "bilinear"
                                    )
      
      v_cat <- v_num %>%
        gdalcubes::cube_view(aggregation = "max"
                             , resampling = "near"
                             )
      
      if(!is.null(mask$band)) {
        
        cloud_mask <- gdalcubes::image_mask(mask$band
                                            , values = mask$mask
                                            ) # clouds and cloud shadows
        
      }
        
      
      # process bands------
      
      purrr::walk(bands
                  , function(x) {
                    
                    message(x)
                    
                    out_file <- fs::path(settings[["lc_cube_dir", exact = TRUE]]
                                         , paste0(gsub("nbart_", "", x), "__", start_date, ".tif")
                                         )
                    
                    run <- if(!file.exists(out_file)) TRUE else force_new
                    
                    if(run) {
                      
                      r <- gdalcubes::raster_cube(col
                                                  , v_num
                                                  , mask = if(exists("cloud_mask")) cloud_mask else NULL
                                                  ) %>%
                        gdalcubes::select_bands(x) %>%
                        gdalcubes::reduce_time(names = x
                                               , FUN = function(a) {
                                                 
                                                 median(a, na.rm = TRUE)
                                                 
                                                 }
                                               )
                      
                      gdalcubes::write_tif(r
                                           , dir = settings[["lc_cube_dir", exact = TRUE]]
                                           , prefix = paste0(gsub("nbart_", "", x), "__")
                                           , pack = list(type = "uint16"
                                                         , scale = 1
                                                         , offset = 0
                                                         , nodata = 65535
                                                         )
                                           , creation_options = list("COMPRESS" = "NONE")
                                           )
                      
                      
                      }
                    
                  }
                  
      )
      
      
      # process indices-------
      
      purrr::iwalk(indices
                   , ~ {
                     
                     message(.y)
                     
                     out_file <- fs::path(settings[["lc_cube_dir", exact = TRUE]]
                                          , paste0(.y, "__", start_date, ".tif")
                                          )
                     
                     
                     run <- if(!file.exists(out_file)) TRUE else force_new
                     
                     if(run) {
                       
                       .x[[1]] <- grep(.x[[1]], bands, value = TRUE)
                       .x[[2]] <- grep(.x[[2]], bands, value = TRUE)
                       
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
                                            , dir = settings[["lc_cube_dir", exact = TRUE]]
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
        
        stack <- fs::dir_info(settings[["lc_cube_dir", exact = TRUE]]
                              , regexp = "tif$"
                              ) %>%
          dplyr::filter(grepl(paste0(c(gsub("nbart_", "", bands)
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