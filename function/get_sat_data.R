  
  get_sat_data <- function(start_date
                           , end_date
                           , season
                           , sat_sets # named list with use_extent, epsg_proj, sat_collection, bbox, sat_res, period
                           , layers = c("green", "blue", "red")
                           , indices = list(gdvi = c("green", "nir")
                                            , ndvi = c("nir", "red")
                                            )
                           , force_new = FALSE
                           , mask = list(band = "oa_fmask"
                                          , mask = c(2, 3)
                                          )
                           , sleep = 60 # seconds to wait between failed get_request calls
                           , ... # passed to gdalcubes::stac_image_collection
                           ) {
    
    message(start_date, " to ", end_date)
    
    # check files ------
    
    check_files <- tibble::tibble(out_file = c(layers, names(indices))
                                  , start_date = start_date
                                  , season = season
                                  ) %>%
      dplyr::mutate(out_file = fs::path(sat_sets$out_dir
                                        , paste0(out_file
                                                 , "__"
                                                 , season
                                                 , "__"
                                                 , start_date
                                                 , ".tif"
                                                 )
                                        )
                    , done = file.exists(out_file)
                    ) %>%
      dplyr::filter(!done)
    
    if(nrow(check_files)) {
    
      # find images--------
      
      items <- NULL
      
      counter <- 0
      
      while(!any(!is.null(items), counter > 10)) {
        
        if(counter > 0) Sys.sleep(sleep)
        
        stac_request <- function(start_date, end_date, sat_sets) {
          
          rstac::stac("https://explorer.sandbox.dea.ga.gov.au/stac") %>%
            rstac::stac_search(collections = sat_sets[["sat_collection", exact = TRUE]]
                               , bbox = sat_sets[["bbox", exact = TRUE]]
                               , datetime = paste0(as.character(start_date)
                                                   , "/"
                                                   , as.character(end_date)
                                                   )
                               ) %>%
            rstac::get_request() %>%
            rstac::items_fetch()
          
        }
        
        safe_request <- purrr::safely(stac_request)
        
        items <- safe_request(start_date, end_date, sat_sets)
        
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
      
      
      # collection ------
      
      if(length(items)) {
        
        layers <- items %>%
          rstac::items_assets() %>%
          grep(paste0(unique(c(layers, unname(unlist(indices)))), collapse = "$|"), ., value = TRUE) %>%
          grep("nbar_", ., value = TRUE, invert = TRUE)
        
        col <- gdalcubes::stac_image_collection(items$features
                                                , asset_names = c(layers, mask$band)
                                                , ...
                                                )
        
        if(FALSE) {
          
          # interactive
          col <- gdalcubes::stac_image_collection(items$features
                                                  , asset_names = c(layers, mask$band)
                                                  , property_filter = function(x) {x[["eo:cloud_cover"]] < 10}
                                                  )
          
        }
        
        
        # cube setup------
        
        use_extent <- c(sat_sets[["use_extent", exact = TRUE]]
                        , t0 = as.character(start_date)
                        , t1 = as.character(end_date)
                        )
        
        v_num <- gdalcubes::cube_view(srs = paste0("EPSG:"
                                                   , sat_sets[["epsg_proj", exact = TRUE]]
                                                   )
                                      , extent = use_extent
                                      , dx = sat_sets[["sat_res", exact = TRUE]] 
                                      , dy = sat_sets[["sat_res", exact = TRUE]]
                                      , dt = sat_sets[["period", exact = TRUE]]
                                      , aggregation = "median"
                                      , resampling = "bilinear"
                                      )
        
        if(!is.null(mask$band)) {
          
          cloud_mask <- gdalcubes::image_mask(mask$band
                                              , values = mask$mask
                                              ) # clouds and cloud shadows
          
        }
          
        
        # process layers------
        
        purrr::walk(layers
                    , function(x) {
                      
                      message(x)
                      
                      out_file <- fs::path(sat_sets$out_dir
                                           , paste0(gsub("nbart_", "", x)
                                                    , "__"
                                                    , season
                                                    , "__"
                                                    , start_date
                                                    , ".tif"
                                                    )
                                           )
                      
                      run <- if(!file.exists(out_file)) TRUE else force_new
                      
                        if(run) {
                        
                        r <- gdalcubes::raster_cube(col
                                                    , v_num
                                                    , mask = if(exists("cloud_mask")) cloud_mask else NULL
                                                    ) %>%
                          gdalcubes::select_bands(x) %>%
                          gdalcubes::reduce_time(names = gsub("nbart_", "", x)
                                                 , FUN = \(a) {
                                                   
                                                   median(a, na.rm = TRUE)
                                                   
                                                 }
                                                 )
                        
                        gdalcubes::write_tif(r
                                             , dir = dirname(out_file)
                                             , prefix = paste0(gsub("nbart_", "", x)
                                                               , "__"
                                                               , season
                                                               , "__"
                                                               )
                                             , pack = gdalcubes::pack_minmax(type = "int16"
                                                                             , min = 0
                                                                             , max = 10000
                                                                             )
                                             , creation_options = list("COMPRESS" = "NONE")
                                             )
                        
                        }
                      
                      }
                    
                    )
        
        
        # process indices-------
        
        purrr::iwalk(indices
                     , \(x, idx) {
                       
                       message(idx)
                       
                       out_file <- fs::path(sat_sets$out_dir
                                            , paste0(idx
                                                     , "__"
                                                     , start_date
                                                     , ".tif"
                                                     )
                                            )
                       
                       run <- if(!file.exists(out_file)) TRUE else force_new
                       
                       if(run) {
                       
                         # this deals with 'nbart_' 
                         x[[1]] <- grep(x[[1]], layers, value = TRUE)
                         x[[2]] <- grep(x[[2]], layers, value = TRUE)
                         
                         r <- gdalcubes::raster_cube(col
                                                     , v_num
                                                     , mask = cloud_mask
                                                     ) %>%
                           gdalcubes::select_bands(c(x[[1]], x[[2]])) %>%
                           gdalcubes::apply_pixel(paste0("("
                                                         , x[[1]]
                                                         , "-"
                                                         , x[[2]]
                                                         , ")/("
                                                         , x[[1]]
                                                         , "+"
                                                         , x[[2]]
                                                         , ")"
                                                         )

                                                  ) %>%
                           gdalcubes::reduce_time(names = idx
                                                  , FUN = \(a) {
                                                    
                                                    median(a, na.rm = TRUE)
                                                    
                                                  }
                                                  
                                                  )
                         
                         gdalcubes::write_tif(r
                                              , dir = dirname(out_file)
                                              , prefix = paste0(idx
                                                                , "__"
                                                                , season
                                                                , "__"
                                                                )
                                              , pack = gdalcubes::pack_minmax(type="int16"
                                                                              , min = -1
                                                                              , max = 1
                                                                              )
                                              , creation_options = list("COMPRESS" = "NONE")
                                              )
                         
                       }
                       
                     }
                     
        )
          
        } else {
          
          message("No stac items retrieved")
          
        }
      
    } else message("all files done")
      
    # return------
    
    return(invisible(NULL))
    
  }
  