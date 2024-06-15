

  get_sat_data <- function(x
                           , start_date
                           , end_date
                           , season
                           , out_dir
                           , collections
                           , period
                           , layers = c("green", "blue", "red")
                           , indices = list(gdvi = c("green", "nir")
                                            , ndvi = c("nir", "red")
                                            )
                           , force_new = FALSE
                           , attempts = 3
                           , mask = list(band = "oa_fmask"
                                         , mask = c(2, 3)
                                         )
                           , sleep = 60 # seconds to wait between failed get_request calls
                           , ... # passed to gdalcubes::stac_image_collection
                         ) {
    
    # check files ------
    check_files <- tibble::tibble(out_file = c(layers, names(indices))
                                  , start_date = start_date
                                  , season = season
                                  ) %>%
      dplyr::mutate(out_file = fs::path(out_dir
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
    
    
    if(any(as.logical(nrow(check_files)), force_new)) {
      
      fs::dir_create(out_dir)
      
      items <- NULL
      
      counter <- 0
      
      r <- terra::rast(x)
      
      x_bbox_latlong <- sf::st_bbox(r) %>%
        sf::st_as_sfc() %>%
        sf::st_transform(crs = 4326) %>%
        sf::st_bbox()
      
      while(!any(!is.null(items), counter > attempts)) {
        
        if(counter > 0) Sys.sleep(sleep)
        
        stac_request <- function(start_date, end_date, collections) {
          
          rstac::stac("https://explorer.sandbox.dea.ga.gov.au/stac") %>%
            rstac::stac_search(collections = collections
                               , bbox = x_bbox_latlong
                               , datetime = paste0(as.character(start_date)
                                                   , "/"
                                                   , as.character(end_date)
                                                   )
                               ) %>%
            rstac::get_request() %>%
            rstac::items_fetch()
          
        }
        
        safe_stac <- purrr::safely(stac_request)
        
        items <- safe_stac(start_date, end_date, collections)
        
        if(is.null(items$error)) {
          
          items <- items$result
        
        } else items <- NULL
        
        counter <- counter + 1
        
        message("counter: "
                , counter
                , ". features: "
                , length(items$features)
                , ". for: "
                , out_dir
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
        use_extent <- c(as.list(sf::st_bbox(r))
                        , t0 = as.character(start_date)
                        , t1 = as.character(end_date)
                        )
        
        names(use_extent)[1:4] <- c("left", "bottom", "right", "top")
        
        v_num <- gdalcubes::cube_view(srs = paste0("EPSG:", terra::crs(r, describe = T)$code)
                                      , extent = use_extent
                                      , dx = terra::res(r)[1]
                                      , dy = terra::res(r)[2]
                                      , dt = period
                                      , aggregation = "median"
                                      , resampling = "bilinear"
                                      )
        
        if(!is.null(mask$band)) {
          
          cloud_mask <- gdalcubes::image_mask(mask$band
                                              , values = mask$mask
                                              ) # clouds and cloud shadows
          
        }
          
        
        # process layers------
        
        still_to_do <- check_success(out_dir
                                     , paste0(layers)
                                     , removes = "nbart_"
                                     )
        
        counter <- 0
        
        while(!any(length(still_to_do) == 0, counter > attempts)) {
          
          force_new_layers <- if(counter == 0) force_new else TRUE 
        
          purrr::walk(still_to_do
                      , \(this_layer) {
                        
                        message(this_layer)
                        
                        out_file <- fs::path(out_dir
                                             , paste0(gsub("nbart_", "", this_layer)
                                                      , "__"
                                                      , season
                                                      , "__"
                                                      , start_date
                                                      , ".tif"
                                                      )
                                             )
                        
                        run <- if(!file.exists(out_file)) TRUE else force_new_layers
                        
                        if(run) {
                          
                          r <- gdalcubes::raster_cube(col
                                                      , v_num
                                                      , mask = if(exists("cloud_mask")) cloud_mask else NULL
                                                      ) %>%
                            gdalcubes::select_bands(this_layer) %>%
                            gdalcubes::reduce_time(names = gsub("nbart_", "", this_layer)
                                                   , FUN = \(a) {
                                                     
                                                     median(a, na.rm = TRUE)
                                                     
                                                   }
                                                   )
                          
                          capture.output(
                             
                             gdalcubes::write_tif(r
                                                 , dir = dirname(out_file)
                                                 , prefix = paste0(gsub("nbart_", "", this_layer)
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
                             
                             , file = gsub("tif$", "log", out_file)
                             , type = "message"
                             )
                          
                        }
                        
                        }
                      
                      )
          
          counter <- counter + 1
          
          still_to_do <- check_success(out_dir
                                       , tif_name = still_to_do
                                       , removes = "nbart_"
                                       )
          
          message("counter: ", counter
                  , ". "
                  , out_dir
                  , ". layers with errors: "
                  , vec_to_sentence(still_to_do)
                  )
          
        }
        
        
        # process indices-------
        
        still_to_do <- check_success(out_dir
                                     , names(indices)
                                     )
        
        counter <- 0
        
        while(!any(length(still_to_do) == 0, counter > attempts)) {
          
          force_new_layers <- if(counter == 0) force_new else TRUE 
        
          purrr::iwalk(indices[still_to_do]
                       , \(x, idx) {
                         
                         message(idx)
                         
                         out_file <- fs::path(out_dir
                                              , paste0(idx
                                                       , "__"
                                                       , season
                                                       , "__"
                                                       , start_date
                                                       , ".tif"
                                                       )
                                              )
                         
                         run <- if(!file.exists(out_file)) TRUE else force_new_layers
                         
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
                                                    , names = idx
                                                    ) %>%
                             gdalcubes::reduce_time(names = idx
                                                    , FUN = \(a) {
                                                      
                                                      median(a, na.rm = TRUE)
                                                      
                                                    }
                                                    
                                                    )
                           capture.output(
                             
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
                            , file = gsub("tif$", "log", out_file)
                            , type = "message"
                           )
                           
                         }
                         
                       }
                       )
          
          counter <- counter + 1
          
          still_to_do <- check_success(out_dir
                                       , still_to_do
                                       )
          
          message("counter: ", counter
                  , ". layers with errors: "
                  , vec_to_sentence(still_to_do)
                  )
          
        }
          
      }
      
    }
    
    return(invisible(NULL))
    
  }
  