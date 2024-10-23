

  get_sat_data <- function(x
                           , start_date
                           , end_date
                           , out_dir = NULL
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
                           , max_image_cloud = 50
                           , sleep = 60 # seconds to wait between failed get_request calls
                           , save_cube = FALSE
                         ) {
    
    # gdalcubes--------
    # see https://gdalcubes.github.io/source/concepts/config.html
    gdalcubes::gdalcubes_set_gdal_config("VSI_CACHE", "TRUE")
    gdalcubes::gdalcubes_set_gdal_config("GDAL_CACHEMAX","5%") # 5% per core
    gdalcubes::gdalcubes_set_gdal_config("VSI_CACHE_SIZE","100000000")
    gdalcubes::gdalcubes_set_gdal_config("GDAL_HTTP_MULTIPLEX","YES")
    gdalcubes::gdalcubes_set_gdal_config("GDAL_INGESTED_BYTES_AT_OPEN","32000")
    gdalcubes::gdalcubes_set_gdal_config("GDAL_DISABLE_READDIR_ON_OPEN","EMPTY_DIR")
    gdalcubes::gdalcubes_set_gdal_config("GDAL_HTTP_VERSION","2")
    gdalcubes::gdalcubes_set_gdal_config("GDAL_HTTP_MERGE_CONSECUTIVE_RANGES","YES")
    gdalcubes::gdalcubes_set_gdal_config("GDAL_NUM_THREADS", "1") # only one core as parallel over periods
    
    items <- NULL
    
    counter <- 0
    
    if(! "spatRaster" %in% class(x)) x <- terra::rast(x)
    
    x_bbox_latlong <- sf::st_bbox(x) %>%
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
              )
      
    }
    
    
    # collection ------
    
    if(length(items)) {
      
      needed_layers <- items %>%
        rstac::items_assets() %>%
        grep(paste0(unique(c(layers, unname(unlist(indices)))), collapse = "$|"), ., value = TRUE) %>%
        grep("nbar_", ., value = TRUE, invert = TRUE)
      
      safe_collection <- purrr::safely(gdalcubes::stac_image_collection)
      
      col <- safe_collection(items$features
                             , asset_names = c(needed_layers, mask$band)
                             , property_filter = function(x) {x[["eo:cloud_cover"]] < max_image_cloud}
                             )
      
      if(is.null(col$error)) {
        
        col <- col$result
      
      } else col <- NULL
      
      
      if(!is.null(col)) {
        
        # cube setup------
        use_extent <- c(as.list(sf::st_bbox(x))
                        , t0 = as.character(start_date)
                        , t1 = as.character(end_date)
                        )
        
        names(use_extent)[1:4] <- c("left", "bottom", "right", "top")
        
        v_num <- gdalcubes::cube_view(srs = paste0("EPSG:", terra::crs(x, describe = T)$code)
                                      , dx = terra::res(x)[1]
                                      , dy = terra::res(x)[2]
                                      , dt = period
                                      , extent = use_extent
                                      , aggregation = "median"
                                      , resampling = "bilinear"
                                      )
        
        if(!is.null(mask$band)) {
          
          cloud_mask <- gdalcubes::image_mask(mask$band
                                              , values = mask$mask
                                              ) # clouds and cloud shadows
          
        }
          
        
        if(save_cube) {
          
        fs::dir_create(out_dir)
          
        # process layers------
        
        purrr::walk(needed_layers
                    , \(this_layer) {
                      
                      message(paste0(this_layer
                                     , " "
                                     , start_date
                                     )
                              )
                      
                      out_file <- fs::path(out_dir
                                           , paste0(gsub("nbart_", "", this_layer)
                                                    , "__"
                                                    , start_date
                                                    , ".tif"
                                                    )
                                           )
                      
                      run <- if(!file.exists(out_file)) TRUE else force_new
                      
                      if(run) {
                        
                        safe_select_band <- purrr::safely(gdalcubes::select_bands)
                        
                        r <- gdalcubes::raster_cube(col
                                                    , v_num
                                                    , mask = if(exists("cloud_mask")) cloud_mask else NULL
                                                    ) %>%
                          safe_select_band(this_layer)
                        
                        if(!is.null(r$result)) {
                          
                          gdalcubes::write_tif(r$result
                                               , dir = dirname(out_file)
                                               , prefix = paste0(gsub("nbart_", "", this_layer)
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
                      
                    }
                    
                    )
        
        
        # process indices-------
        
        purrr::iwalk(indices
                     , \(x, idx) {
                       
                       message(idx)
                       
                       out_file <- fs::path(out_dir
                                            , paste0(idx
                                                     , "__"
                                                     , start_date
                                                     , ".tif"
                                                     )
                                            )
                       
                       run <- if(!file.exists(out_file)) TRUE else force_new
                       
                       if(run) {
                       
                         # this deals with 'nbart_' 
                         x[[1]] <- grep(x[[1]], needed_layers, value = TRUE)
                         x[[2]] <- grep(x[[2]], needed_layers, value = TRUE)
                         
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
                                                  )
                         
                         gdalcubes::write_tif(r
                                              , dir = dirname(out_file)
                                              , prefix = paste0(idx
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
          
          # cube only ------
          
          cube <- gdalcubes::raster_cube(col
                                         , v_num
                                         , mask = if(exists("cloud_mask")) cloud_mask else NULL
                                         )
          
        }
        
      }
      
    }
    
  if(!save_cube) cube else return(invisible(NULL))
    
}
      
    
  