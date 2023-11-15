  
  make_sat_data <- function(start_date = "2019-12-01"
                            , end_date = "2020-02-29"
                            , settings = list(use_epsg = 7845
                                              , use_res = 30
                                              , sat_source = "DEA"
                                              , sat_collection = "ga_ls8c_ard_3"
                                              , use_period = "P3M"
                                              , sat_save_dir = "temp"
                                              , bbox = c(xmin = 138.75
                                                         , ymin = -35.45
                                                         , xmax = 138.85
                                                         , ymax = -35.55
                                                         )
                                            )
                            , force_new = FALSE
                            , get_bands = c("blue", "red", "green"
                                            , "swir_1", "swir_2", "coastal_aerosol"
                                            , "nir", "oa_fmask"
                                            )
                            , indices = list(gdvi = c("green", "nir")
                                             , ndvi = c("nir", "red")
                                             , nbr = c("nir", "swir_1")
                                             , nbr2 = c("nir", "swir_2")
                                             )
                            , mask = list(band = "oa_fmask"
                                          , mask = c(2, 3)
                                          )
                            , do_pixel_count = FALSE # Not sure this is working anyway
                            , categorical = NULL # Also not sure if this is working
                            , return_stack = FALSE
                            ) {
    
    
    message(paste0(start_date, " to ", end_date))
    
    # find images--------
    items <- rstac::stac("https://explorer.sandbox.dea.ga.gov.au/stac") %>%
      rstac::stac_search(collections = settings[["sat_collection", exact = TRUE]]
                         , bbox = settings[["bbox", exact = TRUE]]
                         , datetime = paste0(as.character(start_date)
                                             , "/"
                                             , as.character(end_date)
                                             )
                         , limit = 1000
                         ) %>%
      rstac::get_request()
    
    get_bands <- items %>%
      rstac::items_assets() %>%
      grep(paste0(unique(c(get_bands, mask$band)), collapse = "$|"), ., value = TRUE) %>%
      grep("nbar_", ., value = TRUE, invert = TRUE)
    
    col <- gdalcubes::stac_image_collection(items$features
                                            , asset_names = get_bands
                                            , property_filter = function(x) {x[["eo:cloud_cover"]] < 50}
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
    
    cloud_mask <- gdalcubes::image_mask(mask$band
                                        , values = mask$mask
                                        ) # clouds and cloud shadows
    
    # pixel count -------
    
    if(do_pixel_count) {
      
      pixel_file <- fs::path(settings[["sat_save_dir", exact = TRUE]]
                           , paste0("pixel_count__", start_date, ".tif")
                           )
      
      run <- if(!file.exists(pixel_file)) TRUE else force_new
      
      if(run) {
        
        message("count")
        
        r <- gdalcubes::raster_cube(col
                                    , v_cat
                                    , mask = cloud_mask
                                    ) %>%
          gdalcubes::select_bands(mask$band) %>%
          gdalcubes::reduce_time(paste0("count("
                                        , mask$band
                                        , ")"
                                        )
                                 )
        
        gdalcubes::write_tif(r
                             , dir = settings[["sat_save_dir", exact = TRUE]]
                             , prefix = "pixel_count__"
                             )
        
        r <- terra::rast(pixel_file)
        names(r) <- "pixel_count"
        terra::update(r, names = TRUE)
        
      }
      
    }
    
    
    # process bands------
    
    purrr::walk(head(get_bands, -1)
                , function(x) {
                  
                  message(x)
                  
                  out_file <- fs::path(settings[["sat_save_dir", exact = TRUE]]
                                       , paste0(gsub("nbart_", "", x), "__", start_date, ".tif")
                                       )
                  
                  run <- if(!file.exists(out_file)) TRUE else force_new
                  
                  if(run) {
                    
                    r <- gdalcubes::raster_cube(col
                                                , v_num
                                                , mask = cloud_mask
                                                ) %>%
                      gdalcubes::select_bands(x) %>%
                      gdalcubes::reduce_time(names = x
                                             , FUN = function(a) {
                                               
                                               median(a, na.rm = TRUE)
                                               
                                               }
                                             )
                    
                    gdalcubes::write_tif(r
                                         , dir = settings[["sat_save_dir", exact = TRUE]]
                                         , prefix = paste0(gsub("nbart_", "", x), "__")
                                         )
                    
                    }
                  
                }
                
    )
    
    
    # process indices-------
    
    purrr::iwalk(indices
                 , ~ {
                   
                   message(.y)
                   
                   out_file <- fs::path(settings[["sat_save_dir", exact = TRUE]]
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
                                          , dir = settings[["sat_save_dir", exact = TRUE]]
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
                     
                     out_file <- fs::path(settings[["sat_save_dir", exact = TRUE]]
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
                       
                       temp_file <- fs::path(settings[["sat_save_dir", exact = TRUE]]
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
      
      stack <- fs::dir_info(settings[["sat_save_dir", exact = TRUE]]
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
  