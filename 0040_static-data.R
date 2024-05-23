
  start_date <- "1970-01-01"
  end_date <- "1970-01-01"
  
  # run wofs -----
    
  items <- rstac::stac("https://explorer.sandbox.dea.ga.gov.au/stac") %>%
    rstac::stac_search(collections = settings[["wofs_filtered_summary", exact = TRUE]]
                       , bbox = settings[["bbox", exact = TRUE]]
                       , datetime = paste0(start_date
                                           , "/"
                                           , end_date
                                           )
                       ) %>%
    rstac::get_request() %>%
    rstac::items_fetch()
  
  col <- gdalcubes::stac_image_collection(items$features
                                          , asset_names = "wofs_filtered_summary"
                                          )
  
  use_extent <- c(settings[["use_extent", exact = TRUE]]
                  , t0 = as.character(start_date)
                  , t1 = as.character(end_date)
                  )

  v_num <- gdalcubes::cube_view(srs = paste0("EPSG:"
                                             , settings[["epsg_proj", exact = TRUE]]
                                             )
                                , extent = use_extent
                                , dx = settings[["sat_res", exact = TRUE]] 
                                , dy = settings[["sat_res", exact = TRUE]]
                                , dt = "P1D"
                                , aggregation = "median"
                                , resampling = "bilinear"
                                )   
  
  out_file <- fs::path(settings$wofs_dir
                       , paste0("wofs_filtered_summary__all__"
                                , start_date
                                , ".tif"
                                )
                       )
  
  run <- if(!file.exists(out_file)) TRUE else force_new
                      
  if(run) {
    
    if(!dir.exists(dirname(out_file))) fs::dir_create(dirname(out_file))
  
    r <- gdalcubes::raster_cube(col
                                , v_num
                                )
    
    gdalcubes::write_tif(r
                         , dir = dirname(out_file)
                         , prefix = paste0("wofs_filtered_summary"
                                           , "__all__"
                                           )
                         , pack = gdalcubes::pack_minmax(type = "int16"
                                                         , min = 0
                                                         , max = 1
                                                         )
                         , creation_options = list("COMPRESS" = "NONE")
                         )
    
  }
                      
  