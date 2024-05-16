
  # cube settings ------
  
  wofs_sets <- c(settings[c("use_extent"
                              , "epsg_proj"
                              , "wofs_collection"
                              , "bbox"
                              , "wofs_res"
                              , "wofs_dir"
                              )]
                   , period = "static"
                   )
  
  # 
  
  ## run cube -----
  
    purrr::pwalk(list(stacks$start_date
                      , stacks$end_date
                      , stacks$season
                      )
                 , \(x, y, z) safe_get_sat_data(x
                                                , y
                                                , z
                                                , sat_sets = sat_sets
                                                , layers = settings$sat_layers
                                                , indices = settings$sat_indices
                                                
                                                # dots
                                                , property_filter = \(x) {x[["eo:cloud_cover"]] < 50}
                                                )
                 )
  
  