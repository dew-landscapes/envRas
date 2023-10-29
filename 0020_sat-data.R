
  # get data-------
  
  fs::dir_create(settings$sat_save_dir)
  
  safe_sat <- purrr::safely(make_sat_data)
  
  purrr::walk2(stacks$start_date
               , stacks$end_date
               , safe_sat
               , settings = settings
               , force_new = F
               )
  
  
  # results-------
  
  results <- fs::dir_info(settings$sat_save_dir
                          , regexp = "tif$"
                          ) %>%
    dplyr::arrange(desc(modification_time)) %>%
    dplyr::mutate(name = gsub("\\.tif", "", basename(path))
                  , name2 = basename(dirname(path))
                  ) %>%
    dplyr::select(path, name, name2) %>%
    tidyr::separate(name, into = c("layer", "start_date"), sep = "__") %>%
    tidyr::separate(name2,into = c("source", "collection", "aoi", "buffer", "res")
                    , sep = "__"
                    ) %>%
    dplyr::mutate(start_date = as.Date(start_date))
  
  if(FALSE) {
    
    # Test results
    
    temp <- results %>%
      #dplyr::filter(grepl("count|water", path)) %>%
      dplyr::slice(1:9) %>%
      dplyr::pull(path) %>%
      terra::rast()
  
    terra::plot(temp)
    
  }
  
  
