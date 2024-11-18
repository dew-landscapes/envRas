

  name_from <- name_env_tif(dirname(settings[["sat_month_cube", exact = TRUE]][[1]]), parse = TRUE) %>%
    dplyr::filter(grepl("gdvi", path)) %>%
    dplyr::pull(path)
  
  name_to <- gsub("gdvi", "ndwi", name_from)
  
  fs::file_move(name_from, name_to)
  
  
  static_name_from <- name_env_tif(gsub("P1M", "P120M--P3M", settings$sat_month_cube)
                             , parse = TRUE
                             ) %>%
    dplyr::filter(grepl("gdvi", path)) %>%
    dplyr::pull(path)
  
  static_name_to <- gsub("gdvi", "ndwi", static_name_from)
  
  fs::file_move(static_name_from, static_name_to)
  