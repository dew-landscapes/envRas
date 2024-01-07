

  # mung ----------
  
  out_names <- c("source"
                 , "collection"
                 , "res"
                 , "period"
                 , "epoch"
                 , "season"
                 , "layer"
                 )

  epoch_seasons <- envFunc::make_epochs(start_year = settings[["start_year", exact = TRUE]]
                                        , end_year = settings[["end_year", exact = TRUE]]
                                        , epoch_step = 10
                                        , epoch_overlap = FALSE
                                        ) %>%
    tidyr::unnest(cols = c(years)) %>%
    dplyr::rename(year = years) %>%
    dplyr::select(year, epoch) %>%
    dplyr::left_join(settings[["seasons", exact = TRUE]]$seasons) %>%
    dplyr::left_join(results) %>%
    dplyr::filter(!is.na(path)) %>%
    tidyr::nest(data = -c(source, collection, aoi, buffer, res, layer, epoch, season)) %>%
    tidyr::unite(col = "out_file", tidyselect::any_of(out_names), sep = "__", remove = FALSE) %>%
    dplyr::mutate(stack = purrr::map(data
                                     , ~ terra::rast(.$path)
                                     )
                  , out_file = fs::path(settings[["munged_dir", exact = TRUE]]
                                        , paste0(out_file, ".tif")
                                        )
                  , done = file.exists(out_file)
                  ) %>%
    dplyr::left_join(get_layers)
  
  fs::dir_create(settings[["munged_dir", exact = TRUE]])
  
  align_func <- function(stack, out_file, func, base, ...) {
    
    reproj <- stack %>%
      terra::app(fun = func
                 , ...
                 ) %>%
      terra::project(terra::crs(base))
    
    ratio <- terra::res(reproj) / terra::res(base)
    
    reproj %>%
      terra::disagg(ratio) %>%
      terra::project(base) %>%
      terra::writeRaster(out_file
                         , overwrite = FALSE
                         )
    
    return(invisible(NULL))
    
    
    if(FALSE) {
      
      if(is.character(base)) base <- base %>% stars::read_stars()
      if("SpatRaster" %in% class(base)) base <- terra::sources(base)[[1]] %>% stars::read_stars()
      
      dates <- names(stack)
      
      stack %>%
        stars::st_as_stars(proxy = TRUE) %>%
        stars::st_set_dimensions(3, values = dates, names = "date")
        stars::st_as_stars(proxy = TRUE
                           , along = time
                           ) %>%
        aggregate(by = "10 years"
                  , FUN = get(func)
                  , na.rm = TRUE
                  ) %>%
        stars::st_warp(dest = base
                       , method = "bilinear"
                       , use_gdal = TRUE
                       , no_data_value = -Inf
                       ) %>%
        stars::write_stars(out_file)
      
      return(invisible(NULL))
        
    }
    
  }
  
  purrr::pwalk(list(epoch_seasons$stack[!epoch_seasons$done]
                    , epoch_seasons$out_file[!epoch_seasons$done]
                    , epoch_seasons$func[!epoch_seasons$done]
                    )
               , function(a, b, c) align_func(stack = a
                                              , out_file = b
                                              , func = c
                                              , base = settings$base
                                              , na.rm = TRUE
                                              )
               )
  