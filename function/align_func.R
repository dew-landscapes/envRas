  
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