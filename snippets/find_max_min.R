

  dir <- "H:/data/raster/cube__P3M/NCI__ANUClimate__sa_ibrasub_xn____0"
  
  layers <- c("rain","evap","tavg","vpd","tmin","tmax")
  
  fs::dir_info(dir) %>%
    dplyr::select(path) %>%
    dplyr::mutate(layer = gsub("_.*", "", basename(path))) %>%
    tidyr::nest(data = path) %>%
    dplyr::mutate(r = purrr::map(data, \(x) terra::rast(x$path))
                  , values = purrr::map(r, \(x) terra::values(x))
                  , min = purrr::map_dbl(values, \(x) min(x[!is.infinite(x)], na.rm = TRUE))
                  , max = purrr::map_dbl(values, \(x) max(x[!is.infinite(x)], na.rm = TRUE))
                  )
  