save_climate_layer <- function(layer
                               , settings
                               , start_date
                               , end_date
                               , func = "median"
                               , bbox
                               , base_dir
                               , files_epsg = 4283
                               , force_new = TRUE
                               ) {
  
  # out file ------
  out_file <- fs::path(base_dir
                       , paste0(settings$source
                                , "__"
                                , settings$collection
                                )
                       , paste0(layer
                                , "__"
                                , start_date
                                , ".tif"
                                )
                       )
  
  if(any(!file.exists(out_file), force_new)) {
  
    fs::dir_create(dirname(out_file))
    
    # func -------
    ras_func <- if(grepl("min$", layer)) {
      
      "min"
      
    } else if(grepl("max$", layer)) {
      
      "max"
      
    } else func
    
    # files --------
    files <- envFunc::make_seasons(start_year = as.numeric(format(start_date, "%Y"))
                          , end_year = as.numeric(format(end_date, "%Y"))
                          , include_all = FALSE
                          , dec_adjust = FALSE
                          )$months |>
      dplyr::mutate(season = "all") |>
      dplyr::mutate(file_specific = format(start_date, "%Y%m")
                    , file = paste0(settings$source_url
                                    , "/"
                                    , layer
                                    , "/"
                                    , year
                                    , "/"
                                    , paste0("ANUClimate_v2-0_"
                                             , layer
                                             , "_monthly_"
                                             , file_specific
                                             , ".nc"
                                             )
                                    )
                    )
    
    safe_ncdf <- purrr::safely(stars::read_ncdf)
    
    use_bbox <- sf::st_as_sfc(bbox) |>
      sf::st_transform(crs = files_epsg) |>
      sf::st_bbox()
    
    r <- purrr::map(files$file
                    , safe_ncdf
                    , proxy = TRUE
                    ) |>
      purrr::map("result") |>
      purrr::compact() |>
      purrr::map(sf::st_set_crs, files_epsg) |>
      purrr::map(`[`
                 , i = sf::st_bbox(use_bbox)
                 ) |>
      purrr::map(stars::st_as_stars
                 , proxy = FALSE
                 ) %>%
      do.call("c", .) |>
      stars::st_redimension() |>
      terra::rast() |>
      terra::app(fun = get(func)
                 , na.rm = TRUE
                 , filename = out_file
                 , wopt = list(names = layer)
                 , overwrite = TRUE
                 )
    
  }
    
  return(out_file)
  
}
