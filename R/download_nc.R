download_nc <- function(save_file
                        , remote_files
                        , bbox
                        , files_epsg = 4283
                        , func = "mean"
                        , force_new = FALSE
                        , base_grid_path = NULL
                        ) {
  
  if(any(!file.exists(save_file), force_new)) {
    
    safe_ncdf <- purrr::safely(stars::read_ncdf)
    
    use_bbox <- sf::st_as_sfc(bbox) |>
      sf::st_transform(crs = files_epsg) |>
      sf::st_bbox()
    
    # proxy object
    p <- purrr::map(remote_files
                    , safe_ncdf
                    , proxy = TRUE
                    ) |>
      purrr::map("result") |>
      purrr::compact()
    
    if(length(p)) {
      
      time_stamps <- purrr::map(p
                                , \(x) stars::st_get_dimension_values(x, which = "time") |>
                                  lubridate::as_date()
                                ) |>
        unlist() |>
        as.Date()
      
      r <- p |>
        purrr::map(sf::st_set_crs, files_epsg) |>
        purrr::map(`[`
                   , i = sf::st_bbox(use_bbox)
                   ) |>
        purrr::map(stars::st_as_stars
                   , proxy = FALSE
                   ) |>
        append(list(along = "time")) |>
        do.call(what = c, args = _) |>
        stars::st_set_dimensions(which = "time"
                                 , values = time_stamps
                                 ) |>
        terra::rast() |>
        terra::app(fun = get(func))
      
      terra::project(r
                     , y = terra::rast(base_grid_path)
                     , filename = save_file
                     , overwrite = TRUE
                     , names = gsub("__.*", "", basename(save_file))
                     )
      
      }
                        
  }
  
  return(save_file)

}
