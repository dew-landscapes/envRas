download_nc <- function(save_file
                        , remote_files
                        , bbox
                        , files_epsg = 4283
                        , func = mean
                        , force_new = FALSE
                        ) {
  
  if(any(!file.exists(save_file), force_new)) {
    
    safe_ncdf <- purrr::safely(stars::read_ncdf)
    
    use_bbox <- sf::st_as_sfc(bbox) |>
      sf::st_transform(crs = files_epsg) |>
      sf::st_bbox()
    
    r <- purrr::map(remote_files
                    , safe_ncdf
                    , proxy = TRUE
                    ) |>
      purrr::map("result") |>
      purrr::compact()
    
    if(length(r)) {
    
      r |>
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
        terra::app(fun = func
                   , filename = save_file
                   , overwrite = TRUE
                   , wopt = list(names = gsub("__.*", "", basename(save_file)))
                   )
      
      }
                        
  }
  
  return(save_file)

}
