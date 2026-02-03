make_dist_tile <- function(base_grid_path
                           , extent
                           , vect_line_file
                           , terra_options = NULL
                           , vect_mask_file = NULL
                           , dist_limit = 10000
                           , out_dir
                           , ...
                           ) {
  
  ## terra options -------
  if(!is.null(terra_options)) {

    do.call(terra::terraOptions
            , args = terra_options
            )

  }
  
  out_file <- fs::path(out_dir, paste0(extent$tile_name, ".tif"))
  
  if(!dir.exists(out_dir)) fs::dir_create(out_dir)
    
  r <- terra::rast(base_grid_path)
    
  terra::window(r) <-  terra::ext(as.numeric(extent[1, 1:4]))
  
  use_lines <-
    sf::st_intersection(x = sfarrow::st_read_parquet(vect_line_file)
                        , y = terra::ext(r) |>
                          terra::extend(dist_limit) |>
                          terra::as.polygons(crs = terra::crs(r)) |>
                          sf::st_as_sf()
                        ) |>
    sf::st_union() |>
    terra::vect()
    
  if(length(use_lines)) {
    
    d <- terra::distance(r
                         , y = use_lines
                         )
    
  } else {
    
    d <- terra::app(r
                    , fun = \(x) x * dist_limit
                    )
    
  }
  
  if(!is.null(vect_mask_file)) {
    
    use_mask <-
      sf::st_intersection(x = sfarrow::st_read_parquet(vect_mask_file)
                          , y = terra::ext(r) |>
                            terra::extend(dist_limit) |>
                            terra::as.polygons(crs = terra::crs(r)) |>
                            sf::st_as_sf()
                          ) |>
      sf::st_union() |>
      terra::vect()
    
    m <- terra::mask(r
                     , mask = use_mask
                     , updatevalue = -1
                     , inverse = TRUE
                     )
    
  } else m <- r
    
  terra::lapp(x = terra::sds(d, m)
              , fun = \(x, y, limit = dist_limit) {
                
                pmin(x, limit) * y
                
              }
              , filename = out_file
              , overwrite = TRUE
              , ...
              )

  return(out_file)
  
}
