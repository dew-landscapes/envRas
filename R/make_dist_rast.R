make_dist_rast <- function(base_grid_path
                           , extent = NULL
                           , sf
                           , terra_options = NULL
                           ) {
  
  
    
  ## terra options -------
  if(!is.null(terra_options)) {

    do.call(terra::terraOptions
            , args = terra_options
            )

  }
    
  r <- terra::rast(base_grid_path)
    
  if(!is.null(extent)) {
    
    terra::window(r) <-  terra::ext(as.numeric(extent[1, 1:4]))
    
  }
    
  if(! all.equal(sf::st_crs(sf), sf::st_crs(r))) {
    
    sf <- sf |>
      sf::st_transform(crs = sf::st_crs(r)) |>
      sf::st_make_valid()
    
  }
    
  terra::distance(r
                  , y = sf
                  )
  
}