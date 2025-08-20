make_dist_rast <- function(base_grid_path
                           , sf
                           , out_file
                           , terra_options = NULL
                           , force_new = FALSE
                           ) {
  
  if(any(!file.exists(out_file), force_new)) {
    
    r <- terra::rast(base_grid_path)
    
    if(! all.equal(sf::st_crs(sf), sf::st_crs(r))) {
      
      sf <- sf |>
        sf::st_transform(crs = sf::st_crs(r)) |>
        sf::st_make_valid()
      
    }
    
    ## terra options -------
    if(!is.null(terra_options)) {
      
      do.call(terra::terraOptions
              , args = terra_options
              )
      
    }
    
    terra::distance(r
                    , y = sf
                    , filename = out_file
                    , wopt = list(datatype = "INT4S")
                    )
    
  }
  
  return(out_file)
  
}