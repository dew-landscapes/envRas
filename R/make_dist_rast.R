make_dist_rast <- function(base_grid_path
                           , extent = NULL
                           , sf_line
                           , terra_options = NULL
                           , name = "distance"
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
    
  terra::distance(r
                  , y = terra::vect(sf_line)
                  , names = name
                  )
  
}