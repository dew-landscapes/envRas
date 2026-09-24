save_canopy_layer <- function(aoi_sf
                              , base_grid_path
                              , out_file
                              , force_new = FALSE
                              , ...
                              ) {
  
  run <- if(!file.exists(out_file)) TRUE else force_new
  
  if(run) {
  
    tiles <- forestdata::fd_canopy_height(x = aoi_sf
                                          , model = "eth"
                                          , layer = "chm"
                                          )
    
    r <- if("SpatRasterCollection" %in% class(tiles)) terra::merge(tiles) else tiles
    
    terra::project(x = r
                   , y = terra::rast(base_grid_path)
                   , method = "median"
                   , filename = out_file
                   , ...
                   )
    
  }
  
  return(out_file)
  
}