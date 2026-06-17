save_canopy_layer <- function(aoi_sf
                              , base_grid_path
                              , out_file
                              , force_new = FALSE
                              ) {
  
  run <- if(!file.exists(out_file)) TRUE else force_new
  
  if(run) {
  
    tiles <- forestdata::fd_canopy_height(x = aoi_sf
                                          , model = "eth"
                                          , layer = "chm"
                                          )
    
    r <- terra::merge(tiles)
    
    terra::project(x = r
                   , y = terra::rast(base_grid_path)
                   , filename = out_file
                   , method = "median"
                   , overwrite = TRUE
                   , datatype = "INT1U"
                   , names = "chm"
                   )
    
  }
  
  return(out_file)
  
}