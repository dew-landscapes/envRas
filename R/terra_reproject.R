terra_reproject <- function(in_file
                            , base_grid_path
                            , out_file
                            , force_new = TRUE
                            , ...
                            ) {
  
  if(!file.exists(in_file)) {
    
    stop("Need to create "
         , in_file
         , "\ni.e. settings$grain$res of 90 m"
         )
    
  }
  
  if(any(!file.exists(out_file), force_new)) {
    
    terra::project(terra::rast(in_file)
                   , y = terra::rast(out_file)
                   , ...
                   )
    
  }
  
  return(out_file)
  
}