
make_epoch_layer <- function(tif_paths
                             , func = "median"
                             , ...
                             ) {
    
    
    terra::app(terra::rast(tif_paths)
               , fun = func
               , ...
               )
    
    return(invisible(out_file))
    
  }
  