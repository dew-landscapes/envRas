terra_app <- function(ras_files
                      , func = "mean"
                      , out_file
                      , force_new = TRUE
                      , ...
                      ) {
  
  if(any(!file.exists(out_file), force_new)) {
    
    terra::rast(ras_files) |>
      terra::app(fun = get(func)
                 , na.rm = TRUE
                 , filename = out_file
                 , overwrite = TRUE
                 )
    
  }
  
  return(out_file)
  
}