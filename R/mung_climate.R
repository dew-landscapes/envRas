mung_climate <- function(files_df
                         , func = "mean"
                         , aoi
                         , out_file
                         , force_new = FALSE
                         ) {
  
  if(any(!file.exists(out_file), force_new)) {
    
    if(!is.data.frame(files_df)) files_df <- files_df[[1]]
    
    r <- terra::rast(files_df$path)
    
    terra::window(r) <- terra::vect(aoi |> sf::st_transform(crs = sf::st_crs(r)))
    
    terra::app(r
               , fun = get(func)
               , filename = out_file
               , overwrite = TRUE
               )
    
  }
  
  return(out_file)
  
}
