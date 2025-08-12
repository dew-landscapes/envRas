make_indice <- function(indice
                        , start_date
                        , layers
                        , base_dir
                        , settings
                        , terra_options = list(memfrac = 0.2)
                        , force_new = TRUE
                        ) {
  
  out_file <- fs::path(base_dir, paste0(names(indice), "__", start_date, ".tif"))
  
  if(any(! file.exists(out_file), force_new)) {
  
    if(!is.null(terra_options)) {
      
      do.call(terra::terraOptions
              , args = terra_options
              )
  
    }
    
    a <- terra::rast(layers[grepl(indice[[1]][[1]], names(layers))][[1]])
    
    b <- terra::rast(layers[grepl(indice[[1]][[2]], names(layers))][[1]])
    
    i_func <- function(x, y) { (x - y) / (x + y) }
    
    r <- terra::lapp(c(a, b)
                     , fun = i_func
                     , filename = out_file
                     , overwrite = TRUE
                     , wopt = list(names = names(indice))
                     )
    
  }
    
  return(out_file)
  
}
