make_indice <- function(indice
                        , start_date
                        , layers
                        , base_dir
                        , settings
                        , terra_options = list(memfrac = 0.2)
                        ) {
  
  if(!is.null(terra_options)) {
    
    do.call(terra::terraOptions
            , args = terra_options
            )

  }
  
  indice_name <- indice
  
  indice <- settings$indices[names(settings$indices) == indice]
  
  a <- terra::rast(layers[1, grepl(indice[[1]][[1]], names(layers))][[1]])
  
  b <- terra::rast(layers[1, grepl(indice[[1]][[2]], names(layers))][[1]])
  
  i_func <- function(x, y) { (x - y) / (x + y) }
  
  r <- terra::lapp(c(a, b)
                   , fun = i_func
                   , filename = fs::path(base_dir, paste0(names(indice), "__", start_date, ".tif"))
                   , overwrite = TRUE
                   , wopt = list(names = indice_name)
                   )
  
  return(terra::sources(r))
  
}
