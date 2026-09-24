make_indice <- function(index_name
                        , path_1
                        , path_2
                        , scale
                        , offset
                        , terra_options = list(memfrac = 0.1)
                        , force_new = TRUE
                        ) {
  
  file_name <- gsub(".*__median", paste0(index_name, "__index"), basename(path_1))
  
  out_file <- fs::path(dirname(path_1), file_name)
  
  if(any(! file.exists(out_file), force_new)) {
  
    if(!is.null(terra_options)) {
      
      do.call(terra::terraOptions
              , args = terra_options
              )
  
    }
    
    a <- terra::rast(path_1)
    
    b <- terra::rast(path_2)
    
    i_func <- function(x, y) {
      
      (x - y) / (x + y)
      
    }
    
    r <- terra::lapp(c(a, b)
                     , fun = i_func
                     , filename = out_file
                     , overwrite = TRUE
                     , wopt = list(names = names(index_name)
                                   , datatype = "INT2S"
                                   , scale = scale
                                   , offset = offset
                                   )
                     )
    
    create_esri_xml(tif_path = out_file
                    , scale = scale
                    , offset = offset
                    )
    
  }
    
  return(out_file)
  
}
