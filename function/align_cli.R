  
  align_cli <- function(paths
                        , out_file
                        , func = "median"
                        , scale = 1
                        , offset = 0
                        , base = NULL
                        ) {
    
    r <- terra::app(terra::rast(paths)
                    , func
                    , na.rm = TRUE
                    )
    
    names(r) <- gsub("\\.tif", "", basename(out_file))
    
    if(!is.null(base)) {
      
      r <- r %>%
      terra::project(terra::crs(base)
                     , method = "near"
                     )
    
    
    
      ratio <- terra::res(r) / terra::res(base)
      
      rat_1 <- floor(sqrt(ratio))
      
      r <- r %>%
        terra::disagg(rat_1
                      , method = "bilinear"
                      ) %>%
        terra::project(base
                       , method = "bilinear"
                       )
      
    }
    
    terra::writeRaster(r
                       , filename = out_file
                       , names = gsub("\\.tif", "", basename(out_file))
                       , datatype = "INT2S"
                       , scale = scale
                       , offset = offset
                       , gdal = c("COMPRESS = NONE")
                       )
      
    return(invisible(NULL))
    
  }
  