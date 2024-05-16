  
  align_cli <- function(s, out_file, func
                        , scale, offset, base
                        ) {
    
    r <- terra::app(s
                    , func
                    , na.rm = TRUE
                    ) %>%
      terra::project(terra::crs(base)
                     , method = "near"
                     )
    
    names(r) <- gsub("\\.tif", "", basename(out_file))
    
    ratio <- terra::res(r) / terra::res(base)
    
    rat_1 <- floor(sqrt(ratio))
    
    r <- r %>%
      terra::disagg(rat_1
                    , method = "bilinear"
                    ) %>%
      terra::project(base
                     , method = "bilinear"
                     )
    
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
  