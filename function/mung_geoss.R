

  mung_geoss <- function(path
                         , out_path
                         , aoi = NULL
                         , base = NULL
                         ) {
    
    # read in raster
    r <- terra::rast(path)
    
    # cut raster to aoi, if provided
    if(!is.null(aoi)) {
      
      r <- r %>%
        terra::crop(aoi %>%
                      sf::st_transform(crs = sf::st_crs(r))
                    ) %>%
        terra::mask(aoi %>%
                      sf::st_transform(crs = sf::st_crs(r))
                    , touches = TRUE
                    )
      
      gc()
      
    }
    
    # align crs of raster and base
    if(terra::crs(r) != terra::crs(base)) {
      
      r <- r %>%
        terra::project(y = terra::crs(base)
                       , method = "near"
                       )
      
      gc()
       
    }
    
    # make a dataframe with the names of each level in r
    names_r <- terra::levels(r)[[1]]
     
    # make a presence/absence raster for each level in r
    r <- r %>%
      terra::segregate()
    
    gc()
    
    # apply the names 
    names(r) <- gsub("\\s", "_", names_r[,2])
    
    gc()
    
    use_ratio <- ceiling(terra::res(base)[[1]] / terra::res(r)[[1]])
    
    r <- r %>%
      terra::aggregate(use_ratio
                       , fun = \(x) sum(x == 1, na.rm = TRUE) / sum(!is.na(x))
                       )
    
    gc()
     
    r <- r %>%
      terra::project(base)
    
    gc()
     
    purrr::walk(names(r)
                , \(x) terra::writeRaster(r[x]
                                          , filename = paste0(out_path, "__", x, ".tif")
                                          )
                )
     
    rm(list = ls())
    
    gc()
    
    return(invisible(NULL))
    
  }
  