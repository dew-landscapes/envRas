  
  get_cli_data <- function(files
                           , out_file
                           , func
                           , scale
                           , offset
                           , base
                           , force_new = FALSE
                           ) {
    
    run <- if(file.exists(out_file)) force_new else TRUE
    
    if(run) {
      
      if("data.frame" %in% class(files)) {
        
        files <- files[,1][[1]]
        
      }
        
      safe_nc <- purrr::safely(stars::read_ncdf)
      
      r <- purrr::map(files
                      , safe_nc
                      , proxy = TRUE
                      ) %>%
        purrr::map("result") %>%
        purrr::compact()
      
      
      if(length(r)) {
      
        r %>%
          purrr::map(sf::st_set_crs, settings$epsg_latlong) %>%
          purrr::map(`[`
                     , i = settings$boundary %>%
                       sf::st_transform(crs = settings$epsg_latlong) %>%
                       sf::st_bbox()
                     ) %>%
          purrr::map(stars::st_as_stars
                     , proxy = FALSE
                     ) %>%
          do.call("c", .) %>%
          aggregate(by = "3 months"
                    , FUN = get(func)
                    , na.rm = TRUE
                    ) %>%
          terra::rast() %>% # switch to terra as stars::write_stars seemed to have trouble with Inf values
          terra::project(y = terra::crs(base)) %>%
          terra::writeRaster(filename = out_file
                             , names = gsub("\\.tif", "", basename(out_file))
                             , datatype = "INT2S"
                             , scale = scale
                             , offset = offset
                             , gdal = c("COMPRESS = NONE")
                             , overwrite = TRUE
                             )
        
      }
      
    }
      
    return(invisible(NULL))
    
  }
  