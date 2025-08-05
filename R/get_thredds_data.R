  
get_thredds_data <- function(files
                             , files_epsg = 4283
                             , func = "median"
                             , bbox = NULL
                             , base = NULL
                             , project_to_base = FALSE
                             , out_file
                             , scale = 1
                             , offset = 0
                             , force_new = FALSE
                             , ...
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
                    )
    
    if(!is.null(r$error)) {
      
      readr::write_lines(r$error
                         , paste0(gsub("tif$", "log", out_file))
                         , append = TRUE
                         )
      
    }
    
    r <- r %>%
      purrr::map("result") %>%
      purrr::compact()
    
    if(length(r)) {
      
      use_bbox <- if(!is.null(base)) {
        
        sf::st_bbox(base) %>%
          sf::st_as_sfc() %>%
          sf::st_transform(crs = files_epsg) %>%
          sf::st_bbox()
        
      } else if(!is.null(bbox)) {
        
        bbox
        
      } else {
        
        sf::st_bbox(r)
        
      }
    
      r <- r %>%
        purrr::map(sf::st_set_crs, files_epsg) %>%
        purrr::map(`[`
                   , i = sf::st_bbox(use_bbox)
                   ) %>%
        purrr::map(stars::st_as_stars
                   , proxy = FALSE
                   ) %>%
        do.call("c", .) %>%
        stars::st_redimension()
      
      r_name <- names(r)
      
      r <- r %>%
        terra::rast() %>%
        terra::app(fun = get(func), na.rm = TRUE)
      
      terra::set.names(r, r_name)
      
      if(project_to_base) {
        
        if(!is.null(base)) {
          
          if("SpatRaster" %in% class(base)) {
        
            r <- r %>%
              terra::project(y = base
                             , ...
                             )
            
          }
          
        }
        
      }
      
      terra::writeRaster(r
                         , filename = out_file
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
  