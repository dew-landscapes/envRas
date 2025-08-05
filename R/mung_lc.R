

  mung_lc <- function(paths
                      , out_path
                      , aoi = NULL
                      , base = NULL
                      ) {
    
    mung_one <- function(x) {
      
      # read in raster
      r <- terra::rast(x)
      
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
       
      # make a presence/absence raster for each level in r
      r <- r %>%
        terra::segregate()
      
      gc()
      
      # apply the names
      # make a dataframe with the names of each level in r
      names_r <- rio::import(fs::path("data", "luDLCD.csv")
                             , setclass = "tibble"
                             ) %>%
        dplyr::mutate(ras_num = (Class)
                      , ras_name = as.character(ras_num)
                      , use_name = gsub("\\s", "_", tolower(stringr::str_squish(`Common name`)))
                      )
      
      names(r) <- purrr::map(names(r)
                             , ~ names_r$use_name[which(names_r$ras_name == (.))]
                             )
      
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
      
      return(r)
      
    }
    
    munged_paths <- purrr::map(paths, mung_one)
    
    n_layers <- terra::nlyr(munged_paths[[1]])
    names_layers <- gsub("\\s", "_", tolower(stringr::str_squish(names(munged_paths[[1]]))))
    
    s <- terra::rast(munged_paths) %>%
      terra::tapp(index = 1:n_layers
                  , fun = mean
                  )
    
    names(s) <- names_layers
    
    purrr::walk(1:nlyr(s)
                , \(x) terra::writeRaster(s[[x]]
                                          , filename = paste0(out_path, "__", names(s)[x], ".tif")
                                          )
                )
    
  }
  