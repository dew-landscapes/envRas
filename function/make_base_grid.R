

  make_base_grid <- function(aoi
                             , out_res
                             , out_epsg
                             , lcm = 9000 # numbers::mLCM(c(1, 2, 3, 4, 5, 10, 20, 30, 50, 90, 100, 500, 1000))
                             , name = "base"
                             , use_mask = NULL
                             , out_file = NULL
                             ) {
    
    # round extent to km
    aoi_extent <- aoi %>%
      sf::st_transform(crs = out_epsg) %>%
      sf::st_bbox() %>%
      round(-3)
    
    # ew and ns distance
    aoi_ew <- as.numeric(aoi_extent$xmax - aoi_extent$xmin)
    aoi_ns <- as.numeric(aoi_extent$ymax - aoi_extent$ymin)
    
    # a function to make sure x is divisible by m (x will be changed)
    round_up <- function(x, m) m * ceiling(x / m)
    
    # find new distance so that it will be divisible by lcm
    ew_dist <- round_up(aoi_ew, lcm)
    ns_dist <- round_up(aoi_ns, lcm)
    
    # mid points of original ew and ns
    ew_mid <- (aoi_extent$xmax + aoi_extent$xmin) / 2
    ns_mid <- (aoi_extent$ymax + aoi_extent$ymin) / 2
    
    # build new raster using extent based on mid points + or - half new distance
    b <- terra::rast(crs = paste0("epsg:", out_epsg)
                     , xmin = ew_mid - (ew_dist / 2)
                     , xmax = ew_mid + (ew_dist / 2)
                     , ymin = ns_mid - (ns_dist / 2)
                     , ymax = ns_mid + (ns_dist / 2)
                     , resolution = out_res
                     , vals = 1
                     , names = name
                     )
    
    if(!is.null(use_mask)) {
      
      stopifnot("sf" %in% class(use_mask))
      
      b <- b %>%
        terra::mask(use_mask)
      
    }
    
    if(!is.null(out_file)) {
      
      terra::writeRaster(b
                         , out_file
                         , datatype = "INT1U"
                         , gdal = c("COMPRESS=NONE") 
                         )
      
    }
    
    return(b)
    
  }
  