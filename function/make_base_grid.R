

  make_base_grid <- function(aoi
                             , base_res
                             , base_epsg
                             , lcm = 9000 # numbers::mLCM(c(1, 2, 3, 4, 5, 10, 20, 30, 50, 90, 100, 500, 1000))
                             , name = "base"
                             , use_mask = NULL
                             , out_file = NULL
                             ) {
    
    aoi_extent <- settings$boundary %>%
      sf::st_transform(crs = base_epsg) %>%
      sf::st_bbox() %>%
      round(-3)
    
    aoi_ew <- as.numeric(aoi_extent$xmax - aoi_extent$xmin)
    aoi_ns <- as.numeric(aoi_extent$ymax - aoi_extent$ymin)
    
    round_up <- function(d, div) div * ceiling(d / div)
    
    ew_dist <- round_up(aoi_ew, lcm)
    ns_dist <- round_up(aoi_ns, lcm)
    
    b <- terra::rast(crs = paste0("epsg:", base_epsg)
                     , xmin = aoi_extent$xmin[[1]] - (ew_dist / 2)
                     , xmax = aoi_extent$xmax[[1]] + (ew_dist / 2)
                     , ymin = aoi_extent$ymin[[1]] - (ns_dist / 2)
                     , ymax = aoi_extent$ymax[[1]] + (ns_dist / 2)
                     , resolution = base_res
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