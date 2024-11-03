

  tifs <- envRaster::name_env_tif(dirname(settings$sat_month_cube[[1]])
                                   , parse = TRUE
                                   )
  
  col <- gdalcubes::create_image_collection(files$path
                                            , date_time = files$start_date
                                            , band_names = files$layer
                                            )
  
  use_extent <- as.list(sf::st_bbox(settings$base))
  
  names(use_extent)[1:4] <- c("left", "bottom", "right", "top")
  
  cube <- gdalcubes::cube_view(extent = col
                               , srs = paste0("EPSG:", settings$epsg_proj)
                               , dx = settings$sat_res
                               , dy = settings$sat_res
                               , dt = settings$period
                               )
  