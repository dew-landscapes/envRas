
  # plot the projected and latlong bboxes together
  tmap_mode("view")
  
  bbox_use_epsg <- settings$bbox_use_epsg %>%
    sf::st_as_sfc(crs = settings$epsg_proj)
  
  bbox_latlong <- settings$bbox_latlong %>%
    sf::st_as_sfc(crs = settings$epsg_latlong)
  
  tm_shape(bbox_use_epsg) +
    tm_borders() +
    tm_shape(bbox_latlong) +
    tm_borders(col = 'red')
  