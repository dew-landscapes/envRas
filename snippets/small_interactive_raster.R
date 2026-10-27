bbox <- st_bbox(
  c(
    xmin = 1280012.81, 
    ymin = 1589890.12, 
    xmax = 1421288.16, 
    ymax = 1751474.74
  ),
  crs = st_crs(8059)
) |>
  sf::st_as_sfc() |>
  sf::st_as_sf()

r <- tar_read(wc, store = tars$watercourse$store) |>
  envRaster::make_env_stack(aoi = bbox)

tm_shape(r) +
  tm_raster()

