

  bak <- sf::st_read(fs::path("D:"
                              , "env"
                              , "data"
                              , "polygon"
                              #, paste0(settings$use_aoi
                                       , "bak.shp"
                              )
                     )


  aoi_grid <- terra::rast(fs::path("D:"
                                   , "env"
                                   , "data"
                                   , "raster"
                                   , "static"
                                   , "Boundary"
                                   , "south_aus_cover_30m.tif"
                                   )
                          )


  dots_list <- get_sat_sits(
    # sits_cube args
    source = "MPC"
    , collection = "LANDSAT-C2-L2"
    , data_dir = "D:/env/data/raster"  # fs::path("D:", "env", "data", "raster")
    , roi = bak
    , start_date = "2022-12-31"
    , end_date = "2022-12-31"
    # sits_regularize args
    , period = "P1M"
    , res = 30
    , multicores = 5
    )


  if(FALSE) {

    # Plot a single image

    local_cube %>%
      dplyr::filter(tile == local_cube$tile[1]) %>%
      plot(red = "RED", green = "GREEN", blue = "BLUE")

    local_cube %>%
      dplyr::filter(tile == local_cube$tile[2]) %>%
      dplyr::pull(file_info) %>%
      `[[`(1) %>%
      dplyr::filter(band == "NDMI") %>%
      dplyr::slice(20) %>%
      dplyr::pull(path) %>%
      terra::rast() %>%
      terra::plot()

  }
