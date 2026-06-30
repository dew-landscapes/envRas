make_env_df <- function(points_df
                        , ras_df
                        , in_epsg = 4326
                        , x = "long"
                        , y = "lat"
                        ) {

  env <- if(nrow(points_df) > 0) {

    df_distinct <- points_df |>
      dplyr::distinct(!!rlang::ensym(x), !!rlang::ensym(y)) |>
      dplyr::arrange(!!rlang::ensym(x), !!rlang::ensym(y))

    use_aoi <- df_distinct |>
      dplyr::reframe(range_x = range(!!rlang::ensym(x), na.rm = TRUE)
                       , range_y = range(!!rlang::ensym(y), na.rm = TRUE)
                       )

    r <- envRaster::make_env_stack(predictors = ras_df$path)

    terra::window(r) <- terra::ext(min(use_aoi$range_x)
                                   , max(use_aoi$range_x)
                                   , min(use_aoi$range_y)
                                   , max(use_aoi$range_y)
                                   ) |>
      terra::vect(crs = paste0("epsg:", in_epsg)) |>
      sf::st_as_sf() |>
      sf::st_segmentize(dfMaxLength = 50000) |>
      sf::st_transform(crs = sf::st_crs(r)) |>
      terra::vect()

    envRaster::add_raster_bin(ras = r
                              , df = df_distinct
                              , crs_df = in_epsg
                              , add_val = TRUE
                              , x = x
                              , y = y
                              ) |>
      dplyr::distinct()

  } else NULL

  return(env)

}
