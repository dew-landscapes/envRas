
save_cube_layer <- function(base_grid
                            , layer
                            , start_date
                            , end_date
                            , cloud_mask = NULL
                            , base_dir
                            , settings
                            , ...
                            ) {
  
  bbox <- sf::st_bbox(base_grid) |>
    sf::st_as_sfc() |>
    sf::st_transform(crs = settings$crs$decdeg) |> # need decimal lat/long for rstac
    sf::st_bbox()
  
  items <- rstac::stac(settings$source_url) |>
    rstac::stac_search(collections = settings$collection
                       , bbox = bbox
                       , datetime = paste0(as.character(start_date)
                                           , "/"
                                           , as.character(end_date)
                                           )
                       ) |>
    rstac::get_request() |>
    rstac::items_fetch()
  
  layer_to_get <- items |>
    rstac::items_assets() |>
    grep(paste0(layer, "$"), x = _, value = TRUE)
  
  coll <- gdalcubes::stac_image_collection(items$features
                                           , asset_names = layer_to_get
                                           )
  
  use_extent <- c(as.list(sf::st_bbox(base_grid)) |> purrr::set_names(c("left", "bottom", "right", "top"))
                  , t0 = as.character(start_date)
                  , t1 = as.character(end_date)
                  )
  
  view <- gdalcubes::cube_view(srs = paste0("EPSG:", terra::crs(base_grid, describe = T)$code)
                               , dx = terra::res(base_grid)[1]
                               , dy = terra::res(base_grid)[2]
                               , dt = settings$grain$temp
                               , extent = use_extent
                               , aggregation = "median"
                               , resampling = "bilinear"
                               )
  
  r <- gdalcubes::raster_cube(coll
                              , view
                              , mask = if(!is.null(cloud_mask)) cloud_mask else NULL
                              ) %>%
    gdalcubes::select_bands(layer_to_get)
  
  res <- gdalcubes::write_tif(r
                              , dir = base_dir
                              , prefix = paste0(layer
                                                , "__"
                                                )
                              , ...
                              )
  
  return(res)
  
}


                         