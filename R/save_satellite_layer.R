
save_satellite_layer <- function(items
                            , base_grid
                            , bbox
                            , layer
                            , agg_func = "median"
                            , start_date
                            , end_date
                            , cloud_mask = NULL
                            , base_dir
                            , period
                            , cores = parallel::detectCores() * 3 / 4
                            , force_new = TRUE
                            , ...
                            ) {
  
  out_file <- fs::path(base_dir
                       , paste0(layer, "__", agg_func, "__", start_date, ".tif")
                       )
  
  if(any(!file.exists(out_file), force_new)) {
  
    gdalcubes::gdalcubes_options(parallel = cores)
    
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
                                 , dt = period
                                 , extent = use_extent
                                 , aggregation = agg_func
                                 , resampling = "bilinear"
                                 )
    
    r <- gdalcubes::raster_cube(coll
                                , view
                                , mask = if(!is.null(cloud_mask)) cloud_mask else NULL
                                ) |>
      gdalcubes::select_bands(layer_to_get)
    
    res <- gdalcubes::write_tif(r
                                , dir = base_dir
                                , prefix = paste0(layer
                                                  , "__"
                                                  , agg_func
                                                  , "__"
                                                  )
                                , ...
                                )
    
  }
  
  return(out_file)
  
}


                         