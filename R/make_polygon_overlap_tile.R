make_polygon_overlap_tile <- function(base_grid_path
                                      , extent
                                      , polygon_file
                                      , polygon_field
                                      , polygon_func = "count"
                                      , tile_background = 0 # min_fireyear <- (min(polys$fireyear) - 1)
                                      , tile_buffer = 1000
                                      , terra_options = NULL
                                      , out_dir
                                      , force_new = TRUE
                                      , test_tile = TRUE
                                      , ...
                                      ) {
  
  out_file <- fs::path(out_dir, paste0(extent$tile_name, ".tif"))
  
  if(test_tile) {
    
    safe_rast <- purrr::safely(terra::rast)
    
    test <- safe_rast(out_file)
    
    if(!is.null(test$error)) force_new <- TRUE
    
  }
  
  if(any(!file.exists(out_file), force_new)) {
      
    ## terra options -------
    if(!is.null(terra_options)) {
      
      do.call(terra::terraOptions
              , args = terra_options
              )
      
    }
      
    r <- terra::rast(base_grid_path)
      
    terra::window(r) <-  terra::ext(as.numeric(extent[1, 1:4]))
    
    polys <-
      sfarrow::st_read_parquet(polygon_file) |>
      # cut back to just the current extent
      sf::st_intersection(y = terra::ext(r) |>
                            terra::extend(tile_buffer) |>
                            terra::as.polygons(crs = terra::crs(r)) |>
                            sf::st_as_sf()
                          )
    
    if(nrow(polys)) {
      
      tile <- terra::rasterize(terra::vect(polys)
                               , field = polygon_field
                               , r
                               , fun = polygon_func
                               , na.rm = TRUE
                               , background = tile_background
                               )
      
    } else {
      
      tile <- terra::rast(r, vals = tile_background)
      
    }
      
    terra::writeRaster(x = tile
                       , filename = out_file
                       , overwrite = TRUE
                       , ...
                       )
    
  }
    
  return(out_file)
    
}
