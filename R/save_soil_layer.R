save_soil_layer <- function(paths_df
                            , key = Sys.getenv("TERN_API_KEY")
                            , in_crs
                            , grid_path
                            , out_file
                            , force_new = FALSE
                            , ... # passed to terra::project (and, possibly, on to terra::writeRaster)
                            ) {
  
  run <- if(!file.exists(out_file)) TRUE else force_new
  
  if(run) {
    
    bbox <- sf::st_bbox(terra::rast(grid_path)) |>
      sf::st_as_sfc() |>
      terra::vect() |>
      terra::densify(50000) |>
      sf::st_as_sf() |>
      sf::st_transform(crs = in_crs) |>
      sf::st_bbox()
    
    r <- SLGACloud::cogLoad(paths_df$COGsPath
                            , api_key = key
                            )
    
    terra::window(r) <- bbox
    
    r <- terra::app(r
                    , fun = "mean"
                    , na.rm = TRUE
                    )
    
    terra::project(x = r
                   , y = terra::rast(grid_path)
                   , filename = out_file
                   , ...
                   )
    
  }
  
  return(out_file)
  
}
