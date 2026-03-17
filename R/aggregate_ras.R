
aggregate_ras <- function(input_ras_path
                          , base_grid_path
                          , in_res
                          , out_res
                          , force_new = TRUE
                          , proj_method = "bilinear"
                          , agg_func = "median"
                          ) {
  
  out_dir <- gsub(paste0("__", in_res, "\\/")
                  , paste0("__", out_res, "/")
                  , dirname(input_ras_path)
                  )
  
  out_file_name <- envRaster::name_env_tif(input_ras_path, parse = TRUE) |>
    dplyr::mutate(func = agg_func) |>
    envRaster::name_env_tif() |>
    dplyr::pull(out_file) |>
    basename()
  
  out_file <- fs::path(out_dir, out_file_name)
  
  if(any(!file.exists(out_file), force_new)) {
    
    fs::dir_create(dirname(out_file))
    
    base <- terra::rast(base_grid_path)
    
    r <- terra::rast(input_ras_path) |>
      terra::project(terra::crs(base)
                     , method = proj_method
                     )
    
    ratio <- terra::res(base) / terra::res(r)
      
    rat_1 <- floor(ratio)
    
    r <- r |>
      terra::aggregate(fact = rat_1
                       , fun = agg_func
                       , na.rm = TRUE
                       ) |>
      terra::project(base
                     , method = proj_method
                     , filename = out_file
                     , overwrite = TRUE
                     )
    
  }
    
  return(out_file)
  
}
  