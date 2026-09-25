
aggregate_ras <- function(input_ras_path
                          , base_grid_path
                          , in_res
                          , out_res
                          , force_new = TRUE
                          , proj_method = "bilinear"
                          , agg_func = "mean"
                          ) {
  
  cube_dir <- dirname(dirname(dirname(dirname(input_ras_path))))
  
  dir_df <- envRaster::name_env_tif(input_ras_path
                                     , parse = TRUE
                                     ) |>
    dplyr::mutate(res_x = out_res
                  , res_y = out_res
                  ) |>
    envRaster::name_env_tif(dir_only = TRUE)
  
  out_file <- fs::path(cube_dir
                       , dir_df$out_dir
                       , gsub("__.*__", paste0("__", agg_func, "__"), basename(input_ras_path))
                       )
  
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
  