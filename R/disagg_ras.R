  
disagg_ras <- function(input_ras_path
                       , base_grid_path
                       , in_res
                       , out_res
                       , out_dir = dirname(base_grid_path)
                       , force_new = TRUE
                       , proj_method = "bilinear"
                       ) {
  
  out_file <- fs::path(out_dir
                       , basename(dirname(input_ras_path))
                       , basename(input_ras_path)
                       )
  
  if(any(!file.exists(out_file), force_new)) {
    
    if(!dir.exists(basename(out_file))) fs::dir_create(dirname(out_file))
    
    base <- terra::rast(base_grid_path)
    
    r <- terra::rast(input_ras_path) |>
      terra::project(terra::crs(base)
                     , method = proj_method
                     )
    
    ratio <- terra::res(r) / terra::res(base)
      
    rat_1 <- floor(sqrt(ratio))
    
    r <- r |>
      terra::disagg(rat_1
                    , method = "bilinear"
                    ) |>
      terra::project(base
                     , method = "bilinear"
                     , filename = out_file
                     , overwrite = TRUE
                     )
    
  }
    
  return(out_file)
  
}
  