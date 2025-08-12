  
align_ras <- function(input_ras_path
                      , base_grid_path
                      , in_res
                      , out_res
                      , force_new = TRUE
                      ) {
  
  out_file <- gsub(paste0("__", in_res)
                   , paste0("__", out_res)
                   , input_ras_path
                   )
  
  if(any(!file.exists(out_file), force_new)) {
    
    fs::dir_create(dirname(out_file))
    
    base <- terra::rast(base_grid_path)
    
    r <- terra::rast(input_ras_path) |>
      terra::project(terra::crs(base)
                     , method = "near"
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
  