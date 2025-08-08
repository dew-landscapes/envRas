  
align_cli <- function(cli_ras_path
                      , settings
                      , base_grid_file
                      ) {
  
  out_file <- gsub("__1000", paste0("__", settings$grain$res), cli_ras_path)
  
  fs::dir_create(dirname(out_file))
  
  base <- terra::rast(base_grid_file)
  
  r <- terra::rast(cli_ras_path) |>
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
                   )
    
  return(out_file)
  
}
  