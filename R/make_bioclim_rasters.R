make_bioclim_rasters <- function(files_df
                                 , out_dir
                                 , start_date
                                 , force_new = TRUE
                                 ) {
  
  bc <- predicts::bcvars(prec = terra::rast(files_df$files[files_df$layer == "rain"][[1]]$path)
                         , tmin = terra::rast(files_df$files[files_df$layer == "tmin"][[1]]$path)
                         , tmax = terra::rast(files_df$files[files_df$layer == "tmax"][[1]]$path)
                         )
  
  purrr::map(names(bc)
             , \(x) {
               
               use_x <- paste0("bio", stringr::str_pad(readr::parse_number(x), width = 2, pad = 0))
               
               out_file <- fs::path(out_dir
                                    , paste0(use_x
                                             , "__"
                                             , use_x
                                             , "__"
                                             , start_date
                                             , ".tif"
                                             )
                                    )
               
               if(any(!file.exists(out_file), force_new)) {
               
                 terra::writeRaster(bc[x]
                                    , filename = out_file
                                    , overwrite = TRUE
                                    )
                 
               }
               
             }
             )
  
  fs::dir_ls(out_dir
             , regexp = "\\/bio.*tif$"
             )
  
}