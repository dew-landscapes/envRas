make_bioclim_rasters <- function(files_df
                                 , out_dir
                                 , start_date
                                 , force_new = TRUE
                                 ) {
  
  bc <- bioclima::clima(bios = c(1:19)
                        , tmin = terra::rast(files_df$files[files_df$layer == "tmin"][[1]]$path)
                        , tmax = terra::rast(files_df$files[files_df$layer == "tmax"][[1]]$path)
                        , tavg = terra::rast(files_df$files[files_df$layer == "tavg"][[1]]$path)
                        , prcp = terra::rast(files_df$files[files_df$layer == "rain"][[1]]$path)
                        )
  
  purrr::map(names(bc)
             , \(x) {
               
               out_file <- fs::path(out_dir
                                    , paste0(x
                                             , "__"
                                             , x
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