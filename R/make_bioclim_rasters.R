make_bioclim_rasters <- function(files_df
                                 , out_dir
                                 , force_new = TRUE
                                 ) {
  
  start_date <- min(files_df$start_date)
  
  bc <- predicts::bcvars(prec = terra::rast(files_df$path[files_df$layer == "rain"])
                         , tmin = terra::rast(files_df$path[files_df$layer == "tmin"])
                         , tmax = terra::rast(files_df$path[files_df$layer == "tmax"])
                         )
  
  result <- purrr::map(names(bc)
                       , \(x) {
                         
                         use_x <- paste0("bio", stringr::str_pad(readr::parse_number(x), width = 2, pad = 0))
                         
                         out_file <- fs::path(out_dir
                                              , paste0(use_x
                                                       , "__bioclim__"
                                                       , start_date
                                                       , ".tif"
                                                       )
                                              )
               
                         if(any(!file.exists(out_file), force_new)) {
                           
                           terra::writeRaster(bc[[x]]
                                              , filename = out_file
                                              , overwrite = TRUE
                                              , names = use_x
                                              )
                           
                           }
               
                         return(out_file)
                         
                       }
                       
                       )
  
  return(unlist(result))
  
}