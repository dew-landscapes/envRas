make_bioclim_rasters <- function(files_df
                                 , scale = 1
                                 , offset = 0
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
                           
                           use_scale <- envRaster::ras_layers$scale[envRaster::ras_layers$layer == use_x]
                           use_offset <- envRaster::ras_layers$offset[envRaster::ras_layers$layer == use_x]
                           
                           terra::writeRaster(bc[[x]]
                                              , filename = out_file
                                              , overwrite = TRUE
                                              , names = use_x
                                              , datatype = "INT2S"
                                              , scale = use_scale
                                              , offset = use_offset
                                              )
                           
                           create_esri_xml(tif_path = out_file
                                           , scale = use_scale
                                           , offset = use_offset
                                           )
                           
                         }
               
                         return(out_file)
                         
                       }
                       
                       )
  
  return(unlist(result))
  
}