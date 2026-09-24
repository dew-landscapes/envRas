fill_NA <- function(r
                    , mask # another raster that defines the extent to fill
                    , fill_val = 0
                    , out_file
                    , force_new = TRUE
                    , ...
                    ) {
  
  if(any(!file.exists(out_file), force_new)) {
    
    if(is.character(r)) r <- terra::rast(r)
    if(is.character(mask)) mask <- terra::rast(mask)
    
    terra::ifel(is.na(r) & !is.na(mask), fill_val, r) |>
      terra::writeRaster(filename = out_file
                         , ...
                         )
    
    used_scoff <- terra::scoff(terra::rast(out_file))
    
    create_esri_xml(tif_path = out_file
                    , scale = used_scoff[[1]]
                    , offset = used_scoff[[2]]
                    )
    
  }
  
  return(out_file)
  
}