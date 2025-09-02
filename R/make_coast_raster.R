make_coast_raster <- function(tiles
                              , base_grid_path
                              , coast_sf
                              , out_file
                              , force_new = TRUE
                              , dist_limit = 10000
                              , ...
                              ) {
  
  if(any(!file.exists(out_file), force_new)) {
    
    combined <- terra::vrt(purrr::map_chr(tiles, terra::sources))
    base <- terra::rast(base_grid_path)
   
    # terrestrial
    terr <- terra::mask(base
                        , mask = terra::vect(coast_sf)
                        , updatevalue = -1
                        )
    
    r <- terra::lapp(x = terra::sds(combined, terr)
                     , fun = \(x, y, limit = dist_limit) {
                       
                       pmin(x, limit) * y
                       
                     }
                     , filename = out_file
                     , ...
                     )
    
  }
  
  return(out_file)
  
}
