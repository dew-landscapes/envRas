combine_tiles <- function(tiles
                          , out_file
                          , sf_mask = NULL
                          , ...
                          ) {
  
  temp <- terra::vrt(tiles) |>
    terra::sds() |>
    terra::rast()
  
  if(!is.null(sf_mask)) {
    
    use_mask <- if(terra::crs(temp, describe = TRUE)$code != terra::crs(sf_mask,describe = TRUE)$code) {
      
      sf_mask |>
        terra::vect() |>
        terra::densify(50000) |>
        sf::st_as_sf() |>
        sf::st_transform(crs = sf::st_crs(temp)) |>
        sf::st_make_valid() |>
        terra::vect()
      
    } else {
      
      sf_mask |>
        terra::vect() |>
        terra::densify(50000)
      
    }
    
    temp <- temp |>
      terra::mask(mask = use_mask)
    
  }
  
  terra::writeRaster(temp
                     , filename = as.character(out_file)
                     , overwrite = TRUE
                     , ...
                     )
  
  return(out_file)
  
}
