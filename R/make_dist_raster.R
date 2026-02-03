make_dist_raster <- function(tiles
                             , out_file
                             , ...
                             ) {
  
  terra::vrt(tiles) |>
    terra::sds() |>
    terra::rast() |>
    terra::writeRaster(filename = as.character(out_file)
                       , overwrite = TRUE
                       , ...
                       )
  
  return(out_file)
  
}
