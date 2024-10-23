  
#accidentially overwrote base.tif 2024-10-18. Remade with this script

  tif <- "H:/data/raster/sa_ibrasub_xn______0/P10Y__90/DEA__ga_ls8c_ard_3--ga_ls9c_ard_3/blue__autumn__2013-12-01.tif"
  
  r <- terra::rast(tif)
  
  terra::plot(r)
  
  new_base <- terra::rast(crs = terra::crs(r)
                          , extent = terra::ext(r)
                          , resolution = terra::res(r)
                          , vals = 1
                          , names = "base"
                          ) %>%
    terra::mask(terra::vect(aus_500m_buf %>%
                              sf::st_transform(crs = sf::st_crs(new_base)) %>%
                              sf::st_make_valid()
                            )
                )
  
  tm_shape(new_base) + tm_raster()
  
  terra::writeRaster(new_base
                     , "H:/data/raster/sa_ibrasub_xn______0/P10Y__90/base.tif"
                     , datatype = "INT1U"
                     )
  