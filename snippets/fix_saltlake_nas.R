
  
  lc <- fs::dir_ls(aligned_dir) %>%
    grep("lc", ., value = TRUE) %>%
    terra::rast()
  
  saltlakes <- lc$salt_lakes > 0.5
  
  src <- fs::dir_ls(aligned_dir) %>%
    grep("src", ., value = TRUE) %>%
    terra::rast()
  
  temp <- terra::ifel(is.na(src$src11) & saltlakes == 1
                      , 1
                      , src$src11
                      )
  
  temp_other <- terra::ifel(is.na(src) & saltlakes == 1
                            , 0
                            , src
                            )
  
  src <- temp_other
  
  src$src11 <- temp
  
  
  
  

  purrr::walk(1:nlyr(src)
              , \(x) terra::writeRaster(src[[x]]
                                        , filename = paste0(aligned_dir, "__", names(src)[x], ".tif")
                                        , overwrite = TRUE
                                        )
              )
  