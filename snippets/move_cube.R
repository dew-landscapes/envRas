

  existing_cube <- fs::path("D:"
                            , "env"
                            , "data"
                            , "raster"
                            , "cube__P3M"
                            , "DEA__ga_ls8c_ard_3--ga_ls9c_ard_3__sa_ibrasub_xn__0__30"
                            )
  
  new_cube <- fs::path("G:"
                       , "cube__P3M"
                       )
  
  safely_rast <- purrr::safely(terra::rast)
  
  cubes <- fs::dir_ls(existing_cube, regexp = "tif$") %>%
    tibble::enframe(name = NULL, value = "old_path") %>%
    dplyr::mutate(new_path = fs::path(new_cube
                                      , basename(dirname(old_path))
                                      , basename(old_path)
                                      )
                  , done = purrr::map(new_path, safely_rast)
                  , done = purrr::map_lgl(done, ~ !is.null(.$result))
                  ) 
  
  fs::file_copy(cubes$old_path[!cubes$done]
                , cubes$new_path[!cubes$done]
                , overwrite = TRUE
                )
  
  
  