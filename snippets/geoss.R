
  geo <- tibble::enframe(fs::dir_ls(fs::path("H:", "data", "raster", "aligned", "sa_ibrasub_xn____0__5000")
                                    , regexp = "tif$"
                                    )
                         , name = NULL
                         , value = "path"
                         ) |>
    dplyr::filter(grepl("geoss", tolower(path))) |>
    dplyr::pull(path) |>
    terra::rast()
  
  terra::set.names(geo
                   , gsub("\\s", "_", names(geo))
                   )
  
  file_names <- gsub("\\s", "_", terra::sources(geo)) |>
    gsub("_old", "", x = _)
  
  purrr::walk2(names(geo)
               , file_names
              , \(x, y) terra::writeRaster(geo[[x]]
                                           , filename = y
                                           )
              )


  