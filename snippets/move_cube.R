

  existing_cube <- fs::path("H:"
                            , "data"
                            , "raster"
                            , "aligned"
                            , "sa_ibrasub_xn____0__90"
                            )
  
  new_cube <- fs::path("H:"
                       , "data"
                       , "raster"
                       , "cube__P10Y"
                       )
  
  safely_rast <- purrr::safely(terra::rast)
  
  cubes <- fs::dir_ls(existing_cube, regexp = "tif$") %>%
    tibble::enframe(name = NULL, value = "old_path") %>%
    dplyr::filter(!grepl("base", old_path)) %>%
    dplyr::mutate(context = gsub("____", "______", gsub("__90", "", basename(dirname(old_path))))
                  , sources = gsub("__0.*", "", basename(old_path))
                  , file = basename(old_path)
                  ) %>%
    tidyr::separate(file, into = c("source", "collection", "buffer", "epoch", "season", "layer"), sep = "__") %>%
    dplyr::mutate(file = paste0(gsub("\\.tif", "", layer), "__", season, "__", "2013-12-01.tif")
                  , new_path = fs::path("H:", "data", "raster", context, "P10Y__90", sources, file)
                  , done = file.exists(new_path)
                  )
  
  dirs <- cubes %>%
    dplyr::select(new_path) %>%
    dplyr::mutate(dir = dirname(new_path)) %>%
    dplyr::pull(dir) %>%
    unique
  
  fs::dir_create(dirs)
  
  fs::file_copy(cubes$old_path[!cubes$done]
                , cubes$new_path[!cubes$done]
                , overwrite = TRUE
                )
  
  
  