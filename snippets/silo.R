
  
  
  # directories -----
  
  silo_dir <- fs::path(data_dir, "raster", "silo")

  
  # aoi --------
  
  aus_500m_buf <- sfarrow::st_read_parquet(fs::path(data_dir
                                                    , "vector"
                                                    , "aus_500m_buf.parquet"
                                                    )
                                           ) %>%
    sf::st_transform(crs = settings$epsg_proj) %>%
    sf::st_make_valid()
  
  sa <- envFunc::make_aoi(aus_500m_buf
                          , clip = sfarrow::st_read_parquet(fs::path(data_dir
                                                                     , "vector"
                                                                     , "sa_ibrasub_xn.parquet"
                                                                     )
                                                            )
                          )
  
  sa_proj <- sa %>%
    sf::st_transform(settings$epsg_proj) %>%
    sf::st_make_valid()

  # base  -------
  
  silo_base <- terra::rast(crs = terra::crs(settings$base)
                           , xmin = terra::xmin(settings$base)
                           , xmax = terra::xmax(settings$base)
                           , ymin = terra::ymin(settings$base)
                           , ymax = terra::ymax(settings$base)
                           , res = 5000
                           )
  
  # Silo
  
  func_df <- tibble::tibble(att = c("monthly_rain"
                                    , "et_morton_actual"
                                    , "max_temp"
                                    , "min_temp"
                                    )
                            , func = c(mean, mean, max, min)
                            )
  
  silo <- fs::dir_info(silo_dir) %>%
    dplyr::filter(type == "directory"
                  , grepl(paste0(func_df$att, collapse = "|"), path)
                  ) %>%
    dplyr::mutate(att = basename(path)
                  , files = purrr::map(path, fs::dir_ls)
                  ) %>%
    dplyr::select(path, att, files) %>%
    dplyr::left_join(func_df) %>%
    dplyr::mutate(silo = purrr::map2(files
                                     , func
                                     , mung_silo
                                     , aoi =  sa
                                     , base = silo_base
                                     )
                  )
  
  bio <- dismo::biovars(prec = raster::stack(silo$silo[[4]])
                        , tmin = raster::stack(silo$silo[[3]])
                        , tmax = raster::stack(silo$silo[[2]])
                        )
  
  bio_t <- terra::rast(bio)
  
  source <- "silo"
  collection <- "climate"
  res <- 5000
  epoch <- "74-22"
  season <- "all"

  name_start <- paste(source, collection, res, epoch, season, sep = "__")
  
  purrr::map(names(bio_t)
             , \(x) writeRaster(bio_t[[x]]
                                , filename = fs::path("H:"
                                                      , "data"
                                                      , "raster"
                                                      , "aligned"
                                                      , "sa_ibrasub_xn____0__5000"
                                                      , paste0(name_start, "__", x, ".tif")
                                                      )
                                )
             )
  
  