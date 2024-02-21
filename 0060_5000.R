
  
  
  # directories -----
  
  silo_dir <- fs::path(data_dir, "raster", "silo")
  
  geoss_dir <- fs::path(data_dir, "raster", "GEOSS")
  
  lc_dir <- fs::path(data_dir, "raster", "DLCD")

  aligned_dir <- fs::path("H:"
                          , "data"
                          , "raster"
                          , "aligned"
                          , "sa_ibrasub_xn____0__5000"
                          )
  
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
    sf::st_make_valid() %>%
    dplyr::mutate(sa = 1)

  # base  -------
  
  base <- terra::rast(crs = terra::crs(settings$base)
                      , xmin = terra::xmin(settings$base)
                      , xmax = terra::xmax(settings$base)
                      , ymin = terra::ymin(settings$base)
                      , ymax = terra::ymax(settings$base)
                      , res = 5000
                      , vals = 1
                      )
  
  base <- sa_proj %>%
    terra::rasterize(base
                     , touches = TRUE
                     )
  
  
  # Silo-------
  
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
                                     #, aoi =  sa
                                     , base = base
                                     )
                  )
  
  bio <- dismo::biovars(prec = raster::stack(silo$silo[[4]])
                        , tmin = raster::stack(silo$silo[[3]])
                        , tmax = raster::stack(silo$silo[[2]])
                        )
  
  bio_t <- terra::rast(bio)
  
  terra::crs(bio_t) <- paste0("epsg:", settings$epsg_proj)
  
  source <- "silo"
  collection <- "climate"
  res <- 5000
  epoch <- "74-22"
  season <- "all"

  name_start <- paste(source, collection, res, epoch, season, sep = "__")
  
  purrr::map(names(bio_t)
             , \(x) writeRaster(bio_t[[x]]
                                , filename = fs::path(aligned_dir
                                                      , paste0(name_start, "__", x, ".tif")
                                                      )
                                , overwrite = TRUE
                                )
             )
  
  
  # geoss ---------
  
  geoss_layers <- c("weathering", "moisture")
  
  geoss <- fs::dir_info(geoss_dir
                        , recurse = TRUE
                        , regexp = "tif$"
                        ) %>%
    dplyr::mutate(att = basename(path)) %>%
    tidyr::separate(att, into = c("source", "collection", "band"), sep = "__") %>%
    dplyr::select(path, source, collection, band) %>%
    dplyr::filter(grepl(paste0(geoss_layers, collapse = "|"), tolower(band))) %>%
    dplyr::mutate(out_path = fs::path(aligned_dir
                                      , paste(source, collection, 5000, "all", "all", sep = "__")
                                      )
                  )
  
  purrr::pwalk(list(path = geoss$path
                    , out_path = geoss$out_path
                    )
               , mung_geoss
               , aoi = sa
               , base = base
               )
    
  
  # landcover ---------
  
  # manual download from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/83868
  # put in data/raster/DLCD
  
  lc_source <- "DEA"
  lc_collection <- "ga_ter_m_dlcd_ann"
  
  lc <- fs::dir_info(lc_dir
                        , recurse = TRUE
                        , regexp = "tif$"
                        ) %>%
    dplyr::mutate(att = basename(path)
                  , year = lubridate::year(lubridate::ymd(stringr::str_extract(att, "-\\d{8}\\.")))
                  , source = lc_source
                  , collection = lc_collection
                  , min_year = min(year)
                  , max_year = max(year)
                  , epoch = paste0(substr(min_year, 3, 4), "-", substr(max_year, 3, 4))
                  ) %>%
    dplyr::select(path, source, collection, epoch) %>%
    dplyr::mutate(out_path = fs::path(aligned_dir
                                      , paste(source, collection, 5000, epoch, "all", sep = "__")
                                      )
                  )
  
  mung_lc(lc$path
          , unique(lc$out_path)
          , aoi = sa_proj
          , base = base
          )
  