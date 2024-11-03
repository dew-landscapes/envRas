  

  # settings------
  
  settings$name <- envFunc::vec_to_sentence(settings$level)
  
  settings$epsg_proj = 8059 # projected
  settings$epsg_latlong = 4283 # for decimal degrees
  
  ## satellite ------
  settings$sat_source <- "DEA"
  
  settings$sat_collection <- list(
    # landsat 9 and 8
    c("ga_ls9c_ard_3"
      , "ga_ls8c_ard_3"
      )
    # landsat 7 and 5
    # , c("ga_ls7e_ard_3"
    #     , "ga_ls5t_ard_3"
    #     )
    # sentinel 2a and 2b
    , c("ga_s2am_ard_3"
        , "ga_s2bm_ard_3"
        )
    )
  
  settings$sat_layers <- c("blue", "red", "green"
                           , "swir_1", "swir_2", "coastal_aerosol"
                           , "nir"
                           )
  
  settings$sat_indices <- list(gdvi = c("green", "nir")
                               , ndvi = c("nir", "red")
                               , nbr = c("nir", "swir_1")
                               , nbr2 = c("nir", "swir_2")
                               )
  
  
  ## climate -------
  settings$cli_source <- "NCI"
  settings$cli_collection <- "ANUClimate"
  settings$cli_res <- 1000 # nearly native resolution for ANUClimate 2.0
  
  ## statics --------
  settings$wofs_source <- "DEA"
  settings$wofs_collection <- "wofs_summary"
  settings$wofs_res <- settings$sat_res
  
  
  ## landcover-------
  settings$lc_source <- "DEA"
  settings$lc_collection <- "ga_ls_landcover_class_cyear_2"
  settings$lc_res <- 5000
  
  
  # max_cores-----
  
  settings$use_cores <- if(parallel::detectCores() < max_cores) parallel::detectCores() - 1 else max_cores


  # packages------
  
  settings$packages <- 
    sort(
      unique(
        c("base"
          
          # tidyverse
          , "dplyr"
          , "tidyr"
          , "purrr"
          , "ggplot2"
          , "tibble"
          , "readr"
          , "forcats"
          , "stringr"
          , "lubridate"
          
          # parallel
          , "furrr"
          
          # misc
          , "rio"
          , "fs"
          , "ggridges"
          , "xfun" # Yihui Xie functions. xfun::in_dir used in comm
          
          # gis
          , "rstac"
          , "gdalcubes"
          , "terra"
          , "sf"
          , "tmap"
          , "tidyterra"
          , "sfarrow"
          , "ncmeta"
          , "ncdf4"
          
          # env
          , "envRaster"
          , "envFunc"
          , "envReport"
          
          # report
          , "knitr"
          , "rmarkdown"
          , "bookdown"
          #, "kableExtra"
        )
      )
    )
  
  ## check for missing -------
  new_packages <- settings$packages[!(settings$packages %in% installed.packages()[,"Package"])]
  
  new_packages <- new_packages[!grepl("*env", new_packages)]
  
  if(length(new_packages)) install.packages(new_packages)
  
  ## load packages-------
  
  purrr::walk(settings$packages
              , library
              , character.only = TRUE
              )
  
  
  ## update 'env' packages -------
  env_packages <- settings$packages[grepl("*env", settings$packages)]
  
  purrr::walk(env_packages
              , \(x) remotes::install_github(paste0("acanthiza/", x)
                                             , dependencies = FALSE
                                             )
              )
  
  
  # functions------
  
  purrr::walk(fs::dir_ls("function")
              , source
              )
  
  
  # mapping options-------
  
  tmap::tmap_mode("view")
  
  tmap::tmap_options(basemaps = c("OpenStreetMap.Mapnik"
                                  , "Esri.WorldImagery"
                                  )
                     , limits = c(facets.plot = 100)
                     , max.raster = c(plot = 1e7, view = 1e6)
                     )
  
  # Sys.setenv(AWS_NO_SIGN_REQUEST = "YES")
  
  gdalcubes::gdalcubes_options(parallel = settings$use_cores)
  
  # gdalcubes--------
  # see https://gdalcubes.github.io/source/concepts/config.html
  gdalcubes::gdalcubes_set_gdal_config("VSI_CACHE", "TRUE")
  gdalcubes::gdalcubes_set_gdal_config("GDAL_CACHEMAX","30%")
  gdalcubes::gdalcubes_set_gdal_config("VSI_CACHE_SIZE","100000000")
  gdalcubes::gdalcubes_set_gdal_config("GDAL_HTTP_MULTIPLEX","YES")
  gdalcubes::gdalcubes_set_gdal_config("GDAL_INGESTED_BYTES_AT_OPEN","32000")
  gdalcubes::gdalcubes_set_gdal_config("GDAL_DISABLE_READDIR_ON_OPEN","EMPTY_DIR")
  gdalcubes::gdalcubes_set_gdal_config("GDAL_HTTP_VERSION","2")
  gdalcubes::gdalcubes_set_gdal_config("GDAL_HTTP_MERGE_CONSECUTIVE_RANGES","YES")
  gdalcubes::gdalcubes_set_gdal_config("GDAL_NUM_THREADS", as.character(settings$use_cores))
  
  # stop extra little files being written
    # see https://gis.stackexchange.com/questions/427923/preventing-terra-from-writing-auxiliary-files-when-writing-to-disc
  terra::setGDALconfig("GDAL_PAM_ENABLED", "FALSE")

  # cube ------
  ## epoch_period cube -------
  
  # add to this later when file paths are available?
  epoch_period_years <- lubridate::time_length(settings[["epoch_period", exact = TRUE]], unit = "years")
  
  settings$epochs <- envFunc::make_epochs(start_year = settings[["min_year", exact = TRUE]]
                                                    , end_year = settings[["max_year", exact = TRUE]]
                                                    , epoch_step = epoch_period_years
                                                    , epoch_overlap = FALSE
                                                    ) %>%
    dplyr::filter(purrr::map_lgl(years
                                 , \(x) length(x) == epoch_period_years)
                  )
  
  ## month cube-------
  
  settings$months <- settings$epochs %>%
    dplyr::mutate(seasons = purrr::map2(start_year
                                        , end_year
                                        , make_seasons
                                        )
                  ) %>%
    dplyr::select(epoch, seasons) %>%
    dplyr::mutate(seasons = purrr::map(seasons, "months")) %>%
    tidyr::unnest(cols = c("seasons")) %>%
    dplyr::filter(start_date < lubridate::floor_date(Sys.Date(), "month"))
  
  ## epoch cube dates -------
  
  # If need to match any observation date to a bin (season * epoch)
  settings$epoch_cube_dates <- settings$months %>%
    dplyr::group_by(epoch, year_use, season) %>%
    dplyr::summarise(min_date = min(start_date)
                     , max_date = max(end_date)
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dates = purrr::map2(min_date
                                      , max_date
                                      , \(x, y) as.Date(x:y)
                                      )
                  ) %>%
    tidyr::unnest(dates) %>%
    dplyr::select(epoch, season, date = dates)
  
  
  # directories------
  
  data_dir <- fs::path("H:"
                       , "data"
                       )
  
  
  ## sat cube ------
  settings$sat_month_cube <- purrr::map(settings$sat_collection
                                        , \(x) name_env_tif(c(settings[names(settings) != "sat_collection"]
                                                              , list(sat_collection = x)
                                                              )
                                                            , dir_only = TRUE
                                                            , prefixes = c("sat", "use")
                                                            , fill_null = TRUE
                                                            )$out_dir %>%
                                          fs::path("I:", .)
                                        )
  
  fs::dir_create(settings$sat_month_cube)
    
  
  ## cli cube ------
  settings$cli_month_cube <- name_env_tif(settings
                                          , dir_only = TRUE
                                          , prefixes = c("cli", "use")
                                          , fill_null = TRUE
                                          )$out_dir %>%
    fs::path("I:", .)
  
  fs::dir_create(settings$cli_month_cube)
  
  ## predict cube -------
  settings$predict_cube <- gsub("P1M"
                                , paste0(settings$epoch_period
                                         , "--"
                                         , settings$season_period
                                         )
                                , dirname(settings$sat_month_cube[[1]])
                                )
  
  
  ## out directories ------
  
  settings$out_dir <- here::here("out"
                                 , paste0(basename(dirname(dirname(settings$sat_month_cube[[1]])))
                                          , "__"
                                          , settings$sat_res
                                          )
                                 )
  
  settings$out_report <- fs::path(settings$out_dir, "report")
  fs::dir_create(settings$out_report)
  
  # maps-------
  
  # sa
  sa <- sfarrow::st_read_parquet(fs::path(data_dir
                                          , "vector"
                                          , "sa.parquet"
                                          )
                                 )
  
  # what aoi to use
  lay <- sfarrow::st_read_parquet(fs::path(data_dir
                                           , "vector"
                                           , paste0(settings[["polygons", exact = TRUE]]
                                                    , ".parquet"
                                                    )
                                           )
                                  )
  
  if(!is.null(settings[["use_clip", exact = TRUE]])) {
    
    clip <- sfarrow::st_read_parquet(fs::path(data_dir
                                              , "vector"
                                              , paste0(settings[["use_clip", exact = TRUE]]
                                                       , ".parquet"
                                                       )
                                              )
                                     )
    
  } else clip <- NULL
  
  # boundary ------
  
  out_file <- fs::path(dirname(settings$sat_month_cube[[1]]), "aoi.parquet")
  
  if(!file.exists(out_file)) {
    
    settings$boundary <- make_aoi(polygons = lay
                                  , filt_col = settings[["filt_col", exact = TRUE]]
                                  , filt_level = settings[["level", exact = TRUE]]
                                  , buffer = settings[["buffer", exact = TRUE]]
                                  , bbox = settings[["use_bbox", exact = TRUE]]
                                  , clip = clip
                                  , clip_buf = settings[["use_clip_buffer", exact = TRUE]]
                                  , out_crs = settings$epsg_proj
                                  )
  
    if(FALSE) tmap::tm_shape(settings[["boundary", exact = TRUE]]) + tmap::tm_polygons()
    
    sfarrow::st_write_parquet(settings$boundary
                              , out_file
                              )
    
  } else {
    
    settings$boundary <- sfarrow::st_read_parquet(out_file)
    
  }
  
  
  # base grid ---------
  
  out_file <- fs::path(dirname(settings$sat_month_cube[[1]]), "base.tif")
  
  if(!file.exists(out_file)) {
    
    settings$base <- make_base_grid(settings$boundary
                                    , out_res = settings$sat_res
                                    , out_epsg = settings$epsg_proj
                                    , use_mask = clip
                                    , out_file = out_file
                                    )
   
    
  }
  
  settings$base <- terra::rast(out_file)
  
  
  # bboxes-------
    
  # projected
  settings$bbox_use_epsg <- sf::st_bbox(settings$base)
  
  # geographic
  settings$bbox <- settings$bbox_use_epsg %>%
    sf::st_as_sfc() %>%
    sf::st_transform(crs = settings$epsg_latlong) %>%
    sf::st_bbox()
  
  # parallel -------
  # Cores to use for any parallel processing
  settings$use_cores <- if(parallel::detectCores() > max_cores) max_cores else parallel::detectCores() - 2
  
  # Plan for any furrr functions
  future::plan(sequential) # this line useful when resetting plan after crashing out of a furrr function
  
  future::plan(multisession
               , workers = settings$use_cores
               )
  
  # save --------
  
  rio::export(settings
              , fs::path(settings$out_dir
                         , "settings.rds"
                         )
              )
  