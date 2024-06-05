  

  # settings------
  
  settings$name <- envFunc::vec_to_sentence(settings$level)
  
  settings$epsg_proj = 8059 # projected
  settings$epsg_latlong = 4283 # for decimal degrees
  
  settings$start_year <- 2014
  settings$end_year <- 2023
  settings$epoch_period <- 10
  
  ## satellite ------
  settings$sat_source <- "DEA"
  settings$sat_collection <- c("ga_ls8c_ard_3", "ga_ls9c_ard_3") # don't include sentinel
  settings$sat_res <- 90
  settings$sat_layers <- c("blue", "red", "green"
                           , "swir_1", "swir_2", "coastal_aerosol"
                           , "nir"
                           )
  
  settings$sat_indices <- list(gdvi = c("green", "nir")
                               , ndvi = c("nir", "red")
                               , nbr = c("nir", "swir_1")
                               , nbr2 = c("nir", "swir_2")
                               )
  
  settings$chunks <- c(2, 2) # should lead to chunks[1] * chunks[2] tiles
  
  ## climate -------
  settings$cli_source <- "NCI"
  settings$cli_collection <- "ANUClimate"
  settings$cli_res <- 1000
  
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
  
  packages <- 
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
  
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(new_packages)) install.packages(new_packages)
  
  purrr::walk(packages
              , library
              , character.only = TRUE
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

  
    # epochs -------
  
  settings$epochs <- envFunc::make_epochs(start_year = settings[["start_year", exact = TRUE]]
                                        , end_year = settings[["end_year", exact = TRUE]]
                                        , epoch_step = settings[["epoch_period", exact = TRUE]]
                                        , epoch_overlap = FALSE
                                        )
  
  # seasons-------
  
  settings$seasons <- settings$epochs %>%
    dplyr::mutate(seasons = purrr::map2(start_year
                                        , end_year
                                        , make_seasons
                                        )
                  ) %>%
    dplyr::pull(seasons) %>%
    `[[`(1)
  
  
  # directories------
  
  data_dir <- fs::path("H:"
                       , "data"
                       )
  
  
  ## sat cube ------
  settings$sat_seas_cube_dir <- name_env_tif(settings
                                        , dir_only = TRUE
                                        , prefixes = c("sat", "use")
                                        , fill_null = TRUE
                                        )$out_dir %>%
    fs::path("I:", .)
  
  fs::dir_create(settings$sat_seas_cube)
    
  ## cli cube ------
  settings$cli_seas_cube_dir <- name_env_tif(settings
                                        , dir_only = TRUE
                                        , prefixes = c("cli", "use")
                                        , fill_null = TRUE
                                        )$out_dir %>%
      fs::path("I:", .)
  
  fs::dir_create(settings$cli_cube_dir)
  
  
  ## wofs rasters -------
  settings$wofs_dir <- name_env_tif(settings
                                    , dir_only = TRUE
                                    , prefixes = c("wofs", "use")
                                    , fill_null = TRUE
                                    )$out_dir %>%
    fs::path("I:", .) %>%
    gsub("P3M", "static", .)
  
  fs::dir_create(settings$static_dir)
  
  
  ## out directories ------
  
  settings$out_dir <- here::here("out"
                                 , paste0(basename(dirname(dirname(settings$sat_seas_cube)))
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
                                           , paste0(settings[["vector", exact = TRUE]]
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
  
  out_file <- fs::path(dirname(settings$sat_seas_cube), "aoi.parquet")
  
  if(!file.exists(out_file)) {
    
    settings$boundary <- make_aoi(layer = lay
                                  , filt_col = settings[["filt_col", exact = TRUE]]
                                  , level = settings[["level", exact = TRUE]]
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
  
  out_file <- fs::path(dirname(settings$sat_seas_cube), "base.tif")
  
  if(!file.exists(out_file)) {
    
    settings$base <- make_base_grid(settings$boundary
                                    , base_res = settings$sat_res
                                    , base_epsg = settings$epsg_proj
                                    , use_mask = clip
                                    , out_file = out_file
                                    )
   
    
  } else settings$base <- terra::rast(out_file)
  
  
  # bboxes-------
    
  # projected
  settings$bbox_use_epsg <- sf::st_bbox(settings$base)
  
  # geographic
  settings$bbox <- settings$bbox_use_epsg %>%
    sf::st_as_sfc() %>%
    sf::st_transform(crs = settings$epsg_latlong) %>%
    sf::st_bbox()
  
                           
  # seasons-------
  
  stacks <- settings[["seasons", exact = TRUE]]$months %>%
    dplyr::group_by(year_use, season) %>%
    dplyr::summarise(start_date = min(start_date)
                     , end_date = max(end_date)
                     ) %>%
    dplyr::ungroup() 
  
  # save --------
  
  rio::export(settings
              , fs::path(settings$out_dir
                         , "settings.rds"
                         )
              )
  