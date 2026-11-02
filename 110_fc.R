
# Tasseled cap

library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_cube_dir.R"
             , "R/make_date_df.R"
             , "R/get_items.R"
             , "R/make_download_df.R"
             , "R/save_dea_layer.R"
             )
           )

# tar options --------
envTargets::env_tar_option_set("fc"
                               , controller_local_args = list(workers = yaml::read_yaml("settings/cores.yaml")$download_cores
                                                              , name = "main_controller"
                                                              )
                               )

targets::tar_option_set(trust_timestamps = TRUE)

targets <- list(
  # targets --------
  ## settings -------
  ### setup -------
  tar_file_read(settings
                , fs::path(tars$setup$store, "objects", "settings")
                , readRDS(!!.x)
                )
  ### fc ------
  , tar_file_read(settings_fc
                  , "settings/fc.yaml"
                  , yaml::read_yaml(!!.x)
                  )
  ## external objects ------
  , tar_file_read(extent_sf
                  , fs::path(tars$setup$store, "objects", "extent_sf")
                  , readRDS(!!.x)
                  )
  ## cube directory ------
  , tar_target(cube_directory
               , make_cube_dir(set_scale = settings
                               , set_source = settings_fc
                               , cube_dir = settings$cube_dir
                               )
               )
  ### base grid -------
  , tar_target(base_grid_path
               , fs::path(dirname(cube_directory), "base.tif") |>
                 as.character()
               )
  ## prep -------
  ### dates -------
  , tar_file_read(date_df
                  , fs::path(tars$setup$store, "objects", "date_df")
                  , arrow::read_parquet(!!.x)
                  )
  ### bbox -------
  , tar_target(bbox
               , sf::st_bbox(terra::rast(base_grid_path)) |>
                 sf::st_as_sfc() |>
                 sf::st_transform(crs = settings$crs$decdeg) |> # need decimal lat/long for rstac
                 sf::st_bbox()
               )
  ### items ------
  , tar_target(items
               , date_df |>
                 dplyr::mutate(items = purrr::map2(start_date
                                                   , end_date
                                                   , \(x, y) get_items(url = settings_fc$source_url
                                                                       , collection = settings_fc$collection
                                                                       , bbox = bbox
                                                                       , min_date = x
                                                                       , max_date = y
                                                                       )
                                                   )
                               )
               )
  ## fc --------
  ### df --------
  , tar_target(download_df
               , make_download_df(date_df = date_df
                                  , settings = settings_fc
                                  )
               )
  ### download --------
  , tar_target(download
               , save_dea_layer(items = download_df |>
                                  dplyr::left_join(items) |>
                                  dplyr::pull(items)
                                , base_grid = terra::rast(base_grid_path)
                                , layer = download_df$layer
                                , agg_func = download_df$func
                                , start_date = download_df$start_date
                                , end_date = download_df$end_date
                                , cloud_mask = NULL
                                , base_dir = cube_directory
                                , period = settings$grain$grain_time
                                , force_new = FALSE
                                , cores = envFunc::use_cores(absolute_max = yaml::read_yaml("settings/cores.yaml")$process_cores)
                                # gdalcubes::write_tif args
                                , pack = list(type = "int16"
                                              , scale = download_df$scale
                                              , offset = download_df$offset
                                              , nodata = -32768
                                              )
                                )
               , format = "file"
               , pattern = map(download_df)
               )
  )
