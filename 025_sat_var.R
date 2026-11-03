
library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_cube_dir.R"
             , "R/make_date_df.R"
             , "R/make_download_df.R"
             , "R/save_dea_layer.R"
             )
           )


# tar options --------
envTargets::env_tar_option_set("sat_var"
                               , controller_local_args = list(workers = yaml::read_yaml("settings/cores.yaml")$download_cores
                                                              , name = "main_controller"
                                                              )
                               )

targets::tar_option_set(trust_timestamps = TRUE)

# targets --------
targets <- list(
  ## settings -------
  ### setup -------
  tar_file_read(settings
                , fs::path(tars$setup$store, "objects", "settings")
                , readRDS(!!.x)
                )
  ### sat_var ------
  , tar_file_read(settings_sat_var
                  , "settings/sat_var.yaml"
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
                               , set_source = settings_sat_var
                               , cube_dir = settings$cube_dir
                               )
               )
  ### base grid path-------
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
  ### items ------
  , tar_change(items_from_sat
               , targets::tar_read(items, store = tars$satellite$store)
               , change = targets::tar_meta(items
                                            , fields = data
                                            , store = tars$satellite$store
                                            )$data
               )
  ## sat_var --------
  ### download df --------
  , tar_target(download_df
               , make_download_df(date_df = date_df
                                  , settings = settings_sat_var
                                  , base_dir = cube_directory
                                  , force_new = FALSE
                                  )
               , format = "parquet"
               )
  ### download --------
  , tar_target(download
               , save_dea_layer(items = download_df |>
                                  dplyr::left_join(items_from_sat) |>
                                  dplyr::pull(items)
                                , base_grid = terra::rast(base_grid_path)
                                , layer = download_df$layer
                                , agg_func = download_df$func
                                , start_date = download_df$start_date
                                , end_date = download_df$end_date
                                , cloud_mask = NULL
                                , base_dir = cube_directory
                                , period = settings$grain$run_time
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
