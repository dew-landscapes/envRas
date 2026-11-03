
# wo

library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/get_items.R"
             , "R/make_download_df.R"
             , "R/save_dea_layer.R"
             , "R/make_cube_dir.R"
             , "R/fill_NA.R"
             )
           )

# tar options --------
envTargets::env_tar_option_set("wo"
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
  , tar_target(scales_file
               , "settings/scales.yaml"
               , format = "file"
               )
  ### wo ------
  , tar_file_read(settings_wo
                  , "settings/wo.yaml"
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
                               , set_source = settings_wo
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
                                                   , \(x, y) get_items(url = settings_wo$source_url
                                                                       , collection = settings_wo$collection
                                                                       , bbox = bbox
                                                                       , min_date = x
                                                                       , max_date = y
                                                                       )
                                                   )
                               )
               )
  ## wo --------
  ### df --------
  , tar_target(download_df
               , make_download_df(date_df = date_df
                                  , settings = settings_wo
                                  , base_dir = cube_directory
                                  , force_new = FALSE
                                  , filter_done = FALSE # need all the 'download's for later fill_NA
                                  )
               , format = "parquet"
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
  ## fill NA values ----------
  # 'steep' areas get NA values in water observation. who knows why, really.
  # this fills terrestrial NA values with 0
  , tar_target(settings_scale
               , envFunc::extract_scale(scales = scales_file)
               )
  , tar_target(env_df
               , envRaster::prepare_env(set_list = settings_scale
                                        , base_dir = settings$cube_dir
                                        )
               , format = "parquet"
               )
  , tar_target(wo
               , fill_NA(r = download
                         , mask = env_df$path[grepl("bio01", env_df$name)]
                         , fill_val = 0
                         , out_file = gsub("frequency__", "wo__", download)
                         , force_new = FALSE
                         # dots to writeRaster
                         , overwrite = TRUE
                         , datatype = "INT1U"
                         , scale = download_df$scale
                         , offset = download_df$offset
                         )
               , pattern = map(download, download_df)
               , format = "file"
               )
)
