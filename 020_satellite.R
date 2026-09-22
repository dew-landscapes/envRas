
library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_cube_dir.R"
             , "R/make_date_df.R"
             , "R/get_items.R"
             , "R/save_satellite_layer.R"
             , "R/make_indice.R"
             )
           )

# tar options --------
envTargets::env_tar_option_set("satellite")

targets <- list(
  # targets --------
  ## settings -------
  ### setup -------
  tar_file_read(settings
                , fs::path(tars$setup$store, "objects", "settings")
                , readRDS(!!.x)
                )
  ### satellite ------
  , tar_file_read(settings_satellite
                  , "settings/satellite.yaml"
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
                               , set_source = settings_satellite
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
                                                   , \(x, y) get_items(url = settings_satellite$source_url
                                                                       , collection = settings_satellite$collection
                                                                       , bbox = bbox
                                                                       , min_date = x
                                                                       , max_date = y
                                                                       )
                                                   )
                               )
               )
  ### layers --------
  , tar_target(name = temporal_run
               , envFunc::find_name(settings, "run_time")
               )
  ### layer df --------
  , tar_target(layer_df
               , tibble::tibble(layer = settings_satellite$layers) |>
                 dplyr::cross_join(items)
               )
  ### download --------
  , tar_target(name = layer
               , command = save_satellite_layer(items = layer_df$items[[1]]
                                                , base_grid = terra::rast(base_grid_path)
                                                , layer = layer_df$layer
                                                , start_date = layer_df$start_date
                                                , end_date = layer_df$end_date
                                                , cloud_mask = NULL
                                                , base_dir = cube_directory
                                                , period = temporal_run
                                                , force_new = FALSE
                                                , cores = envFunc::use_cores(absolute_max = yaml::read_yaml("settings/cores.yaml")$process_cores)
                                                # gdalcubes::write_tif args
                                                , pack = list(type = "int16"
                                                              , scale = 1
                                                              , offset = 0
                                                              , nodata = -999
                                                              )
                                                )
               , pattern = map(layer_df)
               , format = "file"
               )
  ## variability -----
  ### variability_df------
  , tar_target(name = variability_df
               , tibble::tibble(layer = settings_satellite$variability) |>
                 dplyr::cross_join(items)
               )
  ### mean --------
  , tar_target(name = variability
               , command = save_satellite_layer(items = variability_df$items[[1]]
                                                , base_grid = terra::rast(base_grid_path)
                                                , layer = variability_df$layer
                                                , agg_func = "mean"
                                                , start_date = variability_df$start_date
                                                , end_date = variability_df$end_date
                                                , cloud_mask = NULL
                                                , base_dir = cube_directory
                                                , period = temporal_run
                                                , force_new = FALSE
                                                , cores = envFunc::use_cores(absolute_max = yaml::read_yaml("settings/cores.yaml")$process_cores)
                                                # no pack
                                                )
               , pattern = map(variability_df)
               , format = "file"
               )
  ### max --------
  , tar_target(name = max
               , command = save_satellite_layer(items = variability_df$items[[1]]
                                                , base_grid = terra::rast(base_grid_path)
                                                , layer = variability_df$layer
                                                , agg_func = "max"
                                                , start_date = variability_df$start_date
                                                , end_date = variability_df$end_date
                                                , cloud_mask = NULL
                                                , base_dir = cube_directory
                                                , period = temporal_run
                                                , force_new = FALSE
                                                , cores = envFunc::use_cores(absolute_max = yaml::read_yaml("settings/cores.yaml")$process_cores)
                                                # no pack
                                                )
               , pattern = map(variability_df)
               , format = "file"
               )
  ## indices------
  ### indices list --------
  , tar_target(indice_df
               , settings_satellite$indices |>
                 tibble::as_tibble() |>
                 dplyr::mutate(layer_index = paste0("layer_", dplyr::row_number())) |>
                 tidyr::pivot_longer(tidyselect::any_of(names(settings_satellite$indices))
                                     , names_to = "indice"
                                     , values_to = "layer"
                                     )
               )
  ### layers --------
  , tar_target(combined_layers
               , date_df |>
                 dplyr::cross_join(indice_df) |>
                 dplyr::mutate(layer = fs::path(cube_directory
                                                , paste0(layer
                                                         , "__median__"
                                                         , start_date
                                                         , ".tif"
                                                         )
                                                )
                               ) |>
                 dplyr::filter(file.exists(layer)) |>
                 tidyr::pivot_wider(names_from = layer_index
                                    , values_from = layer
                                    )
               )
  ### mung -------
  , tar_target(name = indice
               , command = make_indice(index_name = combined_layers$indice
                                       , path_1 = combined_layers$layer_1
                                       , path_2 = combined_layers$layer_2
                                       , terra_options = list(memfrac = 0.1)
                                       , force_new = FALSE
                                       )
               , pattern = map(combined_layers)
               , format = "file"
               )
  )
