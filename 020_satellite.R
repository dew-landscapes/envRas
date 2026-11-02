
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
envTargets::env_tar_option_set("satellite"
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
               , pattern = map(date_df)
               )
  ## satellite --------
  , tar_target(name = temporal_run
               , envFunc::find_name(settings, "run_time")
               )
  ### download df --------
  , tar_target(download_df
               , make_download_df(date_df = date_df
                                  , settings = settings_satellite
                                  ) |>
                 dplyr::mutate(layer = paste0("nbart_", layer))
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
  ## indices------
  , tar_target(index_prep
               , envRaster::name_env_tif(cube_directory, parse = TRUE) |>
                 dplyr::inner_join(tibble::enframe(download, name = "branch", value = "path")) |>
                 dplyr::select(layer, start_date)
               )
  ### indice_df --------
  , tar_target(indice_df
               , settings_satellite$indices |>
                 tibble::as_tibble() |>
                 dplyr::mutate(layer_index = paste0("layer_", dplyr::row_number())) |>
                 tidyr::pivot_longer(tidyselect::any_of(names(settings_satellite$indices))
                                     , names_to = "index"
                                     , values_to = "layer"
                                     ) |>
                 dplyr::inner_join(index_prep
                                   , relationship = "many-to-many"
                                   ) |>
                 dplyr::mutate(layer = fs::path(cube_directory
                                               , paste0(layer
                                                        , "__median__"
                                                        , start_date
                                                        , ".tif"
                                                        )
                                               )
                               ) |>
                 tidyr::pivot_wider(names_from = layer_index
                                    , values_from = layer
                                    ) |>
                 dplyr::rename(layer = index) |>
                 dplyr::left_join(envRaster::ras_layers |>
                                    dplyr::select(layer, scale, offset)
                                  )
               )
  ### mung -------
  , tar_target(name = indice
               , command = make_indice(index_name = indice_df$layer
                                       , path_1 = indice_df$layer_1
                                       , path_2 = indice_df$layer_2
                                       , scale = indice_df$scale
                                       , offset = indice_df$offset
                                       , terra_options = list(memfrac = 0.1)
                                       , force_new = FALSE
                                       )
               , pattern = map(indice_df)
               , format = "file"
               )
  )
