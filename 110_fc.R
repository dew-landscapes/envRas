
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
             , "R/save_satellite_layer.R"
             , "R/make_indice.R"
             , "R/create_esri_xml.R"
             )
           )

# tar options ------
envTargets::env_tar_option_set("fc")

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
               , format = "file"
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
  ## layers --------
  ### temporal run ------
  , tar_target(name = temporal_run
               , envFunc::find_name(settings, "run_time")
               )
  ### layer df --------
  , tar_target(fc_df
               , tibble::tibble(layer = settings_fc$layers) |>
                 dplyr::cross_join(items) |>
                 dplyr::left_join(envRaster::ras_layers |>
                                    dplyr::select(layer, scale, offset)
                                  )
               )
  ### download --------
  , tar_target(name = fc
               , command = save_satellite_layer(items = fc_df$items[[1]]
                                                , base_grid = terra::rast(base_grid_path)
                                                , layer = fc_df$layer
                                                , start_date = fc_df$start_date
                                                , end_date = fc_df$end_date
                                                , cloud_mask = NULL
                                                , base_dir = cube_directory
                                                , period = temporal_run
                                                , force_new = TRUE
                                                , cores = envFunc::use_cores(absolute_max = yaml::read_yaml("settings/cores.yaml")$process_cores)
                                                # gdalcubes::write_tif args
                                                , pack = list(type = "int16"
                                                              , scale = fc_df$scale
                                                              , offset = fc_df$offset
                                                              , nodata = -32768
                                                              )
                                                )
               , pattern = map(fc_df)
               , format = "file"
               , deployment = "main"
               )
  )
