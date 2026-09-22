
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
             )
           )

# tar options --------
# parallel over individual layer rather than across layers, so no need for crew_controller_local etc
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$packages)))

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
  , tar_target(name = max_date
               , paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-12-31")
               )
  , tar_target(name = min_date
               , command = lubridate::as_date(max_date) - lubridate::as.period(envFunc::find_name(settings, "extent_time")) + lubridate::as.period("P1D")
               )
  , tar_target(date_df
               , make_date_df(min_date = min_date
                              , max_date = max_date
                              , grain_time = envFunc::find_name(settings, "grain_time")
                              , run_time = envFunc::find_name(settings, "run_time")
                              )
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
  , tar_target(layer_df
               , tibble::tibble(layer = settings_fc$layers) |>
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
                                                # none
                                                )
               , pattern = map(layer_df)
               , format = "file"
               )
  )
