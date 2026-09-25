
# dem

library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/get_items.R"
             , "R/save_satellite_layer.R"
             , "R/make_cube_dir.R"
             , "R/aggregate_ras.R"
             )
           )

# tar options ------
envTargets::env_tar_option_set("dem")

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
  ### dem ------
  , tar_file_read(settings_dem
                  , "settings/dem.yaml"
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
                               , set_source = settings_dem
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
  # these are just the dates recorded in the stac for dem
  , tar_target(name = max_date
               , "2014-12-31"
               )
  , tar_target(name = min_date
               , command = "2014-01-01"
               )
  , tar_target(date_df
               , tibble::tibble(start_date = min_date
                                , end_date = max_date
                                )
               )
  #### bbox -------
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
                                                   , \(x, y) get_items(url = settings_dem$source_url
                                                                       , collection = settings_dem$collection
                                                                       , bbox = bbox
                                                                       , min_date = x
                                                                       , max_date = y
                                                                       )
                                                   )
                               )
               )
  ## dem --------
  ### dem df --------
  , tar_target(dem_df
               , items |>
                 dplyr::cross_join(tibble::tibble(layer = settings_dem$layers)) |>
                 dplyr::cross_join(tibble::tibble(func = settings_dem$func)) |>
                 dplyr::left_join(envRaster::ras_layers |>
                                    dplyr::select(layer, scale, offset)
                                  )
               )
  ### download --------
  , tar_target(name = dem
               , command = save_satellite_layer(items = dem_df$items[[1]]
                                                , base_grid = terra::rast(base_grid_path)
                                                , layer = dem_df$layer
                                                , agg_func = dem_df$func
                                                , start_date = dem_df$start_date
                                                , end_date = dem_df$end_date
                                                , cloud_mask = NULL
                                                , base_dir = cube_directory
                                                , period = settings$grain$grain_time
                                                , force_new = FALSE
                                                , cores = envFunc::use_cores(absolute_max = yaml::read_yaml("settings/setup.yaml")$max_cores)
                                                # gdalcubes::write_tif args
                                                , pack = list(type = "int16"
                                                              , scale = dem_df$scale
                                                              , offset = dem_df$offset
                                                              , nodata = -32768
                                                              )
                                                )
               , format = "file"
               , deployment = "main"
               )
  ## mung to coarse grid ----------
  ## aggregate -------
  , tar_target(aggregate_grid_path
               , tar_read(run_time_layer, store = tars$climate$store)[[1]]
               , format = "file"
               )
  , tar_target(name = agg_mean
               , command = aggregate_ras(input_ras_path = dem
                                         , base_grid_path = aggregate_grid_path
                                         , in_res = settings$grain$res_x
                                         , out_res = envFunc::extract_scale("coarse", scales = scales_file)$grain$res_x
                                         , force_new = FALSE
                                         , agg_func = "mean"
                                         )
               , format = "file"
               )
  , tar_target(name = agg_sd
               , command = aggregate_ras(input_ras_path = dem
                                         , base_grid_path = aggregate_grid_path
                                         , in_res = settings$grain$res_x
                                         , out_res = envFunc::extract_scale("coarse", scales = scales_file)$grain$res_x
                                         , force_new = FALSE
                                         , agg_func = "sd"
                                         )
               , format = "file"
               )
)
