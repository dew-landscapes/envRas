
# wo

library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/save_satellite_layer.R"
             , "R/make_indice.R"
             , "R/make_cube_dir.R"
             , "R/make_layer_df.R"
             , "R/aggregate_ras.R"
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
  , tar_target(name = max_date
               , paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-12-31") 
               )
  , tar_target(name = min_date
               , command = lubridate::as_date(max_date) - lubridate::as.period(envFunc::find_name(settings, "temp")) + lubridate::as.period("P1D")
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
               , rstac::stac(settings_wo$source_url) |>
                 rstac::stac_search(collections = settings_wo$collection
                                    , bbox = bbox
                                    , datetime = paste0(as.character(min_date)
                                                        , "/"
                                                        , as.character(max_date)
                                                        )
                                    ) |>
                 rstac::get_request() |>
                 rstac::items_fetch()
               )
  ## layers --------
  ### layer df --------
  , tar_target(layer_df
               , make_layer_df(layers = settings_wo$layers
                               , min_date
                               , max_date
                               , items
                               , period = settings$grain$temp
                               )
               , format = "parquet"
               )
  ### download --------
  , tar_target(name = layer
               , command = save_satellite_layer(items = items
                                                , base_grid = terra::rast(base_grid_path)
                                                , layer = layer_df$layer
                                                , agg_func = "median"
                                                , start_date = layer_df$start_date
                                                , end_date = layer_df$end_date
                                                , cloud_mask = NULL
                                                , base_dir = cube_directory
                                                , period = settings$grain$temp
                                                , force_new = FALSE
                                                # gdalcubes::write_tif args
                                                # none
                                                )
               , pattern = map(layer_df)
               , format = "file"
               )
)
