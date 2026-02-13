
# Tasseled cap

library(targets)
library(geotargets)
library(tarchetypes)
library(crew)
library(crew.cluster)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/save_satellite_layer.R"
             , "R/make_indice.R"
             , "R/make_cube_dir.R"
             , "R/make_layer_df.R"
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
                , "settings/setup.yaml"
                , yaml::read_yaml(!!.x)
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
                               )
               , format = "file"
               )
  ### base grid -------
  , tar_target(base_grid_path
               , fs::path(dirname(cube_directory), "base.tif")
               , format = "file"
               )
  ## prep -------
  ### dates -------
  , tar_target(name = max_date
               , paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-12-31") 
               )
  , tar_target(name = min_date
               , command = "1987-01-01" #lubridate::as_date(max_date) - lubridate::as.period(envFunc::find_name(settings, "temp")) + lubridate::as.period("P1D")
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
               , rstac::stac(settings_dem$source_url) |>
                 rstac::stac_search(collections = settings_dem$collection
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
               , make_layer_df(layers = settings_dem$layers
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
                                                , force_new = TRUE
                                                # gdalcubes::write_tif args
                                                # none
                                                )
               , pattern = map(layer_df)
               , format = "file"
               , cue = tar_cue(depend = FALSE)
               )
  )
