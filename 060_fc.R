
# Fractional cover

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
             )
           )

# tar options --------
# parallel over individual layer rather than across layers, so no need for crew_controller_local etc
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$packages)))

targets <- list(
  # targets --------
  ## settings -------
  ### setup -------
  tar_target(name = set_file
               , command = fs::path("settings/setup.yaml")
               , format = "file"
               )
  , tar_target(name = settings
               , command = yaml::read_yaml(set_file)
               )
  ### fc ------
  , tar_target(set_file_fc
               , fs::path("settings/fc.yaml")
               , format = "file"
               )
  , tar_target(settings_fc
               , yaml::read_yaml(set_file_fc)
               )
  ## external objects ------
  , tar_target(extent_sf_file
               , fs::path(tars$setup$store, "objects", "extent_sf")
               , format = "file"
               )
  , tar_target(extent_sf
               , readRDS(extent_sf_file)
               )
  ## cube directory ------
  , tar_target(cube_directory
               , name_env_tif(x = c(settings$extent
                                    , settings$grain
                                    , source = settings_fc$source
                                    , collection = settings_fc$collection
                                    )
                              , context_defn = c("vector", "filt_col", "filt_level", "buffer")
                              , cube_defn = c("temp", "res")
                              , dir_only = TRUE
                              , prefixes = c("sat", "use")
                              , fill_null = TRUE
                              )$out_dir %>%
                 fs::path("I:", .)
               )
  ## make cube directory --------
  , tar_target(make_cube_dir
               , fs::dir_create(cube_directory)
               )
  ### base grid -------
  , tar_target(base_grid_path
               , fs::path(dirname(cube_directory), "base.tif")
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
               , rstac::stac(settings_fc$source_url) |>
                 rstac::stac_search(collections = settings_fc$collection
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
               , tibble::tibble(layer = settings_fc$layers)
               )
  ### download --------
  , tar_target(name = layer
               , command = save_satellite_layer(items = items
                                                , base_grid = terra::rast(base_grid_path)
                                                , layer = layer_df$layer
                                                , agg_func = "median"
                                                , start_date = min_date
                                                , end_date = max_date
                                                , cloud_mask = NULL
                                                , base_dir = cube_directory
                                                , period = settings$grain$temp
                                                , force_new = TRUE
                                                # gdalcubes::write_tif args
                                                # none
                                                )
               , pattern = map(layer_df)
               , format = "file"
               )
  )
