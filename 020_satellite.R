
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
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$packages))
               # , controller = crew_controller_local(workers = floor(parallel::detectCores() * 2 / 3) # number of cores downloading from DEA
               #                                      , crashes_max = 0L
               #                                      , options_local = crew_options_local(log_directory = fs::path(tars$satellite$store, "log"))
               #                                      )
               # , storage = "worker"
               # , retrieval = "worker"
               )

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
  ### satellite ------
  , tar_target(set_file_satellite
               , fs::path("settings/satellite.yaml")
               , format = "file"
               )
  , tar_target(settings_satellite
               , yaml::read_yaml(set_file_satellite)
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
                                    , source = settings_satellite$source
                                    , collection = settings_satellite$collection
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
               , format = "file"
               )
  ### base grid -------
  , tar_target(base_grid_path
               , make_base_grid(aoi = extent_sf
                                , out_res = settings$grain$res
                                , out_epsg = settings$crs$proj
                                , use_mask = extent_sf
                                , out_file = fs::path(dirname(make_cube_dir), "base.tif")
                                , overwrite = TRUE
                                , ret = "path"
                                , datatype = "INT1U"
                                )
               , format = "file"
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
               , rstac::stac(settings_satellite$source_url) |>
                 rstac::stac_search(collections = settings_satellite$collection
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
               , tibble::tibble(layer = settings_satellite$layers)
               )
  ### download --------
  , tar_target(name = layer
               , command = save_satellite_layer(items = items
                                                , base_grid = terra::rast(base_grid_path)
                                                , layer = layer_df$layer
                                                , start_date = min_date
                                                , end_date = max_date
                                                , cloud_mask = NULL
                                                , base_dir = cube_directory
                                                , settings = c(settings, settings_satellite)
                                                , force_new = TRUE
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
  ## variability-----
  ### variability_df------
  , tar_target(name = variability_df
               , tibble::tibble(layer = settings_satellite$variability)
               )
  ### download --------
  , tar_target(name = variability
               , command = save_satellite_layer(items = items
                                                , base_grid = terra::rast(base_grid_path)
                                                , layer = variability_df$layer
                                                , start_date = min_date
                                                , end_date = max_date
                                                , cloud_mask = NULL
                                                , base_dir = cube_directory
                                                , settings = c(settings, settings_satellite)
                                                , force_new = TRUE
                                                # no pack
                                                )
               , pattern = map(variability_df)
               , format = "file"
               )
  ## indices------
  ### indices list --------
  , tar_target(indice_list
               , settings_satellite$indices
               )
  ### layers --------
  , tar_target(combined_layers
               , layer |>
                 purrr::set_names(settings_satellite$layers)
               )
  ### mung -------
  , tar_target(name = indice
               , command = make_indice(indice = indice_list[1]
                                       , start_date = min_date
                                       , layers = combined_layers
                                       , base_dir = cube_directory
                                       , settings = settings_satellite
                                       , force_new = TRUE
                                       )
               , pattern = map(indice_list)
               , format = "file"
               )
  )
