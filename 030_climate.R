
library(targets)
library(geotargets)
library(tarchetypes)
library(crew)
library(crew.cluster)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/save_climate_layer.R"
             , "R/get_thredds_data.R"
            , "R/align_cli.R"
             )
           )

# mappings -------
mappings <- yaml::read_yaml("settings/climate.yaml")$layers

# tar options --------
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$packages))
               # , controller = crew_controller_local(workers = floor(length(mappings) / 2) # number of cores downloading from DEA
               #                                      , crashes_max = 0L
               #                                      , options_local = crew_options_local(log_directory = fs::path(tars$satellite$store, "log"))
               #                                      )
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
  ### climate ------
  , tar_target(set_file_climate
               , fs::path("settings/climate.yaml")
               , format = "file"
               )
  , tar_target(settings_climate
               , yaml::read_yaml(set_file_climate)
               )
  ## from other stores-----
  ### cube directory-----
  , tar_target(cube_directory_file
               , fs::path(tars$setup$store, "objects", "cube_directory")
               , format = "file"
               )
  , tar_target(cube_directory
               , readRDS(cube_directory_file)
               )
  , tar_target(cube_directory_climate
               , gsub(paste0("__", settings$grain$res)
                      , paste0("__", settings_climate$grain$res)
                      , dirname(cube_directory)
                      )
               )
  ## prep -------
  ### dates -------
  , tar_target(name = max_date
               , lubridate::as_date(paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-12-31"))
               )
  , tar_target(name = min_date
               , command = lubridate::as_date(max_date) - lubridate::as.period(envFunc::find_name(settings, "temp")) + lubridate::as.period("P1D")
               )
  ### base grid -------
  , tar_target(base_grid_file
               , fs::path(tars$setup$store, "objects", "base_grid")
               , format = "file"
               )
  #### bbox -------
  , tar_target(bbox
               , sf::st_bbox(terra::rast(base_grid_file)) |>
                 sf::st_as_sfc() |>
                 sf::st_transform(crs = settings$crs$decdeg) |> # need decimal lat/long for rstac
                 sf::st_bbox()
               )
  )

## get data --------
### layers --------
downloads <- tar_map(values = tibble::tibble(layers = mappings)
                     , unlist = FALSE # Return a nested list from tar_map()
                     , tar_target(name = download
                                  , command = save_climate_layer(layer = layers
                                                                 , settings = c(settings, settings_climate)
                                                                 , start_date = min_date
                                                                 , end_date = max_date
                                                                 , bbox = sf::st_bbox(terra::rast(base_grid_file))
                                                                 , base_dir = cube_directory_climate
                                                                 )
                                  )
                     , tar_target(name = munged
                                  , command = align_cli(cli_ras_path = download
                                                        , settings
                                                        , base_grid_file = base_grid_file
                                                        )
                                  )
                     )

list(targets
     , downloads
     )
