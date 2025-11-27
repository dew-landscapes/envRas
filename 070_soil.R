
library(targets)
library(tarchetypes)
library(geotargets)
library(crew)
library(crew.cluster)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_dist_rast.R"
             , "R/terra_reproject.R"
             , "R/make_coast_raster.R"
             )
           )

# cores --------
max_cores <- 100
use_cores <- min(max_cores, floor(parallel::detectCores() * 3 / 4))

# ram -------
total_terra_ram_prop <- 0.6 # across all cores
terra_memfrac <- total_terra_ram_prop / use_cores # prop of available memory allowed per core (or per tile)

# tar options --------
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$packages))
               , controller = crew_controller_local(workers = use_cores
                                                    , crashes_max = 0L
                                                    , options_local = crew_options_local(log_directory = fs::path(tars$soil$store, "log")
                                                                                         , log_join = TRUE
                                                                                         )
                                                    )
               )

# targets --------
targets <- list(
  ## settings -------
  ### setup -------
  tar_file_read(settings
                , "settings/setup.yaml"
                , yaml::read_yaml(!!.x)
                )
  ### soil -------
  , tar_file_read(settings_soil
                  , "settings/soil.yaml"
                  , yaml::read_yaml(!!.x)
                  )
  ## cube directory ------
  , tar_target(cube_directory
               , name_env_tif(x = c(settings$extent
                                    , list(temp = settings_soil$grain$temp, res = settings$grain$res)
                                    , source = settings_soil$source
                                    , collection = settings_soil$collection
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
  ### bbox -------
  , tar_target(bbox
               , sf::st_bbox(terra::rast(base_grid_path)) |>
                 sf::st_as_sfc() |>
                 terra::vect() |>
                 terra::densify(50000) |>
                 sf::st_as_sf() |>
                 sf::st_transform(crs = settings$crs$decdeg) |>
                 sf::st_bbox()
               )
  ## download cube -------
  ## prep -------
  ### dates -------
  , tar_target(name = max_date
               , paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-12-31") 
               )
  , tar_target(name = min_date
               , command = lubridate::as_date(max_date) - lubridate::as.period(envFunc::find_name(settings, "temp")) + lubridate::as.period("P1D")
               )
  ### files --------
  , tar_target(layer_df
               , SLGACloud::getProductMetaData() |>
                 tibble::as_tibble() |>
                 dplyr::filter(Code %in% settings_soil$code
                               , isCurrentVersion == 1
                               , grepl("Modelled", Component)
                               ) |>
                 tidyr::nest(data = -c(Source, Code, Attribute)) |>
                 dplyr::mutate(out_file = fs::path(directory
                                                   , ""
                                                   )
                               )
               )
  , tar_target(layer
               , save_soil_layer(layer_df$data
                                 
                                 )
               , pattern = map(layer_df)
               , format = "file"
               )
)

