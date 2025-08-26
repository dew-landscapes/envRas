
library(targets)
library(tarchetypes)
library(geotargets)
library(crew)
library(crew.cluster)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_dist_rast.R"))

# cores --------
use_cores <- floor(parallel::detectCores() * 3 / 4)

# ram -------
total_terra_ram_prop <- 0.6 # across all cores
terra_memfrac <- total_terra_ram_prop / use_cores # prop of available memory allowed per core (or per tile)

# tar options --------
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$packages))
               , controller = crew_controller_local(workers = use_cores
                                                    , crashes_max = 0L
                                                    , options_local = crew_options_local(log_directory = fs::path(tars$distance_to$store, "log")
                                                                                         , log_join = TRUE
                                                                                         )
                                                    )
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
  ### distance_to -------
  , tar_target(name = set_file_distance_to
               , command = fs::path("settings/distance_to.yaml")
               , format = "file"
               )
  , tar_target(name = settings_distance_to
               , command = yaml::read_yaml(set_file_distance_to)
               )
  ## cube directory ------
  , tar_target(cube_directory
               , name_env_tif(x = c(settings$extent
                                    , settings$grain
                                    , source = settings_distance_to$source
                                    , collection = settings_distance_to$collection
                                    )
                              , context_defn = c("vector", "filt_col", "filt_level", "buffer")
                              , cube_defn = c("temp", "res")
                              , dir_only = TRUE
                              , prefixes = c("sat", "use")
                              , fill_null = TRUE
                              )$out_dir %>%
                 fs::path("I:", .)
               )
  ### make cube directory --------
  , tar_target(make_cube_dir
               , fs::dir_create(cube_directory)
               )
  ## maps -------
  ### coast ------
  , tar_target(coast_file
               , fs::path("..", "..", "..", "data", "vector", "aus.parquet")
               , format = "file"
               )
  , tar_target(coast_sf
               , sfarrow::st_read_parquet(coast_file)
               )
  ### base grid -------
  , tar_target(base_grid_path
               , fs::path(dirname(cube_directory), "base.tif")
               )
  ## coast--------
  ### split -------
  , tar_target(name = tile_extents
               , make_tile_extents(base_grid_path = base_grid_path
                                   , tile_size = 50000
                                   )
               )
  ### apply -------
  , tar_target(use_memfrac
               , if(nrow(tile_extents >= use_cores)) {terra_memfrac} else
                 {(total_ram * total_terra_ram_prop / nrow(tile_extents)) / total_ram}
               )
  , tar_terra_rast(tile_result
                   , make_dist_rast(base_grid_path
                                    , tile_extents
                                    , sf = coast_sf
                                    , terra_options = list(memfrac = use_memfrac)
                                    )
                   , datatype = "INT4S" # integer metre accuracy
                   , pattern = map(tile_extents)
                   )
  ### combine -------
  , tar_target(coast
               , combine_tiles(tile_result
                               , out_file = fs::path(cube_directory
                                                     , "coast__distance__2025-01-01.tif"
                                                     )
                               # passed via ... to terra::merge()
                               , datatype = "INT4S"
                               , overwrite = TRUE
                               )
               )
  )
