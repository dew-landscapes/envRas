
library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_dist_rast.R"))

# tar options --------
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
  ### water -------
  , tar_target(water_file
               , fs::path("..", "..", "..", "data", "vector", "water_lines.parquet")
               , format = "file"
               )
  , tar_target(water_sf
               , sfarrow::st_read_parquet(water_file)
               )
  ### base grid -------
  , tar_target(base_grid_path
               , fs::path(dirname(cube_directory), "base.tif")
               , format = "file"
               )
  ## coast--------
  , tar_target(coast
               , make_dist_rast(base_grid_path
                                , sf = coast_sf
                                , out_file = fs::path(cube_directory
                                                      , "coast.tif"
                                                      )
                                )
               , format = "file"
               )
  )