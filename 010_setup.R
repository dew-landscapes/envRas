
library(targets)
library(geotargets)
library(tarchetypes)
library(crew)
library(crew.cluster)

# tars -------
# tars <- yaml::read_yaml("_targets.yaml")

# tar options -------
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$packages))
               , controller = crew_controller_local(workers = floor(parallel::detectCores() * (3 / 4))
                                                    , crashes_max = 0L
                                                    , options_local = crew_options_local(log_directory = fs::path(yaml::read_yaml("_targets.yaml")$setup$store, "log")
                                                                                         , log_join = TRUE
                                                                                         )
                                                    )
               )

# source -------
tar_source(c("R/save_geoparquet.R"))

list(
  # targets --------
  ## settings-------
  ### setup -------
  tar_target(name = set_file
               , command = fs::path("settings/setup.yaml")
               , format = "file"
               , deployment = "main"
               )
  , tar_target(name = settings
               , command = yaml::read_yaml(set_file)
               , deployment = "main"
               )
  ## extent directory -------
  , tar_target(extent_dir
               , envFunc::name_env_out(set_list = list(extent = settings$extent)
                                       , base_dir = envFunc::get_env_dir(linux_default = "/mnt/envcube", windows_default = "I:")
                                       )$path |>
                 fs::dir_create()
               )
  ## maps -------
  ### extent sf -------
  , tar_target(name = extent_sf_file
               , command = fs::path("..", "..", "..", "data", "vector", paste0(settings$extent$vector, ".parquet"))
               , format = "file"
               )
  , tar_target(name = extent_sf
               , command = sfarrow::st_read_parquet(extent_sf_file) |>
                 make_aoi(filt_col = settings$extent$filt_col
                          , filt_level = settings$extent$filt_level
                          , buffer = settings$extent$buffer
                          , out_crs = settings$crs$proj
                          )
               )
  , tar_target(name = extent_sf_save
               , save_geoparquet(extent_sf
                                 , out_file = fs::path(extent_dir
                                                       , "aoi.parquet"
                                                       )
                                 )
               , format = "file"
               )
  ## read me --------
  , tar_target(readme_file
               , "cubes.txt"
               , format = "file"
               )
  , tar_target(readme
               , fs::file_copy(readme_file
                               , fs::path(extent_dir, "ReadMe.txt")
                               , overwrite = TRUE
                               )
               , format = "file"
               )
  )

