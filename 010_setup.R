
library(targets)
library(tarchetypes)
library(crew)

# tars -------
# tars <- yaml::read_yaml("_targets.yaml")

# tar options -------
envTargets::env_tar_option_set("setup")

# source -------
tar_source(c("R/save_geoparquet.R"
             , "R/path_create_tar.R"
             , "R/make_cube_dir.R"
             )
           )

list(
  # targets --------
  ## settings-------
  ### setup -------
  tar_file_read(settings_raw
                , fs::path("settings/setup.yaml")
                , yaml::read_yaml(!!.x)
                )
  , tar_target(scales_file
               , fs::path("settings/scales.yaml")
               , format = "file"
               )
  , tar_target(settings
               , c(settings_raw
                   , envFunc::extract_scale(scales = scales_file)
                   )
               )
  ### satellite ------
  # for base grid
  , tar_file_read(settings_satellite
                  , "settings/satellite.yaml"
                  , yaml::read_yaml(!!.x)
                  )
  ## extent directory -------
  , tar_target(extent_dir
               , path_create_tar(envFunc::name_env_out(set_list = list(extent = settings$extent)
                                                       , base_dir = envFunc::get_env_dir(linux_default = settings$cube_dir)
                                                       )$path
                                 )
               )
  ## maps -------
  ### extent sf -------
  , tar_target(name = extent_sf_file
               , command = fs::path(settings$data_dir, "vector", paste0(settings$extent$vector, ".parquet"))
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
  ## cube directory ------
  , tar_target(cube_directory
               , make_cube_dir(set_scale = settings
                               , set_source = settings_satellite
                               , cube_dir = settings$cube_dir
                               )
               )
  ### base grid -------
  , tar_target(base_grid_path
               , envRaster::make_base_grid(aoi = extent_sf
                                           , out_res = settings$grain$res
                                           , out_epsg = settings$crs$proj
                                           , use_mask = extent_sf
                                           , out_file = fs::path(dirname(cube_directory), "base.tif")
                                           , overwrite = TRUE
                                           , ret = "path"
                                           , datatype = "INT1U"
                                           )
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

