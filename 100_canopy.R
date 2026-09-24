
# canopy

library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_cube_dir.R"
             , "R/save_canopy_layer.R"
             , "R/create_esri_xml.R"
             )
           )

# tar options ------
envTargets::env_tar_option_set("canopy")

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
  ### canopy ------
  , tar_file_read(settings_canopy
                  , "settings/canopy.yaml"
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
                               , set_source = settings_canopy
                               , cube_dir = settings$cube_dir
                               )
               , format = "file"
               )
  ### base grid -------
  , tar_target(base_grid_path
               , fs::path(dirname(cube_directory), "base.tif") |>
                 as.character()
               )
  ## download -------
  , tar_target(layer
               , save_canopy_layer(aoi_sf = extent_sf
                                   , base_grid_path
                                   , out_file = fs::path(cube_directory
                                                         , "chm__eth__static.tif"
                                                         )
                                   , force_new = TRUE
                                   )
               )
  )
