
library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_dist_tile.R"
             , "R/combine_tiles.R"
             , "R/make_cube_dir.R"
             )
           )

# tar options --------
envTargets::env_tar_option_set("coast")

# targets --------
targets <- list(
  ## settings -------
  ### settings -------
  tar_file_read(settings
                , fs::path(tars$setup$store, "objects", "settings")
                , readRDS(!!.x)
                )
  , tar_file_read(extent_sf
                  , fs::path(tars$setup$store, "objects", "extent_sf")
                  , readRDS(!!.x)
                  )
  ### coast ------
  , tar_file_read(settings_coast
                  , "settings/coast.yaml"
                  , yaml::read_yaml(!!.x)
                  )
  ## cube directory ------
  , tar_target(cube_directory
               , make_cube_dir(set_scale = settings
                               , set_source = settings_coast
                               , cube_dir = settings$cube_dir
                               )
               , format = "file"
               )
  ### base grid path-------
  , tar_target(base_grid_path
               , fs::path(dirname(cube_directory), "base.tif") |>
                 as.character()
               )
  ## maps --------
  ### coast ------
  , tar_target(coast_file
               , fs::path(settings$data_dir, "vector", "aus.parquet")
               , format = "file"
               )
  ### coast mask --------
  # no separate mask needed as the coast is the mask
  ## prep -------
  ### dates -------
  , tar_target(name = min_date
               , command = "static"
               )
  ### out file --------
  , tar_target(coast_tif_file
                 , fs::path(cube_directory
                            , "coast__distance__static.tif"
                            )
                 )
  ## coast--------
  ### split -------
  , tar_target(name = tile_extents
               , envTargets::make_tile_extents(base_grid_path = base_grid_path)
               )
  ### apply -------
  , tar_target(tile_coast
               , make_dist_tile(base_grid_path = base_grid_path
                                , extent = tile_extents
                                , sf_dist_file = coast_file
                                , sf_mask_file = coast_file # = coast_mask_file
                                , sf_mask_positive = TRUE
                                , dist_limit = 5000
                                , out_dir = fs::path(tars$coast$store, "tiles")
                                , force_new = FALSE
                                # via dots... to terra::lapp
                                , wopt = list(datatype = "INT2S") # easily encompasses -10000 to 10000 m
                                )
               , pattern = map(tile_extents)
               , format = "file"
               )
  ### combine -------
  , tar_target(coast
               , combine_tiles(tiles = tile_coast
                               , out_file = coast_tif_file
                               , sf_mask = extent_sf
                               # via dots to terra::writeRaster
                               , datatype = "INT2S" # easily encompasses -10000 to 10000 m
                               , gdal = c("TILED=YES"
                                          , "COPY_SRC_OVERVIEWS=YES"
                                          , "COMPRESS=DEFLATE"
                                          )
                               , names = "distance"
                               )
               , format = "file"
               )
  )
