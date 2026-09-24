
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
envTargets::env_tar_option_set("watercourse")

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
  ### wc ------
  , tar_file_read(settings_wc
                  , "settings/watercourse.yaml"
                  , yaml::read_yaml(!!.x)
                  )
  ## cube directory ------
  , tar_target(cube_directory
               , make_cube_dir(set_scale = settings
                               , set_source = settings_wc
                               , cube_dir = settings$cube_dir
                               )
               , format = "file"
               )
  ### base grid path-------
  , tar_target(base_grid_path
               , fs::path(dirname(cube_directory), "base.tif") |>
                 as.character()
               )
  ## maps -------
  ### wc ------
  , tar_target(wc_file
               , fs::path(settings$data_dir, "vector", "water_lines.parquet")
               , format = "file"
               )
  ### wc mask --------
  , tar_target(wc_mask_file
               , fs::path(settings$data_dir, "vector", "water_poly.parquet")
               , format = "file"
               )
  ## prep -------
  ### dates -------
  , tar_target(name = min_date
               , command = "static"
               )
  ### out file --------
  , tar_target(wc_tif_file
                 , fs::path(cube_directory
                            , paste0("watercourse__distance__"
                                     , min_date
                                     , ".tif"
                                     )
                            )
                 )
  ## wc--------
  ### split -------
  , tar_target(name = tile_extents
             , envTargets::make_tile_extents(base_grid_path = base_grid_path)
             )
  ### apply -------
  , tar_target(tile_wc
               , make_dist_tile(base_grid_path = base_grid_path
                                , extent = tile_extents
                                , sf_dist_file = wc_file
                                , sf_mask_file = wc_mask_file
                                , sf_mask_positive = FALSE
                                , dist_limit = 2000
                                , out_dir = fs::path(tars$watercourse$store, "tiles")
                                , force_new = TRUE
                                # via dots... to terra::lapp
                                , wopt = list(datatype = "INT2S")
                                )
               , pattern = map(tile_extents)
               , format = "file"
               )
  ### combine -------
  , tar_target(wc
               , combine_tiles(tile_wc
                               , out_file = wc_tif_file
                               , sf_mask = extent_sf
                               # via dots to terra::writeRaster
                               , datatype = "INT2S" # easily encompasses -1000 to 1000 m
                               , gdal = c("TILED=YES"
                                          , "COPY_SRC_OVERVIEWS=YES"
                                          , "COMPRESS=DEFLATE"
                                          )
                               , names = "distance"
                               )
               , format = "file"
               )
)
