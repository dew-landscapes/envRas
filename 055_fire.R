
library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_cube_dir.R"
             , "R/make_polygon_overlap_tile.R"
             , "R/combine_tiles.R"
             )
           )


# tar options --------
envTargets::env_tar_option_set("fire")

mappings <- yaml::read_yaml("settings/fire.yaml")$methods

# targets --------
list(
  ## settings -------
  ### setup -------
  tar_file_read(settings
                , fs::path(tars$setup$store, "objects", "settings")
                , readRDS(!!.x)
                )
  , tar_file_read(extent_sf
                  , fs::path(tars$setup$store, "objects", "extent_sf")
                  , readRDS(!!.x)
                  )
  ### fire -------
  , tar_file_read(settings_fire
                  , "settings/fire.yaml"
                  , yaml::read_yaml(!!.x)
                  )
  ## cube directory ------
  , tar_target(cube_directory
               , make_cube_dir(set_scale = settings
                               , set_source = settings_fire
                               , cube_dir = settings$cube_dir
                               )
               , format = "file"
               )
  ### base grid path-------
  , tar_target(base_grid_path
               , fs::path(dirname(cube_directory), "base.tif") |>
                 as.character()
               )
  ## fires file ------
  , tar_target(fire_file
               , fs::path(settings$data_dir, "vector", "fire.parquet")
               , format = "file"
               )
  ## prep -------
  ### dates -------
  , tar_target(name = min_date
               , command = "static"
               )
  # MAP------
  , tar_map(values = tibble::tibble(method = mappings
                                    , tar_id = envTargets::make_tar_id(method)
                                    , out_file = fs::path(tars$fire$store
                                                          , paste0("tiles_", method)
                                                          )
                                    )
            , names = "tar_id"
    
    ## out file --------
    , tar_target(tif_file
                   , fs::path(cube_directory
                              , paste0("fire__"
                                       , method
                                       , "__"
                                       , min_date
                                       , ".tif"
                                       )
                              )
                   )
    ## fire--------
    ### split -------
    , tar_target(name = tile_extents
                 , envTargets::make_tile_extents(base_grid_path = base_grid_path)
                 )
    ### apply -------
    , tar_target(tiles
                 , make_polygon_overlap_tile(base_grid_path = base_grid_path
                                             , extent = tile_extents
                                             , polygon_file = fire_file
                                             , polygon_field = "fireyear"
                                             , polygon_func = method
                                             , out_dir = fs::path(tars$fire$store, paste0("tiles_", method))
                                             , force_new = FALSE
                                             # via dots... to terra::lapp
                                             , wopt = list(datatype = "INT2S") # covers a bit more than -32000 to 32000
                                             )
                 , pattern = map(tile_extents)
                 , format = "file"
                 )
      ### combine -------
      , tar_target(combine
                   , combine_tiles(tiles
                                   , out_file = tif_file
                                   , sf_mask = extent_sf
                                   # via dots
                                   , datatype = "INT2S" # covers a bit more than -27000 to 27000
                                   , gdal = c("TILED=YES"
                                              , "COPY_SRC_OVERVIEWS=YES"
                                              , "COMPRESS=DEFLATE"
                                              )
                                   , names = method
                                   )
                   , format = "file"
                   )
  )
)
