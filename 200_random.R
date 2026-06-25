
library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source("R/generate_random_env.R")

# tar options -------
envTargets::env_tar_option_set("random")
tar_option_set(trust_timestamps = TRUE) # avoid hashing large tif files

list(
  # targets --------
  ## from setup -------
  ### settings -------
  tar_file_read(settings
                , fs::path(tars$setup$store, "objects", "settings")
                , readRDS(!!.x)
                )
  ### extent -------
  , tar_file_read(extent_sf
                  , fs::path(tars$setup$store, "objects", "extent_sf")
                  , readRDS(!!.x)
                  )
  ## env_df (rasters) --------
  , tar_target(env_df
               , envRaster::prepare_env(set_list = list(extent = settings$extent, grain = settings$grain)
                                        , base_dir = settings$cube_dir
                                        )
               )
  , tar_files(ras_paths
              , env_df$path
              , deployment = "main"
              )
  ## random -------
  #### split ------
  , tar_target(base_grid_path
               , tar_read(base_grid_path, store = tars$satellite$store)
               , format = "file"
               )
  , tar_target(tiles_df
               , envTargets::make_tile_extents(base_grid_path = base_grid_path
                                               , aoi = extent_sf
                                               , tile_length = NULL
                                               , tile_size = 100000
                                               ) |>
                 envTargets::env_tar_group(column = "ha")
               , format = "parquet"
               , iteration = "group"
               )
  #### apply ---------
  , tar_target(random_env_branches
               , generate_random_env(ras_path = ras_paths
                                     , extent_df = tiles_df
                                     , out_epsg = settings$crs$decdeg
                                     , base_grid_path = base_grid_path
                                     )
               , format = "parquet"
               , pattern = cross(tiles_df, ras_paths)
               )
  #### combine -------
  , tar_target(random_env
               , dplyr::bind_rows(random_env_branches)
               , format = "parquet"
               )
  
  ## combined -------
  # , tar_target(comb_env
  #              , env_data |>
  #                dplyr::distinct(dplyr::across(tidyselect::any_of(c(bin
  #                                                                   , env_df$name
  #                                                                   )
  #                                                                 )
  #                                              )
  #                                ) |>
  #                dplyr::mutate(sourced = "floristic bins") |>
  #                dplyr::bind_rows(lc_env |>
  #                                   dplyr::distinct(dplyr::across(tidyselect::any_of(c(bin
  #                                                                                      , env_df$name
  #                                                                                      )
  #                                                                                    )
  #                                                                 )
  #                                                   ) |>
  #                                   dplyr::mutate(sourced = "landcover bins")
  #                                 ) |>
  #                dplyr::bind_rows(random_env  |>
  #                                   # don't want any bins duplicated between bio, lc and random (bio > lc > random)
  #                                   # do anti_joins here rather than in random_env so that random_env isn't dependent on env_data and lc_env
  #                                   dplyr::anti_join(env_data |>
  #                                                      na.omit() |>
  #                                                      dplyr::distinct(dplyr::across(tidyselect::any_of(bin)))
  #                                                    ) |>
  #                                   dplyr::anti_join(lc_env|>
  #                                                      na.omit() |>
  #                                                      dplyr::distinct(dplyr::across(tidyselect::any_of(bin)))
  #                                                    )|>
  #                                   dplyr::distinct(dplyr::across(tidyselect::any_of(c(bin
  #                                                                                      , env_df$name
  #                                                                                      )
  #                                                                                    )
  #                                                                 )
  #                                                   ) |>
  #                                   dplyr::mutate(sourced = "random bins")
  #                                 ) |>
  #                dplyr::select(! matches("ID$|id$")) |>
  #                dplyr::distinct() |>
  #                dplyr::filter(!is.na(cell_lat), !is.na(cell_long))
  #              , format = "parquet"
  #              )
  )

