
library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/generate_random_env.R"
             , "R/generate_random_points.R"
             , "R/make_env_df.R"
             )
           )

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
  , tar_file_read(settings_random
                  , "settings/random.yaml"
                  , yaml::read_yaml(!!.x)
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
                                               , tile_size = 500000
                                               )
               , format = "parquet"
               )
  # generate random cells per tile
  , tar_target(random_points
               , generate_random_points(extent_df = tiles_df
                                        , base_grid_path = base_grid_path
                                        , samp_prop = settings_random$samp_prop
                                        , out_epsg = settings$crs$decdeg
                                        )
               , pattern = map(tiles_df)
               )
  #### apply ---------
  , tar_target(name = random_env_branches
               , command = make_env_df(points_df = random_points
                                       , ras_df = env_df
                                       , in_epsg = settings$crs$decdeg # comes from out_epsg in random_points
                                       )
               , pattern = map(random_points)
               , format = "parquet"
               )
  #### combine -------
  , tar_target(random_env
               , dplyr::bind_rows(random_env_branches)
               , format = "parquet"
               )
  ### env info
  , tar_target(env_df_info
               , random_env |>
                 dplyr::filter(!is.na(cell_lat), !is.na(cell_long)) |>
                 tidyr::pivot_longer(cols = tidyselect::any_of(env_df$name)) |>
                 envFunc::summarise_long_df() |>
                 envRaster::env_add_info() |>
                 dplyr::mutate(dplyr::across(c(mean, sd, max, min
                                               , tidyselect::matches("^q\\d{2}")
                                               )
                                             , \(x) (x * scale) + offset
                                             )
                               ) |>
                 dplyr::mutate(env_id = gsub("[[:punct:]]", "", name))
               )
  )

