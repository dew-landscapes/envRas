
library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_cube_dir.R"
             , "R/make_indice.R"
             )
           )


# tar options --------
envTargets::env_tar_option_set("sat_ind"
                               , controller_local_args = list(workers = yaml::read_yaml("settings/cores.yaml")$download_cores
                                                              , name = "main_controller"
                                                              )
                               )

targets::tar_option_set(trust_timestamps = TRUE)

# targets --------
targets <- list(
  ## settings -------
  ### setup -------
  tar_file_read(settings
                , fs::path(tars$setup$store, "objects", "settings")
                , readRDS(!!.x)
                )
  ### satellite ------
  , tar_file_read(settings_satellite
                  , "settings/satellite.yaml"
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
                               , set_source = settings_satellite
                               , cube_dir = settings$cube_dir
                               )
               )
  ## prep -------
  ## indices------
  , tar_target(index_prep
               , envRaster::name_env_tif(cube_directory, parse = TRUE) |>
                 dplyr::select(layer, start_date)
               , cue = targets::tar_cue("always")
               )
  ### indice_df --------
  , tar_target(indice_df
               , settings_satellite$indices |>
                 tibble::as_tibble() |>
                 dplyr::mutate(layer_index = paste0("layer_", dplyr::row_number())) |>
                 tidyr::pivot_longer(tidyselect::any_of(names(settings_satellite$indices))
                                     , names_to = "index"
                                     , values_to = "layer"
                                     ) |>
                 dplyr::inner_join(index_prep
                                   , relationship = "many-to-many"
                                   ) |>
                 dplyr::mutate(layer = fs::path(cube_directory
                                               , paste0(layer
                                                        , "__median__"
                                                        , start_date
                                                        , ".tif"
                                                        )
                                               )
                               ) |>
                 tidyr::pivot_wider(names_from = layer_index
                                    , values_from = layer
                                    ) |>
                 dplyr::rename(layer = index) |>
                 dplyr::left_join(envRaster::ras_layers |>
                                    dplyr::select(layer, scale, offset)
                                  )
               )
  ### mung -------
  , tar_target(name = indice
               , command = make_indice(index_name = indice_df$layer
                                       , path_1 = indice_df$layer_1
                                       , path_2 = indice_df$layer_2
                                       , scale = indice_df$scale
                                       , offset = indice_df$offset
                                       , terra_options = list(memfrac = 0.1)
                                       , force_new = FALSE
                                       )
               , pattern = map(indice_df)
               , format = "file"
               )
  )
