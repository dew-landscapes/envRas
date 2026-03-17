
library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/terra_reproject.R"
             , "R/save_soil_layer.R"
             , "R/make_cube_dir.R"
             )
           )

# tar options --------
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$packages))
               # , controller = crew_controller_local(workers = use_cores
               #                                      , crashes_max = 0L
               #                                      , options_local = crew_options_local(log_directory = fs::path(tars$soil$store, "log")
               #                                                                           , log_join = TRUE
               #                                                                           )
               #                                      )
               )

# targets --------
targets <- list(
  ## settings -------
  ### setup -------
  tar_file_read(settings
                , fs::path(tars$setup$store, "objects", "settings")
                , readRDS(!!.x)
                )
  ### soil -------
  , tar_file_read(settings_soil
                  , "settings/soil.yaml"
                  , yaml::read_yaml(!!.x)
                  )
  ## cube directory ------
  , tar_target(cube_directory
               , make_cube_dir(set_scale = settings
                               , set_source = settings_soil
                               , cube_dir = settings$cube_dir
                               )
               , format = "file"
               )
  ### base grid -------
  , tar_target(base_grid_path
               , fs::path(dirname(cube_directory), "base.tif")
               , format = "file"
               )
  ## download cube -------
  ## prep -------
  ### dates -------
  , tar_target(name = min_date
               , command = "static"
               )
  ### files --------
  , tar_target(layer_df
               , SLGACloud::getProductMetaData() |>
                 tibble::as_tibble() |>
                 dplyr::filter(Code %in% settings_soil$code
                               , isCurrentVersion == 1
                               , grepl("Modelled", Component)
                               ) |>
                 tidyr::nest(data = -c(Source, Code, Attribute)) |>
                 dplyr::mutate(out_file = fs::path(cube_directory
                                                   , paste0(Code, "__swm__", min_date, ".tif")
                                                   )
                               )
               , format = "parquet"
               )
  ### soil crs --------
  , tar_target(soil_crs
               , terra::rast(layer_df$data[[1]]$StagingPath[[1]], vsi = TRUE) |>
                 terra::crs(describe = TRUE) |>
                 dplyr::pull(code) |>
                 as.numeric()
               )
  ## get soil data ------
  , tar_target(layer
               , save_soil_layer(paths_df = layer_df$data[[1]]
                                 , key = Sys.getenv("TERN_API_KEY")
                                 , in_crs = soil_crs
                                 , grid_path = base_grid_path
                                 , out_file = layer_df$out_file
                                 , force_new = FALSE
                                 )
               , pattern = map(layer_df)
               , format = "file"
               )
)

