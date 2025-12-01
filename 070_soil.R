
library(targets)
library(tarchetypes)
library(geotargets)
library(crew)
library(crew.cluster)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_dist_rast.R"
             , "R/terra_reproject.R"
             , "R/make_coast_raster.R"
             , "R/save_soil_layer.R"
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
                , "settings/setup.yaml"
                , yaml::read_yaml(!!.x)
                )
  ### soil -------
  , tar_file_read(settings_soil
                  , "settings/soil.yaml"
                  , yaml::read_yaml(!!.x)
                  )
  ## cube directory ------
  , tar_target(cube_directory
               , name_env_tif(x = c(settings$extent
                                    , list(temp = settings_soil$grain$temp, res = settings$grain$res)
                                    , source = settings_soil$source
                                    , collection = settings_soil$collection
                                    )
                              , context_defn = c("vector", "filt_col", "filt_level", "buffer")
                              , cube_defn = c("temp", "res")
                              , dir_only = TRUE
                              , prefixes = c("sat", "use")
                              , fill_null = TRUE
                              )$out_dir %>%
                 fs::path("I:", .)
               )
  ## make cube directory --------
  , tar_target(make_cube_dir
               , fs::dir_create(cube_directory)
               )
  ### base grid -------
  , tar_file_read(base_grid_path
                  , fs::path(tars$satellite$store, "objects", "cube_directory")
                  , fs::path(dirname(readRDS(!!.x)), "base.tif")
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
               , SLGACloud::cogLoad(layer_df$data[[1]]$COGsPath[[1]]
                                    , api_key = Sys.getenv("TERN_API_KEY")
                                    ) |>
                 terra::crs()
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

