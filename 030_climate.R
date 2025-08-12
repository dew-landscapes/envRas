
library(targets)
library(geotargets)
library(tarchetypes)
library(crew)
library(crew.cluster)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/download_nc.R"
             , "R/make_bioclim_rasters.R"
             , "R/align_ras.R"
             )
           )

# mappings -------
mappings <- yaml::read_yaml("settings/climate.yaml")$layers

# tar options --------
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$packages))
               , controller = crew_controller_local(workers = 10 # downloading from NCI on main so not parallel
                                                    , crashes_max = 0L
                                                    , options_local = crew_options_local(log_directory = fs::path(tars$satellite$store, "log"))
                                                    )
               )

targets <- list(
  # targets --------
  ## settings -------
  ### setup -------
  tar_target(name = set_file
               , command = fs::path("settings/setup.yaml")
               , format = "file"
               )
  , tar_target(name = settings
               , command = yaml::read_yaml(set_file)
               )
  ### climate ------
  , tar_target(set_file_climate
               , fs::path("settings/climate.yaml")
               , format = "file"
               )
  , tar_target(settings_climate
               , yaml::read_yaml(set_file_climate)
               )
  ## external objects ------
  , tar_target(extent_sf_file
               , fs::path(tars$setup$store, "objects", "extent_sf")
               , format = "file"
               )
  , tar_target(extent_sf
               , readRDS(extent_sf_file)
               )
  ## cube directory ------
  , tar_target(cube_directory
               , name_env_tif(x = c(settings$extent
                                    , settings_climate$grain
                                    , source = settings_climate$source
                                    , collection = settings_climate$collection
                                    )
                              , context_defn = c("vector", "filt_col", "filt_level", "buffer")
                              , cube_defn = c("temp", "res")
                              , dir_only = TRUE
                              , prefixes = c("sat", "use")
                              , fill_null = TRUE
                              )$out_dir %>%
                 fs::path("I:", .)
               )
  ### make cube directory --------
  , tar_target(make_cube_dir
               , fs::dir_create(cube_directory)
               , format = "file"
               )
  ### base grid -------
  , tar_target(base_grid_path
               , make_base_grid(extent_sf
                                , out_res = settings_climate$grain$res
                                , out_epsg = settings$crs$proj
                                , use_mask = extent_sf
                                , out_file = fs::path(dirname(cube_directory), "base.tif")
                                , overwrite = TRUE
                                , ret = "path"
                                , datatype = "INT1U"
                                )
               , format = "file"
               )
  ## prep -------
  ### dates -------
  , tar_target(name = max_date
               , lubridate::as_date(paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-12-31"))
               )
  , tar_target(name = min_date
               , command = lubridate::as_date(max_date) - lubridate::as.period(envFunc::find_name(settings, "temp")) + lubridate::as.period("P1D")
               )
  #### bbox -------
  , tar_target(bbox
               , sf::st_bbox(terra::rast(base_grid_path)) |>
                 sf::st_as_sfc() |>
                 sf::st_transform(crs = settings$crs$decdeg) |> # need decimal lat/long for rstac
                 sf::st_bbox()
               )
  ## download cube ---------
  ### epoch df -------
  , tar_target(download_files_df
               , envFunc::make_epochs(start_year = lubridate::year(min_date)
                                      , end_year = lubridate::year(max_date)
                                      , epoch_step = readr::parse_number(settings$grain$temp)
                                      , epoch_overlap = FALSE
                                      ) |>
                 tidyr::unnest(cols = c(years)) |>
                 dplyr::cross_join(tibble::tibble(month = stringr::str_pad(1:12, 2, pad = 0))) |>
                 dplyr::cross_join(tibble::tibble(layer = settings_climate$layers)) |>
                 dplyr::mutate(remote_file = paste0(settings_climate$source_url
                                                      , "/"
                                                      , layer
                                                      , "/"
                                                      , years
                                                      , "/"
                                                      , paste0("ANUClimate_v2-0_"
                                                               , layer
                                                               , "_monthly_"
                                                               , years
                                                               , month
                                                               , ".nc"
                                                               )
                                                      )
                               ) |>
                 tidyr::nest(remote_files = c(years, remote_file)) |>
                 dplyr::mutate(start_date = lubridate::as_date(paste0(start_year
                                                                      , "-"
                                                                      , month
                                                                      , "-01"
                                                                      )
                                                               )
                               , out_file = fs::path(cube_directory
                                                     , paste0(layer
                                                              , "__"
                                                              , start_date
                                                              , ".tif"
                                                              )
                                                     )
                               )
               )
  ### download ------
  , tar_target(name = nc_download
              , command = download_nc(save_file = download_files_df$out_file
                                      , remote_files = download_files_df$remote_files[[1]]$remote_file
                                      , bbox = bbox
                                      , force_new = FALSE
                                      )
              , pattern = map(download_files_df)
              , format = "file"
              , deployment = "main"
              # This partially ran in parallel (maybe 2-3 out of 6 layers returned before error)
              # but would usually fail in parallel with: error in `RNetCDF::open.nc()`: ! NetCDF: Write to read only
              )
  ## bioclim ------
  , tar_target(bioclim_files_df
               , tibble::tibble(path = nc_download) |>
                 name_env_tif(parse = TRUE) |>
                 tidyr::nest(files = c(start_date, name, path))
               )
  , tar_target(name = bioclim
               , command = make_bioclim_rasters(bioclim_files_df
                                                , out_dir = cube_directory
                                                , start_date = min_date
                                                , force_new = FALSE
                                                )
               , format = "file"
               )
  ## align -------
  , tar_target(align_df
               , tibble::tibble(path = bioclim)
               )
  , tar_target(align_grid_path
               , tar_read(base_grid_path
                          , store = tars$satellite$store
                          )
               )
  , tar_target(name = aligned
               , command = align_ras(input_ras_path = align_df$path[[1]]
                                     , base_grid_path = align_grid_path
                                     , in_res = settings_climate$grain$res
                                     , out_res = settings$grain$res
                                     , force_new = TRUE
                                     )
               , format = "file"
               , pattern = map(align_df)
               )
  )
