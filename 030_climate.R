
library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_date_df.R"
             , "R/download_nc_raw.R"
             , "R/mung_climate.R"
             , "R/make_bioclim_rasters.R"
             , "R/disagg_ras.R"
             , "R/make_cube_dir.R"
             )
           )

# mappings -------
mappings <- yaml::read_yaml("settings/climate.yaml")$layers

# tar options --------
envTargets::env_tar_option_set("climate")

targets <- list(
  # targets --------
  ## settings -------
  ### setup -------
  
  # GRAIN!!
  # access COARSE grain via envFunc::extract_scale("coarse", scales = scales_file)$grain$res
  # access FINE grain via settings$grain$res
  
  tar_file_read(settings
                , fs::path(tars$setup$store, "objects", "settings")
                , readRDS(!!.x)
  )
  , tar_target(scales_file
               , "settings/scales.yaml"
               , format = "file"
  )
  ### climate ------
  , tar_file_read(settings_climate
                  , "settings/climate.yaml"
                  , yaml::read_yaml(!!.x)
  )
  ## external objects ------
  , tar_file_read(extent_sf
                  , fs::path(tars$setup$store, "objects", "extent_sf")
                  , readRDS(!!.x)
  )
  ## cube directory ------
  , tar_target(cube_directory
               , make_cube_dir(set_scale = envFunc::extract_scale("coarse", scales = scales_file)
                               , set_source = settings_climate
                               , cube_dir = settings$cube_dir
               )
               , format = "file"
  )
  ## prep -------
  ### dates -------
  , tar_file_read(date_df
                  , fs::path(tars$setup$store, "objects", "date_df")
                  , arrow::read_parquet(!!.x)
  )
  ## raw climate -------
  , tar_target(raw_scales
               , envFunc::extract_scale("raw_climate"
                                        , scales = scales_file
               )
  )
  , tar_target(raw_directory
               , make_cube_dir(set_scale = raw_scales
                               , set_source = settings_climate
                               , cube_dir = settings$cube_dir
               )
  )
  , tar_target(raw_layer_df
               , tibble::tibble(start_date = seq(min(date_df$start_date), max(date_df$end_date), by = "month")) |>
                 dplyr::cross_join(tibble::tibble(layer = settings_climate$layers))|>
                 dplyr::mutate(func = dplyr::case_when(grepl("min", layer) ~ "min"
                                                       , grepl("max", layer) ~ "max"
                                                       , TRUE ~ "mean"
                 )
                 , remote_file = paste0("https://thredds.nci.org.au/thredds/fileServer/gh70/ANUClimate/v2-0/stable/month/"
                                        , layer
                                        , "/"
                                        , lubridate::year(start_date)
                                        , "/"
                                        , paste0("ANUClimate_v2-0_"
                                                 , layer
                                                 , "_monthly_"
                                                 , lubridate::year(start_date)
                                                 , stringr::str_pad(lubridate::month(start_date)
                                                                    , 2
                                                                    , pad = "0"
                                                 )
                                                 , ".nc"
                                        )
                 )
                 , out_file = fs::path(raw_directory
                                       , paste0(layer
                                                , "__"
                                                , func
                                                , "__"
                                                , start_date
                                                , ".nc"
                                       )
                 )
                 )
  )
  , tar_target(raw_layer
               , download_nc_raw(save_file = raw_layer_df$out_file
                                 , remote_file = raw_layer_df$remote_file
                                 , force_new = TRUE
               )
               , pattern = map(raw_layer_df)
               , format = "file"
               , deployment = "main"
  )
  ## run time climate ----------
  , tar_target(run_time_layer_df
               , date_df |>
                 dplyr::mutate(run_time_id = dplyr::row_number()
                               , start_date = purrr::map2(start_date
                                                          , end_date
                                                          , \(x, y) seq(x, y
                                                                        , "month"
                                                          )
                               )
                 ) |>
                 dplyr::select(- end_date) |>
                 tidyr::unnest(cols = c(start_date)) |>
                 dplyr::mutate(month = lubridate::month(start_date)) |>
                 dplyr::left_join(envRaster::name_env_tif(raw_directory, parse = TRUE) |>
                                    dplyr::left_join(tibble::enframe(raw_layer, name = "branch", value = "path")) |>
                                    dplyr::mutate(start_date = as.Date(start_date)) |>
                                    dplyr::select(start_date, layer, func, path)
                                  , relationship = "many-to-many"
                 ) |>
                 dplyr::mutate(out_file = fs::path(cube_directory
                                                   , paste0(layer, "__", func, "__"
                                                            , min(start_date)
                                                            , ".tif"
                                                   )
                 )
                 , .by = c(run_time_id, month, layer)
                 ) |>
                 tidyr::nest(files = c(start_date, path)) |>
                 dplyr::left_join(envRaster::ras_layers |>
                                    dplyr::select(layer, scale, offset)
                 )
  )
  , tar_target(run_time_layer
               , mung_climate(files_df = run_time_layer_df$files
                              , func = run_time_layer_df$func
                              , aoi = extent_sf
                              , scale = run_time_layer_df$scale
                              , offset = run_time_layer_df$offset
                              , out_file = run_time_layer_df$out_file
                              , force_new = TRUE
               )
               , format = "file"
               , pattern = map(run_time_layer_df)
  )
  ## bioclim ------
  , tar_target(bioclim_files_df
               , envRaster::name_env_tif(cube_directory, parse = TRUE) |>
                 dplyr::filter(! grepl("bio", layer)) |>
                 dplyr::left_join(tibble::enframe(run_time_layer, name = "branch", value = "path")) |>
                 dplyr::select(layer, func, start_date, path) |>
                 dplyr::mutate(year = lubridate::year(start_date)) |>
                 tidyr::nest(files = -c(year))
  )
  , tar_target(name = bioclim
               , command = make_bioclim_rasters(files_df = bioclim_files_df$files[[1]]
                                                , out_dir = cube_directory
                                                , force_new = TRUE
                                                )
               , format = "file"
               , pattern = map(bioclim_files_df)
               )
  ## disaggregate  -------
  , tar_target(disagg_df
               , tibble::tibble(path = bioclim)
               )
  , tar_target(disagg_grid_path
               , tar_read(base_grid_path
                          , store = tars$satellite$store
                          )
               )
  , tar_target(name = disaggregated
               , command = disagg_ras(input_ras_path = disagg_df$path
                                      , base_grid_path = disagg_grid_path
                                      , in_res = envFunc::extract_scale("coarse", scales = scales_file)$grain$res_x
                                      , out_dir <- dirname(disagg_grid_path)
                                      , force_new = TRUE
                                      )
               , format = "file"
               , pattern = map(disagg_df)
               )
  )
