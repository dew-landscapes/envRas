
library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_date_df.R"
             , "R/download_nc_raw.R"
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
                                 , force_new = FALSE
                                 )
               , pattern = map(raw_layer_df)
               , format = "file"
               )
  ### base grid -------
  # Not sure this is necessary (could use extent_sf in bbox instead) but will invalidate the downloads if it is removed
  , tar_target(base_grid_path
               , envRaster::make_base_grid(extent_sf
                                           , out_res = envFunc::extract_scale("coarse", scales = scales_file)$grain$res_x
                                           , out_epsg = settings$crs$proj
                                           , use_mask = extent_sf
                                           , out_file = fs::path(dirname(cube_directory), "base.tif")
                                           , overwrite = TRUE
                                           , ret = "path"
                                           , datatype = "INT1U"
                                           ) |>
                 as.character()
               )
  ### bbox -------
  , tar_target(bbox
               , sf::st_bbox(terra::rast(base_grid_path)) |>
                 sf::st_as_sfc() |>
                 sf::st_transform(crs = settings$crs$decdeg) |> # need decimal lat/long for rstac
                 sf::st_bbox()
               )
  # ## download cube ---------
  # ### epoch df -------
  # , tar_target(download_files_df
  #              , date_df |>
  #                dplyr::mutate(year = purrr::map2(start_date, end_date, \(x, y) lubridate::year(x):lubridate::year(y))) |>
  #                tidyr::unnest(col = c(year)) |>
  #                dplyr::cross_join(tibble::tibble(month = stringr::str_pad(1:12, 2, pad = 0))) |>
  #                dplyr::cross_join(tibble::tibble(layer = settings_climate$layers)) |>
  #                dplyr::mutate(func = dplyr::case_when(grepl("min", layer) ~ "min"
  #                                                      , grepl("max", layer) ~ "max"
  #                                                      , TRUE ~ "mean"
  #                                                      )
  #                              , remote_file = paste0(settings_climate$source_url
  #                                                     , "/"
  #                                                     , layer
  #                                                     , "/"
  #                                                     , lubridate::year(start_date)
  #                                                     , "/"
  #                                                     , paste0("ANUClimate_v2-0_"
  #                                                              , layer
  #                                                              , "_monthly_"
  #                                                              , lubridate::year(start_date)
  #                                                              , month
  #                                                              , ".nc"
  #                                                              )
  #                                                     )
  #                              ) |>
  #                tidyr::nest(remote_files = c(year, remote_file)) |>
  #                dplyr::mutate(start_date = lubridate::as_date(paste0(lubridate::year(start_date)
  #                                                                     , "-"
  #                                                                     , month
  #                                                                     , "-01"
  #                                                                     )
  #                                                              )
  #                              , out_file = fs::path(cube_directory
  #                                                    , paste0(layer
  #                                                             , "__"
  #                                                             , func
  #                                                             , "__"
  #                                                             , start_date
  #                                                             , ".tif"
  #                                                             )
  #                                                    )
  #                              )
  #              )
  # ### download ------
  # , tar_target(name = nc_download
  #             , command = download_nc(save_file = download_files_df$out_file
  #                                     , remote_files = download_files_df$remote_files[[1]]$remote_file
  #                                     , bbox = bbox
  #                                     , func = download_files_df$func
  #                                     , force_new = FALSE
  #                                     , base_grid_path = base_grid_path
  #                                     )
  #             , pattern = map(download_files_df)
  #             , format = "file"
  #             # This partially ran in parallel (maybe 2-3 out of 6 layers returned before error)
  #             # but would usually fail in parallel with: error in `RNetCDF::open.nc()`: ! NetCDF: Write to read only
  #             )
  # ## bioclim ------
  # , tar_target(bioclim_files_df
  #              , tibble::tibble(path = nc_download) |>
  #                name_env_tif(parse = TRUE) |>
  #                tidyr::nest(files = c(start_date, name, path))
  #              )
  # , tar_target(name = bioclim
  #              , command = make_bioclim_rasters(files_df = bioclim_files_df
  #                                               , out_dir = cube_directory
  #                                               , start_date = min_date
  #                                               )
  #              , format = "file"
  #              )
  # ## disaggregate  -------
  # , tar_target(disagg_df
  #              , tibble::tibble(path = bioclim)
  #              )
  # , tar_target(disagg_grid_path
  #              , tar_read(base_grid_path
  #                         , store = tars$satellite$store
  #                         )
  #              )
  # , tar_target(name = disaggregated
  #              , command = disagg_ras(input_ras_path = disagg_df$path
  #                                     , base_grid_path = disagg_grid_path
  #                                     , in_res = envFunc::extract_scale("coarse", scales = scales_file)$grain$res
  #                                     , out_res = settings$grain$res
  #                                     , force_new = FALSE
  #                                     )
  #              , format = "file"
  #              , pattern = map(disagg_df)
  #              )
  )
