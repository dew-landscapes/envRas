

  old_files <- fs::dir_ls(sat_seasonal_cube_dir) %>%
    tibble::enframe(name = NULL, value = "path") %>%
    parse_env_tif() %>%
    tidyr::separate(period, into = c("period", "res")
                    , sep = "__"
                    ) %>%
    dplyr::mutate(start_date = as.Date(start_date)) %>%
    dplyr::left_join(settings$seasons$seasons) %>%
    dplyr::rename(vector = layer
                  , level = aoi
                  , layer = band
                  ) %>%
    dplyr::mutate(filt_col = "") %>%
    name_env_tif() %>%
    dplyr::select(path, out_file) %>%
    dplyr::mutate(out_file = fs::path("I:", out_file))
    
  fs::dir_create(dirname(old_files$out_file[[1]]))
  
  fs::file_move(old_files$path
                , old_files$out_file
                )
  