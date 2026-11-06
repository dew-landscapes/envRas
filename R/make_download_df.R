make_download_df <- function(date_df
                             , settings
                             , base_dir
                             , force_new = FALSE
                             , abs_min_year = 1982
                             , filter_done = TRUE
                             ) {
  
  result <- date_df |>
    dplyr::filter(start_date >= as.Date(paste0(abs_min_year, "-01-01"))) |>
    dplyr::cross_join(tibble::tibble(layer = settings$layers)) |>
    dplyr::cross_join(tibble::tibble(func = settings$func)) |>
    dplyr::left_join(envRaster::ras_layers |>
                       dplyr::select(layer, scale, offset)
                     ) |>
    dplyr::mutate(out_file = fs::path(base_dir
                                      , paste0(gsub("nbart_", "", layer), "__", func, "__", start_date, ".tif")
                                      )
                  , done = file.exists(out_file)
                  , force = force_new
                  , todo = ! purrr::map2_lgl(done, force_new, any)
                  )
  
  if(filter_done) {
    
    result <- result |>
      dplyr::filter(todo)
    
  }
  
  if(nrow(result) == 0) {
    
    result <- tibble::tibble(layer = NA
                             , func = NA
                             , start_date = NA
                             , end_date = NA
                             , scale = NA
                             , offset = NA
                             )
    
  }
  
  return(result)
  
}
