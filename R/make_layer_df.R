make_layer_df <- function(layers
                          , min_date
                          , max_date
                          , items
                          , period
                          ) {
  
  n_features <- length(items$features)
  
  result <- if(n_features > 1) {
  
    n_years <- lubridate::year(max_date) - lubridate::year(min_date)
    
    starts <- lubridate::as_date(min_date) + lubridate::years(0:(n_years + 1))
    starts <- starts[starts <= lubridate::as_date(max_date) - (lubridate::period(period)- lubridate::period("P1Y"))]
    
    ends <- rev(lubridate::as_date(max_date) - lubridate::years(0:n_years))
    ends <- ends[ends > (lubridate::as_date(min_date) + (lubridate::period(period) - lubridate::period("P1Y")))]
    
    tibble::tibble(layer = layers) |>
      dplyr::cross_join(tibble::tibble(start_date = starts, end_date = ends))
    
  } else {
    
    use_start_date <- lubridate::as_date(items$features[[1]]$properties$datetime)
    use_end_date <- lubridate::as_date(items$features[[1]]$properties$datetime) + lubridate::period("P1D")
    
    tibble::tibble(layer = layers) |>
      dplyr::cross_join(tibble::tibble(start_date = use_start_date, end_date = use_end_date))
    
  }
  
  return(result)
  
}
