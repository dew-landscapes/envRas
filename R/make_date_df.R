make_date_df <- function(min_date
                         , max_date
                         , grain_time
                         , run_time
                         , make_cube = TRUE
                         ) {
  
  n_years <- lubridate::year(max_date) - lubridate::year(min_date)
  
  starts <- lubridate::as_date(min_date) + lubridate::years(0:(n_years + 1))
  starts <- starts[starts <= lubridate::as_date(max_date) - lubridate::period(run_time) + 1]
  
  ends <- rev(lubridate::as_date(max_date) - lubridate::years(0:n_years))
  ends <- ends[ends > (lubridate::as_date(min_date) + (lubridate::period(run_time) - lubridate::period(grain_time)))]
  
  result <- tibble::tibble(start_date = starts, end_date = ends)
  
  if(!make_cube) {
    
    result <- result |>
      dplyr::filter(start_date == max(start_date))
    
  }
  
  return(result)
  
}
