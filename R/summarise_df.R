summarise_env <- function(wide_df
                          , qs = c(0.05, 0.25, 0.5, 0.75, 0.95)
                          , cols = names(wide_df)
                          ) {
  
  wide_df |>
    dplyr::bind_rows() |>
    tidyr::pivot_longer(cols = tidyselect::any_of(cols)) |>
    dplyr::summarise(n = dplyr::n()
                     , NAs = sum(is.na(value))
                     , propNA = NAs / n
                     , mean = mean(value, na.rm = TRUE)
                     , sd = sd(value, na.rm = TRUE)
                     , qs = envFunc::quibble(value
                                             , na.rm = TRUE
                                             , q = qs
                                             )
                     , max = max(value, na.rm = TRUE)
                     , min = min(value, na.rm = TRUE)
                     , .by = name
                     ) |>
    tidyr::unnest(cols = c(qs))
  
}
 