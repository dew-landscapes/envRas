extract_summarise <- function(extractr_df
                              , groups = "cluster"
                              , qs = c(0.05, 0.25, 0.5, 0.75, 0.95)
                              ) {
  
  extractr_df |>
    tidyr::pivot_longer(cols = tidyselect::any_of(names(extractr_df)[! names(extractr_df) %in% groups])) |>
    envFunc::summarise_long_df(group_cols = c("name", groups))
  
}
