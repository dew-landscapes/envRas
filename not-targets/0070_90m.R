
if(FALSE) {

  env_30m_data <- fs::dir_ls(settings$munged_dir) %>%
    tibble::enframe(name = NULL, value = "path") %>%
    dplyr::filter(grepl("tif$", path)
                  , !grepl("base.tif$", path)
                  ) %>%
    parse_env_tif(cube = FALSE) %>%
    dplyr::mutate(name = paste0(season, "__", band))
    
  env_90m_data <- env_30m_data %>%
    dplyr::mutate(old_path = path
                  , new_path = gsub("30", "90", path)
                  , done = file.exists(new_path)
                  )
  
  agg_func <- function(path_in, path_out, ...) {
    
    r <- terra::rast(path_in)
    
    terra::aggregate(x = r
                     , ...
                     , filename = path_out
                     )
    
    rm(list = ls())
    
    gc()
    
  }
  
  purrr::map2(env_90m_data$old_path[!env_90m_data$done]
              , env_90m_data$new_path[!env_90m_data$done]
              , agg_func
              , fact = 3
              , fun = "mean"
              , na.rm = TRUE
              )
  
}
  