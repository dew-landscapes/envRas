

  # mung ----------
  
  out_names <- c("source"
                 , "collection"
                 , "res"
                 , "period"
                 , "epoch"
                 , "season"
                 , "layer"
                 )

  epoch_seasons <- envFunc::make_epochs(start_year = settings$start_year
                                        , end_year = settings$end_year
                                        , epoch_step = 10
                                        , epoch_overlap = FALSE
                                        ) %>%
    tidyr::unnest(cols = c(years)) %>%
    dplyr::rename(year = years) %>%
    dplyr::select(year, epoch) %>%
    dplyr::left_join(settings$seasons$seasons) %>%
    dplyr::left_join(results) %>%
    dplyr::filter(!is.na(path)) %>%
    tidyr::nest(data = -c(source, collection, aoi, buffer, res, layer, epoch, season)) %>%
    tidyr::unite(col = "out_file", tidyselect::any_of(out_names), sep = "__", remove = FALSE) %>%
    dplyr::mutate(stack = purrr::map(data
                                     , ~ terra::rast(.$path)
                                     )
                  , out_file = fs::path(settings$munged_dir
                                        , paste0(out_file, ".tif")
                                        )
                  , done = file.exists(out_file)
                  )
  
  qs <- c(0.05, 0.50, 0.95)
  
  fs::dir_create(settings$munged_dir)
  
  align_func <- function(stack, out_file, base, ...) {
    
    reproj <- stack %>%
      terra::app(fun = quantile
                 , ...
                 # , na.rm = TRUE
                 # , probs = qs
                 ) %>%
      terra::project(terra::crs(base))
    
    ratio <- terra::res(reproj) / terra::res(base)
    
    reproj %>%
      terra::disagg(ratio) %>%
      terra::project(base) %>%
      terra::writeRaster(out_file
                         , overwrite = FALSE
                         )
    
    return(invisible(NULL))
    
  }
  
  purrr::walk2(epoch_seasons$stack[!epoch_seasons$done]
               , epoch_seasons$out_file[!epoch_seasons$done]
               , align_func
               , base = settings$base_grid
               , na.rm = TRUE
               , probs = qs
               )
  