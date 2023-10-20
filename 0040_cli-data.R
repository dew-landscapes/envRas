
  #

  fs::dir_create(settings$cli_save_dir)
  
  
  library(ncdf4)
  library(lubridate)
  library(stars)
  
  base_url <- "https://dapds00.nci.org.au/thredds/dodsC/gh70/ANUClimate/v2-0/stable/month"
  
  get_layers <- tibble::tibble(
    layer = c("rain", "evap", "srad", "tavg", "vpd")
    ) %>%
    dplyr::mutate(func = list(mean))
  
  safe_nc <- purrr::safely(stars::read_ncdf)
  
  make_cli_data <- function(urls_df, out_file, func) {
    
    r <- purrr::map(urls_df$file
               , safe_nc
               , proxy = TRUE
               ) %>%
      purrr::map("result") %>%
      purrr::discard(is.null)
    
    if(length(r) > 0) {
    
      r %>%
        purrr::map(sf::st_set_crs, 4283) %>%
        purrr::map(., ~ .[sf::st_bbox(settings$boundary %>%
                                        sf::st_transform(crs = 4283)
                                      )
                          ]
                   ) %>%
        purrr::map(stars::st_as_stars
                   , proxy = FALSE
                   ) %>%
        do.call("c", .) %>%
        aggregate(by = "3 months", FUN = func) %>%
        stars::write_stars(out_file)
      
    }
    
    return(invisible(NULL))
    
  }

  files <- settings$seasons$months %>%
    dplyr::cross_join(get_layers) %>%
    dplyr::mutate(file_specific = format(start_date, "%Y%m")
                  , file = paste0(base_url
                                  , "/"
                                  , layer
                                  , "/"
                                  , year
                                  , "/"
                                  , paste0("ANUClimate_v2-0_"
                                           , layer
                                           , "_monthly_"
                                           , file_specific
                                           , ".nc"
                                           )
                                  )
                  ) %>%
    dplyr::select(year = year_use, season, layer, func, file) %>%
    tidyr::nest(data = -c(year, season, layer, func)) %>%
    dplyr::left_join(settings$seasons$season) %>%
    #dplyr::sample_n(1) %>% # TESTING
    dplyr::mutate(out_file = fs::path(settings$cli_save_dir
                                      , paste0(layer, "__", start_date,".tif")
                                      )
                  , done = file.exists(out_file)
                  )
  
  
  purrr::pwalk(list(files$data[!files$done]
                    , files$out_file[!files$done]
                    , files$func[!files$done]
                    )
               , make_cli_data
               )
  
  
  # results-------
  
  results <- fs::dir_info(settings$cli_save_dir
                          , regexp = "tif$"
                          ) %>%
    dplyr::arrange(desc(modification_time)) %>%
    dplyr::mutate(name = gsub("\\.tif", "", basename(path))
                  , name2 = basename(dirname(path))
                  ) %>%
    dplyr::select(path, name, name2) %>%
    tidyr::separate(name, into = c("layer", "start_date"), sep = "__") %>%
    tidyr::separate(name2,into = c("source", "collection", "aoi", "buffer", "res")
                    , sep = "__"
                    ) %>%
    dplyr::mutate(start_date = as.Date(start_date))
  
  