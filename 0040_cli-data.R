
  # base grid------
  # base grid using settings$aoi and settings$boundary
  
  settings[["aoi"]] <- terra::rast(ext(settings[["base", exact = TRUE]])
                                   , crs = terra::crs(settings[["base", exact = TRUE]])
                                   ) %>%
    terra::project(y = paste0("epsg:", settings$epsg_latlong)) %>%
    stars::st_as_stars()
  
  
  # save-------
  
  rio::export(settings
              , fs::path(settings[["munged_dir", exact = TRUE]]
                         , "settings.rds"
                         )
              )
  
  
  # Climate-------

  fs::dir_create(settings[["cli_cube_dir", exact = TRUE]])
  
  
  library(ncdf4)
  library(lubridate)
  library(stars)
  
  base_url <- "https://dapds00.nci.org.au/thredds/dodsC/gh70/ANUClimate/v2-0/stable/month"
  
  get_layers <- tibble::tribble(
    ~layer, ~func
    ,"rain", "mean"
    , "evap", "mean"
    #, "srad", "mean"
    , "tavg", "mean"
    , "vpd", "mean"
    , "tmin", "min"
    , "tmax", "max"
    )
  
  safe_nc <- purrr::safely(stars::read_ncdf)
  
  make_cli_data <- function(urls_df, out_file, func, base) {
    
    r <- purrr::map(urls_df$file
                    , safe_nc
                    , proxy = TRUE
                    ) %>%
      purrr::map("result") %>%
      purrr::discard(is.null)
    
    if(length(r)) {
    
      r %>%
        purrr::map(sf::st_set_crs, settings$epsg_latlong) %>%
        purrr::map(`[`
                   , i = base
                   ) %>%
        purrr::map(stars::st_as_stars
                   , proxy = FALSE
                   ) %>%
        do.call("c", .) %>%
        aggregate(by = "3 months"
                  , FUN = get(func)
                  , na.rm = TRUE
                  ) %>%
        stars::write_stars(out_file)
      
    }
    
    return(invisible(NULL))
    
  }

  files <- settings[["seasons", exact = TRUE]]$months %>%
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
    dplyr::left_join(settings[["seasons", exact = TRUE]]$season) %>%
    #dplyr::sample_n(1) %>% # TESTING
    dplyr::mutate(out_file = fs::path(settings[["cli_cube_dir", exact = TRUE]]
                                      , paste0(layer, "__", start_date,".tif")
                                      )
                  , done = file.exists(out_file)
                  )
  
  
  purrr::pwalk(list(files$data[!files$done]
                    , files$out_file[!files$done]
                    , files$func[!files$done]
                    )
               , make_cli_data
               , base = settings$aoi
               )
  
  
  # results-------
  
  results <- fs::dir_info(settings[["cli_cube_dir", exact = TRUE]]
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
  
  
  if(FALSE) {
    
    results %>%
      dplyr::sample_n(9) %>%
      dplyr::pull(path) %>%
      terra::rast() %>%
      plot()
    
  }
  