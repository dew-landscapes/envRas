
  #

  fs::dir_create(settings$cli_save_dir)
  
  
  library(ncdf4)
  library(lubridate)
  
  base_url <- "https://dapds00.nci.org.au/thredds/dodsC/gh70/ANUClimate/v2-0/stable/month/rain/"
  
  file_generic <- "ANUClimate_v2-0_rain_monthly_"
  file_specific <- "200001.nc"
  
  safe_nc <- function(url) {
    
    res <- purrr::safely(stars::read_ncdf)
    
    if(!is.null(res$result)) {
      
      p <- res$result %>%
        
      
    }
    
  }

  files <- settings$seasons$months %>%
    dplyr::sample_n(1) %>% # TESTING
    dplyr::mutate(file_specific = format(start_date, "%Y%m")
                  , file = paste0(base_url
                                    , year
                                  , "/"
                                    , paste0(file_generic
                                             , file_specific
                                             , ".nc"
                                             )
                                    )
                  , result = purrr::map(file, safe_nc, proxy = TRUE)
                  , nc = purrr::map(result, "result")
                  ) %>%
    dplyr::filter(purrr::map_lgl(nc, ~!is.null(.))) %>%
    tidyr::nest(data = -c(year_use, season)) %>%
    dplyr::rename(year = year_use) %>%
    dplyr::left_join(settings$season$season) %>%
    dplyr::mutate()
  
  