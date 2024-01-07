
  library(magrittr)
  
  # settings------
  
  settings <- list(epsg_proj = 7845
                   , use_res = 30
                   , sample_n = 9999
                   , epsg_latlong = 4283 # for decimal degrees
                   )
  
  settings$sat_source <- "DEA"
  settings$sat_collection <- c("ga_ls8c_ard_3", "ga_ls9c_ard_3") # don't include sentinel
  
  settings$cli_source <- "NCI"
  settings$cli_collection <- "ANUClimate"
  
  settings$use_period <- "P3M"
  
  settings$layer <- "sa_ibrasub_xn"
  settings$filt_col <- NULL
  settings$use_aoi <- NULL
  settings$use_bbox <- FALSE
  settings$use_buffer <- 0
  settings$use_clip <- NULL
  settings$use_clip_buffer <- 0
  
  settings$start_year <- 2014
  settings$end_year <- 2023
  
  
  #----------RUN---------
  
  if(!exists("run_from")) run_from <- 0
  if(!exists("run_to")) run_to <- 50
  
  max_cores <- 20
  
  skips <- "sat"
  
  dir() %>%
    grep("^\\d{4}_.*\\.R$",.,value=TRUE) %>%
    setNames(stringr::str_extract(.,"\\d{4}")) %>%
    `[` (names(.)[as.numeric(names(.)) <= run_to & as.numeric(names(.)) >= if(run_from == 0) 1 else run_from]) %>%
    {if(exists("skips")) (.) %>% `[` (!grepl(skips, .)) else (.) } %>%
    purrr::walk(source
                , verbose = TRUE
                )
  
