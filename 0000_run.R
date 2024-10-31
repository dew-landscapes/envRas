
  settings <- list()
  
  max_cores <- 32
  
  # context
  settings$polygons <- "sa_ibrasub_xn"
  settings$filt_col <- NULL
  settings$level <- NULL
  settings$buffer <- 0

  # context extra (not in names)  
  settings$use_bbox <- FALSE
  settings$use_clip <- "aus_500m_buf"
  settings$use_clip_buffer <- 0

  # cube
  settings$sat_res <- 90
  settings$max_year <- as.numeric(format(Sys.time(), "%Y")) - 1
  settings$min_year <- settings$max_year - 50
  settings$months <- 1:12
  
  ## periods
  settings$period <- "P1M" # Just don't change this! It is not really a setting. Too much relies on 'month'. Also, need 'period' for name_env_tif
  settings$epoch_period <- "P60M" # Predict period and used to attribute env data to observation dates/locations
  
  
  # comms settings-------
  make_book <- settings$level == "KI" # only make report on a small area!
  do_sat_summary <- TRUE
  do_cli_summary <- TRUE
  
  
  #----------RUN---------
  
  run_from <- 0
  run_to <- 20
  skips <- c("static", 5000, "90m")
  
  envFunc::run(run_from, run_to, skips)
  
