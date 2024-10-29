
  settings <- list()
  
  max_cores <- 32
  
  # context
  settings$polygons <- "sa_ibrasub_xn"
  settings$filt_col <- NULL
  settings$level <- NULL
  settings$buffer <- 0

  # context extra (not in)  
  settings$use_bbox <- FALSE
  settings$use_clip <- "aus_500m_buf"
  settings$use_clip_buffer <- 0

  # periods
  settings$min_period <- "P1M"
  settings$epoch_period <- "P60M" # 5 years
  
  # cube
  settings$sat_res <- 90
  settings$max_year <- as.numeric(format(Sys.time(), "%Y"))
  settings$min_year <- settings$max_year - 50
  settings$months <- 1:12
  
  # period
  settings$period <- "P1M"
  settings$epoch_period <- "P60M"
  # i.e. with min_period = "P1M" and obs_period = "P12M" gives 12 temporal cells (months) per observation period
  
  
  # comms settings-------
  make_book <- settings$level == "KI" # only make report on a small area!
  do_sat_summary <- TRUE
  do_cli_summary <- TRUE
  
  
  #----------RUN---------
  
  run_from <- 0
  run_to <- 20
  skips <- c("static", 5000, "90m")
  
  envFunc::run(run_from, run_to, skips)
  
