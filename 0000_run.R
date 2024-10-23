
  settings <- list()
  
  max_cores <- 32
  
  # context
  settings$polygons <- "parks"
  settings$filt_col <- "RESNAME"
  settings$level <- "Bakara"
  settings$buffer <- 2500

  # context extra (not in)  
  settings$use_bbox <- FALSE
  settings$use_clip <- "aus_500m_buf"
  settings$use_clip_buffer <- 0

  # epochs
  settings$epoch_period <- 5
  
  # cube
  settings$period <- "P1M"
  settings$sat_res <- 90
  settings$max_year <- as.numeric(format(Sys.time(), "%Y"))
  settings$min_year <- settings$max_year - 50
  settings$months <- 1:12
  
  
  
  # comms settings-------
  make_book <- settings$level == "KI" # only make report on a small area!
  do_sat_summary <- TRUE
  do_cli_summary <- TRUE
  
  
  #----------RUN---------
  
  run_from <- 0
  run_to <- 90
  skips <- c("static", 5000, "90m")
  
  envFunc::run(run_from, run_to, skips)
  
