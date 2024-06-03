
  settings <- list()
  
  # context
  settings$vector <- "sa_ibrasub_xn"
  settings$filt_col <- NULL
  settings$level <- NULL
  settings$buffer <- 0

  # context extra (not in)  
  settings$use_bbox <- FALSE
  settings$use_clip <- "aus_500m_buf"
  settings$use_clip_buffer <- 0

  
  # cube
  settings$period <- "P3M"
  #settings$use_res <- 30 # now set as settings$sat_res etc.
  
  # comms settings-------
  make_book <- settings$level == "Bakara" # only make report on a small area!
  do_sat_summary <- TRUE
  
  
  #----------RUN---------
  
  run_from <- 0
  run_to <- 90
  skips <- NULL
  
  max_cores <- 10
  
  envFunc::run(run_from, run_to, skips)
  
