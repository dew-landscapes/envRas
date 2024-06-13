
  settings <- list()
  
  # context
  settings$vector <- "lsa"
  settings$filt_col <- "LSA"
  settings$level <- "KI"
  settings$buffer <- 2500

  # context extra (not in)  
  settings$use_bbox <- FALSE
  settings$use_clip <- "aus_500m_buf"
  settings$use_clip_buffer <- 0

  
  # cube
  settings$period <- "P3M"
  #settings$use_res <- 30 # now set as settings$sat_res etc.
  
  # comms settings-------
  make_book <- settings$level == "KI" # only make report on a small area!
  do_sat_summary <- TRUE
  do_cli_summary <- TRUE
  
  
  #----------RUN---------
  
  run_from <- 0
  run_to <- 90
  skips <- c("static", 5000, "90m")
  
  max_cores <- 2
  
  envFunc::run(run_from, run_to, skips)
  
