
  library(magrittr)

  # Assumes that IST\SRC\MonSurv\MERF\DocWorksp\RC is mapped to F:
  
  
  # function to run a report within its project directory
  
  write_report <- function(rc_id
                           , rc_server_base = "F:/"
                           , rc_local_base = "D:/env/projects/RC/2023"
                           ) {
    
    rc_dir <- fs::path(rc_local_base, rc_id)
    
    # Always bring in new `common` data
    
    if(file.exists(rc_server_base)) {
      
      if(file.exists(fs::path(rc_dir, "common"))) {
        
        fs::dir_delete(fs::path(rc_dir, "common"))
        
      }
      
      fs::dir_copy(fs::path(rc_server_base, "RC_common")
                   , fs::path(rc_dir, "common")
                   , overwrite = TRUE
                   )
      
    }
    
    # Clean up previous knits
    
    fs::file_delete(fs::dir_ls(path = rc_dir
                              , regexp = "_book|_main"
                              )
                   )
    
    fs::dir_delete(fs::dir_ls(path = rc_dir
                              , regexp = "_book|_main"
                              )
                   )
    
    
    # Render report from within its home directory
    
    xfun::in_dir(rc_dir
                 , bookdown::render_book(fs::path(rc_dir, "Report.Rmd")
                                         , params = list(dir = rc_dir)
                                         )
                 )
    
    
    # Copy report to 'out'
    
    fs::file_copy(fs::path(rc_dir, "_book", "_main.docx")
                  , fs::path(rc_dir, "out", paste0(basename(rc_dir), "_tech_base.docx"))
                  , overwrite = TRUE
                  )
    
  }
  
  
  
  # run all reports

  reports <- fs::dir_ls(rc_dir_local
                        , recurse = TRUE
                        , regexp = "Report.Rmd"
                        ) %>%
    tibble::enframe(name = NULL
                    , value = "path"
                    )

  
  