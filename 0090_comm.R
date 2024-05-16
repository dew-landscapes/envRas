

  # html report
  if(make_book) {
    
    del <- here::here("report", "_book")
    if(dir.exists(del)) fs::dir_delete(del)
    
    del <- here::here("report", "envRas.Rmd")
    if(file.exists(del)) fs::file_delete(del)
    
    xfun::in_dir(here::here("report")
                 , bookdown::render_book(here::here("report", "index.Rmd"))
                 )
    
    # move files to out_report
    purrr::walk2(here::here("report", "_book")
                 , settings$out_report
                 , fs::dir_copy
                 , overwrite = TRUE
                 )
    
    # delete packages.bib(s)
    bibs <- fs::dir_ls(here::here()
                       , regexp = "packages.bib"
                       , recurse = TRUE
                       )
    
    fs::file_delete(bibs)
    
  }

  if(FALSE) {
      
      # development
      
      xfun::in_dir(here::here("report")
                 , bookdown::render_book(here::here("report", "0010_introduction.Rmd")
                                         , preview = T
                                         )
                 )
      
    }
  