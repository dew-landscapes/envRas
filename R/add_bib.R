  
  add_bib <- function(DOI
                      , bib_file
                      ) {
      
      new_entry <- RefManageR::GetBibEntryWithDOI(DOI) |>
        RefManageR::toBiblatex()
      
      new_entry[1] <- paste0("\n@Misc{RN", envReport::next_bib_no(bib_file), ",")
      
      readr::write_lines(new_entry
                         , file = bib_file
                         , append = TRUE
                         )
      
  }
  