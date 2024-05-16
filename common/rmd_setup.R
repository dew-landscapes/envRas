  
  # genericData-------

  source(fs::path(here::here()
                  , "common"
                  , "genericData.R"
                  )
         )

  # package .bib-------
  
  knitr::write_bib(packages
                   , file = here::here("packages.bib")
                   , tweak = TRUE
                   )
  

  # knitr options-------
  
  knitr::opts_chunk$set(fig.width = 7
                        , fig.height = 4.5
                        , warning = FALSE
                        , message = FALSE
                        , echo = FALSE
                        , dpi = 300
                        )
  
  options(knitr.kable.NA = "-")
  
  
  # scientific notation-------
  
  options(scipen = 999)

  
  # maps-------
  
  tmap_mode("plot")
  

  # ggplot -------
  
  ggplot2::theme_set(ggplot2::theme(panel.grid.major = element_blank()
                                    , panel.grid.minor = element_blank()
                                    , panel.background = element_blank()
                                    , axis.line = element_line(colour = "black")
                                    )
                     )
  
  
  # RCText names-------
  
  rcTextNames <- c("Theme"
                   , "Subtheme"
                   , "Title"
                   , "Indicator"
                   , "Trend"
                   , "Condition"
                   , "Reliability"
                   
                   , "TrendQuote"
                   , "TrendText"
                   
                   , "ConditionQuote"
                   , "ConditionText"
                   
                   , "Rationale"
                   , "Pressure"
                   , "Management"
                   
                   , "Quote"
                   
                   , "IsAre"
                   )
  
  