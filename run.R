
library(targets)

# packages ---------
envFunc::check_packages(yaml::read_yaml("settings/packages.yaml")$packages
                        , update_env = TRUE
                        )

# tars --------
## local ------
tars_local <- envTargets::make_tars(settings = list(extent = yaml::read_yaml("settings/setup.yaml")$extent
                                                    , grain = yaml::read_yaml("settings/setup.yaml")$grain
                                                    )
                                    , list_names = c("extent", "grain")
                                    , save_yaml = FALSE
                                    )

## external ------
tars <- c(tars_local)

## write tars -------
envTargets::write_tars(tars)

# prune -------
# only if in dev
if(grepl("\\/dev\\/", here::here())) {
  
  purrr::walk2(purrr::map(tars_local, "script")
               , purrr::map(tars_local, "store")
               , \(x, y) if(file.exists(y)) targets::tar_prune(script = x, store = y)
               )
  
}

# make everything ----------
# in _targets.yaml
purrr::walk2(purrr::map(tars_local, "script")
             , purrr::map(tars_local, "store")
             , \(x, y) targets::tar_make(script = x, store = y)
             )

if(FALSE) {
  
  # individual tar_make-------
  
  script <- "setup"
  
  tar_visnetwork(script = tars_local[[script]]$script
                 , store = tars_local[[script]]$store
                 , label = "time"
                 )
  
  # tar_invalidate(report, store = tars_local[[script]]$store)
  
  tar_make(script = tars_local[[script]]$script
           , store = tars_local[[script]]$store
           )
  
  tar_prune(script = tars_local[[script]]$script
           , store = tars_local[[script]]$store
           )
  
  tar_meta(fields = any_of("error"), complete_only = TRUE, store = tars_local[[script]]$store)
  tar_meta(fields = any_of("warnings"), complete_only = TRUE, store = tars_local[[script]]$store)
  
}
