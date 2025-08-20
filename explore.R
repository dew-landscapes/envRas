
library(targets)
library(sf) # some list items are sf and the list won't open properly if sf package isn't in global env
library(tmap)

tmap_mode("view")
tmap_options(basemap.server = c(OSM = "OpenTopoMap"
                          , Imagery = "Esri.WorldImagery"
                          )
             )

# tars -------
tars <- yaml::read_yaml("_targets.yaml")


if(FALSE) {
  
  # individual tar_make-------
  
  script <- "setup"
  
  tar_visnetwork(script = tars[[script]]$script
                 , store = tars[[script]]$store
                 , label = "time"
                 )
  
  tar_poll(store = tars[[script]]$store)
  
  tar_meta(fields = any_of("error"), complete_only = TRUE, store = tars[[script]]$store)
  tar_meta(fields = any_of("warnings"), complete_only = TRUE, store = tars[[script]]$store)
  
}