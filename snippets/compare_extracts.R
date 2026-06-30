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

# source -----
tar_source("R/extract_summarise.R")

# objects ------
tar_load(settings, store = tars$setup$store)
tar_load(c(random_env, env_df), store = tars$random$store)

ibra <- sfarrow::st_read_parquet(fs::path(settings$data_dir, "vector", "sa_ibrasub.parquet"))

use_aoi <- ibra |>
  dplyr::filter(IBRA_SUB_CODE == "MDD02")

# random intersect and summarise
rand_start <- Sys.time()

rand_sum <- random_env |>
  dplyr::filter(!is.na(cell_lat), !is.na(cell_long)) |>
  envClean::filter_geo_range(use_aoi = use_aoi
                             , x = "cell_long"
                             , y = "cell_lat"
                             ) |>
  tidyr::pivot_longer(cols = tidyselect::any_of(env_df$name)) |>
  envFunc::summarise_long_df(group_cols = c("IBRA_REG_NAME", "IBRA_REG_CODE", "IBRA_SUB_NAME", "IBRA_SUB_CODE"))

rand_time <- difftime(Sys.time(), rand_start)

# exactextractr
extractr_start <- Sys.time()

exactextractr::exact_extract(x = envRaster::make_env_stack(env_df$path)
                             , y = use_aoi
                             , \(df) extract_summarise(df)
                             , summarize_df = T
                             , append_cols = c("IBRA_REG_NAME", "IBRA_REG_CODE", "IBRA_SUB_NAME", "IBRA_SUB_CODE")
                             )
  
extractr_time <- difftime(Sys.time(), extractr_start)

print(rand_time)
print(extractr_time)