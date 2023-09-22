  
  
  # packages------
  
  packages <- 
    sort(
      unique(
        c("base"
          
          # tidyverse
          , "dplyr"
          , "tidyr"
          , "purrr"
          , "ggplot2"
          , "tibble"
          , "readr"
          , "forcats"
          , "stringr"
          , "lubridate"
          
          # misc
          , "fs"
          , "ggridges"
          
          # gis
          , "rstac"
          , "gdalcubes"
          , "terra"
          , "sf"
          , "tmap"
          , "tidyterra"
          
          # env
          , "envRaster"
          , "envFunc"
        )
      )
    )
  
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(new_packages)) install.packages(new_packages)
  
  purrr::walk(packages
              , library
              , character.only = TRUE
              )
  
  
  # functions------
  
  purrr::walk(fs::dir_ls("function")
              , source
              )
  
  
  # options-------
  
  gdalcubes_options(parallel = 4) 
  
  tmap_mode("view")
  
  
  # settings------
  
  settings <- list(use_epsg = 7845
                   , target_res = 10
                   , sample_n = 9999
                   )
  
  settings$use_source <- "DEA"
  
  settings$use_collection <- "ga_ls8c_ard_3"
  
  settings$layer <- "parks"
  
  settings$filt_col <- "RESNAME"
  
  settings$use_aoi <- "Bakara"
  
  settings$use_bbox <- TRUE
  
  settings$use_buffer <- 5000
  
  settings$use_res <- 30
  
  settings$start_year <- 2015
  settings$end_year <- 2022
  
  settings$seasons <- make_seasons(settings$start_year
                                   , settings$end_year
                                   )
  
  settings$use_cores <- 4
  
  
  # directories------
  
  data_dir <- fs::path("D:"
                       , "env"
                       , "data"
                       )
  
  ras_save_dir <- fs::path("out"
                           , paste(settings$use_source
                                   , settings$use_collection
                                   , settings$use_aoi
                                   , settings$use_buffer
                                   , settings$use_res
                                   , sep = "__"
                                   )
                           )
  
  fs::dir_create(ras_save_dir)
  
  
  # boundary-------
  
  settings$boundary <- make_aoi(layer = sfarrow::st_read_parquet(fs::path(data_dir
                                                                          , "vector"
                                                                          , paste0(settings$layer
                                                                                   , ".parquet"
                                                                                   )
                                                                          )
                                                                 )
                                , filt_col = settings$filt_col
                                , level = settings$use_aoi
                                , buffer = settings$use_buffer
                                , bbox = TRUE
                                )
  
  if(FALSE) tm_shape(settings$boundary) + tm_polygons()
  
  
  # cubes-------
  
  cubes <- settings$seasons$months %>%
    dplyr::group_by(year_use, season) %>%
    dplyr::summarise(start_date = min(start_date)
                     , end_date = max(end_date)
                     ) %>%
    dplyr::ungroup() %>%
    #dplyr::slice(3) %>% # TESTING
    dplyr::mutate(stack = purrr::map2(start_date
                                      , end_date
                                      , make_sat_data
                                      , settings = settings
                                      , force_new = FALSE
                                      )
                  )
  
  
  # make satellite data-------
  
  make_sat_data <- function(start_date
                            , end_date
                            , settings
                            , force_new = FALSE
                            , get_bands = c("blue", "red", "green"
                                            , "swir_1", "swir_2", "coastal_aerosol"
                                            , "nir", "oa_fmask"
                                            )
                            , indices = list(gdvi = c("green", "nir")
                                             , ndvi = c("nir", "red")
                                             , nbr = c("nir", "swir_1")
                                             , nbr2 = c("nir", "swir_2")
                                             )
                            ) {
  
    
    # find images--------
    items <- rstac::stac("https://explorer.sandbox.dea.ga.gov.au/stac") %>%
      rstac::stac_search(collections = settings$use_collection
                         , bbox = sf::st_bbox(settings$boundary)
                         , datetime = paste0(start_date
                                             , "/"
                                             , end_date
                                             )
                         , limit = 1000
                         ) %>%
      rstac::get_request()
  
    get_bands <- items %>%
      rstac::items_assets() %>%
      grep(paste0(get_bands, collapse = "|"), ., value = TRUE)
    
    col <- gdalcubes::stac_image_collection(items$features
                                            , asset_names = get_bands
                                            , property_filter = function(x) {x[["eo:cloud_cover"]] < 30}
                                            )
  
    # bbox-------
    bbox <- settings$boundary %>%
      sf::st_transform(crs = settings$use_epsg) %>%
      sf::st_bbox()
    
    # extent-------
    use_extent <- list(left = bbox["xmin"][[1]]
                       , right = bbox["xmax"][[1]]
                       , top = bbox["ymax"][[1]]
                       , bottom = bbox["ymin"][[1]]
                       , t0 = as.character(start_date)
                       , t1 = as.character(end_date)
                       #, srs = paste0("EPSG:", settings$use_epsg)
                       )
    
    # cube setup------
    v <- gdalcubes::cube_view(srs = paste0("EPSG:"
                                           , settings$use_epsg
                                           )
                              , extent = use_extent
                              , dx = 30 #ceiling(abs(use_extent$left - use_extent$right) / 30)
                              , dy = 30 #ceiling(abs(use_extent$top - use_extent$bottom) / 30)
                              , dt = "P3M"
                              , aggregation = "median"
                              , resampling = "bilinear"
                              )
    
    # cloud mask -------
    cloud_mask <- gdalcubes::image_mask("oa_fmask", values=c(2, 3)) # clouds and cloud shadows
  
    # process bands------
    purrr::walk(head(get_bands, -1)
                , function(x) {
                  
                  out_file <- fs::path(ras_save_dir
                                       , paste0(gsub("nbart_", "", x), "__", start_date, ".tif")
                                       )
                  
                  run <- if(!file.exists(out_file)) TRUE else force_new
                  
                  if(run) {
                  
                    r <- gdalcubes::raster_cube(col
                                                , v
                                                , mask = cloud_mask
                                                ) %>%
                      gdalcubes::select_bands(x) %>%
                      gdalcubes::reduce_time(names = x
                                             , FUN = function(a) {
                                               
                                               median(a, na.rm = TRUE)
                                               
                                             }
                                             )
                    
                    gdalcubes::write_tif(r
                                         , dir = ras_save_dir
                                         , prefix = paste0(gsub("nbart_", "", x), "__")
                                         )
                    
                    }
                    
                  }
                
                )
    
    # process indices-------
    purrr::iwalk(indices
                 , ~ {
                  
                   print(.y)
                   
                   out_file <- fs::path(ras_save_dir
                                        , paste0(.y, "__", start_date, ".tif")
                                        )
                  
                  
                   run <- if(!file.exists(out_file)) TRUE else force_new
                   
                   if(run) {
                     
                     .x[[1]] <- grep(.x[[1]], get_bands, value = TRUE)
                     .x[[2]] <- grep(.x[[2]], get_bands, value = TRUE)
                     
                     r <- gdalcubes::raster_cube(col
                                                 , v
                                                 , mask = cloud_mask
                                                 ) %>%
                       gdalcubes::select_bands(c(.x[[1]], .x[[2]])) %>%
                       apply_pixel(paste0("("
                                          , .x[[1]]
                                          , "-"
                                          , .x[[2]]
                                          , ")/("
                                          , .x[[1]]
                                          , "+"
                                          , .x[[2]]
                                          , ")"
                                          )
                                   , .y
                       ) %>%
                      gdalcubes::reduce_time(names = .y
                                             , FUN = function(a) {
                                               
                                               median(a, na.rm = TRUE)
                                               
                                             }
                                             )
                    
                    gdalcubes::write_tif(r
                                         , dir = ras_save_dir
                                         , prefix = paste0(.y, "__")
                                         )
                    
                  }
                  
                }
    )
    
    
    stack <- fs::dir_info(ras_save_dir
                         , regexp = "tif$"
                         ) %>%
      dplyr::filter(grepl(paste0(c(gsub("nbart_", "", get_bands)
                                   , names(indices)
                                   )
                                 , collapse = "|"
                                 )
                          , path
                          )
                    ) %>%
      dplyr::mutate(name = gsub("\\.tif", "", basename(path))) %>%
      tidyr::separate(name, into = c("band", "date"), sep = "__") %>%
      dplyr::select(path, band, date) %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::filter(date == start_date) %>%
      dplyr::pull(path) %>%
      terra::rast()
    
    return(stack)
    
  }
  
  temp <- fs::dir_info(ras_save_dir
                       , regexp = "tif$"
                       ) %>%
    dplyr::filter(modification_time == max(modification_time)) %>%
    dplyr::pull(path) %>%
    `[[`(1) %>%
    terra::rast()
  
  terra::plot(temp)
  
  # animate-------
  
  library(colorspace)
  
  ndvi.col = function(n) {
    
    rev(sequential_hcl(n, "Green-Yellow"))
    
  }
  
  v <- v %>%
    cube_view(dt = "P1M"
              , aggregation = "median"
              , resampling = "bilinear"
              )
  
  gdalcubes::raster_cube(col
                         , v
                         ) %>%
    gdalcubes::apply_pixel("(nbart_nir-nbart_red)/(nbart_nir+nbart_red)", "NDVI") %>%
    gdalcubes::animate(col = ndvi.col
                       , zlim = c(-0.5, 1)
                       , key.pos = 1
                       , save_as = "anim.gif"
                       , fps = 4
                       )
  
  
  
  
  