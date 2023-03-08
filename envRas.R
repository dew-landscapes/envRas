
  #-----packages------

  library(sits)
  library(fs)
  library(sf)
  library(dplyr)
  
  
  #------setup------
  
  sat <- "sentinel"
  
  ras_dir <- fs::path("D:"
                      , "env"
                      , "data"
                      , "raster"
                      , sat
                      )
  
  fs::dir_create(ras_dir)
  fs::dir_create("out")
  
  use_cores <- 5
  
  
  # State map
  aoi <- sf::st_read(fs::path("..", "envEco", "data","shp","aust_cd66states.shp")
                     , crs = 4326, quiet = TRUE
                     ) %>%
    dplyr::filter(STE == 4) %>%
    dplyr::mutate(State = "SA") %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_make_valid()
    
  
  out_file <- fs::path("out"
                       , paste0("cube_"
                                , sat
                                , ".rds"
                                )
                       )
  
  if(!file.exists(out_file)) {
    
    if(sat == "sentinel") {
    
      cube <- sits::sits_cube(source = "AWS"
                              , collection = "SENTINEL-S2-L2A-COGS"
                              , roi = aoi
                              , start_date = "2018-12-01"
                              , end_date = "2021-02-28"
                              )
      
      rio::export(cube
                  , out_file
                  )
      
    } else if(sat == "landsat") {
      
      cube <- sits::sits_cube(source = "MPC"
                              , collection = "LANDSAT-C2-L2"
                              , roi = aoi
                              , start_date = "2021-12-01"
                              , end_date = "2022-02-28"
                              )
      
      rio::export(cube
                  , out_file
                  )
      
    }
    
  } else {
    
    cube <- rio::import(out_file)
    
  }
  
  
  reg_cube <- sits::sits_regularize(cube = cube
                                    , output_dir = ras_dir
                                    , res = 30
                                    , period = "P1M"
                                    , multicores = use_cores
                                    )
  
  local_cube <- sits::sits_cube(source = "AWS"
                                , collection = "SENTINEL-S2-L2A-COGS"
                                , data_dir = ras_dir
                                )
  
  if(FALSE) {
    
    plot(local_cube[154, ]
         , red = "B01"
         , green = "B02"
         , blue = "B03"
         , date = "2021-12-01"
         )
    
  }
  
  
  
  
  #------NDVI---------
  
  ndvi_cube <- sits::sits_apply(local_cube
                                , NDVI = (B08 - B04) / (B08 + B04)
                                , output_dir = ras_dir
                                , multicores = use_cores
                                , memsize = 100
                                )
  
  if(FALSE) {
    
    plot(local_cube[100,], band = "NDVI", palette = "RdYlGn")
    
  }
  
  