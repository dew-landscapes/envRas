

library(SLGACloud)

# Get a single cog url
path <- SLGACloud::getProductMetaData()$COGsPath[[1]]

# fails
SLGACloud::cogLoad(path)

# add api key as per: https://esoil.io/TERNLandscapes/Public/Pages/SLGA/GetData-COGSDataStore.html
path_key <- gsub("https://", paste0("https://apikey:", Sys.getenv("TERN_API_KEY"), "@"), x = path)

# succeeds
SLGACloud::cogLoad(path_key)




settings_soil <- yaml::read_yaml("settings/soil.yaml")
  tibble::as_tibble() |>
  dplyr::filter(isCurrentVersion == 1
                , Code %in% settings_soil$code
                , Component == "Modelled-Value"
                ) |>
  dplyr::sample_n(1) |> # TESTING
  dplyr::mutate(r = purrr::map(COGsPath, \(x) SLGACloud::cogLoad(x)))

# Based on code at https://esoil.io/TERNLandscapes/Public/Pages/SLGA/GetData-COGSDataStore.html

apikey <- paste0('apikey:', Sys.getenv("TERN_API_KEY"))

clay <- c(paste0("/vsicurl/https://"
                 , apikey
                 , "@data.tern.org.au/landscapes/slga/NationalMaps/SoilAndLandscapeGrid/CLY/CLY_000_005_EV_N_P_AU_TRN_N_20210902.tif"
                 )
          , paste0("/vsicurl/https://"
                   , apikey
                   , "@data.tern.org.au/landscapes/slga/NationalMaps/SoilAndLandscapeGrid/CLY/CLY_005_015_EV_N_P_AU_TRN_N_20210902.tif"
                   )
          , paste0("/vsicurl/https://"
                   , apikey
                   , "@data.tern.org.au/landscapes/slga/NationalMaps/SoilAndLandscapeGrid/CLY/CLY_015_030_EV_N_P_AU_TRN_N_20210902.tif"
                   )
          , paste0("/vsicurl/https://"
                   , apikey
                   , "@data.tern.org.au/landscapes/slga/NationalMaps/SoilAndLandscapeGrid/CLY/CLY_030_060_EV_N_P_AU_TRN_N_20210902.tif"
                   )
          , paste0("/vsicurl/https://"
                   , apikey
                   , "@data.tern.org.au/landscapes/slga/NationalMaps/SoilAndLandscapeGrid/CLY/CLY_060_100_EV_N_P_AU_TRN_N_20210902.tif"
                   )
          ) |>
  terra::rast()





terra::window(clay) <- terra::ext(terra::vect(settings$boundary |> sf::st_transform(crs = 4326)))





sw_clay_temp <- paste0(tempfile(), ".tif")





sw_clay_native <- terra::app(clay
                             
                             
                             , fun = "mean"
                             
                             
                             , na.rm = TRUE
                             
                             
                             , filename = sw_clay_temp
                             
                             
)





sw_clay <- terra::project(sw_clay_native
                          
                          
                          , y = settings$base
                          
                          
                          , filename = fs::path(settings$soils_out
                                                
                                                
                                                , "sw_clay__static.tif"
                                                
                                                
                          )
                          
                          
                          , overwrite = TRUE
                          
                          
)