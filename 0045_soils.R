
fs::dir_create(settings$soils_out)

# Based on code at https://esoil.io/TERNLandscapes/Public/Pages/SLGA/GetData-COGSDataStore.html

apikey <- paste0('apikey:', Sys.getenv("TERN_API_KEY"))

# clay --------
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


# sand --------

sand <- c(paste0("/vsicurl/https://"
                 , apikey
                 , "@data.tern.org.au/landscapes/slga/NationalMaps/SoilAndLandscapeGrid/SND/SND_000_005_EV_N_P_AU_TRN_N_20210902.tif"
                 )
          , paste0("/vsicurl/https://"
                   , apikey
                   , "@data.tern.org.au/landscapes/slga/NationalMaps/SoilAndLandscapeGrid/SND/SND_005_015_EV_N_P_AU_TRN_N_20210902.tif"
                   )
          , paste0("/vsicurl/https://"
                   , apikey
                   , "@data.tern.org.au/landscapes/slga/NationalMaps/SoilAndLandscapeGrid/SND/SND_015_030_EV_N_P_AU_TRN_N_20210902.tif"
                   )
          , paste0("/vsicurl/https://"
                   , apikey
                   , "@data.tern.org.au/landscapes/slga/NationalMaps/SoilAndLandscapeGrid/SND/SND_030_060_EV_N_P_AU_TRN_N_20210902.tif"
                   )
          , paste0("/vsicurl/https://"
                   , apikey
                   , "@data.tern.org.au/landscapes/slga/NationalMaps/SoilAndLandscapeGrid/SND/SND_060_100_EV_N_P_AU_TRN_N_20210902.tif"
                   )
          ) |>
  terra::rast()

terra::window(sand) <- terra::ext(terra::vect(settings$boundary |> sf::st_transform(crs = 4326)))

sw_sand_temp <- paste0(tempfile(), ".tif")

sw_sand_native <- terra::app(sand
                             , fun = "mean"
                             , na.rm = TRUE
                             , filename = sw_sand_temp
                             )

sw_sand <- terra::project(sw_sand_native
                          , y = settings$base
                          , filename = fs::path(settings$soils_out
                                                , "sw_sand__static.tif"
                                                )
                          , overwrite = TRUE
                          )
