
library(terra)
library(pbapply)
library(parallel)
crop_ext <- c(127, 143,-39,-25)

sa <- vect(geoarrow::read_geoparquet_sf("../../vector/aus.parquet"))
sa <- project(sa, "EPSG:4326")
sa

# rainfall
## make sure file for current year is present
fil <- list.files("daily_rain", full.names = TRUE)
cls <- makeCluster(20)
clusterExport(cls, c("fil", "crop_ext"))
rainfall <- pblapply(fil,
                        function(x) {
                          require(terra)
                          r <- crop(rast(x), crop_ext)
                          values(r) <- as.integer(round(values(r)))
                          terra::wrap(r)
                 })
stopCluster(cls)
rainfall[1:3]
rainfall <- pblapply(rainfall, terra::unwrap)
rainfall <- rast(rainfall)
rainfall
plot(rainfall[[1:4]], fun = function() lines(sa))
writeRaster(rainfall,
            "sa_daily_rainfall_1980_2022.tif",
            datatype = "INT2U",
            NAflag = 65534,
            overwrite = TRUE,
            gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2"))

# max temp
fil <- list.files("max_temp", full.names = TRUE)
cls <- makeCluster(20)
clusterExport(cls, c("fil", "crop_ext"))
max_temp <- pblapply(fil,
                     function(x) {
                       require(terra)
                       r <- crop(rast(x), crop_ext)
                       values(r) <- as.integer(round(values(r)))
                       terra::wrap(r)
                     })
stopCluster(cls)
max_temp[1:3]
max_temp <- pblapply(max_temp, terra::unwrap)
max_temp <- rast(max_temp)
max_temp
plot(max_temp[[1:4]], fun = function() lines(sa))
max(global(max_temp, max, na.rm = TRUE))
min(global(max_temp, min, na.rm = TRUE))

writeRaster(max_temp,
            "sa_daily_maxtemp_1980_2022.tif",
            datatype = "INT1U",
            overwrite = TRUE,
            gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2"))

# min temp
fil <- list.files("min_temp", full.names = TRUE)
cls <- makeCluster(20)
clusterExport(cls, c("fil", "crop_ext"))
min_temp <- pblapply(fil,
                     function(x) {
                       require(terra)
                       r <- crop(rast(x), crop_ext)
                       values(r) <- as.integer(round(values(r)))
                       terra::wrap(r)
                     })
stopCluster(cls)
min_temp[1:3]
min_temp <- pblapply(min_temp, terra::unwrap)
min_temp <- rast(min_temp)
plot(min_temp[[1:4]], fun = function() lines(sa))
max(global(min_temp, max, na.rm = TRUE))
min(global(min_temp, min, na.rm = TRUE))

writeRaster(min_temp,
            "sa_daily_mintemp_1980_2022.tif",
            datatype = "INT2S",
            overwrite = TRUE,
            gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2"))

# Morton ET
fil <- list.files("et_morton_actual", full.names = TRUE)
cls <- makeCluster(20)
clusterExport(cls, c("fil", "crop_ext"))
et_morton <- pblapply(fil,
                     function(x) {
                       require(terra)
                       r <- crop(rast(x), crop_ext)
                       values(r) <- round(values(r), 1)
                       terra::wrap(r)
                     })
stopCluster(cls)
et_morton[1:3]
et_morton <- pblapply(et_morton, terra::unwrap)
et_morton <- rast(et_morton)
et_morton
plot(et_morton[[1:2]], fun = function() lines(sa))
max(global(et_morton, max, na.rm = TRUE))
min(global(et_morton, min, na.rm = TRUE))
writeRaster(et_morton,
            "sa_daily_etmorton_1980_2022.tif",
            datatype = "INT1U",
            overwrite = TRUE,
            scale = 0.1,
            gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2"))

# project
region <- rast("sa_region_epsg8059_5000m.tif") # 5km projected SA + 100km buffer
region
plot(region)

# rainfall
rain_prj <- rast("sa_daily_rainfall_1980_2022_prj.tif")
rain_prj
writeRaster(rain_prj,
            "sa_daily_rainfall_1980_2022_prj_int.tif",
            datatype = "INT2U",
            overwrite = TRUE,
            scale = 0.1,
            gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2"))
rm(rain_prj)

# max temp
tmax <- rast("sa_daily_maxtemp_1980_2022_prj.tif")
tmax
writeRaster(tmax,
            "sa_daily_maxtemp_1980_2022_prj_int.tif",
            datatype = "INT2U",
            overwrite = TRUE,
            scale = 0.1,
            gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2"))
rm(tmax)

# min temp
tmin <- rast("sa_daily_mintemp_1980_2022_prj.tif")
tmin
writeRaster(tmin,
            "sa_daily_mintemp_1980_2022_prj_int.tif",
            datatype = "INT2S",
            overwrite = TRUE,
            scale = 0.1,
            gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2"))
rm(tmin)

# et
et <- rast("sa_daily_etmorton_1980_2022_prj.tif")
et
writeRaster(et,
            "sa_daily_etmorton_1980_2022_prj_int.tif",
            datatype = "INT2U",
            overwrite = TRUE,
            scale = 0.1,
            gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2"))
rm(et)

# weekly
rain <- rast("sa_daily_rainfall_1980_2022_prj_int.tif")
z <- seq(as.Date("1980-01-01"), by = "day", length = nlyr(rain))
any(grepl("02-29", z))
idx <- which(grepl("02-29", z))
rain <- rain[[-idx]]
time(rain) <- z[-idx]
rain
# windex <- as.integer(format(time(rain, "days"), "%j"))
# windex <- paste0(format(time(rain), "%Y"),".", (index-1) %/% 7 + 1)

wRain <- tapp(rain, index = "yearweeks", fun = "sum", na.rm = TRUE,
              filename = "sa_weekly_total_rainfall_1980_2022_prj_int.tif",
              overwrite = TRUE,
              wopt = list(
                datatype = "INT2U",
                scale = 0.1,
                gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2")
              ))
wRain
mRain <- tapp(rain, index = "yearmonths", fun = "sum", na.rm = TRUE,
              filename = "sa_monthly_total_rainfall_1980_2022_prj_int.tif",
              overwrite = TRUE,
              wopt = list(
                datatype = "INT2U",
                scale = 0.1,
                gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2")
              ))
mRain

# max temp
tmax <- rast("sa_daily_maxtemp_1980_2022_prj_int.tif")
tmax <- tmax[[-idx]]
time(tmax) <- z[-idx]
wTmax <- tapp(tmax, "yearweeks", fun = "mean", na.rm = TRUE,
              filename = "sa_weekly_avg_tmax_1980_2022_prj_int.tif",
              overwrite = TRUE,
              wopt = list(
                datatype = "INT2U",
                scale = 0.1,
                gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2")
              ))
mTmax <- tapp(tmax, "yearmonths", fun = "mean", na.rm = TRUE,
              filename = "sa_monthly_avg_tmax_1980_2022_prj_int.tif",
              overwrite = TRUE,
              wopt = list(
                datatype = "INT2U",
                scale = 0.1,
                gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2")
              ))

# min temp
tmin <- rast("sa_daily_mintemp_1980_2022_prj_int.tif")
tmin <- tmin[[-idx]]
time(tmin) <- z[-idx]
wTmin <- tapp(tmin, "yearweeks", fun = "mean", na.rm = TRUE,
              filename = "sa_weekly_avg_tmin_1980_2022_prj_int.tif",
              overwrite = TRUE,
              wopt = list(
                datatype = "INT2S",
                scale = 0.1,
                gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2")
              ))
mTmin <- tapp(tmin, "yearmonths", fun = "mean", na.rm = TRUE,
              filename = "sa_monthly_avg_tmin_1980_2022_prj_int.tif",
              overwrite = TRUE,
              wopt = list(
                datatype = "INT2S",
                scale = 0.1,
                gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2")
              ))

# et
etmorton <- rast("sa_daily_etmorton_1980_2022_prj_int.tif")
etmorton <- etmorton[[-idx]]
time(etmorton) <- z[-idx]
wetmorton <- tapp(etmorton, "yearweeks", fun = "sum", na.rm = TRUE,
              filename = "sa_weekly_total_etmorton_1980_2022_prj_int.tif",
              overwrite = TRUE,
              wopt = list(
                datatype = "INT2U",
                scale = 0.1,
                gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2")
              ))
metmorton <- tapp(etmorton, "yearmonths", fun = "sum", na.rm = TRUE,
              filename = "sa_monthly_total_etmorton_1980_2022_prj_int.tif",
              overwrite = TRUE,
              wopt = list(
                datatype = "INT2U",
                scale = 0.1,
                gdal = c("COMPRESS=LZW", "TFW=NO", "PREDICTOR=2")
              ))
