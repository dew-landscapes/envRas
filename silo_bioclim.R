library(terra)
library(dismo)
library(pbapply)
library(parallel)

prec <- list.files("../../data/raster/silo/",
                   full.names = TRUE, 
                   pattern = "monthly_rain"
                   )
prec <- pblapply(prec, rast)
prec <- rast(prec)
prec <- tapp(prec, index = as.integer(format(time(prec), "%m")), fun = mean)
names(prec) <- month.abb
time(prec) <- seq(as.Date("2001-01-16"), l = 12, by = "month")

tmax <- list.files("../../data/raster/silo/",
                   full.names = TRUE,
                   pattern = "max_temp"
                   )

cls <- makeCluster(14L)
clusterExport(cls, "tmax")
clusterCall(cls, library(terra))
tmax <- pblapply(tmax, function(fil) {
  library(terra)
  r <- rast(fil)
  yr <- format(time(r[[1]]), "%Y")
  r <- tapp(r, index = as.integer(format(time(r), "%m")), max)
  # values(r) <- round(values(r), 1)
  names(r) <- month.abb
  time(r) <- seq(as.Date(paste0(yr, "-01-01")), by = "month", l = 12)
  return(wrap(r))
}, cl = cls)
tmax <- pblapply(tmax, terra::unwrap)
stopCluster(cls)
tmax <- rast(tmax)
tmax <- tapp(tmax, index = as.integer(format(time(tmax), "%m")), fun = max)
names(tmax) <- month.abb
time(tmax) <- seq(as.Date("2001-01-01"), l = 12, by = "month")

tmin <- list.files("../../data/raster/silo/",
                   full.names = TRUE,
                   pattern = "min_temp"
                   )
cls <- makeCluster(14L)
clusterExport(cls, "tmin")
tmin <- pblapply(tmin, function(fil) {
  library(terra)
  r <- rast(fil)
  yr <- format(time(r[[1]]), "%Y")
  r <- tapp(r, index = as.integer(format(time(r), "%m")), min)
  values(r) <- round(values(r), 1)
  names(r) <- month.abb
  time(r) <- seq(as.Date(paste0(yr, "-01-01")), by = "month", l = 12)
  return(wrap(r))
}, cl = cls)
tmin <- pblapply(tmin, terra::unwrap)
stopCluster(cls)
tmin <- rast(tmin)
tmin <- tapp(tmin, index = as.integer(format(time(tmin), "%m")), fun = min)
names(tmin) <- month.abb
time(tmin) <- seq(as.Date("2001-01-01"), l = 12, by = "month")

library(raster)

bio <- dismo::biovars(prec = raster::stack(prec),
                      tmin = raster::stack(tmin),
                      tmax = raster::stack(tmax))

bio <- terra::rast(bio)
values(bio) <- round(values(bio), 3)
plot(bio)

terra::writeRaster(bio,
            "../../data/raster/silo/silo_bioclim_aus.tif",
            gdal = c("COMPRESS=NONE", "TFW=NO", "PREDICTOR=3"))
