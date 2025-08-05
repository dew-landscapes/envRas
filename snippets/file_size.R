library(terra)
pathraster <- system.file("ex/elev.tif", package = "terra")
r <- rast(pathraster)

values(r) <- rnorm(ncell(r))

writeRaster(r
            , "INT2U.tif"
            , datatype = "INT2U"
            , gdal = c("COMPRESS=NONE")
            , overwrite = TRUE
            )

writeRaster(r
            , "FLT4S.tif"
            , datatype = "FLT4S"
            , gdal = c("COMPRESS=NONE")
            , overwrite = TRUE
            )
