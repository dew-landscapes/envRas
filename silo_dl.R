silo_dl <- function(years_vec, variable, dir = tempdir(), mode = "wb", ...) {
  require(pbapply)
  ## can only download rain or temp at the moment
  ## all variables can be found - https://www.longpaddock.qld.gov.au/silo/about/climate-variables/
  match.arg(arg = variable, choices = c("daily_rain", "monthly_rain", "max_temp", "min_temp",
                                        "et_morton_actual"))
  years_dl <- pblapply(years_vec, function(y, ...) {
    url <- sprintf("https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual/%s/%s.%s.nc",
                   variable, y, variable)
    destfile <-  file.path(dir, sprintf("%s.%s.nc", y, variable))
    download.file(url,
                  destfile = destfile,
                  method = "libcurl", mode = mode,
                  quiet = TRUE, cacheOK = FALSE,
                  ...)
    if (file.exists(destfile)) {
      return(y)
    } else (
      return(NULL)
    )
  })
  return(sprintf("Data downloaded for years: %s", paste(unlist(years_dl), collapse = ", ")))
  }
  
  
sapply(c("daily_rain", "monthly_rain", "max_temp", "min_temp",
         "et_morton_actual"), function(var) {
             dir.create(paste0("./", var))
             silo_dl(years_vec = 1980:2022, variable = var, dir = fs::path("../../data/raster/silo"), mode = "wb")})
