# u1 <- "http://www.usgodae.org/ftp/outgoing/argo/latest_data/D20180209_prof.nc"
# u2 <- "http://www.usgodae.org/ftp/outgoing/argo/latest_data/D20180209_prof_0.nc"
# curl::curl_download(u1, file.path("data-raw", basename(u1)))
#
# curl::curl_download(u2, file.path("data-raw", basename(u2)))
f1 <- file.path("data-raw", "D20180209_prof.nc")
library(RNetCDF)
library(raster)

nc <- RNetCDF::open.nc(f1)

## we can't get 1D vars with raster
lon <- RNetCDF::var.get.nc(nc, "LONGITUDE")
lat <- RNetCDF::var.get.nc(nc, "LATITUDE")
juld <- RNetCDF::var.get.nc(nc, "JULD")
temp <- t(raster(f1, varname = "TEMP"))
pres <- t(raster(f1, varname = "PRES"))
## and set extent to 0, ncol, 0, nrow
temp <- setExtent(temp, extent(0, ncol(temp), 0, nrow(temp)))
pres <- setExtent(pres, extent(temp))

## pres is right, negated for the Z on the qm
qm <- quadmesh::quadmesh(pres, z = 0-pres) ## need 0- because bug in raster

## we need a quadmesh, but we can put many variables "on" it
qm$material <- list(col = viridis::viridis(100)[scales::rescale(extract(temp, t(qm$vb[1:2, ]), method = "bilinear"), c(1, 100))][qm$ib])

## no return from this point, if needs to be redone recreate qm
qm$vb[1, ] <- extract(setValues(temp, rep(lon, nrow(temp))), t(qm$vb[1:2, ]),
                      method = "bilinear")
qm$vb[2, ] <- extract(setValues(temp, rep(lat, nrow(temp))), t(qm$vb[1:2, ]),
                      method = "bilinear")


bad <- qm$vb[1, ] < 100
qm$vb[, bad] <- NA
library(rgl)
shade3d(qm)
aspect3d(1, 1, 0.1)
