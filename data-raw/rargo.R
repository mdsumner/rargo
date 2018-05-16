#u1 <- "http://www.usgodae.org/ftp/outgoing/argo/latest_data/D20180209_prof.nc"
u1 <- "http://www.usgodae.org/ftp/outgoing/argo/latest_data/D20180209_prof_0.nc"
# curl::curl_download(u1, file.path("data-raw", basename(u1)))
#
# curl::curl_download(u2, file.path("data-raw", basename(u2)))
#u1 <- "http://data.nodc.noaa.gov/argo/gadr/data/csiro/5903948/nodc_5903948_prof.nc"
#curl::curl_download(u1, file.path("data-raw", basename(u1)))
f1 <- file.path("data-raw", basename(u1))
library(RNetCDF)
library(raster)

nc <- RNetCDF::open.nc(f1)
## we can't get 1D vars with raster
lon <- RNetCDF::var.get.nc(nc, toupper("LONGITUDE"))
lat <- RNetCDF::var.get.nc(nc, toupper("LATITUDE"))
juld <- RNetCDF::var.get.nc(nc, toupper("JULD"))
platform <- RNetCDF::var.get.nc(nc, toupper("PLATFORM_NUMBER"))

temp <- t(raster(f1, varname = toupper("TEMP")))
pres <- t(raster(f1, varname = toupper("PRES")))
## and set extent to 0, ncol, 0, nrow
temp <- setExtent(temp, extent(0, ncol(temp), 0, nrow(temp)))
pres <- setExtent(pres, extent(temp))

## get generalize groupping later
groupi <- platform == unique(platform)[1]
temp <- crop(temp, extent(temp, 1, nrow(temp), 1, 183))
pres <- crop(pres, extent(pres, 1, nrow(pres), 1, 183))


## pres is right, negated for the Z on the qm
qm <- quadmesh::quadmesh(pres, z = 0-pres) ## need 0- because bug in raster

## we need a quadmesh, but we can put many variables "on" it
qm$material <- list(col = viridis::viridis(100)[scales::rescale(extract(temp, t(qm$vb[1:2, ]), method = "bilinear"), c(1, 100))][qm$ib])


## no return from this point, if needs to be redone recreate qm
X <- extract(setValues(temp, rep(lon[1:183], nrow(temp))), t(qm$vb[1:2, ]),
                      method = "bilinear")
Y <- extract(setValues(temp, rep(lat[1:183], nrow(temp))), t(qm$vb[1:2, ]),
                      method = "bilinear")
qm$vb[1:2, ] <- t(rgdal::project(cbind(X, Y), "+proj=laea +lon_0=-174 +lat_0=-39"))


## plotting in 3d is one thing
## but for 2D we need quads as triangles
#qm$ib[,1]

plot(t(qm$vb[, qm$ib[,1000]]))
## first example needed filtering out a longitude range
#bad <- qm$vb[1, ] < 100
#qm$vb[, bad] <- NA
library(rgl)
shade3d(qm)
aspect3d(1, 1, 1)
