library(raster)
DSM_HARV <- raster("/home/rodrigo/Downloads/s24_w048_1arc_v3.tif")
plot(DSM_HARV)

m1 <- (as.matrix(DSM_HARV))
# image(t(apply(m1, 2, rev))) # Plot Image
m1 <- raster(m1)
# plot(m1) # Plot Raster

fd <- terrain(DSM_HARV, opt = "flowdir")
path <- flowPath(fd, p = c(-47.6, -23.8))
xy <- xyFromCell(fd, path)
plot(DSM_HARV, xlim = c(-47.61,-47.59), ylim = c(-23.81, -23.79))
lines(xy)
