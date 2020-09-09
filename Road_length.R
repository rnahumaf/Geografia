# MEASURING ROAD LENGTH WITHIN A MUNICIPALITY

# https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html

library("rgeos")
library("sp")
library("rgdal")
library("osmdata")
library("spatstat")

q1 <- opq('Sorocaba, São Paulo') %>%
  add_osm_feature(key = 'highway')
cway_sev <- osmdata_sp(q1)
sp::plot(cway_sev$osm_lines)

# https://gis.stackexchange.com/questions/119993/convert-line-shapefile-to-raster-value-total-length-of-lines-within-cell
roads_utm <- spTransform(cway_sev$osm_lines, CRS("+init=epsg:3857"))
gLength(roads_utm)
