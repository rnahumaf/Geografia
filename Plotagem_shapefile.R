library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rgeos)

world <- ne_countries(scale = "large", returnclass = "sf")

# Deixar na pasta do diretório de trabalho do R
# os arquivos de shapefile, junto com os outros (shx, dbf, etc.)
shp <- read_sf('MARINE_MAMMALS.shp')
ggplot(shp[which(shp$genus=="Trichechus"),]) + 
  geom_sf(data = world, fill= "antiquewhite") +
  geom_sf(aes(colour = binomial)) +
  coord_sf(xlim = c(-100, 20), ylim = c(-15, 40)) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(linetype = 0), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.title = element_blank(),
        legend.position = c(0.9, 0.9))


ggsave("map_mammal.png", width = 12, height = 6, dpi = 300)
