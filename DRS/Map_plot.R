library(ggplot2)
library(sf)
library(ggspatial)
library(rgeos)
library(readxl)

DRS <- read_xlsx("DRS.xlsx")
DRS <- data.frame(DRS)
shp <- read_sf('35MUE250GC_SIR.shp')

shpdrs <- merge(shp, DRS, by.x = "NM_MUNICIP", by.y = "UF")

# Deixar na pasta do diretório de trabalho do R
# os arquivos de shapefile, junto com os outros (shx, dbf, etc.)

ggplot(shpdrs) + 
  geom_sf(aes(fill = DRS)) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(linetype = 0), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.title = element_blank()
        )

# setEPS()
# postscript("whatever.eps")
# ggplot() + ...
# dev.off()

ggsave("map_SP.png", width = 12, height = 8, dpi = 300)
