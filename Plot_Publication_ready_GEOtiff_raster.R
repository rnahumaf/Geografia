library("terra")
library(RColorBrewer)

# Data extracted from USGS https://earthexplorer.usgs.gov/

DSM_HARV <- terra::rast("raster/s03_w061_1arc_v3.tif")
DSM_HARV2 <- terra::rast("raster/s04_w061_1arc_v3.tif")
DSM_merge <- terra::merge(DSM_HARV, DSM_HARV2)
DSM_crop <- terra::crop(DSM_merge, terra::ext(-60.6, -60, -3.25, -2.4))

library(ggsn)
library(ggplot2)

DSM_df <- as.data.frame(DSM_crop, xy = TRUE)
str(DSM_df)

p <- ggplot() + 
  geom_raster(data = DSM_df, 
              aes(x = x, y = y, fill = s03_w061_1arc_v3)) +
  scalebar(dist = 10, model = "WGS84", transform = T, dist_unit = "km",
           x.min = -60.6, x.max = -60, y.min = -3.25, y.max = -2.4,
           st.size = 3, location = "bottomleft",
           anchor = c(x = -60.55, y = -3.2)) +
  theme(panel.background = element_blank(),
        legend.title = element_blank()) +
  coord_fixed() +
  scale_fill_gradientn(colours=c("#AAC8FA","#026547","#DFC66F", 
                                 "#9E1800", "#FFFFFF"))  +
  annotation_north_arrow(location = "tr", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-60.5, -60.1, 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-3.2, -2.5, 0.1)) +
  ylab("Latitude") + xlab("Longitude")


ggsave(plot = p, filename = "z_map.png", 
       width = 12, height = 6, dpi = 300)
