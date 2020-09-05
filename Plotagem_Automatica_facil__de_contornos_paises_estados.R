library(rnaturalearth)
library(terra)

world <- rnaturalearth::ne_countries()
states <- rnaturalearth::ne_states(country = "Brazil")

plot(world[world$subregion=="South America",])
plot(states[states$name=="Amazonas",], add = T, col = "lightgray")
rect(-60.6, -3.25, -60, -2.4)
