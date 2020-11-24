library("geobr") #atualizado em 2020
library(pracma)
library("covid19br") #de Set 2020 em diante
library(ggplot2)
library(ggpubr)

Estado <- "SP"

SP <- geobr::read_municipality(Estado)

BR <- covid19br::downloadCovid19("cities")

SP2 <- BR[BR$state==Estado,]

SP$imune <- NA

for(i in unique(SP$name_muni)){
  f <- SP2$city==i
  p <- (rev(SP2$accumDeaths[f])[1]*370)/SP2$pop[f][1]
  m <- which(SP$name_muni==i)
  SP$imune[m] <- p
}

p1 <- ggplot(SP) +
  geom_sf() +
  geom_sf(data = SP[SP$imune >0,], aes(fill = imune)) +
  scale_fill_viridis_b(breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7), 
                       labels = c("10%", "20%", "30%", "40%", "50%", "60%", "70%"), name = "Estimativa de\npopulação imunizada\n(calculado pelos óbitos)")+
  theme_classic()

ggexport(p1, filename = "imune_SP.png", res = 300, width = 3000, height = 3000)
