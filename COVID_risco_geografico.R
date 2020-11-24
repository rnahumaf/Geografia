library("geobr") #atualizado em 2020
library(pracma)
library("covid19br") #de Set 2020 em diante
library(ggplot2)
library(ggpubr)

Estado <- "SP"

SP <- geobr::read_municipality(Estado)

BR <- covid19br::downloadCovid19("cities")

SP2 <- BR[BR$state==Estado,]

SP$death <- NA

for(i in unique(SP$name_muni)){
  f <- SP2$city==i
  p <- SP2$pop[f][1] - rev(SP2$accumDeaths[f])[1]*370
  m <- SP$name_muni==i
  prop <- rev( SP2$accumDeaths[f] )[1] * 370 / rev( SP2$accumCases[f] )[1]
  # SP$case[m] <- (mean( rev( SP2$newCases[f] )[1:7] )/p)*100
  SP$death[m] <- (mean(rev(SP2$newDeaths[f])[1:7], na.rm = T)*370 +
    mean(rev(SP2$newCases[f])[1:7])*prop)*100/(2*p)
}

p1 <- ggplot(SP) +
  geom_sf() +
  geom_sf(data = SP[SP$death >0,], aes(fill = log10(death))) +
  scale_fill_viridis_b(breaks = c(-2, -1, 0), labels = c("1 a cada 10 000", "1 a cada 1 000", "1 a cada 100"), name = "Risco diário\nde infecção")+
  theme_classic()

ggexport(p1, filename = "risco_SP.png", res = 300, width = 3000, height = 3000)
