library(raster)
DSM_HARV <- raster("/home/rodrigo/Downloads/s24_w048_1arc_v3.tif")

m1 <- (as.matrix(DSM_HARV))

# m1 <- raster(m1) # Before plotting

# Function: choose next flow direction
Choose_dir <- function(x){
  if(x == 1){yl <<- yl - 1; xl <<- xl - 1} else {
    if(x == 2){xl <<- xl - 1} else {
      if(x == 3){yl <<- yl + 1; xl <<- xl - 1} else {
        if(x == 4){yl <<- yl - 1} else {
          if(x == 6){yl <<- yl + 1} else {
            if(x == 7){yl <<- yl - 1; xl <<- xl + 1} else {
              if(x == 8){xl <<- xl + 1} else {
                yl <<- yl + 1; xl <<- xl + 1 # 9
              }
            }
          }
        }
      }
    }
  }
}

size <- nrow(m1)
m <- m1 # Helper matrix
m2 <- matrix(rep(0, size^2), ncol = size) # Mixer matrix
xlong <- floor(size*0.5) # Initial longitude
ylat <- floor(size*0.8) # Initial latitude
runs <- 1500000 # Estimado 2h
xl <- xlong
yl <- ylat

for(i in 1:runs){
  #print(i)
  # Atingiu alguma borda do mapa? Parar aqui
  if(any(c(xl, yl)%in%c(ncol(m), 1))){
    m[yl,xl]<-m[yl,xl]+1
    m2[yl, xl] <- m2[yl, xl] + 1
    xl <- xlong
    yl <- ylat
    #print("a")
    next
  } else {
    # Nenhuma área ao redor é declive? Parar aqui
    if(!any(m[(yl-1):(yl+1), (xl-1):(xl+1)]<m[yl,xl])){
      m[yl,xl]<-m[yl,xl]+1
      m2[yl, xl] <- m2[yl, xl] + 1
      xl <- xlong
      yl <- ylat
      #print("b")
      next
    } else {
      m2[yl, xl] <- m2[yl, xl] + 1
      Choose_dir(sample(which(m[(yl-1):(yl+1), (xl-1):(xl+1)]<m[yl, xl]), 1))
      #print("c")
      next
    }
  }
}

m2[m2==0]<-NA

a<-(as.matrix(m2)[(ylat-100):(ylat+100), (xlong-100):(xlong+100)])
b<-(as.matrix(m1)[(ylat-100):(ylat+100), (xlong-100):(xlong+100)])

ar <- raster(a)
br <- raster(b)

plot(br); plot(ar, add=T)

library(RColorBrewer)
library(colorspace)
pal <- choose_palette()
pal1 <- choose_palette()

plot(br, col=pal1(15)); plot(ar, add=T, col=pal(200))
