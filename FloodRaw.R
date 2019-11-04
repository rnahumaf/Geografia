

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

size <- 500
m <- matrix(rep(0, size^2), ncol = size) # Raster file
xlong <- floor(size/2) # Initial longitude
ylat <- floor(size/2) # Initial latitude
runs <- 100000
xl <- xlong
yl <- ylat
  
for(i in 1:runs){
  #print(i)
  # Atingiu alguma borda do mapa? Parar aqui
  if(any(c(xl, yl)%in%c(ncol(m), 1))){
    m[yl,xl]<-m[yl,xl]+1
    xl <- xlong
    yl <- ylat
    #print("a")
    next
  } else {
    # Nenhuma área ao redor é declive? Parar aqui
    if(!any(m[(yl-1):(yl+1), (xl-1):(xl+1)]<m[yl,xl])){
      m[yl,xl]<-m[yl,xl]+1
      xl <- xlong
      yl <- ylat
      #print("b")
      next
    } else {
      Choose_dir(sample(which(m[(yl-1):(yl+1), (xl-1):(xl+1)]<m[yl, xl]), 1))
      #print("c")
      next
    }
  }
}
image(m[(ylat-50):(ylat+50), (xlong-50):(xlong+50)])
image(m)
