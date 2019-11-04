# Fill basins from coordinates Lat Long or X Y
# Meters above point = 1

S_map <- DSM_HARV # Raster
m_layer <- S_map
values(m_layer) <- NA
S_map <- as.matrix(S_map) # Matrix transformation for WAY faster calculations
m_layer <- as.matrix(m_layer) # Matrix transformation for WAY faster calculations
meters <- 1
xlong <- 1948 # Initial "Flood point"
ylat <- 1836 # Initial "Flood point"
starting_height <- DSM_HARV[1836,1948]
desired_height <- starting_height + meters
run_x <- xlong
run_y <- ylat
Filled_basin <- F
queue <- c()

while(Filled_basin == F){
  if(S_map[ylat, xlong] < desired_height){ # 14.068, 0.005
    m_layer[ylat, xlong] <- desired_height - S_map[ylat, xlong]
    S_map[ylat, xlong] <- desired_height
    next
  } else {
    # Indexar as células adjacentes que serão preenchidas no futuro
    Next_candidates <- which(S_map[(ylat-1):(ylat+1), (xlong-1):(xlong+1)] < desired_height)
    if(all(length(Next_candidates)==0, length(queue)==0)){Filled_basin <- T; next}
    if(length(Next_candidates)==0){
      pick_dir <- sample(1:nrow(queue),1)
      ylat <- queue[pick_dir,1]
      xlong <- queue[pick_dir,2]
      linhas_queue <- which(apply(queue, 1, function(x) identical(as.vector(x), c(ylat[[1]], xlong[[1]]))))
      queue <- queue[-linhas_queue,]
      next
    }
    x_candidate <- coord_xlong(xlong, Next_candidates)
    y_candidate <- coord_ylat(ylat, Next_candidates)
    queue <- rbind(queue, cbind(y_candidate, x_candidate))
    
    # Escolher uma direção
    pick_dir <- sample(1:nrow(queue),1)
    ylat <- queue[pick_dir,1]
    xlong <- queue[pick_dir,2]
    
    # Excluir direção escolhida da queue (e todas as suas eventuais réplicas)
    linhas_queue <- which(apply(queue, 1, function(x) identical(as.vector(x), c(ylat[[1]], xlong[[1]]))))
    queue <- queue[-linhas_queue,]
  }
}

coord_ylat <- function(ylat, ww){
  result <- c()
  for(i in 1:length(ww)){
    if(ww[i] %in% c(1,4,7)){result <- c(result, ylat-1)}else{
      if(ww[i] %in% c(3,6,9)){result <- c(result, ylat+1)}else{
        result <- c(result, ylat)
      }
    }
  }
  return(result)
}

coord_xlong <- function(xlong, ww){
  result <- c()
  for(i in 1:length(ww)){
    if(ww[i] %in% c(1,2,3)){result <- c(result, xlong - 1)}else{
      if(ww[i] %in% c(7,8,9)){result <- c(result, xlong + 1)}else{
        result <- c(result, xlong)
      }
    }
  } 
  return(result)
}

Rep_m <- DSM_HARV
values(Rep_m) <- NA
values(Rep_m) <- m_layer

plot(DSM_HARV, xlim = c(-47.6,-47.3), ylim = c(-23.65, -23.35))
# coco <- colorRampPalette(c("snow1", "steelblue"), 4); colors()
plot(Rep_m, xlim = c(-47.6,-47.3), ylim = c(-23.65, -23.35), add=T, legend=F, col=coco(10))

