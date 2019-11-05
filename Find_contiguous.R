# This script was taken from StackOverflow

img <- as.matrix(read.table(text="
0  0  0  0  0  1  1  1  0  0 
0  1  0  1  0  0  1  1  0  0
0  1  1  1  0  0  0  0  0  0
0  0  0  0  0  0  0  0  0  1
0  0  0  0  0  0  0  0  1  1
0  0  1  1  1  1  0  0  1  1", header=FALSE))


find.contiguous <- function(img, x, bg) {
  ## we need to deal with a single (row,col) matrix index
  ## versus a collection of them in a two column matrix separately.
  if (length(x) > 2) {
    lbl <- img[x][1]
    img[x] <- bg
    xc <- x[,1]
    yc <- x[,2]
  } else {
    lbl <- img[x[1],x[2]]
    img[x[1],x[2]] <- bg
    xc <- x[1]
    yc <- x[2]
  }    
  ## find all neighbors of x
  x <- rbind(cbind(xc-1, yc-1),
             cbind(xc  , yc-1),
             cbind(xc+1, yc-1),
             cbind(xc-1, yc),
             cbind(xc+1, yc),
             cbind(xc-1, yc+1),
             cbind(xc  , yc+1),
             cbind(xc+1, yc+1))
  ## that have the same label as the original x
  x <- x[img[x] == lbl,]
  ## if there is none, we stop and return the updated image
  if (length(x)==0) return(img);
  ## otherwise, we call this function recursively
  find.contiguous(img,x,bg)
}


## set the background pixel value
bg <- 0
## set the object pixel value
obj <- 1

## pad image so that the edge is background, this is necessary because 
## the neighborhood generated in find.contiguous must lie strictly within 
## the image
tmp <- matrix(bg,nrow=nrow(img)+2,ncol=ncol(img)+2)
tmp[2:(nrow(img)+1),2:(ncol(img)+1)] <- img
img <- tmp

## initialize the count to zero
count <- 0
## get all pixel coordinates that are objects
x <- which(img==obj, arr.ind=TRUE)
## loop until there are no more pixels that are objects
while (length(x) > 0) {
  ## choose a single (e.g., first) pixel location. This belongs to the current
  ## object that we will grow and remove from the image using find.contiguous
  if (length(x) > 2) {
    x <- x[1,]
  }
  ## increment the count
  count <- count + 1
  ## make the call to remove the object from img
  img <- find.contiguous(img, x, bg)
  ## find the remaining pixel locations belonging to objects
  x <- which(img==obj, arr.ind=TRUE)
}


print(paste("number of objects: ",count))
