# source  http://stackoverflow.com/questions/40623573/read-image-data-into-r-piece-by-piece
# code http://stackoverflow.com/users/2792099/aoles

library(RBioFormats)

filename <- system.file("images", "sample-color.png", package="EBImage")

## first, get image dimensions from metadata
meta <- coreMetadata(read.metadata(filename))

xdim <- meta$sizeX
ydim <- meta$sizeY

## set chunk size
chunksize <- 300  

## itarate over image chunks row-wise
for(i in 1:ceiling(ydim/chunksize)) {
  for(j in 1:ceiling(xdim/chunksize)) {
    x1 <- (j-1) * chunksize + 1
    x2 <- min( j * chunksize, xdim )
    y1 <- (i-1) * chunksize + 1
    y2 <- min( i * chunksize, ydim )
    
    cat(sprintf("[%d:%d, %d:%d] ", x1, x2, y1, y2))
    
    img <- read.image(filename, subset = list(X=x1:x2, Y=y1:y2))
    
    ## perform the actual image processing
    ## here we just print the min and max pixel intensities
    cat(range(img), "\n")        
  }
}