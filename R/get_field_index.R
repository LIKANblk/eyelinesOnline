get_field_index <- function(x, y, settings){
  # @todo: write screen size in file
  
  windowWidth <- 1920
  windowHeight <- 1080
  
  
  BB.x <- windowWidth/2 - (settings$nCellsX * settings$cellSize)/2
  BB.y <- windowHeight/2 - (settings$nCellsY * settings$cellSize)/2
  iX <- floor((x - BB.x)/ settings$cellSize)
  iY <- floor((y - BB.y)/ settings$cellSize)
  if(!( iX >= 0 && iX < settings$nCellsX &&
        iY >= 0 && iY < settings$nCellsY )){
    return(NA)
  }
  
  iX + iY*settings$nCellsX
  
}