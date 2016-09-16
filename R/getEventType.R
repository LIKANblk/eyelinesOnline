getEventType <- function(game_state, x, y, windowWidth, windowHeight,
                         nCellsX, nCellsY, cellSize, cellMargin)
{
  BB.x <- windowWidth/2 - (nCellsX * cellSize)/2
  BB.y <- windowHeight/2 - (nCellsY * cellSize)/2
  iX <- floor((x - BB.x)/ cellSize)
  iY <- floor((y - BB.y)/ cellSize)
  if(!( iX >= 0 && iX < nCellsX &&
     iY >= 0 && iY < nCellsY )){
    stop(sprintf("Click to unknown cell %i %i", iX, iY))
  }
  
  return(game_state[ iY+1, iX+1 ])
}