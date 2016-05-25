##draws all planets positions
drawPlantesPositions=function(planets) {
  PlanetsPositionsX <- list()
  PlanetsPositionsY <- list()
  for(i in 1:length(planets)) {
    PlanetsPositionsX <- append(PlanetsPositionsX,planets[[i]]$x)
    PlanetsPositionsY <- append(PlanetsPositionsY,planets[[i]]$y)
  }
  plot(PlanetsPositionsX,PlanetsPositionsY,type='p', ylim=c(-rangeOfAxis,rangeOfAxis), xlim=c(-rangeOfAxis,rangeOfAxis))
}

rangeOfAxis <- 5000
