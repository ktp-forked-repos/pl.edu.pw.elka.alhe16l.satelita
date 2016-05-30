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

drawDiagrams=function(outDEoptim, sizeOfPopulation){
  #best values of ff in each interation
  dev.new()
  plot(outDEoptim,plot.type="bestvalit")
  #values of every objects in each interation
  dev.new()
  plot(outDEoptim,plot.type="storepop")
  times<-list()
  angles<-list()
  velocities<-list()
  for(i in 1:length(outDEoptim$member$storepop)) {
    for(j in 1:sizeOfPopulation)
    {
      times <- append(times,outDEoptim$member$storepop[[i]][[j]])
      angles <- append(angles,outDEoptim$member$storepop[[i]][[j+sizeOfPopulation]])
      velocities <- append(velocities,outDEoptim$member$storepop[[i]][[j+(2*sizeOfPopulation)]])
    }
  }
  dev.new()
  scatterplot3d(times,angles,velocities, pch=19, color="steelblue", type="h")
  summary(outDEoptim)
  print("Srednia najlepszych wartosci")
  print(mean(outDEoptim$member$bestvalit))
  print("Odchylenie standardowe najlepszych wartosci")
  print(sd(outDEoptim$member$bestvalit))
}

## test of metaheuristic DEoptim
testDEoptim=function()
{
  tmp <- minNP
  while(tmp<=maxNP)
  {
    outDEoptim <- DEoptim(fitnessFunction, lower, upper, DEoptim.control(storepopfrom=0, NP=tmp, CR=defaultCR, F=defaultF, itermax= defaultIter, strategy=defaultStrategy))
    drawDiagrams(outDEoptim, tmp)
    tmp<-tmp+stepNP
  }
  tmp<-minCR
  while(tmp<=maxCR)
  {
    outDEoptim <-DEoptim(fitnessFunction, lower, upper, DEoptim.control(storepopfrom=0, NP=defaultNP, CR=tmp, F=defaultF, itermax= defaultIter, strategy=defaultStrategy))
    drawDiagrams(outDEoptim, defaultNP)
    tmp<-tmp+stepCR
  }
  tmp<-minF
  while(tmp<=maxF)
  {
    outDEoptim <- DEoptim(fitnessFunction, lower, upper, DEoptim.control(storepopfrom=0, NP=defaultNP, CR=defaultCR, F=tmp, itermax= defaultIter, strategy=defaultStrategy))
    drawDiagrams(outDEoptim, defaultNP)
    tmp<-tmp+stepF
  }
  tmp<-minIter
  while(tmp<=maxIter)
  {
    outDEoptim <- DEoptim(fitnessFunction, lower, upper, DEoptim.control(storepopfrom=0, NP=defaultNP, CR=defaultCR, F=defaultF, itermax= tmp, strategy=defaultStrategy))
    drawDiagrams(outDEoptim, defaultNP)
    tmp<-tmp+stepIter
  }
  tmp<-minStrategy
  while(tmp<=maxStrategy)
  {
    outDEoptim <- DEoptim(fitnessFunction, lower, upper, DEoptim.control(storepopfrom=0, NP=defaultNP, CR=defaultCR, F=defaultF, itermax=defaultIter, strategy=tmp))
    drawDiagrams(outDEoptim, defaultNP)
    tmp<-tmp+stepStrategy
  }

}

## range of Axis (draw Planets Positions)
rangeOfAxis <- 5000

## constants
## number of population members
defaultNP <- 18
minNP <- 6
maxNP <-30
stepNP <- 4

## crossover probability
defaultCR <- 0.5
minCR <- 0.1
maxCR <-0.9
stepCR <- 0.2

## differenctial weigthing factor
defaultF <- 0.8
minF <- 0.2
maxF <-1.7
stepF <- 0.3

## max iteration allowed
defaultIter <- 100
minIter <- 20
maxIter <-180
stepIter <- 40

## strategy used in optimization procedure
defaultStrategy <- 2
minStrategy <- 1
maxStrategy <-6
stepStrategy <- 1
