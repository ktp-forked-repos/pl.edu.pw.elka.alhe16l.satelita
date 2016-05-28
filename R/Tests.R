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

## test of metaheuristic DEoptim
testDEoptim=function()
{
  tmp <- minNP
  while(tmp<=maxNP)
  {
    resultsOfTests <- append(DEoptim(fitnessFunction, lower, upper, DEoptim.control(NP=tmp, CR=defaultCR, F=defaultF, maxIter= defaultIter, strategy=defaultStrategy)))
    tmp<-tmp+stepNP
  }
  tmp<-minCR
  while(tmp<=maxCR)
  {
    resultsOfTests <- append(DEoptim(fitnessFunction, lower, upper, DEoptim.control(NP=defaultNP, CR=tmp, F=defaultF, maxIter= defaultIter, strategy=defaultStrategy)))
    tmp<-tmp+stepCR
  }
  tmp<-minF
  while(tmp<=maxF)
  {
    resultsOfTests <- append(DEoptim(fitnessFunction, lower, upper, DEoptim.control(NP=defaultNP, CR=defaultCR, F=tmp, maxIter= defaultIter, strategy=defaultStrategy)))
    tmp<-tmp+stepF
  }
  tmp<-minIter
  while(tmp<=maxIter)
  {
    resultsOfTests <- append(DEoptim(fitnessFunction, lower, upper, DEoptim.control(NP=defaultNP, CR=defaultCR, F=defaultF, maxIter= tmp, strategy=defaultStrategy)))
    tmp<-tmp+stepIter
  }
  tmp<-minStrategy
  while(tmp<=maxStrategy)
  {
    resultsOfTests <- append(DEoptim(fitnessFunction, lower, upper, DEoptim.control(NP=defaultNP, CR=defaultCR, F=defaultF, maxIter=defaultIter, strategy=tmp)))
    tmp<-tmp+stepStrategy
  }
}

## range of Axis (draw Planets Positions)
rangeOfAxis <- 5000
##
resultsOfTests <- list()

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
