source("SolarSystem.R")

## Fitness Function
##first parameter is time in seconds after which simulation shuld start
##second parameter is angle of the shot between 0 and @ * PI
##velocity in km / s
fitnessFunction <- function(x) {
  time <- x[0]
  angle <- x[1]
  velocity <- x[2]

  planets <- getPlanetsStart()
  moveSimulation(time, planets)

  ##dodaj rakietę do planet

  ##while !warunek stopu
  ##moveSimulation
  ##sprawdz odleglosc satelity od planety docelowej

  ##zwróć najmniejsza odległość w trakcie symulacji
}

## iteras simulation for s seconds
moveSimualtion <- function(s, planets) {
  ##TODO implement
  ##iterować przez s sekund symulację w której jeden krok trwa timeStep
}
