source("R/SolarSystem.R")
source("R/Configuration.R")

## Fitness Function
##first parameter is time in seconds after which simulation shuld start
##second parameter is angle of the shot between 0 and @ * PI
##velocity in km / s
fitnessFunction <- function(x) {
  time <- x[0]
  angle <- x[1]
  velocity <- x[2]

  ##get planets and move simulation according to current time
  planets <- getPlanetsStart()
  moveSimulation(time, planets)

  ##get source and destination planets
  startPlanetName <- configuration$startPlanetName
  startPlanet <- getPlanet(planets, startPlanetName)
  endPlanetName <- configuration$endPlanetName
  endPlanet <- getPlanet(planets, endPlanetName);
  sun <- getPlanet(planets, "Sun")

  ##launch rocket
  rocket <- solarSystem$getRocket(startPlanet, velocity, angle)

  dist <- getDistance(rocket, endPlanet)
  solarDist <- getDistance(rocket, sun)
  while(solarDist < 5000 && dist > endPlanet$radius) {
    moveSimualtion(solarSystem$timeStep, planets)
    dist <- min(dist, getDistance(rocket, endPlanet))
  }

  return(dist)
}

## iteras simulation for s seconds
moveSimualtion <- function(s, planets) {
  while(s > 0)
  {
    s = s - timeStep
    ##TODO implement
    ##iterować przez s sekund symulację w której jeden krok trwa timeStep
  }
}

getPlanet <- function(allPlanets, planetName) {
  ##TODO implement może jest jakaś fajna metoda do przeszukiwania kolekcji?
  ##a jeśli nie to trzeba pętlę zrobić i wyszukać po nazwie z konfiguracji
}

getDistance <- function(o1, o2) {
  x <- o1$x - o2$x
  y <- o1$y - o2$y
  return(x*x + y*y)
}
