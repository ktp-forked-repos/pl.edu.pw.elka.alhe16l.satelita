source("R/SolarSystem.R")

## Fitness Function
##first parameter is time in seconds after which simulation shuld start
##second parameter is angle of the shot between 0 and @ * PI
##velocity in km / s
fitnessFunction <- function(x) {
  time <- x[[1]]
  angle <- x[[2]]
  velocity <- x[[3]]
  startPlanetIndex <- 4
  endPlanetIndex <- 6
  rocketIndex <- 10
  sunIndex <- 1

  ##get planets and move simulation according to current time
  planets <- solarSystem$getPlanetsStart()
  moveSimulation(time, planets)

  ##launch rocket. add rocket as a new object into solar system
  startPlanet <- planets[[startPlanetIndex]]
  rocket <- solarSystem$getRocket(startPlanet, velocity, angle)
  planets <- c(planets, list(rocket))

  while(1) {
    ##get source, destination planets and sun
    endPlanet <- planets[[endPlanetIndex]]
    sun <- planets[[sunIndex]]
    rocket <- planets[[rocketIndex]]

    ##calculate distance between end planet and sun
    dist <- getDistance(rocket, endPlanet)
    solarDist <- getDistance(rocket, sun)

    if(solarDist > 10000 || dist <= endPlanet$radius) {
      ##we have found solution or we are to far from the sun to find better
      break
    }

    ##simulation steps
    planets <- moveSimulation(timeStep, planets)
    dist <- min(dist, getDistance(rocket, endPlanet))
  }

  return(dist)
}

## iteras simulation for s seconds
moveSimulation <- function(s, allPlanets) {
  ##iterować przez s sekund symulację w której jeden krok trwa timeStep
  while(s>0)
  {
    s <- s - timeStep

    ##policzenie nowych pozycji planet
    for(i in 1:length(allPlanets)) {
      planet <- allPlanets[[i]]
      ##dzielenie przez 1000000 jako przelicznik jednostek 10^6 km
      planet$x = planet$x + (planet$vx * timeStep / 1000000)
      planet$y = planet$y + (planet$vy * timeStep / 1000000)
      allPlanets[[i]] <- planet
    }

    ##policzenie nowych prędkości planet
    for(i in 1:(length(allPlanets)-1)) {
      for(j in (i+1):length(allPlanets)) {
        first <- allPlanets[[i]]
        second <- allPlanets[[j]]
        distance <- getDistance(first, second)
        angle <- getAngleBetween(first, second)
        force <- gravityConstant * first$mass * second$mass / distance / distance / 6046
        dv1 <- force / first$mass * timeStep
        dv2 <- force / second$mass * timeStep
        first$vx <- first$vx + sin(angle) * dv1
        first$vy <- first$vy + cos(angle) * dv1
        second$vx <- second$vx + sin(angle + PI) * dv2
        second$vy <- second$vy + cos(angle + PI) * dv2
        allPlanets[[i]] <- first
        allPlanets[[j]] <- second
      }
    }
  }
  return(allPlanets)
}

##returns distance between two planets in 10^6 km
getDistance <- function(o1, o2) {
  x <- o1$x - o2$x
  y <- o1$y - o2$y
  return(sqrt(x*x + y*y))
}

##returns angle between two planets
getAngleBetween <- function(planet1, planet2)
{
  x <- planet2$x - planet1$x
  y <- planet2$y - planet1$y
  return(atan2(x, y))
}

##constants
PI <- 3.14159265359
##time step in seconds used for calculating physical movement and forces between planets
timeStep <- 1000
##graivty constant described in m^3 / (kg * s^2)
gravityConstant <- 6.67408
