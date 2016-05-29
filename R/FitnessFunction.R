source("R/SolarSystem.R")
source("R/Tests.R")

## Fitness Function
##first parameter is time in days after which simulation should start
##second parameter is angle of the shot between 0 and 2 * PI
##velocity in km / s
##requires 'planetsPerDay' list in global scope which contains planets state per day
fitnessFunction <- function(x) {
  day <- x[[1]]
  angle <- x[[2]]
  velocity <- x[[3]]
  startPlanetIndex <- 4
  endPlanetIndex <- 9
  rocketIndex <- 10
  sunIndex <- 1

  ##get planets for current day
  planets <- planetsPerDay[[day]]

  ##launch rocket. add rocket as a new object into solar system
  endPlanet <- planets[[endPlanetIndex]]
  startPlanet <- planets[[startPlanetIndex]]
  sun <- planets[[sunIndex]]
  rocket <- solarSystem$getRocket(startPlanet, velocity, angle)
  planets <- c(planets, list(rocket))
  ##calculate distance between end planet and rocket
  dist <- getDistance(rocket, endPlanet)
  solarDist <- getDistance(rocket, sun)

  ##iterations untill next planets position draw
  drawIndex <- 0
  ## flag - drawing planet position
  drawPlanetFlag <- TRUE

  secondsToRun <- (daysMax - day) * secondsPerDay
  ##until we are still in solar system and we haven't reached endplanet and time hasn't run off
  while(solarDist < solarDistMax && dist > endPlanet$radius && secondsToRun > 0) {
    if(drawPlanetFlag){
      if(drawIndex==0){
        drawIndex<-3
        drawPlantesPositions(planets)
      }
      else{
        drawIndex<-drawIndex-1
      }
    }

    ##simulation steps
    planets <- moveSimulation(timeStep, planets)
    secondsToRun <- secondsToRun - timeStep

    ##get source, destination planets and sun
    endPlanet <- planets[[endPlanetIndex]]
    sun <- planets[[sunIndex]]
    rocket <- planets[[rocketIndex]]
    dist <- min(dist, getDistance(rocket, endPlanet))
    solarDist <- getDistance(rocket, sun)
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


##preprocessing - calculate planets positions per day for a year
getPlanetsPerDay <- function(days) {
  planets <- solarSystem$getPlanetsStart()
  planetsPerDay <- list(planets)
  while(days > 0) {
    planets <- moveSimulation(secondsPerDay, planets)
    planetsPerDay <- c(planetsPerDay, list(planets))
    days <- days - 1
  }
  return(planetsPerDay)
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

##number of seconds per day
secondsPerDay <- 60*60*24;
##time step in seconds used for calculating physical movement and forces between planets
##should divie secondsPerDay - here it is one hour
timeStep <- 60*60
##graivty constant described in m^3 / (kg * s^2)
gravityConstant <- 6.67408
##maximum acceptable distance from sun in km * 10^6
solarDistMax <- 5000
##maximum number of days for simulation
daysMax <- 365
##planets per each day
planetsPerDay <- getPlanetsPerDay(daysMax)




