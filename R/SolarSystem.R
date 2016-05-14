solarSystem <- list(
  ##returns list o planets in start positions. each planet has following parametrs:
  ##radius radius of the planet described in kilometers * 10^6
  ##mass mass multipled by earths mass = 6.046 * 10^24kg
  ##distance distance from sun in kilometers multiplied by 10^6
  ##velocity velocity of the planet described in kilometers per second
  ##but it isn't the real value of valeicty - for some reason it doesn't work
  ##angle angle relative to sun, between 0 and and 2*PI
  ##name name of the planet
  getPlanetsStart=function() {
    getPlanet <- function(radius, mass, distance, velocity, angle, name) {
      x <- distance * sin(angle);
      y <- distance * cos(angle);
      vx <- velocity * cos(angle);
      vy <- velocity * -sin(angle);
      return(list(radius=radius, mass=mass, x=x, y=y, vx=vx, vy=vy, name=name))
    }
    planets <- list(
      getPlanet(69.5700, 330000, 0, 0, 0, "Sun"),
      getPlanet(2.440, 0.06, 57.149340, 2500, 0.33 * PI, "Mercury"),
      getPlanet(0.6052, 0.82, 108.939123, 1865, 0.25 * PI, "Venus"),
      getPlanet(0.6378, 1, 149.879224, 1600, 1.5 * PI, "Earth"),
      getPlanet(0.3396, 0.11, 227.339977, 1279, 1.75 * PI, "Mars"),
      getPlanet(7.1492, 317.8, 778.009880, 697, 1.4 * PI, "Jupiter"),
      getPlanet(6.0268, 95.2, 1426.283824, 516, 1.66 * PI, "Saturn"),
      getPlanet(2.5559, 14.6, 2870.745626, 363, 0.8 * PI, "Uranus"),
      getPlanet(2.4764, 17.2, 4498.350139, 289, 0.4 * PI, "Neptune")
    )
    return(planets)
  },

  ##returns a rocket that starts on specified planet in current time
  ##with specified velocity and angle
  getRocket=function(startPlanet, velocity, angle) {
    x <- (startPlanet$x + startPlanet$radius) * cos(angle);
    y <- (startPlanet$y + startPlanet$radius) * sin(angle);
    vx <- velocity * cos(angle);
    vy <- velocity * sin(angle);
    return(list(mass=1.0e-18,x=x, y=y, vx=vx, vy=vy, name=""))
  }
)
