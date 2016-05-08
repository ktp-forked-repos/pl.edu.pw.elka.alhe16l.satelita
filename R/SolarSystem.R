##returns list o planets in start positions. each planet has following parametrs:
##radius radius of the planet described in kilometers * 10^6
##mass mass multipled by earths mass = 6.046 * 10^24kg
##distance distance from sun in kilometers multiplied by 10^6
##velocity velocity of the planet described in kilometers per second
##but it isn't the real value of valeicty - for some reason it doesn't work
##angle angle relative to sun, between 0 and and 2*PI
##name name of the planet
getPlanetsStart() <- function() {
  planets <- c(
    list(raiuds=69.5700, mass=330000, distance=0, velocity=0, angle=0, name="Sun"),
    list(radius=2.440, mass=0.06, distance=57.149340, velocity=2500, angle=0.33 * PI, name="Mercury"),
    list(radius=0.6052, mass=0.82, distance=108.939123, velocity=1865, angle=0.25 * PI, name="Venus"),
    list(radius=0.6378, mass=1, distance=149.879224, velocity=1600, angle=1.5 * PI, name="Earth"),
    list(radius=0.3396, mass=0.11, distance=227.339977, velocity=1279, angle=1.75 * PI, name="Mars"),
    list(radius=7.1492, mass=317.8, distance=778.009880, velocity=697, angle=1.4 * PI, name="Jupiter"),
    list(radius=6.0268, mass=95.2, distance=1426.283824, velocity=516, angle=1.66 * PI, name="Saturn"),
    list(radius=2.5559, mass=14.6, distance=2870.745626, velocity=363, angle=0.8 * PI, name="Uranus"),
    list(radius=2.4764, mass=17.2, distance=4498.350139, velocity=289, angle=0.4 * PI, name="Neptune")
  )
  return(planets)
}

##time step in seconds used for calculating physical movement and forces between planets
timeStep <- 10000

##graivty constant described in m^3 / (kg * s^2)
gravityConstant <- 6.67408
