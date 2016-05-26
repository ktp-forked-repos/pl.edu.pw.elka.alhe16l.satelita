source("R/FitnessFunction.R")

##ograniczenia na time angle velocity
##czas w sekundach 1 rok, prędkość wyznaczona na oko
lower <- c(0, 0, 2000);
upper <- c(30000000, 2 * PI, 5000)

set.seed(1234)
# outDEoptim <- DEoptim(fitnessFunction, lower, upper)
#
#
#
#
#
# ##poniżej jakieś wykresy które były w pliku tutoriala z heurystyki DEoptim
#
# ## print output information
# summary(outDEoptim)
# ## plot the best members
# plot(outDEoptim, type = 'b')
# ## plot the best values
# dev.new()
# plot(outDEoptim, plot.type = "bestvalit", type = 'b', col = 'blue')
# ## rerun the optimization, and store intermediate populations
# outDEoptim <- DEoptim(Rosenbrock, lower, upper,
#                       DEoptim.control(itermax = 500,
#                                       storepopfrom = 1, storepopfreq = 2, trace = TRUE))
# summary(outDEoptim)
# ## plot intermediate populations
# dev.new()
# plot(outDEoptim, plot.type = "storepop")
