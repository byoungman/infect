# infect
 Simple generic infection models in R

## installation in R 
remotes::install_github('byoungman/infect')

## example: phytophthora cactorum (apple fruit)

temps <- 0:40
(tmin, tmax, topt) = (1, 35, 25)
resp <- tempresp(temp, 1, 35, 25)
plot(temps, resp, type = 'l', xlab = 'Temperature (deg. C)', ylab = 'Temperature response')

wetness <- reqwetdur(temps, 2, 5, 1, 35, 25)
plot(temps, wetness, type = 'l', xlab = 'Temperature (deg. C)', ylab = 'Wetness duration requirement (hours)')
