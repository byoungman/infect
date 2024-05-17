# infect
 Simple generic infection models in R

## installation in R 
```{r}
remotes::install_github('byoungman/infect')
```

## example: phytophthora cactorum (apple fruit)

```{r}
temps <- 0:40
resp <- tempresp(temps, 1, 35, 25) # (tmin, tmax, topt) = (1, 35, 25)
plot(temps, resp, type = 'l', xlab = 'Temperature (deg. C)', ylab = 'Temperature response')
```

```{r}
wetness <- reqwetdur(temps, 2, 5, 1, 35, 25)
plot(temps, wetness, type = 'l', xlab = 'Temperature (deg. C)', ylab = 'Wetness duration requirement (hours)')
```
