### DPI gridding example
# 1. Load the data
# 2. Make a grid
# 3. Plot the grid
library(SimpleGridder)

## Step 1
dpi = openxlsx::read.xlsx('Data/DPI/Transect 2022-07-12 075941.xlsx')


## Step 2
# Depth = y; distance = x
summary(dpi$Depth)


grid = buildGrid(xlim = c(0,56), ylim = c(0,100), nx = 100, ny = 100)
grid = setGridder(grid, neighborhood = 10)
grid = appendData(grid, dpi$Distance, dpi$Depth, dpi$Temperature, 'temperature')
grid = appendData(grid, dpi$Distance, dpi$Depth, dpi$Salinity, 'salinity')

grid = interpData(grid)


## Step 3
plotGrid(grid, 'salinity', pal = pals::inferno(16))
plotGrid(grid, 'temperature', pal = pals::inferno(16), ylim = c(100,0))

points(grid$data$x, grid$data$y, pch ='.')


#############



x = runif(20, min = 0, max = 10)

y = x^0.5 + runif(20, min = -2, max = 2)


plot(x, y)
model = lm (y ~ x)


dat = data.frame(m = rep(NA, 1000), b = NA)

for (i in 1:nrow(dat)) {
  l = sample(1:length(x), replace = T)
  model = lm (y[l] ~ x[l])
  dat$m[i] = coefficients(model)[2]
  dat$b[i] = coefficients(model)[1]
}

pred = 8 * dat$m + dat$b
points(rep(8, 100), pred, pch = 20)

hist(pred)
mean(pred)
sd(pred)

quantile(pred, probs = c(0.025, 0.975))
mean(pred) + 1.98 * sd(pred)
mean(pred) - 1.98 * sd(pred)

chl = 0.2 +/- 0.25


abline(model)
summary(model)

xx = c(0:10)
yy = 1.05 * xx - 0.222
lines(xx, yy, col = 'black', lwd = 3)

yy = (1.05 + 0.09) * xx - 0.222
lines(xx, yy, col = 'red', lwd = 3)

yy = (1.05 - 0.09) * xx - 0.222
lines(xx, yy, col = 'red', lwd = 3)

yy = 1.05 * xx - 0.222 + 0.5
lines(xx, yy, col = 'red', lwd = 3)

yy = 1.05 * xx - 0.222 - 0.5
lines(xx, yy, col = 'red', lwd = 3)




