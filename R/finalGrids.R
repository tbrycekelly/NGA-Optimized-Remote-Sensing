## Make Grids.R ### Purpose: Takes data and makes grids for parameters ##
## Last modified: 2023-08-07 # Last: Plot NPP and integrated NPP ##

## \(^.^)/ "work first, then make it work well" ## 

library(TheSource)
library(reshape2)

glider = readRDS('directory.rds')

times = seq(min(glider$datetime), make.time(2023, 5, 7), by = '6 hour')
dt = 6
depths = seq(0, 140, by = 5)-2.5


grid = build.section(x = as.numeric(glider$datetime),
                     y = glider$Depth,
                     z = glider[,c('temp', 'Chl', 'PAR','preNPP')],
                     field.names = c('temp', 'Chl', 'PAR','preNPP'),
                     lat = glider$Lat,
                     lon = glider$Lon,
                     gridder = gridBin,
                     xlim = as.numeric(range(times)),
                     ylim = range(depths),
                     nx = length(times),
                     ny = length(depths))

grid$grid$x = conv.time.unix(grid$gVierid$x)##       .｡oO(grids,grids,grids)
grid$x = conv.time.unix(grid$x) ##            -(´-`)-

saveRDS(grid, 'Data/Grid.RDS')

# (>*3*)> Arrays used for grids
PAR = array(grid$grid$PAR, dim = c(grid$grid.meta$nx, grid$grid.meta$ny))
rChl = array(grid$grid$Chl, dim = c(grid$grid.meta$nx, grid$grid.meta$ny))
rTemp = array(grid$grid$temp, dim = c(grid$grid.meta$nx, grid$grid.meta$ny))
rPAR = array(NA, dim = c(grid$grid.meta$nx, grid$grid.meta$ny))
dayL = array(NA, dim = c(grid$grid.meta$nx, grid$grid.meta$ny))
NPP = array(NA, dim = c(grid$grid.meta$nx, grid$grid.meta$ny))

## GAP FILL HERE
## use (⌐▨_▨): arr <- gapFill(arr)
dms = dim(NPP)
gapFill = function(arr) {
  for(i in 1:dms[1]) {
    for(j in 2:(dms[2]-1)) {
      if (is.na(arr[i,j])) {
        arr[i,j] = (arr[i,j+1] + arr[i,j-1])/2
      }
    }
    if(is.na(arr[i,2])) {
      arr[i,2] <- arr[i,3] * (arr[i,3]/arr[i,4])
    }
    if(is.na(arr[i,1])) {
      arr[i,1] <- arr[i,2] * (arr[i,2]/arr[i,3])
    }
  }
  arr
} 

## CALC DAY HOURS HERE
cdayL = function(lat, date) {
  gamma = lat / 180.0 * pi
  psi = get.julian(date) / 365.0 * 2.0 * pi
  solarDec = (0.39637 - 22.9133 * cos(psi) + 4.02543 * sin(psi) - 
                0.38720 * cos(2*psi) + 0.05200 * sin(2*psi)) * pi / 180.0
  r = -tan(gamma) * tan(solarDec)
  
  L = 24.0 * acos(r) / pi
  L[r <= -1] = 24
  L[r > 1] = 0
  
  L
}

# Filter low PAR Values + gap fill 
quantile(as.numeric(PAR), probs = c(0.74, 0.75, 0.8), na.rm = T)
plot(ecdf(as.numeric(PAR)))
ecdf(as.numeric(PAR))(10)

# gapFill for PAR -> fills rPAR array -> plot rPAR
PAR <- gapFill(PAR)

for(i in 1:dms[1]) {
  if(!is.na(PAR[i,1]) & PAR[i,1] < 0) {
    PAR[i,1] <- PAR[i,2]
  }
  if(max(PAR[i,], na.rm = T) <= 0) {
    PAR[i,] <- 0
  }
  rPAR[i,] = round(PAR[i,]/max(PAR[i,],na.rm = T), 3)
  for(j in 2:(dms[2])) {
    if(!is.na(rPAR[i,j]) & rPAR[i,j] < 0) {
      rPAR[i,j] <- 0
    }
  }
}

plot.image(x = conv.time.unix(grid$x), y = grid$y, z = PAR,
           ylab = 'Depth (m)', xlab = '',
           ylim = c(140,0), zlim = c(-1,15),
           xlim = tlim, xaxt = 'n',
           pal = 'ocean.ice',
           main = 'PAR')
axis.POSIXct(1, at = taxis, format = '%b %d', las = 1)

# fill and grid adjustment for Chl
rChl <- gapFill(rChl)

plot.image(x = conv.time.unix(grid$x), y = grid$y, z = log10(rChl),
           ylab = 'Depth (m)', xlab = '',
           ylim = c(140,0), zlim = c(-1.5,1.5), 
           xlim = tlim, rev = T, xaxt = 'n',
           pal = 'ocean.algae',
           main = 'Chl-a')
axis.POSIXct(1, at = taxis, format = '%b %d', las = 1)

# fill and grid adjustment for Temperature
rTemp <- gapFill(rTemp)

plot.image(x = conv.time.unix(grid$x), y = grid$y, z = rTemp,
           ylab = 'Depth (m)', xlab = '',
           ylim = c(140,0), zlim = c(0,8), rev = F,
           xlim = tlim, xaxt = 'n',
           pal = 'ocean.gray',
           main = 'Temperature')
axis.POSIXct(1, at = taxis, format = '%b %d', las = 1)

# fill array representative of Pbo | function of temperature
sst = grid$grid$temp
grid$grid$Pbo = 1.2956 +
  2.749e-1 * sst +
  6.17e-2 * sst^2 -
  2.05e-2 * sst^3 +
  2.462e-3 * sst^4 -
  1.348e-4 * sst^5 +
  3.4132e-6 * sst^6 - 
  3.27e-8 * sst^7
aPbo = array(grid$grid$Pbo, dim = c(grid$grid.meta$nx, grid$grid.meta$ny))
aPbo <- gapFill(aPbo)

# surface PAR plot
sp = rep(NA, dms[1])
for (i in 1:dms[1]) {
    sp[i] = PAR[i,1]
}
plot(sp, type = 'line',
     ylab = 'Surface PAR')

# daylight
daylight = rep(NA, length(times))

for (i in 1:length(times)) {
  l = i:round(i+24/dt)
  daylight[i] = mean(PAR[l,1], na.rm = T)
}

# daily PAR plot
dPAR = rPAR
for (i in 1:dms[2]) {
  dPAR[,i] = daylight * dPAR[,i]
}

plot.image(x = conv.time.unix(grid$x), y = grid$y, z = log10(dPAR + 0.01),
           ylab = 'Depth (m)', xlab = '',
           ylim = c(140,0), zlim = c(-2,2), xlim = tlim,
           xaxt = 'n', 
           pal = 'ocean.speed', rev = T,
           main = 'Daily PAR')
axis.POSIXct(1, at = taxis, format = '%b %d', las = 1)


# approx plots for Lat and Lon ~(Q m Q)>
pLat = approx(grid$x, grid$section.lat)
plot(pLat)
pLon = approx(grid$x, grid$section.lon)
plot(pLon)

#                .｡oO(now we are getting somewhere)
# NPP plot -(´∀`)b
irr = 0.66125 * dPAR / (dPAR + 4.1)

for(i in 1:dms[1]) {
  for (j in 1:(dms[2])) {
    NPP[i,j] <- rChl[i,j] * cdayL(grid$section.lat[i], times[i]) * aPbo[i,j] * irr[i,j] 
  }
}

plot.image(x = conv.time.unix(grid$x), y = grid$y, z = log10(NPP+0.01),
           ylab = 'Depth (m)', xlab = '',
           ylim = c(140,0), zlim = c(-2.5,2.5), rev = F,
           xlim = tlim, xaxt = 'n',
           pal = 'ocean.deep',
           main = 'NPP')
axis.POSIXct(1, at = taxis, format = '%b %d', las = 1)


## Vertical integration for NPP, CHL, etc:
times = c(times, times[321] + 3600*6)
tlim = c(times[1], times[308])

plot(times,
     5 * apply(rChl, 1, function(x) { sum(x, na.rm = T) }),
     type = 'l',
     lwd = 3,
     ylab = '---',
     ylim = c(0, 140),
     yaxs = 'i',
     xlab = '',
     xlim = tlim,
     xaxt = "n",
     xaxs = 'i',
     col = '#03C988'
     )

grid(nx =NA, ny = 7)
box()
taxis = seq(times[1], times[300], by = '7 days')
axis.POSIXct(3, at = taxis, format = '%b %d', las = 1)


plot(times,
     TheSource::calc.sma(5 * apply(NPP, 1, function(x) { sum(x, na.rm = T) }), 4),
     type = 'l',
     lwd = 3,
     ylim = c(0, 1100),
     yaxs = 'i',
     ylab = 'Vertically Integrated NPP',
     xlab = '',
     xlim = tlim,
     xaxs = 'i',
     xaxt = 'n',
     col = '#AA77FF'
)
grid(nx =NA, ny = 7)
box()
taxis = seq(times[1], times[300], by = '7 days')
axis.POSIXct(3, at = taxis, format = '%b %d', las = 1)


# plots for dayligh

plot(times, TheSource::calc.sma(daylight,3), type = 'l',
     lwd = 3,    
     yaxs = 'i',
     ylab = 'Daylight',
     xlab = '',
     xlim = tlim,
     xaxs = 'i',
     col = '#FFAC1C',
     xaxt = 'n'
)
grid(nx =NA, ny = 7)
box()
taxis = seq(times[1], times[300], by = '7 days')
axis.POSIXct(3, at = taxis, format = '%b %d', las = 1)


