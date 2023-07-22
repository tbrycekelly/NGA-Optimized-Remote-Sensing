library(openxlsx)
library(TheSource)

npp = read.xlsx('Data/NPP/NPP.xlsx', startRow = 2)

## Fix missing entries
for (i in 2:nrow(npp)) {
  for (j in 1:ncol(npp)) {
    if (is.na(npp[i,j])) {
      npp[i,j] = npp[i-1,j]
    }
  }
}

# Fix excel datetime stamps
npp$Start.Date = TheSource::conv.time.excel(npp$Start.Date, tz = 'US/Anchorage')

# Fix effective PAR based on incident PAR and lightlevel 
# TODO Usually only ~50% of incident surface radiation enters the surface ocean water (effective albeito ~ 0.5),
#      but I need to check this against how other people do it.
npp$PAR = npp$PAR * npp$Light.Level


# let's visual NPP profiles at a particular station
l = which(npp$Station == 'GAK15')

plot(NULL,
     NULL,
     xlim = c(0, 150),
     ylim = c(100,0),
     xlab = 'NPP (mg C m-3 d-1)',
     ylab = 'Depth (m)')

for (cast in unique(npp$Cast[l])) {
  k = which(npp$Cast == cast & npp$Station == 'GAK15')
  lines(npp$Prod[k], npp$Depth[k], lwd = 3)
  
}



## Okay, let's look for some preliminary relationships
plot(npp$PAR, npp$Prod)
plot(npp$Chl, npp$Prod)
plot(npp$Temp, npp$Prod)

# VPGM (vertically integrated):  pb * Chl.integrated * PAR.incident * day.length * zeu
# VGPM (at one depth) : pb * Chl.instu * PAR.insitu * day.length
# TODO: Check units on all coefficients! 

cal_dayL = function(lat, date){
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

sst = npp$Temp
pb = 1.2956 +
  2.749e-1 * sst +
  6.17e-2 * sst^2 -
  2.05e-2 * sst^3 +
  2.462e-3 * sst^4 -
  1.348e-4 * sst^5 +
  3.4132e-6 * sst^6 - 
  3.27e-8 * sst^7

irr = 0.66125 * npp$PAR / (npp$PAR + 4.1)
day.length = cal_dayL(59, npp$Start.Date)

npp$pred = pb * npp$Chl * irr * day.length

plot(log10(npp$pred),
     log10(npp$Prod),
     xaxt = 'n',
     yaxt = 'n',
     xlab = 'Predicted NPP (mg C m-3 d-1)',
     ylab = 'Measured NPP (mg C m-3 d-1)')

add.log.axis(1, grid.major = T)
add.log.axis(2, grid.major = T)
abline(a = 0, b = 1)
