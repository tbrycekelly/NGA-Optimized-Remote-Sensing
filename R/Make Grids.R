library(TheSource)


glider = readRDS('Data/Glider/Full Data.rds')


times = seq(min(glider$datetime), make.time(2023, 5, 7), by = '4 hour')
depths = seq(0, 140, by = 5)-2.5


grid = build.section(x = as.numeric(glider$datetime),
                     y = glider$Depth,
                     z = glider[,c('temp', 'Chl', 'PAR')],
                     field.names = c('temp', 'Chl', 'PAR'),
                     lat = glider$Lat,
                     lon = glider$Lon,
                     gridder = gridBin,
                     xlim = as.numeric(range(times)),
                     ylim = range(depths),
                     nx = length(times),
                     ny = length(depths))

grid$grid$x = conv.time.unix(grid$grid$x)
grid$x = conv.time.unix(grid$x)

plot.section(grid,
             ylim = c(150,0),
             pal = 'ocean.thermal',
             ylab = 'Depth (m)',
             xlab = '',
             field = 'temp',
             zlim = c(2,10))

plot.section(grid,
             ylim = c(150,0),
             pal = 'ocean.algae',
             ylab = 'Depth (m)',
             xlab = '',
             field = 'Chl',
             log = T,
             zlim = c(-1, 1))

plot.section(grid,
             ylim = c(50,0),
             pal = 'ocean.solar',
             ylab = 'Depth (m)',
             xlab = '',
             field = 'PAR',
             log = F, 
             zlim = c(0,200))



