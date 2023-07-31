library(TheSource)


glider = readRDS('Data/Glider/Full Data.rds')


times = seq(min(glider$datetime), max(glider$datetime), by = '1 hour')
depths = seq(0, 100, by = 5)


grid = build.section(x = as.numeric(glider$datetime),
                     y = glider$Depth,
                     z = glider$temp,
                     gridder = gridBin,
                     xlim = as.numeric(range(times)),
                     ylim = c(0, 100),
                     nx = length(times),
                     ny = length(depths))


