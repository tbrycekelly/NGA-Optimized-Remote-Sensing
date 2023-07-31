library(R.matlab)
library(openxlsx)

## Read in MAT object -- Data we want is in the sci entry:
g191 = readMat('Data/Glider/g191_gak6_data.mat')
g191 = g191$sci

## Construct a dataframe from the raw lists
glider = as.data.frame(g191[[1]])
for (i in 2:16) {
  glider = cbind(glider, as.data.frame(g191[[i]]))
}

# Get colnames from the mat file
colnames(glider) = dimnames(g191)[[1]][1:16]

## Convert and reformat as necessary
glider$time = conv.time.matlab(glider$time)
glider = data.frame(Lon = glider$lon,
                    Lat = glider$lat,
                    Depth = glider$depth,
                    Chl = glider$chl,
                    temp = glider$temp,
                    PAR = glider$par,
                    datetime = glider$time,
                    NPP = NA)


## Filter
glider = glider[!is.na(glider$Lon),]
glider = gldier[!is.na(glider$datetime),]
glider$PAR <- glider$PAR *(86400/10^6) ## PAR unit conversion


## Save as both xlsx (for viewing) and rds (for reloading.)
openxlsx::write.xlsx(glider, 'Data/Glider/Full Data.xlsx')
saveRDS(glider, 'Data/Glider/Full Data.rds')

