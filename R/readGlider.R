## readGlider.R ## Purpose : place glider data into workable data frame ###
## Last Modified : 2023-07-21 
### (1) raw csv into data frame (2) modify data for xlsx conversion (3) load
###modfied file and make workable if already have mod file - can skip to step 3 
library(pacman)
pacman::p_load (     
  data.table, 
  TheSource,
  lubridate,
  openxlsx,
  berryFunctions
)

# (1) load in raw data
g191 <- read.csv('C:\\Users\\lenovo\\Desktop\\xw\\GOAdata\\goa191data.csv', header = F, 
                 skip = 3)
colnames(g191) <- c('Year','Month','Day','Hour','Min','Sec','Lon','Lat','Depth','Temperature',
                    'Salinity','Density','O2','Bbp','Chl-a','PAR')
g191 <- g191[!is.na(g191$Depth),]
g191 <- g191[!is.na(g191$Lat),]

# (2) modify data for xlsx convert | conversion below
glider <- data.frame(Lon = as.numeric(g191$Lon),
                     Lat = as.numeric(g191$Lat),
                     Depth = as.numeric(g191$Depth),
                     Chl = as.numeric(g191$`Chl-a`),
                     temp = as.numeric(g191$Temperature),
                     PAR = as.numeric(g191$PAR))
glider$date <-  as.Date(with(g191,paste(g191$Year, g191$Month, g191$Day,sep = "-")),
                      "%Y-%m-%d")
glider$NPP <- NA
glider <- insertRows(glider, 1, new = NA)  # ^ format purposes

# write simplified glider data in xlsx
xlsx::write.xlsx(glider, 
                 "g191mod.xlsx",
                 col.names = TRUE,
                 row.names = TRUE,
                 sheetName = "UAFg191")

## (3) load in readeable glider data file | make workable
g191mod <-read.xlsx('C:/Users/lenovo/Desktop/xw/VGPMproject/g191mod.xlsx', startRow = 2)
g191mod <- g191mod[,-1]      # see L32
colnames(g191mod) <- c('Longitude','Latitude','Depth','Chl','Temp','PAR','Dates')
g191mod$Dates <- TheSource::conv.time.excel(g191mod$Dates,)
g191mod$preNPP <- NA  # preNPP = predicted NPP (if wanted)
