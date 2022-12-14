# notes ----
# Produce ice cover time series for Bering (55-64N, 180-160W)

# last updated: 2022/8/22
#Script written by Mike Litzow

#To do: Move this script to tidync!!! 
#Move to ORAS5 as source data: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-oras5?tab=overview
#Better resolution in earlier timeseries years 

# load ----

library(tidyverse)
library(ncdf4)
library(zoo)
library(maps)
library(mapdata)
library(chron)
library(fields)
library(oce)
    
    
##Process ------------------------
    
# note that there are separate time series for 1950-1978 and 1979-present
    
nc1 <- nc_open("./Data/ERA5_ice_1950-1978.nc")
    
ncvar_get(nc1, "time")   # hours since 1-1-1900
raw <- ncvar_get(nc1, "time")
h <- raw/24
d1 <- dates(h, origin = c(1,1,1900))
m1 <- months(d1)
yr1 <- years(d1)
    
x1 <- ncvar_get(nc1, "longitude")
y1 <- ncvar_get(nc1, "latitude")
    
ice1 <- ncvar_get(nc1, "siconc", verbose = F)
dim(ice1) # 87 long, 37 lat, 203 months
    
# reverse lat for plotting
ice1 <- ice1[,37:1,]
    
# reverse y too
y1 <- rev(y1)
    
ice1 <- aperm(ice1, 3:1)
    
ice1 <- matrix(ice1, nrow=dim(ice1)[1], ncol=prod(dim(ice1)[2:3]))
    
#plot to check
ice.mean <- colMeans(ice1, na.rm=T)
z <- t(matrix(ice.mean,length(y1)))
image.plot(x1,y1,z, col=oceColorsPalette(64), xlab = "", ylab = "")
contour(x1, y1, z, add=T)
map('world2Hires', c('usa', 'USSR'),  fill=T,add=T, lwd=1, col="lightyellow3")  
    
# now the second time series
nc2 <- nc_open("./Data/ERA5_ice_1979-2022.nc")
    
ncvar_get(nc2, "time")   # hours since 1-1-1900
raw <- ncvar_get(nc2, "time")
h <- raw/24
d2 <- dates(h, origin = c(1,1,1900))
m2 <- months(d2)
yr2 <- years(d2)
    
x2 <- ncvar_get(nc2, "longitude")
y2 <- ncvar_get(nc2, "latitude")
    
expver <-  ncvar_get(nc2, "expver", verbose = F)
expver # 1 and 5??
    
ice2 <- ncvar_get(nc2, "siconc", verbose = F)
dim(ice2) # 87 long, 37 lat, 2 expver, 203 months
    
# expver1 - this is ERA5
ice2 <- ice2[,,1,]
    
# reverse lat for plotting
ice2 <- ice2[,37:1,]
    
# reverse y too
y2 <- rev(y2)
    
ice2 <- aperm(ice2, 3:1)
    
ice2 <- matrix(ice2, nrow=dim(ice2)[1], ncol=prod(dim(ice2)[2:3]))
    
  
# plot to check
ice.mean <- colMeans(ice2, na.rm=T)
z <- t(matrix(ice.mean,length(y2)))
image.plot(x2,y2,z, col=oceColorsPalette(64), xlab = "", ylab = "")
contour(x2, y2, z, add=T)
map('world2Hires', c('usa', 'USSR'),  fill=T,add=T, lwd=1, col="lightyellow3")  
    
# check dimensions
identical(x1, x2)
identical(y1,y2)
    
# Keep track of corresponding latitudes and longitudes of each column:
lat <- rep(y1, length(x1))
lon <- rep(x1, each = length(y1))
    
ice <- rbind(ice1, ice2)
    
# drop E of 165 and N of 63
drop <- lon > -165 | lat > 63
ice[,drop] <- NA
    
# plot to check
ice.mean <- colMeans(ice, na.rm=T)
z <- t(matrix(ice.mean,length(y1)))
image.plot(x1,y1,z, col=oceColorsPalette(64), xlab = "", ylab = "")
contour(x1, y1, z, add=T)
map('world2Hires', c('usa', 'USSR'),  fill=T,add=T, lwd=1, col="lightyellow3") # perfecto
    
dimnames(ice) <- list(as.character(c(d1, d2)), paste("N", lat, "E", lon, sep=""))
    
f <- function(x) colMeans(x, na.rm = T)
    
m <- c(as.character(m1), as.character(m2))
    
yr <- c(as.numeric(as.character(yr1)), as.numeric(as.character(yr2)))
    
means <- data.frame(month = m,
                        year = as.numeric(as.character(yr)),
                        ice = rowMeans(ice, na.rm = T)) 
    
ggplot(means, aes(year, ice, color = month)) +
    geom_line()
    
# drop Oct - Dec
means <- means %>%
  filter(!month %in% c("Oct", "Nov", "Dec"))
    
# pivot wider
means <- means %>% 
      pivot_wider(values_from = ice, names_from = month) %>%
      filter(year %in% 1953:2022) # fixed values through 1952, 2022 incomplete!
    
means[,2:5] <- apply(means[,2:5], 2, scale)
    
plot <- means %>%
    pivot_longer(cols = -year)
    ggplot(plot, aes(year, value, color = name)) +
      geom_line()

# generate Jan and Mar-Apr means
means$Jan_ice <- apply(means[,2], 1, mean)
means$MarApr_ice <- apply(means[,4:5], 1, mean)

#####FOLLOW UP: Pull new data and use Jan-Feb average instead of just Jan!!!
    
# clean up
means <- means %>%
  select(year, Jan_ice, MarApr_ice)
    
plot <- means %>%
      pivot_longer(cols = -year)
    ggplot(plot, aes(year, value, color = name)) +
      geom_line()
 
# save 
write.csv(means, "./Output/seaice_output.csv", row.names = F)
