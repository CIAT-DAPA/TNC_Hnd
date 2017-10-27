library(raster)
library(ncdf4)
library(maptools)
library(rgdal)

chirps_all = stack("C:\\Users\\lllanos\\Downloads\\data_hnd.nc")
plot(chirps_all[[1]])

IT <- getData(name = "GADM", country = "Honduras", level = 1)

plot(hnd_admin, add=T)
plot(IT, add=T)

m = mask(chirps_all,IT)

chirps_hnd = crop(x = m, y =chirps_all )

writeRaster(chirps_hnd,"honduras_chirps.tiff")
data_hnd = t(rasterToPoints(chirps_hnd2))
write.csv(data_hnd,"honduras_chirps_data.csv",quote = F)
plot(chirps_hnd)
zoom(chirps_hnd[[1]])

chirps_hnd2=stack("C:\\Users\\lllanos\\Documents/honduras_chirps.tif")
plot(chirps_hnd2[[1]])



##########################

coord = data.frame("lon"=c(-87.1589,-88.783),"lat" = c(13.40806,14.783))
sitios = as.data.frame(t(raster::extract(x=chirps_hnd, y=coord, method = 'bilinear')))
sitios$date = seq(as.Date("1981/01/01"),as.Date("2017/03/01"),"month")
names(sitios)[1:2] = c("choluteca_c","santa_rosa_c")

library(lubridate)
xx = monthly_precip[13:(nrow(monthly_precip)-2),]                       
cor(xx[,3:4],sitios[1:2],use = "pairwise.complete.obs")


plot(xx[,3],type="l")
lines(sitios[,2],col="red")
plot(xx[,4],type="l")
lines(sitios[,1],col="red")

