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
plot(chirps_hnd[[1]])
zoom(chirps_hnd[[1]])

chirps_hnd2=stack("C:\\Users\\lllanos\\Documents/honduras_chirps.tif")
plot(chirps_hnd2[[1]])



##########################
monthly_precip = read.table("clipboard", header = T)
coord = data.frame("lon"=c(-87.1589,-88.783),"lat" = c(13.40806,14.783))
sitios = as.data.frame(t(raster::extract(x=chirps_hnd, y=coord, method = 'bilinear')))
sitios$date = seq(as.Date("1981/01/01"),as.Date("2017/03/01"),"month")
names(sitios)[1:2] = c("choluteca_c","santa_rosa_c")

library(reshape2)
sitios[,4:5] = monthly_precip[13:(nrow(monthly_precip)-2),3:4]    
santa_rosa = melt(sitios[,c(2,3,4)], id.vars = "date")
choluteca = melt(sitios[,c(1,3,5)], id.vars = "date")
all = data.frame(santa_rosa, choluteca[,3])
names(all) = c("date", "Origen", "Santa Rosa de Copán", "Choluteca")
all$Origen = ifelse(all$Origen=="santa_rosa_c", "CHIRPS", "Observado")
all =melt(all, id.vars = c("date", "Origen"))
cor(xx[,3:4],sitios[1:2],use = "pairwise.complete.obs")

library(ggplot2)

ggplot(all, aes(date, value, color = Origen)) + geom_line() +facet_grid(~variable)+theme_bw()+
  ylab("Precipitación (mm/mes)") + xlab(" ") + scale_color_manual(breaks = c("CHIRPS", "Observado"),values=c("black", "red"))


ggplot(all, aes(x= Origen, y=value, fill = Origen)) + geom_boxplot() +facet_grid(~variable)+theme_bw()+
  ylab("Precipitación (mm/mes)") + xlab(" ")

x11()
par(mfrow=c(2,1))
plot(sitios[,3],xx[,3],type="l",lwd=1.6, main = "Estación Santa Rosa de Copán", xlab = " ", ylab="Precipitación (mm/mes)")
lines(sitios[,3],sitios[,2],col="red",lty = 2,lwd=1.5)

plot(sitios[,3],xx[,4],type="l",lwd=1.6, main = "Estación Choluteca", xlab = " ", ylab="Precipitación (mm/mes)")
lines(sitios[,3],sitios[,1],col="red",lty = 2,lwd=1.5)

boxplot()
