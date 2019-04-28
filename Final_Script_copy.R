library(raster)
library(rgdal)
library(ggplot2)
library(RStoolbox)
library(RColorBrewer)
library(reshape2)
library(gridExtra)

setwd("C:\\Users\\Basil Tufail\\Desktop\\Project")
#Loading raster bands
band2_2018 <- readGDAL("C:/Users/Basil Tufail/Desktop/Project/S211052018/T42RYT_20180511T054641_B02.jp2")
band8_2018 <- readGDAL("C:/Users/Basil Tufail/Desktop/Project/S211052018/T42RYT_20180511T054641_B08.jp2")
band11_2018 <- readGDAL("C:/Users/Basil Tufail/Desktop/Project/S211052018/T42RYT_20180511T054641_B11.jp2")
band2_2018 <- raster(band2_2018)
band8_2018 <- raster(band8_2018)
band11_2018 <- raster(band11_2018)
band11_2018 <- resample(band11_2018, band8_2018, method="bilinear")

band2_2017 <- readGDAL("C:/Users/Basil Tufail/Desktop/Project/S206052017/T42RYT_20170506T054641_B02.jp2")
band8_2017 <- readGDAL("C:/Users/Basil Tufail/Desktop/Project/S206052017/T42RYT_20170506T054641_B08.jp2")
band11_2017 <- readGDAL("C:/Users/Basil Tufail/Desktop/Project/S206052017/T42RYT_20170506T054641_B11.jp2")
band2_2017 <- raster(band2_2017)
band8_2017 <- raster(band8_2017)
band11_2017 <- raster(band11_2017)
band11_2017 <- resample(band11_2017, band8_2017, method="bilinear")

#Extracting AOI
Shp <- readOGR("C:/Users/Basil Tufail/Desktop/Project/Shapefiles/Area1.shp")
AOI <- spTransform(Shp, CRS(proj4string(band2_2018)))

band2_2018 <- mask(band2_2018, AOI)
band2_2018 <- crop(band2_2018, AOI)
band8_2018 <- mask(band8_2018, AOI)
band8_2018 <- crop(band8_2018, AOI)
band11_2018 <- mask(band11_2018, AOI)
band11_2018 <- crop(band11_2018, AOI)

band2_2017 <- mask(band2_2017, AOI)
band2_2017 <- crop(band2_2017, AOI)
band8_2017 <- mask(band8_2017, AOI)
band8_2017 <- crop(band8_2017, AOI)
band11_2017 <- mask(band11_2017, AOI)
band11_2017 <- crop(band11_2017, AOI)

#calculating NDVI
funndvi <- function(nir,red){(nir-red)/(nir+red)}
ndvi_2018 <- funndvi(band8_2018,band2_2018)
ndvi_2017 <- funndvi(band8_2017,band2_2017)
#plot(ndvi)
NDVI_plot_2018 <- ggR(ndvi_2018, geom_raster = T, ggObj = T)+
  scale_fill_gradientn(colours= brewer.pal(n=9,name="Greens"))+
  ggtitle("NDVI Map May 2018")
NDVI_plot_2017 <- ggR(ndvi_2017, geom_raster = T, ggObj = T)+
  scale_fill_gradientn(colours= brewer.pal(n=9,name="Greens"))+
  ggtitle("NDVI Map May 2017")

#calculating NDWI
funndwi <- function(nir,swir){(nir-swir)/(nir+swir)}
ndwi_2018 <- funndwi(band8_2018,band11_2018)
ndwi_2017 <- funndwi(band8_2017,band11_2017)
#plot(ndwi)
NDWI_plot_2018 <- ggR(ndwi_2018, geom_raster = T, ggObj = T)+
  scale_fill_gradientn(colours= brewer.pal(n=9,name="RdBu"))+
  ggtitle("NDWI Map May 2018")
NDWI_plot_2017 <- ggR(ndwi_2017, geom_raster = T, ggObj = T)+
  scale_fill_gradientn(colours= brewer.pal(n=9,name="RdBu"))+
  ggtitle("NDWI Map May 2017")

#calculating NDDI
funnddi <- function(ndvi,ndwi){(ndvi-ndwi)/(ndvi+ndwi)}
nddi_2018 <- funnddi(ndvi_2018,ndwi_2018)
nddi_2018 <- stretch(nddi_2018,minv =0.3,maxv =3,minq = 0.2,0.8)
nddi_2017 <- funnddi(ndvi_2017,ndwi_2017)
nddi_2017 <- stretch(nddi_2017,minv =0.3,maxv =3,minq = 0.2,0.8)
#plot(nddi)
NDDI_plot_2018 <- ggR(nddi_2018, geom_raster = T, ggObj = T)+
  scale_fill_gradientn(colours= brewer.pal(n=9,name="YlOrRd"))+
  ggtitle("NDDI Map May 2018")
NDDI_plot_2017 <- ggR(nddi_2017, geom_raster = T, ggObj = T)+
  scale_fill_gradientn(colours= brewer.pal(n=9,name="YlOrRd"))+
  ggtitle("NDDI Map May 2017")

#loading Vector file for fields 
vector_file <- readOGR("C:/Users/Basil Tufail/Desktop/Project/Shapefiles/Fields.shp")


#extracting mean NDVI for each field
list_2018 <- extract(ndvi_2018, vector_file, fun=mean)
list_2017 <- extract(ndvi_2017, vector_file, fun=mean)

#loading tiff with SAR Polarization
str_name1<-'C:/Users/Basil Tufail/Desktop/Project/20180510T132010.tif' 
Backscatter10052018 <- stack(str_name1)

str_name1<-'C:/Users/Basil Tufail/Desktop/Project/20170515T131937.tif' 
Backscatter <- stack(str_name1)

#extracting mean SAR backscatter for each field
List1_2018 <- extract(Backscatter10052018, vector_file, fun=mean)
List1_2017 <- extract(Backscatter, vector_file, fun=mean)

#Sorting list of values for 2018
vector_file@data <- cbind(vector_file@data,list_2018,List1_2018)
vector_file <- vector_file[apply(vector_file@data,1,FUN=function(x){!anyNA(x)}),]
vector_file_2018 <- vector_file

#changing header names and checking correlation for 2018
test <- (vector_file_2018@data);colnames(test) <- c("id","NDVI","VH","VV","VHVV")
c <- as.data.frame(cor(vector_file_2018@data)[2,]);c$variable <- c("id","NDVI","VH","VV","VHVV");colnames(c) <- c("correlation","variable")
testm <- melt(test,id.vars=c(1:2),measure.vars=c(3:5))
testm <- dplyr::left_join(testm,c,"variable")

#plotting correlation of SAR ploarizations with NDVI for 2018
Cor_plot_2018 <- ggplot(data=testm,aes(x=NDVI,y=value)) +
  geom_point(colour='red') +
  facet_wrap(~variable,scale='free_x')+
  geom_text(data=testm,mapping=aes(x = Inf, y = Inf,label=(paste("Correlation:",round(correlation,digits=3)))),
            hjust   = 1.05,
            vjust   = 1.5)+
  ggtitle("Correlation between NDVI and SAR backscatter, May 2018")

#Sorting list of values for 2017
vector_file <- readOGR("C:/Users/Basil Tufail/Desktop/Project/Shapefiles/Fields.shp")
vector_file@data <- cbind(vector_file@data,list_2017,List1_2017)
vector_file <- vector_file[apply(vector_file@data,1,FUN=function(x){!anyNA(x)}),]
vector_file_2017 <- vector_file

#changing header names and checking correlation for 2017
test <- (vector_file_2017@data);colnames(test) <- c("id","NDVI","VH","VV","VHVV")
c <- as.data.frame(cor(vector_file_2017@data)[2,]);c$variable <- c("id","NDVI","VH","VV","VHVV");colnames(c) <- c("correlation","variable")
testm <- melt(test,id.vars=c(1:2),measure.vars=c(3:5))
testm <- dplyr::left_join(testm,c,"variable")

#plotting correlation of SAR ploarizations with NDVI for 2017
Cor_plot_2017 <- ggplot(data=testm,aes(x=NDVI,y=value)) +
  geom_point(colour='red') +
  facet_wrap(~variable,scale='free_x')+
  geom_text(data=testm,mapping=aes(x = Inf, y = Inf,label=(paste("Correlation:",round(correlation,digits=3)))),
            hjust   = 1.05,
            vjust   = 1.5)+
  ggtitle("Correlation between NDVI and SAR backscatter, May 2017")

#Open a pdf file
pdf("Results.pdf") 

#Exporting Plots
grid.arrange(NDVI_plot_2018, NDVI_plot_2017, ncol=2)
grid.arrange(NDWI_plot_2018, NDWI_plot_2017, ncol=2)
grid.arrange(NDDI_plot_2018, NDDI_plot_2017, ncol=2)

Cor_plot_2018
Cor_plot_2017
# Close the pdf file
dev.off() 
