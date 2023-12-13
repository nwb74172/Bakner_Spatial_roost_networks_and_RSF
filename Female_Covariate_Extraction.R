library(dplyr)
library(raster)
# Read in used and available points
df <- read.csv("E:/Roost_Chapter/Use_Avail_Final_8-3.csv")

df2 <- subset(df, Use==0)
df <- subset(df, Use==1)

# Make ID_Attempt column ID, x Lon, y Lat
df2$Lon <- df2$x
df2$Lat <- df2$y

# Add site and yr to the random points
df2$Site <- df$Site[sapply(df2$ID, function(x) match(x, df$ID))]

df2$yr <- df$yr[sapply(df2$ID, function(x) match(x, df$ID))]

# Use bind_rows to stack the data frames vertically
df <- bind_rows(df, df2)

# Read in habitat features
feature <- raster("E:/Roost_Chapter/Feature_Rasters/Elevation.tif")
x <- terrain(feature, opt=c('slope', 'aspect', 'roughness'), unit='degrees')

# Make a spatial data frame
df.sp <- df

coordinates(df.sp) <- c("Lon","Lat")

# Define crs for df.sp
# Louisiana
#proj4string(df.sp) <- CRS("+proj=utm +zone=15N ellps=WGS84")
# WEBB SRS CC 
proj4string(df.sp) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# subset out years
srs <- subset(df.sp, Site=="SRS")

# subset out years
srs21 <- subset(srs, yr==2021)

# extract covariates
proads <- raster("E:/Roost_Chapter/landscape_co/landscape_co/SRS/Roads/primary_rd1.tif")
proads <- projectRaster(proads, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sroads <- raster("E:/Roost_Chapter/landscape_co/landscape_co/SRS/Roads/secondary_rd1.tif")
sroads <- projectRaster(sroads, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/SRS/dist_open_srs21.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/SRS/dist_water_21.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
feature <- projectRaster(feature, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
x <- projectRaster(x, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

srs$proads <- raster::extract(proads, srs) 
srs$sroads <- raster::extract(sroads, srs)
srs$dist_open <- raster::extract(dist_open, srs)
srs$dist_water <- raster::extract(dist_water, srs)
srs$elevation <- raster::extract(feature, srs)
srs$feature <- raster::extract(x, srs)
summary(srs)

# subset out years
webb <- subset(df.sp, Site=="WEBB")

# subset out years
webb14 <- subset(webb, yr==2014)
webb15 <- subset(webb, yr==2015)
webb16 <- subset(webb, yr==2016)
webb17 <- subset(webb, yr==2017)
webb18 <- subset(webb, yr==2018)

# extract covariates
proads <- raster("E:/Roost_Chapter/landscape_co/landscape_co/WEBB/Roads/Webb_prime_rd_dist.tif")
proads <- projectRaster(proads, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sroads <- raster("E:/Roost_Chapter/landscape_co/landscape_co/WEBB/Roads/Webb_2nd_rd_dist.tif")
sroads <- projectRaster(sroads, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/WEBB/dist_open_webb14.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/WEBB/dist_water_webb14.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

webb14$proads <- raster::extract(proads, webb14) 
webb14$sroads <- raster::extract(sroads, webb14)
webb14$dist_open <- raster::extract(dist_open, webb14)
webb14$dist_water <- raster::extract(dist_water, webb14)
webb14$elevation <- raster::extract(feature, webb14)
webb14$feature <- raster::extract(x, webb14)
summary(webb14)

# extract covariates
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/WEBB/dist_open_webb15.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/WEBB/dist_water_webb15.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

webb15$proads <- raster::extract(proads, webb15) 
webb15$sroads <- raster::extract(sroads, webb15)
webb15$dist_open <- raster::extract(dist_open, webb15)
webb15$dist_water <- raster::extract(dist_water, webb15)
webb15$elevation <- raster::extract(feature, webb15)
webb15$feature <- raster::extract(x, webb15)
summary(webb15)

# extract covariates
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/WEBB/dist_open_webb16.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/WEBB/dist_water_webb16.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

webb16$proads <- raster::extract(proads, webb16) 
webb16$sroads <- raster::extract(sroads, webb16)
webb16$dist_open <- raster::extract(dist_open, webb16)
webb16$dist_water <- raster::extract(dist_water, webb16)
webb16$elevation <- raster::extract(feature, webb16)
webb16$feature <- raster::extract(x, webb16)
summary(webb16)

# extract covariates
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/WEBB/dist_open_webb17.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/WEBB/dist_water_webb17.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

webb17$proads <- raster::extract(proads, webb17) 
webb17$sroads <- raster::extract(sroads, webb17)
webb17$dist_open <- raster::extract(dist_open, webb17)
webb17$dist_water <- raster::extract(dist_water, webb17)
webb17$elevation <- raster::extract(feature, webb17)
webb17$feature <- raster::extract(x, webb17)
summary(webb17)

# extract covariates
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/WEBB/dist_open_webb18.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/WEBB/dist_water_webb18.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

webb18$proads <- raster::extract(proads, webb18) 
webb18$sroads <- raster::extract(sroads, webb18)
webb18$dist_open <- raster::extract(dist_open, webb18)
webb18$dist_water <- raster::extract(dist_water, webb18)
webb18$elevation <- raster::extract(feature, webb18)
webb18$feature <- raster::extract(x, webb18)
summary(webb18)

# subset out years
bfg <- subset(df.sp, Site=="BFG")

# subset out years
bfg17 <- subset(bfg, yr==2017)
bfg18 <- subset(bfg, yr==2018)
bfg19 <- subset(bfg, yr==2019)
bfg20 <- subset(bfg, yr==2020)
bfg21 <- subset(bfg, yr==2021)

# extract covariates
proads <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/Roads/PrimeRdRast(1).tif")
proads <- projectRaster(proads, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sroads <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/Roads/SecondRdRast(1).tif")
sroads <- projectRaster(sroads, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_open_cc17.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_water_cc17.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

bfg17$proads <- raster::extract(proads, bfg17) 
bfg17$sroads <- raster::extract(sroads, bfg17)
bfg17$dist_open <- raster::extract(dist_open, bfg17)
bfg17$dist_water <- raster::extract(dist_water, bfg17)
bfg17$elevation <- raster::extract(feature, bfg17)
bfg17$feature <- raster::extract(x, bfg17)
summary(bfg17)

# extract covariates
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_open_cc18.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_water_cc18.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

bfg18$proads <- raster::extract(proads, bfg18) 
bfg18$sroads <- raster::extract(sroads, bfg18)
bfg18$dist_open <- raster::extract(dist_open, bfg18)
bfg18$dist_water <- raster::extract(dist_water, bfg18)
bfg18$elevation <- raster::extract(feature, bfg18)
bfg18$feature <- raster::extract(x, bfg18)
summary(bfg18)

# extract covariates
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_open_cc19.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_water_cc19.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

bfg19$proads <- raster::extract(proads, bfg19) 
bfg19$sroads <- raster::extract(sroads, bfg19)
bfg19$dist_open <- raster::extract(dist_open, bfg19)
bfg19$dist_water <- raster::extract(dist_water, bfg19)
bfg19$elevation <- raster::extract(feature, bfg19)
bfg19$feature <- raster::extract(x, bfg19)
summary(bfg19)

# extract covariates
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_open_cc20.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_water_cc20.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

bfg20$proads <- raster::extract(proads, bfg20) 
bfg20$sroads <- raster::extract(sroads, bfg20)
bfg20$dist_open <- raster::extract(dist_open, bfg20)
bfg20$dist_water <- raster::extract(dist_water, bfg20)
bfg20$elevation <- raster::extract(feature, bfg20)
bfg20$feature <- raster::extract(x, bfg20)
summary(bfg20)

# extract covariates
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_open_cc21.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_water_cc21.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

bfg21$proads <- raster::extract(proads, bfg21) 
bfg21$sroads <- raster::extract(sroads, bfg21)
bfg21$dist_open <- raster::extract(dist_open, bfg21)
bfg21$dist_water <- raster::extract(dist_water, bfg21)
bfg21$elevation <- raster::extract(feature, bfg21)
bfg21$feature <- raster::extract(x, bfg21)
summary(bfg21)

# subset out years
cc<- subset(df.sp, site=="CC")

# subset out years
cc17 <- subset(cc, yr==2017)
cc18 <- subset(cc, yr==2018)
cc19 <- subset(cc, yr==2019)
cc20 <- subset(cc, yr==2020)
cc21 <- subset(cc, yr==2021)

# extract covariates
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_open_cc17.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_water_cc17.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

cc17$proads <- raster::extract(proads, cc17) 
cc17$sroads <- raster::extract(sroads, cc17)
cc17$dist_open <- raster::extract(dist_open, cc17)
cc17$dist_water <- raster::extract(dist_water, cc17)
cc17$elevation <- raster::extract(feature, cc17)
cc17$feature <- raster::extract(x, cc17)
summary(cc17)

# extract covariates
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_open_cc18.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_water_cc18.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

cc18$proads <- raster::extract(proads, cc18) 
cc18$sroads <- raster::extract(sroads, cc18)
cc18$dist_open <- raster::extract(dist_open, cc18)
cc18$dist_water <- raster::extract(dist_water, cc18)
cc18$elevation <- raster::extract(feature, cc18)
cc18$feature <- raster::extract(x, cc18)
summary(cc18)

# extract covariates
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_open_cc19.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_water_cc19.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

cc19$proads <- raster::extract(proads, cc19) 
cc19$sroads <- raster::extract(sroads, cc19)
cc19$dist_open <- raster::extract(dist_open, cc19)
cc19$dist_water <- raster::extract(dist_water, cc19)
cc19$elevation <- raster::extract(feature, cc19)
cc19$feature <- raster::extract(x, cc19)
summary(cc19)

# extract covariates
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_open_cc20.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_water_cc20.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

cc20$proads <- raster::extract(proads, cc20) 
cc20$sroads <- raster::extract(sroads, cc20)
cc20$dist_open <- raster::extract(dist_open, cc20)
cc20$dist_water <- raster::extract(dist_water, cc20)
cc20$elevation <- raster::extract(feature, cc20)
cc20$feature <- raster::extract(x, cc20)
summary(cc20)

# extract covariates
dist_open <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_open_cc21.tif")
dist_open <- projectRaster(dist_open, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dist_water <- raster("E:/Roost_Chapter/landscape_co/landscape_co/CC/dist_water_cc21.tif")
dist_water <- projectRaster(dist_water, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

cc21$proads <- raster::extract(proads, cc21) 
cc21$sroads <- raster::extract(sroads, cc21)
cc21$dist_open <- raster::extract(dist_open, cc21)
cc21$dist_water <- raster::extract(dist_water, cc21)
cc21$elevation <- raster::extract(feature, cc21)
cc21$feature <- raster::extract(x, cc21)
summary(cc21)

cc <- rbind(cc17,cc18,cc19,cc20,cc21)
bfg <- rbind(bfg18, bfg19)
webb <- rbind(webb14,webb15,webb16,webb17,webb18)
cc <- as.data.frame(cc)
bfg <- as.data.frame(bfg)
webb <- as.data.frame(webb)
srs <- as.data.frame(srs)

final <- rbind(cc,bfg,webb,srs)

# Add site and yr to the random points
final$Sex <- df$Sex[sapply(final$ID, function(x) match(x, df$ID))]
final$Age <- df$Age[sapply(final$ID, function(x) match(x, df$ID))]

write.csv(final, "E:/Roost_Chapter/Roost_Covariates_Males.csv")
