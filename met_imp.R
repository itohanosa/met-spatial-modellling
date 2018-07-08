setwd("D:/itohan/very important")

#https://pakillo.github.io/R-GIS-tutorial/#gmap
#https://cmerow.github.io/RDataScience/05_Raster.html
library(dismo)
library(magrittr)
library(raster)
library(RCurl)
library(rgdal)
library(maptools)
source("varImpBiomod.R")

CMR <- getData('GADM', country="CMR", level=0)

study_area <- CMR
bio <- raster::getData("worldclim", var = "bio", res = 2.5)

bio <- raster::getData('CMIP5', var='bio', res=2.5, rcp=85, 
                       model='MP', year=50)

data(wrld_simpl)
unique(wrld_simpl$REGION)
africa=wrld_simpl[wrld_simpl$REGION==2,]

study_area <- africa

#wc <- getData('worldclim', var='bio', res=10)
#Plot the first raster layer, i.e. annual mean temperature and annual precipitation
#plot(bio, 1)

# plot(bio, 12)#annual precipitation
#plot(raster(bio, 1))
# Add the outline of the study area
# plot(study_area, add=TRUE)

biocrop <- crop(bio, extent(study_area) + 10)
#prec <- getData("worldclim",var="bio",res=10)
# now use the mask function
#rr <- mask(biocrop, study_area)

#rr <- biocrop # just for convenience

#rr <- mask(biocrop, study_area)

#plot(rr)
#plotRGB(rr, stretch = "lin")
#writeRaster("worldclim", format="GTiff", datatype="INT2S", overwrite=TRUE)

###modis ndvi

library(rts)

library(raster)

library(RCurl)
#https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/myd13q1_v006
#evi/ndvi values range from -2000 to 10000 or divide by scale factor of -0.0001
cam <- brick("F:/MOD2/newr.tif")
cam
plot(cam, main="EVI/NDVI")

#srtm90 Elevation
#https://www.gis-blog.com/r-raster-data-acquisition/
#srtm <- getData('SRTM', lon=7, lat=12.4)
#plot(srtm)
#plot(study_area, add=TRUE)
#Download two more tiles
#srtm2 <- getData('SRTM', lon=14, lat=7.5)
#srtm3 <- getData('SRTM', lon=12, lat=7.5)
#srtm4 <- getData('SRTM', lon=12, lat=5)
#srtm3 <- getData('SRTM', lon=15.5, lat=7.5)

#Mosaic/merge srtm tiles
#srtmmosaic <- mosaic(srtm, srtm2, srtm3, fun=mean)
#Lets plot the result:

#plot(srtmmosaic, main="Elevation (SRTM)")
#plot(study_area, add=TRUE)

###stack https://github.com/Model-R/RasterMngmt
#extent(rr) <- cam
#raster::stack("rr", "cam")

#hlp <- stack("rr", "cam")
# product list:

#niche modelling
##http://lab.fengxiao.info/2016/11/23/ENM-in-R-workshop.html

###
#species.crop <- crop(species,rr[[1]])
#plot(species.crop,add=TRUE)
##vaccination data
#yvac <- read.csv("F:/ITO/very important/Supplementary Interactive Plot Data.csv")
#coordinates(yvac) <- "+proj=longlat +datum=WGS84"

#yvac_1 <- read.csv("F:/ITO/very important/Supplementary Interactive Plot Data 2.csv")

##worldpop##
#world_pop <- read.csv("F:/ITO/MET1/worldpop/Metadata_Country_API_SP.POP.TOTL_DS2_en_csv_v2_9944650.csv")
#head(world_pop)
#class(world_pop)
#######


#########
##yellow_fever vectors:mosquitoes
#' Read occurrence points. If they exist locally, use the local file.
library(rgdal)
library(rgbif)
library(dismo)
library(RStoolbox)
#' If they do not exist, download from [gbif](http://www.gbif.org) 
if (file.exists("./GIS/Aedes/Aegypti.mif")) {
  species <- readOGR("./GIS/Aedes/Aegypti.mif", layer = "Aedes")
} else {
  # Download species location data from gbif
  # species <- gbif("Aedes", "aegypti", ext = extent(bio), sp = TRUE, removeZeros = TRUE)
  species0 <- gbif('Aedes', 'aegypti')
  species <- subset(species0,select=c("lat","lon"))
  species <- na.omit(species)
  coordinates(species) <- c("lon", "lat")  # set spatial coordinates
  
  
  # Add projection information
  proj4string(species) <- CRS("+proj=longlat +datum=WGS84")
  # Save species records in mif-format (preserves full column names)
  # writeOGR(species, "./GIS/Anopheles", 
  #          "Anopheles", driver="MapInfo File", dataset_options="FORMAT=MIF")
}
######forget if######

# species2 <- species0[which(species0$country=="Cameroon"),]
# species3 <- subset(species2,select=c('lat','lon'))
# species3 <- na.omit(species3)
# coordinates(species3) <- c("lon", "lat")  # set spatial coordinates
# proj4string(species3) <- CRS("+proj=longlat +datum=WGS84")

# plot(raster(rr, 12))
##visualization
# z <- plot(species3, col="green", pch = 19, add = TRUE)

#ggplot(z, aes(fill = nots)) +
#  geom_sf() +
#  north(z, symbol = 16, scale = 0.15) +
#  scale_fill_brewer(name = 'Animal abuse\nnotifications', palette = 8) +
#  scalebar(z, dist = 5) +
#  xlab('Meters') +
#  ylab('Meters')


#' Data preprocessing
#' ==============================

#' Select species records for which environmental information is available
#' -------------------------------
species <- species[complete.cases(extract(biocrop, species)), ]



#' Collinearity
#' -----------------------------
#' ### Visual inspection of collinearity ###
# cm <- cor(getValues(bio), use = "complete.obs")
library(ellipse)
#plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))

#' ### Select an uncorrelated subset of environmental variables ###
env <- subset(biocrop, c("bio1", "bio2", "bio14","bio15", "bio19"))
env <- subset(biocrop, c("mp85bi501", "mp85bi502", "mp85bi5014","mp85bi5015", "mp85bi5019"))


cm2 <- cor(getValues(env), use = "complete.obs")
plotcorr(cm2, col=ifelse(abs(cm2) > 0.7, "red", "grey"))

#' Sampling of (pseudo-)absence points
#' ====================================================
#' The function randomPoints in package dismo allows to
#' randomly select a certain number of random points,
#' and to adjust the probability of selecting a cell
#' according to its size, which is relevant in lat-lon-grids,
#' where cells are of differing size

#' Selecting 2000 random background points, excluding cells where
#' the species is present
set.seed(2)
background <- randomPoints(env, 2000, species)
#' Select only one presence record in each cell of the environmental layer
presence <- gridSample(species, env, n = 1)

#'
#' Now we combine the presence and background points, adding a
#' column "species" that contains the information about presence (1)
#' and background (0)
fulldata <- SpatialPointsDataFrame(rbind(presence, background),
                                   data = data.frame("species" = rep(c(1,0),
                                                                     c(nrow(presence), nrow(background)))),
                                   match.ID = FALSE,
                                   proj4string = CRS(projection(env)))
#' Add information of environmental conditions at point locations
fulldata@data <- cbind(fulldata@data, extract(env, fulldata))

#'
# Split data set into a training and test data set
set.seed(2)
fold <- kfold(fulldata, k = 5)
traindata <- fulldata[fold != 1, ]
testdata <- fulldata[fold == 1, ]

#' We can now use a range of statistical methods to estimate the
#' probability of species occurrence.
#' Unfortunately, there are often subtle differences in how the models
#' are specified and in which data formats are useable

varnames <- c("bio1", "bio2", "bio14","bio15", "bio19")

varnames <- c("mp85bi501", "mp85bi502", "mp85bi5014","mp85bi5015", "mp85bi5019")

## Generalized Linear Model
library(mgcv)
## Generalized additive models
gammodel <- gam(species ~ s(mp85bi501) + s(mp85bi502) + s(mp85bi5014) +s(mp85bi5015) +s(mp85bi5019),
                family="binomial", data=traindata)
summary(gammodel)

plot(gammodel)

# Now we should do model selection: bio14 does not contribute to the fit

# Evaluate model on test data
# a) Predict to test data
gamtest <- predict(gammodel, newdata = testdata, type = "response")

# b) Calculate performance indices
#val.prob(gamtest, testdata[["species"]])

# Variable importance
gamimp <- varImpBiomod(gammodel, varnames,
                       traindata)


# dev.off()
barplot(100 * gamimp/sum(gamimp), ylab = "Variable importance (%)")

# Response functions
plot(gammodel, pages = 1)

# png("gammodel_resp.png", 800, 800)
# plot(gammodel, pages = 1)
# dev.off()

# Prediction map
gammap <- predict(env, gammodel, type = "response")

plot(gammap, main="GAM")


# gama<- mask(gammap, study_area)

writeRaster(gammap, filename="D:/Itohan/Results/GAM_Future.tif", 
            format="GTiff", overwrite=TRUE)
## Random forest
library(randomForest)
library(mgcv)
# randomForest requires the dependent variable to be a factor
# if we want to do classification
rftraindata <- as(traindata, "data.frame")
rftraindata$species <- factor(rftraindata$species)

# TODO: check proper settings of random forest algorithm
rfmodel <- randomForest(species ~ bio1 + bio2 + bio14 + bio15 + bio19, data = rftraindata)

library(rms)
# Evaluate model on test data
# a) Predict to test data
rftest <- predict(rfmodel, newdata = testdata, type = "prob")[,2]
# b) Calculate performance indices
val.prob(rftest, testdata[["species"]])


# Variable importance
rfImp <- importance(rfmodel)
varImpPlot(rfmodel)

# Response functions
par(mfrow=c(3,2))
for (i in seq_along(varnames)) {
  partialPlot(rfmodel, rftraindata, varnames[i], xlab = varnames[i], main="") 
}

# Prediction map
rfmap <- predict(env, rfmodel, type = "prob", index = 2)
par(mfrow=c(1, 1))
plot(rfmap, main="RANDOM FOREST")

writeRaster(rfmap, filename="D:/Itohan/Results/RF_Presence.tif", 
            format="GTiff", overwrite=TRUE)

## Maxent
# The following code assumes that the column with the species information
# is in the first position
library(dismo)
maxentmodel <- dismo::maxent(traindata@data[, -1], traindata[["species"]],
                      args = c("nothreshold",
                               "nohinge"))


# Model evaluation on test data
maxenttest <- predict(maxentmodel, testdata)
val.prob(maxenttest, testdata[["species"]])

# Alternatively, we can use the evaluate function
maxente <- evaluate(p = maxenttest[testdata[["species"]] == 1],
                    a = maxenttest[testdata[["species"]] == 0])

# Show variable importance
plot(maxentmodel)

# Plot response functions
response(maxentmodel)

# Prediction map
maxentmap <- predict(maxentmodel, env)
plot(maxentmap)

# Plot predictions of several methods, using the same
# colour scheme
par(mfrow = c(3, 1), mar = c(3, 3, 1, 1))
brks <- seq(0, 1, by = 0.1)
arg <- list(at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2))
col <- rev(terrain.colors(length(brks) - 1))
plot(gammap, breaks = brks, col = col, axis.args = arg, main="GAM")
plot(rfmap, breaks = brks, col = col, axis.args = arg, main="RF")
plot(maxentmap, breaks = brks, col = col, axis.args = arg)

