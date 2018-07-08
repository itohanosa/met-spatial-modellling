library(superClassAnalysis)
LoadPackages()

gammap <- brick("D:/Itohan/Results/GAM.tif")
gammap_future <- brick("D:/Itohan/Results/GAM_Future.tif")
rf_presence <-  brick("D:/Itohan/Results/RF_Presence.tif")
rf_future <-  brick("D:/Itohan/Results/RF_Future.tif")

plot(gammap)

CMR <- raster::getData('GADM', country="CMR", level=0)

study_area <- CMR

plot(study_area, add=T)

gammap_crop <- crop(gammap, study_area)
gammap_mask <- mask(gammap_crop, study_area)

gammap_future_crop <- crop(gammap_future, study_area)
gammap_future_mask <- mask(gammap_future_crop, study_area)

rf_presence_crop<- crop(rf_presence, study_area)
rf_future_crop<- crop(rf_future, study_area)

rf_presence_mask<- mask(rf_presence_crop, study_area)
rf_future_mask<- mask(rf_future_crop, study_area)

plot(gammap_mask)
plot(gammap_future_mask)
plot(rf_presence_mask)
plot(rf_future_mask)

writeRaster(gammap_mask, filename="D:/Itohan/Results/GAM_final.tif", 
            format="GTiff", overwrite=TRUE)
writeRaster(gammap_future_mask, filename="D:/Itohan/Results/GAM_Future_final.tif", 
            format="GTiff", overwrite=TRUE)
writeRaster(rf_presence_mask, filename="D:/Itohan/Results/RF_Presence_final.tif", 
            format="GTiff", overwrite=TRUE)
writeRaster(rf_future_mask, filename="D:/Itohan/Results/RF_Future_final.tif", 
            format="GTiff", overwrite=TRUE)



data(wrld_simpl)
unique(wrld_simpl$REGION)
africa=wrld_simpl[wrld_simpl$REGION==2,]

cameroon <- getData('GADM', country='CMR', level=2)

plot(gammap_mask, add=T)


