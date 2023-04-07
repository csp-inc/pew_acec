###########
## SETUP ##
###########

##----------------------------------------------
## Set working directory 
setwd("/Volumes/GoogleDrive/My Drive/00_CSP_Projects/Pew_ACEC")
wd <- "/Volumes/GoogleDrive/My Drive/00_CSP_Projects/Pew_ACEC"
data.dir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/data"
out.dir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/analyses/output/otero_mesa/"



##----------------------------------------------
# Install packages if not already installed
required.packages <- c("plyr", "ggplot2", "gridExtra", "terra", "raster", "sf", "stars", "rgdal", "dplyr",
                       "tidyverse", "maptools", "rgeos", 
                       "partykit", "vcd", "maps", "mgcv", "tmap",
                       "MASS", "pROC", "ResourceSelection", "caret", "broom", "boot",
                       "dismo", "gbm", "usdm", "pscl", "randomForest", "pdp", "classInt", "plotmo",
                       "ggspatial", "lmtest",  "dynatopmodel", "spatialEco", "exactextractr", "fasterize",
                       "chemCal")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages, dependencies = "TRUE")
rm(required.packages, new.packages)

# Libraries
# library(plyr)
library(ggplot2)
library(gridExtra)
library(terra)
library(raster)
# library(sp)
library(sf)
library(stars)
library(rgdal)

# Please note that rgdal will be retired by the end of 2023,
# plan transition to sf/stars/terra functions using GDAL and PROJ
# at your earliest convenience.

library(dplyr)
library(tidyverse)
library(maptools)
library(rgeos)
library(partykit)
library(vcd)
library(maps)
library(mgcv)
library(tmap)
library(MASS)
library(pROC)
library(ResourceSelection)
library(caret)
library(broom)
library(boot)
library(dismo)
library(gbm)
library(usdm)
library(pscl)
library(randomForest)
library(pdp)
library(classInt)
library(plotmo)
library(ggspatial)
library(lmtest)
library(dynatopmodel)
library(spatialEco)
library(exactextractr)
library(RColorBrewer)
library(fasterize)
library(chemCal)



par(mfrow=c(1,1))

# rm(GCtorture)

# # # ref: https://gis.stackexchange.com/questions/389814/r-st-centroid-geos-error-unknown-wkb-type-12
install.packages("gdalUtilities")
library(gdalUtilities)

ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}


# https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
# sf::sf_use_s2(FALSE)

#####################################
# Turn off scientific notation
options(scipen=999) 



#####################################
# Grab date for saving files
currentDate <- Sys.Date()


today <- paste0(mid(Sys.Date(),3,2),
                mid(Sys.Date(),6,2),
                mid(Sys.Date(),9,2))




