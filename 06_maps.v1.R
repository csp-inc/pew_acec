library(basemaps)
library(sf)
library(ggplot2)
library(ggpubr)
library(raster)
library(terra)
library(tidyterra)
library(rnaturalearthdata)
library(rnaturalearth)
library(smoothr)
library(knitr)
library(tinytex)

### Source plotting functions
source("/Users/patrickfreeman-csp/Documents/GitHub/pew_acec/utils/00_map_plot_functions.R")

### Get the aoisShapes object
stillwater <- load_f("/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/data/StillwaterRange_NV/StillwaterACEC_051023/StillwaterAcecTotal-polygon.shp") %>%
  as_Spatial() %>%
  aggregate() %>%
  st_as_sf()

aoisShapes <- list(stillwater)
aoisNames <- c(
  "Stillwater Range"
)

### Set WD to data directory for sourcing 
setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/data/working")

### Get states from 01_dataLoad.v2.R 
states <- west
### Simplify for plotting
states <- st_simplify(west, dTolerance = 1000)

### Bounding box buffered by 10 km for visualization
aoi_bbox <- st_as_sfc(st_bbox(aoisShapes[[1]])) %>%
  st_transform(., crs=5070) %>% 
  st_buffer(., 10000)

### Aoi in proper crs and buffered by 10km for use setting some bounding boxes 
aoi_5070 <- aoisShapes[[1]] %>% 
  st_as_sf() %>%
  st_transform(., crs=5070) %>%
  st_buffer(., 10000)

### Get country boundaries 
countries <- ne_countries(type="countries", continent="north america", scale="medium", returnclass="sf") %>%
  st_transform(., crs=3857)

### Countries that will be plotted 
mexico_us_canada <- countries %>% 
  dplyr::filter(admin%in%c("United States of America", "Canada", "Mexico")) 

### Set indicator 
(intact <- raster("intactNorm.tif"))
(rept <- raster("rept_west_270m.tif"))
(geoDiv <- raster("div_ergo_lth270mnorm.tif"))
(geoRar <- raster("georarity270mnorm.tif"))
(sage <- raster("sage_270m.tif"))
(climStab <- raster("ClimStabNorm.tif"))



### Threat rasters -- slightly different projection issues 
(mineral <- raster("mrdsPA5kmeanmnormPAs0UrbH20.tif"))
(wind <- raster("windprobi_lt30pslope_ddpowerline4normPAs0UrbH20MULT.tif"))
(solar <- raster("maxdnighi_lt5pslope_ddpowerline4normPAs0UrbH20.tif"))
(geotherm <- raster("geotherm_lt10pslop_nourbFWPAspldist.tif"))
(annHerb <- raster("annHerb_270m.tif"))




### Swap out indicator of interest 
ind <- rast(geoDiv)

### Calculate 2nd and 98th quantile for visualization params 
(qr <- global(ind, \(i) quantile(i, c(0.02, 0.98), na.rm=T)))

# ind_fix <- terra::project(ind, rast(intact))
# ind <- ind_fix
 
re2 <- extend(ind, c(3000, 1000))
 

### Setting basemap defaults - for some reason supplying a raster helps to control the resolution that gets funky when plotting -- supply with the indicator raster 
set_defaults(rast(re2), map_service = "esri", map_type = "world_hillshade")

### Get the basemap as a raster 
x <- basemaps::basemap_raster(map_service = "esri", map_type = "world_hillshade", map_res=1)
### convert basemap to terra 
x_terra <- rast(x)
### Mask basemap to country borders 
x_terra_masked <- mask(x_terra, vect(mexico_us_canada))

### Define country boundaries for plotting 
countries_plotting <- crop(vect(mexico_us_canada), x_terra_masked) %>% 
  st_as_sf() %>%
  st_transform(., crs=5070)

### Get min and max of raster values to maintain scales across both plots 
### Sometimes works...sometimes doesn't for reasons unknown to me - 
#my_lims <- minmax(ind, na.rm=T) %>% as.integer()

### Alternative - use the 2nd and 98th percentile as min and max to improve visualization
my_lims <- c(qr$X2., qr$X98.)

### Masking some threat rasters 
ind <- mask(ind, vect(st_transform(states, crs=st_crs(ind))))

### for sage and annHerb rasters
#sb <- load_f("/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/data/working/US_Sagebrush_Biome_2019.shp")
#ind <- mask(ind, vect(st_transform(sb, crs=st_crs(ind))))



### Plot west wide map with or without scale
(ind_west_scale_squish <- west_wide_map_wscale_squishlims(ind, "muted", "red","black", "black"))
#ind_west_scale <- west_wide_map_wscale(ind)
#ind_west_noscale <- west_wide_map_noscale(ind)
  
y_diff <- st_bbox(states)[4]-st_bbox(states)[2]
x_diff <- st_bbox(states)[3]-st_bbox(states)[1]
y_to_x_ratio <- y_diff/x_diff

# 
# ggsave("/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/analyses/output/otero_mesa/otero_amph_richness_west.png", big, width=5.75, h=4.5, units='in', dpi=300)

pdf_file <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/analyses/output/stillwater_range_nv/stillwater_annHerb_west_withscale.pdf"

ggsave(
  pdf_file,
  device = cairo_pdf,
  width = 5.75,
  height = 4.5,
  units="in"
)

knitr::plot_crop(pdf_file)

### Zoom in
### Crop the raster


aoi_buff <- aoi_bbox %>%
  st_buffer(., 100000) %>%
  st_transform(., crs=st_crs(ind))

### Careful here if the raster for the indicator isn't in the exact coordinate reference system that the aoi_buff generated above   
aoi_buff_match <- aoi_buff %>%
  st_transform(., crs=st_crs(ind))

cropped_rast <- terra::crop(ind, aoi_buff_match)


### Get the differences between the x and y lims 
x_diff <- st_bbox(aoi_5070)[3]-st_bbox(aoi_5070)[1]
y_diff <- st_bbox(aoi_5070)[4]-st_bbox(aoi_5070)[2]

### get the adjustment factor to make this map match 'big' above --
y_add <- 1.17*x_diff

ymin <- st_bbox(aoi_5070)[2]

### Plot zoom map with or without scale/squishlims
#(zoom <- zoom_map_wscale(cropped_rast, "muted", "black"))
(zoom <- zoom_map_wscale_squishlims(cropped_rast, "viridi", "black", ymin))
#(zoom <- zoom_map_noscale(cropped_rast))
#(zoom <- zoom_map_noscale_squishlims(cropped_rast))


pdf_file <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/analyses/output/stillwater_range_nv/stillwater-geoDiv-zoom-withscale.pdf"

ggsave(
  pdf_file,
  device = cairo_pdf,
  width = 5.75,
  height = 4.5
)

knitr::plot_crop(pdf_file)

