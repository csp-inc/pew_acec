library(basemaps)
library(sf)
library(ggplot2)
library(ggpubr)
library(raster)
library(terra)
library(tidyterra)
library(rnaturalearthdata)
library(rnaturalearth)
library(knitr)
library(tinytex)

### Get the aoisShapes object

aoisShapes <- list(frenchmans)
aoisNames <- c(
  "Frenchman's Breaks"
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
(ecoRar <- raster("ecorarityaggto270norm.tif"))
(climStab <- raster("ClimStabNorm.tif"))
(climAcc <- raster("ClimAccNorm.tif"))
(connect <- raster("connNorm.tif"))


### Threat rasters -- slightly different projection issues 
(oilGas <- raster("oilgas5k6cellmean270mnorm_PAs0UrbH20.tif"))
(geotherm <- raster("geotherm_lt10pslop_nourbFWPAspldist.tif"))
(wind <- raster("windprobi_lt30pslope_ddpowerline4normPAs0UrbH20MULT.tif"))
(solar <- raster("maxdnighi_lt5pslope_ddpowerline4normPAs0UrbH20.tif"))


sb <- load_f("/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/data/working/US_Sagebrush_Biome_2019.shp")
###
ind <- rast(climAcc)
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
my_lims <- c(0, 1)

### Masking some threat rasters 
ind <- mask(ind, vect(st_transform(states, crs=st_crs(ind))))
### Set up several plotting functions to make things more streamlined
west_wide_map_wscale <- function(ind){
  big <- ggplot() +
    geom_spatraster_rgb(data=x_terra_masked) + 
    geom_spatraster(data = ind) + 
    scale_fill_whitebox_c(
      palette = "viridi",
      na.value = NA,
      limits=my_lims
      #oob = scales::oob_squish_any
    ) + 
    geom_sf(data=states, fill=NA, col="black", lwd=0.5) + 
    #geom_sf(data=aoisShapes[[1]], fill=NA, col="red", lwd=0.5) + 
    geom_sf(data=aoi_bbox, fill=NA, col="#d95a6d", lwd=0.6) + 
    geom_sf(data=mexico_us_canada, col="black", fill=NA, lwd=0.25) + 
    coord_sf(crs=5070,
             xlim = c(st_bbox(states)[1], st_bbox(states)[3]),
             ylim = c(st_bbox(states)[2], st_bbox(states)[4]),
             expand = F) +
    annotation_scale() +
    annotation_north_arrow(which_north = "grid",
                           pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    theme_void() + 
    theme(legend.position="none",
          panel.background = element_rect(fill = "lightblue",
                                          colour = "lightblue")
    )
  return(big)
}
west_wide_map_wscale_squishlims <- function(ind){
  big <- ggplot() +
    geom_spatraster_rgb(data=x_terra_masked) + 
    geom_spatraster(data = ind) + 
    scale_fill_whitebox_c(
      palette = "viridi",
      na.value = NA,
      limits=my_lims,
      oob = scales::oob_squish_any
    ) + 
    geom_sf(data=states, fill=NA, col="black", lwd=0.5) + 
    #geom_sf(data=aoisShapes[[1]], fill=NA, col="red", lwd=0.5) + 
    geom_sf(data=aoi_bbox, fill=NA, col="#d95a6d", lwd=0.6) + 
    geom_sf(data=mexico_us_canada, col="black", fill=NA, lwd=0.25) + 
    coord_sf(crs=5070,
             xlim = c(st_bbox(states)[1], st_bbox(states)[3]),
             ylim = c(st_bbox(states)[2], st_bbox(states)[4]),
             expand = F) +
    annotation_scale() +
    annotation_north_arrow(which_north = "grid",
                           pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    theme_void() + 
    theme(legend.position="none",
          panel.background = element_rect(fill = "lightblue",
                                          colour = "lightblue")
    )
  return(big)
}

west_wide_map_noscale <- function(ind){
  big <- ggplot() +
    geom_spatraster_rgb(data=x_terra_masked) + 
    geom_spatraster(data = ind) + 
    scale_fill_whitebox_c(
      palette = "viridi",
      na.value = NA,
      limits=my_lims
      #oob = scales::oob_squish_any
    ) + 
    geom_sf(data=states, fill=NA, col="black", lwd=0.5) + 
    #geom_sf(data=aoisShapes[[1]], fill=NA, col="red", lwd=0.5) + 
    geom_sf(data=aoi_bbox, fill=NA, col="#d95a6d", lwd=0.6) + 
    geom_sf(data=mexico_us_canada, col="black", fill=NA, lwd=0.25) + 
    coord_sf(crs=5070,
             xlim = c(st_bbox(states)[1], st_bbox(states)[3]),
             ylim = c(st_bbox(states)[2], st_bbox(states)[4]),
             expand = F) +
    theme_void() + 
    theme(legend.position="none",
          panel.background = element_rect(fill = "lightblue",
                                          colour = "lightblue")
    )
  return(big)
}
west_wide_map_noscale_squishlims <- function(ind){
  big <- ggplot() +
    geom_spatraster_rgb(data=x_terra_masked) + 
    geom_spatraster(data = ind) + 
    scale_fill_whitebox_c(
      palette = "viridi",
      na.value = NA,
      limits=my_lims,
      oob = scales::oob_squish_any
    ) + 
    geom_sf(data=states, fill=NA, col="black", lwd=0.5) + 
    #geom_sf(data=aoisShapes[[1]], fill=NA, col="red", lwd=0.5) + 
    geom_sf(data=aoi_bbox, fill=NA, col="#d95a6d", lwd=0.6) + 
    geom_sf(data=mexico_us_canada, col="black", fill=NA, lwd=0.25) + 
    coord_sf(crs=5070,
             xlim = c(st_bbox(states)[1], st_bbox(states)[3]),
             ylim = c(st_bbox(states)[2], st_bbox(states)[4]),
             expand = F) +
    theme_void() + 
    theme(legend.position="none",
          panel.background = element_rect(fill = "lightblue",
                                          colour = "lightblue")
    )
  return(big)
}

### Plot west wide map with or without scale
#ind_west_scale_squish <- west_wide_map_wscale_squishlims(ind)
#ind_west_scale <- west_wide_map_wscale(ind)
ind_west_noscale <- west_wide_map_noscale(ind)
  
y_diff <- st_bbox(states)[4]-st_bbox(states)[2]
x_diff <- st_bbox(states)[3]-st_bbox(states)[1]
y_to_x_ratio <- y_diff/x_diff

# 
# ggsave("/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/analyses/output/otero_mesa/otero_amph_richness_west.png", big, width=5.75, h=4.5, units='in', dpi=300)

pdf_file <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/analyses/output/frenchman_breaks/frenchmansbreaks_west_ecoRar_scale.pdf"

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

cropped_rast <- terra::crop(ind, vect(aoi_buff_match))


### Get the differences between the x and y lims 
x_diff <- st_bbox(aoi_5070)[3]-st_bbox(aoi_5070)[1]
y_diff <- st_bbox(aoi_5070)[4]-st_bbox(aoi_5070)[2]

### get the adjustment factor to make this map match 'big' above --
y_add <- 1.17*x_diff

##manually adjust the x and y lims in this function to match desired AOI
zoom_map_wscale <- function(cropped_rast){
  zoom <- ggplot() +
    geom_sf(data=mexico_us_canada  , fill="darkgray", lwd=0.6) + 
    geom_spatraster(data = cropped_rast) + 
    scale_fill_whitebox_c(
      palette = "viridi",
      na.value = NA,
      limits=my_lims) + 
    geom_sf(data=states, fill=NA, col="black", lwd=0.5) + 
    geom_sf(data=aoisShapes[[1]], fill=NA, col="red", lwd=1.2) + 
    coord_sf(xlim = c(st_bbox(aoi_5070)[1], st_bbox(aoi_5070)[3]),
             ylim = c(2904410.7, (2904410.7+y_add)),### Add adjustment factor to ymax
             expand = T,
             crs=5070
    ) +
    annotation_scale() + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) + 
    theme_void() +
    theme(legend.position="none",
          panel.background = element_rect(fill = "lightblue",
                                          colour = "lightblue"),
          panel.grid=element_blank(),
          panel.border=element_blank())
}
zoom_map_wscale_squishlims <- function(cropped_rast){
  zoom <- ggplot() +
    geom_sf(data=mexico_us_canada  , fill="darkgray", lwd=0.6) + 
    geom_spatraster(data = cropped_rast) + 
    scale_fill_whitebox_c(
      palette = "viridi",
      na.value = NA,
      limits=my_lims,
      oob = scales::oob_squish_any
    ) + 
    geom_sf(data=states, fill=NA, col="black", lwd=0.5) + 
    geom_sf(data=aoisShapes[[1]], fill=NA, col="red", lwd=1.2) + 
    coord_sf(xlim = c(st_bbox(aoi_5070)[1], st_bbox(aoi_5070)[3]),
             ylim = c(2904410.7, (2904410.7+y_add)),### Add adjustment factor to ymax
             expand = T,
             crs=5070
    ) +
    annotation_scale() + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) + 
    theme_void() +
    theme(legend.position="none",
          panel.background = element_rect(fill = "lightblue",
                                          colour = "lightblue"),
          panel.grid=element_blank(),
          panel.border=element_blank())
}

### Mapping functions for zoom in map
zoom_map_noscale <- function(cropped_rast){
  zoom <- ggplot() +
    geom_sf(data=mexico_us_canada  , fill="darkgray", lwd=0.6) + 
    geom_spatraster(data = cropped_rast) + 
    scale_fill_whitebox_c(
      palette = "viridi",
      na.value = NA,
      limits=my_lims) + 
    geom_sf(data=states, fill=NA, col="black", lwd=0.5) + 
    geom_sf(data=aoisShapes[[1]], fill=NA, col="red", lwd=1.2) + 
    coord_sf(xlim = c(st_bbox(aoi_5070)[1], st_bbox(aoi_5070)[3]),
             ylim = c(2904410.7, (2904410.7+y_add)),### Add adjustment factor to ymax
             expand = T,
             crs=5070
    ) +
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) + 
    theme_void() +
    theme(legend.position="none",
          panel.background = element_rect(fill = "lightblue",
                                          colour = "lightblue"),
          panel.grid=element_blank(),
          panel.border=element_blank())
}
zoom_map_noscale_squishlims <- function(cropped_rast){
  zoom <- ggplot() +
    geom_sf(data=mexico_us_canada  , fill="darkgray", lwd=0.6) + 
    geom_spatraster(data = cropped_rast) + 
    scale_fill_whitebox_c(
      palette = "viridi",
      na.value = NA,
      limits=my_lims,
      scales::oob_squish_any
    ) + 
    geom_sf(data=states, fill=NA, col="black", lwd=0.5) + 
    geom_sf(data=aoisShapes[[1]], fill=NA, col="red", lwd=1.2) + 
    coord_sf(xlim = c(st_bbox(aoi_5070)[1], st_bbox(aoi_5070)[3]),
             ylim = c(2904410.7, (2904410.7+y_add)),### Add adjustment factor to ymax
             expand = T,
             crs=5070
    ) +
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) + 
    theme_void() +
    theme(legend.position="none",
          panel.background = element_rect(fill = "lightblue",
                                          colour = "lightblue"),
          panel.grid=element_blank(),
          panel.border=element_blank())
}

### Plot zoom map with or without scale/squishlims
(zoom <- zoom_map_wscale_squishlims(cropped_rast))
#(zoom <- zoom_map_noscale(cropped_rast))

pdf_file <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/analyses/output/frenchman_breaks/frenchmansbreaks_zoom_ecoRar_noscale.pdf"

ggsave(
  pdf_file,
  device = cairo_pdf,
  width = 5.75,
  height = 4.5
)

knitr::plot_crop(pdf_file)

