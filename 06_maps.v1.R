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
  st_transform(., crs=5070) %>%
  st_buffer(., 10000)

### Get country boundaries 
countries <- ne_countries(type="countries", continent="north america", scale="medium", returnclass="sf") %>%
  st_transform(., crs=3857)

### Countries that will be plotted 
mexico_us_canada <- countries %>% 
  dplyr::filter(admin%in%c("United States of America", "Canada", "Mexico")) 

### Set indicator 
(mamm <- raster("mamm_west_270m.tif"))
(rept <- raster("rept_west_270m.tif"))
(amph <- raster("amph_west_270m.tif")) 
(connect <- raster("connNorm.tif"))


### Threat rasters -- slightly different projection issues 
(mineral <- raster("mrdsPA5kmeanmnormPAs0UrbH20.tif"))
(solar <- raster("maxdnighi_lt5pslope_ddpowerline4normPAs0UrbH20.tif"))
(wind <- raster("windprobi_lt30pslope_ddpowerline4normPAs0UrbH20MULT.tif"))


 ind <- rast(rept)
 ind_fix <- terra::project(ind, rast(mamm))
 ind <- ind_fix

### Setting basemap defaults - for some reason supplying a raster helps to control the resolution that gets funky when plotting -- supply with the indicator raster 
set_defaults(rast(ind), map_service = "esri", map_type = "world_hillshade")

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
my_lims <- minmax(ind) %>% as.integer()
#my_lims <- c(0,0.5)

### Westwide plot with state boundaries and the AOI highlighted in red with a bounding box
(big <- ggplot() +
  geom_spatraster_rgb(data=x_terra_masked) + 
  geom_spatraster(data = ind) + 
  scale_fill_whitebox_c(
    palette = "viridi",
    na.value = NA,
    limits=my_lims
    #oob = scales::oob_squish_any
  ) + 
  geom_sf(data=states, fill=NA, col="black", lwd=0.5) + 
  geom_sf(data=aoisShapes[[1]], fill=NA, col="red", lwd=0.5) + 
  geom_sf(data=aoi_bbox, fill=NA, col="black", lwd=0.6) + 
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
        )) 
  
y_diff <- st_bbox(states)[4]-st_bbox(states)[2]
x_diff <- st_bbox(states)[3]-st_bbox(states)[1]
y_to_x_ratio <- y_diff/x_diff

# 
# ggsave("/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/analyses/output/otero_mesa/otero_amph_richness_west.png", big, width=5.75, h=4.5, units='in', dpi=300)

pdf_file <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/analyses/output/otero_mesa/otero_rept_west_scale.pdf"

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
  st_buffer(., 100000)

### Careful here if the raster for the indicator isn't in the exact coordinate reference system that the aoi_buff generated above   
cropped_rast <- terra::crop(ind, vect(aoi_buff))


### Get the differences between the x and y lims 
x_diff <- st_bbox(aoi_5070)[3]-st_bbox(aoi_5070)[1]
y_diff <- st_bbox(aoi_5070)[4]-st_bbox(aoi_5070)[2]

### get the adjustment factor to make this map match 'big' above 
y_add <- 1.17*x_diff

###oob = scales::oob_squish_any ### Can be added if you wnt to fiddle with visualization scales -- squishes any oob values down to max value
zoom <- ggplot() +
  geom_sf(data=mexico_us_canada  , fill="darkgray", lwd=0.6) + 
  geom_spatraster(data = cropped_rast) + 
  scale_fill_whitebox_c(
    palette = "viridi",
    na.value = NA,
    limits=my_lims
    #oob = scales::oob_squish_any
  ) + 
  geom_sf(data=states, fill=NA, col="black", lwd=0.5) + 
  geom_sf(data=aoisShapes[[1]], fill=NA, col="red", lwd=1.2) + 
  coord_sf(xlim = c(st_bbox(aoi_5070)[1], st_bbox(aoi_5070)[3]),
           ylim = c(1036512, (1036512+y_add)),### Add adjustment factor to ymax
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




pdf_file <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/analyses/output/otero_mesa/otero_rept_zoom_scale.pdf"

ggsave(
  pdf_file,
  device = cairo_pdf,
  width = 5.75,
  height = 4.5
)

knitr::plot_crop(pdf_file)


### Get legend colourbar for export
### Make new basic plot with legend 
leg_plot <- ggplot() + 
  geom_spatraster(data = cropped_rast) + 
  scale_fill_whitebox_c(
    palette = "muted",
    na.value = NA,
    limits=c("Low" = my_lims[1], "High"=my_lims[2])) + 
  guides(fill = guide_colourbar(
    direction = "horizontal",
    title.position = "right",
    label.position = "bottom",
    ticks=F,
    nrow = 1,
    title = "",
    override.aes = list(alpha = 0.9)
  )) + 
  theme(legend.key.width = unit(2.5, "cm"),
        legend.key.height=unit(1, "cm"))

ind_legend <- get_legend(leg_plot)
gg_leg <- as_ggplot(ind_legend)


pdf_file <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/analyses/output/otero_mesa/continuous_muted_legend.pdf"

ggsave(
  pdf_file,
  device = cairo_pdf,
  width = 8.5,
  height = 1.5,
  units="in"
)

knitr::plot_crop(pdf_file)
