west_wide_map_wscale <- function(ind, color.palette, aoi.bbox.fill, scale.text.col){
  big <- ggplot() +
    geom_spatraster_rgb(data=x_terra_masked) + 
    geom_spatraster(data = ind) + 
    scale_fill_whitebox_c(
      palette = color.palette,
      na.value = NA,
      limits=my_lims
      #oob = scales::oob_squish_any
    ) + 
    geom_sf(data=states, fill=NA, col="black", lwd=0.5) + 
    #geom_sf(data=aoisShapes[[1]], fill="red", col="red", lwd=0.5) + 
    geom_sf(data=aoi_bbox, fill=aoi.bbox.fill, col="black", lwd=0.6) + 
    geom_sf(data=mexico_us_canada, col="black", fill=NA, lwd=0.25) + 
    coord_sf(crs=5070,
             xlim = c(st_bbox(states)[1], st_bbox(states)[3]),
             ylim = c(st_bbox(states)[2], st_bbox(states)[4]),
             expand = F) +
    annotation_scale(
      text_family="Calibri",
      text_face="bold",
      aes(text_col=scale.text.col)) +
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
west_wide_map_wscale_squishlims <- function(ind, color.palette, aoi.bbox.fill, aoi.bbox.col, scale.text.col){
  big <- ggplot() +
    geom_spatraster_rgb(data=x_terra_masked) + 
    geom_spatraster(data = ind, maxcell = 7e+05) + 
    scale_fill_whitebox_c(
      palette = color.palette,
      na.value = NA,
      limits=my_lims,
      oob = scales::oob_squish_any
    ) + 
    geom_sf(data=states, fill=NA, col="black", lwd=0.5) + 
    geom_sf(data=aoi_bbox, fill=aoi.bbox.fill, col=aoi.bbox.col, lwd=0.6, alpha=0.25) + 
    geom_sf(data=mexico_us_canada, col="black", fill=NA, lwd=0.25) + 
    coord_sf(crs=5070,
             xlim = c(st_bbox(states)[1], st_bbox(states)[3]),
             ylim = c(st_bbox(states)[2], st_bbox(states)[4]),
             expand = F) +
    annotation_scale( 
      text_family="Calibri",
      text_face="bold",
      aes(text_col=scale.text.col)) +
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

west_wide_map_noscale <- function(ind, color.palette, aoi.bbox.fill){
  big <- ggplot() +
    geom_spatraster_rgb(data=x_terra_masked) + 
    geom_spatraster(data = ind) + 
    scale_fill_whitebox_c(
      palette = color.palette,
      na.value = NA,
      limits=my_lims
      #oob = scales::oob_squish_any
    ) + 
    geom_sf(data=states, fill=NA, col="black", lwd=0.5) + 
    geom_sf(data=aoi_bbox, fill=aoi.bbox.fill, col="black", lwd=0.6) + 
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
west_wide_map_noscale_squishlims <- function(ind, color.palette, aoi.bbox.fill){
  big <- ggplot() +
    geom_spatraster_rgb(data=x_terra_masked) + 
    geom_spatraster(data = ind, maxcell = 7e+05) + 
    scale_fill_whitebox_c(
      palette = color.palette,
      na.value = NA,
      limits=my_lims,
      oob = scales::oob_squish_any
    ) + 
    geom_sf(data=states, fill=NA, col="black", lwd=0.5) + 
    geom_sf(data=aoi_bbox, fill=aoi.bbox.fill, col="black", lwd=0.6, alpha=0.6) + 
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

zoom_map_wscale <- function(cropped_rast, color.palette, scale.text.col, ymin){
  zoom <- ggplot() +
    geom_sf(data=canada, fill="#414141", lwd=0) + 
    geom_sf(data=states, fill="gray", col="black", lwd=0.5) + 
    geom_spatraster(data = cropped_rast) + 
    scale_fill_whitebox_c(
      palette = color.palette,
      na.value = NA,
      limits=my_lims) + 
    geom_sf(data=aoisShapes[[1]], fill=NA, col="red", lwd=1.2) + 
    coord_sf(xlim = c(st_bbox(aoi_5070)[1], st_bbox(aoi_5070)[3]),
             ylim = c(ymin, (ymin+y_add)),### Add adjustment factor to ymax
             expand = T,
             crs=5070
    ) +
    annotation_scale(location="br", 
                     width_hint=0.5,
                     text_family="Calibri",
                     text_face="bold",
                     aes(text_col=scale.text.col)) + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) + 
    theme_void() +
    theme(legend.position="none",
          #panel.background = element_rect(fill = "lightblue",
          #                                colour = "lightblue"),
          panel.grid=element_blank(),
          panel.border=element_blank())
}
zoom_map_wscale_squishlims <- function(cropped_rast,color.palette, scale.text.col, ymin){
  zoom <- ggplot() +
    geom_sf(data=canada, fill="#414141", lwd=0) + 
    geom_sf(data=states, fill="gray", col="black", lwd=0.5) + 
    geom_spatraster(data = cropped_rast) + 
    scale_fill_whitebox_c(
      palette = color.palette,
      na.value = NA,
      limits=my_lims,
      oob = scales::oob_squish_any
    ) + 
    geom_sf(data=aoisShapes[[1]], fill="red", col="red", lwd=1.2, alpha=0.1) + 
    coord_sf(xlim = c(st_bbox(aoi_5070)[1], st_bbox(aoi_5070)[3]),
             ylim = c(ymin, (ymin+y_add)),### Add adjustment factor to ymax
             expand = T,
             crs=5070
    ) +
    annotation_scale(location="br", 
                     width_hint=0.5,
                     text_family="Calibri",
                     text_face="bold",
                     aes(text_col=scale.text.col)) + 
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
zoom_map_noscale <- function(cropped_rast, color.palette, ymin){
  zoom <- ggplot() +
    geom_sf(data=canada, fill="#414141", lwd=0) + 
    geom_sf(data=states, fill="gray", col="black", lwd=0.5) + 
    geom_spatraster(data = cropped_rast) + 
    scale_fill_whitebox_c(
      palette = color.palette,
      na.value = NA,
      limits=my_lims) + 
    geom_sf(data=aoisShapes[[1]], fill=NA, col="red", lwd=1.2) + 
    coord_sf(xlim = c(st_bbox(aoi_5070)[1], st_bbox(aoi_5070)[3]),
             ylim = c(ymin, (ymin+y_add)),### Add adjustment factor to ymax
             expand = T,
             crs=5070
    ) +
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) + 
    theme_void() +
    theme(legend.position="none",
          #panel.background = element_rect(fill = "lightblue",
          #                                colour = "lightblue"),
          panel.grid=element_blank(),
          panel.border=element_blank())
}
zoom_map_noscale_squishlims <- function(cropped_rast, color.palette, ymin){
  zoom <- ggplot() +
    geom_sf(data=canada, fill="#414141", lwd=0) + 
    geom_sf(data=states, fill="gray", col="black", lwd=0.5) +  
    geom_spatraster(data = cropped_rast) + 
    scale_fill_whitebox_c(
      palette = color.palette,
      na.value = NA,
      limits=my_lims,
      oob = scales::oob_squish_any
    ) + 
    geom_sf(data=aoisShapes[[1]], fill="red", col="red", lwd=1.2, alpha=0.15) + 
    coord_sf(xlim = c(st_bbox(aoi_5070)[1], st_bbox(aoi_5070)[3]),
             ylim = c(ymin, (ymin+y_add)),### Add adjustment factor to ymax
             expand = T,
             crs=5070
    ) +
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) + 
    theme_void() +
    theme(legend.position="none",
          #panel.background = element_rect(fill = "lightblue",
          #                                colour = "lightblue"),
          panel.grid=element_blank(),
          panel.border=element_blank())
}
