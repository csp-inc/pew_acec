
# maybe necessary if some raster aren't loading
# unlink(".RData")

######################################
## DATA COLLECTION & PRE-PROCESSING ##
######################################


#-------------------------------------------------------------------------------
## Function to load features, set common crs, and fix any invalid geometries
load_f <- function(f) {
  proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  read_sf(f) %>%
    st_transform(proj.crs) %>%
    st_make_valid() %>%
    st_buffer(dist = 0)
}

proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"





#-------------------------------------------------------------------------------
# Load CONUS; retain western states.
usa <- load_f(paste0(data.dir, "/working/tl_2012_us_state.shp"))
keeps <- c("Washington", "Oregon", "California", "Idaho", "Montana",
           "Wyoming", "Nevada", "Utah", "Colorado", "Arizona", "New Mexico")
west <- usa %>% filter(NAME %in% keeps)
nv <- usa %>% filter(NAME == "Nevada") %>% as_Spatial() 
nvArea <- terra::area(nv) %>% sum()/1000000
remove(usa, keeps)




#-------------------------------------------------------------------------------
# Load sagebrush biome; clip to west
#sb <- load_f(paste0(local.data.dir,"eco/US_Sagebrush_Biome_2019.shp")) %>% st_crop(west)




#-------------------------------------------------------------------------------
# Load blm; convert to polygon using stars package; fix spatial issues (see links 00_setup)
# Ref: https://r-spatial.github.io/stars/
# Ref: https://gis.stackexchange.com/questions/192771/how-to-speed-up-raster-to-polygon-conversion-in-r

# sf::sf_use_s2(FALSE)
# 
# blmWest <- raster(paste0(data.dir, "working/blm_west.tif"))
# blmWest <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(blmWest),
#                               as_points = FALSE, merge = TRUE)) %>%
#   st_as_sf() %>%
#   st_make_valid() %>%
#   ensure_multipolygons() %>%
#   st_buffer(dist = 0) %>%
#   st_transform(proj.crs)
# 
# # Set non-BLM lands to NA (currently 0)
# blmWest[blmWest$blm_west == 0,] <- NA 
# # Remove empty geometries
# blmWest <- blmWest %>% filter(!st_is_empty(.))
#
# # Generate Wyo-only layer (crop only gives bounding rectangle; intersect instead)
# blmWyo <- blmWest %>% st_intersection(st_as_sf(wyo)) 
# blmMT <- blmWest %>% st_intersection(st_as_sf(mt))
#
# 
# # Write BLMWest and BLM Wyo layer
# shapefile(as_Spatial(blmWest), paste0(data.dir,"working/blm_west.shp")) 
# shapefile(as_Spatial(blmWyo), paste0(data.dir,"working/blm_wyo.shp"))
# shapefile(as_Spatial(blmMT), paste0(data.dir,"working/blm_mt.shp"))


blmWest <- load_f(paste0(data.dir, "/working/blm_west.shp"))
blmNV <- load_f(paste0(data.dir, "/working/blm_nv.gpkg"))

#-------------------------------------------------------------------------------
## Load AOIs

bahsahwahbee <- load_f("/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/data/Bahsahwahbee_NV/BahsahwahbeeNM110822.shp") %>%
  as_Spatial() %>%
  aggregate() %>%
  st_as_sf()

#-------------------------------------------------------------------------------
## Load indicators
setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/data/working")

(list <- list.files(pattern= ".tif"))

# Load and process spp richness. These are orig from:
# https://www.sciencebase.gov/catalog/item/5bef2935e4b045bfcadf732c
# See notes at bottom re: mis-matched CRS from GEE.
# Only amph is given as an eample below, but did amph, bird, mamm, rept.
# 
# start <- Sys.time()
# (amph_west <- raster(paste0(local.data.dir,
#                        "spp/amphibian_richness_habitat30m.tif")) %>% crop(west) %>% mask(west))
# writeRaster(amph_west, "amph_west.tif")
# (end <- start - Sys.time())
# start <- Sys.time()
# amph_west_270m <- amph_west %>% aggregate(fact = 9, fun = mean) # 30m --> 270m
# writeRaster(amph_west_270m, "amph_west_270m.tif", overwrite = TRUE)
# (end <- start - Sys.time())


(amph <- raster("amph_west_270m.tif")) 
(bird <- raster("bird_west_270m.tif"))
(mamm <- raster("mamm_west_270m.tif"))
(rept <- raster("rept_west_270m.tif"))


(impSpp <- raster("impSppNorm.tif")) ; crs(impSpp) <- proj.crs


# Ecological connectivity, intactness, ecosystem div.
(connect <- raster("connNorm.tif"))
(intact <- raster("intactNorm.tif"))
(ecoRar <- raster("ecorarityaggto270norm.tif"))
(ecoRar <- ifel(rast(ecoRar)==1,NA, rast(ecoRar)))
(vegDiv <- raster("gapdiv270mnorm.tif"))


# Sage & annual herb

### Neither sage brush nor annual herbaceous cover layers on file extend to southern New Mexico (outside of the sagebrush biome)
(sage <- raster("sage_270m.tif"))
(annHerb <- raster("annHerb_270m.tif"))


# Clim
(climAcc <- raster("ClimAccNorm.tif"))
(climStab <- raster("ClimStabNorm.tif"))


# Geophys
(geoDiv <- raster("div_ergo_lth270mnorm.tif"))
(geoRar <- raster("georarity270mnorm.tif"))

 
# Water
(waterAvail <- raster("wateravail_allwater2.tif"))
(waterFut <- raster("wateruseddwaterdist2norm.tif"))


# Nat res
(geotherm <- raster("geotherm_lt10pslop_nourbFWPAspldist.tif"))
(oilGas <- raster("oilgas5k6cellmean270mnorm_PAs0UrbH20.tif"))
(mineral <- raster("mrdsPA5kmeanmnormPAs0UrbH20.tif"))
(solar <- raster("maxdnighi_lt5pslope_ddpowerline4normPAs0UrbH20.tif"))
(wind <- raster("windprobi_lt30pslope_ddpowerline4normPAs0UrbH20MULT.tif"))


# Misc
### Night sky darkness is presented originally as 0=dark 1=brightest -- invert raster to assign high values to darker areas
(nightDark <- raster("virrs2011.tif"))
(nightDark <- (1-nightDark))



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

## Spp richness CRS conundrum.
# These are orig from https://www.sciencebase.gov/catalog/item/5bef2935e4b045bfcadf732c
# I had downloaded, from SciBase, uploaded to GEE, then re-downloaded clipped and at lower res via Colab.
# In GEE/Colab, projection is given as:
# 'PROJCS["NAD_1983_Albers",
# \n  GEOGCS["NAD83",
# \n    DATUM["North_American_Datum_1983",
# \n      SPHEROID["GRS 1980", 6378137.0, 298.2572221010042, AUTHORITY["EPSG","7019"]],
# \n      AUTHORITY["EPSG","6269"]],
# \n    PRIMEM["Greenwich", 0.0],
# \n    UNIT["degree", 0.017453292519943295],
# \n    AXIS["Longitude", EAST],
# \n    AXIS["Latitude", NORTH],
# \n    AUTHORITY["EPSG","4269"]],
# \n  PROJECTION["Albers_Conic_Equal_Area"],
# \n  PARAMETER["central_meridian", -96.0],
# \n  PARAMETER["latitude_of_origin", 23.0],
# \n  PARAMETER["standard_parallel_1", 29.5],
# \n  PARAMETER["false_easting", 0.0],
# \n  PARAMETER["false_northing", 0.0],
# \n  PARAMETER["standard_parallel_2", 45.5],
# \n  UNIT["m", 1.0],
# \n  AXIS["x", EAST],
# \n  AXIS["y", NORTH]]'}
# Yet loading the GEE/Colab export in here gives a crs of:
# +proj=aea +lat_0=0 +lon_0=0 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 
# So somehow export screwed this up? Assigning crs that matches GEE/Colab AND USGS orig works:
# +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs
