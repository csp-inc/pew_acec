##########################################
## CALC AREAS IN IBA ##
##########################################

acre_in_km <- 247.105

### Get ACEC polygon
aoisShapes <- list(musselshell)
aoisNames <- c(
  "Musselshell Breaks"
)

a <- aoisShapes[[1]]; print(aoisNames[1])

### Retrieve the IBA shapefile for NM
iba <- iba_mt

# Calculate the overlap of the IBA with the proposed ACEC 
ibaAreaAoi <- st_as_sf(iba) %>%
  st_intersection(st_as_sf(a)) %>%
  as_Spatial() %>%
  terra::area() %>%
  sum()#/1000000

# Of all IBA area in the state, AOI has x %.
ibaArea <- iba %>% as_Spatial() %>% terra::area() %>% sum()#/1000000
(ibaPercAoi <- ibaAreaAoi/ibaArea)

(ibaAreaAoi/1000000*acre_in_km)

### Overlap with the Otero Mesa IBA in particular 
otero_iba <- iba_nm %>%
  dplyr::filter(SITE_NAME == "Otero Mesa")

otero_iba_area <- otero_iba %>% as_Spatial() %>% terra::area()
otero_iba_AreaAoi <- otero_iba %>%
  st_intersection(a) %>%
  as_Spatial() %>%
  terra::area() %>%
  sum()

(OteroibaPercAoi <- (otero_iba_AreaAoi/otero_iba_area))

