# today <- paste0(mid(Sys.Date(),3,2),
#                 mid(Sys.Date(),6,2),
#                 mid(Sys.Date(),9,2))

today <- paste0(Sys.Date())


#########################################################
## EXTRACT VALUES OF CONSERVATION INDICATORS TO SAMPLE ##
#########################################################

## Add vars to list, var names to vector --------------------------------------

varsRasters <- list(amph, bird, mamm, rept, impSpp, connect,
                    intact, ecoRar, vegDiv, sage, annHerb,
                    climAcc, climStab, geoDiv, geoRar,
                    geotherm, oilGas, mineral, solar, wind,
                    waterAvail, waterFut, nightDark, aquifer_vuln, erosion)


namesRasters <- c("amph", "bird", "mamm", "rept", "impSpp", "connect",
                  "intact", "ecoRar", "vegDiv", "sage", "annHerb",
                  "climAcc", "climStab", "geoDiv", "geoRar",
                  "geotherm", "oilGas", "mineral", "solar", "wind",
                  "waterAvail", "waterFut", "nightDark", "aquifer_vuln", "erosion")

## Domain selection -----------------------------------------------------------

domains <- list(st_as_sf(west),
                blmWest,
                st_as_sf(nm),
                blmNM)
dNames <- c("west",
            "blmWest",
            "nm",
            "blmNM")

# domains <- list(st_as_sf(west),
#                 blmWest,
#                 st_as_sf(mt),
#                 blmMT)
# dNames <- c("west",
#             "blmWest",
#             "MT",
#             "blmMT")


## Sample size selection to match domains -------------------------------------

ns <- c(2000, 2000, 500, 500)
#ns <- c(20, 20, 5, 5)


## AOI selection --------------------------------------------------------------

aoisShapes <- list(otero)
aoisNames <- c(
  "Otero Mesa"
)

# aoisShapes <- list(lewis)
# aoisNames <- c("lewis")


## Loop to extract values------------------------------------------------------

# Empty vectors
an <- as.character()
dn <- as.character()
nv <- as.numeric()
vn <- as.character()
av <- as.numeric()
sv <- as.numeric()
sv.medians <- as.numeric()
sv.means <- as.numeric()
pv <- as.numeric()
cv <- as.numeric()
ev <- as.numeric()


# REFS:
# Options for extracting rank/percentile:
# Ref: https://stackoverflow.com/questions/41087162/calculate-a-percentile-of-dataframe-column-efficiently
# Choosing percent rank in loop below, but could consider ecdf, here:
# Define function for computing empirical cumulative distribution and querying val.
# ecdf_fun <- function(x,perc) ecdf(x)(perc)
# ecdf_fun(samp_vals, aoi_vals)

# FIXME (possibly):
# Consider hard-wiring random sample for reproducibility via set.seed()

start <- Sys.time()
# Work thru each of the AOIs
for (i in 1:length(aoisShapes)){
# for (i in 1){
  # Iteratively select AOIs
  a <- aoisShapes[[i]]
  # Compute AOI area
  trgt_area <- sum(terra::expanse(vect(a), unit = "m")) # sum in case multi-part 
  # Work thru each of the domains.
  for (j in 1:length(domains)){
    # Within a given domain, create n sample points
    pts = sf::st_sample(domains[[j]], size = ns[j])
    # Grow those sample points to match AOI area.
    sample <- gBuffer(as_Spatial(pts),
                    width = sqrt(trgt_area/(3.14)), # grow by AOI radius
                    byid = TRUE) %>% st_as_sf()
    # Work thru each of the indicator rasters, extracting values
    for (k in 1:length(varsRasters)){
    # for (k in 1:5){
      # Pull name of AOI and append to vector
      an <- c(an, aoisNames[i])
      print(paste0("AOI is ", aoisNames[i]))
      # Pull name of domain and append to vector
      dn <- c(dn, dNames[j])
      print(paste0("Domain is ", dNames[j]))
      # Pull sample size and append to vector
      nv <- c(nv, ns[j])
      print(paste0("Sample size is ", ns[j]))
      # Pull name of variable and append to vector
      vn <- c(vn, namesRasters[k])
      print(paste0("Variable is ",namesRasters[k]))
      # Extract mean value of variable within AOI
      av.temp <- exact_extract(varsRasters[[k]], a, fun = "mean")
      # Append that value to vector
      av <- c(av, av.temp)
      # Extract mean values of variable for all random sample points; as vector
      sv.temp <- exact_extract(varsRasters[[k]], sample, fun = "mean") %>% round(2)
      # Get average across all random samples
      sv.mean <- mean(sv.temp, na.rm = TRUE) %>% round(2)
      # Get median across all random samples
      sv.median <- median(sv.temp, na.rm = TRUE) %>% round(2)
      # Bind those vectors.
      sv <- rbind(sv, sv.temp)
      # Bind those means
      sv.means <- c(sv.means, sv.mean)
      # Bind those medians
      sv.medians <- c(sv.medians, sv.median)
      # Extract perc rank of the aoi value relative to all sample values (first one)
      pv.temp <- percent_rank(c(av.temp, sv.temp))[1] %>% round(2)
      # Extract cumulative dist rank
      cv.temp <- cume_dist(c(av.temp, sv.temp))[1] %>% round(2)
      # Create empirical cumulative distribution function from those sample values
      ef <- ecdf(sv.temp)
      # # Extract the percentile of the AOI value within that distribution
      ev.temp <- ef(av.temp) %>% round(2)
      # Append those percentiles to a vector
      pv <- c(pv, pv.temp)
      cv <- c(cv, cv.temp)
      ev <- c(ev, ev.temp)
    }
  }
}
(end <- start - Sys.time()) 


# Bind all together into dataframe
foo <- cbind(an, dn, nv, vn, av, sv.means, sv.medians, pv, cv, ev) %>% as.data.frame()
foo[c(3,5:10)] <- lapply(foo[c(3,5:10)], as.numeric) # Set numbers to numbers.

v <- 2
# v <- v+1
write.csv(foo, paste0(out.dir, "OteroMesa", "_aoi_vs_sample_percentiles_", today, "_v",v, ".csv"))




