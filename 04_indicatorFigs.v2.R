
today <- paste0(Sys.Date())

setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1IzmyhjH2hL-DtYsvhTml0HznlsDMF7p6/Pew_ACEC/analyses/output/bodiehills_ca/")

#########################################
## CREATE INDICATOR FIGURES FOR REPORT ##
#########################################

## Load csvs with raw results --------------------------------------------------

data <- read.csv(paste0(out.dir, "BodieHills_aoi_vs_sample_percentiles_2023-06-01_v1.csv")) %>%
  dplyr::select(an, dn, nv, vn, pv)

## Assign categories/labels ----------------------------------------------------

# Assign value/threat
threats <- c("annHerb", "geotherm", "oilGas", "wind", "solar", "mineral", "waterFut")
data <- data %>%
  mutate(type = ifelse(vn %in% threats, "threat", "value"))

# Assign full labels
lu <-
  data.frame(
    layer = c(
      "amph",
      "bird",
      "mamm",
      "rept",
      "impSpp",
      "connect",
      "intact",
      "ecoRar",
      "vegDiv",
      "sage",
      "annHerb",
      "climAcc",
      "climStab",
      "geoDiv",
      "geoRar",
      "geotherm",
      "oilGas",
      "mineral",
      "solar",
      "wind",
      "waterAvail",
      "waterFut",
      "nightDark"
    ),
    variable = c(
      "Amphibian species richness",
      "Bird species richness",
      "Mammal species richness",
      "Reptile species richness",
      "Imperiled species richness",
      "Ecological connectivity",
      "Ecological intactness",
      "Ecosystem rarity",
      "Vegetation diversity",
      "Percent sagebrush cover",
      "Percent annual herbaceous cover",
      "Climate accessibility",
      "Climate stability",
      "Geophysical diversity",
      "Geophysical rarity",
      "Geothermal resource potential",
      "Oil and gas resource potential",
      "Mineral resource potential",
      "Solar resource potential",
      "Wind resource potential",
      "Water availability",
      "Future water withdrawals",
      "Night sky darkness"
    )
  )
  
## Link value names - remove ecosystem rarity due to edge effects 
data <- data %>% left_join(lu, by = c("vn" = "layer")) 

# Pull colors for green (Value) and red (threat) gradients
# Source: https://colordesigner.io/gradient-generator
# Define list of colors (trial and error showed reverse necessary)
# Length for EACH must match # of indicators
col_value <- c("#056e4f", # dark green
               "#167455",
               "#22795c",
               "#2c7f62",
               "#358569",
               "#3e8a70",
               "#469076",
               "#4f967d",
               "#579c84",
               "#5fa18a",
               "#67a791",
               "#6fad98",
               "#77b39f",
               "#7fb9a6",
               "#87bfad",
               "#8fc5b3",
               "#97cbba",
               "#9fd1c1",
               "#a8d7c8",
               "#b0ddcf",
               "#b8e3d6",
               "#c1e9dd",
"#c9efe4") # light green

col_threat <- c("#c81e43", #dark red
               "#cb2b4a",
               "#ce3651",
               "#d14058",
               "#d4495f",
               "#d65266",
               "#d95a6d",
               "#db6274",
               "#dd6a7b",
               "#df7182",
               "#e17989",
               "#e38090",
               "#e58797",
               "#e68f9d",
               "#e796a4",
               "#e99dab",
               "#eaa4b1",
               "#ebabb7",
               "#ecb3be",
               "#edbac4",
               "#eec1ca",
               "#eec8d0",
"#efcfd6") #light red



## Select AOI/sampling domain combo (one of each!!) ---------------------------

# Filter data to given AOI and sampling domain
sel <- data %>%
  filter(
          an == "Bodie Hills",
         # dn == "west"
         # dn == "blmWest"
         # dn == "ca"
           dn == "blmCA"
         )

# Order data by percentile ranks (greatest first)
sel <- sel[order(sel$pv, decreasing = FALSE), ] 
# Lock the ranks in by setting variable  as a factor based on that order.
sel$variable <- factor(sel$variable, levels = sel$variable, ordered = TRUE)

# Look-up the color lookups and assign threat color (reds) or value (greens)
# Magically, it pulls the color corresponding to the index of sel$type
sel <- sel %>%
  mutate(color = ifelse(sel$type == "value", rev(col_value), rev(col_threat)))

# Retain only top 8; don't hard-wire with index, use tail()!
sel <- sel %>% tail(8) #(tail b/c pv are in oppo order)


## Plot & save ----------------------------------------------------------------

p <- sel %>%
  # group_by(an) %>% # defines as grouped df
  # arrange(desc(pv), .by_group = TRUE) %>% # sorts & remembers the grp
  ggplot(aes(x = variable, y = pv*100,
             fill = variable)) +
  geom_bar(stat = "identity") +
  # scale_fill_manual(values = sel$color) +
  scale_fill_manual(values = sel$color) +
  coord_flip() +
  # theme_bw() +
  # theme_classic() +
  # theme_minimal() +
  theme(#panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(color = "black", size = 14), #, size = rel(1)),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face ="bold", size = 14)) + #, size = rel(1))) +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  labs(y = "Score") +
  #------------- annotation layer ---------------------
  # the anchor points of the labels are inherited from ggplot() call 
  geom_text(aes(label = paste0(pv*100)), hjust = 1.2)

p

v <- 1
#v <- v+1
ggsave(paste0(out.dir, "top8_", sel$an[1], "_", sel$dn[1], "_n", sel$n[1], "_", today, "_v",v, ".png"),
       p,
       # width = 850, height = 550, units = "px")
       width = 6, height = 6, units = "in")
dev.off()


