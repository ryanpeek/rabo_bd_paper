# Map w satscan clusters

# Load Libraries ----------------------------------------------------------

library(tidyverse)
#library(fs)
library(sf)
library(glue)
library(janitor)
library(lubridate)
library(purrr)
library(here)
library(tmap)
library(tmaptools) # for base layers
library(USAboundaries)

# Load Data ---------------------------------------------------------------

# full covars
rb_covar <- read_rds("output/10a_rb_bd_all_covars_recap_filt.rds")
# n=2073 (2145-2073=76)

rb_mus_sf <- rb_covar %>%
  filter(field_or_museum=="Museum") %>%
  # drop tadpoles
  filter(life_stage_a_j_m_t %in%
                  c("A","J","J/A", NA)) %>% # n=1524
  st_as_sf(coords=c("longitude_dd", "latitude_dd"), crs=4326, remove=FALSE) %>%
  distinct(latitude_dd, longitude_dd, .keep_all = TRUE)

# Get HUC12 Centroids -----------------------------------------------------

#get H10 and H12s for geo file:
h12 <- st_read("output/rabo_bd_spatial_data.gpkg", "HUC12") %>%
  mutate(lon_cent=map_dbl(geom, ~st_centroid(.x)[[1]]),
         lat_cent=map_dbl(geom, ~st_centroid(.x)[[2]]))

h12_cent <- h12 %>% st_drop_geometry() %>%
  dplyr::select(areasqkm, name, states, huc12, lon_cent, lat_cent) %>%
  st_as_sf(coords=c("lon_cent", "lat_cent"), crs=4326, remove=FALSE)

# selecth12s that have data (centroids)
h12_mus_cent_rb <- h12_cent %>% filter(huc12 %in% c(unique(rb_mus_sf$huc12)))

# select h12s that have data (polys)
h12_mus_rb <- h12 %>% filter(huc12 %in% c(unique(rb_mus_sf$huc12)))


# SATSCAN MUSEUM  ----------------------------------------------

moddir <- "output/satscan/20210907"
modrun <- "rb_bd_bern_h12_museum_50p_50k_3min"

# don't filter to p<0.05 here, want to include san gabriel cluster
mus_pts <- st_read(here(glue("{moddir}/{modrun}.gis.shp")), quiet = TRUE)
mus_col <- st_read(here(glue("{moddir}/{modrun}.col.shp")), quiet = TRUE) %>%
  # reorder south to north for cluster ID
  arrange(LATITUDE) %>%
  mutate(CLUSTER_REV = seq(1:nrow(.)), .after="CLUSTER")

# reorder south to north (cluster id)
mus_pts <- mus_pts %>%
  left_join(., (mus_col %>% st_drop_geometry() %>% select(CLUSTER, CLUSTER_REV)), by="CLUSTER") %>%
  relocate(CLUSTER_REV, .after="CLUSTER")

# first we filter to sig clusters (p<0.05) then join to give membership
df_mus <- left_join(rb_mus_sf, st_drop_geometry(mus_pts),
                    by = c("huc12"="LOC_ID")) %>%
  select(sampleid:boylii_clade,CLUSTER:CLU_POP, everything()) %>%
  mutate(CLUSTER_REV = as.double(CLUSTER_REV)) %>%
  mutate(CLUSTER_REV = case_when(
    is.na(CLUSTER_REV) ~ 0, # not part of a cluster
    TRUE ~ CLUSTER_REV)) %>%
  mutate(clustrate = case_when(
    CLUSTER_REV==0 ~ 0,
    CLU_RR < 1 ~ 1, # low rate cluster
    CLU_RR >=1 ~ 2 # high rate cluster
  ), .after=bd_positive)

# mapview::mapview(df_mus, zcol="CLUSTER_REV") +
#   mapview(mus_col, zcol="CLUSTER_REV", burst=TRUE) +
#   mapview(h12_mus_rb, col.regions="gray", cex=0.4)

table(df_mus$CLUSTER_REV, useNA="always")

# add clust rate to mus_pts and mus_col
mus_col <- left_join(mus_col, df_mus %>% st_drop_geometry() %>%
                       select(CLUSTER_REV, clustrate) %>%
                       distinct())

mus_pts <- left_join(mus_pts, df_mus %>% st_drop_geometry() %>%
                       select(CLUSTER_REV, clustrate) %>%
                       distinct())


# TMAP: Get Basemap Layers ------------------------------------------------------

library(USAboundaries)
# get counties and states
ca_co<-us_counties(states=c("ca","or"))
ca<-us_states(states=c("ca","or"))
load("data/major_rivers_dissolved.rda")

# buffer out bbox for background
bd_buff <- rb_mus_sf %>%
  st_bbox() %>% # make a boundary box
  st_as_sfc() %>% # this converts bbox to polygon
  st_transform(3310) %>% # need to make it metric for easier buffer
  st_buffer(dist = 50000) %>%  # add 50km buffer
  st_transform(4326) # convert back to lat/lon

# crop rivers to CA only and filter to rivers only
rivs_ca <- st_intersection(rivs, ca) %>%
  filter(FEATURE_TYPE == "river")

# TMAP: Basemap ------------------------------------------------------------

(map_ca_nobase <-
   # for no basemap
   tm_shape(bd_buff, bbox = c(-124.4, 33,-117, 42.5)) + tm_polygons(border.alpha = 0, alpha=0) +
   # states
   tm_shape(ca) + tm_polygons(border.alpha = 0.8, lwd=2,
                              alpha=0, col = "black") +
   # rivers
   tm_shape(rivs_ca) +
   tm_lines(col="steelblue", lwd = .5, alpha = 0.5) +
   tm_layout(frame=FALSE))

# Map Museum Clusters: Equal ----------------------------------------------

cols_rb <- get_brewer_pal("RdBu", n = 2)# c('#B2182B', '#5AA2CB')

# subset to HUCs of interest
mus_hucs <- h12_mus_rb %>% filter(huc12 %in% mus_pts$LOC_ID)

map1_mus <-
  map_ca_nobase +
  tm_grid(n.x = 0, n.y = 5, labels.size = 1) +
  # all pts with data (mus)
  tm_shape(rb_mus_sf) +
  tm_sf(border.col = "black", col = "gray60", shape=21, size=0.1) +
  # color the same
  tm_shape(mus_pts) +
  tm_symbols(col="clustrate", palette=cols_rb,
             legend.col.show = FALSE,
             size=0.4, alpha=0.5) +
  # color by cluster rate
  tm_shape(mus_col) +
  tm_symbols(border.alpha = 0.8, size=8, palette=cols_rb,
              alpha=0.2, col = "clustrate",
             shape = 21, title.col = "Cluster",
             legend.col.show = FALSE) +
  tm_text("CLUSTER_REV", size=2, remove.overlap = TRUE,
          shadow = TRUE, ymod = 0.8, xmod=1.4,
          fontface = "bold", legend.col.show = FALSE) +
  # add layout info:
  tm_layout(frame = FALSE,
            fontfamily = "Roboto Condensed",
            legend.outside = FALSE, attr.outside = FALSE,
            legend.bg.color = TRUE, legend.frame = "gray60",
            inner.margins = 0.02, outer.margins = (0.01),
            legend.position = c(0.6,0.6), # w hist
            #legend.position = c(0.65,0.8), # no hist
            title.position = c(0.6, 0.9)) +
  tm_compass(type = "4star", position = c("left","bottom"), text.size=1) +
  tm_scale_bar(position = c("left","bottom"), text.size=1.1)

map1_mus

tmap_save(map1_mus,
          filename = "figs/Fig_03_museum_satscan_clusters.jpg",
          width = 8, height = 11, dpi=300)


# Map Museum Clusters: By RR -------------------------------------

# rescale a radius?
mus_col$rad_rescale <- (scales::rescale((mus_col$RADIUS + 1), to = c(10, 100)))

map2_mus <-
  map_ca_nobase +
  tm_grid(n.x = 0, n.y = 5, labels.size = 1) +
  # all pts with data (mus)
  tm_shape(rb_mus_sf) +
  tm_sf(border.col = "black", col = "gray60", shape=21, size=0.1) +
  # color the same
  tm_shape(mus_pts) +
  tm_symbols(col="clustrate", palette=cols_rb,
             legend.col.show = FALSE,
             size=0.4, alpha=0.5) +
  # color by cluster rate and size by RR
  tm_shape(mus_col) +
  tm_symbols(border.alpha = 0.8, size="rad_rescale", scale=6,
             palette=cols_rb, legend.size.show = FALSE,
             alpha=0.2, col = "clustrate",
             shape = 21, title.col = "Cluster",
             legend.col.show = FALSE) +
  tm_text("CLUSTER_REV", size=2, remove.overlap = TRUE,
          shadow = TRUE, ymod = 0.8, xmod=1.4,
          fontface = "bold", legend.col.show = FALSE) +
  # add layout info:
  tm_layout(frame = FALSE,
            fontfamily = "Roboto Condensed",
            legend.outside = FALSE, attr.outside = FALSE,
            legend.bg.color = TRUE, legend.frame = "gray60",
            inner.margins = 0.02, outer.margins = (0.01),
            legend.position = c(0.6,0.6),
            title.position = c(0.6, 0.9)) +
  tm_compass(type = "4star", position = c("left","bottom"), text.size = 1) +
  tm_scale_bar(position = c("left","bottom"), text.size = 1.1)

map2_mus

# save
tmap_save(map2_mus,
          filename = "figs/Fig_03_satscan_museum_clusters_by_radius_w_rb_pts.jpg",
          width = 8, height = 11, dpi=300)

# Save out ----------------------------------------------------------------

save(map_ca_nobase, map1_mus, map2_mus, file="data/satscan_mus_maps.rda")



# Create Side by Side Plot -----------------

load("data/satscan_fld_maps.rda")
load("data/satscan_mus_maps.rda")

map_combined <- tmap::tmap_arrange(map2_mus, map2_fld)
map_combined

# save out
tmap::tmap_save(map_combined, filename = "figs/Fig_03_combined_satscan_maps.png", width = 11, height = 8, dpi=300)
tmap::tmap_save(map_combined, filename = "figs/Fig_03_combined_satscan_maps.pdf", width = 11, height = 8, dpi=300, device = cairo_pdf)
