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
# n=2073

rb_fld_sf <- rb_covar %>%
  filter(field_or_museum=="Field") %>%
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
h12_cent_rb <- h12_cent %>% filter(huc12 %in% c(unique(rb_fld_sf$huc12)))

# select h12s that have data (polys)
h12_rb <- h12 %>% filter(huc12 %in% c(unique(rb_fld_sf$huc12)))

#mapview(h12_rb) + mapview(h12_cent, col.regions="gray", cex=0.2)

# SATSCAN FIELD  ----------------------------------------------

moddir <- "output/satscan/20220817"
modrun <- "rb_bd_bern_h12_field_50p_50k_10min"

# satscan data pts (24 unique h12s)
fld_pts <- st_read(here(glue("{moddir}/{modrun}.gis.shp")), quiet = TRUE) %>% filter(P_VALUE<0.05)

# just the cluster centroids
fld_col <- st_read(here(glue("{moddir}/{modrun}.col.shp")), quiet = TRUE) %>% filter(P_VALUE<0.05)

# all pts outside (non sig clusters)
pts_outside <- st_read(here(glue("{moddir}/{modrun}.rr.dbf")), quiet = TRUE)

# reorder south to north (cluster id)
fld_col <- fld_col %>%
  arrange(LATITUDE) %>%
  mutate(CLUSTER_REV = seq(1:nrow(.)), .after="CLUSTER")

# reorder south to north (cluster id)
fld_pts <- fld_pts %>%
  left_join(., (fld_col %>% st_drop_geometry() %>% select(CLUSTER, CLUSTER_REV)), by="CLUSTER") %>%
  relocate(CLUSTER_REV, .after="CLUSTER")

# merge w hucs
pts_outside <- inner_join(h12_cent, pts_outside, by=c("huc12"="LOC_ID")) %>%
  select(name, huc12, OBSERVED:REL_RISK) %>% filter(REL_RISK == 0)

# get just centroids
fld_sig <- fld_col %>% filter(P_VALUE<0.1) %>%
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
  st_drop_geometry() %>%
  st_as_sf(coords=c("lon","lat"), crs=4269, remove=FALSE)

# first we filter to sig clusters (p<0.05) then join to give membership
df_fld <- left_join(rb_fld_sf, st_drop_geometry(fld_pts),
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

# mapview::mapview(df_fld, zcol="CLUSTER_REV") +
#   mapview(fld_col, zcol="CLUSTER_REV", burst=TRUE) +
#   mapview(h12_rb, col.regions="gray", cex=0.4)

table(df_fld$CLUSTER_REV, useNA="always")

# add clust rate to fld_pts and fld_col
fld_col <- left_join(fld_col, df_fld %>% st_drop_geometry() %>%
                       select(CLUSTER_REV, clustrate) %>%
                       distinct())

fld_pts <- left_join(fld_pts, df_fld %>% st_drop_geometry() %>%
                       select(CLUSTER_REV, clustrate) %>%
                       distinct())


# TMAP: Get Basemap Layers ------------------------------------------------------

library(USAboundaries)
# get counties and states
ca_co<-us_counties(states=c("ca","or"))
ca<-us_states(states=c("ca","or"))
load("data/major_rivers_dissolved.rda")

# buffer out bbox for background
bd_buff <- rb_fld_sf %>%
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

# Map Field Clusters: Equal----------------------------------------------

cols_rb <- rev(get_brewer_pal("RdBu", n = 2))# c('#B2182B', '#5AA2CB')

# subset to HUCs of interest
fld_hucs <- h12_rb %>% filter(huc12 %in% fld_pts$LOC_ID)

map1_fld <-
  map_ca_nobase +
  tm_grid(n.x = 0, n.y = 5, labels.size = 1) +
  # all pts with data (fld)
  tm_shape(pts_outside) +
  tm_sf(border.col = "black", col = "gray60", shape=21, size=0.1) +
  # color by cluster
  # tm_shape(fld_pts) +
  # tm_symbols(col="CLUSTER_REV", palette="viridis",
  #            legend.col.show = FALSE,
  #            size=0.3, alpha=0.5) +
  # color the same
  tm_shape(fld_pts) +
  tm_symbols(col="clustrate", palette=cols_rb,
             legend.col.show = FALSE,
             size=0.4, alpha=0.5) +
  # color by cluster rate
  tm_shape(fld_col) +
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
            title.position = c(0.6, 0.9))
  #tm_compass(type = "4star", position = c("left","bottom"), text.size = 1) +
  #tm_scale_bar(position = c("left","bottom"), text.size = 1)

map1_fld

tmap_save(map1_fld,
          filename = "figs/Fig_03_field_satscan_clusters.jpg",
          width = 8, height = 11, dpi=300)


# Map Field Clusters: By RR -------------------------------------

# rescale a radius?
fld_col$rad_rescale <- (scales::rescale((fld_col$RADIUS + 1), to = c(10, 100)))

map2_fld <-
  map_ca_nobase +
  tm_grid(n.x = 0, n.y = 5, labels.size = 1) +
  # all pts with data (fld)
  tm_shape(rb_fld_sf) +
  tm_sf(border.col = "black", col = "gray60", shape=21, size=0.1) +
  # color the same
  tm_shape(fld_pts) +
  tm_symbols(col="clustrate", palette=cols_rb,
             legend.col.show = FALSE,
             size=0.4, alpha=0.5) +
  # color by cluster rate and size by RR
  tm_shape(fld_col) +
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
            legend.position = c(0.6,0.6), # w hist
            #legend.position = c(0.65,0.8), # no hist
            title.position = c(0.6, 0.9))
  #tm_compass(type = "4star", position = c("left","bottom"), text.size = 1) +
  #tm_scale_bar(position = c("left","bottom"), text.size = 1)

map2_fld

# save
tmap_save(map2_fld,
          filename = "figs/Fig_03_field_satscan_clusters_by_radius_w_rb_pts.jpg",
          width = 8, height = 11, dpi=300)


# Save out ----------------------------------------------------------------

save(map_ca_nobase, map1_fld, map2_fld, file="data/satscan_fld_maps.rda")
