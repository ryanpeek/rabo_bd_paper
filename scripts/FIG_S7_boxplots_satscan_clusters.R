# Map w satscan clusters


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(glue)
library(janitor)
library(lubridate)
library(here)
library(mapview)
mapviewOptions(fgb = FALSE)

# Load Data ---------------------------------------------------------------

# full covars
rb_covar <- read_rds("output/10a_rb_bd_all_covars_recap_filt.rds")
# n=2073 (dropped 72 individuals from recap dups)

rb_mus_sf <- rb_covar %>%
  filter(field_or_museum=="Museum") %>%
  # drop tadpoles
  filter(life_stage_a_j_m_t %in%
                  c("A","J","J/A", NA)) %>% # n=1524
  st_as_sf(coords=c("longitude_dd", "latitude_dd"), crs=4326, remove=FALSE) %>%
  distinct(latitude_dd, longitude_dd, .keep_all = TRUE)


rb_fld_sf <- rb_covar %>%
  filter(field_or_museum=="Field") %>%
  # drop tadpoles
  filter(life_stage_a_j_m_t %in%
           c("A","J","J/A", NA)) %>% # n=1524
  st_as_sf(coords=c("longitude_dd", "latitude_dd"), crs=4326, remove=FALSE) %>%
  distinct(latitude_dd, longitude_dd, .keep_all = TRUE)

# Get HUC10 Centroids -----------------------------------------------------

# get h10 names
load("/Users/rapeek/Documents/github/r_officehours/data/ca_huc10_wbd.rda")

#get H10 for geo file:
h10 <- st_read("output/rabo_bd_spatial_data.gpkg", "HUC10") %>%
  mutate(lon_cent=map_dbl(geom, ~st_centroid(.x)[[1]]),
         lat_cent=map_dbl(geom, ~st_centroid(.x)[[2]]))

h10_cent <- h10 %>% st_drop_geometry() %>%
  dplyr::select(huc10, lon_cent, lat_cent) %>%
  st_as_sf(coords=c("lon_cent", "lat_cent"), crs=4326, remove=FALSE)

# selecth12s that have data (centroids)
h10_mus_cent_rb <- h10_cent %>% filter(huc10 %in% c(unique(rb_mus_sf$huc10)))

# select h12s that have data (polys)
h10_mus_rb <- h10 %>% filter(huc10 %in% c(unique(rb_mus_sf$huc10)))


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

#mapview(h12_mus_rb, col.regions="yellow") + mapview(h12_cent, col.regions="gray", cex=0.2)

# SATSCAN FIELD  ----------------------------------------------

moddir <- "output/satscan/20220817/"
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

# SATSCAN MUSEUM  ----------------------------------------------

moddir <- "output/satscan/20220817/"
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


# Summary Table of Years and Watersheds -----------------------------------

#satscan clusters that includes the years of positives and negatives for each cluster, and the watersheds they include
table(df_fld$capt_yr)
table(df_fld$bd_positive)

fld_summary <- df_fld %>% st_drop_geometry() %>%
  select(capt_yr, bd_positive, clustrate, CLUSTER_REV, huc10, huc12, h12_name, NAME_HUC12, geomorph_name) %>%
  group_by(capt_yr, bd_positive, CLUSTER_REV) %>% tally() %>%
  group_by(CLUSTER_REV, bd_positive) %>%
  summarize(min_yr = min(capt_yr),
            max_yr = max(capt_yr)) %>%
  mutate(field_or_museum = "Field")

write_csv(fld_summary, file = "output/satscan_cluster_yr_summary_field.csv")

fld_summary_huc <-
  df_fld %>% st_drop_geometry() %>%
  select(capt_yr, bd_positive, clustrate, CLUSTER_REV, huc10, huc12, h12_name, NAME_HUC12, geomorph_name) %>%
  group_by(bd_positive, CLUSTER_REV, huc10) %>% tally() %>%
  filter(CLUSTER_REV!=0) %>% #View()
  left_join(h10, by=c("huc10")) %>%
  ungroup() %>%
  mutate(field_or_museum = "Field") %>%
  select(1:4, field_or_museum, huc10) # add shape for sf

write_csv(fld_summary_huc, file = "output/satscan_cluster_huc_summary_field.csv")

mus_summary <- df_mus %>% st_drop_geometry() %>%
  select(capt_yr, bd_positive, clustrate, CLUSTER_REV, huc10, huc12, h12_name, NAME_HUC12, geomorph_name) %>%
  group_by(capt_yr, bd_positive, CLUSTER_REV) %>% tally() %>%
  group_by(CLUSTER_REV, bd_positive) %>%
  summarize(min_yr = min(capt_yr),
            max_yr = max(capt_yr)) %>%
  mutate(field_or_museum = "Museum")

write_csv(mus_summary, file = "output/satscan_cluster_yr_summary_museum.csv")


mus_summary_huc <-
  df_mus %>% st_drop_geometry() %>%
  select(capt_yr, bd_positive, clustrate, CLUSTER_REV, huc10, huc12, h12_name, NAME_HUC12, geomorph_name) %>%
  group_by(bd_positive, CLUSTER_REV, huc10) %>% tally() %>%
  filter(CLUSTER_REV!=0) %>% #View()
  left_join(h10, by=c("huc10")) %>%
  ungroup() %>%
  mutate(field_or_museum = "Museum") %>%
  select(1:4, field_or_museum, huc10) # add shape for sf

write_csv(mus_summary_huc, file = "output/satscan_cluster_huc_summary_museum.csv")


#mus_summary_huc %>%
#  st_as_sf()
#mapview(mus_summary_huc, zcol="CLUSTER_REV") +
#  mapview(mus_summary_huc, zcol="bd_positive")




# Boxplots ----------------------------------------------------------------

# museum cluster by year
(boxMus <- ggplot() + geom_boxplot(data=df_mus, aes(y=capt_yr, fill=as.factor(bd_positive), group=bd_positive), alpha=0.7, show.legend = TRUE) +
  facet_grid(~CLUSTER_REV) +
  theme_classic() +
  ggthemes::scale_fill_colorblind("Bd+") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())+
  labs(y="Year Sampled",
       title="Museum Samples by Cluster",
       caption="Clusters South to North, 0 = no cluster"))

# create strip colors (high='#B2182B', low='#5AA2CB')
g <- ggplot_gtable(ggplot_build(boxMus))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- alpha(c('white','#5AA2CB', '#5AA2CB', '#5AA2CB'), 0.7)
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
gg_mus <- gridExtra::arrangeGrob(g)
grid::grid.draw(g)
ggsave(gg_mus, filename = "figs/S7_museum_samples_by_cluster_years_boxplot_col.png",
       width = 10, height = 8, dpi=300)

ggsave(filename = "figs/S7_museum_samples_by_cluster_years_boxplot.png",
       width = 10, height = 8, dpi=300)


# make median lines slightly lighter in color, and color cluster boxes by high/low rate
(boxFld <- ggplot() +
    geom_boxplot(data=df_fld, aes(y=capt_yr, fill=as.factor(bd_positive), group=bd_positive), alpha=0.7,outlier.fill = "gray40", show.legend = TRUE) +
  facet_grid(~CLUSTER_REV) +
  theme_classic() +
  ggthemes::scale_fill_colorblind("Bd+") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())+
  labs(y="Year Sampled",
       title="Field Samples by Cluster",
       caption="Clusters South to North, 0 = no cluster"))

# add strips
# create strip colors (high='#B2182B', low='#5AA2CB')

# look at what is high or low:
df_fld %>% st_drop_geometry() %>%
  group_by(CLUSTER_REV, clustrate) %>%
  tally() %>%
  mutate(ratecols = case_when(
    clustrate == 2 ~ "#B2182B",
    clustrate == 1 ~"#5AA2CB",
    TRUE ~ "white")) -> df_fld_cols
df_fld_cols

g <- ggplot_gtable(ggplot_build(boxFld))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- c(alpha(df_fld_cols$ratecols,alpha = 0.7))
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
gg_mus <- gridExtra::arrangeGrob(g)
grid::grid.draw(g)
ggsave(gg_mus, filename = "figs/S7_field_samples_by_cluster_years_boxplot_col.png",
       width = 10, height = 8, dpi=300)

# save
ggsave(filename = "figs/S7_field_samples_by_cluster_years_boxplot.png",
       width = 10, height = 8, dpi=300)


# patch -------------------------------------------------------------------

library(patchwork)
boxFld/boxMus

ggsave(filename="figs/S7_combined_samples_by_cluster_years_boxplot.png",
       width = 10, height = 8, dpi=300)
