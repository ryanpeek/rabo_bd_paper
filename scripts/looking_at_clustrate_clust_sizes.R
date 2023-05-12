# getting cluster rate group sizes
library(tidyverse)
library(sf)

# base data set:
rb_covar <- read_rds(here("output", "09_rb_bd_all_covariates_joined.rds"))

# museum data
rb_mus <- rb_covar %>%
  mutate( sampleid2 = janitor::make_clean_names(sampleid), .after=sampleid) %>%
  filter(field_or_museum=="Museum") %>% #n=461
  st_as_sf(coords=c("longitude_dd", "latitude_dd"), crs=4269, remove=FALSE)

# field data
rb_fld <- rb_covar %>%
  mutate( sampleid2 = janitor::make_clean_names(sampleid), .after=sampleid) %>%
  filter(field_or_museum=="Field") %>% # n=1635
  # drop tadpoles
  dplyr::filter(life_stage_a_j_m_t %in%
                  c("A","J","J/A", NA)) %>% # n=1524
  st_as_sf(coords=c("longitude_dd", "latitude_dd"), crs=4269, remove=FALSE) #n=1635

# FIELD MODELS
modrun <- "rb_bd_bern_h12_field_50p_50k_10min"
moddir <- "output/satscan/20210907"

fld_pts <- st_read(here(glue("{moddir}/{modrun}.gis.shp")), quiet = TRUE) %>%
  filter(P_VALUE<0.05)
fld_col <- st_read(here(glue("{moddir}/{modrun}.col.shp")), quiet = TRUE)

# need to assign to data?
# first we filter to sig clusters (p<0.05) then join to give membership
df_fld <- left_join(rb_fld, st_drop_geometry(fld_pts),
                    by = c("huc12"="LOC_ID")) %>%
  select(sampleid:boylii_clade,CLUSTER:CLU_POP, everything()) %>%
  mutate(CLUSTER = case_when(
    is.na(CLUSTER) ~ 0, # not part of a cluster
    TRUE ~ CLUSTER)) %>%
  mutate(clustrate = case_when(
    CLUSTER==0 ~ 0,
    CLU_RR < 1 ~ 1, # low rate cluster
    CLU_RR >=1 ~ 2 # high rate cluster
  ), .after=bd_positive) %>%
  # reorder clusters north to south
  mutate(CLUSTER = case_when(
    CLUSTER == 2 ~ 1,
    CLUSTER == 8 ~ 2,
    CLUSTER == 3 ~ 3,
    CLUSTER == 5 ~ 4,
    CLUSTER == 7 ~ 5,
    CLUSTER == 4 ~ 6,
    CLUSTER == 1 ~ 7,
    CLUSTER == 6 ~ 8,
    TRUE ~ CLUSTER))

#mapview::mapview(df_fld, zcol="CLUSTER_rev")
# table(df_fld$CLUSTER, useNA="always")

# MUSEUM MODELS
modrun <- "rb_bd_bern_h12_museum_50p_50k_3min"
moddir <- "output/satscan/20210907"

# don't filter to p<0.05 here, want to include san gabriel cluster
mus_pts <- st_read(here(glue("{moddir}/{modrun}.gis.shp")), quiet = TRUE)
mus_col <- st_read(here(glue("{moddir}/{modrun}.col.shp")), quiet = TRUE)

# join to give membership
df_mus <- left_join(rb_mus, st_drop_geometry(mus_pts), by = c("huc12"="LOC_ID")) %>%
  select(sampleid:boylii_clade,CLUSTER:CLU_POP, everything()) %>%
  mutate(CLUSTER = case_when(
    is.na(CLUSTER) ~ 0, # NOT PART OF A CLUSTER
    TRUE ~ CLUSTER)) %>%
  mutate(clustrate = case_when(
    CLUSTER==0 ~ 0,
    CLU_RR < 1 ~ 1,
    CLU_RR >=1 ~ 2
  ), .after=bd_positive)

#table(df_mus$CLUSTER, useNA="always")
#table(df_mus$clustrate, useNA="always")

clf_fld %>% st_drop_geometry() %>%  group_by(clustrate, CLUSTER) %>% tally()

df_mus %>% st_drop_geometry() %>%  group_by(clustrate, CLUSTER) %>% tally()



df_mus %>% group_by(clustrate, CLUSTER) %>% tally() %>% ggplot() + geom_point(aes(x=as.factor(clustrate), y=n))

df_fld %>% group_by(clustrate, CLUSTER) %>% tally() %>% ggplot() + geom_point(aes(x=as.factor(clustrate), y=n))

# select cols of interest
df_fld2 <- df_fld %>% st_drop_geometry() %>%
  select(sampleid:longitude_dd, huc10:huc12, capt_yr)
df_mus2 <- df_mus %>% st_drop_geometry() %>%
  select(sampleid:longitude_dd, huc10:huc12, capt_yr)

write_csv(df_fld2, "output/rb_fld_satscan_all.csv")
write_csv(df_mus2, "output/rb_mus_satscan_all.csv")
