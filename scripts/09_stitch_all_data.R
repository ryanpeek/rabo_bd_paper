# STITCH DATA TOGETHER

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(fs)
library(sf)
library(glue)
library(lubridate)
library(mapview)
mapviewOptions(fgb = FALSE)

# Load Data ---------------------------------------------------------------

# load main BD dataset
load("output/02_rb_bd_spatial.rda") # rb_bd_sf, n=2145 obs and 28 vars

# ADD HUCS ----------------------------------------------------------------

#get H10 and H12s for geo file:
h12 <- st_read("output/rabo_bd_spatial_data.gpkg", "HUC12") %>%
  select(states, huc10, huc12, name, areasqkm, geom)

# first we need unique spatial list of sites and associated group or site.
# join and get pts and H12s
rb_hucs <- rb_bd_sf %>%
  # make a month and year only col:
  mutate(YYYY=year(date_captured),
         MM=month(date_captured)) %>%
  # subset to just data we need for now:
  select(sampleid, field_or_museum, date_captured:longitude_dd, sex_f_m_u:weight_g, bd_positive, geometry) %>%
  st_join(., h12[,c("name", "states", "huc10", "huc12")])

# rm HUC layers
rm(h12)

# ADD DAYLENGTH (08) -------------------------------------------------------

load("output/08_rb_bd_spatial_daylength.rda") # rb_bd_dl
# trim
rb_bd_dl <- rb_bd_dl %>% select(sampleid, date_captured, daylen_hr) %>%
  st_drop_geometry()

# join with daylength
rb_hucs_dl <- rb_hucs %>%
  left_join(., rb_bd_dl)

rm(rb_bd_dl, rb_hucs)

# ADD SPEI & NOAA (07b) ----------------------------------------------------

load("output/07_rb_bd_noaa_vars.rda") # rb_bd_noaa

# join and rename id col
rb_hucs_dl_noaa <- left_join(rb_hucs_dl, rb_bd_noaa) %>%
  rename(noaa_clim_id = id)

# get spei
rb_spei12 <- read_rds("output/07_rb_spei_12month.rds")
rb_spei24 <- read_rds("output/07_rb_spei_24month.rds")

# bind
rb_spei <- left_join(rb_spei12, rb_spei24, by=c("sampleid","date_captured"))
summary(rb_spei)

# join w spei data
rb_hucs_dl_noaa_spei <- left_join(rb_hucs_dl_noaa, rb_spei, by=c("sampleid","date_captured")) %>%
  # drop dups
  distinct(sampleid, .keep_all = TRUE)


rm(rb_bd_noaa, rb_hucs_dl_noaa, rb_spei, rb_hucs_dl,
   rb_spei12, rb_spei24)

summary(rb_hucs_dl_noaa_spei)

# ADD HEALTHY WATERSHEDS (06_get_healthy_watershed) ------------------------

# this is rb_bd_hw_indicators and indices
load("output/06_rb_bd_hwatersheds_selected_h12.rda")

# bind these together
rb_bd_hw <- rb_bd_hw_indicators %>%
  rename(h12_name=name) %>%
  select(-c(states, huc10))

# mapview(rb_bd_hw, zcol="DAM_DENSITY_PER_KM_STRM")

# join with INDEX
rb_hucs_dl_noaa_spei_hw <- rb_hucs_dl_noaa_spei %>%
  left_join(., st_drop_geometry(rb_bd_hw), by="huc12")


rm(rb_hucs_dl_noaa_spei, rb_bd_hw, rb_bd_hw_indicators)


# ADD ELEV (05_get_elev_data) ---------------------------------------------

load("output/05_rb_bd_locs_elev_adj.rda") # bd_adj_elev

rb_hucs_dl_noaa_spei_hw_ele <- rb_hucs_dl_noaa_spei_hw %>%
  left_join(., st_drop_geometry(bd_adj_elev), by="sampleid") %>%
  rename(X_ele = X, Y_ele = Y)

summary(rb_hucs_dl_noaa_spei_hw_ele$elevation)

rm(rb_hucs_dl_noaa_spei_hw, bd_adj_elev)

# ADD GEOMORPH PROVINCES --------------------------------------------------

st_layers("data/ca-geomorphic-provinces/california-geomorphic-provinces.gpkg")

geomorph <- st_read("data/ca-geomorphic-provinces/california-geomorphic-provinces.gpkg", "california_geomorphic_provinces") %>%
  st_transform(4326)

st_crs(geomorph)
st_crs(rb_hucs_dl_noaa_spei_hw_ele)
#mapview(geomorph, zcol="RANGE_NAME")

# join
final_geomorph <- st_join(rb_hucs_dl_noaa_spei_hw_ele, geomorph[,c("RANGE_NAME")])

rm(geomorph, rb_hucs_dl_noaa_spei_hw_ele)

# Add Dam Metrics ---------------------------------------------------------

# THESE DATA DROPPED FROM SUBSEQUENT ANALYSIS

# from https://doi.org/doi:10.5066/F7FN14C5
#dams <- read_csv("output/04_dam_metrics_for_rabo_bd_comids.csv")


# COMID -------------------------------------------------------------------

bd_coms_df <- read_rds("output/04_bd_comids.rds")

final_out <- left_join(final_geomorph, bd_coms_df)
summary(final_out)
rm(final_geomorph, bd_coms_df)

# Final Data Out ----------------------------------------------------------

# Check for Duplicates

final_out %>% st_drop_geometry() %>%
  group_by(sampleid) %>% tally() %>%
  filter(n>1) %>% nrow() # should be zero!

# check/drop dup cols
janitor::compare_df_cols(final_out)

# drop dup cols and unneeded ones
final_df <- final_out %>% st_drop_geometry() %>%
  select(-c(ends_with(".y"), Shape_Length, Shape_Area, name)) %>%
  rename(climdiv_name=NAME, geomorph_name=RANGE_NAME)

# compare
summary(final_df)

# fix the missing clades
table(final_df$boylii_clade, useNA = "ifany")
class(final_df$boylii_clade)
levels(final_df$boylii_clade)

summary(final_df)

# SAVE OUT ----------------------------------------------------------------

write_rds(final_df, file = "output/09_rb_bd_all_covariates_joined.rds")
write_csv(final_df, file="output/09_rb_bd_all_covariates_joined.csv")

#rb_covar <- read_rds("output/09_rb_bd_all_covariates_joined.rds")

