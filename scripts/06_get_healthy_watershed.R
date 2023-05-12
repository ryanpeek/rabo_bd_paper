# get healthy watershed data

library(sf)
library(tidyverse)
library(janitor)
library(mapview)
mapviewOptions(fgb = FALSE)

# Get Gpkg ----------------------------------------------------------------

gpkg_file <- "output/rabo_bd_spatial_data.gpkg"
st_layers(gpkg_file)

# get the h12 data
h12 <- st_read(gpkg_file, "HUC12")

# load the pt data
load("output/02_rb_bd_spatial.rda")

# Get H12 of each Rb Point ------------------------------------------------

st_crs(rb_bd_sf)==st_crs(h12)

# crop h12s to just Rb bd's
rb_h12 <- h12[rb_bd_sf,]

mapview(rb_h12, alpha.regions=0.5, col.regions="cyan4", color="darkblue", cex=0.4) +
  mapview(rb_bd_sf, zcol="boylii_clade")

# Get Healthy Watershed Data CA -------------------------------------------

st_layers("data/healthy_watersheds/ca_phwa_package_170518/CA_PHWA_Geodatabase_170518.gdb/")

# metdata
metadata <- st_read("data/healthy_watersheds/ca_phwa_package_170518/CA_PHWA_Geodatabase_170518.gdb/",
                    "PHWA_Metadata")


# get CA ecoregions
ecoregs <- st_read("data/healthy_watersheds/ca_phwa_package_170518/CA_PHWA_Geodatabase_170518.gdb/", "CA_Ecoregions")

# all the individual metrics
ca_hw_indicators <- st_read("data/healthy_watersheds/ca_phwa_package_170518/CA_PHWA_Geodatabase_170518.gdb/", "PHWA_Indicators")


# INDICES
# VULN_NDX_ST_2016 # raw scores, vulnerability by state, 2016
# HEALTH_NDX_ST_2016 # raw scores, health by state, 2016
# VULN_NDX_ER_2016 # raw scores, vulnerability by ecoregion, 2016
# HEALTH_NDX_ER_2016 # raw scores, health by ecoregion, 2016
# VULN_NDX_ER_PCT_2016 # scaled across ecoregs in state, vulnerability by ecoregion, 2016
# HEALTH_NDX_ER_PCT_2016 # scaled percentiles across ecoregs, health by ecoregion, 2016

# SPECIFIC INDICATORS to pull?
# DAM_STORAGE_RATIO
# DAM_DENSITY_PER_KM_STRM
# ROADS_ALL_DENSITY_2014_RZ
# PCT_FOREST_REMAIN
# PCT_WETLANDS_REMAIN
# MEAN_WILDFIRE_RISK

# Get Healthy Watershed Data OR -------------------------------------------

st_layers("data/healthy_watersheds/or_phwa_package_170518/OR_PHWA_Geodatabase_170518.gdb/")

# all the individual metrics
or_hw_indicators <- st_read("data/healthy_watersheds/or_phwa_package_170518/OR_PHWA_Geodatabase_170518.gdb/", "PHWA_Indicators")

# Join States Data --------------------------------------------------------

library(janitor)
# make sure cols same
compare_df_cols_same(or_hw_indicators, ca_hw_indicators)

# combine states
hw_indicators <- bind_rows(or_hw_indicators, ca_hw_indicators)

save(hw_indicators, file = "output/06_hwatersheds_metrics_or_ca.rda")

# Join with BD Boylii Data ------------------------------------------------

rb_bd_hw_indicators <- left_join(rb_h12, hw_indicators, by=c("huc12"="HUC12")) %>%
  select(-c(tnmid:noncontributingareasqkm, referencegnis_ids))

save(rb_bd_hw_indicators, file="output/06_rb_bd_hwatersheds_selected_h12.rda")
