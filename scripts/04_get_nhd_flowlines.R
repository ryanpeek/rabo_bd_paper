# get watershed info for each point


# Libraries ---------------------------------------------------------------


library(tidyverse) # wrangling/plotting
library(janitor)
library(sf) # spatial package
library(glue) # pasting things together
#devtools::install_github("USGS-R/nhdplusTools")
library(nhdplusTools) # river data from NHD
library(mapview)
mapviewOptions(fgb = FALSE)

# Load Data ---------------------------------------------------------------

load("output/02_rb_bd_localities_only.rda")
load("output/02_rb_bd_spatial.rda")

# Load DATA FROM SECTIONS 1-4 Below ---------------------------------------

# If Sections 01-04 already run, then load data below:
# mainstems_us <- read_rds("output/04_bd_localities_mainstems_us.rda") # upstream flowline
# mainstems_ds <- read_rds("output/04_bd_localities_mainstems_ds.rda") # downstream flowline
# bd_coms_df <- read_rds("output/04_bd_comids.rds") # comids

gpkg_file <- "output/rabo_bd_spatial_data.gpkg"

# check layers?
st_layers(gpkg_file)

# just get nhdflowline
nhdflowline <- st_read(gpkg_file, "NHDFlowline_Network")

# 01. Get COMIDs ----------------------------------------------------------

# use nhdtools to get comids...this can take a minute or three
bd_coms <- rb_bd_sf %>%
  group_split(sampleid) %>%
  set_names(rb_bd_sf$sampleid) %>%
  map(~discover_nhdplus_id(.x$geometry))
beepr::beep(2)

# flatten into single dataframe instead of list
bd_coms_df <-bd_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("comid"=V1) %>%
  rownames_to_column(var = "sampleid")

# check
summary(bd_coms_df)
length(unique(bd_coms_df$comid))

# write back out
write_rds(bd_coms_df, file="output/04_bd_comids.rds")


bd_coms_df <- read_rds("output/04_bd_comids.rds")

# 02. Get Flowlines: UPSTREAM ---------------------------------------------

# make a com_list for getting data using only unique comids
length(unique(bd_coms_df$comid)) # n=293

coms_list <- map(unique(bd_coms_df$comid), ~list(featureSource = "comid", featureID=.x))

# check
coms_list[[200]] # should list feature source and featureID

# Get upstream mainstem streamlines (20 km limit) from point
mainstemsUS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="UT", # upstream main=UM, upstream tributaries="UT"
                                             distance_km = 20))
beepr::beep(2) # took about 2-3 min

# check length for NAs
mainstemsUS %>%
  purrr::map_lgl(~ length(.x)>1) %>% table()

# make a single flat layer
mainstems_flat_us <- mainstemsUS %>%
  map("UT_flowlines") %>% # this needs to match the col from 'mode' on line 67
  map2_df(unique(bd_coms_df$comid), ~mutate(.x, orig_comid=.y))

# add direction to gage col
mainstems_us <- mainstems_flat_us %>%
  mutate(from_obs = "UT")

# rm temp files
rm(mainstems_flat_us, mainstemsUS)

# mapview
mapview::mapview(mainstems_us, color="cyan4") +
  mapview::mapview(bd_sf_loc, col.regions="maroon", cex=3)

# save
tribs_us <- mainstems_us
write_rds(tribs_us, file = "output/04_bd_localities_tribs_us.rda")

# 03. Get Flowlines: DOWNSTREAM -------------------------------------------

# get NHD segments downstream of selected USGS gages, 50 km buffer
mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="DD",
                                             distance_km = 20))
beepr::beep(2)

# check length (for NAs...should all be "TRUE")
mainstemsDS %>%
  purrr::map_lgl(~ length(.x)>1) %>% table()

# make a single flat layer
mainstems_flat_ds <- mainstemsDS %>%
  map("DD_flowlines") %>%
  map2_df(unique(bd_coms_df$comid), ~mutate(.x, orig_comid=.y))

# add direction to gage col
mainstems_ds <- mainstems_flat_ds %>%
  mutate(from_obs = "DD")

# mapview
mapview::mapview(mainstems_ds, color="steelblue") +
  mapview::mapview(mainstems_us, color="cyan4") +
  mapview::mapview(bd_sf_loc, col.regions="maroon", cex=1.5)

# save
write_rds(mainstems_ds, file = "output/04_bd_localities_mainstems_ds.rda")

# rm temp files
rm(mainstems_flat_ds, mainstemsDS)

# 04. Make geopackage of spatial data --------------------------------------

gpkg_file <- "output/rabo_bd_spatial_data.gpkg"

# write individual stream layers
st_write(mainstems_ds, dsn = gpkg_file, layer = "mainstems_ds", delete_layer = TRUE)
st_write(tribs_us, dsn = gpkg_file, layer = "tribs_us", delete_layer = TRUE)

# get full list of comids and drop duplicates (n=8093)
allcoms <- unique(c(mainstems_ds$nhdplus_comid, tribs_us$nhdplus_comid))

# now fetch nhd attributes with comids...takes a minute
nhd_file <- "output/rabo_bd_nhdplus.gpkg"
geo_dat <- subset_nhdplus(comids = allcoms,
                         output_file = nhd_file,
                         nhdplus_data = "download",
                         flowline_only = FALSE,
                         return_data = TRUE,
                         overwrite = TRUE)
beepr::beep(2)

# check layers
st_layers(nhd_file)

# add specific data to the rabo_bd geopackage
st_write(geo_dat$NHDFlowline_Network, dsn=gpkg_file, layer = "NHDFlowline_Network", delete_layer = TRUE)
#st_write(geo_dat$NHDWaterbody, dsn=gpkg_file, layer = "NHDWaterbodies", delete_layer = TRUE)

# check all layers present?
st_layers(gpkg_file)

## Preview Map -------------------------------------------------------------

# preview
mapview(geo_dat$NHDFlowline_Network, color="blue2", lwd=1, layer.name="Flowlines") +
  mapview(bd_sf_loc, col.regions="maroon", cex=3,
          layer.name="RABO Localities")

# 05. Get Watershed HUC Data ----------------------------------------------

# this a large file so not keeping it in repo, but can download a copy here:
# https://apps.nationalmap.gov/viewer/

#st_layers("~/Downloads/wbdhu8_a_us_september2020.gdb")
#h8 <- st_read("~/Downloads/wbdhu8_a_us_september2020.gdb/", "WBDHU8")

# filter to ca/or only
#h8ca <- h8 %>% dplyr::filter(grepl("CA|OR", x = states))

#plot(h8ca$Shape)

#st_layers("~/Downloads/wbdhu12_a_us_september2020.gdb/")
#h12 <- st_read("~/Downloads/wbdhu12_a_us_september2020.gdb/", "WBDHU12")
#h12ca <- h12 %>% dplyr::filter(grepl("CA|OR", x = states))

## Dissolve and Simplify ---------------------------------------------------

# dissolve
# h6 <- hucs_ca %>% group_by(hudigit) %>%
#   summarize()

# library(rmapshaper)
# h8s <- rmapshaper::ms_simplify(h8ca, keep = .1) # pryr::object_size(), drops from 34 MB to ~3.6MB
# h12s <- rmapshaper::ms_simplify(h12ca, keep = .1) # pryr::object_size(), drops from 34 MB to ~3.6MB
# pryr::object_size(h12ca)
# pryr::object_size(h12s)
#
# h12s <- h12s %>%
#   mutate(huc10=str_sub(huc12, 1, 10)) %>%
#   rename(geometry=Shape)
#
# # pull out huc
# h8s <- h8s %>%
#   mutate(huc6=str_sub(huc8, 1, 6)) %>%
#   rename(geometry=Shape)
#
# # dissolve
# h6s <- h8s %>%
#   rmapshaper::ms_dissolve(., field = "huc6")
#
# h10s <- h12s %>%
#   rmapshaper::ms_dissolve(., field = "huc10")

# can also use dplyr to dissolve
#h6 <- hucs_ca %>% group_by(hudigit) %>%
#  summarize()

# # quick map check:
# plot(h12s$geometry, border = "orange", lwd = 0.4)
# plot(h10s$geometry, border = "steelblue", lwd=0.9, add=T)
# plot(h8s$geometry, border="cyan4", lwd=1)
# plot(h6s$geometry, border="gray", lwd=3, add=T)

## Write out to gpkg ----------------------------------------------------

# st_write(h12s, dsn=gpkg_file, layer = "HUC12", delete_layer=TRUE)
# st_write(h10s, dsn=gpkg_file, layer = "HUC10", delete_layer =TRUE)
# st_write(h8s, dsn=gpkg_file, layer = "HUC8", delete_layer = TRUE)
# st_write(h6s, dsn=gpkg_file, layer = "HUC6", delete_layer = TRUE)

#st_layers(gpkg_file)


# 06. Get Dam Metrics -------------------------------------------------------------

# Download this data from here: https://www.sciencebase.gov/catalog/item/58a60b88e4b057081a24f99d
#
# dams <- vroom::vroom("data/Dam_Metrics_NHDPlusV1_022317.csv.zip")
# dams <- dams %>% clean_names()
#
# # filter by comids
# dams_bd <- filter(dams, comid %in% nhdflowline$comid)
#
# # add to geopackage and save out as csv
# st_write(dams_bd, dsn=gpkg_file, layer="dam_metrics", delete_layer = TRUE)
# st_layers(gpkg_file)
#
# write_csv(dams_bd, file = "output/04_dam_metrics_for_rabo_bd_comids.csv")
#

