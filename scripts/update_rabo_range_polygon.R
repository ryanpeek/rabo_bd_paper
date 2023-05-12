# revise RABO Range

library(tidyverse)
library(sf)
library(mapedit)
library(mapview)
library(rmapshaper)

# get points
rb_sf <- read_rds("output/09_rb_bd_all_covariates_joined.rds")
load("output/02_rb_bd_localities_only.rda")


# get revised/smoothed rb_range:
load("output/rb_range_polygon.rda") # this is customized but smaller in so coast

# fix cols
rb_range <- rb_range %>%
  select(-keeppoly)

# this is CDFW version only CA
rb_range_cdfw <- st_read("data/rb_range_cdfw/a043.shp") %>% st_transform(4326)

# see this map from USGS
# https://www.sciencebase.gov/catalog/item/58fa4046e4b0b7ea54524980
rb_range_usgs <- st_read("data/rb_range_usgs/aFYLFx_CONUS_Range_2001v1.shp") %>%
  st_transform(4326)

# this is usfs/mod version:
rb_range_comb <- st_read("data/rb_range_ca_or/Rb_Potential_Range_CAandOR.shp") %>% st_transform(4326)

# mapview:
mapview(rb_range, col.region="gray50") +
  mapview(rb_range_cdfw, col.region="burlywood4") +
  mapview(rb_range_comb, col.region="maroon") +
  mapview(rb_range_usgs, col.region="yellow") +
  mapview(bd_sf_loc, col.region="orange")



# EDIT THE CLOSEST VERSION ------------------------------------------------

# use the usgs and range update/smoothed:
m1 <- mapview(rb_range, col.region="gray50") +
  mapview(rb_range_usgs, col.region="yellow") +
  mapview(bd_sf_loc, col.region="orange")
m1 <- m1@map %>% leaflet::addMeasure()

# add socal spot
sopoly <- mapedit::editMap(m1)
mapview(sopoly$finished) + mapview(bd_sf_loc, col.region="orange") + mapview(rb_range, col.regions="maroon")

# add ebay/sf
ebpoly <- mapedit::editMap(m1)

# add ebay
eb2poly <- mapedit::editMap(m1)

# add sierras
sspoly <- mapedit::editMap(m1)

# sierrawest
swpoly <- mapedit::editMap(m1)

pitpoly <- mapedit::editMap(m1)

# add just south sf
sfpoly <- mapedit::editMap(m1)

mapview(sopoly$finished, col.regions="maroon") +
  mapview(sfpoly$finished, col.regions="maroon") +
  mapview(pitpoly$finished, col.regions="maroon") +
  mapview(swpoly$finished, col.regions="maroon") +
  mapview(ebpoly$finished, col.regions="maroon") +
  mapview(sspoly$finished, col.regions="maroon") +
  mapview(eb2poly$finished, col.regions="maroon") +
  mapview(bd_sf_loc, col.region="orange") +
  mapview(rb_range, col.regions="gray")


# Dissolve! ---------------------------------------------------------------

# bind all
rb_update <- rbind(sopoly$finished, sfpoly$finished, pitpoly$finished, swpoly$finished,
      ebpoly$finished, sspoly$finished, eb2poly$finished) %>%
  select(-X_leaflet_id, -feature_type)

rb_union <- st_union(rb_update) %>% st_sf()
mapview(rb_union)

# bind with original
rb_update_all <- rbind(rb_union, rb_range)
mapview(rb_update_all)

# union again
rb_range_final <- st_union(rb_update_all) %>% st_sf()
mapview(rb_range_final)

# smooth out
library(smoothr)
rb_range_smth <- smooth(rb_range_final, method = "ksmooth", smoothness = 2) #chaikin #ksmooth spline
mapview(rb_range_smth, col.regions="blue", alpha.regions=0.5) + mapview(rb_range_final, col.regions="yellow", alpha.regions=0.5)


save(rb_range_final, rb_range_smth, file = "output/updated_rabo_range_poly.rda")
