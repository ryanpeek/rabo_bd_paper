# Summarize data for status reports

# Load Libraries ----------------------------------------------------------

library(tidyverse)
options(dplyr.print_max = 300)
library(fs)
library(sf)
library(glue)
library(janitor)
library(mapview)
mapviewOptions(fgb = FALSE)

# Load Data ---------------------------------------------------------------

rb_sf <- read_rds("output/09_rb_bd_all_covariates_joined.rds") %>%
  st_as_sf(coords=c("longitude_dd", "latitude_dd"), crs=4326, remove=FALSE)
load("output/02_rb_bd_localities_only.rda") # bd_sf_loc

mapview(rb_sf, zcol="bd_positive")

# make non sf version
rb <- rb_sf %>% st_drop_geometry()

# Get HUCS
st_layers("output/rabo_bd_spatial_data.gpkg")
h12 <- st_read("output/rabo_bd_spatial_data.gpkg", "HUC12")
h10 <- st_read("output/rabo_bd_spatial_data.gpkg", "HUC10")
h8 <- st_read("output/rabo_bd_spatial_data.gpkg", "HUC8") %>%
  select(states, huc6, huc8, name, areasqkm, geom)
h6 <- st_read("output/rabo_bd_spatial_data.gpkg", "HUC6")

# Check for ZE ------------------------------------------------------------

# how many museum records have ze <1 but >0?
rb %>% filter(field_or_museum == "Museum") %>%
  filter(bd_load_ze_if_q_pcr < 1 & bd_load_ze_if_q_pcr > 0) %>%
  View() # only 2 and both are BD +

# how many ze > 0 in museum
rb %>% filter(field_or_museum == "Museum") %>%
  filter(bd_load_ze_if_q_pcr > 0 ) %>%
  #tally() # n=14
  View()

rb %>% filter(field_or_museum == "Museum") %>%
  tally() # n=461


# Summarize By Groups -----------------------------------------------------

rb %>% group_by(field_or_museum) %>%
  tally()
# field n=1684
# museum n=461

# bd +
rb %>% group_by(field_or_museum, bd_positive) %>%
  tally()
# field_or_museum bd_positive     n
# 1 Field                     0  1072
# 2 Field                     1   612
# 3 Museum                    0   422
# 4 Museum                    1    39


# how many bd + ITS samples?
rb %>% filter(!is.na(bd_its_copies)) %>%
  group_by(field_or_museum) %>% tally() # n=1241

# how many >0
rb %>% filter(!is.na(bd_its_copies), bd_its_copies>0) %>%
  group_by(field_or_museum) %>% tally() # n=521

# how many ZE?
rb %>% filter(!is.na(bd_load_ze_if_q_pcr)) %>%
  group_by(field_or_museum) %>% tally()
#Field             721
#Museum            258

# how many ZE > 1
rb %>% filter(!is.na(bd_load_ze_if_q_pcr), bd_load_ze_if_q_pcr>=1) %>%
  group_by(field_or_museum) %>% tally()
# Field             342
# Museum            12

# Summarize By HUCS -------------------------------------------------------

# by huc10
rb %>%
  group_by(field_or_museum, huc10, bd_positive) %>%
  tally() %>%
  #View()
  write_csv(., file = "output/rb_bd_samples_by_huc10.csv")

# by huc8
rb %>%
  mutate(huc8 = str_sub(huc10, 1, 8), .before=huc10) %>%
  group_by(field_or_museum, huc8, bd_positive) %>%
  tally() %>%
  write_csv(., file = "output/rb_bd_samples_by_huc8.csv")

rb %>%
  mutate(huc6 = str_sub(huc10, 1, 6), .before=huc10) %>%
  group_by(field_or_museum, huc6, bd_positive) %>%
  tally() %>%
  group_by(field_or_museum, huc6) %>%
  add_tally(n) %>%
  mutate(prevalence = n/nn) %>%
  select(huc6, field_or_museum, bd_positive, n, prevalence) %>%
  write_csv(., file = "output/rb_bd_samples_by_huc6.csv")

# Summarize By Clade/EcoRegion --------------------------------------------

rb %>% distinct(river_system_watershed) %>% arrange(desc(.))
rb %>% distinct(boylii_clade)
rb %>% distinct(ECOREGION2013L3_1STCODE)

# get geomorphic provinces
geom_reg <- st_read("data/ca-geomorphic-provinces/california-geomorphic-provinces.gpkg", "california_geomorphic_provinces")

# get ecoregion
# get CA ecoregions
ecoregs <- st_read("data/healthy_watersheds/ca_phwa_package_170518/CA_PHWA_Geodatabase_170518.gdb/", "CA_Ecoregions")

# by ecoregion
rb %>%
  mutate(ECOREGION2013L3_1STCODE = as.character(ECOREGION2013L3_1STCODE)) %>%
  left_join(., st_drop_geometry(ecoregs),
                 by = c("ECOREGION2013L3_1STCODE"="US_L3CODE")) %>%
  rename(ecoreg_l3 = ECOREGION2013L3_1STCODE) %>%
  group_by(field_or_museum, ecoreg_l3, US_L3NAME, bd_positive) %>%
  tally() %>%
  group_by(field_or_museum, US_L3NAME) %>%
  add_tally(n) %>%
  mutate(prevalence = n/nn) %>%
  select(US_L3NAME, field_or_museum, bd_positive, n, prevalence) %>%
  ungroup() %>%
  group_by(US_L3NAME, field_or_museum) %>%
  add_tally(wt = n, name = "N") %>%
  filter(bd_positive==1) %>%
  arrange(US_L3NAME) %>%
  select(US_L3NAME, field_or_museum, N, prevalence, n) %>% View() #%>%
  #write_csv(., file = "output/rb_bd_samples_by_ecoregion.csv")

# now by clade
rb %>%
  group_by(field_or_museum, boylii_clade, bd_positive) %>%
  tally() %>%
  group_by(field_or_museum, boylii_clade) %>%
  add_tally(n) %>%
  mutate(prevalence = n/nn) %>%
  ungroup() %>%
  group_by(boylii_clade, field_or_museum) %>%
  add_tally(wt = n, name = "N") %>%
  filter(bd_positive==1) %>%
  arrange(boylii_clade) %>%
  select(boylii_clade, field_or_museum, N, prevalence, n) %>% View()
  #write_csv(., file = "output/rb_bd_samples_by_clade.csv")

# now by ZE
rb %>%
  filter(!is.na(bd_load_ze_if_q_pcr)) %>%
  group_by(field_or_museum, boylii_clade, bd_positive) %>%
  tally() %>%
  group_by(field_or_museum, boylii_clade) %>%
  add_tally(n) %>%
  mutate(prevalence = n/nn) %>%
  ungroup() %>%
  group_by(boylii_clade, field_or_museum) %>%
  add_tally(wt = n, name = "N") %>%
  #filter(bd_positive==1) %>%
  filter(field_or_museum == "Field") %>%
  arrange(boylii_clade) %>%
  select(boylii_clade, bd_positive, field_or_museum, N, prevalence, n) #%>%
  #View()
#write_csv(., file = "output/rb_bd_ze_samples_by_clade.csv")

# by ITS only
rb %>%
  filter(!is.na(bd_its_copies)) %>%
  group_by(field_or_museum, boylii_clade, bd_positive) %>%
  tally() %>%
  group_by(field_or_museum, boylii_clade) %>%
  add_tally(n) %>%
  mutate(prevalence = n/nn) %>%
  ungroup() %>%
  group_by(boylii_clade, field_or_museum) %>%
  add_tally(wt = n, name = "N") %>%
  #filter(bd_positive==1) %>%
  arrange(boylii_clade) %>%
  select(boylii_clade, bd_positive, field_or_museum, N, prevalence, n) #%>%


# Get Range ---------------------------------------------------------------

# get rb_range:
load("output/updated_rabo_range_poly.rda")


# Get Map Baselayers ------------------------------------------------------

library(USAboundaries)
library(tmap)
library(tmaptools)

# get state & river data
ca <- us_states(states=c("ca","or"))
ca_co <- us_counties(states=c("ca","or"))
load("data/major_rivers_dissolved.rda")

# buffer out bbox
bd_buff <- bd_sf_loc %>%
  st_bbox() %>% # make a boundary box
  st_as_sfc() %>% # this converts bbox to polygon
  st_transform(3310) %>% # need to make it metric for easier buffer
  st_buffer(dist = 50000) %>%  # add 50km buffer
  st_transform(4326) # convert back to lat/lon

# crop rivers to CA only and filter to rivers only
rivs_ca <- st_intersection(rivs, ca) %>%
  filter(FEATURE_TYPE == "river")

## using tmaptools
#gm_osm <- read_osm(bd_buff, type = "esri-topo", zoom = 7, raster=TRUE)

## Make Basemap --------------------------------------------

region <- "county"

# first make CA map with no border: HUC6
(map_ca_nobase_co <-
    tm_shape(bd_buff) + tm_polygons(border.alpha = 0, alpha=0) +
    # counties
    tm_shape(ca_co) + tm_polygons(border.alpha = 0.3, alpha=0.5, col = "gray") +
    # state
    tm_shape(ca) + tm_polygons(border.alpha = 0.9, lwd=2, alpha=0, col = "black") +
    # Rb range
    tm_shape(rb_range_smth) + tm_polygons(border.alpha = 0.9, lwd=1, alpha=0.5,
                                          col = "tan", border.col = "burlywood4") +
    # HUCS:
    # tm_shape(h8) + tm_polygons(border.alpha = 0.5, border.col = "darkblue",
    #                            lwd=0.1, alpha=0) +

    # tm_shape(h6) + tm_polygons(border.alpha = 0.2, border.col = "darkblue",
    #                            lwd=1.5, alpha=0.5, col = "gray") +
    #tm_text(text = "huc6", col = "black", fontface = "bold", fontfamily = "Roboto Condensed", alpha=0.7, size = 0.55) +

    tm_shape(rivs_ca) + tm_lines(col="steelblue", lwd = .1, alpha = 0.55) +

    tm_layout(frame=FALSE))



# first make CA map with no border: HUC6
(map_ca_nobase_h6 <-
    tm_shape(bd_buff) + tm_polygons(border.alpha = 0, alpha=0) +
    # state
    tm_shape(ca) + tm_polygons(border.alpha = 0.9, lwd=2, alpha=0, col = "black") +
    # Rb range
    tm_shape(rb_range_smth) + tm_polygons(border.alpha = 0.9, lwd=1, alpha=0.5,
                                     col = "tan", border.col = "burlywood4") +
    # HUCS:
    # tm_shape(h8) + tm_polygons(border.alpha = 0.5, border.col = "darkblue",
    #                            lwd=0.1, alpha=0) +

    tm_shape(h6) + tm_polygons(border.alpha = 0.2, border.col = "darkblue",
                             lwd=1.5, alpha=0.5, col = "gray") +
    #tm_text(text = "huc6", col = "black", fontface = "bold", fontfamily = "Roboto Condensed", alpha=0.7, size = 0.55) +

    tm_shape(rivs_ca) + tm_lines(col="steelblue", lwd = .1, alpha = 0.55) +

    tm_layout(frame=FALSE))

# make factor
rb_bd_sf <- rb_sf %>%
  mutate(bd_positive_f=as.factor(bd_positive))


# Map of Bd Positive by Type ----------------------------------------------



# map of Bd+ sites
(map_final_sites <-
   #map_ca_base +
   map_ca_nobase_co +
   tm_shape(rb_bd_sf) +
   tm_dots(col="bd_positive_f", palette="viridis", shape="field_or_museum", alpha=0.9,
           size=0.3, title = "RABO Bd +", title.shape="Type") +
   tm_shape(rb_bd_sf %>% filter(bd_positive_f=="1"))+
   tm_dots(col="bd_positive_f", palette="viridis", shape="field_or_museum", alpha=0.7,
           size=0.7, title = "RABO Bd +", title.shape="Type", legend.shape.show=FALSE,  legend.show = FALSE) +

  # huc6
   # tm_shape(h6) +
   # tm_text(text = "huc6", col = "black", fontface = "bold",
   #         fontfamily = "Roboto Condensed", alpha=0.7,
   #         size = 0.6, shadow = TRUE) +
   # layout
   tm_layout(
     frame = FALSE,
     fontfamily = "Roboto Condensed",
     legend.bg.color = TRUE, legend.frame = "gray60",
     legend.outside = FALSE, attr.outside = FALSE,
     inner.margins = 0.01, outer.margins = (0.01),
     legend.position = c(0.67,0.75),
     title.position = c(0.6, 0.95)) +
   tm_compass(type = "4star", position = c("left","bottom")) +
   tm_scale_bar(position = c("left","bottom")))

# save
tmap_save(map_final_sites, filename = glue("output/maps/tmap_{region}_field_museum_bd_pos.jpg"), height = 11, width = 8.5, units = "in", dpi = 300)


# Map of Pts by Clade ----------------------------------------------

# map of Clades
(map_clades <-
   map_ca_nobase_h6 +
   tm_shape(rb_bd_sf) +
   tm_dots(col="boylii_clade", palette="Set2", shape="field_or_museum", alpha=0.9,
           size=0.3, title = "Clade", title.shape="Type") +

   # huc6
   tm_shape(h6) +
   tm_text(text = "huc6", col = "black", fontface = "bold",
           fontfamily = "Roboto Condensed", alpha=0.7,
           size = 0.6, shadow = TRUE) +
   # layout
   tm_layout(
     frame = FALSE,
     fontfamily = "Roboto Condensed",
     legend.bg.color = TRUE, legend.frame = "gray60",
     legend.outside = FALSE, attr.outside = FALSE,
     inner.margins = 0.01, outer.margins = (0.01),
     legend.position = c(0.67,0.75),
     title.position = c(0.6, 0.95)) +
   tm_compass(type = "4star", position = c("left","bottom")) +
   tm_scale_bar(position = c("left","bottom")))

# save
tmap_save(map_clades, filename = glue("output/maps/tmap_{region}_field_museum_by_clade.jpg"), height = 11, width = 8.5, units = "in", dpi = 300)


# Combined ----------------------------------------------------------------

(map_comb<-tmap_arrange(map_final_sites, map_clades))
tmap_save(map_comb,filename = glue("output/maps/combined_final_sites_clades.jpg"), width = 11, height = 8.5, units = "in", dpi = 300)

# Map of ITS Samples -----------------------------------------------------

rb_its <- rb_bd_sf %>%
  filter(!is.na(bd_its_copies)) %>%
  filter(bd_positive == 1) %>%
  filter(field_or_museum == "Field")

# map of ITS+ sites
(map_its_sites <-
    # tm_shape(bd_buff) + tm_polygons(border.alpha = 0, alpha=0) +
    # tm_shape(ca) + tm_polygons(border.alpha = 0.3, alpha=0.5, col = "gray") +
    # tm_shape(rivs_ca) + tm_lines(col="steelblue", lwd = .5, alpha = 0.5) +
    map_ca_nobase_h6 +
    tm_layout(frame=FALSE) +
    tm_shape(rb_its) +
    tm_dots(col="bd_positive_f", palette="viridis", shape=21, alpha=0.9,
            size=0.5, title = "ITS Bd +", legend.show = FALSE) +
    # tm_shape(rb_its %>% filter(bd_its_copies > 1))+
    # tm_dots(size=0.5, shape=1, alpha=0.8,
    #         title = "ITS Bd +", title.size="ITS Load", legend.show = FALSE) +
    tm_layout(title="ITS Bd+",
      frame = FALSE,
      fontfamily = "Roboto Condensed",
      legend.bg.color = TRUE, legend.frame = "gray60",
      legend.outside = FALSE, attr.outside = FALSE,
      inner.margins = 0.01, outer.margins = (0.01),
      legend.position = c(0.6,0.77),
      title.position = c(0.1, 0.3)) +
    tm_compass(type = "4star", position = c("left","bottom")) +
    tm_scale_bar(position = c("left","bottom")))

# save
tmap_save(map_its_sites, filename = glue("output/maps/tmap_{region}_its_samples.jpg"), height = 11, width = 8.5, units = "in", dpi = 300)


# MAP OF ZE ---------------------------------------------------------------

rb_ze <- rb_bd_sf %>%
  filter(!is.na(bd_load_ze_if_q_pcr)) %>%
  filter(bd_positive == 1) %>%
  filter(field_or_museum == "Field")


(map_ze_sites <-
    map_ca_nobase_h6 +

    tm_layout(frame=FALSE) +
    tm_shape(rb_ze) +
    tm_dots(col="bd_positive_f", palette="viridis", shape=21, alpha=0.9,
            size=0.4, title = "ZE Bd +", legend.show = FALSE) +
    tm_shape(rb_its %>% filter(bd_load_ze_if_q_pcr > 1))+
    tm_dots(col="bd_positive_f", palette="viridis",
            size=0.5, shape=21, alpha=0.7,
            title = "ZE Bd +", legend.show = FALSE) +

    tm_layout(title = "ZE Bd+",
      frame = FALSE,
      fontfamily = "Roboto Condensed",
      legend.bg.color = TRUE, legend.frame = "gray60",
      legend.outside = FALSE, attr.outside = FALSE,
      inner.margins = 0.01, outer.margins = (0.01),
      legend.position = c(0.61,0.77),
      title.position = c(0.1, 0.3)) +
    tm_compass(type = "4star", position = c("left","bottom")) +
    tm_scale_bar(position = c("left","bottom")))

# save
tmap_save(map_ze_sites, filename = glue("output/maps/tmap_{region}_ze_samples.jpg"), height = 11, width = 8.5, units = "in", dpi = 300)

# Arrange Both ------------------------------------------------------------

(map_comb<-tmap_arrange(map_final_sites, map_its_sites))
tmap_save(map_comb,filename = glue("output/maps/combined_{region}_bd_pos_and_its_samples.jpg"), height = 11, width = 8.5, units = "in", dpi = 300)

(map_zecomb<-tmap_arrange(map_ze_sites, map_its_sites))
tmap_save(map_zecomb,filename = glue("output/maps/combined_{region}_ze_and_its_samples.jpg"), height = 11, width = 8.5, units = "in", dpi = 300)


