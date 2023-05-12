# Clean and Join Data: Make Locality Table

# Libraries ---------------------------------------------------------------

library(sf) # spatial package
library(tidyverse) # wrangling/plotting
library(glue) # pasting things together
library(mapview) # interactive mapping
mapviewOptions(fgb = FALSE) # set to FALSE to avoid bug
library(tmap)
library(tmaptools) # for base layers
library(USAboundaries)

# Load Data ----------------------------------------------------------------

# this is localities only: bd_sf_loc
load("output/02_rb_bd_localities_only.rda")
# this is all data cleaned as sf data frame: rb_bd_sf
load("output/02_rb_bd_spatial.rda")

# Get Basemap Layers ------------------------------------------------------

# get counties and states
ca_co<-us_counties(states=c("ca","or"))
ca<-us_states(states=c("ca","or"))
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

## using mapmisc
# get a basemap (options: path="bw-mapnik", "hyda-base", ""osm-roads-grey", opentopomap", "cartodb", "cartodb-dark", "spinal")
#basemap_tiles <- mapmisc::openmap(ca_sp, path="cartodb", maxTiles = 12)

## using tmaptools
gm_osm <- read_osm(bd_buff, type = "esri-topo", zoom = 7, raster=TRUE)
class(gm_osm) # should be stars


# Get HUCS ----------------------------------------------------------------

st_layers("output/rabo_bd_spatial_data.gpkg")

h10 <- st_read("output/rabo_bd_spatial_data.gpkg", "HUC10")
h8 <- st_read("output/rabo_bd_spatial_data.gpkg", "HUC8") %>%
  select(states, huc6, huc8, name, areasqkm, geom)

# Make Basemap ------------------------------------------------------------

# first make CA map with no border
(map_ca_nobase <-
   # for no basemap
   tm_shape(bd_buff) + tm_polygons(border.alpha = 0, alpha=0) +
   # counties
   tm_shape(ca_co) + tm_polygons(border.alpha = 0.3, alpha=0.5, col = "gray") +
   # state
   tm_shape(ca) + tm_polygons(border.alpha = 0.8, lwd=2,
                              alpha=0, col = "black") +
   # HUCS
   #tm_shape(h8) + tm_polygons(border.alpha = 0.3, alpha=0.5, col = "gray") +
   tm_shape(rivs_ca) + tm_lines(col="steelblue", lwd = .5, alpha = 0.5) +
   tm_layout(frame=FALSE))

(map_ca_base <-
      # for topo base
      tm_shape(gm_osm) + tm_rgb() +
      tm_shape(ca_co) + tm_polygons(border.alpha = 0.3, alpha=0.1) +
      tm_shape(rivs_ca) + tm_lines(col="steelblue", lwd = .5, alpha = 0.5) +
      tm_layout(frame=FALSE))

# Make Full Map: Field or Museum ----------------------------------------------

region <- "cnty"

# colorblind palette: scales::show_col(ggthemes::colorblind_pal()(8))
# "#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7"
colblind_pal <- c('#009E73','#E69F00')

# make paired sites
(map_final_sites <-
   #map_ca_base +
   map_ca_nobase +
    tm_grid(n.x = 0, n.y = 5) +
    tm_shape(rb_bd_sf) +
    tm_symbols(col="field_or_museum", palette=colblind_pal,
               shape="field_or_museum",
               shapes.legend = c(21, 23),
               title.col = "Sample Type", legend.shape.show = FALSE,
               size=0.5, alpha=0.75,
               legend.hist = TRUE) +

    tm_layout(frame = FALSE,
              fontfamily = "Roboto Condensed",
              legend.outside = FALSE, attr.outside = FALSE,
              legend.bg.color = TRUE, legend.frame = "gray60",
              inner.margins = 0.02, outer.margins = (0.01),
              legend.position = c(0.6,0.6), # w hist
              #legend.position = c(0.65,0.8), # no hist
              title.position = c(0.6, 0.9)) +
    tm_compass(type = "4star", position = c("left","bottom")) +
    tm_scale_bar(position = c("left","bottom")))


# Save Out Maps -----------------------------------------------------------

# save topo base version
# tmap_save(map_final_sites, filename = glue("output/maps/tmap_field_museum_samples_ca_{region}_topobase.jpg"), height = 11, width = 8.5, units = "in", dpi = 300)
#
# cairo_pdf(filename = glue("output/maps/tmap_field_museum_samples_ca_{region}_topobase.pdf"), height = 11, width = 8.5)
# map_final_sites
# dev.off()

# save no basemap version
tmap_save(map_final_sites, filename = glue("output/maps/tmap_field_museum_samples_ca_{region}_nobase_hist.jpg"), height = 11, width = 8.5, units = "in", dpi = 300)

cairo_pdf(filename = glue("output/maps/tmap_field_museum_samples_ca_{region}_nobase_hist.pdf"), height = 11, width = 8.5)
map_final_sites
dev.off()

# Make Full Map: Bd Positive -------------------------------------------------

# drop NA
rb_bd_sf <- rb_bd_sf %>%
  mutate(bd_positive_f=as.factor(bd_positive))

(map_final_sites <-
    #map_ca_base +
    map_ca_nobase +
   tm_shape(rb_bd_sf) +
    tm_dots(col="bd_positive_f", palette="viridis", shape=21, alpha=0.9,
           size=0.3, title = "RABO Bd +") +
    tm_shape(rb_bd_sf %>% filter(bd_positive_f=="1"))+
    tm_dots(col="bd_positive_f", palette="viridis", shape=21, alpha=0.7,
            size=0.7, title = "RABO Bd +", legend.show = FALSE) +
   tm_layout(#title=
     frame = FALSE,
     fontfamily = "Roboto Condensed",
     legend.bg.color = TRUE, legend.frame = "gray60",
     legend.outside = FALSE, attr.outside = FALSE,
     inner.margins = 0.01, outer.margins = (0.01),
     legend.position = c(0.65,0.85),
     title.position = c(0.6, 0.95)) +
   tm_compass(type = "4star", position = c("left","bottom")) +
   tm_scale_bar(position = c("left","bottom")))

# save topo base version
# tmap_save(map_final_sites, filename = glue("output/maps/tmap_field_museum_samples_ca_{region}_topobase.jpg"), height = 11, width = 8.5, units = "in", dpi = 300)
#
# cairo_pdf(filename = glue("output/maps/tmap_field_museum_samples_ca_{region}_topobase.pdf"), height = 11, width = 8.5)
# map_final_sites
# dev.off()

# save no basemap version
tmap_save(map_final_sites, filename = glue("output/maps/tmap_bd_result_ca_{region}_nobase.jpg"), height = 11, width = 8.5, units = "in", dpi = 300)

cairo_pdf(filename = glue("output/maps/tmap_bd_result_ca_{region}_nobase.pdf"), height = 11, width = 8.5)
map_final_sites
dev.off()

