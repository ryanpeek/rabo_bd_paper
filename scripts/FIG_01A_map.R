# Map w bd by lat

# Standardize symbols - orange diamond museum positives, green circles field positives (or could do purple and yellow on both), instead of gray lines for watershed boundaries or counties, maybe show the boundaries of the 6 clades

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(fs)
library(sf)
library(glue)
library(janitor)
library(lubridate)
library(purrr)
library(tmap)
#library(USAboundaries)
library(mapview)
mapviewOptions(fgb = FALSE)
library(cowplot)
library(ggtext)

# Load Data ---------------------------------------------------------------

# full covars
rb_covar <- read_rds("output/10a_rb_bd_all_covars_recap_filt.rds")
nrow(rb_covar)
# went from n=2145 to n=2073

# load from output
load("output/h12.rda")

## Summarize per HUC ---------------------------------------------------

# this adds number of Bd+/- samples per field or museum by huc12
rb_h12_tally <- rb_covar %>% st_drop_geometry() %>%
  group_by(field_or_museum, huc12, bd_positive) %>%
  tally() %>% ungroup() %>%
  # get total samples for each huc12
  group_by(field_or_museum, huc12) %>%
  add_tally(n) %>%
  # calculate prevalence
  mutate(prevalence = n/nn) %>%
  #group_by(huc12) %>%
  #add_count(wt = n) %>%
  select(huc12, field_or_museum, bd_positive, n, prevalence, total_samples=nn) %>%
  inner_join(h12[,c(14)]) %>% st_sf() %>%
  right_join(., st_drop_geometry(rb_covar))

# make only positive samples
df_pos <- rb_h12_tally %>%
  filter(bd_positive==1) #%>% # n=629
  #distinct(latitude_dd, .keep_all = TRUE) # 256 points

# get only negative samples
df_neg <- rb_h12_tally %>%
  filter(bd_positive==0) #%>% # 1444
  #distinct(latitude_dd, .keep_all = TRUE) # 503 points

# all samples
df_all <- rb_h12_tally #%>%
  #distinct(latitude_dd, .keep_all = TRUE)

# get total samples by huc12 back in rb_covar
rb_covar2 <- left_join(st_drop_geometry(rb_covar), rb_h12_tally[,c(4:6,8)], by=c("sampleid")) %>%
  select(-geom) %>%
  st_as_sf(coords=c("longitude_dd","latitude_dd"), remove=FALSE, crs=4326)

#mapview(rb_covar2 %>% filter(field_or_museum=="Field"), zcol="bd_positive", legend=FALSE)

## Mapview Preview by Clade ------------------------------------------------

#mapview(rb_covar2, zcol="boylii_clade")

# make convex hulls by clade
rb_hulls <- rb_covar2 %>%
  group_by(boylii_clade) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull()

# mapview
# mapview(rb_covar2, zcol="boylii_clade", layer.name="Clades") +
#   mapview(rb_hulls, zcol="boylii_clade", layer.name="Hulls")

# library(smoothr)
# rb_hull_smooth <- rb_hulls %>% st_buffer(1000) %>%
#   smooth(method="ksmooth", smoothness=20)
#
# mapview(rb_covar2, zcol="boylii_clade", layer.name="Clades") +
#   mapview(rb_hull_smooth, zcol="boylii_clade", layer.name="Hulls")
#
# # write out to edit in another program
# st_write(rb_hull_smooth, dsn = "output/clades_hulls_smooth.kml", layer = "rb_clade_hulls")
#
# rb_covar2 %>% select(sampleid, field_or_museum, date_captured, county, boylii_clade, simplified_locality, latitude_dd, longitude_dd, capt_yr) %>% st_write(., dsn="output/clades_pts_smooth.kml", layer="rb_pts")

# read in desktop clades
rb_clades <- st_read("output/clades_rabo.kml", layer = "clades") %>%
  select(-Description) %>%
  rename(boylii_clade = Name) %>%
  mutate(boylii_clade = case_when(
    boylii_clade == "N. Feather" ~ "Feather",
    TRUE ~ boylii_clade)) %>%
    mutate(boylii_clade = factor(boylii_clade,
         levels=c("N. Coast", "C. Coast",
                  "S. Coast","Feather",
                  "N. Sierra","S. Sierra")))
levels(rb_clades$boylii_clade)


# mapview(rb_covar2, zcol="boylii_clade", layer.name="Clades") +
#   mapview(rb_clades, zcol="boylii_clade", layer.name="Hulls")

# Basic Barplots -----------------------------------

colblind_pal <- c('#009E73','#E69F00')

df_all_tally <- rb_covar2 %>% st_drop_geometry() %>%
  group_by(field_or_museum) %>% tally()

(bar1 <- ggplot() + geom_col(data=df_all_tally, aes(y=n, x=field_or_museum, group=field_or_museum, fill=field_or_museum), width = 0.8, show.legend = FALSE) +
  #theme_half_open() +
    theme_minimal_hgrid() +
  scale_fill_manual(values = c("Field"=colblind_pal[1], "Museum"=colblind_pal[2])) +
  labs(x=NULL, y=NULL, subtitle="All Samples"))

df_pos_tally <- rb_covar2 %>% st_drop_geometry() %>%
  filter(bd_positive==1) %>%
  group_by(field_or_museum) %>% tally()

(bar2 <- ggplot() + geom_col(data=df_pos_tally, aes(y=n, x=field_or_museum, group=field_or_museum, fill=field_or_museum), width = 0.8, show.legend = FALSE) +
    #theme_half_open() +
    theme_minimal_hgrid() +
    scale_fill_manual(values = c("Field"=colblind_pal[1], "Museum"=colblind_pal[2])) +
    labs(x=NULL, y=NULL, subtitle="Bd+ Samples"))

bar_combined <- plot_grid(bar1, bar2, nrow = 2, align = "v") +
  theme(plot.background = element_rect(fill="white"))
bar_combined

cowplot::save_plot(filename="figs/Fig_01A_barplot_hgrid.jpg", plot=bar_combined, base_width = 2, bg="white")

# MAP: Get Basemap Layers ------------------------------------------------------

library(USAboundaries)
# get counties and states
ca_co<-us_counties(states=c("ca","or", "nv","id"))
ca<-us_states(states=c("ca","or","id","nv"))
load("data/major_rivers_dissolved.rda")

# buffer out bbox for background
bd_buff <- rb_covar2 %>%
  st_bbox() %>% # make a boundary box
  st_as_sfc() %>% # this converts bbox to polygon
  st_transform(3310) %>% # need to make it metric for easier buffer
  st_buffer(dist = 50000) %>%  # add 50km buffer
  st_transform(4326) # convert back to lat/lon

# crop rivers to CA only and filter to rivers only
rivs_ca <- st_intersection(rivs, ca) %>%
  filter(FEATURE_TYPE == "river")

## ggplot map of points  -------------------------------------------------------------

# colorblind palette: scales::show_col(ggthemes::colorblind_pal()(8))
# "#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7"
colblind_pal <- c('#009E73','#E69F00')

# make distinct sample set
rb_sf <- rb_covar2 %>% distinct(latitude_dd, longitude_dd, .keep_all = TRUE)

# map
(ggCA <- ggplot() +
    #coord_sf(label_graticule= "W", label_axes = "W---") +
    #geom_hline(yintercept = c(seq(32,42.5,2)), color="gray40", alpha=0.5) +
    #geom_vline(xintercept = c(seq(-124,-118, 2)), color="gray40", alpha=0.5) +
    geom_sf(data=ca) +
    labs(y="Latitude", x="Longitude") +
    geom_sf(data=rb_clades, fill=NA, col="gray40", linewidth=0.5, alpha=0.8) +
    geom_point(data=rb_sf %>% filter(bd_positive==0),aes(x=longitude_dd, y=latitude_dd, shape=field_or_museum),
            color="gray10", size=2, stroke=1, show.legend = TRUE) +
    geom_point(data=rb_sf %>% filter(bd_positive==1),aes(x=longitude_dd, y=latitude_dd, shape=field_or_museum, fill=field_or_museum),
            color="gray10",size=3, stroke=0.35, show.legend = TRUE) +
    scale_shape_manual("Type", values = c("Field"=21, "Museum"=23))+
    scale_fill_manual("Type", values = c(colblind_pal)) +
    scale_x_continuous(breaks=c(-124,-122,-120,-118), labels=glue("{c(-124,-122,-120,-118)}°W"),limits =c(-124.4,-117))+
    scale_y_continuous(limits=c(32,42.5),labels=glue("{c(seq(32,42,2))}°N"), expand = c(0,0.2))+

    #geom_sf_text(data=rb_clades, aes(label=boylii_clade), nudge_y = 0.5)+

    ggspatial::annotation_north_arrow(height = unit(1.5, "cm"), width = unit(1, "cm")) +
    # add this to match the scale of the fig1b plot
    coord_sf(ylim=c(32,42.5), xlim=c(-124.4,-117)) +
    theme(
      plot.background = element_rect(fill="white", linetype = 0),
      panel.background = element_rect(linetype = 0, fill="white"),
      panel.grid = element_line(color = "gray80", linewidth = 0.2),
      axis.title = element_text(size=16),
      legend.position = c(0.25,0.15),
      legend.background = element_blank(),
      legend.text = element_text(size=16),
      legend.title = element_text(size=16),
      axis.text = element_text(size=16),
      axis.ticks = element_line(color="gray50"),
      legend.key=element_blank()))


#leg <- cowplot::get_legend(ggCA)
#plot_grid(ggCA) + leg

# save just map
#ggsave(ggCA, filename = "figs/Fig_01A_simple_map_no_legend.jpg",
#       width = 8, height = 11, dpi=600)

# make legend say TRUE
ggsave(ggCA, filename = "figs/Fig_01A_simple_map_w_legend_v2.jpg",
       width = 5.25, height = 7.5, dpi=600)

## ggplot: Create Side by Side Plot -----------------

# library(patchwork)
#
# layout <- c(
#   area(t = 1.3, l = 0.1, b = 2, r = 5), # map
#   area(t = 1, l = 6, b = 2, r = 11) # line plot
# )

# ggCA + p1_samp +
#   plot_layout(design = layout)
#
# # save out
# ggsave("figs/year_by_lat_for_museum_v_field_all_samples_wR2_all_w_map.png",
#        width = 11, height = 8, dpi=300)
#
# ggsave("figs/year_by_lat_for_museum_v_field_all_samples_wR2_all_w_map.pdf",
#        width = 11, height = 8, dpi=300)
#
library(cowplot)

ggdraw(ggCA) +
  draw_plot(bar_combined, 0.55, 0.45, width = 0.5, height = 0.75, scale = 0.6)

# try with inset
ggsave(filename = "figs/Fig_01A_w_inset_test.jpg",
       width = 5.25, height = 7.5, dpi=600)


