# Dotplot of Bd Samples Latitude by Year

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(glue)
library(purrr)
library(cowplot)
library(broom)
library(mgcv)
library(tidymv)
library(gratia)
library(visreg)

# Load Data ---------------------------------------------------------------

# full covars
rb_covar <- read_rds("output/10a_rb_bd_all_covars_recap_filt.rds")
# went from n=2145 to n=2073

#get H10 and H12s for geo file:
# h12 <- st_read("output/rabo_bd_spatial_data.gpkg", "HUC12")

# load from output
load("output/h12.rda")

## Summarize per HUC ---------------------------------------------------

# this adds number of Bd+/- samples per field or museum by huc12
rb_h12_tally <- rb_covar %>% st_drop_geometry() %>%
  group_by(field_or_museum, huc12, bd_positive, capt_yr) %>%
  tally() %>% ungroup() %>%
  # get total samples for each huc12
  group_by(capt_yr, huc12) %>%
  add_tally(n, name="tot_n_by_h12_yr") %>%
  arrange(huc12, capt_yr, field_or_museum, bd_positive) %>%
  # calculate prevalence
  mutate(prevalence = n/tot_n_by_h12_yr) %>%
  select(huc12, field_or_museum, bd_positive, n, prevalence, total_samples=tot_n_by_h12_yr) %>%
  inner_join(h12[,c(14)]) %>% st_sf() %>%
  ungroup() %>%
  right_join(., st_drop_geometry(rb_covar)) %>% st_drop_geometry()

# make distinct by H12 and latitude
df_distinct <- rb_h12_tally %>%
  st_drop_geometry() %>%
  distinct(huc12, latitude_dd, capt_yr, bd_positive, .keep_all = TRUE) %>%
  # make year a factor
  mutate(capt_yr_f = as.factor(capt_yr), .after="capt_yr") %>%
  select(huc12:total_samples, capt_yr, capt_yr_f, capt_mon,locality, simplified_locality, river_system_watershed, latitude_dd, longitude_dd, huc10) %>%
  ungroup()

# make only positive samples
df_pos <- rb_h12_tally %>%
  filter(bd_positive==1) #%>% # n=629
  #distinct(latitude_dd, .keep_all = TRUE) # 260 points

# get only negative samples
df_neg <- rb_h12_tally %>%
  filter(bd_positive==0) #%>% # 1444
  #distinct(latitude_dd, .keep_all = TRUE) # 503 points

# get total samples by huc12 back in rb_covar
rb_covar2 <- left_join(st_drop_geometry(rb_covar), rb_h12_tally[,c(4:6,8)]) %>%
  st_as_sf(coords=c("longitude_dd","latitude_dd"), remove=FALSE, crs=4326)

# Scale Parameters --------------------------------------------------------

df_distinct_s <- df_distinct %>%
  mutate(across(c(n, total_samples, capt_yr, latitude_dd), ~scale(.x), .names = "{.col}_s"))

# GAMS: All Bd vs lat + year  --------------
# notes
# cr: cubic spline defined by knots spread evenly through covariates
# cs: shrinkage version of cubic spline basis
# tp: isotropic smoother for any number of covariates
# ts: like tp but modification to smoothing penalty for null space

# use select=TRUE to apply double penalty approach to all smooths in model, Marra and Wood (2011) suggested double penalty approach worked slightly better than the shrinkage smother approach (i.e., adding bs="cs" or "ts" for shrinkage).
# scale data

m1_all <- df_distinct_s %>%
  filter(bd_positive==1) %>%
  mgcv::gam(latitude_dd_s ~ total_samples + s(capt_yr_s),
            data = ., method="REML", select=TRUE)

all_pos_r2 <- round(summary(m1_all)$r.sq, 3)

## Evaluate Model --------------------------------

appraise(m1_all)
summary(m1_all) # R2 = 0.134, deviance 14%
draw(m1_all) # draw partial effect plot
gam.check(m1_all)

library(performance)
model_performance(m1_all)
library(easystats)
plot(check_collinearity(m1_all), type="qq")
plot(estimate_expectation(m1_all, data="grid"))

## Data Vis of Model -----------------

# more vis
m1_pred <- tidymv::predict_gam(m1_all)
m1_pred %>%
  ggplot(aes(x=capt_yr_s, y=fit)) +
  geom_smooth_ci(total_samples, show.legend=FALSE)

# plot parms
library(ggeffects)

ggplot()+ geom_line(data=ggpredict(m1_all, terms=c("capt_yr_s [all]", "total_samples [all]"), ci=TRUE), aes(x=x, y=predicted, group=group, color=group)) +
  labs(title="", x="Year", y = "Latitude (scaled)")

# make R2 file:
library(ggtext)
r2_md <- tibble(
  x=c(1910),
  y=c(41.8),
  label = c(glue("*r*<sup>2</sup> = {all_pos_r2}")))
r2_md


# PLOT: YR SAMPLED VS LATITUDE --------------------------------------------

colblind_pal <- c('#009E73','#E69F00')

# plot
(p1_samp <- ggplot() +
    # plot all the negatives in hollow gray
    geom_point(data = rb_h12_tally %>%
                 filter(bd_positive==0),
               aes(y=latitude_dd, x=capt_yr,
                   shape=field_or_museum,
                   size=total_samples),
               alpha=0.3, color="gray40") +
    # fit a line for all positives for both mus + field
    stat_smooth(data= df_distinct_s %>% filter(bd_positive==1),
                aes(y=latitude_dd, x=capt_yr),
                    #color=field_or_museum, group=field_or_museum),
                method = "gam", formula = y ~ splines::bs(x),
                alpha=0.2, color=alpha("gray30", 0.8),
                lwd=1) +

    # settings
    scale_y_continuous(breaks=c(seq(32,42.5,2)),
                       labels=glue("{seq(32,42.5,2)}°N"),
                       expand = c(0, .2)) +
    scale_x_continuous(labels = c(seq(1900,2021,20)),
                       breaks = c(seq(1900,2021,20))) +
    geom_point(data = rb_h12_tally %>% filter(bd_positive==1),
               aes(y=latitude_dd, x=capt_yr, shape=field_or_museum,
                   fill=field_or_museum, size=total_samples),
               alpha=0.7, color="gray40") +
    # geom_richtext(data=r2_md,
    #               aes(x=x, y=y,
    #                   label=label,
    #                   fill = after_scale(alpha(color, 0.2))),
    #               text.colour = "black", vjust = 1, show.legend = FALSE) +
    scale_size_binned("Samples\nper Watershed",
                      breaks=c(1,10,100,500),
                      range=c(1,9)) +
    theme_cowplot(font_size = 16) +
    background_grid(major = "xy", color.major = "gray90",size.major = 0.2)+
    guides(fill = guide_legend(override.aes = list(size=4))) +
    labs(y="Latitude",#y="Latitude (south to north)",
         x="Year Sampled") +
    scale_shape_manual("Type", values = c("Field"=21, "Museum"=23))+
    scale_fill_manual("Type", values = colblind_pal) +
    scale_color_manual("Type", values = colblind_pal) +

    theme(
      axis.title = element_text(size=16),
      axis.text.y = element_text(size = 16),
      legend.position = c(0.05, 0.2),
      legend.box.just = "bottom",
      legend.spacing = unit(16, "lines"),
      legend.box = "horizontal",
      axis.text.x = element_text(size = 16,
                                 vjust = 0.5, hjust=0.5),
      plot.background = element_rect(fill = "white")))


# save out
#ggsave("figs/Fig_01B_year_by_lat_for_museum_v_field_all_samples_pos_gam_only_wR2.png", width = 11, height = 8, dpi=600)

# save out
ggsave("figs/Fig_01B_year_by_lat_for_museum_v_field_all_samples_pos_gam_only_v2.png",
       height = 7.25, width = 7.5, dpi=600)

## Save Mod Out ------------------------------------------------------------

## save out
sink(file = "output/fig_01b_gam_results_museum_bd+_yr_totsamp_vs_lat.txt", append=TRUE)
summary(m1_all)
glue("\n\n Summary:")
glance(m1_all)
glue("\n\n Final R^2")
m1_r2 <- round(summary(m1_all)$r.sq, 3)
glue("R^2 is {m1_r2}")
sink()
