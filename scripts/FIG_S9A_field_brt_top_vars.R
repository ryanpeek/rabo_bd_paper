# field BRT results with top vars


# Libraries ---------------------------------------------------------------

library(tidymodels)
library(xgboost)
library(patchwork)
library(vip)
library(here)
library(glue)
library(tidyverse)

# now explore
load(here("models/brt_xgboost_field_bd.rda"))

# extract final model fit
final_xgb_fit <- final_xgb %>% fit(data=rb_train)

# view table
final_xgb_fit %>%
  extract_fit_parsnip() %>% vi() %>%
  DT::datatable() %>% DT::formatPercentage("Importance", digits=1)

# plot
# make a version with better names?
vi_final <- final_xgb_fit %>%
  extract_fit_parsnip() %>% vi() %>%
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  dplyr::slice(1:8) %>%
  # fix names
  mutate(Variable = case_when(
    grepl("tmax_noaa", Variable) ~ "Max. Temperature",
    grepl("latitude_dd", Variable) ~ "Latitude",
    grepl("daylen_hr", Variable) ~ "Daylength",
    grepl("AG_HYDRIC_PCT_WS", Variable) ~ "% Agriculture on Hydric Soil",
    grepl("PCT_WETLANDS_REMAINING", Variable) ~ "% Wetlands Remaining",
    grepl("spei24", Variable) ~ "SPEI 24 month",
    grepl("elevation", Variable) ~ "Elevation",
    grepl("capt_yr", Variable) ~ "Capture Year")) %>%
  #grepl("spei12", Variable) ~ "SPEI 12 month",
  #grepl("longitude_dd", Variable) ~ "Longitude",
  #grepl("NRSA_AQUATIC_COND_MEAN", Variable) ~ "Prob of Good Biological Condition",
  #grepl("PCT_IMPERVIOUSNESS2011_MEAN_WS", Variable) ~ "% Impervious Surface",
  #grepl("ROADS_ALL_DENSITY_2014_RZ", Variable) ~ "Road Density in Riparian Zone",
  #grepl("areasqkm", Variable) ~ "Watershed Area (HUC12)",
  #grepl("PCT_FOREST_REMAIN", Variable) ~ "% Forest Remaining")) %>%
  mutate(Variable = fct_reorder(Variable, Importance))

# plot
vi_final %>%
  ggplot() +
  geom_vline(xintercept = 0.05, lty=2, col="gray") +
  geom_linerange(aes(xmin=0, xmax=Importance, y=Variable), col="steelblue", linewidth=1) +
  geom_point(aes(x=Importance, y=forcats::fct_reorder(Variable, Importance)), col="steelblue", size=4) +
  cowplot::theme_cowplot(font_family = "Roboto Condensed") +
  theme(plot.background = element_rect(fill="white")) +
  labs(x="Relative Importance", y=NULL, subtitle = "Boosted Regression Tree: Field (Bd+)") +
  scale_x_continuous(labels=scales::percent)

# save:
ggsave(here("figs/Fig_S9A_modeling_xgb_var_importance_field.png"), width = 7, height = 4.5, dpi=300)
