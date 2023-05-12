# 13_AR1

# resources:
# https://www.tidymodels.org/learn/models/time-series/

# Libraries ---------------------------------------------------------------

library(broom)
library(tidyverse)
library(ggthemes)
library(purrr)
library(glmmTMB)
library(sf)

# Get Data ----------------------------------------------------------------

rb_covar <- read_rds("output/09_rb_bd_all_covariates_joined.rds") %>%
  mutate(sampleid2 = janitor::make_clean_names(sampleid), .after=sampleid)

h12 <- st_read("output/rabo_bd_spatial_data.gpkg", "HUC12")

# summarize by HUC
rb_h12_sum <- rb_covar %>%
  group_by(huc12, bd_positive) %>%
  tally() %>% ungroup() %>%
  group_by(huc12) %>%
  add_count(wt = n) %>%
  mutate(prevalence = n/nn) %>%
  rename(total_samples=nn) %>%
  inner_join(h12[,c(14)]) %>% st_sf()


# Model -------------------------------------------------------------------

## Bd Load (Gaussian) / (only positives) ---------------------


m1 <- glmmTMB(bd_positive ~ daylen_hr + capt_mon + pdsi_noaa + spei + elevation +
                (1|boylii_clade) + (1|field_or_museum) + ar1(as.factor(capt_yr) + 0|boylii_clade),
              data = rb_covar, family = binomial())

summary(m1)
m1
