# 12_corr reduction

# Libraries ---------------------------------------------------------------

library(purrr) # for working with lists/loops
library(glue)
library(tidyverse) # all the things
library(viridis) # colors
library(sf) # spatial operations
library(mapview) # web maps
mapviewOptions(homebutton = FALSE, fgb = TRUE, viewer.suppress = FALSE)
library(rstatix)
library(corrplot)

# 01. Load Data ---------------------------------------------------------------

# load updated data:
rb_sf <- read_rds("output/09_rb_bd_all_covariates_joined.rds")

# just sites
load("output/02_rb_bd_localities_only.rda")

table(rb_sf$RANGE_NAME)

# for geomroph in OR, just use "OR"
rb_sf <- rb_sf %>%
  mutate(geomorph_region = case_when(
    is.na(RANGE_NAME) ~ "OR",
    TRUE ~ RANGE_NAME)
  )

# bd_data <- rb_sf %>% st_drop_geometry() %>%
#   #filter(field_or_museum=="Field") %>%
#   dplyr::select(regulation_0_unreg_1_upstream_dam_2_downstream_dam:weight_g,
#                 huc10, huc12, daylen_hr:elevation, geomorph_region)

# Corr --------------------------------------------------------------------

# cor matrix with p-values
bd_data <- rb_sf %>% st_drop_geometry() %>%
  select(!where(is.character)) %>%
  select(!where(is.factor)) %>%
  select(-c(bd_positive:bd_its_copies, date_captured, spei_date))

# make matrix
cor_mat1 <- bd_data %>% cor_mat(method = "pearson", conf.level = 0.95)
# cor_mat1 %>% rstatix::cor_plot()

hcorrs <- rstatix::cor_gather(cor_mat1) %>%
  dplyr::filter(cor >= 0.5 | cor <= -0.5) %>%
  dplyr::filter(p > 0)

View(hcorrs)

# write out:
write_csv(hcorrs, file = "output/12a_correlated_vars.csv")

# Vars to Drop ------------------------------------------------------------

# vars to drop
todrop <- c("longitude_dd",
            "pmdi_noaa", "phdi_noaa",
            "tmin_noaa", "tmpc_noaa",
            "areaacres", "Shape_Length",
            "N_INDEX2_CDLNLCD11_PCT_WS",
            "N_INDEX2_CDLNLCD11_PCT_HAZ")

# drop
bd_data <- bd_data %>% select(-c({{todrop}}))
