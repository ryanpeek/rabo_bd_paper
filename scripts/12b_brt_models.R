# 12_BRT Models

# run BRTs for
# 1. field
# 2. museum
# 3. Load > 0 only samples

# Libraries ---------------------------------------------------------------

library(purrr) # for working with lists/loops
library(tidylog)
library(glue)
library(tidyverse) # all the things
library(viridis) # colors
library(sf) # spatial operations
library(mapview) # web maps
mapviewOptions(homebutton = FALSE, fgb = TRUE, viewer.suppress = FALSE)

library(gbm) # boosted regression trees
library(rsample) # sampling
library(rlang)
library(dismo)
source("scripts/My.gbm.step.R")

set.seed(321) # reproducibility

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

# get geomorphic provinces
geom_reg <- st_read("data/ca-geomorphic-provinces/california-geomorphic-provinces.gpkg", "california_geomorphic_provinces")
#sf::sf_use_s2(FALSE)

# simplify
geom_reg <- rmapshaper::ms_simplify(geom_reg)

# mapview(geom_reg, zcol="RANGE_NAME")

# 02. Select a Region ---------------------------------------------------------

(ecoregs <- unique(rb_sf$geomorph_region))

# if selecting by a specific region use region select
modname <- "sierras"   # "Sierra Nevada"
(Hregions <- c(ecoregs[1])) # set a region or regions

# now filter data to region(s) of interest
region_sel <- rb_sf %>% filter(geomorph_region %in% Hregions)

mapview(region_sel, zcol="geomorph_region") +
  mapview(geom_reg, zcol = "RANGE_NAME")

# 03. Setup Data for Model ----------------------------------------------------------------

bd_data <- rb_sf %>% st_drop_geometry() %>%
  filter(field_or_museum=="Field") %>%
  dplyr::select(bd_positive,
                regulation_0_unreg_1_upstream_dam_2_downstream_dam:weight_g,
                huc10, huc12, daylen_hr:elevation, geomorph_region)

# check how many rows/cols: KEEP ALL FOR NOW
library(naniar)
gg_miss_var(bd_data)

# show how many missing combos:
library(VIM)
summary(aggr(bd_data, sortVar=TRUE))$combinations

# drop a few cols
bd_data <- bd_data %>%
  dplyr::select(-c(WBAREA_SUP_IMP_DIFF_PCTA, spei8, spei_date, STREAMLGTH_SUP_IMP_DIFF_PCTA, weight_g, NRSA_AQUATIC_COND_OUTLET)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  as.data.frame

gg_miss_var(bd_data)
summary(aggr(bd_data, sortVar=TRUE))$combinations

# 06. GBM.STEP MODEL  ------------------------------------------------------------

# set up tuning params
hyper_grid <- expand.grid(
  shrinkage = c(0.005),
  interaction.depth = c(5),
  n.minobsinnode = c(5),
  bag.fraction = c(0.75)
)

# double check and view
hyper_grid

# load the GBM.step function (requires dismo and function loaded)
gbm_fit_step <- function(
  shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_step <- My.gbm.step(
    gbm.y = 1, # response in training data
    gbm.x = 2:ncol(data), # hydro dat
    family = "gaussian",
    data = data,
    n.trees = 100,
    #max.trees = 8000, # can specify but don't for now
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = FALSE,
    verbose = FALSE)

  # Compute the Deviance Explained: (total dev - cv dev) / total dev
  val <- if(!is.null(m_step$self.statistics$mean.null)){
    (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) / m_step$self.statistics$mean.null}
  else(NA_real_)

  return(val)
}

# 07. RUN GBM.STEP WITH PURRR ---------------------------------------------
# possibly(.f = gbm_fit_step, otherwise = NA_real_)

# use PURRR: this part can take awhile...get some coffee
hyper_grid$dev_explained <- purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = bd_data))

# 08. VIEW AND SAVE MODEL RESULTS -----------------------------------------

# look at results:
hyper_grid %>%
  dplyr::arrange(desc(dev_explained)) %>%
  head(1) # top 5 models

# pick the best solution
(hyper_best <- hyper_grid %>%
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))

# write these all out to a file for reference later
(gbm_file <- paste0("models/12_gbm_final_hypergrid"))

# check for file and delete?
if(fs::file_exists(path = paste0(gbm_file,".csv"))){
  fs::file_delete(path = paste0(gbm_file, ".csv"))
  print("File deleted, time for a fresh start!")
} else {
  print("No file saved yet")
}

# save out
write_csv(hyper_grid, file = paste0(gbm_file,".csv"), append = TRUE)

# read in
hyper_grid <- readr::read_csv(file = paste0(gbm_file,".csv"), col_names = c("shrinkage", "interaction.depth", "n.minobsinnode", "bag.fraction", "dev_explained"))

# get best model solution
(hyper_best <- hyper_grid %>%
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))

# final function
gbm_final_step <- function(
  shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_final <- My.gbm.step(
    gbm.y = 1, # response in training data
    gbm.x = 2:ncol(data), # hydro dat
    family = "gaussian",
    data = data,
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = TRUE,
    verbose = TRUE
  )
}

gbm_fin_out <- purrr::pmap(
  hyper_best,
  ~ gbm_final_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = bd_data # CHECK AND CHANGE!!
  )
)

#strip off a list layer to view data
(gbm_fin_out <- gbm_fin_out[[1]])

# 10. SAVE FINAL GBM AND DATA ---------------------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(glue("gbm_final_bd_positive_field")), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = glue("gbm_final_bd_positive_field")))

# save to RDS
write_rds(x = get(fileToSave), file = glue("models/12_{fileToSave}_model.rds"), compress = "gz")

# Plot --------------------------------------------------------------------

gbm_fin_out <- read_rds("models/12_gbm_final_bd_positive_field_model.rds")

gbm_fin_RI<-as.data.frame(summary(gbm_fin_out, plotit = F, method=relative.influence)) %>%
  mutate("Ymetric"= "bd_positive",
         "datset" = "all",
         "method" = "mse")


gbm_fin_RI %>%
  arrange(desc(rel.inf)) %>%
  ggplot(.) +
  geom_col(aes(x=var,
               y=rel.inf, fill=datset), color="gray20", lwd=.1,
           position="dodge") +
  coord_flip() +
  geom_hline(yintercept = 5, color="gray40", lwd=1, lty=2, alpha=0.8)

