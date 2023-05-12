# Run SATSCAN!

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(fs)
library(sf)
library(glue)
library(janitor)
library(lubridate)
library(purrr)
library(mapview)
mapviewOptions(fgb = FALSE)

library(rsatscan)
# see https://www.satscan.org/rsatscan/rsatscan.html
# download gz: https://cran.r-project.org/src/contrib/Archive/rsatscan/
# install with:
# install.packages("~/Downloads/rsatscan_0.3.9200.tar.gz", repos = NULL, type="source")

# Models ------------------------------------------------------------------

## MUSEUM MODELS:
## BERN of H12 with 50% of pop, 50km window, and 3 min

## FIELD MODELS:
## BERN of H12 with 50% of pop, 50km + 10 min

# pick one based on distributions
## POISSON of JUST POSITIVE log(ITS) with 50% of pop, 50km window, and 10 min for high rate clust
## NORMAL of JUST POSITIVE log(ITS) with 50% of pop, 50km window, and 10 min

# compare BERN and POISSON to see if clusters are really diff...if so, we could pull out just the load models to use the POISSON in the AR1

## NOT DIFFERENT SO DROP
## BERN of Indiv with 50% of pop, 50km + 10 min
## BERN of Indiv with 50% of pop, 50km window, and 3 min for high rate clust

# Load Data ---------------------------------------------------------------

# updated from: output/09_rb_bd_all_covariates_joined.rds on July 2022
rb_covar <- read_rds("output/10a_rb_bd_all_covars_recap_filt.rds")

#get H10 and H12s for geo file:
h12 <- st_read("output/rabo_bd_spatial_data.gpkg", "HUC12")
h10 <- st_read("output/rabo_bd_spatial_data.gpkg", "HUC10")

# Calculate Area of HUCS --------------------------------------------------

# get mean watershed area of hucs, use to set SatScan max window
mean(st_area(h12)) %>% units::set_units("km^2") # 92.63981 [km^2]
mean(st_area(h10)) %>% units::set_units("km^2") # 428.6438 [km^2]
# based on this, a radius of 50km is likely sufficient (~100km diam)

# Prep Rb Bd Data ------------------------------------------------

# first we need unique spatial list of sites and associated group or site.
# need to replace special characters and spaces in IDs
rb_covar <- rb_covar %>% mutate(
  sampleid2 = janitor::make_clean_names(sampleid), .after=sampleid)

# now look for distinct records
rb_covar %>%
  distinct(sampleid, .keep_all = TRUE) %>%
  nrow() # n=2073 (same)

rb_covar %>% group_by(field_or_museum) %>% tally()
# n=1612 unique field records, 461 museum

# hist of its
rb_covar %>% filter(bd_positive==1, bd_its_copies>0) %>%
  ggplot(aes(y=log(bd_its_copies))) + geom_histogram()

rb_h12_tally <- rb_covar %>% st_drop_geometry() %>%
  group_by(field_or_museum, huc12, bd_positive) %>%
  tally() %>% ungroup() %>%
  group_by(field_or_museum, huc12) %>%
  add_tally(n) %>%
  mutate(prevalence = n/nn) %>%
  group_by(huc12) %>%
  add_count(wt = n) %>%
  select(huc12, field_or_museum, bd_positive, n, prevalence, total_samples=nnn) %>%
  inner_join(h12[,c(14)]) %>% st_sf() %>%
  right_join(., st_drop_geometry(rb_covar))

# Look at Records by HUC --------------------------------------------------

datatype <- "field" # all, museum, field

# HUC12:
# FIELD n=39 unique h12, 64 rows
# MUSEUM n=143 unique, 158 rows
(bd_h12_summary <- rb_covar %>% st_drop_geometry() %>%
    #filter(bd_positive==1) %>% # for only positives!
    filter(field_or_museum==str_to_title(datatype)) %>%
    group_by(huc12, bd_positive) %>%
    tally() %>% ungroup() %>%
    mutate(huc12=as.factor(huc12)))
# complete all H12 so record for bd + and bd -, fill with zero
#complete(expand(., huc12, bd_positive), fill=list(n=0)) %>%


# look at distrib?
hist(bd_h12_summary$n, breaks = 25)

# write out
# write_csv(bd_h12_summary, file = glue("output/10_bd_h12_summary_{datatype}.csv"))


# Make HUC Data Layers ----------------------------------------------------

datatype <- "field" # all, museum, field

# join and get pts and H12s
rb_hucs <- rb_covar %>% st_drop_geometry() %>%
  select(sampleid, field_or_museum, boylii_clade, latitude_dd, longitude_dd, date_captured,
         bd_positive, bd_its_copies, capt_yr, capt_mon, huc12, huc10) %>%
  # filter to field/museum only, comment out for all samples
  filter(field_or_museum==str_to_title(datatype))

# summarize by HUC
rb_h12_sum <- rb_hucs %>%
  #group_by(huc12) %>%
  #summarize(bd_positive_tot = sum(bd_positive)) %>%
  group_by(huc12, bd_positive) %>%
  tally() %>% ungroup() %>%
  group_by(huc12) %>%
  add_count(wt = n) %>%
  mutate(prevalence = n/nn) %>%
  rename(total_samples=nn) %>%
  inner_join(h12[,c(14)]) %>% st_sf()

# mapview(rb_h12_sum, zcol="bd_positive_tot")

# Create BERN HUC SatScan -----------------------------------------------------

vrsn <- "v3"

# set model name:
mod <- "bern" # bernoulii

## H12 *.geo file --------------------------------------------------------

# first we need to collapse the hucs to distinct centroid points for use
# in the .geo (coordinates) file for satscan

# make a geo/spatial layer
rb_bd_geo12 <- rb_h12_sum %>%
  mutate(lon=map_dbl(geom, ~st_centroid(.x)[[1]]),
         lat=map_dbl(geom, ~st_centroid(.x)[[2]])) %>%
  select(huc12, lat, lon) %>%
  distinct(.keep_all = TRUE) %>%
  st_drop_geometry() %>% as.data.frame()

# write out
write.geo(rb_bd_geo12, location = glue("output/satscan/{vrsn}/"), file = glue("rb_bd_{mod}_h12_{datatype}"), userownames=FALSE)


## H12 *.cas file ---------------------------------------------------

# make a cases (data) layer (only n=26 field, 24 mus)
rb_bd_cas12 <- rb_h12_sum %>%
  filter(bd_positive==1) %>% # for only positive cases
  select(huc12, n) %>%
  filter(n>0) %>%
  st_drop_geometry() %>% as.data.frame()

write.cas(rb_bd_cas12, location = glue("output/satscan/{vrsn}/"), file = glue("rb_bd_{mod}_h12_{datatype}"), userownames=FALSE)

# double check name
glue("rb_bd_{mod}_h12_{datatype}")

## H12 control *.ctl file ---------------------------------------------------

# The control file is only used with the Bernoulli model. It should contain the following information:
# location id: Any numerical value or string of characters. Empty spaces may not form part of the id.
# controls: The number of controls for the specified location and time
# time: Optional. Time may be specified either in years, months or days, or in a generic format. All control times must fall within the study period as specified on the Analysis tab. The format of the times must be the same as in the case file.
# Note: Multiple lines may be used for different controls with the same location, time and attributes. SaTScan will automatically add them

# make a control layer
rb_bd_ctl12 <- rb_h12_sum %>%
  filter(bd_positive==0) %>% # for only non cases (N=33 hucs for field)
  select(huc12, n) %>%
  st_drop_geometry() %>% as.data.frame()

write.ctl(rb_bd_ctl12, location = glue("output/satscan/{vrsn}/"), file = glue("rb_bd_{mod}_h12_{datatype}"), userownames=FALSE)

rm(rb_bd_cas12, rb_bd_ctl12, rb_bd_geo12)

# NORM INDIV ITS SatScan ------------------------------------------------------

# here we create layers using ITS load data using individual point data from Field only.
# need to group by locality to get case counts...

## FIELD MODELS:
## NORMAL of ITS with 50% of pop, 50km window, and 10 min

## Indiv *.geo file --------------------------------------------------------

datatype <- "field" # field only for ITS

# make layer based on indiv for normal/poisson model
rb_bd_geo_h12_its <-
  rb_covar %>% st_drop_geometry() %>%
  filter(field_or_museum==str_to_title(datatype)) %>%
  filter(!is.na(bd_its_copies)) %>%
  # for h12 version
  inner_join(h12[,c(14)]) %>% st_sf() %>%
  mutate(lon=map_dbl(geom, ~st_centroid(.x)[[1]]),
         lat=map_dbl(geom, ~st_centroid(.x)[[2]])) %>%
  # for just indiv version
  #rename(lon = longitude_dd, lat = latitude_dd) %>%
  select(huc12, lat, lon) %>%
  distinct(.keep_all = TRUE) %>%
  #nrow()
  st_drop_geometry() %>% as.data.frame()

mapview(st_as_sf(rb_bd_geo_h12_its, coords=c("lon", "lat"), crs=4326, remove=FALSE), layer.name="Points")

# write out
write.geo(rb_bd_geo_h12_its, location = "output/satscan/", file = glue("rb_bd_norm_h12_{datatype}_its"), userownames=FALSE)

# make layer based on indiv for normal/poisson model
rb_bd_geo_ind <-
  rb_covar %>%
  filter(field_or_museum==str_to_title(datatype)) %>%
  filter(!is.na(bd_its_copies)) %>%
  # for just indiv version
  rename(lon = longitude_dd, lat = latitude_dd) %>%
  select(huc12, sampleid2, lat, lon) %>%
  distinct(.keep_all = TRUE) %>%
  #nrow()
  as.data.frame()

# preview unique sites:
mapview(st_as_sf(rb_bd_geo_ind, coords=c("lon", "lat"), crs=4326, remove=FALSE), layer.name="Points")

# write out
write.geo(rb_bd_geo_ind, location = "output/satscan/", file = glue("rb_bd_norm_ind_{datatype}_its"), userownames=FALSE)


## .cas file details ------------------------

# not used for continuous Poisson

# Location ID: Any numerical value or string of characters. Empty spaces may not form part of the id.
# Number of Cases: The number of cases for the specified location, time and covariates. For the discrete Poisson, binomial and space-time permutation models, this is the number of observations or individuals with the characteristic of interest, such as cancer or low birth weight. For the ordinal, multinomial, normal and exponential models, it is the total number of observations or individuals in the locations, irrespectively of the value of their categorical characteristic or continuous attribute value.
# Date/Time: Optional. May be specified either in years, months or days, or in a generic format. The format must coincide with the time precision format specified on the Input Tab. Unless the temporal data check is disabled, all case times must fall within the study period as specified on the Input Tab.
# Attribute: For the multinomial, ordinal, exponential and normal models only. A variable describing some characteristic of the case. These may be a category (multinomial or ordinal model), survival time (exponential model), or a continuous variable value (normal model). The categories for the multinomial models can be as any alphanumeric value. The categories for the ordinal model must be specified as a positive or negative numerical value. Survival times must be positive numbers. The numbers for the normal model can be positive or negative.
# Censored: For the exponential model only. Censored is a 0/1 variable with censored=1 and uncensored=0.
# Weight: Optional. For the normal model only. Required if covariates are used, even if all observations have the same variance, in which case all weights should be set to one.
# Covariates: Optional. For discrete Poisson, space-time permutation and normal models only. Any number of categorical covariates may be specified as either numbers or through characters. For the normal model, covariates can only be included if weights are also provided.

## Indiv *.cas file --------------------------------------------------------

# expected format for .cas file:
# (for Normal): total number of obs/indivs in locations, irrespective of the value of  continuous attribute.
# loc ID / Number of Cases / Attribute (continuous ITS load)

# make a case layer for normal model
# (total obs regardless of +/-)
rb_bd_cas_ind <-
  rb_covar %>% st_drop_geometry() %>%
  filter(field_or_museum==str_to_title(datatype)) %>%
  #filter(!is.na(bd_its_copies)) %>%
  filter(bd_its_copies > 0) %>%
  mutate(bd_its_copies_log = log(bd_its_copies)) %>%
  group_by(huc12) %>% add_count() %>% # for h12
  #group_by(huc12, sampleid2) %>% add_count() %>% # for ind
  ungroup() %>%
  # need locID, number of obs at locID, attrib
  select(huc12, sampleid2, n, bd_its_copies_log) %>%
  distinct(.keep_all = TRUE) %>%
  as.data.frame()

# write out
write.cas(rb_bd_cas_ind, location = "output/satscan/", file = glue("rb_bd_norm_h12_{datatype}_its"), userownames=FALSE)

write.cas(rb_bd_cas_ind, location = "output/satscan/", file = glue("rb_bd_norm_ind_{datatype}_its"), userownames=FALSE)

## Indiv *.pop file ------------------------------------------------------

# for discrete Poisson only

# for Poisson we need a population file with the following:
# <loc_id> numeric or character, no empty spaces allowed
# <time> time to which the pop refers, can be Y, M, or D, and can be a dummy year if no time is known/needed
# <population> population size for a particular location, year, and covariate combo. If zero, should be included. Can use decimal to indicate pop at risk rather than actual number
# <covariate> optional, any number of numeric or character covariates, must also have same covariates in case file
#
# # make a pop file
# rb_bd_pop_ind <-
#   rb_covar %>%
#   filter(field_or_museum==str_to_title(datatype)) %>%
#   filter(!is.na(bd_its_copies)) %>%
#   group_by(sampleid2) %>% add_count() %>%
#   ungroup() %>%
#   # add dummy variable for time
#   mutate(poistime = 2000) %>%
#   distinct(.keep_all = TRUE) %>%
#   select(sampleid2, poistime, n) %>%
#   distinct(.keep_all = TRUE) %>%
#   #nrow()
#   as.data.frame()
#
# # write out
# write.pop(rb_bd_pop_ind, location = "output/satscan/", file = glue("rb_bd_ind_{datatype}_h12_its"), userownames=FALSE)

# Tutorial Info -----------------------------------------------------------

# http://cran.uni-muenster.de/web/packages/rsatscan/vignettes/simulation.html
# https://vimeo.com/123859199
# https://knowledgerepository.syndromicsurveillance.org/satscan-tutorial

# sensitivity analysis?
# GLM: hi/lo rate + non-sig clusters and run against sample sizes
# is there correlation between sample size vs significance/relative risk?
# box plot of samples sizes in different cluster (hi/low/n.s.)
