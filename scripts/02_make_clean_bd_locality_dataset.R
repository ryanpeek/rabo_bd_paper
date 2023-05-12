# Clean and Join Data: Make Locality Table

# Libraries ---------------------------------------------------------------

library(sf) # spatial package
library(tidyverse) # wrangling/plotting
library(glue) # pasting things together
library(fs) # working with files & folders
library(lubridate) # work with dates/times
library(janitor) # cleans col names
library(mapview) # interactive mapping
mapviewOptions(fgb = FALSE) # set to FALSE to avoid bug

# Import Raw Data ---------------------------------------------------------

# get paths for files
(field_file <- fs::dir_ls("data", type="file", regexp = "*field*"))
(museum_file <- fs::dir_ls("data", type="file", glob = "*museum*"))

# show most recent file
sort(field_file, decreasing = TRUE)[1]

# read in data
field_df <- read_csv(sort(field_file, decreasing = TRUE)[1],
                     show_col_types = FALSE)
museum_df <- read_csv(sort(museum_file, decreasing = TRUE)[1],
                      show_col_types = FALSE)

# Data Cleaning -----------------------------------------------------------

# check names are same?
compare_df_cols_same(field_df, museum_df)

# find column names that are diff
setdiff(names(field_df), names(museum_df))
setdiff(names(museum_df), names(field_df))

# looks like these cols are missing/diff:
## bd_its_copies,
## regulation_0_unreg_1_upstream_dam_2_downstream_dam
## life_stage_a_j_t
## life_stage_a_j_m_t

# fix names
museum_df <- museum_df %>%
  rename(regulation_0_unreg_1_upstream_dam_2_downstream_dam=regulation_0_none_1_dam_upstream_or_2_dam_or_reservoir_downstream)

# rename
field_df <- field_df %>%
  rename(life_stage_a_j_m_t=life_stage_a_j_t)

# see how many sites have positive load or ITS
# field_df %>% filter(!bd_load_ze_if_q_pcr == "0") %>% group_by(simplified_locality, boylii_clade) %>%  tally() %>% View(title = "ze")
# field_df %>% filter(!bd_its_copies == "0", !bd_its_copies=="NULL") %>% group_by(simplified_locality, boylii_clade) %>%  tally() %>% View(title = "its")

## Deal with NA vs NULL in Bd Positive -------------------------------------

# check Bd fields?
table(field_df$bd_positive_blank_or_1)

# For Bd_positive (1/0): replace na with NA and NULL with 0's
# create one single "bd_positive" column
field_df <- field_df %>%
  mutate(bd_positive = case_when(
    bd_positive_blank_or_1=="NULL" ~ "0",
    bd_positive_blank_or_1=="na" ~ "NA",
    TRUE ~ bd_positive_blank_or_1), .before=bd_positive_blank_or_1,
    bd_positive = as.integer(bd_positive)
  ) # warning is ok, just filling NAs

table(field_df$bd_positive, useNA = "always") # plus one NA (drop ID 5708 bc missing swab)
unique(field_df$bd_positive)

field_df <- field_df %>%
  # drop the record that is NA
  filter(!is.na(bd_positive)) %>% # n=1685 now
  # drop ID record that is NA (jamie betasso's NA)
  filter(!is.na(id_code_in_source_dataset)) # n=1684

# fix museum data in same way
unique(museum_df$bd_positive_blank_or_1)
museum_df <- museum_df %>%
  mutate(bd_positive = case_when(
    is.na(bd_positive_blank_or_1) ~ 0,
    TRUE ~ bd_positive_blank_or_1), .before=bd_positive_blank_or_1)
 # warning is ok

table(museum_df$bd_positive, useNA = "ifany")
unique(museum_df$bd_positive) # no NAs

# Error Check ZE and ITS Columns ------------------------------------------

# adjust ZE for known BD ITS strains (Ale's lab samples)
field_df_rev <- field_df %>%
  mutate(bd_load_ze_if_q_pcr = case_when(
    grepl("+", bd_load_ze_if_q_pcr, fixed=TRUE) ~ "1.11", # replace + for now
    TRUE ~ bd_load_ze_if_q_pcr),
    bd_load_ze_if_q_pcr = as.numeric(bd_load_ze_if_q_pcr)
  )

# now need to update ITS for brazil standard
field_df_rev <- field_df_rev %>%
  mutate(bd_its_copies = as.numeric(bd_its_copies)) %>%
  mutate(bd_its_copies = case_when(
    method_pcr_q_pcr_histology == "qPCR" &
      standard_strain_if_q_pcr == "Brazil" ~ bd_load_ze_if_q_pcr * 60,
    TRUE ~ bd_its_copies))

# now fix/cross ref across columns to make sure bd_positive = 1 if loads or ITS exist
field_df_rev <- field_df_rev %>%
  mutate(bd_positive = case_when(
      #bd_load_ze_if_q_pcr < 1  ~ as.integer(0),
      bd_load_ze_if_q_pcr < 1 & bd_load_ze_if_q_pcr >0 & bd_its_copies > 0 ~ as.integer(1),
      bd_load_ze_if_q_pcr >= 1 | bd_its_copies > 0 ~ as.integer(1),
      TRUE ~ bd_positive)
  )

# Double check values that are ZE < 1 but > 0, did they get dropped or are ITS values >0?
# field_df_rev %>%
#   filter(bd_load_ze_if_q_pcr < 1 &
#            bd_load_ze_if_q_pcr >0 &
#            bd_its_copies > 0) %>%
#   View()

# ok these check out

# Create LAT-LONs from UTMS -----------------------------------------------

summary(field_df_rev$latitude_dd)
summary(field_df_rev$longitude_dd)

# filter to just NA samples from Lowe 2009 (in utms not lat/lon)
field_lowe <- field_df_rev %>%
   filter(contact_person_or_citation=="Lowe 2009 Herp review") %>%
  # convert to integer
  mutate(across(.cols = starts_with("utm_"), as.integer)) # n=49

summary(field_lowe)

# now make sf and convert to UTMs to lat lons
field_lowe_sf <- field_lowe %>%
  st_as_sf(coords=c("utm_easting", "utm_northing"),
           remove=FALSE, crs=26910) %>%
  st_transform(4326) %>% # transform to lat lon
  mutate(longitude_dd=map_dbl(geometry, ~st_coordinates(.x)[,1]),
         latitude_dd=map_dbl(geometry, ~st_coordinates(.x)[,2])) %>% st_drop_geometry()

# now join back to df
field_df_rev2 <- field_df_rev %>%
  select(-utm_easting, -utm_northing) %>%
  filter(!contact_person_or_citation=="Lowe 2009 Herp review") %>%
  bind_rows(., field_lowe_sf)

## Reduce Columns for Spatial Dataframe: ------------------------------------

## FIELD: keep contact_person_or_citation, and source_of_data_literature_dataset
field_sf <- field_df_rev2 %>%
  select(-c(entered_by, month_captured, bd_positive_blank_or_1, bd_negative_blank_or_1, identifier_if_recap, recap_y_n, notes_on_methods, notes, month_number, palmer_z_index:pdsi, ct:monthly_average_precip, utm_easting, utm_northing))

## MUSEUM
museum_sf <- museum_df %>%
  select(-c(entered_by, month_captured, bd_positive_blank_or_1, bd_negative_blank_or_1, identifier_if_recap, recap_y_n, notes_on_methods, notes, month_number, palmer_z_index:pdsi, ct:monthly_average_precip, utm_easting, utm_northing))

# compare names:
janitor::compare_df_cols_same(field_sf, museum_sf)
janitor::compare_df_cols(field_sf, museum_sf)

# Fix Duplicate IDs -------------------------------------------------------

# change sample ID code field name
field_sf <- field_sf %>%
  rename(sampleid=id_code_in_source_dataset)

# check for NAs
field_sf %>% filter(is.na(sampleid)) %>% tally()

# now look for duplicates
length(which(duplicated(field_sf$sampleid)))

# look at museum records now
museum_sf <- museum_sf %>%
  rename(sampleid=id_code_in_source_dataset)

# no dups!
which(duplicated(museum_sf$sampleid))

# FIELD: Make Data Spatial ------------------------------------------------------------------

# now make spatial
summary(field_sf)
unique(field_sf$bd_positive)
summary(field_sf$latitude_dd)
summary(field_sf$longitude_dd)

# make spatial
field_sf <- field_sf %>%
  st_as_sf(coords=c("longitude_dd", "latitude_dd"), crs=4326, remove=FALSE)

# make a map!!
mapview(field_sf, zcol="bd_positive", burst=TRUE,
        col.regions=colorspace::divergingx_hcl(2, palette = "Geyser"))

# pick a palette: colorspace::choose_palette()

# MUSEUM: Make Spatial -------------------------------------------

summary(museum_sf)

# use sf package to make spatial
museum_sf <- museum_sf %>%
  st_as_sf(coords=c("longitude_dd", "latitude_dd"), crs=4326, remove=FALSE)

# make a map!!
mapview(field_sf, zcol="bd_positive", layer.name="Bd+ Field",
        col.regions=colorspace::divergingx_hcl(2, palette = "Geyser")) +
  mapview(museum_sf, zcol="bd_positive", layer.name="Bd+ Museum",burst=TRUE,
          col.regions=colorspace::divergingx_hcl(2, palette = "Earth", rev = TRUE))


# Find Distinct Localities and Join ---------------------------------------

# check cols are same and fix if not
compare_df_cols_same(field_sf, museum_sf)

# fix columns
museum_sf <- museum_sf %>%
  mutate(regulation_0_unreg_1_upstream_dam_2_downstream_dam = as.numeric(regulation_0_unreg_1_upstream_dam_2_downstream_dam),
         bd_positive = as.integer(bd_positive),
         bd_load_ze_if_q_pcr = as.numeric(bd_load_ze_if_q_pcr))
# warnings ok here, just telling us its filling NAs where no value available

# make weight_g numeric
field_sf <- field_sf %>%
  mutate(weight_g = as.numeric(weight_g))

# check again
compare_df_cols_same(field_sf, museum_sf) # TRUE!

# make distinct localities
field_sf_loc <- field_sf %>% # n=569
  distinct(locality, latitude_dd, longitude_dd, .keep_all = TRUE) %>%
  select(sampleid, field_or_museum, date_captured, locality, simplified_locality, latitude_dd, longitude_dd)

museum_sf_loc <- museum_sf %>% # n=229
  distinct(locality, latitude_dd, longitude_dd, .keep_all = TRUE) %>%
  select(sampleid, field_or_museum, date_captured, locality, simplified_locality, latitude_dd, longitude_dd)

# merge into one object (n=798)
bd_sf_loc <- bind_rows(field_sf_loc, museum_sf_loc)

# add year and month (need lubridate package)
bd_sf_loc$year <- as.factor(year(bd_sf_loc$date_captured))

# Bind and Look at Data Distribution --------------------------------------

# look at distribution of years
hist(lubridate::year(field_sf$date_captured)) # all field samples
hist(lubridate::year(museum_sf$date_captured))

# make spatial full dataset (joined) (n=2145)
rb_bd_sf <- bind_rows(field_sf, museum_sf)
hist(lubridate::year(rb_bd_sf$date_captured))

# Make Maps of Localities & Samples --------------------------------------------

dim(bd_sf_loc) # 798 unique locations

# locality only
m1 <- mapview(bd_sf_loc, zcol="field_or_museum", layer.name="Type", burst=TRUE)
m2 <- mapview(bd_sf_loc, zcol="year", layer.name="Year Sampled")

# all samples
m1 <- mapview(rb_bd_sf, zcol="field_or_museum", layer.name="Type", burst=TRUE)
m2 <- mapview(rb_bd_sf, zcol="bd_positive", layer.name="Bd+", burst=TRUE,
              col.regions=colorspace::divergingx_hcl(2, palette = "Geyser"))

# fancy map
library(leafsync)
leafsync::sync(m1, m2)

# Make Clade Labels -------------------------------------------------------

# make clade labels
clade_labels <- c("N. Coast","C. Coast", "S. Coast", "Feather", "N. Sierra", "S. Sierra")
levels <- c("N. Coast"="NNC", "C. Coast"="WC","S. Coast"="SW", "Feather"="NFR", "N. Sierra"="NNS", "S. Sierra"="ESS")
rb_bd_sf$boylii_clade <- forcats::fct_recode(rb_bd_sf$boylii_clade, !!!levels)
rb_bd_sf$boylii_clade <- forcats::fct_relevel(rb_bd_sf$boylii_clade, clade_labels)
levels(rb_bd_sf$boylii_clade)

# fix the missing clades
table(rb_bd_sf$boylii_clade, useNA = "ifany")
levels(rb_bd_sf$boylii_clade)

rb_bd_sf <- rb_bd_sf %>%
  mutate(boylii_clade = case_when(
    is.na(boylii_clade) ~ "C. Coast",
    TRUE ~ as.character(boylii_clade)))

rb_bd_sf<- rb_bd_sf %>%
  mutate(boylii_clade =
           factor(boylii_clade,
                  levels=c("N. Coast", "C. Coast",
                           "S. Coast","Feather",
                           "N. Sierra","S. Sierra")))

table(rb_bd_sf$boylii_clade, useNA = "ifany")
levels(rb_bd_sf$boylii_clade)

# Format Columns ----------------------------------------------------------

rb_bd_sf$bd_its_copies <- as.numeric(rb_bd_sf$bd_its_copies)
rb_bd_sf$bd_load_ze_if_q_pcr <- as.numeric(rb_bd_sf$bd_load_ze_if_q_pcr)
hist(log(rb_bd_sf$bd_its_copies))
hist(log(rb_bd_sf$bd_load_ze_if_q_pcr))
glimpse(rb_bd_sf)

# Save Out ----------------------------------------------------------------

# this is unique localities only: bd_sf_loc
length(unique(bd_sf_loc$locality)) # n=331
table(bd_sf_loc$field_or_museum, useNA = "ifany")
# Field=569
# Museum=229

# this is all data cleaned as sf data frame: rb_bd_sf
length(unique(rb_bd_sf$sampleid)) # n=2145
# unique records:
table(rb_bd_sf$field_or_museum, useNA = "ifany")
# Field=1684, Museum=461

save(rb_bd_sf, file = "output/02_rb_bd_spatial.rda") # read back in with "load"
write_csv(rb_bd_sf, file="output/02_rb_bd_spatial.csv")
save(bd_sf_loc, file="output/02_rb_bd_localities_only.rda")
write_csv(bd_sf_loc, file="output/02_rb_bd_localities_only.csv")

# Make a Map --------------------------------------------------------------

# by museum/field sample
m1 <- mapview(rb_bd_sf, zcol="field_or_museum", layer.name="RABO Bd Samples", burst=TRUE)
(m1 <- m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters"))
# save out as "output/maps/mapview_field_museum_bd_samples.html"

# by bd status
m2 <- mapview(rb_bd_sf, zcol="bd_positive", layer.name="RABO Bd+", burst=TRUE,
              col.regions=colorspace::divergingx_hcl(2, palette = "Earth", rev = TRUE))
(m2 <- m2@map %>% leaflet::addMeasure(primaryLengthUnit = "meters"))
# save out as "output/maps/mapview_bd_status.html"
