
## NOAA Gridded Climate Data
# area-weighted averages of grid-point estimates interpolated from station data at 5km grid
# Global Historical Climatology Network (GHCN)  Daily dataset is the source of station data for nClimDiv
# https://www.ncei.noaa.gov/pub/data/cirs/climdiv/divisional-readme.txt
# https://www.ncdc.noaa.gov/cdo-web/datasets

# Pull Raw Climate Data ---------------------------------------------------

# pull data from batch scripts NOAA
# www.ncei.noaa.gov/pub/data/cirs/climdiv

# NOAA files come as flat text files, associated with state or climate divisions.

# see table in Google Doc here for more metadata on variables:
# https://docs.google.com/document/u/1/d/18xkYw_Un26Bh_KYAw4wkL9q46I43FnRXYuxPHthjCLQ/edit?usp=drive_web&ouid=115138845489099064749

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf) # spatial package
library(glue) # pasting things together
library(lubridate) # datetime
library(fs) # file system and pathnames
library(mapview)
mapviewOptions(fgb = FALSE)

# GET DATA ----------------------------------------------------------------

# get all data
load("output/02_rb_bd_spatial.rda")
rb_bd_sf$capt_mon <- month(rb_bd_sf$date_captured)
rb_bd_sf$capt_yr <- year(rb_bd_sf$date_captured)

table(rb_bd_sf$capt_mon) # all months present, maj in Jul-Aug
table(rb_bd_sf$capt_yr) # 83 years 1897-2021

# make a trimmed dataframe for getting climate vars only
rb_bd <- rb_bd_sf %>%
  select(sampleid, field_or_museum, date_captured, capt_yr, capt_mon, bd_its_copies, locality, simplified_locality, boylii_clade, bd_positive)

# Climate Divisions -------------------------------------------------------

# need to pull climate divisions for each Rb pt
# climdiv <- st_read("data/noaa/GIS.OFFICIAL_CLIM_DIVISIONS.shp")
# climdiv_simp <- rmapshaper::ms_simplify(climdiv, keep = .1)
# pryr::object_size(climdiv_simp)
# filter to OR_CA
# climdiv_ca_or <- climdiv_simp %>% filter(STATE %in% c("California", "Oregon"))

# save out
# save(climdiv_ca_or, file = "output/07b_climdiv_or_ca.rda")
# save(climdiv_simp, file="output/07b_climdiv_us.rda")

load("output/07_climdiv_or_ca.rda")
climdiv_ca_or <- climdiv_ca_or %>% st_transform(st_crs(rb_bd))

# now overlay/join to get the climate divisions for our frog pts
rb_bd <- st_join(rb_bd, climdiv_ca_or[,c(3:10)])

# quick check
mapview(rb_bd, zcol="FIPS_CD", burst=TRUE)

# one point didn't join because it's in the ocean: SPC103:106
# add to region 0606
rb_bd <- rb_bd %>%
  mutate(
    STATE_FIPS = case_when(
      is.na(STATE_FIPS) ~ "06",
      TRUE ~ STATE_FIPS),
    CD_2DIG = case_when(
      is.na(CD_2DIG) ~ "06",
      TRUE ~ CD_2DIG),
    STATE_CODE = case_when(
      is.na(STATE_CODE) ~ as.integer(4),
      TRUE ~ STATE_CODE),
    CLIMDIV = case_when(
      is.na(CLIMDIV) ~ as.integer(406),
      TRUE ~ CLIMDIV),
    CD_NEW = case_when(
      is.na(CD_NEW) ~ as.integer(6),
      TRUE ~ CD_NEW),
    FIPS_CD = case_when(
      is.na(FIPS_CD) ~ "0606",
      TRUE ~ FIPS_CD),
    NCDC_GEO_I = case_when(
      is.na(NCDC_GEO_I) ~ 308,
      TRUE ~ NCDC_GEO_I),
    NAME = case_when(
      is.na(NAME) ~ "SOUTH COAST DRNG.",
      TRUE ~ NAME))

mapview(rb_bd, zcol="FIPS_CD", burst=TRUE)

table(rb_bd$CLIMDIV, useNA = "ifany")

# now drop the geometry:
rb_bd_df <- rb_bd %>% st_drop_geometry()

# Extract NOAA: Function to Map-over --------------------------------------

(noaafiles <- fs::dir_ls(path = glue("data/noaa/"), regexp = "climdiv"))

# can use raw url
# df <- read_table(file="https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pcpndv-v1.0.0-20210805",skip = 0,
#                  skip_empty_rows = TRUE,
#                  col_names=c("id", "jan","feb","mar",
#                              "apr", "may", "jun", "jul", "aug",
#                              "sep", "oct", "nov", "dec"))

# write function for use in purrr
get_noaa <- function(path, varname, date, out){
  fileName <- glue("{path}-{varname}dv-v1.0.0-{date}")
  df <- read_table(file = fileName, col_names = c("id", "jan","feb","mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), skip=0, skip_empty_rows = TRUE)
  # filter to CA/OR
  df_trim <- df %>% filter(grepl("^04|^35", x = id)) %>%
    mutate(CLIMDIV=as.integer(substr(id, 1, 4)),
           metric=substr(id, 5, 6),
           year = as.numeric(substr(id, 7,10)), .after=id)
  # make long instead of wide
  df_trim <- pivot_longer(df_trim, cols=c(jan:dec), names_to = "month", values_to=as.character(glue("{varname}_noaa"))) %>%
    mutate(mon_num = as.numeric(match(tolower(month), tolower(month.abb))), .before=month)
  # now join to data across year/month
  df_join <- left_join(rb_bd_df, df_trim, by=c("capt_yr"="year", "capt_mon"="mon_num", "CLIMDIV"="CLIMDIV")) %>%
    select(sampleid, date_captured:capt_mon, id, CLIMDIV, NAME, glue("{varname}_noaa"))
  write_rds(df_trim, file = glue("output/noaa/{out}_{varname}.rds"))
  return(df_join)
  #write_rds(df_join, file = glue("output/07b_rb_bd_{out}_{varname}.rds"))
}

# get vars (warnings ok!)
pcp <- get_noaa(path = "data/noaa/climdiv", varname = "pcpn",
                date="20210805", out = "noaa_climdiv")
pdsi <- get_noaa(path = "data/noaa/climdiv", "pdsi",
                date="20210805", "noaa_climdiv")
phdi <- get_noaa(path = "data/noaa/climdiv", "phdi",
                 date="20210805", "noaa_climdiv")
pmdi <- get_noaa(path = "data/noaa/climdiv", "pmdi",
                 date="20210805", "noaa_climdiv")
tmax <- get_noaa(path = "data/noaa/climdiv", "tmax",
                 date="20210805", "noaa_climdiv")
tmin <- get_noaa(path = "data/noaa/climdiv", "tmin",
                 date="20210805", "noaa_climdiv")
tmpc <- get_noaa(path = "data/noaa/climdiv", "tmpc",
                 date="20210805", "noaa_climdiv")

summary(pcp) # no nas!
summary(pdsi) # no nas!

# bind together:
rb_bd_noaa <- coalesce(pcp, pdsi, phdi, pmdi, tmax, tmin, tmpc) # slick!!

#SAVE
save(rb_bd_noaa, file = "output/07_rb_bd_noaa_vars.rda")

# TESTING CODE: ----------------------
## Extract NOAA Data: Single File TEST -------------------------------------------

# # extract to regional divisions:
# pdsi <- read_delim(delim = " ", file = "data/noaa/climdiv-pdsidv-v1.0.0-20210204", col_names = c("id", "jan","feb","mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), trim_ws = TRUE)
#
# # filter to CA divisions: STATE/DIVISION/METRIC:
# # CA=04, DIV=XX, ELEMENT CODE=YY (PDSI, TMIN, ETC), YEAR=YYYY)
# pdsi_ca <- pdsi %>% filter(grepl("^04|^35", x = id)) %>%
#   mutate(CLIMDIV=as.integer(substr(id, 1, 4)),
#          metric=substr(id, 5, 6),
#          year = as.numeric(substr(id, 7,10)), .after=id)
#
# # 127 yrs, how many divisions in OR and WA? (n=16)
# pdsi_ca %>% group_by(CLIMDIV) %>% tally()
# # CA: 7
# # OR: 9
#
# # unique metrics? (should be one)
# unique(pdsi_ca$metric)
#
# # pivot
# pdsi_ca_long <- pdsi_ca %>% pivot_longer(cols=c(jan:dec), names_to = "month", values_to="var") %>%
#   mutate(mon_num = as.numeric(match(tolower(month), tolower(month.abb))), .before=month)
#
# df_join <- left_join(rb_bd, pdsi_ca_long, by=c("capt_yr"="year", "capt_mon"="mon_num", "CLIMDIV"="CLIMDIV")) #%>%
#   distinct(sampleid, .keep_all = TRUE) # should still be 1991!
