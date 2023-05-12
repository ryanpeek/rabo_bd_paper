
# Pull Raw Climate Data: NOAA ---------------------------------------------

# www.ncei.noaa.gov/pub/data/cirs/climdiv

# Pull Raw Climate Data: WRCC ---------------------------------------------

# pull data from batch scripts downloadable from here:
## https://wrcc.dri.edu/wwdt/batchdownload.php

# For netcdf files, each file is approx 400-500 MB

# run in TERMINAL
# sh scripts/07a_wwdt_wget_1mon.sh
# data saves to data/netcdf

# Metadata ----------------------------------------------------------------

# see table in Google Doc here for more metadata on variables:
# https://docs.google.com/document/u/1/d/18xkYw_Un26Bh_KYAw4wkL9q46I43FnRXYuxPHthjCLQ/edit?usp=drive_web&ouid=115138845489099064749

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf) # spatial package
library(glue) # pasting things together
library(stars)
library(lubridate)
library(purrr)
library(fs)
library(mapview)
mapviewOptions(fgb = FALSE)

# GET DATA ----------------------------------------------------------------

# get all data
load("output/02_rb_bd_spatial.rda")
rb_bd_sf$month <- lubridate::month(rb_bd_sf$date_captured)
hist(rb_bd_sf$month)
sort(unique(rb_bd_sf$month)) # all months present, maj in Jul-Aug
sort(unique(lubridate::year(rb_bd_sf$date_captured))) # 85 years 1897-2020

# Read in NC files and Convert to RDS --------------------------------------------------------

# mdn = temp, pon = precip, spei, spi, pdsi
varname <- "spei12" # change this for each variable of interest

# file list, assumes files in data/netcdf/varname
(ncfiles <- fs::dir_ls(path = glue("data/netcdf/{varname}/"), regexp = "[.]*.nc"))

# write function for use in purrr
get_ncs <- function(x, extract_layer, ID, var, out){
  fileName <- gsub(pattern = ".nc", replacement="",  basename(x))
  ncs <- read_ncdf(x) # import netcdf
  # now extract values for points from the netcdf layers of time
  bd_ncs <- stars::st_extract(ncs, extract_layer) # can go straight to sf here
  bd_ncs$sampleid <- ID # add id
  xx <- as.data.frame(bd_ncs, xy = TRUE) %>% na.omit() %>% st_as_sf() %>%
    rename(date=day, !!var:=data)
  fs::dir_create(glue("output/wrcc/{var}"))
  write_rds(xx, file=glue("output/wrcc/{var}/{out}_{fileName}.rds"))
}

# this loops over and saves data out as an RDS file, one for every month (aggregated at monthly scale)
map(ncfiles, ~get_ncs(.x, extract_layer = rb_bd_sf, ID = rb_bd_sf$sampleid, var = varname, out = "spei12"))

## Test Read and Plot -----------------------------------------------------

# read in and test (this is the 12 month calc)
tst <- read_rds(glue("output/wrcc/{varname}/{varname}_{varname}_9_PRISM.rds"))
str(tst)
tst %>%
  filter(year(date)==2016) %>%
  mapview(zcol="spei12", layer.name="Sep") # quick mapview

# plot
(tst %>%
    filter(sampleid=="sk2019.06") %>% # alameda at Calaveras
    ggplot() +
    geom_line(aes(x=date, y=spei12), alpha=0.5, col="gray80") +
    geom_point(aes(x=date, y=spei12, fill=spei12), size=3.2, pch=21) +
    scale_fill_viridis_c() +
    stat_smooth(aes(x=date, y=spei12), method = "gam") +
    theme_classic() +
    scale_x_datetime(date_labels = "%Y", date_breaks = "6 years") +
    theme(axis.text.x = element_text(angle=280, hjust=-0.5)) -> gg1)

#plotly::ggplotly(gg1)


# Read in RDS and Coalesce ------------------------------------------------

varname <- "spei12"
selmon <- 12 # the range SPEI calc over

# file list use 12 months:
(rdsfiles <- fs::dir_ls(path = glue("output/wrcc/{varname}/"), regexp = glue("({varname}_{varname})_\\w")))

# get those files
all_spei <- map_df(rdsfiles, ~read_rds(.x)) %>%
  mutate(syear = year(date),
         smonth = month(date)) %>%
  rename("{varname}" := spei12) %>%  # change to 24 or 12
  st_drop_geometry()

summary(all_spei)

# now join with original dataset and save out
rb_bd_spei12 <- rb_bd_sf %>% st_drop_geometry() %>%
  select(sampleid, date_captured) %>%
  # add sample year and month columns
  mutate(syear = year(date_captured),
         smonth = month(date_captured)) %>%
  left_join(., all_spei, by=c("sampleid", "syear", "smonth")) %>%
  select(-c(date, syear, smonth))

summary(rb_bd_spei12)

# save out
write_rds(rb_bd_spei12, file = "output/07_rb_spei_12month.rds")
write_rds(rb_bd_spei24, file = "output/07_rb_spei_24month.rds")

# quick plot?
# rb_bd_spei %>%
#   ggplot() +
#   #geom_line(aes(x=bd_positive, y=spei), alpha=0.5, col="gray80") +
#   geom_boxplot(aes(x=as.factor(bd_positive), y=spei,
#                    group=bd_positive, fill=as.factor(bd_positive))) +
#   theme_classic()

# EVERYTHING BELOW HERE IS TEST CODE...NOT NEEDED -------------------------

## Read in single NC file with STARS ----------------------------------------
#
# ncfile <- "data/netcdf/ppt/pon1_1_PRISM.nc"
#
# spei_nc <- read_ncdf(ncfile)
# spei_nc
# names(spei_nc) # name of the variables in nc
# # set the variable name to the appropriate name
# spei_nc <- spei_nc %>% setNames(varname)
# # get a vector of the dates from time slice
# st_get_dimension_values(spei_nc, "day")
#
# # MATCH POINTS TO RASTER CRS
# #bd_sf_loc <- st_transform(bd_sf_loc, st_crs(spei_nc))
#
# # check crs
# st_crs(spei_nc)==st_crs(bd_sf_loc)
#
# # to extract we can use this:
# bd_spei8 <- stars::st_extract(spei_nc, bd_sf_loc)
# dim(bd_spei8) # check dims, should match nrow(bd_sf_loc)
#
# # convert to datafframe
# xx <- as.data.frame(bd_spei8, xy = TRUE) %>% na.omit() %>%
#   mutate(ppt=as.numeric(ppt)) %>%
#   rename(date=day)
#
# # join?
# bd_xx <- left_join(bd_sf_loc, xx)
#
## Aggregate by a field or SHP ----------------------------------------------
#
# # aggregated by something...could use ecoregions/watersheds/etc here
# # now aggregate to watershed
# a <- aggregate(bd_spei8, by = h8_crop, FUN = median)
# plot(a[,,109:126], max.plot = 24, border = 'grey', lwd = .5)
#
## Export as Points (Wide and Long) ----------------------------------------
#
# # export as points?
# dim(st_as_sf(bd_spei8, as_points = TRUE, merge = FALSE)) # this is wide
# dim(st_as_sf(bd_spei8, long = TRUE, as_points = TRUE, merge = FALSE)) # long
#
# # make long
# bd_spei_long <- st_as_sf(bd_spei8, long = TRUE, as_points = TRUE, merge = FALSE)
# # tidy and join with data
# bd_spei_long <- bd_spei_long %>%
#   rename(date=day, spei8=data)
# # join data
# bd_spei_long_sf <- st_join(bd_sf_loc, bd_spei_long)
# # tally
# bd_spei_long_sf %>% st_drop_geometry() %>%
#   group_by(id_code_in_source_dataset) %>% tally()
#
## Export Dataframes -------------------------------------------------------
#
# readr::write_rds(bd_spei_long_sf, file="output/07_bd_spei_sf_aug_1895-2020.rds")
#
#
## Extract to Raster using ncdf4 -------------------------------------------
#
# library(ncdf4)
# library(RNetCDF)
# library(raster)
# library(lubridate)
#
# # using ncdf4: open a connection
# (spei_output <- nc_open("data/spei8_8_PRISM.nc"))
#
# # list variables
# ncdf4.helpers::nc.get.variable.list(spei_output)
#
# # get variables
# lon <- ncvar_get(spei_output, varid = "longitude")
# lat <- ncvar_get(spei_output, varid = "latitude")
# days <- ncvar_get(spei_output, varid = "day")
# spei <- ncvar_get(spei_output, varid = "data")
# dim(spei)
# length(lon)
# length(lat)
#
# # see time units
# (timeunits <- spei_output$dim$day$units)
# # see calendar used
# spei_output$dim$day$calendar
#
# # convert days to date:
# spei_dates <- RNetCDF::utcal.nc(timeunits, days, type="c")
#
# # see what fill value is
# fillvalue <- ncatt_get(spei_output, "data", "_FillValue")
# fillvalue
#
# # close connection:
# nc_close(spei_output)
#
# # fill with NA
# spei[spei == fillvalue$value] <- NA
# dim(spei)
#
# # pick a date and slice
# yr2get <- 2018
# rowsel <- which(lubridate::year(spei_dates)==yr2get) # use 2014
# spei.slice <- spei[, , rowsel] # for a single date
#
# # extract as a raster
# library(raster)
# r <- raster(t(spei.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
#
# # need to transpose and flip to orient the data correctly.
# # Most netCDF files record spatial data from the bottom left corner
# r <- flip(r, direction='y')
#
# # plot!
# plot(r, main=glue("{yr2get}: SPEI"))
#
#
# image(lon,lat,spei.slice, col=c(rgb(1,1,1),brewer.pal(9,"Blues")) )
#
