# get elevation data for points and snap points to streamline and pull elevation

# Packages ----------------------------------------------------------------

library(tidyverse)
library(glue) # pasting things together
library(elevatr)
library(sf)
library(mapview)
mapviewOptions(fgb = FALSE)

# Load Data ----------------------------------------------------------------

# this is localities only: bd_sf_loc, n=735
load("output/02_rb_bd_localities_only.rda")

# all sites
load("output/02_rb_bd_spatial.rda") # rb_bd_sf, n=2064 obs and 27 vars

# get flowline data
gpkg_file <- "output/rabo_bd_spatial_data.gpkg"

# read individual stream layers
tribs_us <- st_read(dsn = gpkg_file, layer = "tribs_us")
mainstems_us <- st_read(dsn = gpkg_file, layer = "mainstems_us")

# Snap Points to Nearest Streamline ---------------------------------------

# Custom function to snap points to lines using 1000 m buffer
# (adapted from SO and Tim Salabim: https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf )
st_snap_points <- function(x, y, namevar, max_dist = 1000) {

  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)

  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  out_xy <- st_coordinates(out) %>% as.data.frame()
  out_xy <- out_xy %>%
    mutate({{namevar}} := x[[namevar]]) %>%
    st_as_sf(coords=c("X","Y"), crs=st_crs(x), remove=FALSE)

  return(out_xy)
}

## Transform to UTMs (more accurate)
rb_bd_sf <- rb_bd_sf %>% st_transform(3310)
tribs_us <- tribs_us %>% st_transform(3310)

# now snap points to the flowlines using 500 m buffer
pts_snappedA <- st_snap_points(rb_bd_sf[1:500,], tribs_us, namevar = "sampleid", max_dist = 500)
pts_snappedB <- st_snap_points(rb_bd_sf[501:nrow(rb_bd_sf),], tribs_us, namevar = "sampleid", max_dist = 500)
beepr::beep(2)

pts_snapped <- rbind(pts_snappedA, pts_snappedB)

# save the adjusted pts
write_rds(pts_snapped, file="output/05_bd_sf_pts_snapped_to_flowline.rds")

# check!
mapview(pts_snapped, cex=5, col.regions="maroon") +
  mapview(bd_sf_loc, col.regions="yellow", cex=2) +
  #mapview(tribs_us, color="cyan4", lwd=0.7)
  mapview(mainstems_us, color="steelblue", lwd=2)
# save as "mapview_w_sites_adj_trib_flowlines

# Get Elevation for Each Point: ADJUSTED (snapped to streamline) ---------------

# adj points
bd_adj_elev <- elevatr::get_elev_point(pts_snapped)
save(bd_adj_elev, file="output/05_rb_bd_locs_elev_adj.rda")

# raw points
bd_raw_elev <- elevatr::get_elev_point(rb_bd_sf)
save(bd_raw_elev, file="output/05_rb_bd_locs_elev_raw.rda")

par(mfcol=c(2,1))
pdf(file = "figs/elevation_for_sites_adj_vs_raw.pdf", width = 10, height = 7)
hist(bd_raw_elev$elevation, breaks = 125, col="cyan4", main="Elevation for Sites (raw=green, adjusted=pink)", xlab="Elevation (meters)")
hist(bd_adj_elev$elevation, breaks = 125, col=alpha("maroon",0.6), main="", xlab="", add=T)
dev.off()
