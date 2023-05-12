# get day length for each point

# Libraries ---------------------------------------------------------------

library(tidyverse) # wrangling/plotting
library(sf) # spatial package
library(geosphere) # to get daylength

# Load Data ---------------------------------------------------------------

load("output/02_rb_bd_spatial.rda")

# Get Daylength -----------------------------------------------------------

# check for missing dates?
rb_bd_sf %>% filter(is.na(date_captured))

# returns vector of daylength (in hours)
rb_bd_dl <- rb_bd_sf %>%
  mutate(daylen_hr=daylength(lat=latitude_dd, doy = date_captured), .after=bd_positive)

hist(rb_bd_dl$daylen_hr)

# Save out ----------------------------------------------------------------

save(rb_bd_dl, file = "output/08_rb_bd_spatial_daylength.rda") # read back in with "load"
write_csv(rb_bd_dl, file="output/08_rb_bd_spatial_daylength.csv")

# Make some Plots ---------------------------------------------------------

library(ggdark)
library(forcats) # for factors

# drop na
rb_bd_dl <- filter(rb_bd_dl, !is.na(bd_positive))

rb_bd_dl <- rb_bd_dl %>%
  mutate(boylii_clade = case_when(
    is.na(boylii_clade) ~ "C. Coast",
    TRUE ~ as.character(boylii_clade))) %>%
  mutate(boylii_clade = factor(boylii_clade,
                               levels=c("N. Coast",
                                        "C. Coast",
                                        "S. Coast",
                                        "Feather",
                                        "N. Sierra",
                                        "S. Sierra")),
         # add capt_yr
         capt_yr = lubridate::year(date_captured))
levels(rb_bd_dl$boylii_clade)
table(rb_bd_dl$boylii_clade, useNA="ifany")

rb_bd_dl %>% st_drop_geometry() %>% group_by(boylii_clade, field_or_museum) %>% tally()

# boylii_clade field_or_museum     n
# <fct>        <chr>           <int>
# 1 N. Coast     Field             569
# 2 N. Coast     Museum             98
# 3 C. Coast     Field             627
# 4 C. Coast     Museum             98
# 5 S. Coast     Field               4
# 6 S. Coast     Museum            166
# 7 Feather      Field             290
# 8 Feather      Museum              6
# 9 N. Sierra    Field              25
#10 N. Sierra    Museum             26
#11 S. Sierra    Field              14
#12 S. Sierra    Museum             67

# Plot by Clade and Bd Positive --------------------------------------------

# by clade and detection
ggplot() +
  geom_jitter(data=rb_bd_dl, aes(x=as.factor(bd_positive), y=daylen_hr), pch=16, color="gray60", alpha=0.8) +
  geom_boxplot(data=rb_bd_dl, aes(x=as.factor(bd_positive), y=daylen_hr, fill=boylii_clade), alpha=0.8, outlier.shape = NA, show.legend = TRUE) +
  facet_grid(.~boylii_clade) +
  ggdark::dark_theme_classic() +
  scale_fill_viridis_d("Clade") +
  labs(x="Bd Positive (1 = +, 0 = -)", y="Daylength (hrs): based on latitude and date") +
  facet_grid(field_or_museum~.)

#ggsave(filename = "figs/bd_daylength_by_clade_wpts.png", width = 8, height = 6, units = "in", dpi = 300)

# Plot by Load ------------------------------------------------------------

# first need to convert load to numeric and drop NAs
rb_bd_dl_its <- rb_bd_dl %>%
  filter(bd_positive==1) %>%
  mutate(bd_its=as.numeric(bd_load_ze_if_q_pcr)) %>%
  filter(!is.na(bd_its))
summary(rb_bd_dl_its)

ggplot() +
  geom_point(data=rb_bd_dl_its, aes(x=daylen_hr, y=bd_its, fill=boylii_clade), pch=21, color="gray50", alpha=0.8, size=3) +
  #facet_grid(.~boylii_clade) +
  ggdark::dark_theme_classic() +
  scale_fill_viridis_d("Clade") +
  scale_y_log10() +
  labs(x="Daylength (hrs): based on latitude and date", y="ITS copy")


# Plot by Year and Positive -----------------------------------------------

rb_bd_dl <- rb_bd_dl %>%
  mutate(yr_capt = lubridate::year(lubridate::ymd(date_captured)), .after=date_captured)

ggplot() +
  geom_point(data=rb_bd_dl, aes(x=yr_capt, y=latitude_dd, alpha=bd_positive, fill=boylii_clade), pch=21, color="gray50", size=3) +
  ggdark::dark_theme_classic() +
  ggthemes::scale_fill_colorblind("Clade") +
  labs(x="Year of Obs", y="latitude")

# Quick Logistic regression -----------------------------------------------

options(scipen=999)

# get training and test sets
sample <- sample(c(TRUE, FALSE), nrow(rb_bd_dl), replace=TRUE, prob=c(0.7,0.3))
train <- rb_bd_dl[sample, ]
test <- rb_bd_dl[!sample, ]

# run model
model <- glm(bd_positive~daylen_hr+boylii_clade, family="binomial", data=rb_bd_dl)
summary(model)

# r2
pscl::pR2(model)["McFadden"] # over 0.4 is a excellent, 0.2-0.4 good fit...so not great

# look at variable importance with caret
caret::varImp(model)
