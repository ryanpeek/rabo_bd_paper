# Summarize data for status reports

# Load Libraries ----------------------------------------------------------

library(tidyverse)
options(dplyr.print_max = 300)
library(fs)
library(sf)
library(glue)
library(janitor)
library(mapview)
mapviewOptions(fgb = FALSE)

# Load Data ---------------------------------------------------------------

rb_sf <- read_rds("output/09_rb_bd_all_covariates_joined.rds") %>%
  st_as_sf(coords=c("longitude_dd", "latitude_dd"), crs=4326, remove=FALSE)

mapview(rb_sf, zcol="bd_positive")

# raw data with recap cols
df_field <- read_csv("data/rabo_indiv_field_only_downloaded_2021-11-10.csv") %>%
  select(id=1, date=date_captured, loc=locality,
         recap_y_n, recap_id = identifier_if_recap) %>%
  filter(!recap_id %in% c("999","NULL","see notes"))
# table(df_field$recap_id, useNA = "ifany") # n=402

# cleaned data
rb_df <- read_rds("output/09_rb_bd_all_covariates_joined.rds") %>%
  filter(field_or_museum == "Field") %>%
  select(sampleid:date_captured, bd_positive, locality,
         simplified_locality, boylii_clade:life_stage_a_j_m_t,
         capt_yr, capt_mon)

# Join ----------

df_out <- left_join(df_field, rb_df, by=c("id" = "sampleid"))

names(df_out)

rm(rb_df)

# Filter Data -------------------------------------------------------------

# Drop recaps (select one random sample per calendar year)
# **Exclude 777, 888, 106, 900067000073656, 900067000061897
# (errors, fishy cases where multiple captures in a day and sex switches)
df_out %>% filter(!recap_id %in% c(777, 888, 106, 900067000061897, 900067000073656)) %>% filter(!is.na(capt_yr)) %>% count(recap_id) %>% View()

# 86 ids with more than one
df_out %>% filter(!recap_id %in% c(777, 888, 106, 900067000061897, 900067000073656)) %>% filter(!is.na(capt_yr)) %>% count(recap_id) %>% filter(n>1) %>% tally() # 86 ids with

df_out2 <- df_out %>% group_by(date_captured, recap_id) %>%
  arrange(recap_id) %>%
  ungroup() %>%
  # drop 777 and 888s, and obs w dual obs in same day and switch sexes
  filter(!recap_id %in% c(777, 888, 106, 900067000061897, 900067000073656)) %>%
  # tally singletons and filter out
  group_by(recap_id, capt_yr) %>%
  #tally() %>% View() # n=330 unique year + recap ids
  add_tally(name = "n") %>% relocate(n, .after="recap_id") %>%
  slice_sample() %>%
  add_tally(name = "fintally") %>% relocate(fintally, .after="n")

# double check
df_out2 %>%
  group_by(capt_yr, recap_id) %>% tally() %>% View()

# hone in on just the id and date captured to make unique
df_out_fin <- df_out2 %>%
  select(id, date, loc, recap_y_n, recap_id, date_captured, bd_positive, locality, capt_yr) %>% ungroup()

df_out_fin %>% ungroup() %>%  distinct() %>% tally()


# Re-add to Full Dataset ---------------------------------------------------

# total fld vs. mus
table(rb_sf$field_or_museum, useNA = "ifany")
# Field Museum
# 1684    461

rb_sf_mus <- rb_sf %>%
  # filter out all recaps and then rebind dataset
  filter(field_or_museum != "Field")

rb_sf_fld_recap_keep <- rb_sf %>%
  filter(field_or_museum == "Field") %>%
  filter(sampleid %in% c(unique(df_out_fin$id))) %>%
  filter(!is.na(bd_positive))
# drops one recap that doesn't have Bd info from Cresta (5708).
# anti_join(df_out_fin, rb_sf_fld_no_recap, by = c("id"="sampleid")) %>% View()

# now get all the other stuff to keep
rb_sf_fld_nonrecap_keep <- rb_sf %>%
  filter(field_or_museum == "Field") %>%
  filter(!sampleid %in% c(unique(df_field$id)))

# total?
nrow(rb_sf_fld_nonrecap_keep) + nrow(rb_sf_fld_recap_keep)# n=1612
 # All field samples: 1684 - 1612 # so we've dropped 72 samples

# bind together
rb_out_final <- bind_rows(rb_sf_mus, rb_sf_fld_nonrecap_keep, rb_sf_fld_recap_keep)


# Write Out --------------------------------------------------------------

write_rds(rb_out_final, "output/10a_rb_bd_all_covars_recap_filt.rds")
rb_out_final %>% st_drop_geometry %>% write_csv(., file = "output/10a_rb_bd_all_covars_recap_filt.csv")
