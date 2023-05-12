# Read Data from googlesheets

library(tidyverse)
library(googlesheets4)
library(purrr)
library(glue)
library(here)
library(fs)
library(janitor)

# Authorize Googlesheets --------------------------------------------------

# auth # make sure .secrets is in gitignore
options(gargle_oauth_cache = here::here(".secrets"))

# auth for sheets for readonly
gs4_auth(email = "rapeek@ucdavis.edu", cache = here::here(".secrets"),
         scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")

# Import GoogleSheet Field & Museum Data ----------------------------------

# data: 1B3DMnK_w8X8muTwBR7GsKc33sG9DRjR_8k7R7RrH__w
# only want field and museum tabs
gs_id <- "1B3DMnK_w8X8muTwBR7GsKc33sG9DRjR_8k7R7RrH__w"
wb_id <- gs4_get(gs_id)

(wb_names <- googlesheets4::sheet_names(wb_id)) # list sheets in workbook

# open sheet in browser:
# gs4_browse(wb_id)

# Get Field (n=1686)
wb_field <- read_sheet(wb_id, sheet = "field only boylii indiv data") %>% clean_names()

# Get Museum (n=461)
wb_museum <- read_sheet(wb_id, sheet = "museum  only boylii indiv data") %>% clean_names()

# Tidy Data ---------------------------------------------------------------

# need to fix the weird "list" issue that is occurring in some columns (likely b/c of mixed data types)
glimpse(wb_field)
glimpse(wb_museum)

# this code looks for all "list" cols and converts to character
wb_field_df <- wb_field %>%
  mutate(date_captured=as.character(date_captured)) %>%
  mutate(across(where(is.list), as.character))

wb_museum_df <- wb_museum %>%
  mutate(date_captured=as.character(date_captured)) %>%
  mutate(across(where(is.list), as.character))

glimpse(wb_field_df)
glimpse(wb_museum_df)

# double check for duplicate IDs, TRUE if all unique
length(unique(wb_field_df$id_code_in_source_dataset)) == nrow(wb_field_df)
length(unique(wb_museum_df$id_code_in_source_dataset)) == nrow(wb_museum_df)

# find duplicates (if necessary!)
wb_field_df[duplicated(wb_field_df$id_code_in_source_dataset),]

# Save Out Googlesheets Data ----------------------------------------------

write_csv(wb_field_df, glue("data/rabo_indiv_field_only_downloaded_{Sys.Date()}.csv"))
write_csv(wb_museum_df, glue("data/rabo_indiv_museum_only_downloaded_{Sys.Date()}.csv"))

