# recap

# sort by only with recap ID, then if it's a recap (Y), sort by date
# then look at trend...does it switch between?

# decreasing/increasing load
# +1/0

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)

# raw data with recap cols
df_field <- read_csv("data/rabo_indiv_field_only_downloaded_2021-11-10.csv") %>%
  select(id=1, date=date_captured, loc=locality,
         recap_y_n, recap_id = identifier_if_recap) %>%
  filter(!recap_id %in% c("999","NULL","see notes"))
table(df_field$recap_id, useNA = "ifany") # n=402

# cleaned data
rb_df <- read_rds("output/09_rb_bd_all_covariates_joined.rds") %>%
  filter(field_or_museum == "Field") %>%
  select(sampleid:capt_mon, elevation)

# Filter ------------------------------------------------------------------

df_out <- left_join(df_field, rb_df, by=c("id" = "sampleid"))

# Arrange
df_final <- df_out %>% group_by(date_captured, recap_id) %>%
  select(id, date_captured, recap_y_n, recap_id, method=method_pcr_q_pcr_histology, bd_positive:simplified_locality, loc, boylii_clade:weight_g, elevation) %>%
  arrange(recap_id) %>%
  ungroup() %>%
  # drop 777 and 888s, and obs w dual obs in same day and switch sexes
  filter(!recap_id %in% c(777, 888, 106, 900067000061897, 900067000073656)) %>%
  # tally singletons and filter out
  group_by(recap_id, date_captured) %>%
  add_tally() %>% relocate(n, .after="recap_id")
  #filter(n>1)

# randomly sample one per year from recaps, rebind with original field dataset,
# (sans the recaps), rerun satscan/models for field data


# Look at Lag Changes -----------------------------------------------------

df_final <- df_final %>% group_by(recap_id) %>%
  arrange(recap_id, date_captured) %>%
  mutate(lag_pos = bd_positive - lag(bd_positive)) %>%
  select(id:recap_id, bd_positive, bd_its_copies, n, lag_pos)

table(df_final$lag_pos, useNA = "ifany")

# plot

# drop zeros
df_final %>% filter(!lag_pos==0, !is.na(lag_pos)) %>%
  ggplot(.) +
  geom_bar(aes(x=recap_id, y=lag_pos,
               fill=as.factor(lag_pos)),
           show.legend=FALSE, stat="identity") +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(subtitle = "Negative is moving from Bd+ to Bd-, Positive is Bd- to Bd+", x="Bd direction", y="")

ggsave(filename = "figs/bd_recap_summary.png", width = 11, height = 8, dpi=300)

write_csv(df_final, file = "output/14_bd_recap_summary.csv")
