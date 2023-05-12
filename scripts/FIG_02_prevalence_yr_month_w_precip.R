# FIG02 prevalance and precip

# Libraries ---------------------------------------------------------------

library(PropCIs)
library(ggplot2)
library(ggbreak)
library(dplyr)
library(tidyr)
library(grid)
library(readr)

# decade function
f_floor_decade <- function(x){ return(x - x %% 10) }

# colors
colblind_pal <- c('#009E73','#E69F00')

# Get Data ----------------------------------------------------------------

boylii_ind_data<-read_csv("output/10a_rb_bd_all_covars_recap_filt.csv") %>%
  rename(year = capt_yr)
str(boylii_ind_data)

# Field: Filter Data --------------------------------------------------------

# field only
field_samples <- filter(boylii_ind_data, field_or_museum == "Field") %>%
  rename(month=capt_mon)

# filter out tadpoles and NAs
field_samples_no_larvae<- filter(field_samples,
                                 life_stage_a_j_m_t != "T" |
                                   is.na(life_stage_a_j_m_t))

## Field: Calc prevalance ------------------------------

# add decade
field_samples_no_larvae <- field_samples_no_larvae %>%
  mutate(dec_int = f_floor_decade(year),
         .after="date_captured") %>%
  mutate(decade = case_when(
    dec_int == 2000 ~ "2005-2009",
    dec_int == 2010 ~ "2010-2019",
    dec_int == 2020 ~ "2020-2022"
  ), .after="date_captured")


# make prevalence df with CI
prev_field <- field_samples_no_larvae %>%
  group_by(year) %>% # summarize by year
  add_tally(name = "OverallSampleSize") %>%
  select(year, OverallSampleSize, bd_positive) %>%
  group_by(year, bd_positive) %>%
  add_tally(name="bd_pos_total") %>%
  distinct(year, .keep_all = TRUE) %>% #drop duplicate yrs
  ungroup() %>%
  # now fill and filter to keep unique totals for Bd_pos
  complete(year, bd_positive, fill = list(bd_pos_total=0)) %>%
  # fill totals
  group_by(year) %>% fill(OverallSampleSize, .direction = "downup") %>%
  # filter to just the pos
  filter(bd_positive == 1) %>%
  select(-bd_positive) %>%
  group_by(year) %>%
  # calc prevalence
  mutate(BdPrevalence = (bd_pos_total/OverallSampleSize)*100,
         LowerCI = exactci(bd_pos_total, OverallSampleSize, conf.level = 0.95)$conf.int[1]*100,
         UpperCI = exactci(bd_pos_total, OverallSampleSize, conf.level = 0.95)$conf.int[2]*100) %>%
  ungroup() %>%
  rename(Year=year)

# make prevalence df with CI by decade
prev_field_dec <- field_samples_no_larvae %>%
  group_by(decade) %>% # summarize by decade
  add_tally(name = "OverallSampleSize") %>%
  select(decade, OverallSampleSize, bd_positive) %>%
  group_by(decade, bd_positive) %>%
  add_tally(name="bd_pos_total") %>%
  distinct(decade, .keep_all = TRUE) %>% #drop duplicate yrs
  ungroup() %>%
  # now fill and filter to keep unique totals for Bd_pos
  complete(decade, bd_positive, fill = list(bd_pos_total=0)) %>%
  # fill totals
  group_by(decade) %>% fill(OverallSampleSize, .direction = "downup") %>%
  # filter to just the pos
  filter(bd_positive == 1) %>%
  select(-bd_positive) %>%
  group_by(decade) %>%
  # calc prevalence
  mutate(BdPrevalence = (bd_pos_total/OverallSampleSize)*100,
         LowerCI = exactci(bd_pos_total, OverallSampleSize, conf.level = 0.95)$conf.int[1]*100,
         UpperCI = exactci(bd_pos_total, OverallSampleSize, conf.level = 0.95)$conf.int[2]*100) %>%
  ungroup() %>%
  mutate(Type="Field")


# Museum: Filter Data -----------------------------------------------------

mus_samples <- filter(boylii_ind_data,
                               field_or_museum == "Museum")

# add decade and name
mus_samples <- mus_samples %>%
  mutate(dec_int = f_floor_decade(year), .after="date_captured") %>%
  mutate(decade = case_when(
    dec_int == 1890 ~ "1890-1899",
    dec_int == 1900 ~ "1900-1909",
    dec_int == 1910 ~ "1910-1919",
    dec_int == 1920 ~ "1920-1929",
    dec_int == 1930 ~ "1930-1939",
    dec_int == 1940 ~ "1940-1949",
    dec_int == 1950 ~ "1950-1959",
    dec_int == 1960 ~ "1960-1969",
    dec_int == 1970 ~ "1970-1979",
    dec_int == 1980 ~ "1980-1989",
    dec_int == 1990 ~ "1990-1999",
    dec_int == 2000 ~ "2000-2005"
  ), .after="date_captured")

## Museum: Calc prevalance ---------------------------------

# make a museum version (by decade)
prev_museum_dec <- mus_samples %>%
  group_by(decade) %>% # summarize by year
  add_tally(name = "OverallSampleSize") %>%
  select(decade, OverallSampleSize, bd_positive) %>%
  group_by(decade, bd_positive) %>%
  add_tally(name="bd_pos_total") %>%
  distinct(decade, .keep_all = TRUE) %>% #drop duplicate yrs
  ungroup() %>%
  # now fill and filter to keep unique totals for Bd_pos
  complete(decade, bd_positive, fill = list(bd_pos_total=0)) %>%
  # fill totals
  group_by(decade) %>% fill(OverallSampleSize, .direction = "downup") %>%
  # filter to just the pos
  filter(bd_positive == 1) %>%
  select(-bd_positive) %>%
  group_by(decade) %>%
  # calc prevalence
  mutate(BdPrevalence = (bd_pos_total/OverallSampleSize)*100,
         LowerCI = exactci(bd_pos_total, OverallSampleSize, conf.level = 0.95)$conf.int[1]*100,
         UpperCI = exactci(bd_pos_total, OverallSampleSize, conf.level = 0.95)$conf.int[2]*100) %>%
  ungroup() %>%
  mutate(Type="Museum")

## Combined ---------------------

# combine for combined plot

prev_dec_all <- bind_rows(prev_field_dec, prev_museum_dec)



# Dot Plot with Lines ------------------------------------------

## Field Year -----------------------------------------

(gg_field_yr <- ggplot(prev_field) +
  #geom_bar(aes(x=Year, y=OverallSampleSize/max(OverallSampleSize)*100), stat="identity") +
  geom_line(aes(x=Year, y=BdPrevalence, group=1), size=0.5, lty=2, col="red4", alpha=0.8) +
  geom_errorbar(aes(Year, ymin = LowerCI, ymax = UpperCI),
                width= 0.2, size=0.7, col="red4") +
  geom_point(aes(Year, BdPrevalence, size=OverallSampleSize), fill="red2", pch=21, alpha=0.9) +
  guides(size = guide_legend("Total Samples")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        axis.text = element_text(size = 12),
        axis.title = element_text(size=14),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),

        #axis.title.y.right = element_text(color="gray40", vjust = 1),
        #axis.text.y.right = element_text(color="gray40"),
        #axis.ticks.y.right = element_line(color="gray40"),
        #axis.line.y.right = element_line(color = "gray40"),
        plot.background = element_rect(fill="white")) +
  labs(title = "Field samples", y="Bd Prevalence (%)", x="")) #+
  #scale_y_continuous(sec.axis=sec_axis(~.*max(prev_field$OverallSampleSize)/100, name="Total Sample Size"))


#ggsave("figs/prevalence_by_yr_w_samplesizedots_field_w2yax.png",
#       width = 10, height = 6, dpi=300)

#ggsave("figs/Fig_02_prevalence_by_yr_w_samplesizedots_field.png",
#       width = 10, height = 6, dpi=300)

## Field Decade -----------------------------------------

(gg_field_dec <- ggplot(prev_field_dec) +
  #geom_bar(aes(x=Year, y=OverallSampleSize/max(OverallSampleSize)*100), stat="identity") +
  geom_line(aes(x=decade, y=BdPrevalence, group=1), size=0.5, lty=2, col="red4", alpha=0.8) +
  geom_errorbar(aes(x=decade, ymin = LowerCI, ymax = UpperCI),
                width= 0.2, size=0.7, col="red4") +
  geom_point(aes(x=decade, y=BdPrevalence, size=OverallSampleSize), fill="red2", pch=21, alpha=0.9) +
  guides(size = guide_legend("Total Samples")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        axis.text = element_text(size = 12),
        axis.title = element_text(size=14),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),

        #axis.title.y.right = element_text(color="gray40", vjust = 1),
        #axis.text.y.right = element_text(color="gray40"),
        #axis.ticks.y.right = element_line(color="gray40"),
        #axis.line.y.right = element_line(color = "gray40"),
        plot.background = element_rect(fill="white")) +
  labs(title = "Field samples", y="Bd Prevalence (%)", x="")) #+
#scale_y_continuous(sec.axis=sec_axis(~.*max(prev_field$OverallSampleSize)/100, name="Total Sample Size"))


#ggsave("figs/prevalence_by_yr_w_samplesizedots_field_w2yax.png",
#       width = 10, height = 6, dpi=300)

#ggsave("figs/Fig_02_prevalence_by_dec_w_samplesizedots_field.png",
#       width = 10, height = 6, dpi=300)

## Museum -----------------------------------------

(gg_mus_dec <- ggplot(prev_museum_dec) +
  #geom_bar(aes(x=decade, y=OverallSampleSize/max(OverallSampleSize)*100), stat="identity") +
  geom_line(aes(x=decade, y=BdPrevalence, group=1), size=0.5, lty=2, col="red4", alpha=0.8) +
  geom_errorbar(aes(decade, ymin = LowerCI, ymax = UpperCI),
                width= 0.2, size=0.7, col="maroon") +
  geom_point(aes(decade, BdPrevalence, size=OverallSampleSize), fill="maroon", pch=21, alpha=0.9) +
  guides(size = guide_legend("Total Samples")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=0.5, vjust=0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size=14),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),

        #axis.title.y.right = element_text(color="gray40", vjust = 1),
        #axis.text.y.right = element_text(color="gray40"),
        #axis.ticks.y.right = element_line(color="gray40"),
        #axis.line.y.right = element_line(color = "gray40"),
        plot.background = element_rect(fill="white")) +
  labs(title = "Museum samples", y="Bd Prevalence (%)", x="") +
  scale_size_continuous(breaks=c(30,60,90), range = c(1,8)))#+
#scale_y_continuous(sec.axis=sec_axis(~.*max(prev_field$OverallSampleSize)/100, name="Total Sample Size"))


# ggsave("figs/prevalence_by_yr_w_samplesizedots_museum_w2yax.png",
#        width = 10, height = 6, dpi=300)

#ggsave("figs/Fig_02_prevalence_by_dec_w_samplesizedots_museum.png",
#       width = 10, height = 6, dpi=300)


# FIG02A: All Samples ----------------------------

# w combined data to address sample scale legend
(gg_prev_by_dec2 <- ggplot() +
    # museum by dec
    geom_line(data = prev_dec_all, aes(x=decade, y=BdPrevalence, group=Type, color=Type), size=0.5, lty=2, alpha=0.8, show.legend = FALSE) +
    geom_errorbar(data = prev_dec_all, aes(decade, ymin = LowerCI, ymax = UpperCI, color=Type),
                  width= 0.2, size=0.7, show.legend = FALSE) +
    geom_point(data = prev_dec_all, aes(decade, BdPrevalence, size=OverallSampleSize, fill=Type), pch=21, alpha=0.8) +

    ## field by dec
    # geom_line(data=prev_dec_all, aes(x=decade, y=BdPrevalence, group=Type, color=Type), size=0.5, lty=2, alpha=0.8) +
    # geom_errorbar(data=prev_dec_all, aes(x=decade, ymin = LowerCI, ymax = UpperCI, color=Type),
    #               width= 0.2, size=0.7, show.legend = FALSE) +
    # geom_point(data=prev_dec_all, aes(x=decade, y=BdPrevalence, size=OverallSampleSize, fill=Type), pch=21, alpha=0.9) +

   scale_fill_manual("Type", values = colblind_pal) +
   scale_color_manual("Type", values = colblind_pal) +
   # legend
  guides(size = guide_legend("Total Samples"),
         fill = "none",
         color = "none") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust=0.5, vjust=0.5),
          axis.text = element_text(size = 12),
          axis.title = element_text(size=14),
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          plot.background = element_rect(fill="white")) +
    labs(y="Bd Prevalence (%)", x="") +
   # breaks for dot sizes
   scale_size_continuous(breaks=c(10,100,300), range = c(3,12)) +
    annotate(geom = "segment", x=12.5, xend=12.5, y = 0, yend=100, color="gray80", lwd=1.5, lty=2) +
    annotate(geom = "text",label="Museum", x=11, y = 85, color=colblind_pal[2], size=5) +
    annotate(geom = "text", label="Field", x=13.5, y = 85, color=colblind_pal[1], size=5))

# save it
ggsave("figs/Fig_02_prevalence_by_dec_w_samplesizedots_combined.png",
       width = 10, height = 6, dpi=300, bg="white")


# Field: Make Data By Month -----------------------------------------------------------

## FIELD

# make prevalence df with CI for month
prev_field_month <- field_samples_no_larvae %>%
  group_by(month) %>% # summarize
  add_tally(name = "OverallSampleSize") %>%
  select(month, OverallSampleSize, bd_positive) %>%
  group_by(month, bd_positive) %>%
  add_tally(name="bd_pos_total") %>%
  distinct(month, .keep_all = TRUE) %>% #drop duplicate yrs
  ungroup() %>%
  # now fill and filter to keep unique totals for Bd_pos
  complete(month, bd_positive, fill = list(bd_pos_total=0)) %>%
  # fill totals
  group_by(month) %>% fill(OverallSampleSize, .direction = "downup") %>%
  # filter to just the pos
  filter(bd_positive == 1) %>%
  select(-bd_positive) %>%
  group_by(month) %>%
  # calc prevalence
  mutate(BdPrevalence = (bd_pos_total/OverallSampleSize)*100,
         LowerCI = exactci(bd_pos_total, OverallSampleSize, conf.level = 0.95)$conf.int[1]*100,
         UpperCI = exactci(bd_pos_total, OverallSampleSize, conf.level = 0.95)$conf.int[2]*100) %>%
  ungroup() #%>%
  # make months factors that are ordered
  #mutate(month = factor(month, levels = 1:12))

# make precip by month
# make prevalence df with CI for month
prev_field_ppt_month <- field_samples_no_larvae %>%
  # convert ppt to mm
  mutate(pcpn_noaa = pcpn_noaa * 25.4) %>%
  #mutate(month = factor(month, levels = 1:12)) %>%
  group_by(month) %>% # summarize
  summarize(ppt_avg = mean(pcpn_noaa, na.rm = TRUE),
            ppt_max = max(pcpn_noaa, na.rm = TRUE),
            ppt_min = min(pcpn_noaa, na.rm = TRUE))

# bind data in to same dataset
prev_field_ppt_df <- left_join(prev_field_month, prev_field_ppt_month) %>%
  mutate(month=as.integer(month))

prev_field_spei_month <- field_samples_no_larvae %>%
  group_by(month) %>% # summarize
  summarize(spei_avg = mean(spei24, na.rm = TRUE),
            spei_max = max(spei24, na.rm = TRUE),
            spei_min = min(spei24, na.rm = TRUE))

# bind data in to same dataset
prev_field_spei_df <- left_join(prev_field_month, prev_field_spei_month) %>%
  mutate(month=as.integer(month))


# FIG02B: Field by month w Precip -----------------------------------------------------------

(gg_field_mon_precip <- ggplot() +
   geom_ribbon(data = prev_field_ppt_df, aes(x=month, ymax=ppt_max, ymin=ppt_min), fill="cornflowerblue",alpha=0.7) +
   geom_line(data = prev_field_ppt_df, aes(x=month, y=ppt_avg), color="darkblue",alpha=0.5, lwd=0.5, lty=3) +
   geom_line(data = prev_field_ppt_df, aes(x=month, y=BdPrevalence, group=1), size=0.5, lty=2, col=colblind_pal[1], alpha=0.8) +
   geom_errorbar(data = prev_field_ppt_df, aes(x=month, ymin = LowerCI, ymax = UpperCI),
                 width= 0.2, size=0.7, col=colblind_pal[1]) +
   geom_point(data=prev_field_ppt_df, aes(x=month, y=BdPrevalence, size=OverallSampleSize), fill=colblind_pal[1], alpha=0.9, pch=21) +
   guides(size = guide_legend("Total Samples")) +
    # match scale
   scale_size_continuous(breaks=c(10,100,300), range = c(3,12)) +
   #scale_size_continuous(breaks=c(10,50,100,500), range = c(3,9)) +
   theme_minimal() +
   scale_x_continuous("Month", breaks=c(1:12))+
   theme(
     axis.text = element_text(size = 12),
     axis.title = element_text(size=14),
     plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
     plot.background = element_rect(fill="white"),
     axis.title.y.right = element_text(color="darkblue", vjust = 1),
     axis.text.y.right = element_text(color="darkblue"),
     axis.ticks.y.right = element_line(color="darkblue"),
     axis.line.y.right = element_line(color = "darkblue")) +
   labs(y="Bd Prevalence (%)", x="Month") +
   scale_y_continuous(sec.axis=sec_axis(~./1, name="Precip (mm)")))

ggsave("figs/Fig_02_bd_prevalence_by_month_w_precip_2yax_field_mm.png",
       width = 10, height = 6, dpi=300, bg = "white")


# Prevalence with SPEI ----------------------------------------------------

# rescale 0-100
scales::rescale(prev_field_spei_df$spei_avg, to = c(0,100))
plogis(prev_field_spei_df$spei_avg)*100
qlogis(plogis(prev_field_spei_df$spei_avg))
prev_field_spei_df$spei_avg


(gg_field_mon_spei <- ggplot() +
   geom_ribbon(data = prev_field_spei_df, aes(x=month, ymax=plogis(spei_max)*100, ymin=plogis(spei_min)*100), fill="orange",alpha=0.7) +
   geom_line(data = prev_field_spei_df, aes(x=month, y=plogis(spei_avg)*100), color="orange4",alpha=0.5, lwd=0.5, lty=3) +
   geom_line(data = prev_field_spei_df, aes(x=month, y=BdPrevalence, group=1), size=0.5, lty=2, col=colblind_pal[1], alpha=0.8) +
   geom_errorbar(data = prev_field_spei_df, aes(x=month, ymin = LowerCI, ymax = UpperCI),
                 width= 0.2, size=0.7, col=colblind_pal[1]) +
   geom_point(data=prev_field_spei_df, aes(x=month, y=BdPrevalence, size=OverallSampleSize), fill=colblind_pal[1], alpha=0.9, pch=21) +
   guides(size = guide_legend("Total Samples")) +
   theme_minimal() +
    scale_size_continuous(breaks=c(10,100,300), range = c(3,12)) +
   scale_x_continuous("Month", breaks=c(1:12))+
   theme(
     axis.text = element_text(size = 12),
     axis.title = element_text(size=14),
     plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
     plot.background = element_rect(fill="white"),
     axis.title.y.right = element_text(color="darkorange", vjust = 1),
     axis.text.y.right = element_text(color="darkorange"),
     axis.ticks.y.right = element_line(color="darkorange"),
     axis.line.y.right = element_line(color = "darkorange")) +
   labs(title = "Field samples", y="Bd Prevalence (%)", x="Month") +
   scale_y_continuous(sec.axis=sec_axis(~qlogis(./100), name="SPEI24")))

ggsave("figs/Fig_02_prevalence_by_month_w_spei_2yax_field.png",
       width = 10, height = 6, dpi=300, bg="white")


# Cowplot things together -----------------------

library(cowplot)

gg_final <- cowplot::plot_grid(gg_prev_by_dec2, gg_field_mon_precip, nrow = 2, labels = c("A","B"))
gg_final
cowplot::save_plot(gg_final, filename = "figs/Fig_02_prevalence_combined_plot.png", base_height = 10, base_width = 8.5, dpi=600)
