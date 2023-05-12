# plots for 5B
# aridity vs raw log ITS load with basic linear regression between coast and sierra
# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(glue)
library(cowplot)
# load library
library(sjPlot) # for plotting
library(ggeffects)
library(patchwork)

# Load Data ---------------------------------------------------------------

# load field model data
boylii_ind_data<-read_csv("output/10a_rb_bd_all_covars_recap_filt.csv") %>%
  # convert to C
  mutate(tmax_noaa = (tmax_noaa-32) * (5/9))

field_samples<- boylii_ind_data %>%
  filter(field_or_museum == "Field") %>%
  filter(life_stage_a_j_m_t != "T" | is.na(life_stage_a_j_m_t))

ITS_samples<- field_samples %>% filter(bd_its_copies>0)

# make coast and sierra
table(ITS_samples$boylii_clade) # n=378 for coast, n=118 sierra
ITS_samples_coast<- ITS_samples %>% filter(grepl("Coast", boylii_clade)) %>%
  mutate(coast_sierra = "Coast")

ITS_samples_sierra<- ITS_samples %>% filter(boylii_clade %in% c("S. Sierra", "N. Sierra", "Feather")) %>%
  mutate(coast_sierra = "Sierra")

# make final data of all
ITS_samples_all <- bind_rows(ITS_samples_coast, ITS_samples_sierra)


# Plots: Aridity -------------------------------------------------------

#first calculating ci for coast subset
mod_spei_coast <- lm(log10(bd_its_copies) ~ spei24, data = ITS_samples_coast)
pred_spei_coast <- predict(mod_spei_coast, se.fit = TRUE, interval = "confidence")
limits_spei_coast <- as.data.frame(pred_spei_coast$fit)

#then doing the same for sierra
mod_spei_sierra <- lm(log10(bd_its_copies) ~ spei24, data=ITS_samples_sierra)
pred_spei_sierra <- predict(mod_spei_sierra, se.fit = TRUE, interval = "confidence")
limits_spei_sierra <- as.data.frame(pred_spei_sierra$fit)

# PLOT
(gg_spei <- ggplot(data=ITS_samples_all, aes(x=spei24, y=log10(bd_its_copies))) +
  geom_point(color="gray50", alpha=0.8)+
  facet_grid(coast_sierra~.) +
  geom_smooth(se = FALSE, method = "lm", color="black", lwd=1.2)+
  # coast
  geom_line(data=ITS_samples_coast, aes(x=spei24, y=limits_spei_coast$lwr),
            linetype = 2, linewidth=0.5) +
  geom_line(data=ITS_samples_coast, aes(x=spei24, y=limits_spei_coast$upr),
            linetype = 2, linewidth=0.5) +
  # sierra
  geom_line(data=ITS_samples_sierra, aes(x=spei24, y=limits_spei_sierra$lwr),
            linetype = 2, linewidth=0.5) +
  geom_line(data=ITS_samples_sierra, aes(x=spei24, y=limits_spei_sierra$upr),
            linetype = 2, linewidth=0.5) +
  labs(y = expression("Bd load ( log"["10"]~" ITS copies)"),
       x="Aridity (SPEI 24 mo)") +
  theme_half_open() +
  cowplot::background_grid(major = "xy")+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size = 12)))

# save
ggsave(filename = "figs/Fig_5B_marg_fx_ITS_aridity_dash_ci.png",
       width = 6, height = 4, dpi=300, bg = "white")


# Plots: SVL -------------------------------------------------------------

#first calculating ci for coast subset (drop NAs)
ITS_samples_coast_svl <- ITS_samples_coast %>% filter(!is.na(svl_mm))
mod_svl_coast <- lm(log10(bd_its_copies) ~ svl_mm, data = ITS_samples_coast_svl)
pred_svl_coast <- predict(mod_svl_coast, se.fit = TRUE, interval = "confidence")
limits_svl_coast <- as.data.frame(pred_svl_coast$fit)

#then doing the same for sierra
mod_svl_sierra <- lm(log10(bd_its_copies) ~ svl_mm, data=ITS_samples_sierra)
pred_svl_sierra <- predict(mod_svl_sierra, se.fit = TRUE, interval = "confidence")
limits_svl_sierra <- as.data.frame(pred_svl_sierra$fit)

# SVL
(gg_svl <- ggplot(data=ITS_samples_all, aes(x=svl_mm, y=log10(bd_its_copies))) +
  geom_point(color="gray50", alpha=0.8)+
  facet_grid(coast_sierra~.) +
  geom_smooth(se = FALSE, method = "lm", color="black", lwd=1.2) +
  # coast
  geom_line(data=ITS_samples_coast_svl, aes(x=svl_mm, y=limits_svl_coast$lwr),
            linetype = 2, linewidth=0.5) +
  geom_line(data=ITS_samples_coast_svl, aes(x=svl_mm, y=limits_svl_coast$upr),
            linetype = 2, linewidth=0.5) +
  # sierra
  geom_line(data=ITS_samples_sierra, aes(x=svl_mm, y=limits_svl_sierra$lwr),
            linetype = 2, linewidth=0.5) +
  geom_line(data=ITS_samples_sierra, aes(x=svl_mm, y=limits_svl_sierra$upr),
            linetype = 2, linewidth=0.5) +
  labs(y=NULL,
    #y = expression("Bd load ( log"["10"]~" ITS copies)"),
    x="Snout-vent Length (mm)") +
  theme_half_open() +
  cowplot::background_grid(major = "xy")+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size = 12)))

# save
ggsave(filename = "figs/Fig_5B_marg_fx_ITS_svl_dash_ci.png",
       width = 6, height = 4, dpi=300, bg = "white")


# Plots: Tmax -------------------------------------------------------------

mod_tmax_coast <- lm(log10(bd_its_copies) ~ tmax_noaa, data = ITS_samples_coast)
pred_tmax_coast <- predict(mod_tmax_coast, se.fit = TRUE, interval = "confidence")
limits_tmax_coast <- as.data.frame(pred_tmax_coast$fit)

#then doing the same for sierra
mod_tmax_sierra <- lm(log10(bd_its_copies) ~ tmax_noaa, data=ITS_samples_sierra)
pred_tmax_sierra <- predict(mod_tmax_sierra, se.fit = TRUE, interval = "confidence")
limits_tmax_sierra <- as.data.frame(pred_tmax_sierra$fit)

# tmax
(gg_tmax <- ggplot(data=ITS_samples_all, aes(x=tmax_noaa, y=log10(bd_its_copies))) +
  geom_point(color="gray50", alpha=0.8)+
  facet_grid(coast_sierra~.) +
  geom_smooth(se = FALSE, method = "lm", color="black", lwd=1.2) +
  # coast
  geom_line(data=ITS_samples_coast, aes(x=tmax_noaa, y=limits_tmax_coast$lwr),
            linetype = 2, linewidth=0.5) +
  geom_line(data=ITS_samples_coast, aes(x=tmax_noaa, y=limits_tmax_coast$upr),
            linetype = 2, linewidth=0.5) +
  # sierra
  geom_line(data=ITS_samples_sierra, aes(x=tmax_noaa, y=limits_tmax_sierra$lwr),
            linetype = 2, linewidth=0.5) +
  geom_line(data=ITS_samples_sierra, aes(x=tmax_noaa, y=limits_tmax_sierra$upr),
            linetype = 2, linewidth=0.5) +
  labs(y=NULL,
       #y = expression("Bd load ( log"["10"]~" ITS copies)"),
       x=expression("T"["max"])) +
  theme_half_open() +
  cowplot::background_grid(major = "xy")+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size = 12)))


# save
ggsave(filename = "figs/Fig_5B_marg_fx_ITS_tmax_dash_ci.png",
       width = 6, height = 4, dpi=300, bg = "white")



# Cowplot -----------------------------------------------------------------

# dash w cowplot
comb_bdload <- cowplot::plot_grid(gg_spei, gg_svl, gg_tmax, align = "h", nrow = 1)
comb_bdload

ggsave(plot=comb_bdload, filename = "figs/Fig_5B_ITS_dash_ci.png",scale = 1.1,
       width = 9, height = 3, dpi=300, bg = "white")

