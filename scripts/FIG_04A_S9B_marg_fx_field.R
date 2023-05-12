# plot marginal fx

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(glue)
library(janitor)
library(lubridate)
library(here)
# load library
library(glmmTMB)
library(DHARMa) # for evaluating
library(sjPlot) # for plotting
library(ggeffects)
library(patchwork)

# fn to back transform (values * sd + mean)
f_backtransform <- function(x){
  x * attr(x, 'scaled:scale') + attr(x, 'scaled:center')
}


# Load Data ---------------------------------------------------------------

# load field model data
load("output/models_dat_out_field.rda")

# single marg FX ----------------------------------------

# look at marginal effects
p1 <- plot(ggpredict(m_all_f, terms=c("tmax_noaa [all]"), type="re"), ci=TRUE) +
  ggthemes::scale_color_colorblind() +
  labs(title = "Predicted Probabilities: Field +1/0",
       y = "Prob Bd+") +  theme(legend.position = "right")
p2 <- plot(ggpredict(m_all_f, terms=c("daylen_hr [all]"), type="re"), ci=TRUE) +
  labs(title="", y = "Prob Bd+") +
  ggthemes::scale_color_colorblind()
p3 <- plot(ggpredict(m_all_f, terms=c("spei24 [all]"), type="re"), ci=TRUE) +
  labs(title="", y = "Prob Bd+") +
  ggthemes::scale_color_colorblind()
# just elevation?
p4 <- plot(ggpredict(m_all_f, terms=c("elevation [all]"), type="re"), ci=TRUE) +
  labs(title = "",
       y = "Prob Bd+") +  theme(legend.position = "right") +
  ggthemes::scale_color_colorblind()
# just ag hydric
p5 <- plot(ggpredict(m_all_f, terms=c("AG_HYDRIC_PCT_WS [all]"), type="re"), ci=TRUE) +
  labs(title = "",
       y = "Prob Bd+") +  theme(legend.position = "right") +
  ggthemes::scale_color_colorblind()

p6 <- plot(ggpredict(m_all_f, terms=c("PCT_WETLANDS_REMAINING [all]"), type="re"), ci=TRUE) +
  labs(title = "",
       y = "Prob Bd+") +  theme(legend.position = "right") +
  ggthemes::scale_color_colorblind()
#ggsave(here("figs/field_bern_re_cluster.png"), width = 10, height = 8, units = "in", dpi = 300)

p7 <- plot(ggpredict(m_all_f, terms=c("latitude_dd [all]"), type="re"), ci=TRUE) +
  labs(title = "Predicted Probabilities: Field +1/0",
       y = "Prob Bd+") +  theme(legend.position = "right") +
  ggthemes::scale_color_colorblind()
#p7

# make plot with patchwork
(p1 + p2 + p3) / (p4 + p5 + p6) +  plot_layout(guides = 'collect')

#ggsave(here("figs/field_bern_re_predicted_probs.png"), width = 10, height = 8, units = "in", dpi = 300)


# Colored Marg Fx ---------------------------------------------------------

# now by cluster rate
p1cr <- plot(ggpredict(m_all_f,
                       terms=c("tmax_noaa [all]", "clustrate [0,1,2]"),
                       type="re"),
             ci=TRUE, ci.style =  "dash", alpha = 0.15,
             line.size = 1.7, add.data = FALSE,
             colors = c("gray50", "darkblue","red2")) +
  labs(title = "Predicted Probabilities: Field +1/0",
       color="Cluster Rate", y = "Prob Bd+") +
  theme(legend.position = "right")
#p1cr

p2cr <- plot(ggpredict(m_all_f,
                       terms=c("daylen_hr [all]", "clustrate [0,1,2]"),
                       type="re"),
             ci=TRUE, ci.style =  "dash", alpha = 0.15,
             line.size = 1.7, add.data = FALSE,
             colors = c("gray50", "darkblue","red2")) +
  labs(title = "",
       color="Cluster Rate", y = "Prob Bd+") +
  theme(legend.position = "right")
#p2cr

p3cr <- plot(ggpredict(m_all_f,
                       terms=c("spei24 [all]", "clustrate [0,1,2]"),
                       type="re"),
             ci=TRUE, ci.style =  "dash", alpha = 0.15,
             line.size = 1.7, add.data = FALSE,
             colors = c("gray50", "darkblue","red2")) +
  labs(title = "",
       color="Cluster Rate", y = "Prob Bd+") +
  theme(legend.position = "right")
#pc3cr

p4cr <- plot(ggpredict(m_all_f,
                       terms=c("elevation [all]", "clustrate [0,1,2]"),
                       type="re"),
             ci=TRUE, ci.style =  "dash", alpha = 0.15,
             line.size = 1.7, add.data = FALSE,
             colors = c("gray50", "darkblue","red2")) +
  labs(title = "",
       color="Cluster Rate", y = "Prob Bd+") +
  theme(legend.position = "right")
#p4cr

p5cr <- plot(ggpredict(m_all_f,
                       terms=c("AG_HYDRIC_PCT_WS [all]", "clustrate [0,1,2]"),
                       type="re"),
             ci=TRUE, ci.style =  "dash", alpha = 0.15,
             line.size = 1.7, add.data = FALSE,
             colors = c("gray50", "darkblue","red2")) +
  labs(title = "",
       color="Cluster Rate", y = "Prob Bd+") +
  theme(legend.position = "right")
#p5cr

p6cr <- plot(ggpredict(m_all_f,
                       terms=c("PCT_WETLANDS_REMAINING [all]", "clustrate [0,1,2]"),
                       type="re"),
             ci=TRUE, ci.style =  "dash", alpha = 0.15,
             line.size = 1.7, add.data = FALSE,
             colors = c("gray50", "darkblue","red2")) +
  labs(title = "",
       color="Cluster Rate", y = "Prob Bd+") +
  theme(legend.position = "right")

(p1cr + p2cr + p3cr) / (p4cr + p5cr + p6cr) +
  plot_layout(guides = 'collect')

## Fig 4A ------------------------------------

p1_4a <- plot(ggpredict(m_all_f,
                       terms=c("tmax_noaa [all]", "clustrate [0,1,2]"),
                       type="re"),
             ci=TRUE, ci.style =  "dash", alpha = 0.15,
             line.size = 1.7, add.data = FALSE,
             colors = c("gray50", "darkblue","red2")) +
  labs(title = "",
       color="Cluster Rate", y = "Prob Bd+") +
  theme(legend.position = "right")

p2_4a <- plot(ggpredict(m_all_f,
                       terms=c("spei24 [all]", "clustrate [0,1,2]"),
                       type="re"),
             ci=TRUE, ci.style =  "dash", alpha = 0.15,
             line.size = 1.7, add.data = FALSE,
             colors = c("gray50", "darkblue","red2")) +
  labs(title = "",
       color="Cluster Rate", y = "Prob Bd+") +
  theme(legend.position = "right")

p1_4a + p2_4a +
  plot_layout(guides = 'collect')

ggsave(here("figs/Fig_4A_margfx_field_bern_re_clustrate.png"), width = 10, height = 4.5, units = "in", dpi = 300)

# Fig S9B ---------------------------------------


p9B_a <- plot(ggpredict(m_all_f,
                       terms=c("daylen_hr [all]", "clustrate [0,1,2]"),
                       type="re"),
             ci=TRUE, ci.style =  "dash", alpha = 0.15,
             line.size = 1.7, add.data = FALSE,
             colors = c("gray50", "darkblue","red2")) +
  labs(title = "",
       color="Cluster Rate", y = "Prob Bd+") +
  theme(legend.position = "right")


p9B_b <- plot(ggpredict(m_all_f,
                       terms=c("elevation [all]", "clustrate [0,1,2]"),
                       type="re"),
             ci=TRUE, ci.style =  "dash", alpha = 0.15,
             line.size = 1.7, add.data = FALSE,
             colors = c("gray50", "darkblue","red2")) +
  labs(title = "",
       color="Cluster Rate", y = "Prob Bd+") +
  theme(legend.position = "right")

p9B_a + p9B_b +
  plot_layout(guides = 'collect')

ggsave(here("figs/Fig_S9B_margfx_field_bern_re_clustrate.png"), width = 10, height = 4.5, units = "in", dpi = 300)
