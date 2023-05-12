# plot marginal fx for 5a

# see here: https://docs.google.com/presentation/d/1sZG0s4m6kyGWFI69zBKBB663m_zFbCL2mr_sC3XnkTk/edit#slide=id.g17d567bf2b1_0_0

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(glue)
library(cowplot)
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
#load("output/models_dat_out_its.rda")
load("output/model_its_out.rda")
m_its_f <- m_its_1


# Plots for 5A ------------------------------------------------------------

# marginal effects plots for ITS model
# aridity
# snout vent
# Tmax

## Aridity: single marg FX lines ------------------------------

# look at marginal effects with high and low dash
(aridity_dash <- plot(ggpredict(m_its_f,
               terms=c("spei24"), type="re"),
     ci=TRUE, ci.style =  "dash", alpha = 0.15,
     line.size = 1.7, add.data = FALSE) +
  labs(title="", y = expression(atop("Model estimate of Bd load ",paste("( log"["10"]~" ITS copies)"))), x="Aridity (SPEI 24 mo)") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size = 12)))

ggsave(filename = "figs/Fig_5A_marg_fx_ITS_aridity_dash_ci.png",
       width = 4, height = 3, dpi=300, bg = "white")

# # look at marginal effects with shade
# (aridity_ribbon <- plot(ggpredict(m_its_f,
#                                 terms=c("spei24"), type="re"),
#                       ci=TRUE, ci.style =  "ribbon", alpha = 0.15,
#                       line.size = 1.7, add.data = FALSE) +
#     labs(title="",
#           y = expression("Model estim. of Bd load ( log"["10"]~" ITS copies)"),
#          x="Aridity (SPEI 24 mo)") +
#     theme(axis.title = element_text(size=14),
#           axis.text = element_text(size = 12)))
#
# ggsave(filename = "figs/Fig_5A_marg_fx_ITS_aridity_ribbon_ci.png",
#        width = 4, height = 3, dpi=300, bg = "white")


## SVL: single marg FX lines ------------------------------

# look at marginal effects with high and low dash
(svl_dash <- plot(ggpredict(m_its_f,
                                terms=c("svl_mm"), type="re"),
                      ci=TRUE, ci.style =  "dash", alpha = 0.15,
                      line.size = 1.7, add.data = FALSE) +
   labs(title="",
        y = "",
        #y = expression("Bd load ( log"["10"]~" ITS copies)"),
        x="Snout-vent length (mm)") +
   theme(axis.title = element_text(size=14),
         axis.text = element_text(size = 12)))

ggsave(filename = "figs/Fig_5A_marg_fx_ITS_svl_dash_ci.png",
       width = 4, height = 3, dpi=300, bg = "white")


# look at marginal effects with shade
# (svl_ribbon <- plot(ggpredict(m_its_f,
#                                   terms=c("svl_mm"), type="re"),
#                         ci=TRUE, ci.style =  "ribbon", alpha = 0.15,
#                         line.size = 1.7, add.data = FALSE) +
#     labs(title="",
#          y = "",
#          #y = expression("Bd load ( log"["10"]~" ITS copies)"),
#          x="Snout-vent length (mm)") +
#     theme(axis.title = element_text(size=14),
#           axis.text = element_text(size = 12))) #+
#
# ggsave(filename = "figs/Fig_5A_marg_fx_ITS_svl_ribbon_ci.png",
#        width = 4, height = 3, dpi=300, bg = "white")


## Tmax: single marg FX lines ------------------------------

# look at marginal effects with high and low dash
(tmax_dash <- plot(ggpredict(m_its_f,
                            terms=c("tmax_noaa"), type="re"),
                  ci=TRUE, ci.style =  "dash", alpha = 0.15,
                  line.size = 1.7, add.data = FALSE) +
   labs(title="",
        y="",
        #y = expression("Bd load ( log"["10"]~" ITS copies)"),
        x=expression("T"["max"])) +
   theme(axis.title = element_text(size=14),
         axis.text = element_text(size = 12)))

ggsave(filename = "figs/Fig_5A_marg_fx_ITS_tmax_dash_ci.png",
       width = 4, height = 3, dpi=300, bg = "white")


# look at marginal effects with shade
# (tmax_ribbon <- plot(ggpredict(m_its_f,
#                               terms=c("tmax_noaa"), type="re"),
#                     ci=TRUE, ci.style =  "ribbon", alpha = 0.15,
#                     line.size = 1.7, add.data = FALSE) +
#     labs(title="",
#          y = "",
#          #y = expression("Bd load ( log"["10"]~" ITS copies)"),
#          x=expression("T"["max"])) +
#     theme(axis.title = element_text(size=14),
#           axis.text = element_text(size = 12))) #+
#
# ggsave(filename = "figs/Fig_5A_marg_fx_ITS_tmax_ribbon_ci.png",
#        width = 4, height = 3, dpi=300, bg = "white")


# cowplot -----------------------------------------------------------------

# dash w cowplot
comb_dash <- cowplot::plot_grid(aridity_dash, svl_dash, tmax_dash, align = "h", nrow = 1)
comb_dash

ggsave(plot=comb_dash, filename = "figs/Fig_5A_marg_fx_ITS_all_dash_ci.png",scale = 1.1,
       width = 9, height = 3.1, dpi=300, bg = "white")


# ribbon w cowplot
comb_ribbon <- cowplot::plot_grid(aridity_ribbon, svl_ribbon, tmax_ribbon, align = "h", nrow = 1, labels = "(a)")
comb_ribbon

ggsave(plot=comb_ribbon, filename = "figs/Fig_5A_marg_fx_ITS_all_ribbon_ci.png",scale = 1.1,
       width = 9, height = 3, dpi=300, bg = "white")

