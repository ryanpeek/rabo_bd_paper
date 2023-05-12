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

# load mus model data
load("output/models_dat_out_mus.rda")


m_plot <- m_mus_1

# now by cluster rate
p1m <- plot(ggpredict(m_plot,
                      terms=c("daylen_hr [all]", "clustrate [0,2]"),
                      type="re"),
            ci=TRUE, ci.style =  "dash", alpha = 0.15,
            line.size = 1.7, add.data = FALSE,
            colors = c("gray30", "red2")) +
  labs(title = "Predicted Probabilities: Museum +1/0",
       color="Cluster Rate", y = "Prob Bd+") +
  theme(legend.position = "right")

p2m <- plot(ggpredict(m_plot,
                      terms=c("spei24 [all]", "clustrate [0,2]"),
                      type="re"),
            ci=TRUE, ci.style =  "dash", alpha = 0.15,
            line.size = 1.7, add.data = FALSE,
            colors = c("gray30", "red2")) +
  labs(title = "",
       color="Cluster Rate", y = "Prob Bd+") +
  theme(legend.position = "right")
#p2m

p3m <- plot(ggpredict(m_plot,
                      terms=c("decade [all]", "clustrate [0,2]"),
                      type="re"),
            ci=TRUE, ci.style =  "dash", alpha = 0.15,
            line.size = 1.7, add.data = FALSE,
            colors = c("gray30", "red2")) +
  labs(title = "",
       color="Cluster Rate", y = "Prob Bd+") +
  theme(legend.position = "right")

p4m <- plot(ggpredict(m_plot,
                      terms=c("latitude_dd [all]", "clustrate [0,2]"),
                      type="re"),
            ci=TRUE, ci.style =  "dash", alpha = 0.15,
            line.size = 1.7, add.data = FALSE,
            colors = c("gray30", "red2")) +
  labs(title = "",
       color="Cluster Rate", y = "Prob Bd+") +
  theme(legend.position = "right")


(p1m + p2m) / (p3m + p4m) +
  plot_layout(guides = 'collect')

ggsave(here("figs/Fig_S8_museum_bern_margfx_plots.png"), width = 10, height = 8, units = "in", dpi = 300)

