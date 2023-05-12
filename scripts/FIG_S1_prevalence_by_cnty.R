#Fig S1

library(plyr)
library(ggplot2)
library(forcats)
library(dplyr)
library(sf)

# get data -----------
boylii_ind_data <- readRDS("output/10a_rb_bd_all_covars_recap_filt.rds") %>%
  sf::st_drop_geometry()

# tidy data ------------

# fix boylii clade as integer
levels(boylii_ind_data$boylii_clade) # since level order correct can convert
# relevel
boylii_ind_data$boylii_clade <- forcats::fct_relevel(boylii_ind_data$boylii_clade, c("S. Coast",  "C. Coast",
                                                                                     "N. Coast", "S. Sierra", "N. Sierra", "Feather"))
levels(boylii_ind_data$boylii_clade) # since level order correct
# make numeric
boylii_ind_data$clade_order<- as.integer(boylii_ind_data$boylii_clade)


# check these all line up
table(boylii_ind_data$boylii_clade)
table(boylii_ind_data$clade_order)
class(boylii_ind_data$clade_order) #class is still integer

# summarize by county, field or museum

# using dplyr
r2 <- boylii_ind_data %>% group_by(county, field_or_museum) %>%
  summarize(mean_prev=mean(bd_positive)*100) %>% ungroup()

# now add total using dplyr
boylii_ind_data <- boylii_ind_data %>% sf::st_drop_geometry() %>%
  group_by(county, field_or_museum) %>%
  add_tally() %>%
  left_join(r2) %>%
  select(sampleid:county, n, mean_prev, clade_order, boylii_clade:comid)

# remove extra fresno for plotting purposes
boylii_ind_data <- boylii_ind_data %>%
  filter(!sampleid=="MVZ94419")

# data viz ------------

# setup color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## plot
ggplot(data=boylii_ind_data,
       aes(x = fct_reorder(.f = county, clade_order),
           fill=boylii_clade)) +
  geom_bar(color="black") +
  geom_text(data=boylii_ind_data %>% distinct(.keep_all = TRUE),
            aes(x = fct_reorder(.f = county, clade_order),
                fill=boylii_clade, y=n, label=round(mean_prev, 1)), angle=90, hjust=-0.2, size = 3) +
  scale_y_continuous(limits=c(0,400))+
  scale_fill_manual(values=cbPalette, name="Clade") +
  theme_bw(base_family = "Roboto", base_size = 14) +
  theme(axis.text.x=element_text(angle=45, hjust=0.9)) +
  xlab("County") +
  facet_wrap(~field_or_museum, nrow=2, scales="free")


# ggsave
ggsave(filename = "figs/FIG_S1_prevalence_by_cnty_clade.png",
       dpi=300, width = 11, height = 8.5)
ggsave(filename = "figs/FIG_S1_prevalence_by_cnty_clade.pdf",
       dpi=300, width = 11, height = 8.5, device = cairo_pdf)
