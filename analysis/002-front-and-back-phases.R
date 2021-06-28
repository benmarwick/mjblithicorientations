library(CircStats)
library(fields)
library(circular)
library(tidyverse)
library(ggpubr)

# we have made a few minor changes to McPherron's code, so we use our local version
source(here::here("McPherron/orientations.R"))

mjb <- lithics_with_two_points_mcpherron_position_phases

# simplify phase groupings
mjb_phase_groups_front_back <-
mjb %>%
  mutate(phase_groups = case_when(
    phase == 2 & exc_loc == "front" ~ "Phase two: front",
    phase == 2 & exc_loc == "back" ~ "Phase two: back",
    phase %in% 3:7 ~ "Upper phases",
    TRUE  ~ "other"
  )) %>%
  filter(phase_groups != "other")

# how many in each group?
mjb_phase_groups_front_back %>%
  group_by(phase_groups) %>%
  tally()

#-----------------------------------------------------------------------
# adapt McPherron's figure 3 "Benn diagram for all La Ferrassie layers."
# functions are in orientations.R

# so we can save as an image using ggsave
plot_1 <- ~{benn_diagram_for_all_units(xyz = mjb_phase_groups_front_back)}

# draw the plot
library(cowplot)
ggdraw(plot_1)

ggsave(here::here("figures/benn_diagram_for_all_units.png"),
       height = 5,
       width = 5,
       dpi = 900)

#-----------------------------------------------------------------------
# adapt McPherron's Fig 4. Artifact orientations for La Ferrassie Layers 1 and 2.
# and similar to Fig 10 from Li et al 2021 https://www.sciencedirect.com/science/article/pii/S0305440320302302
# from https://github.com/lili0824/sdg_orientation/blob/main/analysis_final.R#L38


# now apply this function to make a composite figure...
for(i in unique(mjb_phase_groups_front_back$phase_groups)){

  # extract artfacts from the i-th analytical unit
  xyz_level_i <- mjb_phase_groups_front_back %>%
    filter(phase_groups == i)

  # compute the plot
  the_plot <- ~{make_benn_rose_plots_panel(xyz_level = xyz_level_i)}

  # draw the plot
  ggdraw(the_plot)

  # save the plot
  ggsave(here::here(paste0("figures/panel_plot_for_", i, ".png")),
         height = 6,
         width = 10,
         dpi = 900)

  # this will give us a panel of three plots (Benn, rose-bearing, rose-plunge) for
  # each analytical unit in our data
}


#---------------------------------------------------------------------
# https://www-sciencedirect-com.offcampus.lib.washington.edu/science/article/pii/S03054403203023024
# All orientation data, both simulated and archaeological, are evaluated
# using the Rayleigh test and the Kuiper's test functions from the R package#
# circular (Agostinelli and Lund, 2017) for bearing, and a Kolmogorov-Smirnov
# test for plunge. Some of this is from https://github.com/lili0824/sdg_orientation/blob/main/analysis_final.R

# run the Kuiper's test on all bearings, call the plunge_and_bearing function
# from orientations.R to process the data and get bearings only

# Code for this function is in orientations.R

# summary table for bearing and plunge stat tests
mjb_phase_groups_front_back_bearing_tests_all_tests_tbl <-
mjb_phase_groups_front_back %>%
  ungroup() %>%
  nest(-phase_groups) %>%
  mutate(tests = map(data, ~kuiper_and_rayleigh_and_ks_tests(.x))) %>%
  unnest(tests) %>%
  dplyr::select(-data,
                `Phase groups` = phase_groups) %>%
  relocate(N, .after = `Phase groups` )


















