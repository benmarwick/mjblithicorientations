# run all of 001 and 002 before running anything here

# Data from Li et al 2021

li <- list.files(here::here("analysis/data"), pattern = "twoshots", full.names = TRUE)
terrain <- str_extract(basename(li), ".*_") %>%  str_remove("_")

li_tbl <-
  map_df(li, ~read_csv(.x) %>%
           # get into format x1, y1, z1, x2, y2, z2 for McPherron's workflow
           generalized_prepare_dataset(),
         .id = "phase_groups") %>%
  mutate(phase_groups = case_when(
    phase_groups == 1 ~ terrain[1],
    phase_groups == 2 ~ terrain[2],
    phase_groups == 3 ~ terrain[3]
  ))

# join Li et al with archaeological data
li_tbl <-
  mjb_phase_groups_front_back %>%
  bind_rows(li_tbl)

# using the function in orientations.R, now showing the archaeological and trampling artefacts
plot_li <- ~{benn_diagram_for_all_units(xyz = li_tbl)}

# draw the plot
library(cowplot)
ggdraw(plot_li)

ggsave(here::here("analysis/figures/benn_diagram_for_li_et_al.png"),
       height = 5,
       width = 5,
       dpi = 900)


# panel plots,  apply this function...
for(i in unique(li_tbl$phase_groups)){

  # extract artfacts from the i-th analytical unit
  xyz_level_i <- li_tbl %>%
    filter(phase_groups == i)

  # compute the plot
  the_plot <- ~{make_benn_rose_plots_panel(xyz_level = xyz_level_i)}

  # draw the plot
  ggdraw(the_plot)

  # save the plot
  ggsave(here::here(paste0("analysis/figures/panel_plot_for_li_", i, ".png")),
         height = 6,
         width = 10,
         dpi = 900)

  # this will give us a panel of three plots (Benn, rose-bearing, rose-plunge) for
  # each analytical unit in our data
}

# summary table for bearing and plunge stat tests
li_tbl_bearing_and_plunge_tests <-
  li_tbl %>%
  ungroup() %>%
  nest(-phase_groups) %>%
  mutate(tests = map(data, ~kuiper_and_rayleigh_and_ks_tests(.x))) %>%
  unnest(tests) %>%
  dplyr::select(-data,
                `Phase groups` = phase_groups)

# take a look
li_tbl_bearing_and_plunge_tests


