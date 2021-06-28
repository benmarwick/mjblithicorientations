# run all of 001 and 002 before running anything here

# Data from Marwick et al 2017, a trampling experiment at MJB

tramp <- read_csv(here::here("data/trampling.csv"))

# filter to keep only lithics with exactly 2 total station points,
# suitable for orientation, etc. analysis
tramp_with_two_points <-
  tramp %>%
  add_count(Description) %>%
  filter(n == 2) %>%
  filter(str_detect(Description, "A5|A10|A15")) %>%
  arrange(Description) %>%
  tibble()

# get into format x1, y1, z1, x2, y2, z2 for McPherron's workflow
# our values are  Ynew, Xnew_flipped, depth_below_ground_surface
tramp_with_two_points_mcpherron <-
  tramp_with_two_points %>%
  group_by(Description) %>%
  mutate(id = row_number()) %>%
  dplyr::select(Description, id, Northing, Easting, Elevation) %>%
  pivot_longer(cols = c(Northing, Easting, Elevation),
               names_to = "coord",
               values_to = "value") %>%
  mutate(coord = case_when(
    coord == "Easting" ~ "X",
    coord == "Northing" ~ "Y",
    coord == "Elevation" ~ "Z"
  )) %>%
  unite(col="coord_set",
        coord,
        id,
        sep = "") %>%
  pivot_wider(names_from = coord_set,
              values_from = value) %>%
  mutate(useful::cart2pol(X1-X2, Y1-Y2, degrees = T)) %>%
  # make it join-able with the MJB data
  mutate(exc_row  = NA,
         exc_loc = NA,
         phase_groups = str_extract(Description, "A[[:digit:]]{1,2}"))

# join with archaeological data
mjb_phase_groups_front_back_tramp <-
mjb_phase_groups_front_back %>%
  bind_rows(tramp_with_two_points_mcpherron)

# using the function in 003, now showing the archaeological and trampling artefacts
plot_2 <- ~{benn_diagram_for_all_units(xyz = mjb_phase_groups_front_back_tramp)}

# draw the plot
library(cowplot)
ggdraw(plot_2)

ggsave(here::here("figures/benn_diagram_for_all_units_and_trampling.png"),
       height = 5,
       width = 5,
       dpi = 900)


# panel plots,  apply this function...
for(i in unique(tramp_with_two_points_mcpherron$phase_groups)){

  # extract artfacts from the i-th analytical unit
  xyz_level_i <- tramp_with_two_points_mcpherron %>%
    filter(phase_groups == i)

  # compute the plot
  the_plot <- ~{make_benn_rose_plots_panel(xyz_level = xyz_level_i)}

  # draw the plot
  ggdraw(the_plot)

  # save the plot
  ggsave(here::here(paste0("figures/panel_plot_for_tramp_", i, ".png")),
         height = 6,
         width = 10,
         dpi = 900)

  # this will give us a panel of three plots (Benn, rose-bearing, rose-plunge) for
  # each analytical unit in our data
}

# summary table for bearing and plunge stat tests
tramp_with_two_points_mcpherron_bearing_tests <-
  tramp_with_two_points_mcpherron %>%
  ungroup() %>%
  nest(-phase_groups) %>%
  mutate(tests = map(data, ~kuiper_and_rayleigh_and_ks_tests(.x))) %>%
  unnest(tests) %>%
  dplyr::select(-data,
                `Phase groups` = phase_groups)

tramp_with_two_points_mcpherron_bearing_tests


