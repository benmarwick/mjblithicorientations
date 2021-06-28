
source("https://raw.githubusercontent.com/surf3s/Orientations/master/orientations.R")

mjb <- lithics_with_two_points_mcpherron_position_phases

# simplify phase groupings
mjb_phase_groups <-
mjb %>%
  mutate(phase_groups = case_when(
    phase == 2 ~ "phase two",
    phase %in% 3:7 ~ "upper phases",
    TRUE  ~ "other"
  )) %>%
  filter(phase_groups != "other")

# how many in each group?
mjb_phase_groups %>%
  group_by(phase_groups) %>%
  tally()

# can we add the trampling experiment artefact data here?
# can we separate phase two front and back artefact clusters?

#-----------------------------------------------------------------------
# adapt McPherron's figure 3 "Benn diagram for all La Ferrassie layers."

benn_mjb = round(benn(xyz = mjb_phase_groups,
                      level = mjb_phase_groups$phase_groups,
                      min_sample = 1), 3)


benn_diagram(cbind(benn_mjb[,"EL"], benn_mjb[,"IS"]),
             id = rownames(benn_mjb),
             main = "",
             cex = 1.2,
             labels = "outside")

p = .05
resampling = 10

permutations =
  benn_permutations_by_level(mjb_phase_groups,
                             level = as.character(mjb_phase_groups$phase_groups),
                             resampling = resampling,
                             min_sample = 1,
                             p = p)

segments(permutations$segments[,1],permutations$segments[,2],
         permutations$segments[,3],permutations$segments[,4])

#-----------------------------------------------------------------------
# adapt McPherron's Fig 4. Artifact orientations for La Ferrassie Layers 1 and 2.

mjb_2 <-
mjb %>%
  filter(phase == 2)

par(mfrow= c(2,2))
  orientations(mjb_2,
             min_sample = 30,
             level = mjb_2$phase,
             overlay = NULL,
             spatial_benn = TRUE,
             main = paste("Phase ", unique(mjb_2$phase)))


mjb_upper <-
    mjb %>%
    filter(phase %in% 3:7) %>%
  mutate(phase = 'upper')

  par(mfrow= c(2,2))
  orientations(mjb_upper,
               min_sample = 30,
               level = mjb_upper$phase,
               overlay = NULL,
               spatial_benn = TRUE,
               main = paste("Phase ", unique(mjb_upper$phase)))












