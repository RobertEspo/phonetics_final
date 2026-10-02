### GAMs ###

# spectral centroids

gam_f1_preds <- predict_gam(gam_f1,
                            exclude_terms = "s(session,participant)") %>%
  mutate(participant = "mean")

gam_f1_preds_re <- predict_gam(gam_f1)

gam_f2_preds <- predict_gam(gam_f2,
                            exclude_terms = "s(session,participant)") %>%
  mutate(participant = "mean")

gam_f2_preds_re <- predict_gam(gam_f2)

gam_preds <- bind_rows(
  gam_f1_preds %>%
    rename(centroid = centroid_f1) %>%
    mutate(formant = "F1"),
  gam_f2_preds %>%
    rename(centroid = centroid_f2) %>%
    mutate(formant = "F2"),
  gam_f1_preds_re %>%
    rename(centroid = centroid_f1) %>%
    mutate(formant = "F1"),
  gam_f2_preds_re %>%
    rename(centroid = centroid_f2) %>%
    mutate(formant = "F2")
)

direction_dat_re <- tibble(
  formant = c("F1", "F2"),
  participant = c("bco", "bco"),
  x = c(.5, .5),
  y = c(1050, 1700),
  label = c(
    "↑ more open\n↓ more closed",
    "↑ more front\n↓ more back"
  )
)

biggest_changes <- gam_preds %>%
  filter(participant != "mean") %>%
  arrange(participant, formant, stress, session) %>%
  group_by(participant, formant, stress) %>%
  mutate(
    change = centroid - lag(centroid),
    abs_change = abs(change)
  ) %>%
  filter(!is.na(abs_change)) %>%
  slice_max(abs_change, n = 1, with_ties = FALSE) %>%
  ungroup()

f1_f2_p_all <- ggplot(
  gam_preds,
  aes(x = session, y = centroid, color = stress, fill = stress, linetype = stress)
) +
  geom_ribbon(
    aes(ymin = lower_ci[, 1], ymax = upper_ci[, 1]),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(
    data = biggest_changes,
    size = .5,
    shape = 21,
    color = "black"
  ) +
  geom_text(
    data = direction_dat_re,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    size = 1
  ) +
  scale_color_grey(start = 0.2, end = 0.6) +
  scale_fill_grey(start = 0.2, end = 0.6) +
  facet_grid(formant ~ participant, scales = "free_y") +
  labs(y = "Formant centroid (Hz)") +
  ds4ling_bw_theme()

ggsave(
  here("includes", "figures", "f1_f2_p_all.png"),
  f1_f2_p_all,
  width = 20,
  height = 6,
  units = "cm",
  dpi = 600
)

## diff plots

diff_f1 <- plot_diff(
  gam_f1,
  view = "session",
  comp = list(stress = c("unstressed", "stressed"))
) %>%
  mutate(formant = "F1")

diff_f2 <- plot_diff(
  gam_f2,
  view = "session",
  comp = list(stress = c("unstressed", "stressed"))
) %>%
  mutate(formant = "F2")

diff_f1_f2 <- bind_rows(diff_f1, diff_f2)

diff_f1_f2_p <- ggplot(diff_f1_f2, aes(x = session, y = est)) +
  geom_ribbon(
    aes(
      ymin = est - CI,
      ymax = est + CI
    ),
    alpha = 0.2
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line(linewidth = 1.5) +
  facet_grid(formant ~ ., scales = "free_y") +
  labs(
    x = "Session",
    y = "Unstressed - Stressed (Hz)"
  ) +
  ds4ling_bw_theme()

ggsave(
  here("includes", "figures", "f1_f2_p_diff.png"),
  f1_f2_p_all,
  width = 21,
  height = 6,
  units = "cm",
  dpi = 600
)
  
########

# TL plot

gam_tl_preds <- predict_gam(gam_tl,
                             exclude_terms = "s(session,participant)")

gam_tl_preds_re <- predict_gam(gam_tl)

ggplot(
  gam_tl_preds,
  aes(x = session, y = tl, color = stress, fill = stress, linetype = stress)
) +
  geom_ribbon(
    aes(ymin = lower_ci[, 1], ymax = upper_ci[, 1]),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(size = 1.5) +
  labs(y = "TL (Hz)") +
  ds4ling_bw_theme()

ggplot(
  gam_tl_preds_re,
  aes(x = session, y = tl, color = stress, fill = stress, linetype = stress)
) +
  geom_ribbon(
    aes(ymin = lower_ci[, 1], ymax = upper_ci[, 1]),
    alpha = 0.2,
    color = NA
  ) +
  facet_grid(~ participant) +
  geom_line(size = 1.5) +
  labs(y = "TL (Hz)") +
  ds4ling_bw_theme()

# diff plot

diff_tl <- plot_diff(gam_tl, view = "session",
                     comp = list(stress=c("unstressed","stressed")))

ggplot(diff_tl, aes(x = session, y = est)) +
  geom_ribbon(
    aes(
      ymin = est - CI,
      ymax = est + CI
    ),
    alpha = 0.2
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line(linewidth = 1.5) +
  labs(
    x = "Session",
    y = "TL difference (Hz)"
  ) +
  ds4ling_bw_theme()
