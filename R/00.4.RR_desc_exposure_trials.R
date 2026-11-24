# REGISTERED ANALYSES FOR ACTPSY-S-25-05866-R4
# LO
# 2026/11/24
# TASK: DESCRIBE AND ANALYSE ACCURACY ON EXPOSURE TRIALS FOR BOTH SESSIONS

library(dplyr)
readRDS(df_included,"output/interim/df_included.rds")

str(df_included)

# DESCRIBE exposure trials  -----
df_exposure <- df_included  %>%
  filter(phase == "exposure") %>%
           droplevels()

glimpse(df_exposure)

# describe strictly accurate responses (defined as StrictAccuracy in the manuscript), namely those identified as correct in both gender and number
accuracy_exposure <- df_exposure %>%
  group_by(subject, group, session) %>%
  summarise(
    accuracy = mean(correct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(group, session) %>%
  summarise(
    mean_accuracy = mean(accuracy, na.rm = TRUE),
    sd_accuracy = sd(accuracy, na.rm = TRUE),
    se_accuracy = sd_accuracy / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

accuracy_exposure


# # Compute performance per subject in exposure session 1
# 
# exposure_perf <- df_exposure %>%
#   group_by(subject,group,session) %>%
#   summarise(
#     n_trials_exposure  = n(),
#     n_correct_exposure = sum(correct, na.rm = TRUE),
#     prop_correct       = n_correct_exposure / n_trials_exposure,
#     .groups = "drop"
#   )
# 
# View(exposure_perf)
# 
# # PLOT MEAN ACCURACY IN EXPOSURE TRIALS FOR SESSION 1 ----
# library(ggplot2)
# library(dplyr)
# 
# # Summary stats based on df_included (calculated later in 003. script)
# summary_df_sessions <- exposure_perf %>%
#   group_by(group,session) %>%
#   summarise(
#     mean_prop = mean(prop_correct, na.rm = TRUE),
#     sem = sd(prop_correct, na.rm = TRUE) / sqrt(n()),
#     .groups = "drop"
#   )
# summary_df_sessions
# exposure_perf
# # PLOT ----
# ggplot(
#   exposure_perf,
#   aes(
#     x = session,
#     y = prop_correct,
#     colour = group
#   )
# ) +
#   
#   # individual data points
#   geom_jitter(
#     position = position_jitterdodge(
#       jitter.width = 0.1,
#       dodge.width = 0.3
#     ),
#     alpha = 0.7,
#     size = 2
#   ) +
#   
#   # mean points
#   geom_point(
#     data = summary_df_sessions,
#     aes(
#       x = session,
#       y = mean_prop,
#       colour = group
#     ),
#     position = position_dodge(width = 0.3),
#     size = 4,
#     inherit.aes = FALSE
#   ) +
#   
#   # SEM error bars
#   geom_errorbar(
#     data = summary_df_sessions,
#     aes(
#       x = session,
#       ymin = mean_prop - sem,
#       ymax = mean_prop + sem,
#       colour = group
#     ),
#     position = position_dodge(width = 0.3),
#     width = 0.15,
#     linewidth = 1,
#     inherit.aes = FALSE
#   ) +
#   
#   scale_colour_brewer(
#     palette = "Dark2",
#     name = "Group"
#   ) +
#   
#   labs(
#     x = "Session",
#     y = "Proportion Correct",
#     title = "Exposure Performance by Group over Session"
#   ) +
#   
#   theme_classic()
# 
# ## ALTERNATIVE PLOT
# ggplot(df_exposure, aes(x = group, y = correct, colour = group)) +
#   
#   geom_jitter(width = 0.15, alpha = 0.15, size = 1) +
#   
#   stat_summary(fun = mean, geom = "point", size = 4) +
#   
#   stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15) +
#   
#   scale_colour_brewer(palette = "Dark2", guide = "none") +
#   
#   facet_wrap(~ session) +
#   
#   labs(
#     x = "Group",
#     y = "Accuracy (0/1)",
#     title = "Exposure Performance by Group and Session"
#   ) +
#   
#   theme_classic()
# 
# 
# ## RUN LINEAR MODEL ON EXPOSURE TRIALS
# 
# df_exposure
# 
# # Make sure group is a factor
# exposure_s1_perf$group <- factor(exposure_s1_perf$group)
# 
# # Linear model
# model <- lm(prop_correct_s1 ~ group, data = exposure_s1_perf)
# 
# # Model summary
# summary(model)
# 
