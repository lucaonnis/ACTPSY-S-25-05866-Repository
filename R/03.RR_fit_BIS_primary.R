# REGISTERED ANALYSES FOR ACTPSY-S-25-05866-R2
# Luca Onnis
# 2026/10/13
# Secondary Analysis: Balanced Integration Score (BIS)
# Single Multivariate Modeling Framework (brms), p.30-31 of RR manuscript.
# DV1: test item BIS Acc + RTs on Strict Accuracy
# Extra Non-registered:
# DV2: test item BIS Acc + RTs on Gender Accuracy alone

# 1. Check how session and block are coded ----
# Run this first to make sure we can correctly map the eight blocks in each session onto blocks 1–16.
library(dplyr)
library(brms)
library(ggplot2)

df_test <- df_included %>%
  filter(phase == "test") %>%
  droplevels()

glimpse(df_test)

df_test %>%
  distinct(session, block) %>%
  arrange(session, block) %>%
  print(n = Inf)

# 2. Calculate block-level accuracy and RT ----
library(readr)

# Original trial data: block-level accuracy
levels(df_test$subject)


df_bis_accuracy <- df_test %>%
  mutate(
    StrictAccuracy = case_when(
      is.na(gender) | is.na(number) ~ NA_integer_,
      gender == 1 & number == 1 ~ 1L,
      TRUE ~ 0L
    ),
    session_num = readr::parse_number(as.character(session)),
    block_num = readr::parse_number(as.character(block))
  ) %>%
  filter(
    subject != "MN14A",
    !is.na(StrictAccuracy),
    !is.na(subject),
    !is.na(group),
    !is.na(session_num),
    !is.na(block_num)
  ) %>%
  group_by(subject, group, session_num, block_num) %>%
  summarise(
    Acc = mean(StrictAccuracy),
    n_accuracy_trials = n(),
    .groups = "drop"
  )

raw_cells <- df_test %>%
  mutate(
    StrictAccuracy = case_when(
      is.na(gender) | is.na(number) ~ NA_integer_,
      gender == 1 & number == 1 ~ 1L,
      TRUE ~ 0L
    ),
    session_num = readr::parse_number(as.character(session)),
    block_num = readr::parse_number(as.character(block)),
    global_block = case_when(
      session_num == 1 ~ block_num,
      session_num == 2 ~ block_num + 8,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(
    subject != "MN14A",
    !is.na(subject),
    !is.na(group),
    !is.na(global_block)
  ) %>%
  group_by(subject, group, global_block) %>%
  summarise(
    n_raw_trials = n(),
    n_correct = sum(StrictAccuracy == 1, na.rm = TRUE),
    .groups = "drop"
  )

raw_cells

# Preprocessed RT data: mean RT on correct trials
df_bis_rt <- df_rt_preprocessed %>%
  mutate(
    session_num = parse_number(as.character(session)),
    block_num = parse_number(as.character(block))
  ) %>%
  filter(
    StrictAccuracy == 1,
    !is.na(RT),
    !is.na(subject),
    !is.na(group),
    !is.na(session_num),
    !is.na(block_num)
  ) %>%
  group_by(subject, group, session_num, block_num) %>%
  summarise(
    RT_mean = mean(RT),
    n_correct_rt_trials = n(),
    .groups = "drop"
  )

# Combine accuracy and RT for each participant/session/block
df_bis <- df_bis_accuracy %>%
  left_join(
    df_bis_rt,
    by = c("subject", "group", "session_num", "block_num")
  ) %>%
  mutate(
    global_block = case_when(
      session_num == 1 ~ block_num,
      session_num == 2 ~ block_num + 8,
      TRUE ~ NA_real_
    )
  )

glimpse(df_bis_accuracy)


stopifnot(
  all(df_bis$session_num %in% c(1, 2)),
  all(df_bis$block_num %in% 1:8),
  all(df_bis$global_block %in% 1:16)
)

df_bis %>%
  distinct(session_num, block_num, global_block) %>%
  arrange(global_block)

df_bis %>%
  summarise(
    n_observations = n(),
    n_participants = n_distinct(subject),
    n_control = n_distinct(subject[group == "control"]),
    n_experimental = n_distinct(subject[group == "experimental"]),
    n_blocks = n_distinct(global_block),
    min_block = min(global_block),
    max_block = max(global_block)
  )


# Check the block-level data ----
# Check the number of cells and the trial counts
df_bis %>%
  summarise(
    n_cells = n(),
    n_missing_accuracy = sum(is.na(Acc)),
    n_missing_rt = sum(is.na(RT_mean)),
    n_participants = n_distinct(subject)
  )

# Check each group across all 16 blocks
df_bis %>%
  count(group, global_block) %>%
  arrange(group, global_block) %>%
  print(n = Inf)

# Check whether the two sessions have the intended eight blocks each
df_bis %>%
  distinct(session_num, block_num, global_block) %>%
  arrange(global_block)

# 3. Standardize accuracy and RT and calculate BIS ----
# Keep participant-session-block cells with both measures available
df_bis <- df_bis %>%
  filter(
    is.finite(Acc),
    is.finite(RT_mean),
    is.finite(global_block)
  )

# Standardize across aggregated participant-session-block cells
mu_acc <- mean(df_bis$Acc)
sd_acc <- sd(df_bis$Acc)

mu_rt <- mean(df_bis$RT_mean)
sd_rt <- sd(df_bis$RT_mean)

stopifnot(sd_acc > 0, sd_rt > 0)

df_bis <- df_bis %>%
  mutate(
    Z_Acc = (Acc - mu_acc) / sd_acc,
    Z_RT  = (RT_mean - mu_rt) / sd_rt,
    BIS   = Z_Acc - Z_RT
  )

# Verify standardization
df_bis %>%
  summarise(
    mean_Z_Acc = mean(Z_Acc),
    sd_Z_Acc   = sd(Z_Acc),
    mean_Z_RT  = mean(Z_RT),
    sd_Z_RT    = sd(Z_RT),
    mean_BIS   = mean(BIS),
    n_cells    = n()
  )

# 4. Prepare the model predictors -----
# Use centered block numbers, from 1–16, centered at 8.5. 
# This makes the intercept represent the expected BIS at the midpoint of the block sequence, averaged over the group reference level, rather than at a nonexistent Block 0.

df_bis <- df_bis %>%
  mutate(
    group = relevel(factor(group), ref = "control"),
    block_c = global_block - 8.5
  )

# Verify group coding and block coverage
levels(df_bis$group)


df_bis %>%
  summarise(
    min_block = min(global_block),
    max_block = max(global_block),
    min_block_c = min(block_c),
    max_block_c = max(block_c)
  )

# 5. Fit the registered Bayesian multilevel model ----

# Priors on the BIS scale, not the log-RT scale
priors_bis <- c(
  brms::prior(normal(0, 1), class = "b"),
  brms::prior(normal(0, 1.5), class = "Intercept"),
  brms::prior(student_t(3, 0, 2.5), class = "sd"),
  brms::prior(student_t(3, 0, 2.5), class = "sigma"),
  brms::prior(lkj(2), class = "cor")
)

brms::get_prior(
  BIS ~ group * block_c + (1 + block_c | subject),
  data = df_bis,
  family = gaussian()
)

model_bis <- brms::bf(
  BIS ~ group * block_c + (1 + block_c | subject)
)

fit_bis <- brms::brm(
  formula = model_bis,
  data = df_bis,
  family = gaussian(),
  prior = priors_bis,
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 1000,
  seed = 2030,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 13
  )
)

# Add posterior summaries of the learning trajectories -----

# Extract posterior draws
draws_bis <- posterior::as_draws_df(fit_bis)

# Inspect coefficient names
names(draws_bis)

# Group-specific slopes and changes across the 16 blocks
# Check the coefficient names above before running this section.

interaction_draws <- draws_bis %>%
  transmute(
    slope_control = b_block_c,
    slope_experimental =
      b_block_c + `b_groupexperimental:block_c`,
    difference_in_slopes =
      `b_groupexperimental:block_c`,
    change_control = 15 * b_block_c,
    change_experimental =
      15 * (b_block_c + `b_groupexperimental:block_c`),
    difference_in_change =
      15 * `b_groupexperimental:block_c`
  )

# Summarise posterior distributions
summarise_bis_contrast <- function(x) {
  tibble(
    median = median(x),
    lower_95 = quantile(x, 0.025),
    upper_95 = quantile(x, 0.975),
    prob_positive = mean(x > 0)
  )
}

bis_trajectory_summary <- bind_rows(
  slope_control = summarise_bis_contrast(
    interaction_draws$slope_control
  ),
  slope_experimental = summarise_bis_contrast(
    interaction_draws$slope_experimental
  ),
  difference_in_slopes = summarise_bis_contrast(
    interaction_draws$difference_in_slopes
  ),
  change_control = summarise_bis_contrast(
    interaction_draws$change_control
  ),
  change_experimental = summarise_bis_contrast(
    interaction_draws$change_experimental
  ),
  difference_in_change = summarise_bis_contrast(
    interaction_draws$difference_in_change
  ),
  .id = "contrast"
)

bis_trajectory_summary


# 6. Check convergence and model fit -----
# # Convergence and effective sample sizes
summary(fit_bis)

# Number of divergent transitions
np_bis <- brms::nuts_params(fit_bis)

sum(
  np_bis$Parameter == "divergent__" &
    np_bis$Value == 1
)

sum(
  np_bis$Parameter == "divergent__" &
    np_bis$Value == 1
)

# Maximum tree depth reached
max(np_bis$Value[np_bis$Parameter == "treedepth__"])

# Posterior predictive check
brms::pp_check(fit_bis)



# Confirm the corrected BIS dataset
nrow(df_bis)
dplyr::n_distinct(df_bis$subject)

# Confirm that StrictAccuracy was calculated from gender and number
df_bis %>%
  summarise(
    n_observations = n(),
    n_subjects = n_distinct(subject),
    n_missing_BIS = sum(!is.finite(BIS))
  )

raw_cells <- df_test %>%
  mutate(
    StrictAccuracy = case_when(
      is.na(gender) | is.na(number) ~ NA_integer_,
      gender == 1 & number == 1 ~ 1L,
      TRUE ~ 0L
    ),
    session_num = readr::parse_number(as.character(session)),
    block_num = readr::parse_number(as.character(block))
  ) %>%
  filter(
    subject != "MN14A",
    !is.na(subject),
    !is.na(group),
    !is.na(session_num),
    !is.na(block_num)
  ) %>%
  group_by(subject, group, session_num, block_num) %>%
  summarise(
    n_accuracy_trials = sum(!is.na(StrictAccuracy)),
    n_correct = sum(StrictAccuracy == 1, na.rm = TRUE),
    .groups = "drop"
  )

# Check the corrected cell counts
raw_cells %>%
  summarise(
    total_cells = n(),
    cells_with_no_correct = sum(n_correct == 0),
    cells_with_accuracy_data = sum(n_accuracy_trials > 0)
  )

raw_cells %>%
  mutate(
    global_block = if_else(session_num == 1, block_num, block_num + 8)
  ) %>%
  left_join(
    rt_cells %>%
      distinct(subject, group, global_block) %>%
      mutate(has_eligible_rt = TRUE),
    by = c("subject", "group", "global_block")
  ) %>%
  mutate(
    has_eligible_rt = coalesce(has_eligible_rt, FALSE),
    missing_reason = case_when(
      n_correct == 0 ~ "No correct trials",
      !has_eligible_rt ~ "No eligible correct-trial RT",
      TRUE ~ "Included in BIS"
    )
  ) %>%
  count(missing_reason)

# Check which blocks and participants are missing: ----
# Number of observations by group and block
df_bis %>%
  count(group, global_block) %>%
  tidyr::complete(
    group,
    global_block = 1:16,
    fill = list(n = 0)
  ) %>%
  arrange(group, global_block) %>%
  print(n = Inf)

# Number of observed blocks per participant
df_bis %>%
  group_by(subject, group) %>%
  summarise(
    n_blocks = n_distinct(global_block),
    .groups = "drop"
  ) %>%
  count(group, n_blocks) %>%
  arrange(group, n_blocks)

# 1. Check whether blocks are missing from the raw data or excluded during BIS construction
# Run this diagnostic code:
  
library(dplyr)
library(tidyr)
library(readr)

# Count blocks represented in the original trial data
raw_blocks <- df_test %>%
  mutate(
    session_num = parse_number(as.character(session)),
    block_num = parse_number(as.character(block)),
    global_block = case_when(
      session_num == 1 ~ block_num,
      session_num == 2 ~ block_num + 8,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(subject), !is.na(global_block)) %>%
  distinct(subject, group, global_block) %>%
  count(group, name = "raw_blocks_per_group") # overall count; inspect below

# How many participant-blocks exist in raw data?
raw_blocks %>% print(n = Inf)

# Compare raw blocks with blocks retained in the BIS dataset
raw_cells <- df_test %>%
  mutate(
    StrictAccuracy = as.integer(correct),
    session_num = parse_number(as.character(session)),
    block_num = parse_number(as.character(block)),
    global_block = case_when(
      session_num == 1 ~ block_num,
      session_num == 2 ~ block_num + 8,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(subject), !is.na(group), !is.na(global_block)) %>%
  group_by(subject, group, global_block) %>%
  summarise(
    n_raw_trials = n(),
    n_correct = sum(StrictAccuracy == 1, na.rm = TRUE),
    .groups = "drop"
  )

rt_cells <- df_rt_preprocessed %>%
  mutate(
    session_num = parse_number(as.character(session)),
    block_num = parse_number(as.character(block)),
    global_block = case_when(
      session_num == 1 ~ block_num,
      session_num == 2 ~ block_num + 8,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(
    StrictAccuracy == 1,
    !is.na(subject), !is.na(group), !is.na(RT),
    !is.na(global_block)
  ) %>%
  group_by(subject, group, global_block) %>%
  summarise(n_correct_rt = n(), .groups = "drop")

diagnostic <- raw_cells %>%
  left_join(rt_cells, by = c("subject", "group", "global_block")) %>%
  mutate(
    n_correct_rt = replace_na(n_correct_rt, 0L),
    has_correct_rt = n_correct_rt > 0
  )

diagnostic %>%
  summarise(
    participant_blocks_in_raw = n(),
    blocks_with_no_correct_trials = sum(n_correct == 0),
    blocks_with_correct_trials_but_no_eligible_rt =
      sum(n_correct > 0 & !has_correct_rt),
    blocks_with_eligible_rt = sum(has_correct_rt)
  )

diagnostic %>%
  filter(!has_correct_rt) %>%
  count(group, global_block, name = "missing_rt_cells") %>%
  complete(group, global_block = 1:16,
           fill = list(missing_rt_cells = 0)) %>%
  arrange(group, global_block) %>%
  print(n = Inf)

# Quantify the proportion of blocks with no correct responses by group and block. ----
# This will show whether missingness is concentrated in particular parts of the learning sequence.

diagnostic %>%
  group_by(group, global_block) %>%
  summarise(
    n_participants = n(),
    n_zero_correct = sum(n_correct == 0),
    prop_zero_correct = mean(n_correct == 0),
    n_no_eligible_rt = sum(!has_correct_rt),
    .groups = "drop"
  ) %>%
  arrange(group, global_block) %>%
  print(n = Inf)


# Trace plots for key parameters
plot(
  fit_bis,
  variable = c(
    "b_groupExperimental",
    "b_block_c",
    "b_groupExperimental:block_c"
  )
)

# 7. Plot the estimated BIS trajectory for each group ----
# This plot shows the model-estimated BIS across all 16 blocks, with 95% credible intervals. It is often more informative than interpreting the interaction coefficient alone.
new_bis <- expand.grid(
  global_block = 1:16,
  group = levels(df_bis$group)
) %>%
  mutate(
    group = factor(group, levels = levels(df_bis$group)),
    block_c = global_block - 8.5
  )

# Population-level predictions, excluding participant-specific effects
pred_bis <- brms::posterior_epred(
  fit_bis,
  newdata = new_bis,
  re_formula = NA
)

new_bis$BIS_est <- apply(pred_bis, 2, median)
new_bis$BIS_low <- apply(
  pred_bis, 2, quantile, probs = 0.025
)
new_bis$BIS_high <- apply(
  pred_bis, 2, quantile, probs = 0.975
)

p_bis <- ggplot(
  new_bis,
  aes(
    x = global_block,
    y = BIS_est,
    colour = group,
    fill = group,
    group = group
  )
) +
  geom_ribbon(
    aes(ymin = BIS_low, ymax = BIS_high),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_vline(
    xintercept = 8.5,
    linetype = "dashed",
    colour = "grey50"
  ) +
  scale_x_continuous(
    breaks = 1:16,
    labels = 1:16
  ) +
  labs(
    x = "Training block",
    y = "Balanced Integration Score (BIS)",
    colour = "Group",
    fill = "Group"
  ) +
  theme_minimal(base_size = 13)

print(p_bis)

ggsave(
  "figures/BIS_model_trajectory.pdf",
  plot = p_bis,
  width = 8,
  height = 5
)

# PLOT MODEL LINE + OBSERVED DATA ----
p_bis_observed <- ggplot() +
  geom_point(
    data = df_bis,
    aes(x = global_block, y = BIS, colour = group),
    alpha = 0.2,
    position = position_jitter(width = 0.08, height = 0),
    size = 1.3
  ) +
  geom_ribbon(
    data = new_bis,
    aes(
      x = global_block,
      ymin = BIS_low,
      ymax = BIS_high,
      fill = group
    ),
    alpha = 0.15
  ) +
  geom_line(
    data = new_bis,
    aes(x = global_block, y = BIS_est, colour = group),
    linewidth = 0.9
  ) +
  geom_vline(
    xintercept = 8.5,
    linetype = "dashed",
    colour = "grey50"
  ) +
  scale_x_continuous(breaks = 1:16) +
  labs(
    x = "Training block",
    y = "Balanced Integration Score (BIS)",
    colour = "Group",
    fill = "Group"
  ) +
  theme_minimal(base_size = 13)

p_bis_observed

ggsave(
  "figures/BIS_model_trajectory_observed.pdf",
  plot = p_bis_observed,
  width = 8,
  height = 5
)


# 8. Extract the posterior estimates for the group difference in trajectory ----
draws_bis <- posterior::as_draws_df(fit_bis)

# Inspect the actual coefficient names
names(draws_bis)


# Find the name corresponding to groupexperimental:block_c in the output, then summarize it:
posterior::summarise_draws(
  draws_bis,
  "mean",
  "sd",
  "median",
  ~ quantile(.x, 0.025),
  ~ quantile(.x, 0.975)
) %>%
  filter(grepl("groupexperimental:block_c", variable, fixed = TRUE))