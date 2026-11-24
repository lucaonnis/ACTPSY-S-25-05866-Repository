# Exploratory, non-registered analysis FOR ACTPSY-S-25-05866-R3
# Luca Onnis
# 2026/11/20

# Accuracy by Group, Session, and Agreement type

# 1. Prepare the data -------------

# Packages
library(dplyr)
library(tidyr)
library(brms)
library(posterior)
library(ggplot2)

# Prepare test-trial data
df_agreement <- df_test %>%
  filter(
    phase == "test",
    subject != "MN14A"
  ) %>%
  mutate(
    trial_id = factor(row_number()),
    subject = factor(subject),
    item = factor(item),
    group = factor(group, levels = c("control", "experimental")),
    session = factor(as.character(session), levels = c("1", "2"))
  ) %>%
  pivot_longer(
    cols = c(gender, number),
    names_to = "agreement_type",
    values_to = "accuracy"
  ) %>%
  mutate(
    agreement_type = factor(
      agreement_type,
      levels = c("gender", "number"),
      labels = c("Gender", "Number")
    ),
    accuracy = as.integer(accuracy)
  ) %>%
  filter(
    !is.na(accuracy),
    !is.na(group),
    !is.na(session),
    !is.na(subject),
    !is.na(item)
  )

# Check that accuracy is coded as 0/1
stopifnot(all(df_agreement$accuracy %in% c(0L, 1L)))

# Inspect the resulting dataset
glimpse(df_agreement)
length(levels(df_agreement$subject)) # 75

# Number of observations and participants
df_agreement %>%
  summarise(
    n_observations = n(),
    n_participants = n_distinct(subject),
    n_items = n_distinct(item),
    n_trials = n_distinct(trial_id)
  )

# Observations by group, session, and agreement type
df_agreement %>%
  count(group, session, agreement_type)



# 2. Descriptive accuracy -----------------------------------
descriptive_accuracy <- df_agreement %>%
  group_by(group, session, agreement_type) %>%
  summarise(
    n = n(),
    accuracy = mean(accuracy),
    .groups = "drop"
  )

descriptive_accuracy


ggplot(
  descriptive_accuracy,
  aes(
    x = session,
    y = accuracy,
    colour = group,
    group = group
  )
) +
  geom_point(position = position_dodge(width = 0.1), size = 3) +
  geom_line(position = position_dodge(width = 0.1)) +
  facet_wrap(~ agreement_type) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  labs(
    x = "Session",
    y = "Proportion correct",
    colour = "Group"
  ) +
  theme_classic()

# 3. Fit the Bayesian mixed-effects logistic regression -----------------------------------

# Model priors
priors_agreement <- c(
  set_prior("normal(0, 1)", class = "b"),
  set_prior("normal(0, 1.5)", class = "Intercept"),
  set_prior("student_t(3, 0, 2.5)", class = "sd"),
  set_prior("lkj(2)", class = "cor")
)

# Fit model
fit_agreement <- brm(
  formula = accuracy ~
    group * session * agreement_type +
    (1 + session * agreement_type | subject) +
    (1 | item) +
    (1 | trial_id),
  
  data = df_agreement,
  family = bernoulli(link = "logit"),
  prior = priors_agreement,
  
  chains = 4,
  iter = 4000,
  warmup = 1000,
  cores = 4,
  
  seed = 2031,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 13
  )
)

saveRDS(fit_agreement,"output/final/fit_agreement.rds")
# 4. Inspect model estimates -----------------------------------

summary(fit_agreement)

fixef(fit_agreement)["groupexperimental:session2:agreement_typeNumber", ]


# 5. Convergence and model diagnostics -----------------------------------

# Rhat and effective sample sizes
summary(fit_agreement)

# Number of divergent transitions
np_agreement <- brms::nuts_params(fit_agreement)

sum(
  np_agreement$Parameter == "divergent__" &
    np_agreement$Value == 1
)

# Maximum tree depth reached
max(
  np_agreement$Value[
    np_agreement$Parameter == "treedepth__"
  ]
)

# Posterior predictive check
pp_check(fit_agreement)

# Trace plots
plot(fit_agreement)

# 6. Estimate predicted accuracy for each condition -----------------------------------
# Create prediction grid
prediction_grid <- expand_grid(
  group = factor(
    c("control", "experimental"),
    levels = levels(df_agreement$group)
  ),
  session = factor(
    c("1", "2"),
    levels = levels(df_agreement$session)
  ),
  agreement_type = factor(
    c("Gender", "Number"),
    levels = levels(df_agreement$agreement_type)
  )
) %>%
  mutate(
    subject = factor(
      levels(df_agreement$subject)[1],
      levels = levels(df_agreement$subject)
    ),
    item = factor(
      levels(df_agreement$item)[1],
      levels = levels(df_agreement$item)
    ),
    trial_id = factor(
      levels(df_agreement$trial_id)[1],
      levels = levels(df_agreement$trial_id)
    )
  )

# Obtain posterior predicted probabilities
agreement_epred <- posterior_epred(
  fit_agreement,
  newdata = prediction_grid,
  re_formula = NA
)

# Summarise posterior predictions
agreement_predictions <- prediction_grid %>%
  mutate(
    median = apply(agreement_epred, 2, median),
    lower_95 = apply(
      agreement_epred, 2, quantile, probs = 0.025
    ),
    upper_95 = apply(
      agreement_epred, 2, quantile, probs = 0.975
    )
  )

agreement_predictions %>%
  select(
    group, session, agreement_type,
    median, lower_95, upper_95
  )



# 7. Calculate the three-way interaction on the probability scale -----------------------------------
# Helper function to identify prediction-grid columns
get_prediction_index <- function(g, s, a) {
  which(
    prediction_grid$group == g &
      prediction_grid$session == s &
      prediction_grid$agreement_type == a
  )
}

# Identify the eight prediction columns
idx <- list(
  ctrl_s1_gender = get_prediction_index("control", "1", "Gender"),
  ctrl_s2_gender = get_prediction_index("control", "2", "Gender"),
  exp_s1_gender = get_prediction_index("experimental", "1", "Gender"),
  exp_s2_gender = get_prediction_index("experimental", "2", "Gender"),
  
  ctrl_s1_number = get_prediction_index("control", "1", "Number"),
  ctrl_s2_number = get_prediction_index("control", "2", "Number"),
  exp_s1_number = get_prediction_index("experimental", "1", "Number"),
  exp_s2_number = get_prediction_index("experimental", "2", "Number")
)

# Calculate posterior draws of the three-way contrast
gender_group_change <-
  (agreement_epred[, idx$exp_s2_gender] -
     agreement_epred[, idx$exp_s1_gender]) -
  (agreement_epred[, idx$ctrl_s2_gender] -
     agreement_epred[, idx$ctrl_s1_gender])

number_group_change <-
  (agreement_epred[, idx$exp_s2_number] -
     agreement_epred[, idx$exp_s1_number]) -
  (agreement_epred[, idx$ctrl_s2_number] -
     agreement_epred[, idx$ctrl_s1_number])

three_way_draws <- gender_group_change - number_group_change

# Summarise posterior contrast
three_way_summary <- tibble(
  median = median(three_way_draws),
  lower_95 = quantile(three_way_draws, 0.025),
  upper_95 = quantile(three_way_draws, 0.975),
  prob_positive = mean(three_way_draws > 0),
  prob_negative = mean(three_way_draws < 0)
)

three_way_summary

# Test the overall Gender–Number accuracy difference -----------------------
# Find the prediction columns for each agreement type
gender_idx <- which(prediction_grid$agreement_type == "Gender")
number_idx <- which(prediction_grid$agreement_type == "Number")

# Average across the four Group x Session conditions,
# separately for Gender and Number, within each posterior draw
gender_mean <- rowMeans(
  agreement_epred[, gender_idx, drop = FALSE]
)

number_mean <- rowMeans(
  agreement_epred[, number_idx, drop = FALSE]
)

# Gender minus Number
gender_number_diff <- gender_mean - number_mean

overall_difference <- tibble(
  median = median(gender_number_diff),
  lower_95 = quantile(gender_number_diff, 0.025),
  upper_95 = quantile(gender_number_diff, 0.975),
  prob_positive = mean(gender_number_diff > 0),
  prob_negative = mean(gender_number_diff < 0)
)

overall_difference

# 1. Gender and Number against chance ------------------------
# We use the posterior draws from the existing joint accuracy model (fit_accuracy_full) to test performance against chance, without fitting another model.

library(brms)
library(dplyr)
library(tidyr)
library(tibble)

# Four Group x Session conditions
prediction_grid <- expand_grid(
  group = factor(
    c("control", "experimental"),
    levels = c("control", "experimental")
  ),
  session = factor(
    c("1", "2"),
    levels = c("1", "2")
  ),
  agreement_type = factor(
    c("Gender", "Number"),
    levels = c("Gender", "Number")
  )
)

# Set arbitrary factor levels for the random-effect grouping variables.
# Random effects are set to zero below, so these are not used.
prediction_grid <- prediction_grid %>%
  mutate(
    subject = df_agreement$subject[1],
    item = df_agreement$item[1],
    trial_id = df_agreement$trial_id[1]
  )

# Population-level predictions
agreement_epred <- posterior_epred(
  fit_agreement,
  newdata = prediction_grid,
  re_formula = NA
)

# Summarise each condition against 50% chance
chance_results <- lapply(
  seq_len(nrow(prediction_grid)),
  function(j) {
    
    draws <- agreement_epred[, j]
    
    tibble(
      group = prediction_grid$group[j],
      session = prediction_grid$session[j],
      agreement_type = prediction_grid$agreement_type[j],
      median = median(draws),
      lower_95 = quantile(draws, 0.025),
      upper_95 = quantile(draws, 0.975),
      prob_above_chance = mean(draws > 0.50)
    )
  }
) %>%
  bind_rows()

chance_results


# Strict Accuracy against chance ---------------

fit_accuracy_full <- readRDS("output/final/fit_accuracy_full.rds")


library(brms)
library(dplyr)
library(tidyr)
library(tibble)

# 1. Create the four Group x Session conditions
strict_grid <- expand_grid(
  group = factor(
    c("control", "experimental"),
    levels = c("control", "experimental")
  ),
  session = factor(
    c("1", "2"),
    levels = c("1", "2")
  )
)

# 2. Add the variables required by the fitted model.
# Random effects will be set to zero.
strict_grid <- strict_grid %>%
  mutate(
    subject = fit_accuracy_full$data$subject[1],
    session_item = fit_accuracy_full$data$session_item[1]
  )

# 3. Obtain posterior predictions for both outcomes
gg_epred <- posterior_epred(
  fit_accuracy_full,
  newdata = strict_grid,
  resp = "GG",
  re_formula = NA
)

nn_epred <- posterior_epred(
  fit_accuracy_full,
  newdata = strict_grid,
  resp = "NN",
  re_formula = NA
)

gg_epred <- as.matrix(gg_epred)
nn_epred <- as.matrix(nn_epred)

stopifnot(identical(dim(gg_epred), dim(nn_epred)))

# 4. Calculate the joint probability of both responses being correct
# This follows the conditional-independence specification
# of the fitted model (set_rescor(FALSE)).
strict_epred <- gg_epred * nn_epred

# 5. Summarise Strict Accuracy against chance = 0.25
strict_chance_results <- lapply(
  seq_len(nrow(strict_grid)),
  function(j) {
    
    draws <- strict_epred[, j]
    
    tibble(
      group = strict_grid$group[j],
      session = strict_grid$session[j],
      median = median(draws),
      lower_95 = quantile(draws, 0.025),
      upper_95 = quantile(draws, 0.975),
      prob_above_chance = mean(draws > 0.25),
      prob_below_chance = mean(draws < 0.25)
    )
  }
) %>%
  bind_rows()

strict_schance_results
