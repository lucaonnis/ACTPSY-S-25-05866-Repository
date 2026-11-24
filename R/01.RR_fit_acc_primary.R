# ============================================================
# ACCURACY ANALYSES
# Revised multivariate Bayesian model
# ============================================================

# 1. Load packages --------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(brms)
library(bridgesampling)
library(posterior)
library(bayesplot)
library(ggplot2)
packageVersion("brms")

# 2. Prepare accuracy dataset ---------------------------------

df_test <- readRDS("output/final/df_test.rds")
df_test <- droplevels(df_test)
length(levels(df_test$subject)) # 76
length(levels(df_accuracy$subject))

df_accuracy <- df_test %>%
  mutate(
    GG = as.integer(as.character(gender)),
    NN = as.integer(as.character(number)),
    
    group = factor(
      group,
      levels = c("control", "experimental")
    ),
    
    session = factor(
      session,
      levels = c("1", "2")
    ),
    
    subject = factor(subject),
    item = factor(item),
    
    # Item nested within session
    session_item = interaction(
      session,
      item,
      drop = TRUE,
      sep = ":"
    )
  ) %>%
  filter(
    !is.na(GG),
    !is.na(NN),
    !is.na(group),
    !is.na(session),
    !is.na(subject),
    !is.na(item)
  ) %>%
  droplevels()

# Check that both outcomes are binary
stopifnot(all(df_accuracy$GG %in% c(0L, 1L)))
stopifnot(all(df_accuracy$NN %in% c(0L, 1L)))

# Participants in each dataset
subjects_test <- unique(as.character(df_test$subject))
subjects_accuracy <- unique(as.character(df_accuracy$subject))

# Participant(s) present in df_test but absent from df_accuracy
setdiff(subjects_test, subjects_accuracy) # MN14A

# MN14A is present in df_test but absent from df_accuracy. Why?

df_test %>%
  filter(as.character(subject) == "MN14A") %>%
  summarise(
    n_trials = n(),
    missing_gender = sum(is.na(gender)),
    missing_number = sum(is.na(number)),
    missing_group = sum(is.na(group)),
    missing_session = sum(is.na(session)),
    missing_subject = sum(is.na(subject)),
    missing_item = sum(is.na(item)),
    complete_accuracy_trials = sum(
      !is.na(gender) &
        !is.na(number) &
        !is.na(group) &
        !is.na(session) &
        !is.na(subject) &
        !is.na(item)
    )
  )

df_test %>%
  filter(as.character(subject) == "MN14A") %>%
  count(gender, number, .drop = FALSE)

# Inspect sample size
cat("Number of observations:", nrow(df_accuracy), "\n")
cat("Number of participants:", n_distinct(df_accuracy$subject), "\n")
cat("Number of items:", n_distinct(df_accuracy$item), "\n")

# Check observations by group and session
df_accuracy %>%
  count(group, session)

# Check missingness in the original outcomes
cat("Missing gender scores:", sum(is.na(df_test$gender)), "\n")
cat("Missing number scores:", sum(is.na(df_test$number)), "\n")

# Check Strict Accuracy against the original correct variable
df_accuracy <- df_accuracy %>%
  mutate(
    StrictAccuracy = as.integer(GG == 1 & NN == 1)
  )

saveRDS(df_accuracy, "output/final/df_accuracy.rds")

# 3. Specify full and reduced models ---------------------------
# This is the revised random-effects structure:
#   - Subject: random intercept and Session slope.
# - Item-within-session: random intercept.
# - Cross-outcome correlations: explicitly specified using |p| and |q|.

# Full model: includes Group x Session interaction

bf_GG_full <- brms::bf(
  GG ~ group * session +
    (1 + session |p| subject) +
    (1 |q| session_item),
  family = brms::bernoulli()
)

bf_NN_full <- brms::bf(
  NN ~ group * session +
    (1 + session |p| subject) +
    (1 |q| session_item),
  family = brms::bernoulli()
)

formula_full <- bf_GG_full +
  bf_NN_full +
  brms::set_rescor(FALSE)


# Reduced model: excludes Group x Session interaction

bf_GG_null <- brms::bf(
  GG ~ group + session +
    (1 + session |p| subject) +
    (1 |q| session_item),
  family = brms::bernoulli()
)

bf_NN_null <- brms::bf(
  NN ~ group + session +
    (1 + session |p| subject) +
    (1 |q| session_item),
  family = brms::bernoulli()
)

# Combine the response formulas.
# Residual correlations are not estimated for Bernoulli outcomes;
# dependence is represented through the correlated group-level effects.

formula_full <- bf_GG_full +
  bf_NN_full +
  set_rescor(FALSE)

formula_null <- bf_GG_null +
  bf_NN_null +
  set_rescor(FALSE)

# 4. Priors ----------------------------------------------------

make_accuracy_priors <- function(scale = 0.5) {
  c(
    # Gender outcome: fixed effects and intercept
    brms::set_prior(
      paste0("normal(0, ", scale, ")"),
      class = "b", resp = "GG"
    ),
    brms::set_prior(
      paste0("normal(0, ", scale, ")"),
      class = "Intercept", resp = "GG"
    ),
    
    # Number outcome: fixed effects and intercept
    brms::set_prior(
      paste0("normal(0, ", scale, ")"),
      class = "b", resp = "NN"
    ),
    brms::set_prior(
      paste0("normal(0, ", scale, ")"),
      class = "Intercept", resp = "NN"
    ),
    
    # Random-effect SDs, specified separately by outcome
    brms::set_prior(
      "student_t(3, 0, 2.5)",
      class = "sd", resp = "GG"
    ),
    brms::set_prior(
      "student_t(3, 0, 2.5)",
      class = "sd", resp = "NN"
    ),
    
    # Correlation prior
    brms::set_prior("lkj(1)", class = "cor")
  )
}

priors_accuracy <- make_accuracy_priors(scale = 0.5)


# Inspect the prior specification
#get_prior(
#  formula_full,
#  data = df_accuracy
#)

# Inspect the actual prior assignments
priors_accuracy

# 5. Fit primary models ----------------------------------------

# Full model
fit_accuracy_full <- brms::brm(
  formula = formula_full,
  data = df_accuracy,
  family = NULL,
  prior = priors_accuracy,
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 1000,
  
  seed = 2026,
  
  control = list(
    adapt_delta = 0.95,
    max_treedepth = 12
  ),
  
  save_pars = save_pars(all = TRUE)
)

default_priors <- brms::get_prior(
  formula = formula_full,
  data = df_accuracy
)

print(default_priors)

# Reduced model
fit_accuracy_null <- brm(
  formula = formula_null,
  data = df_accuracy,
  family = NULL,
  prior = priors_accuracy,
  
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 1000,
  
  seed = 2027,
  
  control = list(
    adapt_delta = 0.95,
    max_treedepth = 12
  ),
  
  save_pars = save_pars(all = TRUE)
)

# Save fitted models
saveRDS(
  fit_accuracy_full,
  "output/final/fit_accuracy_full.rds"
)

fit_accuracy_full <- readRDS("output/final/fit_accuracy_full.rds")

saveRDS(
  fit_accuracy_null,
  "output/final/fit_accuracy_null.rds"
)

fit_accuracy_null <- readRDS("output/final/fit_accuracy_null.rds")

# 6. Bayes factor comparison -----------------------------------

BF_accuracy <- bayes_factor(
  fit_accuracy_full,
  fit_accuracy_null
)

print(BF_accuracy)

saveRDS(
  BF_accuracy,
  "output/final/BF_accuracy.rds"
)

BF_accuracy <- readRDS("output/final/BF_accuracy.rds")

# 7. Prior robustness ------------------------------------------

# Fit models for alternative prior scales

fit_accuracy_prior_scale <- function(scale, seed_full, seed_null) {
  
  priors <- make_accuracy_priors(scale)
  
  fit_full <- brm(
    formula = formula_full,
    data = df_accuracy,
    family = NULL,
    prior = priors,
    
    chains = 4,
    cores = 4,
    iter = 4000,
    warmup = 1000,
    
    seed = seed_full,
    
    control = list(
      adapt_delta = 0.95,
      max_treedepth = 12
    ),
    
    save_pars = save_pars(all = TRUE)
  )
  
  fit_null <- brm(
    formula = formula_null,
    data = df_accuracy,
    family = NULL,
    prior = priors,
    
    chains = 4,
    cores = 4,
    iter = 4000,
    warmup = 1000,
    
    seed = seed_null,
    
    control = list(
      adapt_delta = 0.95,
      max_treedepth = 12
    ),
    
    save_pars = save_pars(all = TRUE)
  )
  
  list(
    full = fit_full,
    null = fit_null,
    BF = bayes_factor(fit_full, fit_null)
  )
}

# Alternative prior scales
robust_03 <- fit_accuracy_prior_scale(
  scale = 0.3,
  seed_full = 2030,
  seed_null = 2031
)

robust_10 <- fit_accuracy_prior_scale(
  scale = 1.0,
  seed_full = 2040,
  seed_null = 2041
)


# Save robustness models
saveRDS(robust_03, "output/final/accuracy_prior_robustness_03.rds")
saveRDS(robust_10, "output/final/accuracy_prior_robustness_10.rds")

# Load robustness models
robust_03 <- readRDS("output/final/accuracy_prior_robustness_03.rds")
robust_10 <- readRDS("output/final/accuracy_prior_robustness_10.rds")

# Compare Bayes factors
cat("Prior SD = 0.3\n")
print(robust_03$BF)

cat("Prior SD = 0.5\n")
print(BF_accuracy)

cat("Prior SD = 1.0\n")
print(robust_10$BF)


# 8. Model summaries -------------------------------------------

summary(fit_accuracy_full)
summary(fit_accuracy_null)

# 9. Divergent transitions -------------------------------------

nuts_params(fit_accuracy_full) %>%
  filter(Parameter == "divergent__") %>%
  summarise(
    n_divergent = sum(Value)
  )

nuts_params(fit_accuracy_null) %>%
  filter(Parameter == "divergent__") %>%
  summarise(
    n_divergent = sum(Value)
  )

# 10. Maximum treedepth ----------------------------------------

nuts_params(fit_accuracy_full) %>%
  filter(Parameter == "treedepth__") %>%
  summarise(
    max_treedepth = max(Value)
  )

nuts_params(fit_accuracy_null) %>%
  filter(Parameter == "treedepth__") %>%
  summarise(
    max_treedepth = max(Value)
  )



# 11. Posterior predictive checks ------------------------------

# Gender accuracy
pp_check(
  fit_accuracy_full,
  resp = "GG",
  type = "bars"
)

# Number accuracy
pp_check(
  fit_accuracy_full,
  resp = "NN",
  type = "bars"
)

# Reduced model
pp_check(
  fit_accuracy_null,
  resp = "GG",
  type = "bars"
)

pp_check(
  fit_accuracy_null,
  resp = "NN",
  type = "bars"
)

# 12. Strict Accuracy: joint posterior probability -------------

# Posterior expected probabilities for each outcome
gg_epred <- posterior_epred(
  fit_accuracy_full,
  resp = "GG",
  re_formula = NULL
)

nn_epred <- posterior_epred(
  fit_accuracy_full,
  resp = "NN",
  re_formula = NULL
)

# Convert to matrices if necessary
gg_epred <- as.matrix(gg_epred)
nn_epred <- as.matrix(nn_epred)

# Verify dimensions
stopifnot(
  identical(dim(gg_epred), dim(nn_epred))
)

# Joint probability of both gender and number being correct
strict_prob_draws <- gg_epred * nn_epred

# 13. Summarize Strict Accuracy by condition -------------------

strict_results <- list()
counter <- 1

for (g in levels(df_accuracy$group)) {
  
  for (s in levels(df_accuracy$session)) {
    
    # Identify trials in the current condition
    idx <- which(
      df_accuracy$group == g &
        df_accuracy$session == s
    )
    
    # Average joint probability across trials
    condition_draws <- rowMeans(
      strict_prob_draws[, idx, drop = FALSE]
    )
    
    # Posterior summary
    strict_results[[counter]] <- data.frame(
      group = g,
      session = s,
      
      median = median(condition_draws),
      
      lower_95 = quantile(
        condition_draws,
        0.025
      ),
      
      upper_95 = quantile(
        condition_draws,
        0.975
      )
    )
    
    counter <- counter + 1
  }
}

strict_summary <- bind_rows(strict_results)

print(strict_summary)

# 14. Posterior predictive Strict Accuracy ---------------------

gg_rep <- posterior_predict(
  fit_accuracy_full,
  resp = "GG"
)

nn_rep <- posterior_predict(
  fit_accuracy_full,
  resp = "NN"
)

# Both outcomes are paired by posterior draw and trial
strict_rep <- gg_rep * nn_rep

# Summarize realized Strict Accuracy by Group and Session
strict_predictive_results <- list()
counter <- 1

for (g in levels(df_accuracy$group)) {
  
  for (s in levels(df_accuracy$session)) {
    
    idx <- which(
      df_accuracy$group == g &
        df_accuracy$session == s
    )
    
    condition_draws <- rowMeans(
      strict_rep[, idx, drop = FALSE]
    )
    
    strict_predictive_results[[counter]] <- data.frame(
      group = g,
      session = s,
      median = median(condition_draws),
      lower_95 = quantile(condition_draws, 0.025),
      upper_95 = quantile(condition_draws, 0.975)
    )
    
    counter <- counter + 1
  }
}

strict_predictive_summary <- bind_rows(
  strict_predictive_results
)

print(strict_predictive_summary)

# 15. Calculate the strict-accuracy interaction ---------------------------------------------
# Store posterior draws of strict accuracy for each condition
strict_condition_draws <- list()

for (g in levels(df_accuracy$group)) {
  for (s in levels(df_accuracy$session)) {
    
    idx <- which(
      df_accuracy$group == g &
        df_accuracy$session == s
    )
    
    strict_condition_draws[[paste(g, s, sep = "_")]] <-
      rowMeans(strict_prob_draws[, idx, drop = FALSE])
  }
}

# Difference in change: experimental minus control
strict_interaction_draws <-
  (strict_condition_draws[["experimental_2"]] -
     strict_condition_draws[["experimental_1"]]) -
  (strict_condition_draws[["control_2"]] -
     strict_condition_draws[["control_1"]])

# Summarise posterior interaction
strict_interaction_summary <- data.frame(
  median = median(strict_interaction_draws),
  lower_95 = quantile(strict_interaction_draws, 0.025),
  upper_95 = quantile(strict_interaction_draws, 0.975),
  probability_positive = mean(strict_interaction_draws > 0)
)

print(strict_interaction_summary)


# 15. Save results ---------------------------------------------

saveRDS(
  strict_summary,
  "output/final/strict_accuracy_posterior_summary.rds"
)

saveRDS(
  strict_predictive_summary,
  "output/final/strict_accuracy_predictive_summary.rds"
)

write.csv(
  strict_summary,
  "output/final/strict_accuracy_posterior_summary.csv",
  row.names = FALSE
)

write.csv(
  strict_predictive_summary,
  "output/final/strict_accuracy_predictive_summary.csv",
  row.names = FALSE
)
