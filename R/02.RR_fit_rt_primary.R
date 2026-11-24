# REGISTERED ANALYSES FOR ACTPSY-S-25-05866-R3
# Luca Onnis
# 2026/10/12
# Interacted with chatGPT

# Step 1. Prepare the data and preprocess RTs ----
# Start with your original df_test and run:
library(dplyr)
library(brms)
library(posterior)
library(bayesplot)

# Prepare RT and Strict Accuracy
df_rt_base <- df_test %>%
  mutate(
    RT = suppressWarnings(as.numeric(as.character(response_time))),
    StrictAccuracy = case_when(
      is.na(gender) | is.na(number) ~ NA_integer_,
      gender == 1 & number == 1 ~ 1L,
      TRUE ~ 0L
    )
  )

# Remove implausibly fast RTs
df_rt_basic <- df_rt_base %>%
  filter(
    !is.na(RT),
    RT >= 200,
    !is.na(subject),
    !is.na(item),
    !is.na(group),
    !is.na(session)
  )

# Identify long RT outliers (> 3 SD above participant-session mean)
df_rt_audit <- df_rt_basic %>%
  group_by(subject, session) %>%
  mutate(
    rt_mean = mean(RT),
    rt_sd = sd(RT),
    rt_z = if_else(
      rt_sd > 0,
      (RT - rt_mean) / rt_sd,
      NA_real_
    ),
    rt_outlier = !is.na(rt_z) & rt_z > 3
  ) %>%
  ungroup()

# Exclude outliers and log-transform RT
df_rt_preprocessed <- df_rt_audit %>%
  filter(!rt_outlier) %>%
  mutate(
    log_rt = log(RT),
    group = relevel(factor(group), ref = "control"),
    session = relevel(factor(session), ref = "1")
  )

# Create analysis datasets
df_rt_correct <- df_rt_preprocessed %>%
  filter(StrictAccuracy == 1)

df_rt_all <- df_rt_preprocessed %>%
  filter(!is.na(StrictAccuracy)) %>%
  mutate(
    Accuracy = factor(
      StrictAccuracy,
      levels = c(0, 1),
      labels = c("Incorrect", "Correct")
    )
  )

# Report sample sizes
cat("Initial trials:", nrow(df_rt_base), "\n")
cat("Trials after RT preprocessing:",
    nrow(df_rt_preprocessed), "\n")
cat("Correct trials:", nrow(df_rt_correct), "\n")
cat("Sensitivity analysis trials:", nrow(df_rt_all), "\n")
cat("Participants:", n_distinct(df_rt_correct$subject), "\n")
cat("Items:", n_distinct(df_rt_correct$item), "\n")

# Exclusion counts
cat("Missing RT:", sum(is.na(df_rt_base$RT)), "\n")
cat("RT below 200 ms:",
    sum(!is.na(df_rt_base$RT) & df_rt_base$RT < 200), "\n")
cat("Long RT outliers:",
    sum(df_rt_audit$rt_outlier, na.rm = TRUE), "\n")


# Step 2. Fit the primary RT model ----
# Calculate the intercept prior center
rt_intercept_center <- log(
  median(df_rt_correct$RT, na.rm = TRUE)
)

# Insert the numerical value into the prior
priors_rt <- c(
  prior(normal(0, 1), class = "b"),
  set_prior(
    paste0(
      "normal(",
      rt_intercept_center,
      ", 1.5)"
    ),
    class = "Intercept"
  ),
  prior(student_t(3, 0, 2.5), class = "sd"),
  prior(student_t(3, 0, 2.5), class = "sigma")
)

# Fit primary model
fit_rt <- brm(
  log_rt ~ group * session +
    (1 | subject) +
    (1 | item),
  data = df_rt_correct,
  family = gaussian(),
  prior = priors_rt,
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 1000,
  seed = 2026,
  control = list(
    adapt_delta = 0.95,
    max_treedepth = 12
  )
)

summary(fit_rt)
pp_check(fit_rt)


# Step 3. Fit the registered sensitivity model ----
fit_rt_sensitivity <- brm(
  log_rt ~ group * session + Accuracy +
    (1 | subject) +
    (1 | item),
  data = df_rt_all,
  family = gaussian(),
  prior = priors_rt,
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 1000,
  seed = 2027,
  control = list(
    adapt_delta = 0.95,
    max_treedepth = 12
  )
)

summary(fit_rt_sensitivity)
pp_check(fit_rt_sensitivity)

saveRDS(fit_rt, "output/final/fit_rt_primary.rds")
saveRDS(fit_rt_sensitivity, "output/final/fit_rt_sensitivity.rds")

# Step 4. Inspect diagnostics and summarize effects -----
# For both models, inspect:
summary(fit_rt)
summary(fit_rt_sensitivity)

# Model Diagnostics --------------------
# Divergent transitions
# Primary model
nuts_params(fit_rt) %>%
  filter(Parameter == "divergent__") %>%
  summarise(divergences = sum(Value))

# Sensitivity model
nuts_params(fit_rt_sensitivity) %>%
  filter(Parameter == "divergent__") %>%
  summarise(divergences = sum(Value))


# Number of rows at each preprocessing stage --------------
cat("Original trials:", nrow(df_rt_base), "\n")
cat("Missing/non-numeric RT:",
    sum(is.na(df_rt_base$RT)), "\n")
cat("RT below 200 ms:",
    sum(!is.na(df_rt_base$RT) & df_rt_base$RT < 200), "\n")
cat("Long RT outliers:",
    sum(df_rt_audit$rt_outlier, na.rm = TRUE), "\n")
cat("Remaining after RT exclusions:",
    nrow(df_rt_preprocessed), "\n")
cat("Missing Strict Accuracy:",
    sum(is.na(df_rt_preprocessed$StrictAccuracy)), "\n")
cat("Strictly correct trials (primary model):",
    nrow(df_rt_correct), "\n")
cat("Trials in sensitivity model:",
    nrow(df_rt_all), "\n")

# 2. Posterior predictive checks -----
# Primary model
pp_check(fit_rt)

# Sensitivity model
pp_check(fit_rt_sensitivity)

# Maximum treedepth check ----
  # Primary model
  nuts_params(fit_rt) %>%
  filter(Parameter == "treedepth__") %>%
  summarise(max_treedepth = max(Value))

# Sensitivity model
nuts_params(fit_rt_sensitivity) %>%
  filter(Parameter == "treedepth__") %>%
  summarise(max_treedepth = max(Value))

