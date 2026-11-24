# REGISTERED ANALYSES FOR ACTPSY-S-25-05866-R4
# LO
# 2026/11/24
# TASK: exclude subjects according to registered criteria
library(dplyr)

# LOAD DATAFRAME DF
#readRDS(df,"output/interim/df_before_exclusions.rds")
length(levels(df$subject)) # 90 subjects with data

# EXCLUSION CRITERION #1 ---------
# if they produced more than 20\% incomprehensible, unrelated, or silent responses in exposure trials
# checked by RAs

levels(df$subject) # 90
speechless <- c('AN20A','AN25A','AN31I','AN32A','AN33A','DA02I','MN04A','MN12A','MN16I','MN21I','MN28A','MN35A')
length(speechless) # 12 subjects to be excluded

df_criterion1 <- df %>%
  filter(!subject %in% speechless) %>%
  droplevels()
df_criterion1
levels(df_criterion1$subject) # 78

# EXCLUSION CRITERION #2 ----
# And/or if they fail to meet a minimum performance on *exposure trials*: fewer than 12 correct out of 32 (binomial $p < .05$ for 4AFC). 
# p (12/32) = 0.375
#12/32
# 1. Consider only exposure trials
df_exposure <- df_criterion1 %>%
  filter(phase == "exposure")

#View(df_exposure)
levels(df_exposure$subject) # 78 participants


# 2. Total exposure performance per subject (all 64 trials) ----
exposure_perf_subject <- df_exposure %>%
  group_by(subject) %>%
  summarise(
    n_trials_exposure      = n(),
    n_correct_exposure     = sum(correct, na.rm = TRUE),
    prop_correct_exposure  = n_correct_exposure / n_trials_exposure,
    .groups = "drop"
  )

exposure_perf_subject

# 3. Exposure performance per subject × session (each should have 32 trials) ----
exposure_perf_subj_sess <- df_exposure %>%
  group_by(subject, session) %>%
  summarise(
    n_trials_exposure      = n(),
    n_correct_exposure     = sum(correct, na.rm = TRUE),
    prop_correct_exposure  = n_correct_exposure / n_trials_exposure,
    .groups = "drop"
  )

exposure_perf_subj_sess

# SUBJECTS TO EXCLUDE ----
# subjects to exclude if they have < 12 correct in *any* exposure session
# As long as one session (any) is poor, exclude subject:
to_exclude <- exposure_perf_subj_sess %>%
  filter(n_correct_exposure < 12) %>%
  distinct(subject)

to_exclude # 3 subjects to exclude: DA09I DA15I DA16A 
78-3 # = 75


#
saveRDS(exposure_perf_subject, "output/interim/exposure_perf_subject.rds")
saveRDS(exposure_perf_subj_sess, "output/interim/exposure_perf_subject_session.rds")
saveRDS(to_exclude,     "outpu/interim/to_exclude_subjects.rds")

# to_exclude is currently a tibble with one column 'subject'
# Convert it to a plain vector of subject IDs
to_exclude_vec <- to_exclude %>%
  pull(subject) %>%        # extract the subject column as a vector
  as.character()           # optional: make sure it’s character, not factor

length(to_exclude_vec)     # 3
to_exclude_vec
df_included <- df_criterion1 %>%
  # filter(condition == "test") %>%
  filter(!subject %in% to_exclude_vec) %>%
  droplevels()

df_included
length(levels(df_included$subject)) # 75 final subjects


# number of subjects per group
df_included %>%
  distinct(subject, group) %>%
  count(group)
# control         39
# experimental    36

levels(df_included$session) # 2 levels
levels(df_included$subject) # 76 subjects
levels(df_included$group) # 2 levels

saveRDS(df_included, "output/interim/df_included.rds")
