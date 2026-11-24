# REGISTERED ANALYSES FOR ACTPSY-S-25-05866-R4
# LO
# 2026/11/24
# TASK: DESCRIBE FINAL SAMPLE OF INCLUDED SUBJECTS 
# Sample group descriptive statistics
library(tidyverse)
library(readr)
library(stringr)
#df_included <- read_csv("output/interim/df_included_subjects.csv")
blp <- read_csv("BLP/blp_entropies.csv") 
View(blp)

# convert to factor

df_included <- df_included %>%
  mutate(
    subject  = factor(subject),
    group      = factor(group)   )

blp <- blp %>%
  mutate(across(where(is.character), as.factor))

levels(blp$subject)

# Keep only BLP IDs that occur exactly once
df_subjects <- df_included %>%
  distinct(subject, group) %>%
  mutate(
    subject_key = str_remove(as.character(subject), "[AI]$")
  ) %>%
  inner_join(
    blp %>%
      transmute(
        subject_key = as.character(subject),
        age,
        sex,
        l1,
        l2,
        l3,
        overall_entropy
      ),
    by = "subject_key"
  ) %>%
  select(subject, group, age, sex, l1, l2, l3, overall_entropy)


levels(blp$subject)
levels(df_subjects$subject)
# Create one row for each of the 76 included participants
df_subjects <- df_included %>%
  distinct(subject, group) %>%
  mutate(
    subject_key = str_remove(as.character(subject), "[AI]$")
  ) %>%
  left_join(blp_lookup_unique, by = "subject_key") %>%
  select(subject, group, age, sex, l1, l2, l3, overall_entropy) %>%
  mutate(
    subject = droplevels(subject),
    group = droplevels(group)
  )


# Check that all experimental participants are retained:

df_subjects %>%
  summarise(
    n_rows = n(),
    n_unique_subjects = n_distinct(subject),
    n_missing_blp = sum(is.na(age))
  )

# Missing in blp:
df_subjects %>%
  filter(is.na(age)) %>%
  select(subject, group)

# AN18A, AN21I, AN35A
# fixed: MN15A, MN32I, MN33I
View(df_subjects)

df_subjects <- droplevels(df_subjects)

levels(df_subjects$subject) # 75

glimpse(df_subjects)

################################ -----
## Report descriptive statistics of this sample per group:
# mean/sd age; number of female/males/other; mean BLP entropy. I want to report that these differences are not statistically different across group.

# Ensure sex has consistent labels
df_subjects <- df_subjects %>%
  mutate(
    sex = as.character(sex),
    sex = tolower(sex),
    sex = case_when(
      sex %in% c("female", "male", "other") ~ sex,
      is.na(sex) ~ NA_character_,
      TRUE ~ "other"
    )
  )

# Sample descriptives by group
group_summary <- df_subjects %>%
  group_by(group) %>%
  summarise(
    n_total = n(),
    
    n_age = sum(!is.na(age)),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    
    n_female = sum(sex == "female", na.rm = TRUE),
    n_male = sum(sex == "male", na.rm = TRUE),
    n_other = sum(sex == "other", na.rm = TRUE),
    n_sex_available = sum(!is.na(sex)),
    
    n_entropy = sum(!is.na(overall_entropy)),
    mean_entropy = mean(overall_entropy, na.rm = TRUE),
    sd_entropy = sd(overall_entropy, na.rm = TRUE),
    
    .groups = "drop"
  )

group_summary %>%
  select(group, n_entropy, mean_entropy, sd_entropy)


# Run Welch independent-samplest-tests for age and BLP entropy: ----
age_ttest <- t.test(
  age ~ group,
  data = df_subjects
)

entropy_ttest <- t.test(
  overall_entropy ~ group,
  data = df_subjects
)

age_ttest
entropy_ttest

# For sex/gender composition, use Fisher's exact test. It is preferable to a chi-square test when some categories, especially “other,” have small cell counts.
sex_table <- df_subjects %>%
  filter(!is.na(sex)) %>%
  count(group, sex) %>%
  pivot_wider(
    names_from = sex,
    values_from = n,
    values_fill = 0
  )

sex_table

sex_fisher <- df_subjects %>%
  filter(!is.na(sex)) %>%
  with(table(group, sex)) %>%
  fisher.test()

sex_fisher

# Create a concise table of the continuous-variable tests: ----

test_results <- tibble(
  variable = c("Age", "BLP overall entropy"),
  statistic = c(
    unname(age_ttest$statistic),
    unname(entropy_ttest$statistic)
  ),
  df = c(
    unname(age_ttest$parameter),
    unname(entropy_ttest$parameter)
  ),
  p_value = c(
    age_ttest$p.value,
    entropy_ttest$p.value
  )
)

test_results

if (age_ttest$p.value > .05) {
  message("Age did not significantly differ between groups.")
}

if (entropy_ttest$p.value > .05) {
  message("BLP entropy did not significantly differ between groups.")
}

if (sex_fisher$p.value > .05) {
  message("Sex composition did not significantly differ between groups.")
}


# APA REPORT:
# The final sample comprised 76 participants, including 39 in the control group and 37 in the experimental group. Age data were available for 38 control-group participants and 35 experimental-group participants. The control group had a mean age of 21.21 years (\textit{SD} = 1.54), whereas the experimental group had a mean age of 21.54 years (\textit{SD} = 2.32). This difference was not statistically significant, $t(58.51) = -0.71$, $p = .478$. Sex data were available for 37 control-group participants and 35 experimental-group participants. Of these, the control group included 22 females and 15 males, while the experimental group included 21 females and 14 males; no participants reported another gender identity. Sex distributions did not differ significantly between groups, Fisher's exact test $p = 1.00$. BLP overall entropy data were available for 38 control-group participants and 35 experimental-group participants. Mean overall entropy was 1.27 (\textit{SD} = 0.24) in the control group and 1.20 (\textit{SD} = 0.30) in the experimental group. This difference was not statistically significant, $t(66.00) = 1.09$, $p = .280$.
