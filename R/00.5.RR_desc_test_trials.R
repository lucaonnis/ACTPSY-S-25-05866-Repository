# REGISTERED ANALYSES FOR ACTPSY-S-25-05866-R4
# LO
# 2026/11/24
# TASK: # DESCRIBE AND ANALYSE TEST TRIALS

# restrict to test trials
length(levels(df$subject)) # 90
# df_included <- read_csv("df_included_subjects.csv") 
length(levels(df_included$subject)) # 75 final subjects


# DESCRIBE test trials  -----
df_test <- df_included %>%
  filter(phase == "test") %>%
  droplevels()

length(levels(df_test$subject))         # 75 subjects
# Check how many missing responses
colSums(is.na(df_test))

# number of subjects per group
df_test %>%
  distinct(subject, group) %>%
  count(group)
# control         39
# experimental    36

nrow(df_test)

# number of test trials per group
df_used <- df_test %>%
  filter(
    !is.na(gender),
    !is.na(number),
    !is.na(group),
    !is.na(session),
    !is.na(subject),
    !is.na(item)
  )

112/nrow(df_test)*100
4800/64

# total trials
nrow(df_used)

# Test trials per group
df_used %>% count(group)


View(df_test)
levels(df_test$group)
levels(df_test$session)

saveRDS(df_test, "output/final/df_test.rds")


# Describe strictly accurate responses (defined as StrictAccuracy in the manuscript), namely those identified as correct in both gender and number
accuracy_test <- df_test %>%
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

accuracy_test

