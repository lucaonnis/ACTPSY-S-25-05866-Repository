# REGISTERED ANALYSES FOR ACTPSY-S-25-05866-R4
# LO
# 2026/11/24

library(tidyverse)
library(readr)
library(stringr)
library(purrr)
library(broom)

# Where are we?
getwd()

# Load data from individual participants -----------
base_dir <- "data/"
rm(subject_dirs)
## 1. List subject folders ----
subject_dirs <- list.dirs(
  base_dir,
  full.names = TRUE,
  recursive = FALSE
)
# exclude all hidden directories
subject_dirs <- subject_dirs[!grepl("^\\.", basename(subject_dirs))]
subject_dirs # 94 initial directories/subjects

## 2. Exclude subjects at folder level because their responses were not recorded by OpenSesame----
subject_dirs <- subject_dirs[!grepl("^AN01|MN01I|MN02A|MN14A", basename(subject_dirs))]

subject_dirs # 91 directories/subjects

## 3. Helper to read one CSV and add session information ----
read_session_file <- function(file_path) {
  
  filename <- basename(file_path)
  
  session_num <- case_when(
    str_detect(filename, "1\\.csv$") ~ 1L,
    str_detect(filename, "2\\.csv$") ~ 2L,
    TRUE ~ NA_integer_
  )
  
  if (is.na(session_num)) {
    stop("Could not determine session from filename: ", filename)
  }
  
  read_csv(
    file_path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) %>%
    mutate(
      session = session_num,
      source_file = filename
    )
}


## 4. Build df ----
df <- map_dfr(subject_dirs, function(sdir) {
  
  f1 <- list.files(
    path = sdir,
    pattern = "^subject-.*1\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  f2 <- list.files(
    path = sdir,
    pattern = "^subject-.*2\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  
  message(
    "Subject folder: ", basename(sdir),
    " | session 1: ",
    if (length(f1)) paste(basename(f1), collapse = ", ") else "none",
    " | session 2: ",
    if (length(f2)) paste(basename(f2), collapse = ", ") else "none"
  )
  
  if (length(f1) == 0 && length(f2) == 0) {
    return(NULL)
  }
  
  df1 <- if (length(f1) > 0) {
    map_dfr(f1, read_session_file)
  } else {
    NULL
  }
  
  df2 <- if (length(f2) > 0) {
    map_dfr(f2, read_session_file)
  } else {
    NULL
  }
  
  # Assign the subject's group using session 1 only
  if (!is.null(df1) && nrow(df1) > 0) {
    contains_lanonna <- any(
      str_detect(
        unlist(df1, use.names = FALSE),
        regex("lanonna", ignore_case = TRUE)
      ),
      na.rm = TRUE
    )
    
    group_value <- if_else(
      contains_lanonna,
      "experimental",
      "control"
    )
  } else {
    group_value <- NA_character_
  }
  
  subject_df <- bind_rows(df1, df2)
  
  if (nrow(subject_df) == 0) {
    return(NULL)
  }
  
  subject_df %>%
    mutate(
      subject = basename(sdir),
      group = group_value
    )
})

df %>%
  distinct(subject, session, group, source_file) %>%
  arrange(subject, session)

df %>%
  distinct(subject, session) %>%
  count(session)

## 5. Remove practice rows in experiment_sequence
if (nrow(df) > 0) {
  df <- df %>%
    filter(
      !str_detect(
        experiment_sequence,
        regex("practice", ignore_case = TRUE)
      )
    )
}

## Inspect result
unique(df$subject) # 91 subjects
glimpse(df)

######### code responses by correct gender and number -----

library(dplyr)
library(stringr)
library(tidyr)

## Helper to infer gender and number from Italian determiners
get_gender <- function(det) {
  case_when(
    det %in% c("il","i") ~ "m",
    det %in% c("la", "le") ~ "f",
    TRUE                               ~ NA_character_
  )
}

get_number <- function(det) {
  case_when(
    det %in% c("il", "la")       ~ "sg",
    det %in% c("i", "le")       ~ "pl",
    TRUE                               ~ NA_character_
  )
}

df <- df %>%
  mutate(
    ## ---------- EXPOSURE TRIALS (filenames like: lapenna_f_sg.png) ----------
    exp_resp_gender = str_extract(response_pic, "_[fm]_"),     # "_f_" or "_m_"
    exp_corr_gender = str_extract(correct_pic,  "_[fm]_"),
    exp_resp_number = str_extract(response_pic, "_(sg|pl)"),  # "_sg_" or "_pl_"
    exp_corr_number = str_extract(correct_pic,  "_(sg|pl)"),
    
    ## ---------- TEST TRIALS (e.g., "i vasi.png") ----------
    # take first token as determiner, strip .png, lower case
    resp_det = response_pic %>%
      str_remove("\\.png$") %>%
      str_trim() %>%
      str_split_fixed("\\s+", 2) %>%
      .[, 1] %>%
      str_to_lower(),
    
    corr_det = correct_pic %>%
      str_remove("\\.png$") %>%
      str_trim() %>%
      str_split_fixed("\\s+", 2) %>%
      .[, 1] %>%
      str_to_lower(),
    
    test_resp_gender = get_gender(resp_det),
    test_corr_gender = get_gender(corr_det),
    test_resp_number = get_number(resp_det),
    test_corr_number = get_number(corr_det),
    
    ## ---------- FINAL gender / number ----------
    gender = case_when(
      # exposure: match/mismatch on filename gender tag
      condition == "exposure" &
        !is.na(exp_resp_gender) & !is.na(exp_corr_gender) &
        exp_resp_gender == exp_corr_gender ~ 1,
      
      condition == "exposure" &
        !is.na(exp_resp_gender) & !is.na(exp_corr_gender) &
        exp_resp_gender != exp_corr_gender ~ 0,
      
      # test: match/mismatch on determiner gender
      condition == "test" &
        !is.na(test_resp_gender) & !is.na(test_corr_gender) &
        test_resp_gender == test_corr_gender ~ 1,
      
      condition == "test" &
        !is.na(test_resp_gender) & !is.na(test_corr_gender) &
        test_resp_gender != test_corr_gender ~ 0,
      
      TRUE ~ NA_real_
    ),
    
    number = case_when(
      # exposure: match/mismatch on filename number tag
      condition == "exposure" &
        !is.na(exp_resp_number) & !is.na(exp_corr_number) &
        exp_resp_number == exp_corr_number ~ 1,
      
      condition == "exposure" &
        !is.na(exp_resp_number) & !is.na(exp_corr_number) &
        exp_resp_number != exp_corr_number ~ 0,
      
      # test: match/mismatch on determiner number
      condition == "test" &
        !is.na(test_resp_number) & !is.na(test_corr_number) &
        test_resp_number == test_corr_number ~ 1,
      
      condition == "test" &
        !is.na(test_resp_number) & !is.na(test_corr_number) &
        test_resp_number != test_corr_number ~ 0,
      
      TRUE ~ NA_real_
    )
  ) %>%
  # Drop helpers if you don’t need them
  select(-exp_resp_gender, -exp_corr_gender,
         -exp_resp_number, -exp_corr_number,
         -resp_det, -corr_det,
         -test_resp_gender, -test_corr_gender,
         -test_resp_number, -test_corr_number)

names(df)
unique(df$subject) # 91
df$subject_ID <- NULL # clean up

# Fix some of the wrong subject IDs ----
#df <- df %>%
#  mutate(
#    subject = case_when(
#      subject %in% c("AN10", "AN10A")          ~ "AN10A",
#      subject %in% c("AN11", "AN11A", "AN11I") ~ "AN11I",
#      subject %in% c("AN15", "AN15A")          ~ "AN15A",
#      TRUE                                     ~ subject
#    )
#  )

# CORRECT UPPER CASE RESPONSES to lowercase
df <- df %>%
  mutate(
    # normalise to uppercase and trim any stray whitespace
    correct_ans_norm = str_trim(str_to_upper(correct_ans)),
    response_norm    = str_trim(str_to_upper(response)),
    # recompute correctness as 1/0
    correct_agreement = if_else(
      !is.na(correct_ans_norm) & !is.na(response_norm) &
        correct_ans_norm == response_norm,
      1L, 0L
    )
  )

library(dplyr)
names(df)

df <- df %>%
  # drop old subject and helper normalization columns
  select(-correct_ans_norm, -response_norm) # %>%
  # rename subject_clean -> subject
 # rename(subject = subject_clean)

df <- df %>%
  mutate(correct = correct_agreement)

df <- df %>%
  select(-correct_agreement)



# Specify factors -----------------------
# rename 'condition' as 'phase' (because condition reminds one of an experimental manipulation, whereas phase is more neutral and is experienced by each participant)
df <- df %>%
  mutate(
    phase  = factor(condition),
    group      = factor(group),
    session    = factor(session),
    subject    = factor(subject), # to be included as random factor
    item = factor(correct_pic), # rename to be included as random factor
    block = as.integer(block),
    block_sequence = as.integer(block_sequence),
    trial_number = as.integer(trial_number),
    experiment_sequence = as.integer(experiment_sequence)
  )

# delete duplicates:
df$condition <- df$correct_pic <- NULL
names(df)
str(df)

length(levels(df$subject)) # 91 subjects
str(df)

# SANITY CHECK ---------------
# number of subjects per group
levels(df$phase)
levels(df$session) # 2 levels
levels(df$subject) # 90 subjects, 4 had already been excluded
levels(df$group) # 2 levels

names(df)
glimpse(df)

# Subject-level counts
df %>%
  distinct(subject, group) %>%
  count(group)
# group            n
# <fct>        <int>
# 1 control         45
# 2 experimental    45

# verify that all participants have both sessions:
df %>%
  distinct(subject, session) %>%
  count(session)

# session     n
#  <fct>   <int>
# 1          90
# 2          90

# confirm each subject has exactly one stable group label across both sessions:
# should return zero rows:
df %>%
  distinct(subject, group) %>%
  count(subject) %>%
  filter(n != 1)

# SAVE DATAFRAME DF -----------
# Make sure the output folders exist before saving:
dir.create("output/interim", recursive = TRUE,
           showWarnings = FALSE)

dir.create("output/final", recursive = TRUE,
           showWarnings = FALSE)

saveRDS(df, file = "output/interim/df_before_exclusions.rds")
