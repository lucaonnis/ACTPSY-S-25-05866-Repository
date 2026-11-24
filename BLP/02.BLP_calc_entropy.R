# Task: Calculates language entropy values per subject from a language questionnaire
# Bilingual Language Profile, BLP (Birdsong, D., Gertken, L.M., & Amengual, 2012)

# 1. Get row‑wise proportions + entropy.
# 2. Compute entropies for all 4 modules.
# 3. Compute overall entropy per subject.
# 4. Export CSV
# 5. Plot entropies and save figure
View(testdata)
#rm(testdata)
#testdata <- read.csv("testdata.csv")
rowhold <- nrow(testdata)
rowhold # 103 individuals recorded on Nettskjema
glimpse(testdata)

# -------------------------------
# Entropy function
# -------------------------------
entropy <- function(x, base = 2){
  entr <- 0
  
  if(sum(x, na.rm = TRUE) == 0){
    return(NA)
  } else {
    for (i in x){
      if(i > 0 & !is.na(i)){
        entr = entr + (i * log(1 / i, base = base))
      }
    }
  }
  return(entr)
}

# -------------------------------
# Helper: entropy of row-wise proportions
# -------------------------------
# prop_entropy is a small helper function that bundles together two things.
#   
# 1. Normalize: Turning raw scores for each language into proportions. “Out of everything this person reported here, what fraction is L1, what fraction is L2, what fraction is L3?”

# 2. Summarize: “Given those fractions, how evenly spread are they across languages?”. Feeding those proportions into the entropy function to get a single number.

# The output is a single entropy value per person for that module

prop_entropy <- function(df, cols, base = 2) {
  m <- as.matrix(df[, cols])
  row_sums <- rowSums(m, na.rm = TRUE)
  # proportions; division by 0 will give NaN, which entropy() turns into NA
  props <- sweep(m, 1, row_sums, FUN = "/")
  apply(props, 1, entropy, base = base)
}

## =====================================================
## MODULE 1: LANGUAGE HISTORY
## =====================================================

testdata$hist_known_entropy <- prop_entropy(
  testdata,
  c("years_known_l1", "years_known_l2", "years_known_l3")
)

testdata$hist_school_entropy <- prop_entropy(
  testdata,
  c("classes_l1", "classes_l2", "classes_l3")
)

testdata$hist_country_entropy <- prop_entropy(
  testdata,
  c("years_country_l1", "years_country_l2", "years_country_l3")
)

testdata$hist_family_env_entropy <- prop_entropy(
  testdata,
  c("family_l1", "family_l2", "family_l3")
)

testdata$hist_work_env_entropy <- prop_entropy(
  testdata,
  c("work_l1", "work_l2", "work_l3")
)

testdata$module1_history_entropy <- rowMeans(
  testdata[, c("hist_known_entropy",
               "hist_school_entropy",
               "hist_country_entropy",
               "hist_family_env_entropy",
               "hist_work_env_entropy")],
  na.rm = TRUE
)

## =====================================================
## MODULE 2: LANGUAGE USE
## =====================================================

## ---- Work, family, friends: raw matrices ----
work_mat    <- as.matrix(testdata[, c("use_work_l1",   "use_work_l2",   "use_work_l3")])
family_mat  <- as.matrix(testdata[, c("use_family_l1", "use_family_l2", "use_family_l3")])
friends_mat <- as.matrix(testdata[, c("friends_l1",    "friends_l2",    "friends_l3")])

## ---- Row-wise sums ----
work_sums    <- rowSums(work_mat,    na.rm = TRUE)
family_sums  <- rowSums(family_mat,  na.rm = TRUE)
friends_sums <- rowSums(friends_mat, na.rm = TRUE)

## ---- Row-wise proportions per setting ----
work_props    <- sweep(work_mat,    1, work_sums,    FUN = "/")
family_props  <- sweep(family_mat,  1, family_sums,  FUN = "/")
friends_props <- sweep(friends_mat, 1, friends_sums, FUN = "/")

## ---- Save proportions back to testdata ----
testdata$work_prop_l1    <- work_props[, 1]
testdata$work_prop_l2    <- work_props[, 2]
testdata$work_prop_l3    <- work_props[, 3]

testdata$family_prop_l1  <- family_props[, 1]
testdata$family_prop_l2  <- family_props[, 2]
testdata$family_prop_l3  <- family_props[, 3]

testdata$friends_prop_l1 <- friends_props[, 1]
testdata$friends_prop_l2 <- friends_props[, 2]
testdata$friends_prop_l3 <- friends_props[, 3]

## ---- Overall weekly use (work + family + friends) per language ----
overall_l1 <- work_mat[, 1] + family_mat[, 1] + friends_mat[, 1]
overall_l2 <- work_mat[, 2] + family_mat[, 2] + friends_mat[, 2]
overall_l3 <- work_mat[, 3] + family_mat[, 3] + friends_mat[, 3]

overall_mat  <- cbind(overall_l1, overall_l2, overall_l3)
overall_sums <- rowSums(overall_mat, na.rm = TRUE)
overall_props <- sweep(overall_mat, 1, overall_sums, FUN = "/")

testdata$l1_prop <- overall_props[, 1]
testdata$l2_prop <- overall_props[, 2]
testdata$l3_prop <- overall_props[, 3]

## ---- Entropy per setting ----
testdata$work_entropy    <- apply(work_props,    1, entropy, base = 2)
testdata$family_entropy  <- apply(family_props,  1, entropy, base = 2)
testdata$friends_entropy <- apply(friends_props, 1, entropy, base = 2)

## ---- Overall language use entropy (dominance) ----
testdata$percentuse_entropy <- apply(overall_props, 1, entropy, base = 2)

## ---- Self-talk and counting: proportions + entropy ----
selftalk_mat <- as.matrix(testdata[, c("selftalk_l1", "selftalk_l2", "selftalk_l3")])
count_mat    <- as.matrix(testdata[, c("count_l1",    "count_l2",    "count_l3")])

selftalk_sums <- rowSums(selftalk_mat, na.rm = TRUE)
count_sums    <- rowSums(count_mat,    na.rm = TRUE)

selftalk_props <- sweep(selftalk_mat, 1, selftalk_sums, FUN = "/")
count_props    <- sweep(count_mat,    1, count_sums,    FUN = "/")

testdata$selftalk_entropy <- apply(selftalk_props, 1, entropy, base = 2)
testdata$count_entropy    <- apply(count_props,    1, entropy, base = 2)

## ---- Module 2 summary entropy (mean across use domains) ----
testdata$module2_use_entropy <- rowMeans(
  testdata[, c("work_entropy",
               "family_entropy",
               "friends_entropy",
               "selftalk_entropy",
               "count_entropy")],
  na.rm = TRUE
)

## =====================================================
## MODULE 3: LANGUAGE PROFICIENCY
## =====================================================

testdata$speaking_entropy <- prop_entropy(
  testdata,
  c("speak_l1", "speak_l2", "speak_l3")
)

testdata$understanding_entropy <- prop_entropy(
  testdata,
  c("understand_l1", "understand_l2", "understand_l3")
)

testdata$writing_entropy <- prop_entropy(
  testdata,
  c("write_l1", "write_l2", "write_l3")
)

testdata$reading_entropy <- prop_entropy(
  testdata,
  c("read_l1", "read_l2", "read_l3")
)

testdata$module3_proficiency_entropy <- rowMeans(
  testdata[, c("speaking_entropy",
               "understanding_entropy",
               "writing_entropy",
               "reading_entropy")],
  na.rm = TRUE
)

## =====================================================
## MODULE 4: LANGUAGE ATTITUDES
## =====================================================

testdata$feel_self_entropy <- prop_entropy(
  testdata,
  c("feel_l1", "feel_l2", "feel_l3")
)

testdata$culture_ident_entropy <- prop_entropy(
  testdata,
  c("culture_l1", "culture_l2", "culture_l3")
)

testdata$native_import_entropy <- prop_entropy(
  testdata,
  c("use_native_l1", "use_native_l2", "use_native_l3")
)

testdata$module4_attitudes_entropy <- rowMeans(
  testdata[, c("feel_self_entropy",
               "culture_ident_entropy",
               "native_import_entropy")],
  na.rm = TRUE
)

## =====================================================
## OVERALL ENTROPY ACROSS MODULES
## =====================================================

testdata$overall_entropy <- rowMeans(
  testdata[, c("module1_history_entropy",
               "module2_use_entropy",
               "module3_proficiency_entropy",
               "module4_attitudes_entropy")],
  na.rm = TRUE
)

## =====================================================
## EXPORT ENTROPY SCORES
## =====================================================
glimpse(testdata)
blp_entropies <- testdata %>%
  dplyr::select(
    subject,
    age,
    sex,
    l1,
    l2,
    l3,
    #nettskjema_id,
    percentuse_entropy,              # overall language use / dominance
    module1_history_entropy,
    module2_use_entropy,
    module3_proficiency_entropy,
    module4_attitudes_entropy,
    overall_entropy
  )

#colnames(blp_entropies)[3] <- "language_entropy_dominance"

# trim all numeric values to 2 decimal points:
blp_entropies <- blp_entropies %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

write.csv(blp_entropies, file = "blp_entropies.csv", row.names = FALSE)

View(blp_entropies)

## =====================================================
## PLOT DISTRIBUTION OF ENTROPY VALUES (MULTI-PAGE PDF)
## =====================================================

# Assumes you have already run the script that adds these columns to `testdata`:
#   module1_history_entropy
#   module2_use_entropy
#   module3_proficiency_entropy
#   module4_attitudes_entropy
#   overall_entropy

pdf("blp_module_entropies.pdf")

## ---- Page 1: Module 1–4 entropies ----
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

hist(
  testdata$module1_history_entropy,
  main = "Module 1: History entropy",
  xlab = "Entropy",
  ylab = "# participants",
  col = "grey90",
  border = "grey40"
)

hist(
  testdata$module2_use_entropy,
  main = "Module 2: Use entropy",
  xlab = "Entropy",
  ylab = "# participants",
  col = "grey90",
  border = "grey40"
)

hist(
  testdata$module3_proficiency_entropy,
  main = "Module 3: Proficiency entropy",
  xlab = "Entropy",
  ylab = "# participants",
  col = "grey90",
  border = "grey40"
)

hist(
  testdata$module4_attitudes_entropy,
  main = "Module 4: Attitudes entropy",
  xlab = "Entropy",
  ylab = "# participants",
  col = "grey90",
  border = "grey40"
)

## ---- Page 2: Overall entropy ----
par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))

hist(
  testdata$overall_entropy,
  main = "Overall entropy (Modules 1–4)",
  xlab = "Entropy",
  ylab = "# participants",
  col = "grey90",
  border = "grey40"
)

dev.off()
