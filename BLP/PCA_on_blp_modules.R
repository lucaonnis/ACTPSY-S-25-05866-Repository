
dataset <- testdata
# Select the module variables
modules <- dataset[, c(
  "module1_history_entropy",
  "module2_use_entropy",
  "module3_proficiency_entropy",
  "module4_attitudes_entropy"
)]

# 1. Correlation analysis between modules ----
# Pearson correlation
cor_matrix <- cor(modules, use = "pairwise.complete.obs",
                  method = "pearson")

round(cor_matrix, 2)

# Spearman correlations, which are less sensitive to non-normal distributions and outliers:
cor(modules, use = "pairwise.complete.obs",
    method = "spearman")

# Visualize the correlations:
library(corrplot)

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45)

cor(modules, method = "spearman",
    use = "complete.obs")

# 2. Compare each module against the other three
model_history <- lm(
  module1_history_entropy ~
    module2_use_entropy +
    module3_proficiency_entropy +
    module4_attitudes_entropy,
  data = dataset
)

summary(model_history)

# Repeat this for the other modules.
# A very high R² indicates that a module's entropy scores can be largely reconstructed from the other modules. This may indicate redundancy.

# 3. Perform PCA ----
pca <- prcomp(modules, scale. = TRUE)
summary(pca)
round(pca$rotation, 3)

# 4. An important additional analysis: Unique contribution of each module ----
# You can calculate the R² for each module separately:
models <- list(
  History = lm(
    module1_history_entropy ~ module2_use_entropy +
      module3_proficiency_entropy +
      module4_attitudes_entropy,
    data = dataset
  ),
  
  Use = lm(
    module2_use_entropy ~ module1_history_entropy +
      module3_proficiency_entropy +
      module4_attitudes_entropy,
    data = dataset
  ),
  
  Proficiency = lm(
    module3_proficiency_entropy ~ module1_history_entropy +
      module2_use_entropy +
      module4_attitudes_entropy,
    data = dataset
  ),
  
  Attitudes = lm(
    module4_attitudes_entropy ~ module1_history_entropy +
      module2_use_entropy +
      module3_proficiency_entropy,
    data = dataset
  )
)

sapply(models, function(x) summary(x)$r.squared)

# 5. An important next step: Calculate the unique contribution more rigorously ----

library(car)
model_all <- lm(
  overall_entropy ~
    module1_history_entropy +
    module2_use_entropy +
    module3_proficiency_entropy +
    module4_attitudes_entropy,
  data = dataset
)

vif(model_all)

# 6. Compare the two-module combinations directly ----
library(dplyr)
library(purrr)
library(tibble)

module_names <- names(modules)

combinations <- combn(
  module_names,
  2,
  simplify = FALSE
)

full_score <- rowMeans(scale(modules))

results <- map_dfr(combinations, function(vars) {
  
  reduced_score <- rowMeans(
    scale(modules[, vars])
  )
  
  tibble(
    combination = paste(vars, collapse = " + "),
    correlation = cor(
      reduced_score,
      full_score,
      method = "spearman"
    ),
    MAE = mean(abs(reduced_score - full_score))
  )
})

results %>% arrange(desc(correlation))

# 7. Compare against the full questionnaire using cross-validation ----

install.packages("rsample")
library(rsample)
library(dplyr)
library(purrr)

set.seed(123)

# Create 10-fold cross-validation
folds <- vfold_cv(dataset, v = 10)

module_names <- names(modules)

combinations <- combn(
  module_names,
  2,
  simplify = FALSE
)

cv_results <- map_dfr(combinations, function(vars) {
  
  fold_results <- map_dfr(folds$splits, function(split) {
    
    test_data <- assessment(split)
    
    # Standardize using training-set parameters
    train_data <- analysis(split)
    
    means <- sapply(train_data[, vars], mean)
    sds <- sapply(train_data[, vars], sd)
    
    reduced_score <- rowMeans(
      sweep(
        sweep(test_data[, vars], 2, means, "-"),
        2, sds, "/"
      )
    )
    
    # Full score using all four modules
    full_means <- sapply(train_data[, module_names], mean)
    full_sds <- sapply(train_data[, module_names], sd)
    
    full_score <- rowMeans(
      sweep(
        sweep(test_data[, module_names], 2, full_means, "-"),
        2, full_sds, "/"
      )
    )
    
    tibble(
      correlation = cor(reduced_score, full_score,
                        method = "spearman"),
      MAE = mean(abs(reduced_score - full_score))
    )
  })
  
  tibble(
    combination = paste(vars, collapse = " + "),
    mean_correlation = mean(fold_results$correlation,
                            na.rm = TRUE),
    mean_MAE = mean(fold_results$MAE, na.rm = TRUE)
  )
})

cv_results %>%
  arrange(desc(mean_correlation))

##############################################################
# Can we reduce the questionnaire to just one module? ----
##############################################################

full_score <- rowMeans(scale(modules))

single_module_results <- map_dfr(
  names(modules),
  function(variable) {
    
    score <- scale(modules[[variable]])
    
    tibble(
      module = variable,
      spearman = cor(
        as.numeric(score),
        full_score,
        method = "spearman"
      )
    )
  }
)

single_module_results %>%
  arrange(desc(spearman))


# FINAL RECOMMMENDATION: -----
# One module: Use performs best (ρ = 0.932).
# Two modules: History + Use performed best for preserving participant rankings (ρ = 0.971).
# Two-module cross-validation: History + Use and Use + Attitudes were very close (0.962 versus 0.958).