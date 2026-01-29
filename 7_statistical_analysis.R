# Statistical Analysis Module
# Statistical tests and confidence intervals

# ============================================================================
# DESCRIPTIVE STATISTICS (Required by report structure)
# ============================================================================

comprehensive_descriptive_stats <- function(data, variable_name = "Variable") {
  cat("\n", rep("=", 70), "\n")
  cat(sprintf("DESCRIPTIVE STATISTICS: %s\n", variable_name))
  cat(rep("=", 70), "\n\n")
  
  # Remove NA values
  data_clean <- data[!is.na(data)]
  n <- length(data_clean)
  
  # Measures of central tendency
  mean_val <- mean(data_clean)
  median_val <- median(data_clean)
  
  # Measures of dispersion
  sd_val <- sd(data_clean)
  var_val <- var(data_clean)
  range_val <- range(data_clean)
  iqr_val <- IQR(data_clean)
  
  # Quantiles
  q25 <- quantile(data_clean, 0.25)
  q75 <- quantile(data_clean, 0.75)
  q10 <- quantile(data_clean, 0.10)
  q90 <- quantile(data_clean, 0.90)
  
  # Skewness and kurtosis
  skewness <- mean((data_clean - mean_val)^3) / (sd_val^3)
  kurtosis <- mean((data_clean - mean_val)^4) / (sd_val^4) - 3
  
  # Outlier detection (1.5*IQR rule)
  lower_fence <- q25 - 1.5 * iqr_val
  upper_fence <- q75 + 1.5 * iqr_val
  outliers <- data_clean[data_clean < lower_fence | data_clean > upper_fence]
  n_outliers <- length(outliers)
  
  cat("Sample Size:\n")
  cat(sprintf("  n = %d observations\n\n", n))
  
  cat("Measures of Central Tendency:\n")
  cat(sprintf("  Mean:             %.4f\n", mean_val))
  cat(sprintf("  Median:           %.4f\n", median_val))
  cat(sprintf("  Difference:       %.4f (%.2f%%)\n\n", 
              abs(mean_val - median_val), 
              abs(mean_val - median_val) / mean_val * 100))
  
  cat("Measures of Dispersion:\n")
  cat(sprintf("  Standard Dev:     %.4f\n", sd_val))
  cat(sprintf("  Variance:         %.4f\n", var_val))
  cat(sprintf("  Range:            [%.4f, %.4f]\n", range_val[1], range_val[2]))
  cat(sprintf("  IQR:              %.4f\n", iqr_val))
  cat(sprintf("  Coeff. of Var:    %.4f\n\n", sd_val / mean_val))
  
  cat("Quantiles:\n")
  cat(sprintf("  Min:              %.4f\n", min(data_clean)))
  cat(sprintf("  10th percentile:  %.4f\n", q10))
  cat(sprintf("  Q1 (25th):        %.4f\n", q25))
  cat(sprintf("  Median (50th):    %.4f\n", median_val))
  cat(sprintf("  Q3 (75th):        %.4f\n", q75))
  cat(sprintf("  90th percentile:  %.4f\n", q90))
  cat(sprintf("  Max:              %.4f\n\n", max(data_clean)))
  
  cat("Distribution Shape:\n")
  cat(sprintf("  Skewness:         %.4f ", skewness))
  if (abs(skewness) < 0.5) {
    cat("(approximately symmetric)\n")
    cat("  → Recommendation: Use MEAN and STANDARD DEVIATION\n")
  } else if (skewness > 0) {
    cat("(right-skewed)\n")
    cat("  → Recommendation: Use MEDIAN and IQR\n")
  } else {
    cat("(left-skewed)\n")
    cat("  → Recommendation: Use MEDIAN and IQR\n")
  }
  cat(sprintf("  Kurtosis:         %.4f ", kurtosis))
  if (abs(kurtosis) < 1) {
    cat("(similar to normal)\n\n")
  } else if (kurtosis > 0) {
    cat("(heavy tails)\n\n")
  } else {
    cat("(light tails)\n\n")
  }
  
  cat("Outlier Detection (1.5×IQR Rule):\n")
  cat(sprintf("  Lower fence:      %.4f\n", lower_fence))
  cat(sprintf("  Upper fence:      %.4f\n", upper_fence))
  cat(sprintf("  Outliers found:   %d (%.2f%% of data)\n", 
              n_outliers, n_outliers / n * 100))
  if (n_outliers > 0 && n_outliers <= 10) {
    cat("  Outlier values:  ", paste(sprintf("%.4f", outliers), collapse=", "), "\n")
  } else if (n_outliers > 10) {
    cat(sprintf("  (too many to display, showing extreme 5)\n"))
    cat("  Most extreme:    ", 
        paste(sprintf("%.4f", head(sort(outliers, decreasing = TRUE), 5)), collapse=", "), "\n")
  }
  cat("\n")
  
  cat(rep("=", 70), "\n\n")
  
  return(list(
    n = n,
    mean = mean_val,
    median = median_val,
    sd = sd_val,
    iqr = iqr_val,
    quantiles = c(q10, q25, median_val, q75, q90),
    skewness = skewness,
    kurtosis = kurtosis,
    outliers = outliers,
    n_outliers = n_outliers
  ))
}

# ============================================================================
# ANALYSIS 1: CHI-SQUARE GOODNESS OF FIT TEST
# ============================================================================
# Test if empirical FPR matches theoretical FPR

chi_square_fpr_test <- function(empirical_fp, n_tests, theoretical_fpr, alpha = 0.05) {
  cat("\n", rep("-", 70), "\n")
  cat("CHI-SQUARE GOODNESS OF FIT TEST\n")
  cat(rep("-", 70), "\n\n")
  
  # Observed counts
  obs_fp <- empirical_fp
  obs_tn <- n_tests - empirical_fp
  
  # Expected counts
  exp_fp <- n_tests * theoretical_fpr
  exp_tn <- n_tests * (1 - theoretical_fpr)
  
  # Chi-square statistic
  chi_sq <- ((obs_fp - exp_fp)^2 / exp_fp) + ((obs_tn - exp_tn)^2 / exp_tn)
  df <- 1
  p_value <- pchisq(chi_sq, df, lower.tail = FALSE)
  
  # Critical value
  critical_value <- qchisq(1 - alpha, df)
  
  cat(sprintf("H0: Empirical FPR = Theoretical FPR (%.6f)\n", theoretical_fpr))
  cat(sprintf("H1: Empirical FPR ≠ Theoretical FPR\n\n"))
  
  cat("Observed vs Expected:\n")
  cat(sprintf("  False Positives: %d (expected: %.2f)\n", obs_fp, exp_fp))
  cat(sprintf("  True Negatives: %d (expected: %.2f)\n\n", obs_tn, exp_tn))
  
  cat(sprintf("Chi-square statistic: %.4f\n", chi_sq))
  cat(sprintf("Degrees of freedom: %d\n", df))
  cat(sprintf("P-value: %.6f\n", p_value))
  cat(sprintf("Critical value (α=%.2f): %.4f\n", alpha, critical_value))
  cat(sprintf("\nDecision: %s H0\n", ifelse(p_value > alpha, "Accept", "Reject")))
  cat(sprintf("Conclusion: Empirical FPR %s significantly from theoretical\n",
              ifelse(p_value > alpha, "does NOT differ", "DIFFERS")))
  
  return(list(
    chi_square = chi_sq,
    df = df,
    p_value = p_value,
    reject_h0 = p_value < alpha
  ))
}

# ============================================================================
# ANALYSIS 2: TWO-SAMPLE T-TEST (PAIRED)
# ============================================================================
# Test if Bloom filters significantly reduce latency

paired_t_test_latency <- function(latencies_without, latencies_with, alpha = 0.05) {
  cat("\n", rep("-", 70), "\n")
  cat("PAIRED T-TEST: LATENCY COMPARISON\n")
  cat(rep("-", 70), "\n\n")
  
  # Calculate differences
  differences <- latencies_without - latencies_with
  
  # Perform paired t-test
  t_test <- t.test(latencies_without, latencies_with, paired = TRUE, 
                   alternative = "greater")
  
  # Effect size (Cohen's d)
  mean_diff <- mean(differences)
  sd_diff <- sd(differences)
  cohens_d <- mean_diff / sd_diff
  
  cat(sprintf("H0: μ_without ≤ μ_with (Bloom filters do NOT reduce latency)\n"))
  cat(sprintf("H1: μ_without > μ_with (Bloom filters DO reduce latency)\n\n"))
  
  cat("Sample statistics:\n")
  cat(sprintf("  n = %d request pairs\n", length(latencies_without)))
  cat(sprintf("  Mean latency without BF: %.4f ms\n", mean(latencies_without)))
  cat(sprintf("  Mean latency with BF: %.4f ms\n", mean(latencies_with)))
  cat(sprintf("  Mean difference: %.4f ms\n", mean_diff))
  cat(sprintf("  SD of differences: %.4f ms\n\n", sd_diff))
  
  cat(sprintf("T-statistic: %.4f\n", t_test$statistic))
  cat(sprintf("Degrees of freedom: %d\n", t_test$parameter))
  cat(sprintf("P-value: %.6e\n", t_test$p.value))
  cat(sprintf("95%% CI for difference: [%.4f, %.4f]\n", 
              t_test$conf.int[1], t_test$conf.int[2]))
  cat(sprintf("\nEffect size (Cohen's d): %.4f ", cohens_d))
  cat(sprintf("(%s)\n", ifelse(abs(cohens_d) < 0.2, "negligible",
                         ifelse(abs(cohens_d) < 0.5, "small",
                         ifelse(abs(cohens_d) < 0.8, "medium", "large")))))
  
  cat(sprintf("\nDecision: %s H0\n", ifelse(t_test$p.value < alpha, "Reject", "Accept")))
  cat(sprintf("Conclusion: Bloom filters %s reduce latency\n",
              ifelse(t_test$p.value < alpha, "SIGNIFICANTLY", "do NOT significantly")))
  
  return(list(
    t_statistic = t_test$statistic,
    df = t_test$parameter,
    p_value = t_test$p.value,
    mean_diff = mean_diff,
    ci = t_test$conf.int,
    cohens_d = cohens_d,
    reject_h0 = t_test$p.value < alpha
  ))
}

# ============================================================================
# ANALYSIS 3: CONFIDENCE INTERVALS FOR FPR
# ============================================================================

confidence_interval_fpr <- function(false_positives, n_tests, confidence = 0.95) {
  cat("\n", rep("-", 70), "\n")
  cat(sprintf("CONFIDENCE INTERVAL FOR FALSE POSITIVE RATE (%.0f%%)\n", confidence * 100))
  cat(rep("-", 70), "\n\n")
  
  # Point estimate
  p_hat <- false_positives / n_tests
  
  # Standard error
  se <- sqrt(p_hat * (1 - p_hat) / n_tests)
  
  # Z-score for confidence level
  z <- qnorm((1 + confidence) / 2)
  
  # Confidence interval (normal approximation)
  ci_lower <- p_hat - z * se
  ci_upper <- p_hat + z * se
  
  # Wilson score interval (better for extreme probabilities)
  wilson_center <- (p_hat + z^2 / (2 * n_tests)) / (1 + z^2 / n_tests)
  wilson_half_width <- z * sqrt(p_hat * (1 - p_hat) / n_tests + z^2 / (4 * n_tests^2)) / 
                       (1 + z^2 / n_tests)
  wilson_lower <- wilson_center - wilson_half_width
  wilson_upper <- wilson_center + wilson_half_width
  
  cat(sprintf("Sample size: %d\n", n_tests))
  cat(sprintf("False positives: %d\n", false_positives))
  cat(sprintf("Point estimate: %.6f\n\n", p_hat))
  
  cat("Normal Approximation:\n")
  cat(sprintf("  Standard error: %.6f\n", se))
  cat(sprintf("  %.0f%% CI: [%.6f, %.6f]\n", confidence * 100, ci_lower, ci_upper))
  cat(sprintf("  Margin of error: ±%.6f\n\n", z * se))
  
  cat("Wilson Score Interval (recommended for small p):\n")
  cat(sprintf("  %.0f%% CI: [%.6f, %.6f]\n", confidence * 100, wilson_lower, wilson_upper))
  
  return(list(
    point_estimate = p_hat,
    se = se,
    ci_normal = c(ci_lower, ci_upper),
    ci_wilson = c(wilson_lower, wilson_upper),
    margin_of_error = z * se
  ))
}

# ============================================================================
# ANALYSIS 4: ANOVA FOR MULTIPLE SCENARIOS
# ============================================================================

anova_cache_scenarios <- function(exp3_results) {
  cat("\n", rep("-", 70), "\n")
  cat("ONE-WAY ANOVA: CACHE PERFORMANCE ACROSS SCENARIOS\n")
  cat(rep("-", 70), "\n\n")
  
  # Prepare data
  scenarios <- names(exp3_results)
  all_data <- data.frame()
  
  for (scenario in scenarios) {
    result <- exp3_results[[scenario]]
    df <- data.frame(
      Latency = result$latencies_with_bloom,
      Scenario = scenario
    )
    all_data <- rbind(all_data, df)
  }
  
  # Perform ANOVA
  anova_result <- aov(Latency ~ Scenario, data = all_data)
  anova_summary <- summary(anova_result)
  
  cat(sprintf("H0: All scenarios have equal mean latency\n"))
  cat(sprintf("H1: At least one scenario differs\n\n"))
  
  print(anova_summary)
  
  # Post-hoc Tukey HSD test
  cat("\n\nTukey HSD Post-hoc Test:\n")
  cat(rep("-", 70), "\n")
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  
  # Effect size (eta squared)
  ss_total <- sum((all_data$Latency - mean(all_data$Latency))^2)
  ss_between <- sum(anova_summary[[1]]$"Sum Sq"[1])
  eta_squared <- ss_between / ss_total
  
  cat(sprintf("\nEffect size (η²): %.4f ", eta_squared))
  cat(sprintf("(%s effect)\n", ifelse(eta_squared < 0.01, "negligible",
                              ifelse(eta_squared < 0.06, "small",
                              ifelse(eta_squared < 0.14, "medium", "large")))))
  
  return(list(
    anova = anova_summary,
    tukey = tukey_result,
    eta_squared = eta_squared
  ))
}

# ============================================================================
# ANALYSIS 5: CORRELATION ANALYSIS
# ============================================================================

correlation_analysis <- function(exp4_results) {
  cat("\n", rep("-", 70), "\n")
  cat("CORRELATION ANALYSIS: FILL RATIO vs FPR\n")
  cat(rep("-", 70), "\n\n")
  
  # Extract data
  fill_ratios <- sapply(exp4_results, function(x) x$fill_ratio)
  empirical_fprs <- sapply(exp4_results, function(x) x$empirical_fpr)
  
  # Pearson correlation
  cor_test <- cor.test(fill_ratios, empirical_fprs, method = "pearson")
  
  # Spearman correlation (non-parametric)
  cor_test_spearman <- cor.test(fill_ratios, empirical_fprs, method = "spearman")
  
  cat("Pearson Correlation:\n")
  cat(sprintf("  r = %.4f\n", cor_test$estimate))
  cat(sprintf("  t = %.4f, df = %d\n", cor_test$statistic, cor_test$parameter))
  cat(sprintf("  p-value = %.6e\n", cor_test$p.value))
  cat(sprintf("  95%% CI: [%.4f, %.4f]\n\n", cor_test$conf.int[1], cor_test$conf.int[2]))
  
  cat("Spearman Correlation (rank-based):\n")
  cat(sprintf("  ρ = %.4f\n", cor_test_spearman$estimate))
  cat(sprintf("  p-value = %.6e\n\n", cor_test_spearman$p.value))
  
  # Interpretation
  r <- abs(cor_test$estimate)
  strength <- ifelse(r < 0.3, "weak", 
                     ifelse(r < 0.7, "moderate", "strong"))
  
  cat(sprintf("Interpretation: %s positive correlation\n", strength))
  cat("As fill ratio increases, false positive rate increases\n")
  
  return(list(
    pearson = cor_test,
    spearman = cor_test_spearman
  ))
}

# ============================================================================
# ANALYSIS 6: REGRESSION ANALYSIS
# ============================================================================

regression_analysis_fpr <- function() {
  cat("\n", rep("-", 70), "\n")
  cat("REGRESSION ANALYSIS: FPR vs m/n RELATIONSHIP\n")
  cat(rep("-", 70), "\n\n")
  
  # Simulate data
  set.seed(42)
  n <- 1000
  m_over_n_values <- seq(5, 15, by = 0.5)
  
  data <- data.frame()
  
  for (m_over_n in m_over_n_values) {
    m <- n * m_over_n
    k <- max(1, round(m_over_n * log(2)))
    
    # Create filter and test
    bf <- BloomFilter$new(n, 0.01, size = m, num_hash = k)
    elements <- paste0("el_", 1:n)
    bf$add_all(elements)
    
    # Measure empirical FPR
    test_elements <- paste0("test_", 1:1000)
    empirical_fpr <- sum(bf$contains_all(test_elements)) / 1000
    theoretical_fpr <- bf$current_fpr()
    
    # Avoid log(0) by adding small constant
    empirical_fpr <- max(empirical_fpr, 1e-10)
    theoretical_fpr <- max(theoretical_fpr, 1e-10)
    
    data <- rbind(data, data.frame(
      m_over_n = m_over_n,
      log_empirical_fpr = log(empirical_fpr),
      log_theoretical_fpr = log(theoretical_fpr)
    ))
  }
  
  # Remove any rows with NA, NaN, or Inf
  data <- data[is.finite(data$log_empirical_fpr) & is.finite(data$m_over_n), ]
  
  # Linear regression on log scale
  if (nrow(data) > 2) {
    model <- lm(log_empirical_fpr ~ m_over_n, data = data)
    summary_model <- summary(model)
  } else {
    cat("Not enough valid data points for regression\n")
    return(NULL)
  }
  
  cat("Linear Model: log(FPR) ~ m/n\n\n")
  print(summary_model)
  
  cat(sprintf("\nR-squared: %.4f\n", summary_model$r.squared))
  cat(sprintf("Adjusted R-squared: %.4f\n", summary_model$adj.r.squared))
  cat(sprintf("F-statistic: %.2f, p-value: %.6e\n",
              summary_model$fstatistic[1],
              pf(summary_model$fstatistic[1], 
                 summary_model$fstatistic[2], 
                 summary_model$fstatistic[3], 
                 lower.tail = FALSE)))
  
  cat("\nInterpretation:\n")
  cat(sprintf("  For each additional bit per element, log(FPR) changes by %.4f\n",
              coef(model)[2]))
  cat(sprintf("  Model explains %.1f%% of variance in log(FPR)\n",
              summary_model$r.squared * 100))
  
  return(list(
    model = model,
    summary = summary_model,
    data = data
  ))
}

# ============================================================================
# COMPREHENSIVE STATISTICAL REPORT
# ============================================================================

generate_statistical_report <- function(experimental_results) {
  cat("\n", rep("=", 80), "\n")
  cat("COMPREHENSIVE STATISTICAL ANALYSIS REPORT\n")
  cat(rep("=", 80), "\n")
  
  all_stats <- list()
  
  # DESCRIPTIVE STATISTICS (Required by report structure)
  cat("\n\n### DESCRIPTIVE STATISTICS ###\n")
  
  # Latency statistics - Without Bloom Filter
  all_latencies_no_bloom <- unlist(lapply(experimental_results$exp3_cache, 
                                          function(x) x$latencies_no_bloom))
  all_stats$descriptive_no_bloom <- comprehensive_descriptive_stats(
    all_latencies_no_bloom, 
    "Latency WITHOUT Bloom Filter (ms)"
  )
  
  # Latency statistics - With Bloom Filter
  all_latencies_with_bloom <- unlist(lapply(experimental_results$exp3_cache, 
                                            function(x) x$latencies_with_bloom))
  all_stats$descriptive_with_bloom <- comprehensive_descriptive_stats(
    all_latencies_with_bloom, 
    "Latency WITH Bloom Filter (ms)"
  )
  
  # False Positive Rate statistics
  all_empirical_fprs <- sapply(experimental_results$exp1_fpr, 
                               function(x) x$empirical_fpr)
  all_stats$descriptive_fpr <- comprehensive_descriptive_stats(
    all_empirical_fprs, 
    "Empirical False Positive Rate"
  )
  
  # Analysis 1: Chi-square tests for FPR validation
  cat("\n\n### ANALYSIS 1: FALSE POSITIVE RATE VALIDATION ###\n")
  all_stats$chi_square_tests <- list()
  for (name in names(experimental_results$exp1_fpr)) {
    cat(sprintf("\n--- Configuration: %s ---\n", name))
    result <- experimental_results$exp1_fpr[[name]]
    test <- chi_square_fpr_test(result$false_positives, result$n_tests, 
                                 result$theoretical_fpr)
    all_stats$chi_square_tests[[name]] <- test
  }
  
  # Analysis 2: Paired t-test for cache performance
  cat("\n\n### ANALYSIS 2: CACHE LATENCY COMPARISON ###\n")
  all_stats$t_tests <- list()
  for (name in names(experimental_results$exp3_cache)) {
    cat(sprintf("\n--- Scenario: %s ---\n", name))
    result <- experimental_results$exp3_cache[[name]]
    test <- paired_t_test_latency(result$latencies_no_bloom, 
                                   result$latencies_with_bloom)
    all_stats$t_tests[[name]] <- test
  }
  
  # Analysis 3: Confidence intervals
  cat("\n\n### ANALYSIS 3: CONFIDENCE INTERVALS ###\n")
  result <- experimental_results$exp1_fpr[[1]]
  all_stats$confidence_intervals <- confidence_interval_fpr(
    result$false_positives, result$n_tests
  )
  
  # Analysis 4: ANOVA
  cat("\n\n### ANALYSIS 4: ANOVA ACROSS SCENARIOS ###\n")
  all_stats$anova <- anova_cache_scenarios(experimental_results$exp3_cache)
  
  # Analysis 5: Correlation
  cat("\n\n### ANALYSIS 5: CORRELATION ANALYSIS ###\n")
  all_stats$correlation <- correlation_analysis(experimental_results$exp4_scale)
  
  # Analysis 6: Regression
  cat("\n\n### ANALYSIS 6: REGRESSION ANALYSIS ###\n")
  all_stats$regression <- regression_analysis_fpr()
  
  cat("\n\n", rep("=", 80), "\n")
  cat("STATISTICAL ANALYSIS COMPLETED\n")
  cat(rep("=", 80), "\n\n")
  
  # Save results
  saveRDS(all_stats, "statistical_analysis.rds")
  cat("Results saved to: statistical_analysis.rds\n\n")
  
  return(all_stats)
}

cat("Statistical analysis module loaded successfully!\n")
cat("Run: stats <- generate_statistical_report(experimental_results)\n")
