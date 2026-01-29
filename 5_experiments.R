# Experimental Suite
# Bloom filter analysis and cache optimization experiments

source("2_bloom_filter.R")
source("3_hash_functions.R")
source("4_cache_simulator.R")

# ============================================================================
# EXPERIMENT 1: FALSE POSITIVE RATE VALIDATION
# ============================================================================
# Validate theoretical FPR formula empirically

experiment_1_fpr_validation <- function() {
  cat("\n", rep("=", 80), "\n")
  cat("EXPERIMENT 1: FALSE POSITIVE RATE VALIDATION\n")
  cat(rep("=", 80), "\n\n")
  
  cat("Objective: Verify that empirical false positive rates match\n")
  cat("           theoretical predictions from formula: (1 - e^(-kn/m))^k\n\n")
  
  # Test parameters
  test_configs <- list(
    list(n = 500, fpr = 0.01, name = "Small-Low"),
    list(n = 500, fpr = 0.05, name = "Small-High"),
    list(n = 5000, fpr = 0.01, name = "Large-Low"),
    list(n = 5000, fpr = 0.05, name = "Large-High")
  )
  
  results <- list()
  
  for (config in test_configs) {
    cat(sprintf("Testing configuration: %s (n=%d, target FPR=%.4f)\n",
                config$name, config$n, config$fpr))
    
    # Create Bloom filter
    bf <- BloomFilter$new(config$n, config$fpr)
    
    # Add n elements
    elements <- paste0("element_", 1:config$n)
    bf$add_all(elements)
    
    # Test with non-existent elements
    n_tests <- 10000
    test_elements <- paste0("test_", 1:n_tests)
    test_results <- bf$contains_all(test_elements)
    
    false_positives <- sum(test_results)
    empirical_fpr <- false_positives / n_tests
    theoretical_fpr <- bf$current_fpr()
    
    # Statistical test
    # H0: empirical FPR = theoretical FPR
    # Using normal approximation for binomial
    se <- sqrt(theoretical_fpr * (1 - theoretical_fpr) / n_tests)
    z_score <- (empirical_fpr - theoretical_fpr) / se
    p_value <- 2 * pnorm(-abs(z_score))
    
    # 95% confidence interval
    ci_lower <- empirical_fpr - 1.96 * sqrt(empirical_fpr * (1 - empirical_fpr) / n_tests)
    ci_upper <- empirical_fpr + 1.96 * sqrt(empirical_fpr * (1 - empirical_fpr) / n_tests)
    
    cat(sprintf("  False positives: %d / %d\n", false_positives, n_tests))
    cat(sprintf("  Empirical FPR: %.6f\n", empirical_fpr))
    cat(sprintf("  Theoretical FPR: %.6f\n", theoretical_fpr))
    cat(sprintf("  Difference: %.6f (%.2f%%)\n", 
                abs(empirical_fpr - theoretical_fpr),
                abs(empirical_fpr - theoretical_fpr) / theoretical_fpr * 100))
    cat(sprintf("  95%% CI: [%.6f, %.6f]\n", ci_lower, ci_upper))
    cat(sprintf("  Z-score: %.4f, p-value: %.4f\n", z_score, p_value))
    cat(sprintf("  Match: %s\n\n", ifelse(p_value > 0.05, "YES (accept H0)", "NO (reject H0)")))
    
    results[[config$name]] <- list(
      config = config,
      n = config$n,
      target_fpr = config$fpr,
      empirical_fpr = empirical_fpr,
      theoretical_fpr = theoretical_fpr,
      false_positives = false_positives,
      n_tests = n_tests,
      z_score = z_score,
      p_value = p_value,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      filter_stats = bf$stats()
    )
  }
  
  cat(rep("=", 80), "\n")
  cat("EXPERIMENT 1 COMPLETED\n")
  cat(rep("=", 80), "\n\n")
  
  return(results)
}

# ============================================================================
# EXPERIMENT 2: PARAMETER OPTIMIZATION
# ============================================================================
# Find optimal m and k for different scenarios

experiment_2_parameter_optimization <- function() {
  cat("\n", rep("=", 80), "\n")
  cat("EXPERIMENT 2: PARAMETER OPTIMIZATION\n")
  cat(rep("=", 80), "\n\n")
  
  cat("Objective: Test various combinations of m (size) and k (hash count)\n")
  cat("           to find optimal parameters for different target FPRs\n\n")
  
  n <- 1000  # Fixed number of elements
  target_fprs <- c(0.001, 0.01, 0.05, 0.1)
  
  results <- list()
  
  for (target_fpr in target_fprs) {
    cat(sprintf("Testing target FPR: %.4f\n", target_fpr))
    
    # Create filter with optimal parameters
    bf <- BloomFilter$new(n, target_fpr)
    
    # Add elements
    elements <- paste0("element_", 1:n)
    bf$add_all(elements)
    
    # Measure empirical FPR
    n_tests <- 10000
    test_elements <- paste0("test_", 1:n_tests)
    test_results <- bf$contains_all(test_elements)
    empirical_fpr <- sum(test_results) / n_tests
    
    stats <- bf$stats()
    
    cat(sprintf("  Optimal m: %d bits (%.2f KB)\n", stats$size_bits, stats$size_kb))
    cat(sprintf("  Optimal k: %d\n", stats$num_hash))
    cat(sprintf("  Bits per element: %.2f\n", stats$bits_per_element))
    cat(sprintf("  Empirical FPR: %.6f\n", empirical_fpr))
    cat(sprintf("  Error: %.2f%%\n\n", 
                abs(empirical_fpr - target_fpr) / target_fpr * 100))
    
    results[[as.character(target_fpr)]] <- list(
      target_fpr = target_fpr,
      m = stats$size_bits,
      k = stats$num_hash,
      bits_per_element = stats$bits_per_element,
      empirical_fpr = empirical_fpr,
      error_percent = abs(empirical_fpr - target_fpr) / target_fpr * 100
    )
  }
  
  # Now test sub-optimal parameters
  cat("\n--- Testing Sub-optimal Parameters ---\n\n")
  
  target_fpr <- 0.01
  test_k_values <- c(3, 5, 7, 10, 15)
  
  suboptimal_results <- list()
  
  for (k in test_k_values) {
    bf <- BloomFilter$new(n, target_fpr)
    optimal_k <- bf$num_hash
    
    # Create filter with specific k
    m <- bf$size
    bf_test <- BloomFilter$new(n, target_fpr, size = m, num_hash = k)
    
    elements <- paste0("element_", 1:n)
    bf_test$add_all(elements)
    
    n_tests <- 10000
    test_elements <- paste0("test_", 1:n_tests)
    empirical_fpr <- sum(bf_test$contains_all(test_elements)) / n_tests
    
    cat(sprintf("k=%d (optimal=%d): Empirical FPR=%.6f\n", 
                k, optimal_k, empirical_fpr))
    
    suboptimal_results[[as.character(k)]] <- list(
      k = k,
      optimal_k = optimal_k,
      empirical_fpr = empirical_fpr
    )
  }
  
  cat("\n", rep("=", 80), "\n")
  cat("EXPERIMENT 2 COMPLETED\n")
  cat(rep("=", 80), "\n\n")
  
  return(list(
    optimal = results,
    suboptimal_k = suboptimal_results
  ))
}

# ============================================================================
# EXPERIMENT 3: CACHE PERFORMANCE COMPARISON
# ============================================================================
# Compare cache performance with and without Bloom filters

experiment_3_cache_performance <- function() {
  cat("\n", rep("=", 80), "\n")
  cat("EXPERIMENT 3: CACHE PERFORMANCE COMPARISON\n")
  cat(rep("=", 80), "\n\n")
  
  cat("Objective: Measure latency improvement and hit rate changes\n")
  cat("           when using Bloom filters in distributed cache\n\n")
  
  # Test different workload characteristics
  test_scenarios <- list(
    list(alpha = 0.5, name = "Low Skew", n_requests = 5000, n_items = 5000),
    list(alpha = 1.0, name = "Medium Skew", n_requests = 5000, n_items = 5000),
    list(alpha = 1.5, name = "High Skew", n_requests = 5000, n_items = 5000),
    list(alpha = 2.0, name = "Very High Skew", n_requests = 5000, n_items = 5000)
  )
  
  # Cache configuration
  tier_config <- list(
    list(name = "L1", capacity = 50, access_time_ms = 0.5, fpr = 0.01),
    list(name = "L2", capacity = 200, access_time_ms = 5, fpr = 0.01),
    list(name = "L3", capacity = 1000, access_time_ms = 25, fpr = 0.01)
  )
  
  results <- list()
  
  for (scenario in test_scenarios) {
    cat(sprintf("\n--- Scenario: %s (alpha=%.1f) ---\n", scenario$name, scenario$alpha))
    
    # Generate workload
    workload <- generate_zipf_workload(scenario$n_requests, scenario$n_items, scenario$alpha)
    
    # Test without Bloom filters
    cat("\nWithout Bloom filters:\n")
    cache_no_bloom <- DistributedCacheSystem$new(tier_config, origin_time_ms = 100, use_bloom = FALSE)
    result_no_bloom <- cache_no_bloom$simulate(workload)
    stats_no_bloom <- cache_no_bloom$stats()
    
    # Test with Bloom filters
    cat("\nWith Bloom filters:\n")
    cache_with_bloom <- DistributedCacheSystem$new(tier_config, origin_time_ms = 100, use_bloom = TRUE)
    result_with_bloom <- cache_with_bloom$simulate(workload)
    stats_with_bloom <- cache_with_bloom$stats()
    
    # Calculate improvement
    latency_improvement <- ((stats_no_bloom$avg_latency_ms - stats_with_bloom$avg_latency_ms) / 
                            stats_no_bloom$avg_latency_ms) * 100
    
    cat(sprintf("\nResults:\n"))
    cat(sprintf("  Avg latency without BF: %.3f ms\n", stats_no_bloom$avg_latency_ms))
    cat(sprintf("  Avg latency with BF: %.3f ms\n", stats_with_bloom$avg_latency_ms))
    cat(sprintf("  Improvement: %.2f%%\n", latency_improvement))
    cat(sprintf("  Origin fetches without BF: %d (%.2f%%)\n", 
                stats_no_bloom$origin_fetches, stats_no_bloom$origin_rate * 100))
    cat(sprintf("  Origin fetches with BF: %d (%.2f%%)\n",
                stats_with_bloom$origin_fetches, stats_with_bloom$origin_rate * 100))
    
    results[[scenario$name]] <- list(
      scenario = scenario,
      no_bloom = stats_no_bloom,
      with_bloom = stats_with_bloom,
      latency_improvement_percent = latency_improvement,
      latencies_no_bloom = result_no_bloom$latencies,
      latencies_with_bloom = result_with_bloom$latencies
    )
  }
  
  cat("\n", rep("=", 80), "\n")
  cat("EXPERIMENT 3 COMPLETED\n")
  cat(rep("=", 80), "\n\n")
  
  return(results)
}

# ============================================================================
# EXPERIMENT 4: SCALABILITY ANALYSIS
# ============================================================================
# Test how Bloom filter performance scales with size

experiment_4_scalability <- function() {
  cat("\n", rep("=", 80), "\n")
  cat("EXPERIMENT 4: SCALABILITY ANALYSIS\n")
  cat(rep("=", 80), "\n\n")
  
  cat("Objective: Measure how FPR and performance scale with\n")
  cat("           increasing number of elements\n\n")
  
  target_fpr <- 0.01
  expected_n <- 1000
  test_n_values <- c(500, 1000, 2000, 5000, 10000)  # Including overload
  
  results <- list()
  
  for (n in test_n_values) {
    cat(sprintf("Testing with n=%d elements (expected=%d, %.1f%% capacity)\n",
                n, expected_n, (n / expected_n) * 100))
    
    # Create filter designed for expected_n
    bf <- BloomFilter$new(expected_n, target_fpr)
    
    # Add n elements (may exceed capacity)
    elements <- paste0("element_", 1:n)
    bf$add_all(elements)
    
    # Measure empirical FPR
    n_tests <- 10000
    test_elements <- paste0("test_", 1:n_tests)
    empirical_fpr <- sum(bf$contains_all(test_elements)) / n_tests
    
    theoretical_fpr <- bf$current_fpr()
    fill_ratio <- bf$fill_ratio()
    
    cat(sprintf("  Fill ratio: %.4f (%.1f%% bits set)\n", fill_ratio, fill_ratio * 100))
    cat(sprintf("  Theoretical FPR: %.6f\n", theoretical_fpr))
    cat(sprintf("  Empirical FPR: %.6f\n", empirical_fpr))
    cat(sprintf("  FPR increase: %.2fx from target\n\n", empirical_fpr / target_fpr))
    
    results[[as.character(n)]] <- list(
      n = n,
      expected_n = expected_n,
      capacity_percent = (n / expected_n) * 100,
      fill_ratio = fill_ratio,
      theoretical_fpr = theoretical_fpr,
      empirical_fpr = empirical_fpr,
      fpr_multiplier = empirical_fpr / target_fpr
    )
  }
  
  cat(rep("=", 80), "\n")
  cat("EXPERIMENT 4 COMPLETED\n")
  cat(rep("=", 80), "\n\n")
  
  return(results)
}

# ============================================================================
# EXPERIMENT 5: HASH FUNCTION COMPARISON
# ============================================================================
# Compare different hash functions for Bloom filters

experiment_5_hash_comparison <- function() {
  cat("\n", rep("=", 80), "\n")
  cat("EXPERIMENT 5: HASH FUNCTION COMPARISON\n")
  cat(rep("=", 80), "\n\n")
  
  cat("Objective: Compare uniformity and performance of different\n")
  cat("           hash functions in Bloom filter context\n\n")
  
  m <- 1000
  n <- 5000
  
  hash_functions <- list(
    "Simple" = simple_hash,
    "MurmurHash3" = murmur_hash,
    "xxHash" = xxhash_hash,
    "MD5" = md5_hash
  )
  
  results <- list()
  
  for (name in names(hash_functions)) {
    cat(sprintf("Testing %s:\n", name))
    
    hash_func <- hash_functions[[name]]
    
    # Test uniformity
    uniformity <- test_hash_uniformity(hash_func, m, n)
    
    # Test collisions  
    collisions <- test_collision_rate(hash_func, m, n)
    
    results[[name]] <- list(
      name = name,
      cv = uniformity$cv,
      chi_square = uniformity$chi_square,
      p_value = uniformity$p_value,
      uniform = uniformity$uniform,
      collision_rate = collisions$collision_rate
    )
    
    cat("\n")
  }
  
  # Test double hashing
  cat("Testing double hashing technique:\n")
  double_hash_results <- test_double_hashing(m = 1000, k = 7, n = 5000)
  
  cat("\n", rep("=", 80), "\n")
  cat("EXPERIMENT 5 COMPLETED\n")
  cat(rep("=", 80), "\n\n")
  
  return(list(
    hash_comparison = results,
    double_hashing = double_hash_results
  ))
}

# ============================================================================
# RUN ALL EXPERIMENTS
# ============================================================================

run_all_experiments <- function() {
  cat("\n", rep("#", 80), "\n")
  cat("#", rep(" ", 78), "#\n")
  cat("#", " ", "COMPREHENSIVE EXPERIMENTAL SUITE", rep(" ", 44), "#\n")
  cat("#", " ", "Bloom Filters for Distributed Cache Optimization", rep(" ", 29), "#\n")
  cat("#", rep(" ", 78), "#\n")
  cat(rep("#", 80), "\n")
  
  # Store all results
  all_results <- list()
  
  # Run experiments
  all_results$exp1_fpr <- experiment_1_fpr_validation()
  all_results$exp2_params <- experiment_2_parameter_optimization()
  all_results$exp3_cache <- experiment_3_cache_performance()
  all_results$exp4_scale <- experiment_4_scalability()
  all_results$exp5_hash <- experiment_5_hash_comparison()
  
  # Save results
  saveRDS(all_results, "experimental_results.rds")
  
  cat("\n", rep("#", 80), "\n")
  cat("ALL EXPERIMENTS COMPLETED\n")
  cat("Results saved to: experimental_results.rds\n")
  cat(rep("#", 80), "\n\n")
  
  return(all_results)
}

# ============================================================================
# LOAD RESULTS (if needed)
# ============================================================================

load_experimental_results <- function() {
  if (file.exists("experimental_results.rds")) {
    return(readRDS("experimental_results.rds"))
  } else {
    warning("No experimental results file found. Run experiments first.")
    return(NULL)
  }
}

cat("Experimental suite loaded successfully!\n")
cat("Run: results <- run_all_experiments()\n")
