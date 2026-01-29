# Hash Function Analysis
# Implementation and comparison of hash functions

library(digest)
library(microbenchmark)

# ============================================================================
# HASH FUNCTION IMPLEMENTATIONS
# ============================================================================

# ----------------------------------------------------------------------------
# 1. Simple Modulo Hash (baseline - not recommended for production)
# ----------------------------------------------------------------------------
simple_hash <- function(item, m, seed = 0) {
  item_str <- as.character(item)
  # Simple character code sum
  char_sum <- sum(utf8ToInt(item_str)) + seed
  return(char_sum %% m)
}

# ----------------------------------------------------------------------------
# 2. MurmurHash3 (via digest package)
# ----------------------------------------------------------------------------
murmur_hash <- function(item, m, seed = 0) {
  item_str <- paste0(as.character(item), seed)
  hash_val <- digest(item_str, algo = "murmur32", serialize = FALSE)
  hash_int <- strtoi(hash_val, base = 16)
  return(abs(hash_int) %% m)
}

# ----------------------------------------------------------------------------
# 3. xxHash (fast, high quality)
# ----------------------------------------------------------------------------
xxhash_hash <- function(item, m, seed = 0) {
  item_str <- paste0(as.character(item), seed)
  hash_val <- digest(item_str, algo = "xxhash64", serialize = FALSE)
  # Take first 15 characters to avoid overflow
  hash_int <- strtoi(substr(hash_val, 1, 15), base = 16)
  return(abs(hash_int) %% m)
}

# ----------------------------------------------------------------------------
# 4. MD5 (cryptographic - slower but very uniform)
# ----------------------------------------------------------------------------
md5_hash <- function(item, m, seed = 0) {
  item_str <- paste0(as.character(item), seed)
  hash_val <- digest(item_str, algo = "md5", serialize = FALSE)
  hash_int <- strtoi(substr(hash_val, 1, 15), base = 16)
  return(abs(hash_int) %% m)
}

# ----------------------------------------------------------------------------
# 5. SHA256 (cryptographic - most secure but slowest)
# ----------------------------------------------------------------------------
sha256_hash <- function(item, m, seed = 0) {
  item_str <- paste0(as.character(item), seed)
  hash_val <- digest(item_str, algo = "sha256", serialize = FALSE)
  hash_int <- strtoi(substr(hash_val, 1, 15), base = 16)
  return(abs(hash_int) %% m)
}

# ============================================================================
# HASH FUNCTION TESTING
# ============================================================================

# ----------------------------------------------------------------------------
# Test uniformity of hash distribution
# ----------------------------------------------------------------------------
test_hash_uniformity <- function(hash_func, m = 1000, n = 10000, seed = 42) {
  cat(sprintf("\nTesting uniformity for hash function (m=%d, n=%d)...\n", m, n))
  
  # Generate random items
  set.seed(seed)
  items <- paste0("item_", 1:n)
  
  # Hash all items
  hash_values <- sapply(items, function(x) hash_func(x, m, 0))
  
  # Count distribution across buckets
  bucket_counts <- table(factor(hash_values, levels = 0:(m-1)))
  bucket_counts <- as.vector(bucket_counts)
  
  # Expected count per bucket (uniform distribution)
  expected <- n / m
  
  # Chi-square test for uniformity
  chi_sq <- sum((bucket_counts - expected)^2 / expected)
  df <- m - 1
  p_value <- pchisq(chi_sq, df, lower.tail = FALSE)
  
  # Statistics
  mean_count <- mean(bucket_counts)
  sd_count <- sd(bucket_counts)
  cv <- sd_count / mean_count  # Coefficient of variation
  
  # Results
  results <- list(
    mean = mean_count,
    sd = sd_count,
    cv = cv,
    chi_square = chi_sq,
    df = df,
    p_value = p_value,
    uniform = p_value > 0.05,  # If TRUE, distribution is uniform
    bucket_counts = bucket_counts
  )
  
  cat(sprintf("  Mean count per bucket: %.2f (expected: %.2f)\n", mean_count, expected))
  cat(sprintf("  Std deviation: %.2f\n", sd_count))
  cat(sprintf("  Coefficient of variation: %.4f\n", cv))
  cat(sprintf("  Chi-square: %.2f (df=%d)\n", chi_sq, df))
  cat(sprintf("  P-value: %.6f\n", p_value))
  cat(sprintf("  Uniform? %s\n", ifelse(results$uniform, "YES", "NO")))
  
  return(results)
}

# ----------------------------------------------------------------------------
# Test collision rate
# ----------------------------------------------------------------------------
test_collision_rate <- function(hash_func, m = 1000, n = 10000, seed = 42) {
  cat(sprintf("\nTesting collision rate (m=%d, n=%d)...\n", m, n))
  
  set.seed(seed)
  items <- paste0("item_", 1:n)
  
  # Hash all items
  hash_values <- sapply(items, function(x) hash_func(x, m, 0))
  
  # Count unique hash values
  unique_hashes <- length(unique(hash_values))
  collisions <- n - unique_hashes
  collision_rate <- collisions / n
  
  # Expected collisions (birthday paradox)
  expected_unique <- m * (1 - exp(-n/m))
  expected_collisions <- n - expected_unique
  
  cat(sprintf("  Unique hashes: %d / %d\n", unique_hashes, n))
  cat(sprintf("  Collisions: %d (%.2f%%)\n", collisions, collision_rate * 100))
  cat(sprintf("  Expected collisions: %.0f (%.2f%%)\n", 
              expected_collisions, (expected_collisions/n) * 100))
  
  return(list(
    unique = unique_hashes,
    collisions = collisions,
    collision_rate = collision_rate,
    expected_collisions = expected_collisions
  ))
}

# ----------------------------------------------------------------------------
# Benchmark hash function speed
# ----------------------------------------------------------------------------
benchmark_hash_functions <- function(n = 1000) {
  cat(sprintf("\nBenchmarking hash functions (%d iterations)...\n", n))
  
  items <- paste0("item_", 1:n)
  m <- 10000
  
  results <- microbenchmark(
    simple = sapply(items, function(x) simple_hash(x, m)),
    murmur = sapply(items, function(x) murmur_hash(x, m)),
    xxhash = sapply(items, function(x) xxhash_hash(x, m)),
    md5 = sapply(items, function(x) md5_hash(x, m)),
    sha256 = sapply(items, function(x) sha256_hash(x, m)),
    times = 10
  )
  
  print(results)
  return(results)
}

# ============================================================================
# COMPREHENSIVE HASH COMPARISON
# ============================================================================

compare_all_hash_functions <- function(m = 1000, n = 10000) {
  cat("\n", rep("=", 70), "\n")
  cat("COMPREHENSIVE HASH FUNCTION COMPARISON\n")
  cat(rep("=", 70), "\n")
  
  hash_functions <- list(
    "Simple (Modulo)" = simple_hash,
    "MurmurHash3" = murmur_hash,
    "xxHash" = xxhash_hash,
    "MD5" = md5_hash,
    "SHA-256" = sha256_hash
  )
  
  results <- list()
  
  for (name in names(hash_functions)) {
    cat("\n", rep("-", 70), "\n")
    cat(sprintf("TESTING: %s\n", name))
    cat(rep("-", 70), "\n")
    
    hash_func <- hash_functions[[name]]
    
    # Test uniformity
    uniformity <- test_hash_uniformity(hash_func, m, n)
    
    # Test collisions
    collisions <- test_collision_rate(hash_func, m, n)
    
    results[[name]] <- list(
      uniformity = uniformity,
      collisions = collisions
    )
  }
  
  cat("\n", rep("=", 70), "\n")
  cat("SUMMARY TABLE\n")
  cat(rep("=", 70), "\n\n")
  
  # Create summary data frame
  summary_df <- data.frame(
    Hash_Function = names(results),
    Mean_Count = sapply(results, function(x) x$uniformity$mean),
    CV = sapply(results, function(x) x$uniformity$cv),
    Chi_Square = sapply(results, function(x) x$uniformity$chi_square),
    P_Value = sapply(results, function(x) x$uniformity$p_value),
    Uniform = sapply(results, function(x) x$uniformity$uniform),
    Collision_Rate = sapply(results, function(x) x$collisions$collision_rate * 100)
  )
  
  print(summary_df, row.names = FALSE)
  
  return(results)
}

# ============================================================================
# AVALANCHE EFFECT TEST
# ============================================================================
# Tests if small changes in input cause large changes in hash output

test_avalanche_effect <- function(hash_func, m = 1000) {
  cat("\nTesting avalanche effect...\n")
  
  base_string <- "test_string_12345"
  base_hash <- hash_func(base_string, m, 0)
  
  # Test changing one character
  changed_hashes <- numeric(nchar(base_string))
  
  for (i in 1:nchar(base_string)) {
    # Change one character
    modified <- base_string
    substr(modified, i, i) <- "X"
    changed_hashes[i] <- hash_func(modified, m, 0)
  }
  
  # Calculate how different the hashes are
  differences <- abs(changed_hashes - base_hash)
  mean_diff <- mean(differences)
  expected_diff <- m / 2  # Expected for uniform random changes
  
  avalanche_quality <- mean_diff / expected_diff
  
  cat(sprintf("  Base hash: %d\n", base_hash))
  cat(sprintf("  Mean difference: %.2f\n", mean_diff))
  cat(sprintf("  Expected difference: %.2f\n", expected_diff))
  cat(sprintf("  Avalanche quality: %.4f (1.0 = ideal)\n", avalanche_quality))
  cat(sprintf("  Assessment: %s\n", 
              ifelse(avalanche_quality > 0.7, "GOOD", 
                     ifelse(avalanche_quality > 0.4, "FAIR", "POOR"))))
  
  return(list(
    mean_difference = mean_diff,
    expected_difference = expected_diff,
    quality = avalanche_quality
  ))
}

# ============================================================================
# DOUBLE HASHING VALIDATION
# ============================================================================
# Verify that double hashing maintains uniformity

test_double_hashing <- function(m = 1000, k = 7, n = 10000) {
  cat("\n", rep("=", 70), "\n")
  cat("TESTING DOUBLE HASHING TECHNIQUE\n")
  cat(rep("=", 70), "\n")
  cat(sprintf("Creating %d hash functions from 2 base hashes\n", k))
  cat(sprintf("Testing with m=%d buckets, n=%d items\n\n", m, n))
  
  items <- paste0("item_", 1:n)
  
  # For each item, generate k hash values using double hashing
  all_hashes <- matrix(0, nrow = n, ncol = k)
  
  for (i in 1:n) {
    item <- items[i]
    h1 <- xxhash_hash(item, m, 1)
    h2 <- xxhash_hash(item, m, 2)
    
    for (j in 1:k) {
      # gi(x) = [h1(x) + i*h2(x)] mod m
      all_hashes[i, j] <- (h1 + (j-1) * h2) %% m
    }
  }
  
  # Test uniformity for each hash function
  results <- list()
  
  for (j in 1:k) {
    bucket_counts <- table(factor(all_hashes[, j], levels = 0:(m-1)))
    bucket_counts <- as.vector(bucket_counts)
    
    expected <- n / m
    chi_sq <- sum((bucket_counts - expected)^2 / expected)
    df <- m - 1
    p_value <- pchisq(chi_sq, df, lower.tail = FALSE)
    
    cv <- sd(bucket_counts) / mean(bucket_counts)
    
    cat(sprintf("Hash function g%d:\n", j))
    cat(sprintf("  CV: %.4f, Chi-sq: %.2f, p-value: %.4f, Uniform: %s\n",
                cv, chi_sq, p_value, ifelse(p_value > 0.05, "YES", "NO")))
    
    results[[j]] <- list(
      cv = cv,
      chi_square = chi_sq,
      p_value = p_value,
      uniform = p_value > 0.05
    )
  }
  
  cat(sprintf("\nAll %d hash functions uniform: %s\n",
              k, ifelse(all(sapply(results, function(x) x$uniform)), "YES", "NO")))
  
  return(results)
}

# ============================================================================
# MAIN EXECUTION (for testing)
# ============================================================================

if (FALSE) {  # Set to TRUE to run tests
  # Compare all hash functions
  comparison <- compare_all_hash_functions(m = 1000, n = 10000)
  
  # Test avalanche effect for xxHash
  cat("\n\nAvalanche effect for xxHash:\n")
  avalanche <- test_avalanche_effect(xxhash_hash, m = 10000)
  
  # Test double hashing
  double_hash_test <- test_double_hashing(m = 1000, k = 7, n = 10000)
  
  # Benchmark
  cat("\n\n")
  bench_results <- benchmark_hash_functions(n = 1000)
}

cat("Hash function module loaded successfully!\n")
