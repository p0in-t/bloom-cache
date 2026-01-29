# Bloom Filter Implementation
# R6 class for probabilistic membership testing

library(digest)      # For hash functions
library(R6)         # For object-oriented programming

BloomFilter <- R6Class("BloomFilter",
  public = list(
    # Attributes
    bit_array = NULL,           # The bit array (logical vector)
    size = NULL,                # m: number of bits
    num_hash = NULL,            # k: number of hash functions
    num_elements = 0,           # n: count of inserted elements
    expected_elements = NULL,   # Expected number of elements
    target_fpr = NULL,          # Target false positive rate
    hash_seeds = NULL,          # Seeds for hash functions
    
    # Constructor
    initialize = function(expected_elements = 1000, 
                         false_positive_rate = 0.01,
                         size = NULL,
                         num_hash = NULL) {
      
      self$expected_elements <- expected_elements
      self$target_fpr <- false_positive_rate
      
      # Calculate optimal parameters if not provided
      if (is.null(size)) {
        self$size <- self$optimal_size(expected_elements, false_positive_rate)
      } else {
        self$size <- size
      }
      
      if (is.null(num_hash)) {
        self$num_hash <- self$optimal_hash_count(self$size, expected_elements)
      } else {
        self$num_hash <- num_hash
      }
      
      # Initialize bit array (all FALSE)
      self$bit_array <- rep(FALSE, self$size)
      
      # Generate random seeds for hash functions
      set.seed(42)  # For reproducibility
      self$hash_seeds <- sample(1:100000, self$num_hash)
      
      cat(sprintf("Bloom Filter initialized:\n"))
      cat(sprintf("  Size (m): %d bits (%.2f KB)\n", self$size, self$size/8/1024))
      cat(sprintf("  Hash functions (k): %d\n", self$num_hash))
      cat(sprintf("  Expected elements (n): %d\n", expected_elements))
      cat(sprintf("  Target FPR: %.4f%%\n", false_positive_rate * 100))
      cat(sprintf("  Bits per element: %.2f\n\n", self$size / expected_elements))
    },
    
    # Calculate optimal size: m = -n * ln(p) / (ln(2)^2)
    optimal_size = function(n, p) {
      m <- ceiling(-n * log(p) / (log(2)^2))
      return(m)
    },
    
    # ========================================================================
    # OPTIMAL HASH COUNT CALCULATION
    # ========================================================================
    # Formula: k = (m/n) * ln(2)
    optimal_hash_count = function(m, n) {
      k <- round((m / n) * log(2))
      k <- max(1, k)  # At least 1 hash function
      return(k)
    },
    
    # ========================================================================
    # HASH FUNCTION (using double hashing technique)
    # ========================================================================
    # Kirsch-Mitzenmacher technique: g_i(x) = [h1(x) + i*h2(x)] mod m
    hash = function(item, seed) {
      # Convert item to string if necessary
      item_str <- as.character(item)
      
      tryCatch({
        # Use digest package for fast hashing
        hash_val <- digest(paste0(item_str, "_", seed), algo = "xxhash64", serialize = FALSE)
        
        # FIXED: Use smaller chunks to avoid R integer overflow
        # R's integer.max is 2^31-1, so use 7 hex chars (28 bits) max
        hash_int <- 0L
        chunk_size <- 7  # 7 hex chars = 28 bits, safely under 2^31
        
        for (i in seq(1, nchar(hash_val), by = chunk_size)) {
          chunk <- substr(hash_val, i, min(i + chunk_size - 1, nchar(hash_val)))
          chunk_int <- strtoi(chunk, base = 16)
          
          if (!is.na(chunk_int) && !is.infinite(chunk_int)) {
            # Use XOR for avalanche effect
            hash_int <- bitwXor(as.integer(hash_int), as.integer(abs(chunk_int)))
          }
        }
        
        # Additional mixing to improve distribution
        hash_int <- abs(hash_int)
        hash_int <- bitwXor(hash_int, as.integer(hash_int %/% 65536))
        
        # Ensure non-zero
        if (hash_int == 0 || is.na(hash_int)) {
          hash_int <- as.integer(seed %% 2147483647)
        }
        
        return(as.integer(abs(hash_int) %% self$size))
        
      }, error = function(e) {
        # Fallback: simple character-based hash with better mixing
        char_sum <- sum(utf8ToInt(item_str))
        return(as.integer(abs((char_sum * 2654435761 + seed * 2246822519)) %% self$size))
      })
    },
    
    # ========================================================================
    # DOUBLE HASH (more efficient for multiple hash functions)
    # ========================================================================
    # FIXED: Ensure h1 and h2 are independent and h2 has good properties
    double_hash = function(item, i) {
      # Calculate two independent hash values
      h1 <- self$hash(item, self$hash_seeds[1])
      h2 <- self$hash(item, self$hash_seeds[2])
      
      # Ensure h2 is odd for better distribution (coprime with power-of-2 sizes)
      # Also ensure h2 is not too small
      h2 <- abs(h2)
      if (h2 < self$size / 100) {
        h2 <- h2 + as.integer(self$size / 10)
      }
      if (h2 %% 2 == 0) {
        h2 <- h2 + 1
      }
      
      # Apply double hashing formula: g_i(x) = (h1 + i * h2) mod m
      h <- as.integer((h1 + i * h2) %% self$size)
      
      # Final safety check
      if (is.na(h) || is.nan(h) || is.infinite(h) || h < 0 || h >= self$size) {
        # Use a deterministic fallback
        fallback <- (abs(sum(utf8ToInt(as.character(item)))) * (i + 1) * 2654435761)
        h <- as.integer(abs(fallback) %% self$size)
      }
      
      return(h)
    },
    
    # ========================================================================
    # ADD ELEMENT
    # ========================================================================
    add = function(item) {
      for (i in 1:self$num_hash) {
        # pos <- self$hash(item, self$hash_seeds[i])
        pos <- self$double_hash(item, i-1)  # Using double hashing
        
        # Ensure pos is valid
        if (is.na(pos) || pos < 0 || pos >= self$size) {
          pos <- 0  # Fallback to first position
        }
        
        self$bit_array[pos + 1] <- TRUE  # R uses 1-indexing
      }
      self$num_elements <- self$num_elements + 1
    },
    
    # ========================================================================
    # ADD MULTIPLE ELEMENTS (vectorized)
    # ========================================================================
    add_all = function(items) {
      for (item in items) {
        self$add(item)
      }
    },
    
    # ========================================================================
    # CHECK MEMBERSHIP
    # ========================================================================
    contains = function(item) {
      for (i in 1:self$num_hash) {
        # pos <- self$hash(item, self$hash_seeds[i])
        pos <- self$double_hash(item, i-1)
        
        # Ensure pos is valid
        if (is.na(pos) || pos < 0 || pos >= self$size) {
          pos <- 0  # Fallback to first position
        }
        
        if (!self$bit_array[pos + 1]) {
          return(FALSE)  # Definitely not in set
        }
      }
      return(TRUE)  # Probably in set
    },
    
    # ========================================================================
    # CHECK MULTIPLE ELEMENTS (vectorized)
    # ========================================================================
    contains_all = function(items) {
      sapply(items, function(item) self$contains(item))
    },
    
    # ========================================================================
    # CURRENT FALSE POSITIVE RATE (theoretical)
    # ========================================================================
    current_fpr = function() {
      if (self$num_elements == 0) return(0)
      
      # Formula: (1 - e^(-kn/m))^k
      prob_bit_zero <- exp(-self$num_hash * self$num_elements / self$size)
      fpr <- (1 - prob_bit_zero)^self$num_hash
      return(fpr)
    },
    
    # ========================================================================
    # FILL RATIO (proportion of bits set to 1)
    # ========================================================================
    fill_ratio = function() {
      return(sum(self$bit_array) / self$size)
    },
    
    # ========================================================================
    # STATISTICS
    # ========================================================================
    stats = function() {
      list(
        size_bits = self$size,
        size_kb = self$size / 8 / 1024,
        num_hash = self$num_hash,
        num_elements = self$num_elements,
        expected_elements = self$expected_elements,
        target_fpr = self$target_fpr,
        current_fpr = self$current_fpr(),
        fill_ratio = self$fill_ratio(),
        bits_per_element = self$size / max(1, self$num_elements)
      )
    },
    
    # ========================================================================
    # PRINT STATISTICS
    # ========================================================================
    print_stats = function() {
      s <- self$stats()
      cat("\n=== Bloom Filter Statistics ===\n")
      cat(sprintf("Size: %d bits (%.2f KB)\n", s$size_bits, s$size_kb))
      cat(sprintf("Hash functions: %d\n", s$num_hash))
      cat(sprintf("Elements inserted: %d / %d expected\n", 
                  s$num_elements, s$expected_elements))
      cat(sprintf("Fill ratio: %.4f (%.2f%% bits set)\n", 
                  s$fill_ratio, s$fill_ratio * 100))
      cat(sprintf("Target FPR: %.4f%%\n", s$target_fpr * 100))
      cat(sprintf("Current theoretical FPR: %.4f%%\n", s$current_fpr * 100))
      cat(sprintf("Bits per element: %.2f\n", s$bits_per_element))
      cat("================================\n\n")
    },
    
    # ========================================================================
    # CLEAR FILTER
    # ========================================================================
    clear = function() {
      self$bit_array <- rep(FALSE, self$size)
      self$num_elements <- 0
    }
  )
)

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

# ----------------------------------------------------------------------------
# Test false positive rate empirically
# ----------------------------------------------------------------------------
test_false_positive_rate <- function(bf, num_tests = 10000) {
  cat("Testing false positive rate...\n")
  
  # Generate test elements that are NOT in the filter
  test_elements <- paste0("test_", 1:num_tests)
  
  # Check how many are false positives
  results <- bf$contains_all(test_elements)
  false_positives <- sum(results)
  
  empirical_fpr <- false_positives / num_tests
  theoretical_fpr <- bf$current_fpr()
  
  cat(sprintf("False positives: %d / %d\n", false_positives, num_tests))
  cat(sprintf("Empirical FPR: %.4f%% (%.6f)\n", 
              empirical_fpr * 100, empirical_fpr))
  cat(sprintf("Theoretical FPR: %.4f%% (%.6f)\n", 
              theoretical_fpr * 100, theoretical_fpr))
  cat(sprintf("Difference: %.4f%%\n\n", 
              abs(empirical_fpr - theoretical_fpr) * 100))
  
  return(list(
    empirical = empirical_fpr,
    theoretical = theoretical_fpr,
    false_positives = false_positives,
    num_tests = num_tests
  ))
}

# ----------------------------------------------------------------------------
# Memory comparison with hash table
# ----------------------------------------------------------------------------
compare_memory <- function(num_elements, element_size_bytes = 64, fpr = 0.01) {
  # Bloom filter size
  bf_size <- BloomFilter$new(num_elements, fpr)$size
  bf_kb <- bf_size / 8 / 1024
  bf_mb <- bf_kb / 1024
  
  # Hash table size (pointer + data)
  ht_bytes <- num_elements * (8 + element_size_bytes)  # 8 bytes pointer
  ht_kb <- ht_bytes / 1024
  ht_mb <- ht_kb / 1024
  
  savings <- (1 - bf_mb / ht_mb) * 100
  
  cat("\n=== Memory Comparison ===\n")
  cat(sprintf("Number of elements: %d\n", num_elements))
  cat(sprintf("Element size: %d bytes\n", element_size_bytes))
  cat(sprintf("Target FPR: %.2f%%\n\n", fpr * 100))
  
  cat(sprintf("Bloom Filter: %.2f MB\n", bf_mb))
  cat(sprintf("Hash Table: %.2f MB\n", ht_mb))
  cat(sprintf("Space savings: %.2f%%\n", savings))
  cat("=========================\n\n")
  
  return(list(
    bloom_filter_mb = bf_mb,
    hash_table_mb = ht_mb,
    savings_percent = savings
  ))
}

# ============================================================================
# DEMONSTRATION
# ============================================================================

if (FALSE) {  # Set to TRUE to run demo
  cat("\n" , rep("=", 70), "\n")
  cat("BLOOM FILTER DEMONSTRATION\n")
  cat(rep("=", 70), "\n\n")
  
  # Create a Bloom filter
  bf <- BloomFilter$new(expected_elements = 1000, false_positive_rate = 0.01)
  
  # Add elements
  cat("Adding 1000 elements...\n")
  elements <- paste0("element_", 1:1000)
  bf$add_all(elements)
  
  # Print statistics
  bf$print_stats()
  
  # Test true positives (should all be TRUE)
  cat("Testing true positives (elements that were added)...\n")
  sample_elements <- sample(elements, 100)
  tp_results <- bf$contains_all(sample_elements)
  cat(sprintf("True positives: %d / %d (%.1f%%)\n\n", 
              sum(tp_results), length(sample_elements), 
              mean(tp_results) * 100))
  
  # Test false positive rate
  fp_test <- test_false_positive_rate(bf, 10000)
  
  # Memory comparison
  mem_comp <- compare_memory(1000, 64, 0.01)
}

cat("Bloom filter implementation loaded successfully!\n")
