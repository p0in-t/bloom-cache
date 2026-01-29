# Distributed Cache Simulator
# Multi-tier cache system with Bloom filter integration

source("2_bloom_filter.R")
library(ggplot2)

# ============================================================================
# CACHE NODE CLASS
# ============================================================================

CacheNode <- R6Class("CacheNode",
  public = list(
    name = NULL,
    capacity = NULL,
    access_time_ms = NULL,
    cache = NULL,          # Named list: key -> value
    bloom_filter = NULL,
    use_bloom = FALSE,
    access_order = NULL,   # For LRU eviction
    
    # Statistics
    hits = 0,
    misses = 0,
    false_positives = 0,
    
    # ========================================================================
    # CONSTRUCTOR
    # ========================================================================
    initialize = function(name, capacity, access_time_ms, use_bloom = FALSE, fpr = 0.01) {
      self$name <- name
      self$capacity <- capacity
      self$access_time_ms <- access_time_ms
      self$use_bloom <- use_bloom
      self$cache <- list()
      self$access_order <- character(0)
      
      if (use_bloom) {
        self$bloom_filter <- BloomFilter$new(
          expected_elements = capacity,
          false_positive_rate = fpr
        )
      }
      
      cat(sprintf("Cache node '%s' initialized:\n", name))
      cat(sprintf("  Capacity: %d items\n", capacity))
      cat(sprintf("  Access time: %.2f ms\n", access_time_ms))
      cat(sprintf("  Using Bloom filter: %s\n\n", use_bloom))
    },
    
    # ========================================================================
    # CHECK IF KEY EXISTS (with Bloom filter pre-check)
    # ========================================================================
    check = function(key) {
      # If using Bloom filter, check it first
      if (self$use_bloom) {
        if (!self$bloom_filter$contains(key)) {
          # Definitely not in cache
          return(FALSE)
        }
        # Might be in cache (could be false positive)
        # Fall through to actual check
      }
      
      # Actual cache check
      exists <- key %in% names(self$cache)
      
      # Track false positives
      if (self$use_bloom && !exists) {
        self$false_positives <- self$false_positives + 1
      }
      
      return(exists)
    },
    
    # ========================================================================
    # GET VALUE FROM CACHE
    # ========================================================================
    get = function(key) {
      if (self$check(key)) {
        self$hits <- self$hits + 1
        
        # Update LRU order
        self$access_order <- c(key, setdiff(self$access_order, key))
        
        return(list(found = TRUE, value = self$cache[[key]], time = self$access_time_ms))
      } else {
        self$misses <- self$misses + 1
        return(list(found = FALSE, value = NULL, time = self$access_time_ms))
      }
    },
    
    # ========================================================================
    # PUT VALUE INTO CACHE
    # ========================================================================
    put = function(key, value) {
      # Check if we need to evict (LRU)
      if (length(self$cache) >= self$capacity && !(key %in% names(self$cache))) {
        # Evict least recently used
        lru_key <- self$access_order[length(self$access_order)]
        self$cache[[lru_key]] <- NULL
        self$access_order <- self$access_order[-length(self$access_order)]
      }
      
      # Add to cache
      self$cache[[key]] <- value
      self$access_order <- c(key, setdiff(self$access_order, key))
      
      # Add to Bloom filter
      if (self$use_bloom) {
        self$bloom_filter$add(key)
      }
    },
    
    # ========================================================================
    # GET STATISTICS
    # ========================================================================
    stats = function() {
      total_requests <- self$hits + self$misses
      hit_rate <- ifelse(total_requests > 0, self$hits / total_requests, 0)
      
      list(
        name = self$name,
        capacity = self$capacity,
        size = length(self$cache),
        access_time_ms = self$access_time_ms,
        hits = self$hits,
        misses = self$misses,
        total_requests = total_requests,
        hit_rate = hit_rate,
        false_positives = self$false_positives,
        use_bloom = self$use_bloom
      )
    },
    
    # ========================================================================
    # RESET STATISTICS
    # ========================================================================
    reset_stats = function() {
      self$hits <- 0
      self$misses <- 0
      self$false_positives <- 0
    },
    
    # ========================================================================
    # CLEAR CACHE
    # ========================================================================
    clear = function() {
      self$cache <- list()
      self$access_order <- character(0)
      if (self$use_bloom) {
        self$bloom_filter$clear()
      }
      self$reset_stats()
    }
  )
)

# ============================================================================
# DISTRIBUTED CACHE SYSTEM
# ============================================================================

DistributedCacheSystem <- R6Class("DistributedCacheSystem",
  public = list(
    tiers = NULL,
    origin_time_ms = NULL,
    use_bloom = FALSE,
    
    # Statistics
    total_requests = 0,
    origin_fetches = 0,
    total_latency = 0,
    
    # ========================================================================
    # CONSTRUCTOR
    # ========================================================================
    initialize = function(tier_config, origin_time_ms = 200, use_bloom = FALSE) {
      self$origin_time_ms <- origin_time_ms
      self$use_bloom <- use_bloom
      self$tiers <- list()
      
      cat("\n", rep("=", 70), "\n")
      cat("DISTRIBUTED CACHE SYSTEM INITIALIZATION\n")
      cat(rep("=", 70), "\n\n")
      
      for (i in 1:length(tier_config)) {
        tier <- tier_config[[i]]
        cache_node <- CacheNode$new(
          name = tier$name,
          capacity = tier$capacity,
          access_time_ms = tier$access_time_ms,
          use_bloom = use_bloom,
          fpr = ifelse(is.null(tier$fpr), 0.01, tier$fpr)
        )
        self$tiers[[i]] <- cache_node
      }
      
      cat(sprintf("Origin server access time: %.2f ms\n", origin_time_ms))
      cat(sprintf("Bloom filters enabled: %s\n", use_bloom))
      cat(rep("=", 70), "\n\n")
    },
    
    # ========================================================================
    # GET RESOURCE (main query method)
    # ========================================================================
    get = function(key) {
      self$total_requests <- self$total_requests + 1
      cumulative_time <- 0
      
      # Try each tier in order
      for (i in 1:length(self$tiers)) {
        tier <- self$tiers[[i]]
        
        # Check if resource is in this tier
        if (tier$check(key)) {
          result <- tier$get(key)
          cumulative_time <- cumulative_time + result$time
          
          if (result$found) {
            # Cache hit! Populate higher tiers
            if (i > 1) {
              for (j in 1:(i-1)) {
                self$tiers[[j]]$put(key, result$value)
              }
            }
            
            self$total_latency <- self$total_latency + cumulative_time
            return(list(
              found = TRUE,
              value = result$value,
              tier = i,
              tier_name = tier$name,
              latency_ms = cumulative_time
            ))
          }
        } else {
          # Bloom filter says not here, skip this tier
          # (no access time incurred with Bloom filter!)
          if (!self$use_bloom) {
            # Without Bloom filter, we still had to check
            cumulative_time <- cumulative_time + tier$access_time_ms
          }
        }
      }
      
      # Not in any cache tier, fetch from origin
      self$origin_fetches <- self$origin_fetches + 1
      cumulative_time <- cumulative_time + self$origin_time_ms
      
      # Populate all cache tiers
      value <- paste0("data_", key)  # Simulated data fetch
      for (tier in self$tiers) {
        tier$put(key, value)
      }
      
      self$total_latency <- self$total_latency + cumulative_time
      
      return(list(
        found = TRUE,
        value = value,
        tier = 0,
        tier_name = "Origin",
        latency_ms = cumulative_time
      ))
    },
    
    # ========================================================================
    # SIMULATE WORKLOAD
    # ========================================================================
    simulate = function(requests) {
      cat(sprintf("\nSimulating %d requests...\n", length(requests)))
      
      latencies <- numeric(length(requests))
      tiers_hit <- character(length(requests))
      
      pb <- txtProgressBar(min = 0, max = length(requests), style = 3)
      
      for (i in 1:length(requests)) {
        result <- self$get(requests[i])
        latencies[i] <- result$latency_ms
        tiers_hit[i] <- result$tier_name
        
        if (i %% 100 == 0) {
          setTxtProgressBar(pb, i)
        }
      }
      
      close(pb)
      
      return(list(
        latencies = latencies,
        tiers_hit = tiers_hit
      ))
    },
    
    # ========================================================================
    # GET STATISTICS
    # ========================================================================
    stats = function() {
      tier_stats <- lapply(self$tiers, function(t) t$stats())
      
      avg_latency <- ifelse(self$total_requests > 0, 
                            self$total_latency / self$total_requests, 0)
      
      origin_rate <- ifelse(self$total_requests > 0,
                            self$origin_fetches / self$total_requests, 0)
      
      list(
        total_requests = self$total_requests,
        origin_fetches = self$origin_fetches,
        origin_rate = origin_rate,
        total_latency = self$total_latency,
        avg_latency_ms = avg_latency,
        tier_stats = tier_stats,
        use_bloom = self$use_bloom
      )
    },
    
    # ========================================================================
    # PRINT STATISTICS
    # ========================================================================
    print_stats = function() {
      stats <- self$stats()
      
      cat("\n", rep("=", 70), "\n")
      cat("CACHE SYSTEM STATISTICS\n")
      cat(rep("=", 70), "\n\n")
      
      cat(sprintf("Total requests: %d\n", stats$total_requests))
      cat(sprintf("Origin fetches: %d (%.2f%%)\n", 
                  stats$origin_fetches, stats$origin_rate * 100))
      cat(sprintf("Average latency: %.2f ms\n", stats$avg_latency_ms))
      cat(sprintf("Total latency: %.2f seconds\n\n", stats$total_latency / 1000))
      
      cat("Per-tier statistics:\n")
      cat(rep("-", 70), "\n")
      
      for (tier_stat in stats$tier_stats) {
        cat(sprintf("\n%s (Capacity: %d, Access time: %.2f ms):\n", 
                    tier_stat$name, tier_stat$capacity, tier_stat$access_time_ms))
        cat(sprintf("  Requests: %d (Hits: %d, Misses: %d)\n",
                    tier_stat$total_requests, tier_stat$hits, tier_stat$misses))
        cat(sprintf("  Hit rate: %.2f%%\n", tier_stat$hit_rate * 100))
        
        if (tier_stat$use_bloom) {
          cat(sprintf("  False positives: %d\n", tier_stat$false_positives))
        }
      }
      
      cat("\n", rep("=", 70), "\n\n")
    },
    
    # ========================================================================
    # RESET ALL
    # ========================================================================
    reset = function() {
      for (tier in self$tiers) {
        tier$clear()
      }
      self$total_requests <- 0
      self$origin_fetches <- 0
      self$total_latency <- 0
    }
  )
)

# ============================================================================
# WORKLOAD GENERATOR
# ============================================================================

# ----------------------------------------------------------------------------
# Generate Zipfian distributed requests (realistic web traffic)
# ----------------------------------------------------------------------------
generate_zipf_workload <- function(n_requests, n_unique_items, alpha = 1.0) {
  # Zipf distribution: P(k) ~ 1/k^alpha
  # Models that some items are much more popular than others
  
  # Generate probabilities
  ranks <- 1:n_unique_items
  probs <- 1 / (ranks^alpha)
  probs <- probs / sum(probs)
  
  # Sample requests according to Zipf distribution
  requests <- sample(1:n_unique_items, size = n_requests, replace = TRUE, prob = probs)
  requests <- paste0("item_", requests)
  
  return(requests)
}

# ----------------------------------------------------------------------------
# Generate uniform random requests (baseline)
# ----------------------------------------------------------------------------
generate_uniform_workload <- function(n_requests, n_unique_items) {
  requests <- sample(1:n_unique_items, size = n_requests, replace = TRUE)
  requests <- paste0("item_", requests)
  return(requests)
}

# ============================================================================
# COMPARISON EXPERIMENT
# ============================================================================

compare_with_and_without_bloom <- function(n_requests = 10000, 
                                          n_unique_items = 10000,
                                          alpha = 1.0) {
  
  cat("\n", rep("=", 70), "\n")
  cat("COMPARISON: WITH vs WITHOUT BLOOM FILTERS\n")
  cat(rep("=", 70), "\n\n")
  
  # Define cache tiers
  tier_config <- list(
    list(name = "L1_Local", capacity = 100, access_time_ms = 1, fpr = 0.01),
    list(name = "L2_Regional", capacity = 500, access_time_ms = 10, fpr = 0.01),
    list(name = "L3_Global", capacity = 2000, access_time_ms = 50, fpr = 0.01)
  )
  
  # Generate workload
  cat(sprintf("Generating Zipfian workload (alpha=%.2f)...\n", alpha))
  workload <- generate_zipf_workload(n_requests, n_unique_items, alpha)
  
  # Test WITHOUT Bloom filters
  cat("\n--- Testing WITHOUT Bloom filters ---\n")
  cache_no_bloom <- DistributedCacheSystem$new(tier_config, origin_time_ms = 200, use_bloom = FALSE)
  result_no_bloom <- cache_no_bloom$simulate(workload)
  stats_no_bloom <- cache_no_bloom$stats()
  cache_no_bloom$print_stats()
  
  # Test WITH Bloom filters
  cat("\n--- Testing WITH Bloom filters ---\n")
  cache_with_bloom <- DistributedCacheSystem$new(tier_config, origin_time_ms = 200, use_bloom = TRUE)
  result_with_bloom <- cache_with_bloom$simulate(workload)
  stats_with_bloom <- cache_with_bloom$stats()
  cache_with_bloom$print_stats()
  
  # Calculate improvements
  cat("\n", rep("=", 70), "\n")
  cat("PERFORMANCE IMPROVEMENT\n")
  cat(rep("=", 70), "\n\n")
  
  latency_reduction <- ((stats_no_bloom$avg_latency_ms - stats_with_bloom$avg_latency_ms) / 
                        stats_no_bloom$avg_latency_ms) * 100
  
  time_saved <- stats_no_bloom$total_latency - stats_with_bloom$total_latency
  
  cat(sprintf("Average latency reduction: %.2f%%\n", latency_reduction))
  cat(sprintf("  Without Bloom: %.2f ms\n", stats_no_bloom$avg_latency_ms))
  cat(sprintf("  With Bloom: %.2f ms\n", stats_with_bloom$avg_latency_ms))
  cat(sprintf("\nTotal time saved: %.2f seconds (%.2f%%)\n",
              time_saved / 1000,
              (time_saved / stats_no_bloom$total_latency) * 100))
  
  cat(rep("=", 70), "\n\n")
  
  return(list(
    no_bloom = stats_no_bloom,
    with_bloom = stats_with_bloom,
    improvement = latency_reduction,
    time_saved_ms = time_saved,
    latencies_no_bloom = result_no_bloom$latencies,
    latencies_with_bloom = result_with_bloom$latencies,
    tiers_no_bloom = result_no_bloom$tiers_hit,
    tiers_with_bloom = result_with_bloom$tiers_hit
  ))
}

# ============================================================================
# DEMONSTRATION
# ============================================================================

if (FALSE) {  # Set to TRUE to run demo
  # Run comparison
  results <- compare_with_and_without_bloom(
    n_requests = 10000,
    n_unique_items = 10000,
    alpha = 1.0
  )
}

cat("Cache simulator loaded successfully!\n")
