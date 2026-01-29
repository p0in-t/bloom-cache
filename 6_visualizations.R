# Visualization Suite
# Figure generation for Bloom filter analysis

library(ggplot2)
library(gridExtra)
library(reshape2)
library(scales)

# Set theme for all plots
theme_set(theme_minimal(base_size = 12) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5)))

# ============================================================================
# FIGURE 1: FALSE POSITIVE RATE VALIDATION
# ============================================================================

plot_fpr_validation <- function(exp1_results) {
  # Extract data
  data <- data.frame(
    Configuration = names(exp1_results),
    Theoretical = sapply(exp1_results, function(x) x$theoretical_fpr),
    Empirical = sapply(exp1_results, function(x) x$empirical_fpr),
    CI_Lower = sapply(exp1_results, function(x) x$ci_lower),
    CI_Upper = sapply(exp1_results, function(x) x$ci_upper)
  )
  
  # Reshape for plotting
  data_long <- melt(data[, 1:3], id.vars = "Configuration")
  names(data_long) <- c("Configuration", "Type", "FPR")
  
  # Plot
  p <- ggplot(data, aes(x = Configuration)) +
    geom_col(data = data_long, aes(x = Configuration, y = FPR, fill = Type),
             position = "dodge", alpha = 0.8) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper),
                  width = 0.2, position = position_dodge(0.9)) +
    scale_fill_manual(values = c("Theoretical" = "#2E86AB", "Empirical" = "#A23B72")) +
    labs(
      title = "False Positive Rate: Theoretical vs Empirical",
      subtitle = "Error bars show 95% confidence intervals",
      x = "Configuration",
      y = "False Positive Rate",
      fill = "Type"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("figures/fig1_fpr_validation.png", p, width = 8, height = 6, dpi = 300)
  return(p)
}

# ============================================================================
# FIGURE 2: PARAMETER OPTIMIZATION (FPR vs m/n)
# ============================================================================

plot_parameter_optimization <- function() {
  # Generate theoretical curves
  fpr_values <- c(0.001, 0.01, 0.05, 0.1)
  n <- 1000
  
  data <- expand.grid(
    m_per_n = seq(5, 20, by = 0.1),
    fpr_target = fpr_values
  )
  
  data$k_optimal <- round(data$m_per_n * log(2))
  data$fpr_actual <- (1 - exp(-data$k_optimal / data$m_per_n))^data$k_optimal
  data$fpr_label <- factor(data$fpr_target, 
                           labels = c("0.1%", "1%", "5%", "10%"))
  
  # Calculate optimal points
  optimal_points <- data.frame(
    fpr_target = fpr_values,
    m_per_n = -log(fpr_values) / (log(2)^2)
  )
  optimal_points$k <- round(optimal_points$m_per_n * log(2))
  optimal_points$fpr_actual <- (1 - exp(-optimal_points$k / optimal_points$m_per_n))^optimal_points$k
  optimal_points$fpr_label <- factor(optimal_points$fpr_target,
                                     labels = c("0.1%", "1%", "5%", "10%"))
  
  # Plot
  p <- ggplot(data, aes(x = m_per_n, y = fpr_actual, color = fpr_label)) +
    geom_line(size = 1) +
    geom_point(data = optimal_points, size = 4, shape = 21, fill = "white", stroke = 2) +
    geom_hline(yintercept = fpr_values, linetype = "dashed", alpha = 0.3) +
    scale_y_log10(labels = percent_format()) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "False Positive Rate vs Bits per Element",
      subtitle = "Circles indicate optimal k = (m/n)ln(2)",
      x = "Bits per Element (m/n)",
      y = "False Positive Rate (log scale)",
      color = "Target FPR"
    )
  
  ggsave("figures/fig2_parameter_optimization.png", p, width = 10, height = 6, dpi = 300)
  return(p)
}

# ============================================================================
# FIGURE 3: CACHE PERFORMANCE COMPARISON
# ============================================================================

plot_cache_performance <- function(exp3_results) {
  # Extract latency improvements
  data <- data.frame(
    Scenario = names(exp3_results),
    Alpha = sapply(exp3_results, function(x) x$scenario$alpha),
    Without_BF = sapply(exp3_results, function(x) x$no_bloom$avg_latency_ms),
    With_BF = sapply(exp3_results, function(x) x$with_bloom$avg_latency_ms),
    Improvement = sapply(exp3_results, function(x) x$latency_improvement_percent)
  )
  
  data$Scenario <- factor(data$Scenario, levels = data$Scenario)
  
  # Reshape for grouped bar chart
  data_long <- melt(data[, c("Scenario", "Without_BF", "With_BF")], 
                    id.vars = "Scenario")
  names(data_long) <- c("Scenario", "Filter", "Latency")
  data_long$Filter <- factor(data_long$Filter, 
                              labels = c("Without Bloom Filter", "With Bloom Filter"))
  
  # Plot 1: Bar chart
  p1 <- ggplot(data_long, aes(x = Scenario, y = Latency, fill = Filter)) +
    geom_col(position = "dodge", alpha = 0.8) +
    geom_text(aes(label = sprintf("%.2f", Latency)), 
              position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
    scale_fill_manual(values = c("Without Bloom Filter" = "#E63946", 
                                 "With Bloom Filter" = "#06A77D")) +
    labs(
      title = "Average Latency: With vs Without Bloom Filters",
      subtitle = "Different access pattern skewness (Zipf parameter α)",
      x = "Workload Scenario",
      y = "Average Latency (ms)",
      fill = ""
    ) +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot 2: Improvement percentage
  p2 <- ggplot(data, aes(x = Scenario, y = Improvement)) +
    geom_col(fill = "#457B9D", alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", Improvement)), vjust = -0.5) +
    labs(
      title = "Latency Improvement with Bloom Filters",
      x = "Workload Scenario",
      y = "Improvement (%)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  combined <- grid.arrange(p1, p2, ncol = 1)
  ggsave("figures/fig3_cache_performance.png", combined, width = 10, height = 10, dpi = 300)
  
  return(list(p1 = p1, p2 = p2))
}

# ============================================================================
# FIGURE 4: LATENCY DISTRIBUTIONS
# ============================================================================

plot_latency_distributions <- function(exp3_results) {
  # Use one scenario (e.g., Medium Skew)
  scenario_name <- "Medium Skew"
  result <- exp3_results[[scenario_name]]
  
  data <- data.frame(
    Latency = c(result$latencies_no_bloom, result$latencies_with_bloom),
    Filter = rep(c("Without Bloom Filter", "With Bloom Filter"),
                 c(length(result$latencies_no_bloom), length(result$latencies_with_bloom)))
  )
  
  # Histogram + density
  p1 <- ggplot(data, aes(x = Latency, fill = Filter)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 50) +
    scale_fill_manual(values = c("Without Bloom Filter" = "#E63946", 
                                  "With Bloom Filter" = "#06A77D")) +
    labs(
      title = "Latency Distribution Comparison",
      subtitle = paste("Scenario:", scenario_name),
      x = "Latency (ms)",
      y = "Frequency",
      fill = ""
    ) +
    theme(legend.position = "top")
  
  # Box plot
  p2 <- ggplot(data, aes(x = Filter, y = Latency, fill = Filter)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = c("Without Bloom Filter" = "#E63946",
                                  "With Bloom Filter" = "#06A77D")) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      title = "Latency Distribution (Box Plot)",
      subtitle = "Diamond indicates mean",
      x = "",
      y = "Latency (ms)"
    ) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  combined <- grid.arrange(p1, p2, ncol = 2, widths = c(2, 1))
  ggsave("figures/fig4_latency_distributions.png", combined, width = 12, height = 5, dpi = 300)
  
  return(list(p1 = p1, p2 = p2))
}

# ============================================================================
# FIGURE 5: SCALABILITY ANALYSIS
# ============================================================================

plot_scalability <- function(exp4_results) {
  data <- data.frame(
    N = sapply(exp4_results, function(x) x$n),
    Capacity_Percent = sapply(exp4_results, function(x) x$capacity_percent),
    Fill_Ratio = sapply(exp4_results, function(x) x$fill_ratio),
    Empirical_FPR = sapply(exp4_results, function(x) x$empirical_fpr),
    Theoretical_FPR = sapply(exp4_results, function(x) x$theoretical_fpr),
    FPR_Multiplier = sapply(exp4_results, function(x) x$fpr_multiplier)
  )
  
  # Plot 1: Fill ratio vs capacity
  p1 <- ggplot(data, aes(x = Capacity_Percent, y = Fill_Ratio)) +
    geom_line(color = "#2E86AB", size = 1.5) +
    geom_point(color = "#2E86AB", size = 3) +
    geom_vline(xintercept = 100, linetype = "dashed", color = "red", alpha = 0.5) +
    annotate("text", x = 100, y = max(data$Fill_Ratio) * 0.9, 
             label = "Design Capacity", angle = 90, vjust = -0.5, color = "red") +
    scale_x_continuous(labels = percent_format(scale = 1)) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Fill Ratio vs Capacity Utilization",
      x = "Capacity Utilization (%)",
      y = "Fill Ratio (% of bits set)"
    )
  
  # Plot 2: FPR degradation
  data_long <- melt(data[, c("Capacity_Percent", "Empirical_FPR", "Theoretical_FPR")],
                    id.vars = "Capacity_Percent")
  names(data_long) <- c("Capacity_Percent", "Type", "FPR")
  data_long$Type <- factor(data_long$Type, labels = c("Empirical", "Theoretical"))
  
  p2 <- ggplot(data_long, aes(x = Capacity_Percent, y = FPR, color = Type)) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    geom_vline(xintercept = 100, linetype = "dashed", color = "red", alpha = 0.5) +
    scale_x_continuous(labels = percent_format(scale = 1)) +
    scale_y_log10(labels = percent_format()) +
    scale_color_manual(values = c("Empirical" = "#A23B72", "Theoretical" = "#2E86AB")) +
    labs(
      title = "False Positive Rate Degradation",
      subtitle = "FPR increases when exceeding design capacity",
      x = "Capacity Utilization (%)",
      y = "False Positive Rate (log scale)",
      color = ""
    ) +
    theme(legend.position = "top")
  
  combined <- grid.arrange(p1, p2, ncol = 1)
  ggsave("figures/fig5_scalability.png", combined, width = 10, height = 10, dpi = 300)
  
  return(list(p1 = p1, p2 = p2))
}

# ============================================================================
# FIGURE 6: HASH FUNCTION COMPARISON
# ============================================================================

plot_hash_comparison <- function(exp5_results) {
  hash_comp <- exp5_results$hash_comparison
  
  data <- data.frame(
    Hash_Function = names(hash_comp),
    CV = sapply(hash_comp, function(x) x$cv),
    Chi_Square = sapply(hash_comp, function(x) x$chi_square),
    P_Value = sapply(hash_comp, function(x) x$p_value),
    Collision_Rate = sapply(hash_comp, function(x) x$collision_rate * 100),
    Uniform = sapply(hash_comp, function(x) x$uniform)
  )
  
  # Plot 1: Coefficient of Variation
  p1 <- ggplot(data, aes(x = reorder(Hash_Function, -CV), y = CV, fill = Uniform)) +
    geom_col(alpha = 0.8) +
    scale_fill_manual(values = c("TRUE" = "#06A77D", "FALSE" = "#E63946"),
                      labels = c("TRUE" = "Uniform", "FALSE" = "Not Uniform")) +
    labs(
      title = "Hash Function Uniformity (Coefficient of Variation)",
      subtitle = "Lower is better (more uniform distribution)",
      x = "Hash Function",
      y = "Coefficient of Variation",
      fill = "Uniformity Test"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
  
  # Plot 2: Collision rate
  p2 <- ggplot(data, aes(x = reorder(Hash_Function, -Collision_Rate), y = Collision_Rate)) +
    geom_col(fill = "#457B9D", alpha = 0.8) +
    geom_text(aes(label = sprintf("%.2f%%", Collision_Rate)), vjust = -0.5) +
    labs(
      title = "Hash Collision Rate",
      x = "Hash Function",
      y = "Collision Rate (%)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  combined <- grid.arrange(p1, p2, ncol = 1)
  ggsave("figures/fig6_hash_comparison.png", combined, width = 10, height = 10, dpi = 300)
  
  return(list(p1 = p1, p2 = p2))
}

# ============================================================================
# FIGURE 7: SPACE SAVINGS COMPARISON
# ============================================================================

plot_space_savings <- function() {
  n_values <- c(100, 1000, 10000, 100000, 1000000)
  element_size <- 64  # bytes
  fpr <- 0.01
  
  data <- data.frame(
    N = n_values,
    Bloom_MB = sapply(n_values, function(n) {
      m <- ceiling(-n * log(fpr) / (log(2)^2))
      m / 8 / 1024 / 1024
    }),
    HashTable_MB = n_values * (8 + element_size) / 1024 / 1024
  )
  
  data$Savings_Percent <- (1 - data$Bloom_MB / data$HashTable_MB) * 100
  
  # Reshape
  data_long <- melt(data[, c("N", "Bloom_MB", "HashTable_MB")], id.vars = "N")
  names(data_long) <- c("N", "Structure", "Memory_MB")
  data_long$Structure <- factor(data_long$Structure, 
                                 labels = c("Bloom Filter", "Hash Table"))
  
  # Plot
  p <- ggplot(data_long, aes(x = N, y = Memory_MB, color = Structure)) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    scale_x_log10(labels = comma_format()) +
    scale_y_log10(labels = comma_format()) +
    scale_color_manual(values = c("Bloom Filter" = "#06A77D", "Hash Table" = "#E63946")) +
    labs(
      title = "Memory Usage: Bloom Filter vs Hash Table",
      subtitle = "Both axes on log scale",
      x = "Number of Elements",
      y = "Memory (MB)",
      color = ""
    ) +
    theme(legend.position = "top")
  
  ggsave("figures/fig7_space_savings.png", p, width = 10, height = 6, dpi = 300)
  return(p)
}

# ============================================================================
# FIGURE 8: THEORETICAL CURVES
# ============================================================================

plot_theoretical_curves <- function() {
  # Generate data for different k values
  k_values <- c(3, 5, 7, 10, 15)
  m_over_n <- seq(2, 20, by = 0.1)
  
  data <- expand.grid(
    m_over_n = m_over_n,
    k = k_values
  )
  
  data$fpr <- (1 - exp(-data$k / data$m_over_n))^data$k
  data$k_label <- factor(data$k, labels = paste("k =", k_values))
  
  # Calculate optimal curve
  optimal_data <- data.frame(
    m_over_n = m_over_n,
    k_optimal = round(m_over_n * log(2)),
    fpr = NA
  )
  optimal_data$fpr <- (1 - exp(-optimal_data$k_optimal / optimal_data$m_over_n))^optimal_data$k_optimal
  
  # Plot
  p <- ggplot(data, aes(x = m_over_n, y = fpr, color = k_label)) +
    geom_line(size = 1, alpha = 0.6) +
    geom_line(data = optimal_data, aes(x = m_over_n, y = fpr), 
              color = "black", size = 1.5, linetype = "dashed",
              inherit.aes = FALSE) +
    annotate("text", x = 15, y = 0.001, label = "Optimal k", size = 4, fontface = "bold") +
    scale_y_log10(labels = percent_format()) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "False Positive Rate vs m/n for Different k",
      subtitle = "Dashed line shows optimal k = (m/n)ln(2)",
      x = "Bits per Element (m/n)",
      y = "False Positive Rate (log scale)",
      color = "Hash Functions"
    ) +
    theme(legend.position = "right")
  
  ggsave("figures/fig8_theoretical_curves.png", p, width = 10, height = 6, dpi = 300)
  return(p)
}

# ============================================================================
# GENERATE ALL FIGURES
# ============================================================================

generate_all_figures <- function(experimental_results) {
  # Create figures directory
  if (!dir.exists("figures")) {
    dir.create("figures")
  }
  
  cat("Generating all figures...\n\n")
  
  cat("Figure 1: FPR Validation...\n")
  fig1 <- plot_fpr_validation(experimental_results$exp1_fpr)
  
  cat("Figure 2: Parameter Optimization...\n")
  fig2 <- plot_parameter_optimization()
  
  cat("Figure 3: Cache Performance...\n")
  fig3 <- plot_cache_performance(experimental_results$exp3_cache)
  
  cat("Figure 4: Latency Distributions...\n")
  fig4 <- plot_latency_distributions(experimental_results$exp3_cache)
  
  cat("Figure 5: Scalability...\n")
  fig5 <- plot_scalability(experimental_results$exp4_scale)
  
  cat("Figure 6: Hash Comparison...\n")
  fig6 <- plot_hash_comparison(experimental_results$exp5_hash)
  
  cat("Figure 7: Space Savings...\n")
  fig7 <- plot_space_savings()
  
  cat("Figure 8: Theoretical Curves...\n")
  fig8 <- plot_theoretical_curves()
  
  cat("\nAll figures generated and saved to 'figures/' directory!\n")
  
  return(list(
    fig1 = fig1, fig2 = fig2, fig3 = fig3, fig4 = fig4,
    fig5 = fig5, fig6 = fig6, fig7 = fig7, fig8 = fig8
  ))
}

cat("Visualization suite loaded successfully!\n")
cat("Run: figures <- generate_all_figures(experimental_results)\n")
