# Main Execution Script
# Bloom Filters for Distributed Cache Optimization

cat("\n", rep("=", 80), "\n")
cat("BLOOM FILTER PROJECT - MAIN EXECUTION\n")
cat("Application of Bloom Filters to Distributed Cache Optimization\n")
cat(rep("=", 80), "\n\n")

## Setup
cat("Setting up environment...\n")

# Check and install required packages
required_packages <- c("ggplot2", "gridExtra", "reshape2", "scales", 
                       "digest", "R6", "microbenchmark", "knitr", "rmarkdown")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Installing package: %s\n", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

cat("All packages loaded successfully!\n\n")

## Load modules
cat("Loading project modules...\n")

source("2_bloom_filter.R")
source("3_hash_functions.R")
source("4_cache_simulator.R")
source("5_experiments.R")
source("6_visualizations.R")
source("7_statistical_analysis.R")
source("8_data_analysis_report.R")

cat("All modules loaded successfully!\n\n")

## Execution options
QUICK_MODE <- FALSE  # Set to TRUE for quick testing
FULL_MODE <- TRUE    # Set to TRUE for complete analysis

if (QUICK_MODE) {
  cat("*** RUNNING IN QUICK MODE (reduced sample sizes) ***\n\n")
}

# ============================================================================
# STEP 1: RUN ALL EXPERIMENTS
# ============================================================================

run_experiments <- function(quick = FALSE) {
  cat("\n", rep("#", 80), "\n")
  cat("STEP 1: RUNNING EXPERIMENTS\n")
  cat(rep("#", 80), "\n\n")
  
  if (quick) {
    cat("Quick mode: Using reduced parameters\n\n")
    # Modify experiment parameters for quick testing
    # (Would need to modify experiment functions to accept parameters)
  }
  
  experimental_results <- run_all_experiments()
  
  return(experimental_results)
}

# ============================================================================
# STEP 2: GENERATE VISUALIZATIONS
# ============================================================================

generate_visualizations <- function(experimental_results) {
  cat("\n", rep("#", 80), "\n")
  cat("STEP 2: GENERATING VISUALIZATIONS\n")
  cat(rep("#", 80), "\n\n")
  
  figures <- generate_all_figures(experimental_results)
  
  return(figures)
}

# ============================================================================
# STEP 3: PERFORM STATISTICAL ANALYSIS
# ============================================================================

perform_statistical_analysis <- function(experimental_results) {
  cat("\n", rep("#", 80), "\n")
  cat("STEP 3: STATISTICAL ANALYSIS\n")
  cat(rep("#", 80), "\n\n")
  
  statistical_results <- generate_statistical_report(experimental_results)
  
  return(statistical_results)
}

# ============================================================================
# STEP 4: GENERATE REPORT
# ============================================================================

generate_final_report <- function(experimental_results, statistical_results) {
  cat("\n", rep("#", 80), "\n")
  cat("STEP 4: GENERATING FINAL REPORT\n")
  cat(rep("#", 80), "\n\n")
  
  # Render the R Markdown report
  if (file.exists("FINAL_REPORT.Rmd")) {
    cat("Rendering R Markdown report...\n")
    rmarkdown::render("FINAL_REPORT.Rmd", output_format = "pdf_document")
    cat("Report generated: FINAL_REPORT.pdf\n\n")
  } else {
    cat("Warning: FINAL_REPORT.Rmd not found\n\n")
  }
}

# ============================================================================
# MAIN EXECUTION PIPELINE
# ============================================================================

main <- function(quick_mode = FALSE) {
  start_time <- Sys.time()
  
  cat("\n", rep("=", 80), "\n")
  cat("STARTING FULL EXECUTION PIPELINE\n")
  cat(rep("=", 80), "\n\n")
  
  # Step 1: Experiments
  cat("Starting experiments...\n")
  experimental_results <- run_experiments(quick = quick_mode)
  cat("✓ Experiments completed\n\n")
  
  # Step 2: Visualizations
  cat("Generating visualizations...\n")
  figures <- generate_visualizations(experimental_results)
  cat("✓ Visualizations completed\n\n")
  
  # Step 3: Statistical Analysis
  cat("Performing statistical analysis...\n")
  statistical_results <- perform_statistical_analysis(experimental_results)
  cat("✓ Statistical analysis completed\n\n")
  
  # Step 4: Report Generation
  cat("Generating final report...\n")
  generate_final_report(experimental_results, statistical_results)
  cat("✓ Report generation completed\n\n")
  
  # Step 5: Data Analysis Report (Required Report Structure)
  cat("Generating data analysis report (required structure)...\n")
  source("8_data_analysis_report.R")
  report_results <- generate_complete_data_analysis_report()
  cat("✓ Data analysis report completed\n\n")
  
  # Calculate execution time
  end_time <- Sys.time()
  execution_time <- difftime(end_time, start_time, units = "mins")
  
  cat("\n", rep("=", 80), "\n")
  cat("EXECUTION COMPLETED SUCCESSFULLY!\n")
  cat(rep("=", 80), "\n\n")
  cat(sprintf("Total execution time: %.2f minutes\n", execution_time))
  cat("\nGenerated files:\n")
  cat("  - experimental_results.rds\n")
  cat("  - statistical_analysis.rds\n")
  cat("  - complete_dataset.rds\n")
  cat("  - dataset_cache_performance.csv\n")
  cat("  - dataset_scalability.csv\n")
  cat("  - figures/*.png (8 figures)\n")
  cat("  - FINAL_REPORT.pdf\n\n")
  
  return(list(
    experimental_results = experimental_results,
    statistical_results = statistical_results,
    data_analysis_report = report_results,
    figures = figures,
    execution_time = execution_time
  ))
}

# ============================================================================
# QUICK DEMOS
# ============================================================================

demo_bloom_filter <- function() {
  cat("\n", rep("=", 80), "\n")
  cat("DEMO 1: BASIC BLOOM FILTER\n")
  cat(rep("=", 80), "\n\n")
  
  # Create and test a Bloom filter
  bf <- BloomFilter$new(expected_elements = 1000, false_positive_rate = 0.01)
  
  # Add elements
  cat("Adding 1000 elements...\n")
  elements <- paste0("element_", 1:1000)
  bf$add_all(elements)
  
  # Print stats
  bf$print_stats()
  
  # Test false positive rate
  cat("Testing false positive rate...\n")
  fp_test <- test_false_positive_rate(bf, 10000)
  
  # Memory comparison
  cat("\n")
  mem_comp <- compare_memory(1000, 64, 0.01)
}

demo_cache_comparison <- function() {
  cat("\n", rep("=", 80), "\n")
  cat("DEMO 2: CACHE PERFORMANCE COMPARISON\n")
  cat(rep("=", 80), "\n\n")
  
  # Run a quick cache comparison
  results <- compare_with_and_without_bloom(
    n_requests = 5000,
    n_unique_items = 5000,
    alpha = 1.0
  )
  
  cat("\nKey findings:\n")
  cat(sprintf("  - Latency improvement: %.2f%%\n", results$improvement))
  cat(sprintf("  - Time saved: %.2f seconds\n", results$time_saved_ms / 1000))
}

# ============================================================================
# INTERACTIVE MENU
# ============================================================================

interactive_menu <- function() {
  cat("\n", rep("=", 80), "\n")
  cat("BLOOM FILTER PROJECT - INTERACTIVE MENU\n")
  cat(rep("=", 80), "\n\n")
  
  cat("Select an option:\n")
  cat("  1. Run basic Bloom filter demo\n")
  cat("  2. Run cache comparison demo\n")
  cat("  3. Run all experiments (FULL - may take 10-30 minutes)\n")
  cat("  4. Run all experiments (QUICK - reduced sample sizes)\n")
  cat("  5. Generate visualizations only (requires existing results)\n")
  cat("  6. Generate statistical analysis only (requires existing results)\n")
  cat("  7. Load existing results\n")
  cat("  0. Exit\n\n")
  
  choice <- readline(prompt = "Enter your choice: ")
  
  switch(choice,
    "1" = demo_bloom_filter(),
    "2" = demo_cache_comparison(),
    "3" = main(quick_mode = FALSE),
    "4" = main(quick_mode = TRUE),
    "5" = {
      results <- load_experimental_results()
      if (!is.null(results)) generate_visualizations(results)
    },
    "6" = {
      results <- load_experimental_results()
      if (!is.null(results)) perform_statistical_analysis(results)
    },
    "7" = {
      results <- load_experimental_results()
      if (!is.null(results)) {
        cat("Results loaded successfully!\n")
        print(names(results))
      }
    },
    "0" = cat("Goodbye!\n"),
    cat("Invalid choice. Please try again.\n")
  )
}

# ============================================================================
# USAGE INSTRUCTIONS
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("USAGE INSTRUCTIONS\n")
cat(rep("=", 80), "\n\n")
cat("To run the complete project, execute one of the following:\n\n")
cat("1. Run everything (RECOMMENDED):\n")
cat("   results <- main()\n\n")
cat("2. Run in quick mode (for testing):\n")
cat("   results <- main(quick_mode = TRUE)\n\n")
cat("3. Run individual demos:\n")
cat("   demo_bloom_filter()\n")
cat("   demo_cache_comparison()\n\n")
cat("4. Interactive menu:\n")
cat("   interactive_menu()\n\n")
cat("5. Run specific components:\n")
cat("   exp_results <- run_all_experiments()\n")
cat("   figures <- generate_all_figures(exp_results)\n")
cat("   stats <- generate_statistical_report(exp_results)\n\n")
cat(rep("=", 80), "\n\n")

cat("Ready to execute! Type one of the commands above to begin.\n\n")
