# bloom-cache

Bloom Filters for Distributed Cache Optimization

---

## Project Structure

```
bloom-cache/
├── MAIN.R                          # Main execution script
├── 2_bloom_filter.R                # Bloom filter implementation (R6 class)
├── 3_hash_functions.R              # Hash function implementations
├── 4_cache_simulator.R             # Cache simulation (multi-tier)
├── 5_experiments.R                 # All experimental procedures
├── 6_visualizations.R              # Figure generation
├── 7_statistical_analysis.R        # Statistical tests & analysis
├── 8_data_analysis_report.R        # Data analysis report generation
├── experimental_results.rds        # Raw experimental data
├── statistical_analysis.rds        # Statistical test results
├── complete_dataset.rds            # Combined dataset (all experiments)
├── dataset_cache_performance.csv   # Cache performance data
├── dataset_scalability.csv         # Scalability data
├── figures/                        # All generated figures (8 total)
│   ├── fig1_fpr_validation.png
│   ├── fig2_parameter_optimization.png
│   ├── fig3_cache_performance.png
│   ├── fig4_latency_distributions.png
│   ├── fig5_scalability.png
│   ├── fig6_hash_comparison.png
│   ├── fig7_space_savings.png
│   └── fig8_theoretical_curves.png
├── documentation/                   # Mathematical derivations
└── report/                          # LaTeX presentation and report

```

---

## Execution

Run the complete analysis:

```r
source("MAIN.R")
```

This will execute all experiments, statistical analysis, and generate figures. Runtime: approximately 5 minutes.

---

## Project Overview

### Implementation

**Bloom Filter**: Probabilistic data structure implementing membership queries with space-efficient storage
- R6 object-oriented implementation
- Five hash function variants (xxHash64, MurmurHash3, MD5, SHA-256, basic)
- Multi-tier distributed cache simulator

### Data Analysis

**Dataset**: ~50,000 observations from controlled experiments
- Synthetic Zipf-distributed workloads (α = 0.5, 1.0, 1.5, 2.0)
- 10 variables total: 6 predictors, 4 outcomes

**Primary Variables**:
- Predictors: m/n ratio, k (hash functions), fill ratio, workload skewness, cache capacity
- Outcomes: latency (primary), false positive rate, cache hit rate, origin fetch rate

### Statistical Methods

**Hypothesis Tests**:
- Paired t-test for latency comparison (primary)
- Chi-square goodness-of-fit for FPR validation
- ANOVA for workload scenario effects
- Pearson correlation for variable associations
- Linear regression for predictive modeling

**Descriptive Statistics**: Mean, median, SD, IQR, quantiles, skewness, kurtosis, outlier detection

---

## Key Results

### Primary Finding
Bloom filters significantly reduce cache latency by 19-23% across all workload scenarios (p < 0.001). Effect sizes range from Cohen's d = 0.14 to 1.92 depending on workload skewness.

### Theoretical Validation
Empirical false positive rates match theoretical predictions (Bloom, 1970) with no significant deviation (chi-square p > 0.05). Implementation correctly implements the formula: FPR = (1 - e^(-kn/m))^k

### Model Performance
- Strong correlation between fill ratio and FPR (r = 0.89, p = 0.041)
- Linear regression: m/n ratio significantly predicts FPR (β₁ = -1.40, p = 0.002, R² = 0.41)
- ANOVA: Workload scenarios explain 46% of latency variance (η² = 0.46)

### Space Efficiency
98.4% memory reduction compared to hash tables. For 1 million elements: 1.14 MB (Bloom filter) vs 72 MB (hash table).

---

## Figures

All figures generated at 300 DPI, 10×8 inches:

1. **FPR Validation**: Empirical vs theoretical false positive rates
2. **Parameter Optimization**: m/n ratio and k selection effects
3. **Cache Performance**: Latency comparison across workload scenarios
4. **Latency Distributions**: Histogram comparison with/without Bloom filters
5. **Scalability**: Fill ratio vs FPR relationship
6. **Hash Function Comparison**: Performance across hash implementations
7. **Space Savings**: Memory usage comparison (log scale)
8. **Theoretical Curves**: Mathematical relationships

---

## Dataset Files

**Submitted Data**:
- `dataset_cache_performance.csv`: Cache latency measurements (4 scenarios)
- `dataset_scalability.csv`: Scalability experiments (5 capacity levels)
- `complete_dataset.rds`: Full experimental results

**Data Characteristics**:
- Source: Controlled computational experiments
- Workload: Zipf-distributed synthetic cache requests
- Size: ~50,000 observations total
- Validation: Industry-standard approach (Google, Amazon, Akamai)

---

## Dependencies

R packages (auto-installed):

```r
install.packages(c("R6", "ggplot2", "gridExtra", "digest", "dplyr", "moments"))
```

---

## Applications

Bloom filters are widely used in:
- Content Delivery Networks (CDNs)
- Database query optimization
- Web proxy caching
- Distributed storage systems

Current implementations: Google BigTable, Amazon DynamoDB, Apache Cassandra, Akamai CDN
