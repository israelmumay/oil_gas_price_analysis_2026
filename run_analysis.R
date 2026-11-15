
# =============================================================================
# Project: Oil and Gasoline Price Analysis in 2026
# Script:  run_analysis.R
# Authors: 
#
# Purpose:
#   Execute the complete analytical pipeline and export statistical
#   results, cleaned datasets, and visualizations.
#
# Note:
#   Run this script from the repository root.
#   Internet access is required for the forecasting analysis.
# =============================================================================


# 1. Create output directories 

dir.create("data/cleaned", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
dir.create("reports", showWarnings = FALSE)


# 2. Execute the analytical pipeline 

# Capture statistical outputs in a text file while displaying
# the results in the RStudio Console.

sink("reports/statistical_results.txt", split = TRUE)

tryCatch(
  {
    cat("============================================================\n")
    cat("OIL AND GASOLINE PRICE ANALYSIS IN 2026\n")
    cat("============================================================\n")
    cat("Execution date:", format(Sys.time()), "\n")
    
    
    # Data cleaning
    cat("\n1. DATA CLEANING\n")
    cat("------------------------------------------------------------\n")
    
    source("scripts/01_data_cleaning.R")
    
    
    # Exploratory data analysis
    cat("\n2. EXPLORATORY DATA ANALYSIS\n")
    cat("------------------------------------------------------------\n")
    
    source("scripts/02_exploratory_analysis.R")
    
    
    # Regression and ANOVA
    cat("\n3. REGRESSION AND ANOVA\n")
    cat("------------------------------------------------------------\n")
    
    source("scripts/03_regression_anova.R")
    
    
    # Forecasting analysis
    cat("\n4. FORECASTING ANALYSIS\n")
    cat("------------------------------------------------------------\n")
    
    source("scripts/04_forecasting.R")
    
    
    # Completion message
    cat("\n============================================================\n")
    cat("ANALYSIS COMPLETED SUCCESSFULLY\n")
    cat("============================================================\n")
    cat("Completion date:", format(Sys.time()), "\n")
  },
  
  error = function(e) {
    cat("\nAnalysis failed:", conditionMessage(e), "\n")
    stop(e)
  },
  
  finally = {
    sink()
  }
)
