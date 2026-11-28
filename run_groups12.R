# Run from repository root in a fresh R session.
packages <- c("ggplot2", "lmtest", "sandwich", "car")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install missing packages: ", paste(missing, collapse = ", "))
dir.create("reports", showWarnings = FALSE)
sink("reports/groups12_results.txt", split = TRUE)
tryCatch({
  for (file in c("01_data_cleaning.R", "02_exploratory_analysis.R", "03_regression_anova.R")) {
    cat("\nRUNNING:", file, "\n")
    source(file.path("scripts", file))
  }
  print(sessionInfo())
  cat("\nGROUPS 1 AND 2 COMPLETED SUCCESSFULLY\n")
}, finally = sink())
