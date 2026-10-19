# DTS 204 Lab Assessment - Task 1: Data Exploration and Preprocessing
#
# Run this script from the project root:
#   Rscript analysis.R

options(width = 160)
#
# The source workbook is read only. This script does not alter, impute, or
# remove any observations.

dataset_path <- "DTS_204_Dataset.xlsx"

if (!file.exists(dataset_path)) {
  stop(
    "Cannot find ", dataset_path,
    ". Run this script from the project root."
  )
}

penguins <- readxl::read_excel(dataset_path)

cat("DATASET DIMENSIONS\n")
cat("Rows:", nrow(penguins), "\n")
cat("Columns:", ncol(penguins), "\n\n")

cat("FIRST SIX OBSERVATIONS\n")
print(head(penguins, 6), n = 6, width = Inf)
cat("\n")

cat("DATASET STRUCTURE\n")
str(penguins)
cat("\n")

cat("VARIABLE TYPES\n")
variable_types <- data.frame(
  Variable = names(penguins),
  Type = vapply(
    penguins,
    function(column) paste(class(column), collapse = ", "),
    character(1)
  ),
  row.names = NULL
)
print(variable_types, row.names = FALSE)
cat("\n")

cat("MISSING VALUES BY VARIABLE\n")
missing_counts <- data.frame(
  Variable = names(penguins),
  Missing_Count = vapply(penguins, function(column) sum(is.na(column)), integer(1)),
  Valid_Count = vapply(penguins, function(column) sum(!is.na(column)), integer(1)),
  row.names = NULL
)
print(missing_counts, row.names = FALSE)
cat("\n")

cat(
  "Missing-value handling: the original observations are preserved. ",
  "No rows are dropped and no values are imputed. Each descriptive statistic ",
  "uses the non-missing values for its own variable; N_valid reports the ",
  "number used for that variable.\n",
  sep = ""
)
cat(
  "The numeric year variable is included in the summary because it is stored ",
  "as a numeric calendar year; it is not a biological measurement.\n\n",
  sep = ""
)

numeric_variables <- names(penguins)[
  vapply(penguins, is.numeric, logical(1))
]

if (length(numeric_variables) == 0L) {
  stop("No numerical variables were found.")
}

numeric_summary <- do.call(
  rbind,
  lapply(numeric_variables, function(variable_name) {
    values <- penguins[[variable_name]]
    valid_values <- values[!is.na(values)]
    n_valid <- length(valid_values)

    if (n_valid > 0L) {
      range_min <- min(valid_values)
      range_max <- max(valid_values)
      range_width <- range_max - range_min
      variable_mean <- mean(valid_values)
      variable_median <- median(valid_values)
    } else {
      range_min <- NA_real_
      range_max <- NA_real_
      range_width <- NA_real_
      variable_mean <- NA_real_
      variable_median <- NA_real_
    }

    variable_sd <- if (n_valid > 1L) stats::sd(valid_values) else NA_real_

    data.frame(
      Variable = variable_name,
      N_valid = n_valid,
      Mean = variable_mean,
      Median = variable_median,
      Range_min = range_min,
      Range_max = range_max,
      Range_width = range_width,
      Standard_deviation = variable_sd,
      row.names = NULL
    )
  })
)

cat("DESCRIPTIVE STATISTICS FOR NUMERICAL VARIABLES\n")
print(numeric_summary, row.names = FALSE, digits = 7)

cat("\n\nTASK 2: HYPOTHESIS TESTING\n")

# Test 1: Welch's two-sample t-test compares Adelie and Gentoo mean body mass.
# Welch's test does not assume equal population variances. Only observations
# in these two groups with an observed body_mass_g value enter the test.
body_mass_observed <- !is.na(penguins$body_mass_g)
two_species_body_mass <- penguins[
  body_mass_observed & penguins$species %in% c("Adelie", "Gentoo"),
  c("species", "body_mass_g")
]

adelie_mass <- two_species_body_mass$body_mass_g[
  two_species_body_mass$species == "Adelie"
]
gentoo_mass <- two_species_body_mass$body_mass_g[
  two_species_body_mass$species == "Gentoo"
]

if (length(adelie_mass) < 2L || length(gentoo_mass) < 2L) {
  stop("At least two non-missing body-mass observations per species are needed for Task 2.")
}

body_mass_test <- stats::t.test(
  x = adelie_mass,
  y = gentoo_mass,
  alternative = "two.sided",
  var.equal = FALSE,
  conf.level = 0.95
)

adelie_mean <- mean(adelie_mass)
adelie_sd <- stats::sd(adelie_mass)
gentoo_mean <- mean(gentoo_mass)
gentoo_sd <- stats::sd(gentoo_mass)
alpha <- 0.05
body_mass_decision <- if (body_mass_test$p.value < alpha) {
  "Reject H0"
} else {
  "Fail to reject H0"
}

cat("\nTEST 1: ADELIE VS GENTOO BODY MASS\n")
cat("H0: The population mean body mass of Adelie penguins equals that of Gentoo penguins.\n")
cat("H1: The population mean body masses differ.\n")
cat("Method: two-sided independent-samples Welch t-test; alpha = 0.05.\n")
cat("Only Adelie and Gentoo records with non-missing body_mass_g are included.\n")
body_mass_group_summary <- data.frame(
  Species = c("Adelie", "Gentoo"),
  N = c(length(adelie_mass), length(gentoo_mass)),
  Mean_body_mass_g = c(adelie_mean, gentoo_mean),
  SD_body_mass_g = c(adelie_sd, gentoo_sd),
  row.names = NULL
)
print(body_mass_group_summary, row.names = FALSE, digits = 7)
cat(sprintf("t = %.6f\n", unname(body_mass_test$statistic)))
cat(sprintf("Degrees of freedom = %.6f\n", unname(body_mass_test$parameter)))
cat(sprintf("Two-sided p-value = %.8g\n", body_mass_test$p.value))
cat(sprintf(
  "95%% CI for mean difference (Adelie - Gentoo), g: [%.6f, %.6f]\n",
  body_mass_test$conf.int[1], body_mass_test$conf.int[2]
))
cat("Decision at alpha = 0.05:", body_mass_decision, "\n")
if (body_mass_test$p.value < alpha) {
  cat(sprintf(
    paste0(
      "Interpretation: The data provide evidence that mean body mass differs ",
      "between Adelie and Gentoo penguins. Adelie penguins in this sample ",
      "average %.1f g and Gentoo penguins average %.1f g.\n"
    ),
    adelie_mean, gentoo_mean
  ))
} else {
  cat(
    "Interpretation: The data do not provide sufficient evidence that mean body mass differs between Adelie and Gentoo penguins.\n"
  )
}

# Test 2: an exact two-sided binomial test compares the known-sex male share
# with 0.50. Missing values and values other than male/female (including an
# explicit unknown label, if present) are excluded from both counts.
sex_clean <- tolower(trimws(as.character(penguins$sex)))
known_sex <- !is.na(sex_clean) & sex_clean %in% c("male", "female")
known_sex_values <- sex_clean[known_sex]
male_count <- sum(known_sex_values == "male")
known_sex_count <- length(known_sex_values)

if (known_sex_count == 0L) {
  stop("No penguins with known male/female sex values were found for Task 2.")
}

male_proportion_test <- stats::binom.test(
  x = male_count,
  n = known_sex_count,
  p = 0.50,
  alternative = "two.sided",
  conf.level = 0.95
)

observed_male_proportion <- male_count / known_sex_count
male_proportion_decision <- if (male_proportion_test$p.value < alpha) {
  "Reject H0"
} else {
  "Fail to reject H0"
}

cat("\nTEST 2: PROPORTION OF MALE PENGUINS\n")
cat("H0: p = 0.50.\n")
cat("H1: p != 0.50.\n")
cat("Method: two-sided exact binomial test; alpha = 0.05.\n")
cat("Missing sex values and values other than male/female are excluded.\n")
cat("Number with known sex:", known_sex_count, "\n")
cat("Number of males:", male_count, "\n")
cat(sprintf("Observed male proportion = %.6f\n", observed_male_proportion))
cat(sprintf("Two-sided p-value = %.8g\n", male_proportion_test$p.value))
cat(sprintf(
  "95%% exact binomial CI for male proportion: [%.6f, %.6f]\n",
  male_proportion_test$conf.int[1], male_proportion_test$conf.int[2]
))
cat("Decision at alpha = 0.05:", male_proportion_decision, "\n")
if (male_proportion_test$p.value < alpha) {
  cat(sprintf(
    paste0(
      "Interpretation: The data provide evidence that the proportion of ",
      "males among penguins with known sex differs from 50%%; the observed ",
      "sample proportion is %.1f%%.\n"
    ),
    100 * observed_male_proportion
  ))
} else {
  cat(sprintf(
    paste0(
      "Interpretation: The data do not provide sufficient evidence that ",
      "the proportion of males among penguins with known sex differs from ",
      "50%%; the observed sample proportion is %.1f%%.\n"
    ),
    100 * observed_male_proportion
  ))
}

cat("\n\nTASK 3: CORRELATION ANALYSIS\n")

# Pearson correlations use pairwise complete observations. Each pair therefore
# uses all rows where both measurements are present; no values are imputed.
correlation_variables <- c(
  "bill_length_mm",
  "bill_depth_mm",
  "flipper_length_mm",
  "body_mass_g"
)
correlation_data <- penguins[correlation_variables]
correlation_matrix <- stats::cor(
  correlation_data,
  method = "pearson",
  use = "pairwise.complete.obs"
)
pairwise_n <- crossprod(as.matrix(!is.na(correlation_data)))
dimnames(pairwise_n) <- list(correlation_variables, correlation_variables)

cat("Pearson correlation matrix (pairwise complete observations; rounded to 3 decimals)\n")
print(round(correlation_matrix, 3))
cat("\nValid observation counts for each variable pair\n")
print(pairwise_n)
cat("\nPairwise missing-data handling: no imputation; each correlation uses rows with both measurements observed.\n")

# Work from the unique off-diagonal pairs to identify calculated relationships
# without counting each symmetric matrix entry twice.
upper_triangle_indices <- which(upper.tri(correlation_matrix), arr.ind = TRUE)
correlation_pairs <- data.frame(
  Variable_1 = rownames(correlation_matrix)[upper_triangle_indices[, "row"]],
  Variable_2 = colnames(correlation_matrix)[upper_triangle_indices[, "col"]],
  Correlation = correlation_matrix[upper_triangle_indices],
  row.names = NULL
)

correlation_labels <- c(
  bill_length_mm = "Bill length (mm)",
  bill_depth_mm = "Bill depth (mm)",
  flipper_length_mm = "Flipper length (mm)",
  body_mass_g = "Body mass (g)"
)
pair_label <- function(variable_1, variable_2) {
  paste(correlation_labels[variable_1], "and", correlation_labels[variable_2])
}

positive_pairs <- correlation_pairs[correlation_pairs$Correlation > 0, , drop = FALSE]
negative_pairs <- correlation_pairs[correlation_pairs$Correlation < 0, , drop = FALSE]

if (nrow(positive_pairs) > 0L) {
  strongest_positive <- positive_pairs[which.max(positive_pairs$Correlation), ]
  cat(sprintf(
    "Strongest positive correlation: %s (r = %.3f).\n",
    pair_label(strongest_positive$Variable_1, strongest_positive$Variable_2),
    strongest_positive$Correlation
  ))
} else {
  cat("No positive off-diagonal correlations were observed.\n")
}

if (nrow(negative_pairs) > 0L) {
  strongest_negative <- negative_pairs[which.min(negative_pairs$Correlation), ]
  cat(sprintf(
    "Strongest negative correlation: %s (r = %.3f).\n",
    pair_label(strongest_negative$Variable_1, strongest_negative$Variable_2),
    strongest_negative$Correlation
  ))
} else {
  cat("No negative off-diagonal correlations were observed.\n")
}

strong_pairs <- correlation_pairs[
  abs(correlation_pairs$Correlation) >= 0.70,
  ,
  drop = FALSE
]
cat("Relationships meeting the descriptive strong-correlation threshold (|r| >= 0.70):\n")
if (nrow(strong_pairs) == 0L) {
  cat("None.\n")
} else {
  for (pair_index in seq_len(nrow(strong_pairs))) {
    pair <- strong_pairs[pair_index, ]
    direction <- if (pair$Correlation > 0) "positive" else "negative"
    cat(sprintf(
      "- %s: r = %.3f (%s association).\n",
      pair_label(pair$Variable_1, pair$Variable_2),
      pair$Correlation,
      direction
    ))
  }
}
cat(
  paste0(
    "Interpretation: Correlation describes association and does not establish ",
    "causation. These coefficients combine all penguin species, so some ",
    "relationships may be influenced by differences among species.\n"
  )
)

# Save a symmetric, annotated heatmap for later report inclusion. The axis
# labels use plain-language variable names and the color scale is centered at 0.
correlation_long <- as.data.frame(as.table(correlation_matrix))
names(correlation_long) <- c("Variable_1", "Variable_2", "Correlation")
correlation_long$Variable_1 <- factor(
  correlation_long$Variable_1,
  levels = correlation_variables
)
correlation_long$Variable_2 <- factor(
  correlation_long$Variable_2,
  levels = rev(correlation_variables)
)

correlation_heatmap <- ggplot2::ggplot(
  correlation_long,
  ggplot2::aes(x = Variable_1, y = Variable_2, fill = Correlation)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.7) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.3f", Correlation)),
    size = 4
  ) +
  ggplot2::scale_fill_gradient2(
    low = "#2166AC",
    mid = "#F7F7F7",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Pearson r"
  ) +
  ggplot2::scale_x_discrete(labels = correlation_labels) +
  ggplot2::scale_y_discrete(labels = correlation_labels) +
  ggplot2::coord_fixed() +
  ggplot2::labs(
    title = "Pearson Correlations Among Penguin Measurements",
    subtitle = "Pairwise complete observations",
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
    axis.text.y = ggplot2::element_text(size = 10),
    panel.grid = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold")
  )

figure_directory <- "figures"
if (!dir.exists(figure_directory)) {
  dir.create(figure_directory)
}
heatmap_path <- file.path(figure_directory, "correlation_heatmap.png")
ggplot2::ggsave(
  filename = heatmap_path,
  plot = correlation_heatmap,
  width = 8,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)
cat("Correlation heatmap saved to:", heatmap_path, "(300 dpi)\n")

cat("\n\nTASK 4: REGRESSION MODELLING\n")

# Both regression models use the same rows with all four required
# measurements observed, allowing a fair in-sample model comparison.
regression_variables <- c(
  "body_mass_g",
  "flipper_length_mm",
  "bill_length_mm",
  "bill_depth_mm"
)
regression_complete <- stats::complete.cases(penguins[regression_variables])
regression_data <- as.data.frame(penguins[regression_complete, regression_variables])
regression_data$source_row <- which(regression_complete)
regression_n <- nrow(regression_data)
regression_excluded_n <- nrow(penguins) - regression_n

if (regression_n <= length(regression_variables)) {
  stop("Too few complete observations are available for Task 4 regression models.")
}

cat("\nCOMMON COMPLETE-CASE DATA\n")
cat("Observations used:", regression_n, "\n")
cat("Observations excluded for one or more missing model measurements:", regression_excluded_n, "\n")
cat("Both regression models below use these same complete cases.\n")

simple_model <- stats::lm(
  body_mass_g ~ flipper_length_mm,
  data = regression_data
)
multiple_model <- stats::lm(
  body_mass_g ~ flipper_length_mm + bill_length_mm + bill_depth_mm,
  data = regression_data
)

if (!identical(stats::nobs(simple_model), stats::nobs(multiple_model)) ||
    !identical(rownames(stats::model.frame(simple_model)),
               rownames(stats::model.frame(multiple_model)))) {
  stop("The simple and multiple models do not use identical observations.")
}

model_coefficient_table <- function(model) {
  coefficient_table <- summary(model)$coefficients
  data.frame(
    Term = rownames(coefficient_table),
    Estimate = coefficient_table[, "Estimate"],
    Standard_Error = coefficient_table[, "Std. Error"],
    t_statistic = coefficient_table[, "t value"],
    p_value = coefficient_table[, "Pr(>|t|)"],
    row.names = NULL,
    check.names = FALSE
  )
}

model_fit_metrics <- function(model) {
  model_summary <- summary(model)
  f_statistic <- model_summary$fstatistic
  overall_p_value <- stats::pf(
    f_statistic[["value"]],
    f_statistic[["numdf"]],
    f_statistic[["dendf"]],
    lower.tail = FALSE
  )
  c(
    N = stats::nobs(model),
    R_squared = model_summary$r.squared,
    Adjusted_R_squared = model_summary$adj.r.squared,
    Residual_Std_Error = model_summary$sigma,
    RMSE = sqrt(mean(stats::residuals(model)^2)),
    Overall_F = unname(f_statistic[["value"]]),
    Overall_F_df1 = unname(f_statistic[["numdf"]]),
    Overall_F_df2 = unname(f_statistic[["dendf"]]),
    Overall_p_value = overall_p_value
  )
}

format_equation <- function(model, predictor_labels) {
  coefficients <- stats::coef(model)
  equation <- sprintf("body_mass_g (g) = %.3f", coefficients[["(Intercept)"]])
  for (predictor in names(predictor_labels)) {
    equation <- paste0(
      equation,
      sprintf(" %+.3f × %s", coefficients[[predictor]], predictor_labels[[predictor]])
    )
  }
  equation
}

simple_labels <- c(flipper_length_mm = "flipper_length_mm (mm)")
multiple_labels <- c(
  flipper_length_mm = "flipper_length_mm (mm)",
  bill_length_mm = "bill_length_mm (mm)",
  bill_depth_mm = "bill_depth_mm (mm)"
)

cat("\nSIMPLE LINEAR REGRESSION\n")
cat("Model: body_mass_g ~ flipper_length_mm\n")
cat("Fitted equation:\n", format_equation(simple_model, simple_labels), "\n", sep = "")
cat(
  "Interpretation: the slope is the estimated change in body mass (g) for a 1 mm increase in flipper length. ",
  "The intercept is the estimated body mass at zero flipper length, which is outside the observed data range and is not biologically meaningful.\n",
  sep = ""
)
cat("Coefficient estimates (body mass in g; predictor in mm):\n")
print(model_coefficient_table(simple_model), row.names = FALSE, digits = 7)

simple_metrics <- model_fit_metrics(simple_model)
cat(sprintf("R-squared = %.6f\n", simple_metrics[["R_squared"]]))
cat(sprintf("Adjusted R-squared = %.6f\n", simple_metrics[["Adjusted_R_squared"]]))
cat(sprintf("Residual standard error = %.3f g\n", simple_metrics[["Residual_Std_Error"]]))
cat(sprintf("Training RMSE = %.3f g\n", simple_metrics[["RMSE"]]))
cat(sprintf(
  "Overall model: F(%g, %g) = %.4f, p = %.8g; %s at alpha = 0.05.\n",
  simple_metrics[["Overall_F_df1"]], simple_metrics[["Overall_F_df2"]],
  simple_metrics[["Overall_F"]], simple_metrics[["Overall_p_value"]],
  if (simple_metrics[["Overall_p_value"]] < alpha) "statistically significant" else "not statistically significant"
))
simple_slope <- stats::coef(simple_model)[["flipper_length_mm"]]
cat(sprintf(
  paste0(
    "Plain-language interpretation: in this sample, each additional millimetre ",
    "of flipper length is associated with an estimated %.2f g %s in body mass. ",
    "This describes an association, not a causal effect.\n"
  ),
  abs(simple_slope), if (simple_slope >= 0) "increase" else "decrease"
))

cat("\nMULTIPLE LINEAR REGRESSION\n")
cat("Model: body_mass_g ~ flipper_length_mm + bill_length_mm + bill_depth_mm\n")
cat("Fitted equation:\n", format_equation(multiple_model, multiple_labels), "\n", sep = "")
cat(
  "Interpretation: each slope estimates the change in body mass (g) for a 1 mm increase in that measurement, ",
  "holding the other two measurements constant. The intercept is the fitted mass when all measurements are zero, ",
  "which is outside the observed data range and has no useful biological interpretation.\n",
  sep = ""
)
cat("Coefficient estimates (body mass in g; predictors in mm):\n")
print(model_coefficient_table(multiple_model), row.names = FALSE, digits = 7)

multiple_metrics <- model_fit_metrics(multiple_model)
cat(sprintf("R-squared = %.6f\n", multiple_metrics[["R_squared"]]))
cat(sprintf("Adjusted R-squared = %.6f\n", multiple_metrics[["Adjusted_R_squared"]]))
cat(sprintf("Residual standard error = %.3f g\n", multiple_metrics[["Residual_Std_Error"]]))
cat(sprintf("Training RMSE = %.3f g\n", multiple_metrics[["RMSE"]]))
cat(sprintf(
  "Overall model: F(%g, %g) = %.4f, p = %.8g; %s at alpha = 0.05.\n",
  multiple_metrics[["Overall_F_df1"]], multiple_metrics[["Overall_F_df2"]],
  multiple_metrics[["Overall_F"]], multiple_metrics[["Overall_p_value"]],
  if (multiple_metrics[["Overall_p_value"]] < alpha) "statistically significant" else "not statistically significant"
))

multiple_coefficients <- model_coefficient_table(multiple_model)
multiple_predictor_tests <- multiple_coefficients[
  multiple_coefficients$Term != "(Intercept)",
  ,
  drop = FALSE
]
multiple_predictor_tests$Significant_at_0.05 <- multiple_predictor_tests$p_value < alpha
cat("Predictor significance at alpha = 0.05:\n")
print(
  multiple_predictor_tests[
    c("Term", "p_value", "Significant_at_0.05")
  ],
  row.names = FALSE,
  digits = 7
)

cat("\nIN-SAMPLE MODEL COMPARISON\n")
comparison_table <- data.frame(
  Model = c("Simple", "Multiple"),
  N = c(simple_metrics[["N"]], multiple_metrics[["N"]]),
  R_squared = c(simple_metrics[["R_squared"]], multiple_metrics[["R_squared"]]),
  Adjusted_R_squared = c(simple_metrics[["Adjusted_R_squared"]], multiple_metrics[["Adjusted_R_squared"]]),
  RMSE_g = c(simple_metrics[["RMSE"]], multiple_metrics[["RMSE"]]),
  Residual_Std_Error_g = c(simple_metrics[["Residual_Std_Error"]], multiple_metrics[["Residual_Std_Error"]]),
  row.names = NULL
)
print(comparison_table, row.names = FALSE, digits = 7)
cat(
  "These are in-sample fit measures. Lower training RMSE or higher R-squared does not guarantee better predictions for new observations.\n"
)

cat("\nNESTED-MODEL F-TEST\n")
nested_model_test <- stats::anova(simple_model, multiple_model)
print(nested_model_test)
nested_f <- nested_model_test$F[2]
nested_p <- nested_model_test$`Pr(>F)`[2]
if (is.na(nested_p)) {
  cat("The nested-model comparison did not return a p-value.\n")
} else if (nested_p < alpha) {
  cat(sprintf(
    "At alpha = 0.05, the added bill measurements significantly improve in-sample fit (F = %.4f, p = %.8g). This does not establish improved out-of-sample prediction.\n",
    nested_f, nested_p
  ))
} else {
  cat(sprintf(
    "At alpha = 0.05, there is insufficient evidence that the added bill measurements improve in-sample fit (F = %.4f, p = %.8g).\n",
    nested_f, nested_p
  ))
}

# Standard regression diagnostic plots for both fits: residuals vs fitted
# (linearity/variance), normal Q-Q, scale-location, and residuals vs leverage.
for (model_name in c("simple", "multiple")) {
  model <- if (model_name == "simple") simple_model else multiple_model
  diagnostic_path <- file.path(
    figure_directory,
    paste0(model_name, "_regression_diagnostics.png")
  )
  grDevices::png(
    filename = diagnostic_path,
    width = 2400,
    height = 1800,
    res = 300,
    pointsize = 12
  )
  graphics::par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
  graphics::plot(model, which = c(1, 2, 3, 5))
  grDevices::dev.off()
  cat("Regression diagnostic plots saved to:", diagnostic_path, "(300 dpi)\n")
}

# Numeric diagnostics supplement visual inspection. Thresholds identify cases
# for review; they are screening rules, not automatic reasons to delete data.
multiple_residuals <- stats::residuals(multiple_model)
multiple_fitted <- stats::fitted(multiple_model)
multiple_standard_residuals <- stats::rstandard(multiple_model)
multiple_studentized_residuals <- stats::rstudent(multiple_model)
multiple_cooks_distance <- stats::cooks.distance(multiple_model)
multiple_hat_values <- stats::hatvalues(multiple_model)
model_parameter_count <- length(stats::coef(multiple_model))
cook_threshold <- 4 / regression_n
leverage_threshold <- 2 * model_parameter_count / regression_n
unusual_residual_indices <- which(abs(multiple_studentized_residuals) > 3)
influential_indices <- which(multiple_cooks_distance > cook_threshold)
high_leverage_indices <- which(multiple_hat_values > leverage_threshold)

cat("\nMODEL ASSUMPTIONS AND DIAGNOSTICS (MULTIPLE MODEL)\n")
cat(
  "Linearity and constant variance: assess the residuals-versus-fitted and scale-location panels in the saved diagnostics; curvature or a funnel pattern would indicate a concern.\n"
)
shapiro_result <- stats::shapiro.test(multiple_residuals)
cat(sprintf(
  "Residual normality screening (Shapiro-Wilk): W = %.5f, p = %.8g; %s at alpha = 0.05. Review the Q-Q plot as well.\n",
  unname(shapiro_result$statistic), shapiro_result$p.value,
  if (shapiro_result$p.value < alpha) "evidence against normal residuals" else "no statistically significant evidence against normal residuals"
))
cat(sprintf(
  "Potential unusual observations (absolute studentized residual > 3): %d; source rows: %s\n",
  length(unusual_residual_indices),
  if (length(unusual_residual_indices) == 0L) "none" else paste(regression_data$source_row[unusual_residual_indices], collapse = ", ")
))
cat(sprintf(
  "Potential influential observations (Cook's distance > 4/n = %.5f): %d; source rows: %s\n",
  cook_threshold,
  length(influential_indices),
  if (length(influential_indices) == 0L) "none" else paste(regression_data$source_row[influential_indices], collapse = ", ")
))
cat(sprintf(
  "Potential high-leverage observations (hat value > 2p/n = %.5f): %d; source rows: %s\n",
  leverage_threshold,
  length(high_leverage_indices),
  if (length(high_leverage_indices) == 0L) "none" else paste(regression_data$source_row[high_leverage_indices], collapse = ", ")
))

vif_values <- vapply(names(multiple_labels), function(target_predictor) {
  other_predictors <- setdiff(names(multiple_labels), target_predictor)
  auxiliary_formula <- stats::reformulate(other_predictors, response = target_predictor)
  auxiliary_r_squared <- summary(stats::lm(auxiliary_formula, data = regression_data))$r.squared
  1 / (1 - auxiliary_r_squared)
}, numeric(1))
cat("Variance inflation factors (VIF; larger values indicate more predictor redundancy):\n")
print(data.frame(Predictor = names(vif_values), VIF = unname(vif_values)), row.names = FALSE, digits = 6)
cat(
  "Independence and limitations: independence cannot be established from these measurements alone. The data are observational, and unmodeled species differences may affect both measurements and body mass because species is not included in either specified regression.\n"
)

cat("\n\nTASK 5: DATA VISUALIZATION\n")

visualization_theme <- ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )

# The scatter plot uses the same complete cases as the Task 4 simple model.
# Its line is drawn directly from that model's fitted intercept and slope.
scatter_plot <- ggplot2::ggplot(
  regression_data,
  ggplot2::aes(x = flipper_length_mm, y = body_mass_g)
) +
  ggplot2::geom_point(color = "#2166AC", alpha = 0.65, size = 2) +
  ggplot2::geom_abline(
    intercept = stats::coef(simple_model)[["(Intercept)"]],
    slope = stats::coef(simple_model)[["flipper_length_mm"]],
    color = "#B2182B",
    linewidth = 1.1
  ) +
  ggplot2::labs(
    title = "Penguin Body Mass by Flipper Length",
    subtitle = sprintf("Observed penguins and fitted simple regression (n = %d)", nrow(regression_data)),
    x = "Flipper length (mm)",
    y = "Body mass (g)"
  ) +
  visualization_theme

scatter_path <- file.path(figure_directory, "body_mass_vs_flipper_length.png")
ggplot2::ggsave(
  filename = scatter_path,
  plot = scatter_plot,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)
cat("Scatter plot observations:", nrow(regression_data), "\n")
cat("Scatter plot saved to:", scatter_path, "(300 dpi)\n")

# Include all species and all recorded body masses. geom_boxplot() retains
# the standard outlier display; no observations are removed as outliers.
body_mass_by_species <- penguins[
  !is.na(penguins$body_mass_g),
  c("species", "body_mass_g")
]
species_levels <- c("Adelie", "Chinstrap", "Gentoo")
body_mass_by_species$species <- factor(
  body_mass_by_species$species,
  levels = species_levels
)
species_plot_counts <- table(body_mass_by_species$species)

species_boxplot <- ggplot2::ggplot(
  body_mass_by_species,
  ggplot2::aes(x = species, y = body_mass_g, fill = species)
) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_fill_manual(
    values = c(Adelie = "#4C78A8", Chinstrap = "#F2A541", Gentoo = "#59A14F"),
    guide = "none"
  ) +
  ggplot2::labs(
    title = "Body Mass Distribution by Penguin Species",
    subtitle = sprintf("Recorded body masses (n = %d); boxplot outliers are retained", nrow(body_mass_by_species)),
    x = "Penguin species",
    y = "Body mass (g)"
  ) +
  visualization_theme

boxplot_path <- file.path(figure_directory, "body_mass_by_species_boxplot.png")
ggplot2::ggsave(
  filename = boxplot_path,
  plot = species_boxplot,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)
cat("Boxplot observations:", nrow(body_mass_by_species), "\n")
cat("Boxplot observations by species:\n")
print(species_plot_counts)
cat("Boxplot saved to:", boxplot_path, "(300 dpi)\n")

# A density-scaled histogram allows a normal density with the observed mean
# and standard deviation to be overlaid. The curve is only a reference shape.
body_mass_observed <- penguins$body_mass_g[!is.na(penguins$body_mass_g)]
body_mass_mean <- mean(body_mass_observed)
body_mass_sd <- stats::sd(body_mass_observed)
body_mass_histogram <- ggplot2::ggplot(
  data.frame(body_mass_g = body_mass_observed),
  ggplot2::aes(x = body_mass_g)
) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = ggplot2::after_stat(density)),
    bins = 30,
    fill = "#4C78A8",
    color = "white",
    linewidth = 0.35
  ) +
  ggplot2::stat_function(
    fun = stats::dnorm,
    args = list(mean = body_mass_mean, sd = body_mass_sd),
    color = "#B2182B",
    linewidth = 1.1
  ) +
  ggplot2::labs(
    title = "Penguin Body Mass Distribution",
    subtitle = sprintf(
      "Reference normal curve (mean = %.0f g; SD = %.0f g)",
      body_mass_mean,
      body_mass_sd
    ),
    caption = "Reference curve only; pooled body masses are not assumed normal.",
    x = "Body mass (g)",
    y = "Density"
  ) +
  visualization_theme +
  ggplot2::theme(
    plot.subtitle = ggplot2::element_text(size = 11),
    plot.caption = ggplot2::element_text(size = 10, hjust = 0)
  )

histogram_path <- file.path(figure_directory, "body_mass_histogram_normal_curve.png")
ggplot2::ggsave(
  filename = histogram_path,
  plot = body_mass_histogram,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)
cat("Histogram observations:", length(body_mass_observed), "\n")
cat(sprintf(
  "Histogram reference normal parameters: mean = %.3f g; SD = %.3f g.\n",
  body_mass_mean,
  body_mass_sd
))
cat("The normal curve is a reference distribution and does not imply that pooled penguin body masses are normally distributed.\n")
cat("Histogram saved to:", histogram_path, "(300 dpi)\n")
