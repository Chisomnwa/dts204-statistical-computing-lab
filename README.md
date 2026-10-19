# DTS 204 — Statistical Computing, Inference and Modelling

## Overview

This Miva Open University, Department of Computer Science lab assessment uses R to analyze the Palmer Penguins dataset. It applies exploratory data analysis, statistical inference, correlation analysis, regression modelling, and data visualization to real observations.

## Dataset

The workbook contains 344 observations, three species (Adelie, Chinstrap, and Gentoo), and eight variables: species, island, bill length, bill depth, flipper length, body mass, sex, and year. The data were collected by Dr. Kristen Gorman and Palmer Station, Antarctica LTER. The Palmer Penguins documentation identifies the data as CC0; see the [dataset documentation and license](https://allisonhorst.github.io/palmerpenguins/).

Missing values are retained in the original workbook. Descriptive statistics use each variable's available values; tests and models use the complete observations needed for their variables. Correlations use pairwise complete observations, while the two regression models use the same complete-case dataset. No values are imputed.

## Assessment tasks

1. Data exploration and preprocessing.
2. Hypothesis testing: a two-sided Welch t-test for Adelie and Gentoo body mass, and an exact binomial test of the known-sex male proportion against 50%.
3. Pearson correlation analysis with pairwise counts and a heatmap.
4. Simple and multiple linear regression, model comparison, and diagnostics.
5. Visualizations: body mass against flipper length, body mass by species, and a body-mass histogram with a normal reference curve.

## Key findings

- Gentoo penguins have a higher mean body mass than Adelie penguins; the Welch test finds a statistically significant difference (p < 0.001).
- The observed male proportion is 50.5%; the exact binomial test does not find evidence that it differs from 50% (p = 0.913).
- Flipper length and body mass have a strong positive correlation (Pearson r = 0.871). This association does not establish causation and may partly reflect species differences.
- The simple regression using flipper length has R² = 0.759, explaining about 75.9% of the in-sample body-mass variation.
- Adding bill length and bill depth does not significantly improve in-sample fit in the nested-model test (p = 0.1744). In-sample fit does not guarantee out-of-sample predictive performance.

## Technology

- R and base R statistical functions (`t.test()`, `binom.test()`, `cor()`, `lm()`, and `anova()`).
- `readxl` for workbook import.
- `ggplot2` for visualizations.
- `rmarkdown` and `knitr` for report generation.

## Project structure

```text
.
├── .gitignore
├── README.md
├── DTS_204_Dataset.xlsx
├── analysis.R
└── figures/
    ├── body_mass_by_species_boxplot.png
    ├── body_mass_histogram_normal_curve.png
    ├── body_mass_vs_flipper_length.png
    ├── correlation_heatmap.png
    ├── multiple_regression_diagnostics.png
    └── simple_regression_diagnostics.png
```

The student report source (`report.Rmd`) and generated PDFs are kept locally and excluded from this public repository because they contain student-identifying information. The assessment instruction PDF is also excluded. If the local `report.Rmd` is available, it can be rendered with the command below.

## Installation and reproducibility

Install the required R packages from CRAN:

```r
install.packages(
  c("readxl", "ggplot2", "rmarkdown", "knitr"),
  repos = "https://cloud.r-project.org"
)
```

From the project root, run the standalone analysis script to recalculate the results and regenerate the six figures:

```sh
Rscript analysis.R
```

When the local `report.Rmd` is present, regenerate the academic PDF report with:

```sh
Rscript -e 'rmarkdown::render("report.Rmd")'
```

PDF rendering requires Pandoc and a LaTeX installation; this report uses the XeLaTeX engine.

## Academic context

Prepared for the DTS 204 lab assessment, Department of Computer Science, Miva Open University.
