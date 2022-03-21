# approche-directe
![R build status](https://github.com/aflatoune/etalonnage/workflows/R-CMD-check/badge.svg)

# Description

Having an accurate knowledge of the current economic situation and growth prospects is essential for business analysts and policy makers. Economic policy decisions
are based on assessments of the current and near future economic conditions, which are available with a significant delay. Nowcasting GDP growth is necessary to initiate longer-term forecasts, and it represents an essential piece of information for assessing real-time changes in the economy.

Two approches are used to forecast short-term GDP growth:

1. Statistical models of the relationship between GDP growth and an panel of variables (survey variables, consumption data, ...) released at a higher frequency than the GDP growth rate. These series are released with various delays so that the forecasts are conditioned on the sample of series that are known at the time the forecast is performed.
2. Sectoral models combining statistical models and accounting balances from national accounting.

This package provides functions to implement the first approach to GDP forecasting.

# Installation

The package has been tested with R 3.6 and R >= 4.0.5.

```r
# install.packages("devtools")
devtools::install_github("aflatoune/approche-directe")
```

# Usage

For a detailed presentation of the package, see the associated vignette (to use the following command, you have to specify `build_vignettes = TRUE` when installing the package)

```r
utils::vignette("approche-directe")
```
