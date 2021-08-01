# etalonnage
![R build status](https://github.com/aflatoune/etalonnage/workflows/R-CMD-check/badge.svg)

# Description

Having an accurate knowledge of the current economic situation and growth prospects is essential for business analysts and policy makers. Indeed, economic policy decisions
are based on assessments of the current and near future economic conditions, which are available with a significant delay. Nowcasting GDP growth is necessary to initiate longer-term forecasts, and it represents an essential piece of information for assessing real-time changes in the economic.

Within the French Treasury, two approches are used to forecast short-term GDP growth:

1. Statistical models of the relationship between GDP growth and an panel of variables (survey variables, consumption data, ...) released at a higher frequency than the GDP growth rate. These series are released with various delays so that the forecasts are conditioned on the sample of series that are known at the time the estimation is performed.
2. Sectoral models combining statistical models and accounting balances from national accounting.

This package provides functions to implement the first approach to GDP forecasting within the French Treasury.

# Usage
