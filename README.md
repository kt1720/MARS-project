# Stat 360 Project group 11 member:

1. Kyle Deng 

2. Andy Liang 

3. Rachel Loo

# Files:
mars.R
- Implementation of the Multivariate Adaptive Regression Splines(MARS) algorithm and its helper functions.

test.R
- Test suite for the MARS algorithm and all its methods, includes three examples.

predict_mars.R
- Predict method for a mars object, return the fitted values if no input of new data.

plot_mars.R
- Plots of the univariate ANOVA contribution of a given variable (for numeric input) or boxplots by category of residuals for a given variable (for categorical input) for a mars object.

anova_mars.R
- Return the basis functions of 1 and 2 variables in a mars object.

summary.R
- Return the estimated coefficients of each basis functions and information on how they are constructed in a mars object.

print.R
- Return basic information regarding a mars object.

premier_league_stats.csv<br>
Nalla, Z. (2018). Premier League. Retrieved April 20, 2021 from https://www.kaggle.com/zaeemnalla/premier-league
- Data set of Premier league team stats from season 2006/2007 to 2017/2018, used in test.R for the second test.

polar_bear.csv<br>
Rode, K. D., 2020, Measurement data of Polar Bears captured in the Chukchi and southern Beaufort Seas, 1981-2017: U.S. Geological Survey data release, https://doi.org/10.5066/P9TVK3PX.
- Data set of measures collected on polar bears captured in the Chukchi and Beaufort Seas, 1981-2017 by the U.S. Geological Survey and U.S. Fish and Wildlife Service, used in test.R for the third test.
