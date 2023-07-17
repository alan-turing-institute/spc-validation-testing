
### Label clustering. Possible labels:
#     - [ ] Centrality measures within transportation network
#     - [ ] “Geographic centrality” aka distance from highest density areas
#     - [ ] Population density (better than categorical rural urban -- see def pb and World Pop 1000 inh./sqkm)
#     - [ ] Age; ethnicity? (<- also pb is categorical; maybe keep numerical initially)
#     - [ ] Index of deprivation
#     - [ ] Life satisfaction?
#
# SUPPORTED MODELS: stats::lm; stats::glm; ranger::ranger; mgcv::gam; xgboost::xgb.train
# Custom model possible, but implementation looks wonky

# Note: categorical supported (see full doc online: https://cran.r-project.org/web/packages/shapr/vignettes/understanding_shapr.html), but more restrictive
# Also, no NA

## For initial analysis, let's compute for each MSOA:
#   - NUM: Closeness centrality from ONS OD matrix (Azure)
#   - NUM: Betweenness centrality from ONS OD matrix (Azure)
#   - NUM: Distance from nearest local high density area (?)
#   - NUM: Population density (Compute from Shp)
#   - NUM: Median age (Compute from SPC - Azure)
#   - NUM: Index of deprivation (2019; https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019)
#
# MODEL: xgboost (Ref: https://xgboost.readthedocs.io/en/stable/tutorials/model.html)

## Define:
#   - "Local high density area"


### METHODS:
#     - SHAP <- Prob best?
#             Four approaches: "empirical" (default), "gaussian", "copula", or "ctree" (gausian and copula require gaussian assumptions; only ctree for cat var; no independence assumption vs original kernel approach).
#     - LIME <- Similar, less theoretically sound (supposedly)
#     - TREPAN? (model specific)


####################
####################
####### SHAP #######
####################
####################


library(xgboost)
library(shapr)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Prepare the data for explanation
explainer <- shapr(x_train, model)
#> The specified model provides feature classes that are NA. The classes of data are taken as the truth.

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)

# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$dt)
#>      none     lstat         rm       dis      indus
#> 1: 22.446 5.2632030 -1.2526613 0.2920444  4.5528644
#> 2: 22.446 0.1671901 -0.7088401 0.9689005  0.3786871
#> 3: 22.446 5.9888022  5.5450858 0.5660134 -1.4304351
#> 4: 22.446 8.2142204  0.7507572 0.1893366  1.8298304
#> 5: 22.446 0.5059898  5.6875103 0.8432238  2.2471150
#> 6: 22.446 1.9929673 -3.6001958 0.8601984  3.1510531

# Plot the resulting explanations for observations 1 and 6
plot(explanation, plot_phi0 = FALSE, index_x_test = c(1, 6))

# Use the Gaussian approach
explanation_gaussian <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p
)

# Plot the resulting explanations for observations 1 and 6
plot(explanation_gaussian, plot_phi0 = FALSE, index_x_test = c(1, 6))

# Use the Gaussian copula approach
explanation_copula <- explain(
  x_test,
  approach = "copula",
  explainer = explainer,
  prediction_zero = p
)

# Plot the resulting explanations for observations 1 and 6, excluding
# the no-covariate effect
plot(explanation_copula, plot_phi0 = FALSE, index_x_test = c(1, 6))

# Use the conditional inference tree approach
explanation_ctree <- explain(
  x_test,
  approach = "ctree",
  explainer = explainer,
  prediction_zero = p
)

# Plot the resulting explanations for observations 1 and 6, excluding 
# the no-covariate effect
plot(explanation_ctree, plot_phi0 = FALSE, index_x_test = c(1, 6))


###


#######
#######
#######