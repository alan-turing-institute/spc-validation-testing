# SPC Validation Testing

WIP to create a robust data validation method for https://github.com/alan-turing-institute/uatk-spc

## USAGE

Currently, there are two types of analyses that can be run somewhat easily:
- Moment-based clustering, use `clean.R`
- XAI (SHAP), use `runXAI.R` (not optimised and methods will be made more robust, but script = easy steps to follow; results should look like [this](https://github.com/alan-turing-institute/spc-validation-testing/blob/main/Output/plots/west-yorkshire-2020-MSOA11CD-incomeH-feature_importance_gg.png) for now)

## Original issue

**Algorithm 1** learns the SPC dataset, attempts to cluster distributions & flags outliers

**Algorithm 2** matches a region and vartiable to the best external source it knows

Then, when a user wants to check a particular output:

- The output is checked for internal coherence with the rest of SPC by algorithm 1

- The output is checked compared to reference data by algorithm 2

- Some measures of deviance are provided

- The user is prompted whether they want to readjust their synthetic population and how

By extension, the same adjustment can be made to model future data.

NB: ideally, algorithm 1 and 2 would "co-learn" from each other.
