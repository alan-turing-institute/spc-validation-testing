# SPC Validation Testing

WIP to create a robust data validation method for [SPC](https://github.com/alan-turing-institute/uatk-spc).

Use `create_environment.R` to set up the environment for both methods.

## Method 1: XAI-based approach

We test how SHAP explains an xgboost model that predicts the four first moments of any distribution from potentially significant variables.

<mark>Use `runXAI.R`. Still WIP but checked and improved to satisfactory levels. Version 1: use old package `shapr 2.2` (approved by CRAN) and use function `runSHAP` to produce results looking like [this](https://github.com/alan-turing-institute/spc-validation-testing/blob/main/Output/plots/west-yorkshire-2020-MSOA11CD-incomeH-feature_importance_gg.png). Version 2: use development package `shapr 2.3` (see setup) and use function `runSHAP2.3`to get results that look like this. Be aware that instalation of the development package might be difficult on an overbearing system.</mark>


Explanatory variables included:

| Name | Description | Source | Scale | Note |
|------|-------------------------------------|--------|-------|------|
| IMD19_ranks | Index of Multiple Deprivation 2019 - ranks | [gov](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019) | LSOA | Despite the name, supporting data are from 2015/16. Rank 1 = most deprived area |
| IMD19_scores | Index of Multiple Deprivation 2019 - scores | [gov](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019) | LSOA | ONS advises to use ranks instead for most analyses as the combined IMD does not have a direct interpretation. Also: income indicates the proportion of people under a specific threshold, including non-workers; so, only weak expected correlation to median income. Scores inverted for readability: lowest score = most deprived area |
| medAge | Median Age | [SPC](https://github.com/alan-turing-institute/uatk-spc) | OA | |
| popDens | Population Density | tbc | LSOA | |
| distHPD | Distance to local High Pop Density area | calc | LSOA | Minimum distance to a local High Pop Density area, as calculated within the commuting network considering inwards flows|
| distAllHPD | Distance to local High Pop Density area | calc | LSOA | Total distance to all local High Pop Density areas, as calculated within the commuting network considering inwards flows|
| betweenness | Betweenness centrality measure | [nomis](https://www.nomisweb.co.uk/census/2011/wf01bew) | OA | Directed betweenness weighted by (inverted) flow volumes from commuting data |
| closeness_all | Closeness centrality measure | [nomis](https://www.nomisweb.co.uk/census/2011/wf01bew) | OA | Undirected closeness centrality weighted by (inverted) flow volumes from commuting data |
| closeness_in | Closeness centrality measure | [nomis](https://www.nomisweb.co.uk/census/2011/wf01bew) | OA | Inwards directed closeness centrality weighted by (inverted) flow volumes from commuting data |
| closeness_out | Closeness centrality measure | [nomis](https://www.nomisweb.co.uk/census/2011/wf01bew) | OA | Outwards directed closeness centrality weighted by (inverted) flow volumes from commuting data |

Scale refers to the base scale in the raw data; gets aggregated when requested by user. IMD19 ranks and scores are aggregated by population weighted averaging; for centrality measures, flows inside the commuting networks are merged.

## Method 2: Clustering intersection approach

<mark>Use `runInter.R`.</mark> This file contains a few functions to test how similar two clustering based on different features are and to plot the contents of particular clusters.


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
