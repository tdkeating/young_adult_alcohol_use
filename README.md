# young_adult_alcohol_use
Project Exploring Depression and Alcohol Use in Young Adults.

This project is based on data from the [National Longitudinal Survey of Youth (1997)](https://www.bls.gov/nls/nlsy97.htm).

## Files

### Data

- subdirectory containing data from National Longitudinal Survey of Youth and codebook

### Plots_Tables

- subdirectory containing all output tables and plots created in `descriptive_statistics.R` and `models_and_modelling_assumptions_limitations.R`

### Report

- subdirectory containg final study report

### data_cleaning.R

- script loads data from National Longitudinal Survey of Youth and processes the data to be ready for analysis. Writes file `new_data.rds`.

### descriptive_statistics.R

- script which uses `new_data.rds` to get descriptive statistics from analysis. 
- summary of predictor variables
- mean and std dev of days_drank and days_heavy_drank for each depression category
- distributions of subject answers to days_drank and days_heavy_drank
- distributions of subject answers to income and log(income + 1)
- all created plots and tables are output in `Plots_Tables` subdirectory

### models_and_modelling_assumptions_limitations.R

- script which runs ordinal logistic regression analysis and checks modelling assumptions on `new_data.rds`
- all created plots and tables are output in `Plots_Tables` subdirectory
