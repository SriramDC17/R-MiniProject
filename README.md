# Global Population Growth Analysis and Prediction Using R

**Author:** Zore, MCA, Karpagam College of Engineering

An R project that analyses world population from 1800 to 2023 and compares four regression models
(linear, polynomial, log-linear and random forest) for predicting later years from earlier history.

## Dataset

- **Source:** [Our World in Data, Population](https://ourworldindata.org/population-growth) (`data/population.csv`)
- **Size:** 58,824 rows x 4 columns (`Entity`, `Code`, `Year`, `Population`), 269 entities
- **Modelling data:** the World series, 1800 to 2023 (224 yearly observations)
- The dataset contains population only, so the project can describe and predict the trend but cannot show what caused it.

## How to run

1. Install R (4.x) and the packages:
   ```r
   install.packages(c("ggplot2", "corrplot", "randomForest"))
   ```
2. Set the working directory to the project root, then run:
   ```r
   source("scripts/population_analysis.R")
   ```
3. Outputs are written to `outputs/plots`, `outputs/logs` and `outputs/*.csv`; cleaned data is written to `data/`.

## Project structure

```
global-population-growth-r/
|-- README.md
|-- data/
|   |-- population.csv                    original dataset
|   |-- population_clean_1800_2023.csv    cleaned data with growth features
|   `-- world_population_1800_2023.csv    World series used for modelling
|-- scripts/
|   `-- population_analysis.R             complete analysis script
`-- outputs/
    |-- plots/                            charts (PNG)
    |-- logs/                             R console output (text)
    |-- console/                          console screenshots used in the slides
    |-- model_comparison.csv
    |-- test_predictions.csv
    `-- forecast_2024_2050.csv
```

## Method

1. Load, inspect (`dim`, `str`, `summary`) and clean the data (missing values, duplicates, inconsistent entries).
2. Engineer features: yearly growth, growth rate (%) and log10 population.
3. Visualise population, growth rate, continents, correlation matrix and boxplots.
4. Split chronologically: train 1800-1978 (179 years), test 1979-2023 (45 years).
5. Train linear, polynomial (degree 3), log-linear and random forest models.
6. Predict the test years and evaluate with RMSE, MAE, R-squared and MAPE.

## Results (test years 1979-2023)

| Model | RMSE (billion) | MAE (billion) | R-squared | MAPE % |
|---|---|---|---|---|
| Linear Regression | 3.054 | 2.908 | -6.534 | 45.2 |
| **Polynomial (degree 3)** | **0.364** | **0.349** | **0.893** | **5.8** |
| Log-linear Regression | 2.628 | 2.514 | -4.578 | 39.2 |
| Random Forest | 2.363 | 2.085 | -3.513 | 31.1 |

Polynomial regression is the best of the four. The other three fail on the test years because population kept
accelerating after 1978 (the random forest predicts a flat 4.17 billion). World growth peaked at 2.25 % a year in 1964
and was 0.88 % in 2023.

![World population](outputs/plots/01_world_population.png)
![All models](outputs/plots/11_all_models.png)

## Limitations

- Models use `Year` only; the long-range forecasts in `outputs/forecast_2024_2050.csv` are illustrative, not reliable predictions
  (the cubic polynomial gives 12.4 billion for 2050).
- Residual diagnostics show autocorrelation, non-constant variance and non-normal residuals, so p-values should be read with caution.
- The dataset has no medical, technological or economic variables, so causes of growth cannot be tested.
