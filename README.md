# **Bayesian Return Modeling**

This project employs Bayesian models (using **Stan** and **R**) to analyze and compare different models predicting daily stock returns based on economic indicators.

---

## **Project Structure**
bayesian-return-modeling/
├── data/
│└── rate_parameters_complete.csv # Input data file 
├── models/ 
│ ├── model_1.stan # First Bayesian model 
│ ├── model_2.stan # Second Bayesian model 
│ └── prior_model.stan # Prior predictive model 
├── 
src/
│ └── final_work.R # R script for running the models
├── results/ 
│ ├── figures/ # Output plots and visualizations 
│ └── tables/ # Summary tables 
├── README.md 
└── .gitignore

---

## **Installation and Dependencies**

### **Prerequisites**
To run this project, you need:
- **R (version 4.0 or later)**
- The following R packages:

```r
install.packages(c("rstan", "bayesplot", "ggplot2", "posterior", "tidyquant", 
                   "dplyr", "bayestestR", "loo", "bridgesampling", "gridExtra"))
Running the Analysis
Ensure the dataset rate_parameters_complete.csv is in the data/ directory.
Run the script src/final_work.R in RStudio or the R environment.
Model Descriptions
This project compares two Bayesian linear regression models:

Model 1 (Full Model)
A regression model using the following predictors:

Previous day's VIX
Annual Inflation (CPI YoY)
GDP Growth (GDP YoY)
Private Consumption Expenditures (PCE YoY)
Unemployment Rate
Forecast Change
Model 2 (Simplified Model)
A regression model with a reduced set of predictors:

Previous day's VIX
Annual Inflation (CPI YoY)
Private Consumption Expenditures (PCE YoY)
Forecast Change
Model Evaluation & Comparison
The models are evaluated based on the following Bayesian model comparison metrics:

WAIC (Widely Applicable Information Criterion)
LOO-CV (Leave-One-Out Cross-Validation)
Bayes Factor (BF)
These metrics help determine which model provides a better predictive fit.

Posterior Predictive Checks (PPC)
Posterior predictive distributions are generated for each model.
Visual checks include:

Density plots of simulated vs. observed data.
Model residual diagnostics.
Key Findings
Model 1 incorporates more predictors, but its complexity might lead to overfitting.
Model 2 aims for a balance between predictive performance and generalization.
Model comparison metrics (WAIC, LOO-CV, Bayes Factor) provide insights into which model is preferable.
Next Steps
Extend the models by incorporating additional economic indicators.
Explore alternative priors and hyperparameter tuning.
Implement a Bayesian decision-making framework for stock trading strategies.
