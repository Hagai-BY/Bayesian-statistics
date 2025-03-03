#################################################
# Bayesian Models for Financial Return Analysis
# 
# This script compares two Bayesian linear 
# regression models for predicting daily returns
# based on economic indicators
#################################################

# Load required libraries
library(rstan)
library(bayesplot)
library(ggplot2)
library(posterior)
library(tidyquant)
library(dplyr)
library(bayestestR)
library(loo)
library(bridgesampling)
library(gridExtra)

# Set seed for reproducibility
set.seed(123)

# Create results directories if they don't exist
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

# Load the data
rate_data <- read.csv('data/rate_parameters_complete.csv')

#################################################
# Model 1: Full model with all predictors
#################################################

# Prepare the data for Model 1
stan_data_m1 <- list(
  N = nrow(rate_data),
  y = rate_data$daily_return,
  vix_prev_day = rate_data$vix_prev_day,
  cpi_yoy = rate_data$CPI_YoY,
  gdp_yoy = rate_data$GDP_YoY,
  pce_yoy = rate_data$PCE_YoY,
  un_rate = rate_data$un_rate,
  forcast_change = rate_data$forcast_change  
)

# Compile Stan model 1
model_1 <- stan_model('models/model_1.stan')

# Run MCMC sampling for Model 1
fit_m1 <- sampling(model_1,
                  data   = stan_data_m1,
                  iter   = 5000,
                  warmup = 1500,
                  chains = 10,
                  seed   = 123
)



# Check convergence diagnostics for Model 1
summary_fit_m1 <- summary(fit_m1)
rhat_values_m1 <- summary_fit_m1$summary[, "Rhat"]
ess_values_m1 <- summary_fit_m1$summary[, "n_eff"]

# Print diagnostics for primary parameters
print("Model 1 - Rhat values (should be close to 1):")
print(rhat_values_m1[1:7])

print("Model 1 - Effective Sample Size values:")
print(ess_values_m1[1:7])

# Extract parameters for posterior predictive checks
params_m1 <- as_draws_rvars(fit_m1) |>
  mutate_variables(b = c(alpha, beta_vix, beta_cpi, beta_gdp, beta_pce, beta_un, beta_forcast)) |>
  subset_draws(variable = c("b", "sigma"))

# Create design matrix for Model 1
X_m1 <- cbind(1,
              stan_data_m1$vix_prev_day, 
              stan_data_m1$cpi_yoy, 
              stan_data_m1$gdp_yoy, 
              stan_data_m1$pce_yoy, 
              stan_data_m1$un_rate,
              stan_data_m1$forcast_change)  

# Calculate predicted means
mu_m1 <- as.vector(X_m1 %**% params_m1$b)
sigma_m1 <- params_m1$sigma

# Generate posterior predictive distribution
PPD_m1 <- rvar_rng(rnorm, n = stan_data_m1$N, mean = mu_m1, sd = sigma_m1)
PPD_m1_matrix <- draws_of(PPD_m1)

# Create posterior predictive check plot for Model 1
ppc_m1 <- ppc_stat(y = stan_data_m1$y, yrep = PPD_m1_matrix, stat = "mean") + 
  theme(legend.position = "bottom") +
  ggtitle("Posterior PPC Model 1")

# Save PPC plot
ggsave("results/figures/ppc_model1.png", ppc_m1, width = 8, height = 6)

#################################################
# Model 2: Reduced model with fewer predictors
#################################################

# Prepare the data for Model 2
stan_data_m2 <- list(
  N = nrow(rate_data),
  y = rate_data$daily_return,
  vix_prev_day = rate_data$vix_prev_day,
  cpi_yoy = rate_data$CPI_YoY,
  pce_yoy = rate_data$PCE_YoY,
  forcast_change = rate_data$forcast_change
)

# Compile Stan model 2
model_2 <- stan_model('models/model_2.stan')

# Run MCMC sampling for Model 2
fit_m2 <- sampling(model_2, 
                   data = stan_data_m2, 
                   chains = 10,
                   iter = 5000, 
                   warmup = 1500, 
                   seed = 123)

# Check convergence diagnostics for Model 2
summary_fit_m2 <- summary(fit_m2)
rhat_values_m2 <- summary_fit_m2$summary[, "Rhat"]
ess_values_m2 <- summary_fit_m2$summary[, "n_eff"]

# Print diagnostics for primary parameters
print("Model 2 - Rhat values (should be close to 1):")
print(rhat_values_m2[1:5])

print("Model 2 - Effective Sample Size values:")
print(ess_values_m2[1:5])

# Extract parameters for posterior predictive checks
params_m2 <- as_draws_rvars(fit_m2) |>
  mutate_variables(b = c(alpha, beta_vix, beta_cpi, beta_pce, beta_forcast)) |>
  subset_draws(variable = c("b", "sigma"))

# Create design matrix for Model 2
X_m2 <- cbind(1,
              stan_data_m2$vix_prev_day, 
              stan_data_m2$cpi_yoy, 
              stan_data_m2$pce_yoy,
              stan_data_m2$forcast_change) 

# Calculate predicted means
mu_m2 <- as.vector(X_m2 %**% params_m2$b)
sigma_m2 <- params_m2$sigma

# Generate posterior predictive distribution
PPD_m2 <- rvar_rng(rnorm, n = stan_data_m2$N, mean = mu_m2, sd = sigma_m2)
PPD_m2_matrix <- draws_of(PPD_m2)

# Create posterior predictive check plot for Model 2
ppc_m2 <- ppc_stat(y = stan_data_m2$y, yrep = PPD_m2_matrix, stat = "mean") + 
  theme(legend.position = "bottom") +
  ggtitle("Posterior PPC Model 2")

# Save PPC plot
ggsave("results/figures/ppc_model2.png", ppc_m2, width = 8, height = 6)

#################################################
# Prior predictive checks for Model 2
#################################################

# Prepare the data for prior predictive checks
stan_data_prior_m2 <- list(
  N = nrow(rate_data), 
  vix_prev_day = rate_data$vix_prev_day,  
  cpi_yoy = rate_data$CPI_YoY,  
  pce_yoy = rate_data$PCE_YoY,
  forcast_change = rate_data$forcast_change  
)

# Compile prior model
model_prior_m2 <- stan_model('models/prior_model.stan')

# Sample from prior
fit_prior_m2 <- sampling(model_prior_m2,
                 data = stan_data_prior_m2,
                  chains = 4,
                  iter = 5000,
                  warmup  = 3000,
                  seed = 123,
)


# Extract parameters for prior predictive checks
params_prior_m2 <- as_draws_rvars(fit_prior_m2) |>
  mutate_variables(b = c(alpha, beta_vix, beta_cpi, beta_pce, beta_forcast)) |> 
  subset_draws(variable = c("b", "sigma"))

# Create design matrix
X_prior_m2 <- cbind(1,
                    stan_data_prior_m2$vix_prev_day, 
                    stan_data_prior_m2$cpi_yoy, 
                    stan_data_prior_m2$pce_yoy,
                    stan_data_prior_m2$forcast_change)  

# Calculate predicted means from prior
mu_prior_m2 <- as.vector(X_prior_m2 %**% params_prior_m2$b)
sigma_prior_m2 <- params_prior_m2$sigma

# Generate prior predictive distribution
PPD_prior_m2 <- rvar_rng(rnorm, n = stan_data_prior_m2$N, mean = mu_prior_m2, sd = sigma_prior_m2)
PPD_prior_matrix_m2 <- draws_of(PPD_prior_m2)

# Create prior density overlay plot
prior_dens <- ppd_dens_overlay(ypred = PPD_prior_matrix_m2[1:50,]) +
  labs(title = "Prior PPC without the data - Model 2")

# Save prior density plot
ggsave("results/figures/prior_density_m2.png", prior_dens, width = 8, height = 6)

# Create prior PPC with data overlay
prior_ppc_data <- ppc_dens_overlay(y = rep(0, stan_data_prior_m2$N), 
                                   yrep = PPD_prior_matrix_m2[1:50,]) +
  labs(title = "Prior PPC with the data - Model 2") +
  theme_minimal()

# Save prior PPC with data
ggsave("results/figures/prior_ppc_data_m2.png", prior_ppc_data, width = 8, height = 6)

# Create prior distribution mean statistic plot
prior_ppc_stat <- ppc_stat(y = rep(0, stan_data_prior_m2$N), 
                           yrep = PPD_prior_matrix_m2, stat = "mean") + 
  theme(legend.position = "bottom")+
  labs(title = "Prior PPC Model 2")

# Save prior distribution stat plot
ggsave("results/figures/prior_ppc_stat_m2.png", prior_ppc_stat, width = 8, height = 6)

#################################################
# Q1: Model Comparison
#################################################

# Extract log-likelihood for WAIC calculation
log_lik_m1 <- extract_log_lik(fit_m1)
log_lik_m2 <- extract_log_lik(fit_m2)

# Calculate WAIC for both models
waic_m1 <- waic(log_lik_m1)
waic_m2 <- waic(log_lik_m2)

# Compare models using WAIC
waic_comparison <- loo_compare(waic_m1, waic_m2)
print("Model comparison using WAIC:")
print(waic_comparison)

# Calculate LOO-CV for both models
loo_m1 <- loo(fit_m1)
loo_m2 <- loo(fit_m2)

# Compare models using LOO-CV
loo_comparison <- loo_compare(loo_m1, loo_m2)
print("Model comparison using LOO-CV:")
print(loo_comparison)

# Calculate Bayes Factors via bridge sampling
bs_m1 <- bridge_sampler(fit_m1)
bs_m2 <- bridge_sampler(fit_m2)

# Compare models using Bayes Factor
bf_comparison <- bf(bs_m2, bs_m1)
print("Model comparison using Bayes Factor:")
print(bf_comparison)

# Save comparison results
comparison_results <- data.frame(
  Model = c("Model 1 (Full)", "Model 2 (Reduced)"),
  WAIC  = c(waic_m1$estimates["waic", "Estimate"], 
            waic_m2$estimates["waic", "Estimate"]),
  LOO   = c(loo_m1$estimates["looic", "Estimate"], 
            loo_m2$estimates["looic", "Estimate"]),
  BF    = c(1, as.numeric(bf_comparison)[1])
)


# Save comparison table
write.csv(comparison_results, "results/tables/model_comparison.csv", row.names = FALSE)

#################################################
# Q2: Parameter Analysis with ROPE and PD
#################################################

# Define ROPE range (0.1% margin)
rope_range <- c(-0.1, 0.1)

# Extract posterior samples from Model 2
posterior_samples_m2 <- extract(fit_m2)

# Calculate ROPE for each parameter in Model 2
rope_alpha_m2 <- rope(posterior_samples_m2$alpha, range = rope_range)
rope_beta_vix_m2 <- rope(posterior_samples_m2$beta_vix, range = rope_range)
rope_beta_cpi_m2 <- rope(posterior_samples_m2$beta_cpi, range = rope_range)
rope_beta_pce_m2 <- rope(posterior_samples_m2$beta_pce, range = rope_range)
rope_beta_forcast_m2 <- rope(posterior_samples_m2$beta_forcast, range = rope_range) 

# Print ROPE results
print("ROPE Analysis for Model 2 parameters:")
print(rope_alpha_m2)
print(rope_beta_vix_m2)
print(rope_beta_cpi_m2)
print(rope_beta_pce_m2)
print(rope_beta_forcast_m2)  

# Function to create ROPE plots
plot_rope <- function(param_samples, param_name, rope_range) {
  ggplot(data = data.frame(param_samples = param_samples), aes(x = param_samples)) +
    geom_density(fill = "blue", alpha = 0.5) + 
    geom_vline(xintercept = rope_range, color = "red", linetype = "dashed", size = 1) + 
    labs(title = paste(param_name),
         x = param_name, y = "Density") +
    theme_minimal()
}

# Create ROPE plots for each parameter
intercept <- plot_rope(posterior_samples_m2$alpha, "Intercept", rope_range)
vix <- plot_rope(posterior_samples_m2$beta_vix, "VIX", rope_range)
cpi <- plot_rope(posterior_samples_m2$beta_cpi, "CPI", rope_range)
pce <- plot_rope(posterior_samples_m2$beta_pce, "PCE", rope_range)
forcast <- plot_rope(posterior_samples_m2$beta_forcast, "Forecast Change", rope_range)

# Combine plots into a grid
rope_grid <- grid.arrange(intercept, vix, cpi, pce, forcast, ncol = 2)

# Save the combined ROPE plot
ggsave("results/figures/rope_analysis_m2.png", rope_grid, width = 10, height = 8)

# Calculate Probability of Direction (PD) for each parameter
pd_alpha_m2 <- p_direction(posterior_samples_m2$alpha)
pd_beta_vix_m2 <- p_direction(posterior_samples_m2$beta_vix)
pd_beta_cpi_m2 <- p_direction(posterior_samples_m2$beta_cpi)
pd_beta_pce_m2 <- p_direction(posterior_samples_m2$beta_pce)
pd_beta_forcast_m2 <- p_direction(posterior_samples_m2$beta_forcast) 

# Print PD results
print("Probability of Direction (PD) for Model 2 parameters:")
print(pd_alpha_m2)
print(pd_beta_vix_m2)
print(pd_beta_cpi_m2)
print(pd_beta_pce_m2)
print(pd_beta_forcast_m2) 

# Compile PD results into a table
pd_results <- data.frame(
  Parameter = c("Intercept", "VIX", "CPI", "PCE", "Forecast Change"),
  PD = c(pd_alpha_m2$pd, pd_beta_vix_m2$pd, pd_beta_cpi_m2$pd, 
         pd_beta_pce_m2$pd, pd_beta_forcast_m2$pd),
  Direction = c(ifelse(pd_alpha_m2$pd > 0.95, "Positive", "Uncertain"),
                ifelse(pd_beta_vix_m2$pd > 0.95, "Positive", "Uncertain"),
                ifelse(pd_beta_cpi_m2$pd > 0.95, "Positive", "Uncertain"),
                ifelse(pd_beta_pce_m2$pd > 0.95, "Positive", "Uncertain"),
                ifelse(pd_beta_forcast_m2$pd > 0.95, "Positive", "Uncertain"))
)

# Save PD results
write.csv(pd_results, "results/tables/pd_analysis_m2.csv", row.names = FALSE)

#################################################
# Q3: Probability of Positive/Negative Returns
#################################################

# Calculate probability of positive and negative returns
prob_above_0_m2 <- Pr(PPD_m2_matrix > 0)
prob_below_0_m2 <- Pr(PPD_m2_matrix < 0)

# Create PPD density plot with annotations
ppd_density_plot <- ggplot(data.frame(y_rep_value_m2 = as.vector(PPD_m2_matrix)), 
                           aes(x = y_rep_value_m2)) + 
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Posterior PPD - Percentage Yield - Model 2",
       subtitle = paste0("Positive return probability: ", round(prob_above_0_m2, 3), 
                         ", Negative return probability: ", round(prob_below_0_m2, 3)),
       x = "Percentage Yield", 
       y = "Density") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, 
           label = paste0("Positive return prob = ", round(prob_above_0_m2, 2)), 
           hjust = 1.1, vjust = 2, color = "darkgreen", size = 4) + 
  annotate("text", x = -Inf, y = Inf, 
           label = paste0("Negative return prob = ", round(prob_below_0_m2, 2)), 
           hjust = -0.1, vjust = 2, color = "darkred", size = 4)

# Save the PPD density plot
ggsave("results/figures/return_probability_m2.png", ppd_density_plot, width = 8, height = 6)

# Print a summary of results
cat("\n===== ANALYSIS SUMMARY =====\n")
cat("Model comparison suggests:", 
    ifelse(waic_m2$estimates["waic", "Estimate"] < waic_m1$estimates["waic", "Estimate"], 
           "Model 2 (reduced model) is preferred", 
           "Model 1 (full model) is preferred"), 
    "based on WAIC and LOO metrics.\n")

cat("The probability of positive returns is", round(prob_above_0_m2, 3), "\n")
cat("The probability of negative returns is", round(prob_below_0_m2, 3), "\n")

