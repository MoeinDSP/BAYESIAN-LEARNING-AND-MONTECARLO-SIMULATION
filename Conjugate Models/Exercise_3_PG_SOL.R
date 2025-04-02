# Part 1: Simulation and Prior Specification
set.seed(42)

# 1A. Simulate 30 days of ER arrivals with true lambda = 15
true_lambda <- 15
days <- 30
arrivals <- rpois(days, true_lambda)

# 1B. Define Gamma prior parameters
alpha_prior <- 10
beta_prior <- 1

# 1C. Visualize the true Poisson distribution and prior
par(mfrow = c(1, 2))

# Poisson distribution with true lambda
x <- 0:30
poisson_pmf <- dpois(x, true_lambda)
barplot(poisson_pmf, names.arg = x, col = "lightblue", 
        main = "Poisson Distribution (λ = 15)",
        xlab = "Number of arrivals", ylab = "Probability")

# Prior distribution for lambda
lambda_range <- seq(0, 30, length.out = 1000)
prior_density <- dgamma(lambda_range, shape = alpha_prior, rate = beta_prior)
plot(lambda_range, prior_density, type = "l", col = "red", lwd = 2,
     main = "Prior Distribution for λ ~ Gamma(10, 1)",
     xlab = "λ (arrival rate)", ylab = "Density")
abline(v = alpha_prior/beta_prior, lty = 2, col = "darkred")  # Prior mean
par(mfrow = c(1, 1))

# Part 2: Posterior Analysis

# 2A. Calculate posterior parameters
total_arrivals <- sum(arrivals)
alpha_posterior <- alpha_prior + total_arrivals
beta_posterior <- beta_prior + days

# 2B. Compute Bayes estimator (posterior mean)
bayes_estimator <- alpha_posterior / beta_posterior
cat("Total arrivals:", total_arrivals, "\n")
cat("Posterior parameters: Gamma(", alpha_posterior, ",", beta_posterior, ")\n")
cat("Bayes estimator (posterior mean):", bayes_estimator, "\n")
cat("True lambda:", true_lambda, "\n")
cat("Sample mean (MLE):", mean(arrivals), "\n")

# 2C. Generate posterior samples and create histogram
posterior_samples <- rgamma(5000, shape = alpha_posterior, rate = beta_posterior)
hist(posterior_samples, breaks = 30, col = "lightgreen", 
     main = "Posterior Distribution for λ",
     xlab = "λ (arrival rate)", ylab = "Frequency")
abline(v = bayes_estimator, col = "darkgreen", lwd = 2, lty = 1)  # Posterior mean
abline(v = true_lambda, col = "blue", lwd = 2, lty = 2)  # True lambda

# Part 3: Sequential Updating

# 3A. Show how posterior evolves with more data
observation_points <- c(5, 10, 20, 30)
lambda_range <- seq(5, 25, length.out = 1000)

# Initialize plot
plot(lambda_range, dgamma(lambda_range, shape = alpha_prior, rate = beta_prior),
     type = "l", col = "red", lwd = 2, 
     main = "Evolution of Posterior Distribution",
     xlab = "λ (arrival rate)", ylab = "Density",
     ylim = c(0, 0.5))

colors <- c("orange", "green", "blue", "purple")
legend_text <- c("Prior")

# Add posterior curves at different points
for (i in 1:length(observation_points)) {
  n <- observation_points[i]
  current_alpha <- alpha_prior + sum(arrivals[1:n])
  current_beta <- beta_prior + n
  
  # Add density curve
  lines(lambda_range, 
        dgamma(lambda_range, shape = current_alpha, rate = current_beta),
        col = colors[i], lwd = 2)
  
  legend_text <- c(legend_text, paste("After", n, "days"))
}

# Add legend and true lambda line
legend("topright", legend = legend_text, 
       col = c("red", colors), lwd = 2, cex = 0.8)
abline(v = true_lambda, lty = 2)

# 3B. Plot how Bayes estimator changes with more data
bayes_estimates <- numeric(days)
for (i in 1:days) {
  current_alpha <- alpha_prior + sum(arrivals[1:i])
  current_beta <- beta_prior + i
  bayes_estimates[i] <- current_alpha / current_beta
}

plot(1:days, bayes_estimates, type = "b", pch = 19,
     col = "darkblue", xlab = "Days of data", ylab = "Estimate of λ",
     main = "Convergence of Bayes Estimator")
abline(h = true_lambda, col = "red", lty = 2)
text(days/2, true_lambda + 0.5, "True λ = 15", col = "red")

# Part 4: Prior Sensitivity Analysis

# 1. Define alternative priors
informative_prior <- c(30, 2)  # Strong belief in lambda ≈ 15
diffuse_prior <- c(0.5, 0.05)  # Minimal prior knowledge
priors <- list(
  Original = c(alpha_prior, beta_prior),
  Informative = informative_prior,
  Diffuse = diffuse_prior
)

# Calculate posteriors for each prior
posterior_params <- list()
bayes_estimates <- numeric(length(priors))
for (i in 1:length(priors)) {
  prior_name <- names(priors)[i]
  alpha <- priors[[i]][1]
  beta <- priors[[i]][2]
  
  # Calculate posterior parameters
  alpha_post <- alpha + total_arrivals
  beta_post <- beta + days
  
  posterior_params[[prior_name]] <- c(alpha_post, beta_post)
  bayes_estimates[i] <- alpha_post / beta_post
}

# Plot posterior distributions for different priors
plot(0, 0, type = "n", xlim = c(10, 20), ylim = c(0, 0.6),
     xlab = "λ (arrival rate)", ylab = "Density",
     main = "Sensitivity Analysis: Impact of Different Priors")

colors <- c("darkgreen", "blue", "red")
for (i in 1:length(posterior_params)) {
  prior_name <- names(posterior_params)[i]
  alpha_post <- posterior_params[[prior_name]][1]
  beta_post <- posterior_params[[prior_name]][2]
  
  lines(lambda_range, 
        dgamma(lambda_range, shape = alpha_post, rate = beta_post),
        col = colors[i], lwd = 2)
}

# Add legend and true lambda line
legend("topright", 
       legend = paste0(names(posterior_params), ": λ̂ = ", round(bayes_estimates, 2)),
       col = colors, lwd = 2, cex = 0.8)
abline(v = true_lambda, lty = 2)