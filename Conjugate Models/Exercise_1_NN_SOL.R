# Generate data
set.seed(123)
n <- 100; mu_true <- 2; sig2 <- 1
data <- rnorm(n, mu_true, sqrt(sig2))

# Hyperparameters
mu.0 <- 0
sig2.0 <- 10

mu.post <- (mu.0*sig2+n*sig2.0*mean(data))/(sig2+n*sig2.0)
var.post <- sig2*sig2.0/(sig2+n*sig2.0)

# Predictive density function
pred_pdf <- function(x) {dnorm(x, mu.post, sqrt(var.post + sig2))}

# Plot predictive density against data histogram
hist(data, probability = T, main="", xlab="Data", ylim = c(0,1))
curve(pred_pdf(x), n=1000, lwd=2, col='blue', add = T)
legend("topright",c("Predictive"), lwd=2,col=c("blue"))