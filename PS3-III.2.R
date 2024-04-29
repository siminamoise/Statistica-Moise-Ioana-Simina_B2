GammaTCL <- function(alpha, lambda, n = 50, N = c(5000, 10000, 20000), z = c(-1.5, 0, 1.5)) 
{
  for (samples_n in N) 
  {
    for (val_z in z) 
    {
      samples <- matrix(rgamma(samples_n * n, shape = alpha, rate = lambda), ncol = n)
      
      sample_means <- apply(samples, 1, mean)
      
      mu <- n * alpha / lambda
      sigma <- sqrt(n * alpha) / lambda
      
      z_quantile <- qnorm(pnorm(val_z))
      
      hist(sample_means, breaks = 30, freq = FALSE, main = paste0("n = ", n, ", N = ", samples_n, ", z = ", val_z))
      curve(dnorm(x, mean = mu, sd = sigma), add = TRUE, col = "red")
      abline(v = mu + z_quantile * sigma, col = "blue")
    }
  }
}
GammaTCL(1, 2)
