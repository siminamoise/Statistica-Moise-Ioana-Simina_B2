N <- function(miu, sigma) 
{
  x <- seq(-5, 5, length.out = 1000)
  densitate <- dnorm(x, mean = miu, sd = sigma)
  
  plot(x, densitate, type = "l", xlab = "x", ylab = "Densitate",
       main = paste0("Densitate Normala (miu = ", miu, ", sigma^2 = ", sigma^2, ")"))
}
N(0, 1)
