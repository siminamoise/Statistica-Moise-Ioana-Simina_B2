Student_LNM <- function(n = c(1000, 10000, 100000, 1000000), r = c(2, 3, 4, 5), samples_n, sample_size) 
{
  for (samples_n in n) 
  {
    for (val_r in r) 
    {
      sample_average <- matrix(nrow = samples_n, ncol = length(sample_size))
      
      for (i in 1:length(sample_size)) 
      {
        samples <- matrix(rt(samples_n * sample_size[i], df = r), ncol = sample_size[i])
        sample_average[, i] <- apply(samples, 1, mean)
      }
      
      plot(sample_size, colMeans(sample_average), type = "l", xlab = "Sample size", ylab = "Sample average")
      
      abline(h = 0, col = "red", lty = 2)
    }
  }
}

Student_LNM(samples_n = 100, sample_size = c(100, 200, 500, 1000))
