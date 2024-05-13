f <- function(u) {
  exp(-2*u^2)
}

N <- 50000
x <- rnorm(N, 0, 1)  

a <- -10
b <- 10

integral_estimate <- (b - a) * mean(f(x))

true_value <- sqrt(pi)/8
error <- abs(integral_estimate - true_value)

cat("Estimare integrală:", integral_estimate, "\n")
cat("Valoare reală:", true_value, "\n")
cat("Eroare:", error, "\n")
