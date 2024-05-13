p1 <- 1/4
p2 <- 3/4

lambda1 <- 1/4  
lambda2 <- 1/12  

clients_mechanic1 <- 1
clients_mechanic2 <- 3

N <- 10000
sum_x <- 0
for (i in 1:N) {
  x <- clients_mechanic1*rexp(1, lambda1) + clients_mechanic2*rexp(1, lambda2)
  sum_x <- sum_x + x
}
expectation <- sum_x/N

cat("Estimated expectation:", expectation, "\n")
