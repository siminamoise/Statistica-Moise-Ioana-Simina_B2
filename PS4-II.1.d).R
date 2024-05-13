integrala_monte_carlo <- function() {
  f <- function(x) 1 / (4 * x^2 - 1)
  
  limita_inferioara <- 0
  limita_superioara <- 1
  valoare_exacta <- log(3/4)
  
  set.seed(123)
  nr_iteratii <- 100000
  x_aleator <- runif(nr_iteratii, limita_inferioara, limita_superioara)
  
  integrala_estimata <- (limita_superioara - limita_inferioara) * mean(f(x_aleator))
  
  eroare_absoluta <- abs(integrala_estimata - valoare_exacta)
  eroare_relativa <- eroare_absoluta / abs(valoare_exacta)
  
  rezultat <- list(integrala_estimata = integrala_estimata, valoare_exacta = valoare_exacta, eroare_absoluta = eroare_absoluta, eroare_relativa = eroare_relativa)
  return(rezultat)
}

rezultat_integrala <- integrala_monte_carlo()

cat("Valoare estimata:", format(rezultat_integrala$integrala_estimata, digits = 8), "\n")
cat("Valoare exacta:", format(rezultat_integrala$valoare_exacta, digits = 8), "\n")
cat("Eroare absoluta:", format(rezultat_integrala$eroare_absoluta, digits = 8), "\n")
cat("Eroare relativa:", format(rezultat_integrala$eroare_relativa * 100, digits = 2), "%\n")
