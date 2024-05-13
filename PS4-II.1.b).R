functie_f <- function(x) exp(x)

limita_inferioara <- 1
limita_superioara <- 4

valoare_exacta <- integrate(functie_f, limita_inferioara, limita_superioara)$value
