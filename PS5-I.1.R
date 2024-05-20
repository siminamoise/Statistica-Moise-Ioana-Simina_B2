variabilaDiscreta <- function(valori, probabilitati) {
  if (length(valori) != length(probabilitati)) {
    stop("Trebuie să avem același număr de valori și de probabilități.")
  }
  
  if (any(probabilitati < 0) || sum(probabilitati) != 1) {
    stop("Nu putem avea probabilități negative, iar suma tuturor probabilităților trebuie să fie 1.")
  }
  
  nrRandom <- runif(1)
  probabilCumulate <- cumsum(probabilitati)
  indexSelectat <- sum(nrRandom > probabilCumulate) + 1
  return(valori[indexSelectat])
}
valori <- c(1, 2, 3)
probabilitati <- c(0.1, 0.3, 0.6)
valoareSimulata <- variabilaDiscreta(valori, probabilitati)
print(valoareSimulata)
