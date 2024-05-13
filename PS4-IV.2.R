set.seed(42)  

nr_computere <- 40
prob_infectare <- 0.2
nr_curatari <- 4  
nr_simulari <- 10000  

simuleaza_proces <- function() {
  computere_infectate <- rep(FALSE, nr_computere)
  computere_infectate[sample(nr_computere, 1)] <- TRUE  
  
  zile <- 0
  toate_infectate <- FALSE
  cel_putin_15 <- FALSE
  
  while(zile < 1000) { 
    zile <- zile + 1
    
    infectari_potentiale <- which(!computere_infectate)
    for (i in infectari_potentiale) {
      if (runif(1) < 1 - (1 - prob_infectare)^sum(computere_infectate)) {
        computere_infectate[i] <- TRUE
      }
    }
    
    
    if (all(computere_infectate)) {
      toate_infectate <- TRUE
      cel_putin_15 <- TRUE
      break
    }
    
    
    if (sum(computere_infectate) >= 15) {
      cel_putin_15 <- TRUE
    }
    
    infectate_curente <- which(computere_infectate)
    if (length(infectate_curente) <= nr_curatari) {
      computere_infectate[infectate_curente] <- FALSE
    } else {
      computere_infectate[sample(infectate_curente, nr_curatari)] <- FALSE
    }
  }
  
  return(c(toate_infectate, cel_putin_15))
}

rezultate <- replicate(nr_simulari, simuleaza_proces())
prob_toate_infectate <- mean(rezultate[1, ])
prob_cel_putin_15 <- mean(rezultate[2, ])

cat("Probabilitatea ca într-o zi să se infecteze toate calculatoarele:", prob_toate_infectate, "\n")
cat("Probabilitatea ca într-o zi cel puțin 15 calculatoare să fie infectate:", prob_cel_putin_15, "\n")
