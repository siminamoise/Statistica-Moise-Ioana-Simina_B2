#C1.a)
generate_random_permutation <- function(n) {
  # Generăm n valori uniforme standard
  u <- runif(n)
  # Sortăm valorile și returnăm permutarea lor
  return(order(u))
}
#C1.b)
lexicographic_compare <- function(word1, word2) {
  len1 <- length(word1)
  len2 <- length(word2)
  l <- min(len1, len2)
  
  for (i in 1:l) {
    if (word1[i] < word2[i]) {
      return(-1)  # word1 este mai mic lexicografic
    } else if (word1[i] > word2[i]) {
      return(1)  # word2 este mai mic lexicografic
    }
  }
  
  if (len1 < len2) {
    return(-1)  # word1 este mai mic lexicografic
  } else if (len1 > len2) {
    return(1)  # word2 este mai mic lexicografic
  }
  
  return(0)  # cuvintele sunt egale
}
#C1.c)
randomized_quicksort <- function(words) {
  if (length(words) <= 1) {
    return(words)
  }
  
  pivot_index <- sample(length(words), 1)
  pivot <- words[pivot_index]
  words <- words[-pivot_index]
  
  smaller <- vector("list")
  equal <- vector("list")
  larger <- vector("list")
  
  for (word in words) {
    comparison <- lexicographic_compare(word, pivot)
    if (comparison == -1) {
      smaller <- c(smaller, list(word))
    } else if (comparison == 1) {
      larger <- c(larger, list(word))
    } else {
      equal <- c(equal, list(word))
    }
  }
  
  return(c(randomized_quicksort(smaller), equal, randomized_quicksort(larger)))
}
#C1.d)
generate_random_permutation_of_words <- function(n, k) {
  # Generăm n cuvinte aleatoare de lungime k
  words <- matrix(sample(0:1, n*k, replace = TRUE), nrow = n)
  
  # Sortăm cuvintele folosind algoritmul QuickSort randomizat
  sorted_indices <- randomized_quicksort(1:n)
  
  # Returnăm permutarea
  return(sorted_indices)
}
# Exemplu de utilizare pentru generarea unei permutări aleatoare de cuvinte
n <- 5  # Numărul de cuvinte
k <- 4  # Lungimea fiecărui cuvânt
random_permutation_of_words <- generate_random_permutation_of_words(n, k)

# Convertim lista de indici în cuvinte
random_words <- sapply(random_permutation_of_words, function(index) {
  paste(sample(c(0, 1), k, replace = TRUE), collapse = "")
})

# Afisam cuvintele
cat("Permutarea aleatoare generată de cuvinte:\n")
for (i in 1:n) {
  cat("Cuvântul", i, ":", random_words[i], "\n")
}
#C2.a)

random_maximum_cut <- function(V, E) {
  n <- length(V) / 2
  A <- sample(V, n)  # Alegem n noduri aleator din V pentru A
  B <- setdiff(V, A)  # B este complementul lui A in V
  
  # Calculăm cardinalul tăieturii corespunzătoare
  cut_size <- length(which((E[,1] %in% A & E[,2] %in% B) | (E[,1] %in% B & E[,2] %in% A)))
  
  return(list(A = A, B = B, cut_size = cut_size))
}


#C2.b)

improve_maximum_cut <- function(V, E, iterations) {
  max_cut <- list(A = NULL, B = NULL, cut_size = 0)
  for (i in 1:iterations) {
    cut <- random_maximum_cut(V, E)
    if (cut$cut_size > max_cut$cut_size) {
      max_cut <- cut
    }
  }
  return(max_cut)
}
# Definirea unui graf simplu (ca listă de adiacență)
V <- c(1, 2, 3, 4)
E <- matrix(c(1, 2, 2, 3, 3, 4, 4, 1), ncol = 2, byrow = TRUE)

# Apelarea funcției pentru a obține o tăietură de cardinal maxim
cut_result <- random_maximum_cut(V, E)

# Afișarea rezultatelor
cat("Nodurile din A:", cut_result$A, "\n")
cat("Nodurile din B:", cut_result$B, "\n")
cat("Cardinalul tăieturii:", cut_result$cut_size, "\n")
