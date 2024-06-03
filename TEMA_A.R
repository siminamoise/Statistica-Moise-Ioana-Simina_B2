#A1.a)
Geometric = function(m, p, k) #Nr de esecuri inainte de primul succes
{
  x = k:m
  y = dgeom(x, p)
  
}

Poisson =function(m, lambda, k) #Pb unui nr de evenimente dintr-un interval fix, dupa o rata medie
{
  x = k:m
  y = dpois(x, lambda)
}

B = function(n, m, p, k) #Nr de succese intr-un nr fix de experimente
{
  x = k:m
  y = dbinom(x, size=n, prob=p)
}

# Parametri specificati
lambda = 3
p = 0.3
n= 15
m = 7
k = 2
#b)
functii = cbind(Geometric(m, p, k), Poisson(m, lambda, k), B(n, m, p, k))
colnames(functii) = c("Geometric", "Poisson", "Binomial")

barplot(t(functii), beside = TRUE, col = c("yellow", "red", "purple"), 
        names.arg = Geometric(m, p, k), legend = TRUE)
title(main = "MASA DE PROBABILITATE", xlab = "k", ylab = "P(X=k)")
#c)# cea mai mica valoare k0
find_k0 = function(lambda) 
{
  k0 = 0 
  prob = ppois(k0, lambda)
  while (prob <= 1 - 1e-6) {
    k0 = k0 + 1
    prob = ppois(k0, lambda)
  }
  return(k0)
}
lambda = 3.0
k0 = find_k0(lambda)
print(paste("Cea mai mică valoare a lui k0 pentru care P(Y ≤ k0) > 1 - 10^-6 este", k0))
#A2.a)
calculate_frequencies = function(file_path) {
  
  data = read.table(file_path, header=TRUE)
  
  sample_P = data$P
  sample_S = data$S
  
  freq_P = as.vector(table(sample_P))
  freq_S = as.vector(table(sample_S))
  #media valorilor 
  mean_P = mean(sample_P)
  mean_S = mean(sample_S)
  
  return(list(freq_P = freq_P, freq_S = freq_S, mean_P = mean_P, mean_S = mean_S))
}

file_path = "note_PS.txt"
results = calculate_frequencies(file_path)
print("Frecventele absolute pentru esantionul P:")
print(results$freq_P) 
print("Frecventele absolute pentru esantionul S:")
print(results$freq_S) 
print(paste("Media esantionului P:", results$mean_P)) 
print(paste("Media esantionului S:", results$mean_S)) 

#b)
clean_outliers = function(file_path, sample_name) 
{
  
  data = read.table(file_path, header=TRUE)
  
  
  sample = data[[sample_name]]
  
  # Calculează media și deviația eșantionului
  m = mean(sample)
  s = sd(sample) #abaterea
  
  #maj valorilor se afla in interval
  cleaned_sample <- sample[(sample >= m - 2 * s) & (sample <= m + 2 * s)]
  
  
  intervals = seq(1, 10, 1)
  hist(cleaned_sample, breaks = intervals, main = paste("Distribuția frecvențelor pentru eșantionul", sample_name), 
       xlab = "Valoare", ylab = "Frecvență", col = "blue", border = "black")
}

clean_outliers("note_PS.txt", "P")
